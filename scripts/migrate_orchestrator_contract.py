#!/usr/bin/env python3

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


ROUND_ID_RE = re.compile(r"^round-(\d+)$")
CONTROL_BUNDLE_RE = re.compile(
    r"orchestrator/roadmaps/[^/\s`]+/rev-\d+/"
    r"(roadmap|retry-subloop|verification)\.md"
)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Migrate repo-local orchestrator history to revisioned roadmap bundles."
    )
    parser.add_argument(
        "mode",
        choices=("audit", "write"),
        help="Print the inferred mapping or apply the migration.",
    )
    parser.add_argument(
        "--repo-root",
        default=str(Path(__file__).resolve().parents[1]),
        help="Repository root to migrate.",
    )
    args = parser.parse_args()

    repo_root = Path(args.repo_root).resolve()
    migration = Migration(repo_root)
    plan = migration.build_plan()

    if args.mode == "audit":
        migration.print_plan(plan)
        return 0

    migration.apply(plan)
    migration.print_plan(plan)
    return 0


def git(repo_root: Path, *args: str, check: bool = True) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo_root), *args],
        check=False,
        capture_output=True,
        text=True,
    )
    if check and result.returncode != 0:
        raise RuntimeError(
            f"git {' '.join(args)} failed with code {result.returncode}:\n"
            f"{result.stderr.strip()}"
        )
    return result.stdout


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def round_number(round_id: str) -> int:
    match = ROUND_ID_RE.match(round_id)
    if not match:
        raise ValueError(f"Unrecognized round id: {round_id}")
    return int(match.group(1))


def slugify(title: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", title.lower()).strip("-")
    return slug or "unnamed-roadmap"


def roadmap_title(markdown: str) -> str:
    for line in markdown.splitlines():
        if line.startswith("# "):
            return line[2:].strip()
    raise ValueError("Roadmap markdown is missing a top-level heading")


def selection_packet_date(markdown: str, fallback: str | None = None) -> str:
    match = re.search(r"^Date:\s*(\d{4}-\d{2}-\d{2})\s*$", markdown, re.MULTILINE)
    if match:
        return match.group(1)
    if fallback is not None:
        return fallback
    raise ValueError("Selection markdown is missing a Date line")


def normalize_worktree_paths_only(text: str, repo_root: Path) -> str:
    repo_prefix = re.escape(str(repo_root))
    text = re.sub(repo_prefix + r"/\.worktrees/round-\d+/", "", text)
    text = re.sub(r"(?<![A-Za-z0-9_./-])\.worktrees/round-\d+/", "", text)
    return text


def normalize_repo_and_worktree_paths(text: str, repo_root: Path) -> str:
    text = normalize_worktree_paths_only(text, repo_root)
    repo_prefix = re.escape(str(repo_root)) + r"/"
    return re.sub(repo_prefix, "", text)


def canonicalize_control_text(text: str, repo_root: Path) -> str:
    text = normalize_repo_and_worktree_paths(text, repo_root)
    return CONTROL_BUNDLE_RE.sub(
        lambda match: f"orchestrator/{match.group(1)}.md", text
    )


def first_commit_adding(repo_root: Path, path: str) -> str:
    output = git(
        repo_root,
        "log",
        "--diff-filter=A",
        "--format=%H",
        "--",
        path,
    ).strip()
    if not output:
        return ""
    return output.splitlines()[0]


def last_touch_at_or_before(repo_root: Path, commit: str, path: str) -> str:
    output = git(repo_root, "rev-list", "-1", commit, "--", path).strip()
    return output


def show_file_at_commit(repo_root: Path, commit: str, path: str) -> str:
    return git(repo_root, "show", f"{commit}:{path}")


def commit_date(repo_root: Path, commit: str) -> str:
    return git(repo_root, "show", "-s", "--format=%cs", commit).strip()


def commit_has_path(repo_root: Path, commit: str, path: str) -> bool:
    result = subprocess.run(
        ["git", "-C", str(repo_root), "cat-file", "-e", f"{commit}:{path}"],
        check=False,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def stable_hash(*parts: str) -> str:
    digest = hashlib.sha256()
    for part in parts:
        digest.update(part.encode("utf-8"))
        digest.update(b"\0")
    return digest.hexdigest()


@dataclass
class Snapshot:
    round_id: str
    source_kind: str
    round_dir: Path
    selection_path: Path
    review_record_path: Path
    review_anchor_path: str
    selection_commit: str | None
    review_record_add_commit: str | None
    roadmap_text: str
    retry_text: str
    verification_text: str
    state_obj: dict
    roadmap_title: str
    selection_date: str
    roadmap_id: str = ""
    roadmap_revision: str = ""
    roadmap_dir: str = ""

    @property
    def round_number(self) -> int:
        return round_number(self.round_id)

    @property
    def state_snapshot_path(self) -> Path:
        return self.round_dir / "state-snapshot.json"

    @property
    def is_archived(self) -> bool:
        return self.source_kind == "archived"

    def state_literal(self, key: str) -> str:
        value = self.state_obj.get(key)
        if value is None:
            return "null"
        return json.dumps(value)


@dataclass
class MigrationPlan:
    repo_root: Path
    active_round_id: str | None
    active_worktree_path: Path | None
    snapshots: list[Snapshot]


class Migration:
    def __init__(self, repo_root: Path) -> None:
        self.repo_root = repo_root
        self.orchestrator_root = repo_root / "orchestrator"
        self.rounds_root = self.orchestrator_root / "rounds"

    def build_plan(self) -> MigrationPlan:
        live_state = read_json(self.orchestrator_root / "state.json")
        active_round_id = live_state.get("active_round_id")
        active_worktree = None
        if live_state.get("worktree_path"):
            active_worktree = Path(live_state["worktree_path"])

        snapshots = self._load_archived_snapshots()
        active_snapshot = self._load_active_snapshot(live_state, active_worktree)
        if active_snapshot is not None:
            snapshots.append(active_snapshot)

        snapshots.sort(key=lambda snap: (snap.round_number, snap.source_kind))
        self._assign_roadmap_revisions(snapshots)

        return MigrationPlan(
            repo_root=self.repo_root,
            active_round_id=active_round_id,
            active_worktree_path=active_worktree,
            snapshots=snapshots,
        )

    def _load_archived_snapshots(self) -> list[Snapshot]:
        snapshots: list[Snapshot] = []
        for round_dir in sorted(self.rounds_root.glob("round-*")):
            if not round_dir.is_dir():
                continue
            round_id = round_dir.name
            selection_rel = f"orchestrator/rounds/{round_id}/selection.md"
            review_rel = f"orchestrator/rounds/{round_id}/review-record.json"
            review_md_rel = f"orchestrator/rounds/{round_id}/review.md"
            selection_path = round_dir / "selection.md"
            review_path = round_dir / "review-record.json"
            review_md_path = round_dir / "review.md"
            if not selection_path.is_file():
                continue

            if review_path.is_file():
                review_add = first_commit_adding(self.repo_root, review_rel)
                if review_add:
                    review_anchor_path = review_rel
                elif review_md_path.is_file():
                    review_add = first_commit_adding(self.repo_root, review_md_rel)
                    review_anchor_path = review_md_rel
                else:
                    continue
            elif review_md_path.is_file():
                review_add = first_commit_adding(self.repo_root, review_md_rel)
                review_anchor_path = review_md_rel
            else:
                continue
            if not review_add:
                continue
            selection_commit = last_touch_at_or_before(
                self.repo_root, review_add, selection_rel
            )
            if not selection_commit:
                selection_commit = review_add

            selection_text = selection_path.read_text(encoding="utf-8")

            roadmap_text = canonicalize_control_text(
                show_file_at_commit(self.repo_root, selection_commit, "orchestrator/roadmap.md"),
                self.repo_root,
            )
            retry_text = canonicalize_control_text(
                self._load_control_text_at_commit(
                    selection_commit, round_id, "orchestrator/retry-subloop.md"
                ),
                self.repo_root,
            )
            verification_text = canonicalize_control_text(
                self._load_control_text_at_commit(
                    selection_commit, round_id, "orchestrator/verification.md"
                ),
                self.repo_root,
            )
            state_obj = json.loads(
                show_file_at_commit(self.repo_root, selection_commit, "orchestrator/state.json")
            )

            snapshots.append(
                Snapshot(
                    round_id=round_id,
                    source_kind="archived",
                    round_dir=round_dir,
                    selection_path=selection_path,
                    review_record_path=review_path,
                    review_anchor_path=review_anchor_path,
                    selection_commit=selection_commit,
                    review_record_add_commit=review_add,
                    roadmap_text=roadmap_text,
                    retry_text=retry_text,
                    verification_text=verification_text,
                    state_obj=state_obj,
                    roadmap_title=roadmap_title(roadmap_text),
                    selection_date=selection_packet_date(
                        selection_text, fallback=commit_date(self.repo_root, selection_commit)
                    ),
                )
            )
        return snapshots

    def _load_control_text_at_commit(
        self, commit: str, round_id: str, path: str
    ) -> str:
        if commit_has_path(self.repo_root, commit, path):
            return show_file_at_commit(self.repo_root, commit, path)
        if path.endswith("retry-subloop.md"):
            return self._historical_retry_placeholder(round_id, commit)
        if path.endswith("verification.md"):
            return self._historical_verification_placeholder(round_id, commit)
        raise RuntimeError(f"Required control file missing at {commit}: {path}")

    def _historical_retry_placeholder(self, round_id: str, commit: str) -> str:
        return "\n".join(
            [
                "# Retry Subloop Contract",
                "",
                "This roadmap revision predates the repo-local retry-subloop",
                "contract.",
                "",
                "## Historical Note",
                "",
                f"- Snapshot source round: `{round_id}`",
                f"- Snapshot source commit: `{commit[:12]}`",
                "- `orchestrator/retry-subloop.md` did not exist for this",
                "  roadmap revision.",
                "- Same-round retry behavior was not defined by a dedicated",
                "  repo-local retry contract at this point in history.",
                "- Use the round-local `selection.md`, `plan.md`, `review.md`,",
                "  and `review-record.json` artifacts as the authoritative",
                "  record of stage flow for this revision.",
            ]
        )

    def _historical_verification_placeholder(
        self, round_id: str, commit: str
    ) -> str:
        return "\n".join(
            [
                "# Verification Contract",
                "",
                "This roadmap revision predates the repo-local verification",
                "contract.",
                "",
                "## Historical Note",
                "",
                f"- Snapshot source round: `{round_id}`",
                f"- Snapshot source commit: `{commit[:12]}`",
                "- `orchestrator/verification.md` did not exist for this",
                "  roadmap revision.",
                "- Verification requirements for this revision must be read from",
                "  the round-local `plan.md`, `review.md`, and accepted",
                "  `review-record.json` artifacts.",
            ]
        )

    def _load_active_snapshot(
        self, live_state: dict, active_worktree: Path | None
    ) -> Snapshot | None:
        active_round_id = live_state.get("active_round_id")
        if not active_round_id or active_worktree is None:
            return None

        selection_path = (
            active_worktree / "orchestrator" / "rounds" / active_round_id / "selection.md"
        )
        if not selection_path.is_file():
            return None

        selection_text = selection_path.read_text(encoding="utf-8")

        current_state = read_json(self.orchestrator_root / "state.json")
        roadmap_text, retry_text, verification_text = self._load_live_control_texts(
            self.repo_root, current_state
        )
        selection_state = self._load_active_selection_state(
            live_state, selection_text, active_round_id
        )

        return Snapshot(
            round_id=active_round_id,
            source_kind="active",
            round_dir=active_worktree / "orchestrator" / "rounds" / active_round_id,
            selection_path=selection_path,
            review_record_path=active_worktree
            / "orchestrator"
            / "rounds"
            / active_round_id
            / "review-record.json",
            review_anchor_path="current-live-state",
            selection_commit=None,
            review_record_add_commit=None,
            roadmap_text=roadmap_text,
            retry_text=retry_text,
            verification_text=verification_text,
            state_obj=selection_state,
            roadmap_title=roadmap_title(roadmap_text),
            selection_date=selection_packet_date(selection_text),
        )

    def _load_active_selection_state(
        self, live_state: dict, selection_text: str, active_round_id: str
    ) -> dict:
        active_round_dir = (
            self._selection_state_value(selection_text, "active_round_dir")
            or f"orchestrator/rounds/{active_round_id}"
        )
        return {
            "contract_version": live_state.get("contract_version", 2),
            "base_branch": live_state.get("base_branch"),
            "active_round_id": (
                self._selection_state_value(selection_text, "active_round_id")
                or active_round_id
            ),
            "stage": self._selection_state_value(selection_text, "stage")
            or "select-task",
            "current_task": self._selection_state_value(
                selection_text, "current_task"
            ),
            "branch": self._selection_state_value(selection_text, "branch")
            or live_state.get("branch"),
            "worktree_path": live_state.get("worktree_path"),
            "active_round_dir": active_round_dir,
            "round_artifacts": {},
            "last_completed_round": self._selection_state_value(
                selection_text, "last_completed_round"
            )
            or live_state.get("last_completed_round"),
            "resume_error": None,
            "retry": self._selection_state_value(selection_text, "retry"),
        }

    @staticmethod
    def _selection_state_value(selection_text: str, key: str) -> str | None:
        match = re.search(
            rf"`{re.escape(key)}:\s*(null|\"[^\"]*\")`",
            selection_text,
            re.DOTALL,
        )
        if not match:
            return None
        raw = match.group(1)
        if raw == "null":
            return None
        return json.loads(raw)

    def _load_live_control_texts(
        self, repo_root: Path, state_obj: dict
    ) -> tuple[str, str, str]:
        roadmap_dir = state_obj.get("roadmap_dir")
        if roadmap_dir:
            control_root = repo_root / roadmap_dir
        else:
            control_root = repo_root / "orchestrator"

        roadmap_text = canonicalize_control_text(
            (control_root / "roadmap.md").read_text(encoding="utf-8"), repo_root
        )
        retry_text = canonicalize_control_text(
            (control_root / "retry-subloop.md").read_text(encoding="utf-8"), repo_root
        )
        verification_text = canonicalize_control_text(
            (control_root / "verification.md").read_text(encoding="utf-8"), repo_root
        )
        return roadmap_text, retry_text, verification_text

    def _assign_roadmap_revisions(self, snapshots: list[Snapshot]) -> None:
        family_firsts: dict[str, tuple[str, int]] = {}
        for snapshot in snapshots:
            base_slug = slugify(snapshot.roadmap_title)
            first = family_firsts.get(base_slug)
            candidate = (snapshot.selection_date, snapshot.round_number)
            if first is None or candidate < first:
                family_firsts[base_slug] = candidate

        prefixed_ids: dict[str, str] = {}
        date_groups: dict[str, list[tuple[int, str]]] = {}
        for base_slug, (date, first_round_number) in family_firsts.items():
            date_groups.setdefault(date, []).append((first_round_number, base_slug))

        for date, entries in date_groups.items():
            for ordinal, (_, base_slug) in enumerate(sorted(entries)):
                prefixed_ids[base_slug] = f"{date}-{ordinal:02d}-{base_slug}"

        families: dict[str, dict[str, str]] = {}

        for snapshot in snapshots:
            base_slug = slugify(snapshot.roadmap_title)
            roadmap_id = prefixed_ids[base_slug]
            family_revisions = families.setdefault(roadmap_id, {})
            triple_key = stable_hash(
                snapshot.roadmap_text, snapshot.retry_text, snapshot.verification_text
            )
            roadmap_revision = family_revisions.get(triple_key)
            if roadmap_revision is None:
                roadmap_revision = f"rev-{len(family_revisions) + 1:03d}"
                family_revisions[triple_key] = roadmap_revision

            snapshot.roadmap_id = roadmap_id
            snapshot.roadmap_revision = roadmap_revision
            snapshot.roadmap_dir = (
                f"orchestrator/roadmaps/{roadmap_id}/{roadmap_revision}"
            )

    def print_plan(self, plan: MigrationPlan) -> None:
        for snapshot in plan.snapshots:
            source_bits = [snapshot.source_kind]
            if snapshot.selection_commit:
                source_bits.append(f"selection={snapshot.selection_commit[:12]}")
            if snapshot.review_record_add_commit:
                source_bits.append(
                    f"review_add={snapshot.review_record_add_commit[:12]}"
                )
            print(
                f"{snapshot.round_id}: {snapshot.roadmap_id}/{snapshot.roadmap_revision} "
                f"({', '.join(source_bits)})"
            )

    def apply(self, plan: MigrationPlan) -> None:
        roadmaps_root = self.orchestrator_root / "roadmaps"
        if roadmaps_root.exists():
            shutil.rmtree(roadmaps_root)

        bundles = self._bundle_writes(plan.snapshots)
        for bundle_dir, bundle_files in bundles.items():
            for filename, text in bundle_files.items():
                write_text(self.repo_root / bundle_dir / filename, text)

        for snapshot in plan.snapshots:
            self._write_round_snapshot(snapshot)
            self._rewrite_round_packet_markdown(snapshot)
            if snapshot.source_kind == "archived":
                self._rewrite_review_record(snapshot)

        self._rewrite_broken_worktree_links()
        active_snapshot = self._active_snapshot(plan.snapshots)
        if active_snapshot is None:
            raise RuntimeError("No active snapshot was inferred for write mode")

        self._rewrite_live_state(active_snapshot)
        self._write_pointer_stubs(active_snapshot)
        self._sync_live_contract_to_active_worktree(plan, active_snapshot)
        self._write_round_snapshot(active_snapshot)
        self._rewrite_round_packet_markdown(active_snapshot)

    def _bundle_writes(
        self, snapshots: Iterable[Snapshot]
    ) -> dict[str, dict[str, str]]:
        bundle_map: dict[str, dict[str, str]] = {}
        for snapshot in snapshots:
            bundle_dir = snapshot.roadmap_dir
            if bundle_dir in bundle_map:
                continue
            bundle_map[bundle_dir] = {
                "roadmap.md": self._rewrite_bundle_text(
                    snapshot.roadmap_text, bundle_dir
                ),
                "retry-subloop.md": self._rewrite_bundle_text(
                    snapshot.retry_text, bundle_dir
                ),
                "verification.md": self._rewrite_bundle_text(
                    snapshot.verification_text, bundle_dir
                ),
            }
        return bundle_map

    def _rewrite_bundle_text(self, text: str, bundle_dir: str) -> str:
        replacements = {
            "orchestrator/roadmap.md": f"{bundle_dir}/roadmap.md",
            "orchestrator/retry-subloop.md": f"{bundle_dir}/retry-subloop.md",
            "orchestrator/verification.md": f"{bundle_dir}/verification.md",
        }
        for source, target in replacements.items():
            text = text.replace(source, target)
        return text

    def _write_round_snapshot(self, snapshot: Snapshot) -> None:
        state_obj = dict(snapshot.state_obj)
        state_obj["roadmap_id"] = snapshot.roadmap_id
        state_obj["roadmap_revision"] = snapshot.roadmap_revision
        state_obj["roadmap_dir"] = snapshot.roadmap_dir
        write_json(snapshot.state_snapshot_path, state_obj)

    def _rewrite_round_packet_markdown(self, snapshot: Snapshot) -> None:
        markdown_files = sorted(snapshot.round_dir.rglob("*.md"))
        for path in markdown_files:
            original = path.read_text(encoding="utf-8")
            rewritten = normalize_repo_and_worktree_paths(original, self.repo_root)
            rewritten = self._rewrite_round_control_refs(rewritten, snapshot)
            if path == snapshot.selection_path:
                rewritten = self._upsert_selection_provenance(rewritten, snapshot)
                if not snapshot.is_archived:
                    rewritten = self._harmonize_active_selection_text(
                        rewritten, snapshot
                    )
            if rewritten != original:
                write_text(path, rewritten)

    def _rewrite_round_control_refs(self, text: str, snapshot: Snapshot) -> str:
        replacements = {
            "orchestrator/state.json": f"orchestrator/rounds/{snapshot.round_id}/state-snapshot.json",
            "orchestrator/roadmap.md": f"{snapshot.roadmap_dir}/roadmap.md",
            "orchestrator/retry-subloop.md": f"{snapshot.roadmap_dir}/retry-subloop.md",
            "orchestrator/verification.md": f"{snapshot.roadmap_dir}/verification.md",
        }
        for source, target in replacements.items():
            text = text.replace(source, target)
        text = CONTROL_BUNDLE_RE.sub(
            lambda match: f"{snapshot.roadmap_dir}/{match.group(1)}.md",
            text,
        )
        return text

    def _upsert_selection_provenance(self, text: str, snapshot: Snapshot) -> str:
        lines = [
            "## Roadmap Provenance",
            "",
            f"- Roadmap ID: `{snapshot.roadmap_id}`",
            f"- Roadmap Revision: `{snapshot.roadmap_revision}`",
            f"- Roadmap Dir: `{snapshot.roadmap_dir}`",
            f"- State Snapshot: `orchestrator/rounds/{snapshot.round_id}/state-snapshot.json`",
        ]
        if snapshot.is_archived:
            lines.append(
                "- Migration note: backfilled from git history using the last "
                "authoritative control-plane anchor available for this round."
            )
        else:
            lines.append(
                "- Migration note: backfilled from the active round selection "
                "packet; live resumability continues in `orchestrator/state.json`."
            )
        section = "\n".join(lines)

        existing = re.compile(
            r"\n## Roadmap Provenance\n.*?(?=\n## |\Z)", re.DOTALL
        )
        if existing.search(text):
            return existing.sub("\n" + section + "\n", text)

        marker = "\n## Selected Roadmap Item\n"
        if marker in text:
            return text.replace(marker, "\n" + section + "\n\n## Selected Roadmap Item\n", 1)

        heading = re.search(r"^# .+\n\n", text)
        if heading:
            return text[: heading.end()] + section + "\n\n" + text[heading.end() :]

        return section + "\n\n" + text

    def _harmonize_active_selection_text(self, text: str, snapshot: Snapshot) -> str:
        old_paragraph = "\n".join(
            [
                f"`orchestrator/rounds/{snapshot.round_id}/state-snapshot.json` "
                "fixes the live controller state at",
                f"`active_round_id: {snapshot.state_literal('active_round_id')}`, "
                f"`stage: {snapshot.state_literal('stage')}`, "
                f"`current_task: {snapshot.state_literal('current_task')}`,",
                f"`retry: {snapshot.state_literal('retry')}`, "
                f"`branch: {snapshot.state_literal('branch')}`, "
                "`active_round_dir:",
                f"{snapshot.state_literal('active_round_dir')}`, and "
                f"`last_completed_round: {snapshot.state_literal('last_completed_round')}`.",
            ]
        )
        state_paragraph = "\n".join(
            [
                f"`orchestrator/rounds/{snapshot.round_id}/state-snapshot.json` "
                "fixes the selection-time controller state for this packet at",
                f"`active_round_id: {snapshot.state_literal('active_round_id')}`, "
                f"`stage: {snapshot.state_literal('stage')}`, "
                f"`current_task: {snapshot.state_literal('current_task')}`,",
                f"`retry: {snapshot.state_literal('retry')}`, "
                f"`branch: {snapshot.state_literal('branch')}`, "
                "`active_round_dir:",
                f"{snapshot.state_literal('active_round_dir')}`, and "
                f"`last_completed_round: {snapshot.state_literal('last_completed_round')}`.",
                "The live resumable controller state for this still-open round "
                "continues in `orchestrator/state.json` inside the active worktree.",
            ]
        )
        text = text.replace(old_paragraph, state_paragraph)
        stale_status = (
            "Repository status in the active\n"
            "worktree shows only controller-owned "
            f"`M orchestrator/rounds/{snapshot.round_id}/state-snapshot.json` drift. No\n"
            "live blocker forces a different selection."
        )
        replacement_status = (
            "The migration only backfills roadmap provenance for this packet.\n"
            "No live blocker or retry obligation forces a different selection."
        )
        text = text.replace(stale_status, replacement_status)
        stale_live_pointer = (
            "The live resumable controller state for this still-open round "
            f"continues in `orchestrator/rounds/{snapshot.round_id}/state-snapshot.json` "
            "inside the active worktree."
        )
        fixed_live_pointer = (
            "The live resumable controller state for this still-open round "
            "continues in `orchestrator/state.json` inside the active worktree."
        )
        return text.replace(stale_live_pointer, fixed_live_pointer)

    def _rewrite_review_record(self, snapshot: Snapshot) -> None:
        if snapshot.review_record_path.exists():
            payload = read_json(snapshot.review_record_path)
        else:
            payload = {
                "status": "historical-pre-review-record-schema",
                "authoritative_source": snapshot.review_anchor_path,
                "migration_note": (
                    "This round predates repo-local review-record.json. "
                    "The revisioned-roadmap migration added this file only "
                    "to preserve roadmap provenance."
                ),
            }
        payload["roadmap_id"] = snapshot.roadmap_id
        payload["roadmap_revision"] = snapshot.roadmap_revision
        payload["roadmap_dir"] = snapshot.roadmap_dir
        write_json(snapshot.review_record_path, payload)

    def _rewrite_broken_worktree_links(self) -> None:
        roots = [
            self.repo_root / "docs" / "plans",
            self.repo_root / "tasks",
        ]
        for root in roots:
            if not root.exists():
                continue
            for path in root.rglob("*.md"):
                original = path.read_text(encoding="utf-8")
                rewritten = normalize_worktree_paths_only(original, self.repo_root)
                if rewritten != original:
                    write_text(path, rewritten)

        for path in (
            self.repo_root / "TODO.md",
            self.repo_root / "CHANGELOG.md",
            self.repo_root / "implementation_notes.md",
        ):
            if not path.exists():
                continue
            original = path.read_text(encoding="utf-8")
            rewritten = normalize_worktree_paths_only(original, self.repo_root)
            if rewritten != original:
                write_text(path, rewritten)

    def _rewrite_live_state(self, active_snapshot: Snapshot) -> None:
        live_state_path = self.orchestrator_root / "state.json"
        payload = read_json(live_state_path)
        payload["roadmap_id"] = active_snapshot.roadmap_id
        payload["roadmap_revision"] = active_snapshot.roadmap_revision
        payload["roadmap_dir"] = active_snapshot.roadmap_dir
        write_json(live_state_path, payload)

    def _write_pointer_stubs(self, active_snapshot: Snapshot) -> None:
        bundle_dir = active_snapshot.roadmap_dir
        roadmap_stub = "\n".join(
            [
                "# Live Roadmap Pointer Stub",
                "",
                "This file is not authoritative.",
                "Read `orchestrator/state.json` and resolve the live roadmap bundle through:",
                "",
                f"- `roadmap_id`: `{active_snapshot.roadmap_id}`",
                f"- `roadmap_revision`: `{active_snapshot.roadmap_revision}`",
                f"- `roadmap_dir`: `{bundle_dir}`",
                "",
                f"Authoritative roadmap: `{bundle_dir}/roadmap.md`",
            ]
        )
        retry_stub = "\n".join(
            [
                "# Live Retry Pointer Stub",
                "",
                "This file is not authoritative.",
                "Read `orchestrator/state.json` and resolve the live roadmap bundle through:",
                "",
                f"- `roadmap_id`: `{active_snapshot.roadmap_id}`",
                f"- `roadmap_revision`: `{active_snapshot.roadmap_revision}`",
                f"- `roadmap_dir`: `{bundle_dir}`",
                "",
                f"Authoritative retry contract: `{bundle_dir}/retry-subloop.md`",
            ]
        )
        verification_stub = "\n".join(
            [
                "# Live Verification Pointer Stub",
                "",
                "This file is not authoritative.",
                "Read `orchestrator/state.json` and resolve the live roadmap bundle through:",
                "",
                f"- `roadmap_id`: `{active_snapshot.roadmap_id}`",
                f"- `roadmap_revision`: `{active_snapshot.roadmap_revision}`",
                f"- `roadmap_dir`: `{bundle_dir}`",
                "",
                f"Authoritative verification contract: `{bundle_dir}/verification.md`",
            ]
        )
        write_text(self.orchestrator_root / "roadmap.md", roadmap_stub + "\n")
        write_text(self.orchestrator_root / "retry-subloop.md", retry_stub + "\n")
        write_text(
            self.orchestrator_root / "verification.md", verification_stub + "\n"
        )

    def _sync_live_contract_to_active_worktree(
        self, plan: MigrationPlan, active_snapshot: Snapshot
    ) -> None:
        active_worktree = plan.active_worktree_path
        if active_worktree is None:
            return

        parent_state = read_json(self.orchestrator_root / "state.json")
        worktree_state_path = active_worktree / "orchestrator" / "state.json"
        worktree_state = read_json(worktree_state_path)
        worktree_state["roadmap_id"] = parent_state["roadmap_id"]
        worktree_state["roadmap_revision"] = parent_state["roadmap_revision"]
        worktree_state["roadmap_dir"] = parent_state["roadmap_dir"]
        write_json(worktree_state_path, worktree_state)

        sync_targets = [
            Path("AGENTS.md"),
            Path("tasks/readme"),
            Path("TODO.md"),
            Path("CHANGELOG.md"),
            Path("implementation_notes.md"),
            Path("orchestrator/roadmap.md"),
            Path("orchestrator/retry-subloop.md"),
            Path("orchestrator/verification.md"),
            Path("orchestrator/roles"),
            Path("orchestrator/roadmaps"),
            Path(".codex/agents/orchestrator-guider.toml"),
            Path(".codex/agents/orchestrator-implementer.toml"),
            Path(".codex/agents/orchestrator-merger.toml"),
            Path(".codex/agents/orchestrator-planner.toml"),
            Path(".codex/agents/orchestrator-reviewer.toml"),
        ]
        for relative_path in sync_targets:
            source = self.repo_root / relative_path
            destination = active_worktree / relative_path
            if not source.exists():
                continue
            if source.is_dir():
                if destination.exists():
                    shutil.rmtree(destination)
                shutil.copytree(source, destination)
            else:
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(source, destination)

        active_snapshot_text = normalize_repo_and_worktree_paths(
            active_snapshot.selection_path.read_text(encoding="utf-8"), self.repo_root
        )
        active_snapshot_text = self._rewrite_round_control_refs(
            active_snapshot_text, active_snapshot
        )
        active_snapshot_text = self._upsert_selection_provenance(
            active_snapshot_text, active_snapshot
        )
        write_text(active_snapshot.selection_path, active_snapshot_text)
        self._write_round_snapshot(active_snapshot)

    def _active_snapshot(self, snapshots: Iterable[Snapshot]) -> Snapshot | None:
        active: Snapshot | None = None
        for snapshot in snapshots:
            if snapshot.source_kind == "active":
                active = snapshot
        return active


if __name__ == "__main__":
    sys.exit(main())
