#!/usr/bin/env python3
"""Scaffold goal table + orchestrator prompt + round log artifacts."""

from __future__ import annotations

import argparse
import datetime as dt
import re
from pathlib import Path
from typing import Iterable


def slugify(text: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
    return slug or "goal"


def default_mechanisms() -> list[str]:
    return [
        "Mechanism 1",
        "Mechanism 2",
        "Mechanism 3",
        "Mechanism 4",
    ]


def ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def write_text(path: Path, text: str, overwrite: bool) -> str:
    existed = path.exists()
    if existed and not overwrite:
        return "skipped"
    ensure_parent(path)
    path.write_text(text, encoding="utf-8")
    return "updated" if existed else "created"


def render_table(date_str: str, goal: str, source: str, mechanisms: Iterable[str]) -> str:
    rows = "\n".join(
        f"| {m} | <What exists now> | <What must be true> | <Why current != target> | <spec>; <file>; <test> | NO | <next step> |"
        for m in mechanisms
    )
    return f"""# Goal Mechanism Table (Current vs Target)

Last updated (UTC): {date_str}
Goal: `{goal}`
Source of truth: `{source}`

| Mechanism | Current codebase behavior | Target behavior | Gap summary | Evidence (spec/code/tests) | Gate (YES/NO) | Next action |
|---|---|---|---|---|---|---|
{rows}

## Gate Rules

- Keep mechanism order fixed.
- Gate values must be exactly `YES` or `NO`.
- Change `NO` to `YES` only with passing verification evidence.
"""


def render_prompt(goal: str, source: str, table_path: Path, mechanisms: Iterable[str]) -> str:
    ordered_mechanisms = "\n".join(f"{i}. {m}" for i, m in enumerate(mechanisms, start=1))
    return f"""You are an autonomous workflow orchestrator.

Objective:
- Drive this repository toward `{goal}` by improving mechanism rows in `{table_path}`.

Required Inputs:
- Goal statement: `{goal}`
- Source of truth: `{source}`
- Mechanism order:
{ordered_mechanisms}
- Status table: `{table_path}`

Hard constraints:
- Work in this repository only.
- Max planning rounds: 10.
- Max implementation attempts per round: 6.
- Every gate value must be exactly `YES` or `NO`.
- Final statuses are exactly one of: `COMPLETED`, `FAILED`, `MAXIMUMRETRY`.

Roles:
- Orchestrator (coordination only)
- Verifier
- Planner
- Implementer
- Reviewer
- QA
- Integrator

Algorithm:
1. Verifier sweep across mechanisms in fixed order.
2. If all rows are YES -> stop with `COMPLETED`.
3. Select first NO row as the target mechanism.
4. Planner creates a file-level plan with binary acceptance criteria.
5. Attempt loop (1..6): implement -> review gate -> QA gate -> verifier gate.
6. If all gates are YES, continue to next round.
7. If attempt 6 fails, stop with `MAXIMUMRETRY`.
8. If round 10 ends without completion, stop with `FAILED`.

Output requirements:
- Log round, target mechanism, attempt, and reasons for NO gates.
- Print exactly one final line:
  - `FINAL STATUS: COMPLETED`
  - `FINAL STATUS: FAILED`
  - `FINAL STATUS: MAXIMUMRETRY`
"""


def render_round_log(date_str: str, goal: str, prompt_path: Path, table_path: Path) -> str:
    return f"""# Orchestrated Execution Log Template (Fresh Round 1)

Date (UTC): {date_str}
Goal: `{goal}`
Prompt template: `{prompt_path}`
Table: `{table_path}`

| Round | Selected mechanism | Attempt | Reviewer | QA | Verifier | Decision | Notes |
|---|---|---|---|---|---|---|---|
| 1 | <first NO mechanism> | 1 | NO | YES | NO | REPLAN | <blocking finding> |

## Final Status

`FINAL STATUS: <COMPLETED|FAILED|MAXIMUMRETRY>`
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Scaffold goal-loop docs artifacts.")
    parser.add_argument("--goal", required=True, help="Goal statement.")
    parser.add_argument(
        "--date",
        default=dt.datetime.now(dt.UTC).strftime("%Y-%m-%d"),
        help="UTC date (YYYY-MM-DD). Defaults to current UTC date.",
    )
    parser.add_argument(
        "--source",
        default="<PRIMARY_SOURCE_DOCS>",
        help="Source-of-truth label used in generated docs.",
    )
    parser.add_argument(
        "--mechanism",
        action="append",
        default=[],
        help="Ordered mechanism entry. Repeat to provide multiple entries.",
    )
    parser.add_argument("--notes-dir", default="docs/notes", help="Notes directory.")
    parser.add_argument("--prompts-dir", default="docs/prompts", help="Prompts directory.")
    parser.add_argument("--table-path", help="Explicit table output path.")
    parser.add_argument("--prompt-path", help="Explicit orchestrator prompt output path.")
    parser.add_argument("--round-path", help="Explicit round-log output path.")
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite existing files (default skips existing files).",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    goal_slug = slugify(args.goal)
    mechanisms = args.mechanism or default_mechanisms()

    notes_dir = Path(args.notes_dir)
    prompts_dir = Path(args.prompts_dir)

    table_path = Path(args.table_path) if args.table_path else notes_dir / f"{args.date}-{goal_slug}-mechanism-table.md"
    prompt_path = Path(args.prompt_path) if args.prompt_path else prompts_dir / f"{goal_slug}-improving-loop-agent.prompt.md"
    round_path = (
        Path(args.round_path)
        if args.round_path
        else prompts_dir / f"{args.date}-orchestrated-execution-{goal_slug}-codex-subagents-fresh-round-1.md"
    )

    table_state = write_text(table_path, render_table(args.date, args.goal, args.source, mechanisms), args.overwrite)
    prompt_state = write_text(
        prompt_path,
        render_prompt(args.goal, args.source, table_path, mechanisms),
        args.overwrite,
    )
    round_state = write_text(
        round_path,
        render_round_log(args.date, args.goal, prompt_path, table_path),
        args.overwrite,
    )

    print(f"table: {table_state} -> {table_path}")
    print(f"prompt: {prompt_state} -> {prompt_path}")
    print(f"round: {round_state} -> {round_path}")


if __name__ == "__main__":
    main()
