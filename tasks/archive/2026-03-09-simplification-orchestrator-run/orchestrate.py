#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import sys
import textwrap
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

REPO_ROOT = Path('/Volumes/src/mlf4')
TASK_DIR = REPO_ROOT / 'tasks' / 'todo' / '2026-03-09-simplification-orchestrator-run'
SCHEMA_DIR = TASK_DIR / 'schemas'
TMP_DIR = TASK_DIR / 'tmp'
LOG_PATH = TASK_DIR / 'orchestrator-log.jsonl'
PROGRESS_PATH = TASK_DIR / 'progress.md'
FINDINGS_PATH = TASK_DIR / 'findings.md'
TASK_PLAN_PATH = TASK_DIR / 'task_plan.md'
WORKTREE_BASE = Path.home() / '.config' / 'superpowers' / 'worktrees' / 'mlf4'
BASE_BRANCH = 'master'
BASE_COMMIT = subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=REPO_ROOT, capture_output=True, text=True, check=True).stdout.strip()
TMP_DIR.mkdir(parents=True, exist_ok=True)
WORKTREE_BASE.mkdir(parents=True, exist_ok=True)


class OrchestrationError(RuntimeError):
    pass


@dataclass
class RoundSummary:
    round: int
    accepted_idea: str
    verifier_initial_gate: str
    planner_summary: str
    attempt_count: int
    qa_final_gate: str
    verifier_final_gate: str
    commit_hash: str
    merge_result: str
    next_candidate_area: str = 'n/a'


def now_utc() -> str:
    return datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')


def run(cmd: list[str], cwd: Path = REPO_ROOT, check: bool = True, input_text: str | None = None) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        cmd,
        cwd=str(cwd),
        input=input_text,
        text=True,
        capture_output=True,
    )
    if check and result.returncode != 0:
        raise OrchestrationError(
            f'Command failed ({result.returncode}): {cmd}\nSTDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}'
        )
    return result


def shell(cmd: str, cwd: Path = REPO_ROOT, check: bool = True) -> str:
    result = subprocess.run(cmd, cwd=str(cwd), shell=True, text=True, capture_output=True)
    if check and result.returncode != 0:
        raise OrchestrationError(
            f'Shell failed ({result.returncode}): {cmd}\nSTDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}'
        )
    return result.stdout.strip()


def append_progress(message: str) -> None:
    with PROGRESS_PATH.open('a', encoding='utf-8') as handle:
        handle.write(f'- {now_utc()} {message}\n')


def append_findings(message: str) -> None:
    with FINDINGS_PATH.open('a', encoding='utf-8') as handle:
        handle.write(f'- {message}\n')


def update_task_plan_phase(phase1: str, phase2: str, phase3: str) -> None:
    text = TASK_PLAN_PATH.read_text(encoding='utf-8')
    text = re.sub(r'1\\. Initialize orchestration artifacts, agent prompts, and round ledger\\. - .*',
                  f'1. Initialize orchestration artifacts, agent prompts, and round ledger. - {phase1}', text)
    text = re.sub(r'2\\. Run rounds 1-10 with per-round idea, plan, implementation, QA, verifier, and integration gates\\. - .*',
                  f'2. Run rounds 1-10 with per-round idea, plan, implementation, QA, verifier, and integration gates. - {phase2}', text)
    text = re.sub(r'3\\. Produce final 10-row summary and archive-ready notes\\. - .*',
                  f'3. Produce final 10-row summary and archive-ready notes. - {phase3}', text)
    TASK_PLAN_PATH.write_text(text, encoding='utf-8')


def record_error(error: str, attempt: str, resolution: str) -> None:
    text = TASK_PLAN_PATH.read_text(encoding='utf-8')
    insertion = f'| {error} | {attempt} | {resolution} |\n'
    marker = '|------|---------|------------|\n'
    if insertion not in text:
        text = text.replace(marker, marker + insertion)
        TASK_PLAN_PATH.write_text(text, encoding='utf-8')


def log_event(round_no: int, phase: str, agent: str, idea_title: str, attempt: int, gate: str,
              reason: str, files_touched: list[str] | None = None, commands_run: list[str] | None = None,
              commit_hash: str = '') -> None:
    event = {
        'round': round_no,
        'phase': phase,
        'agent': agent,
        'idea_title': idea_title,
        'attempt': attempt,
        'gate': gate,
        'reason': reason,
        'files_touched': files_touched or [],
        'commands_run': commands_run or [],
        'commit_hash': commit_hash,
    }
    with LOG_PATH.open('a', encoding='utf-8') as handle:
        json.dump(event, handle, ensure_ascii=False)
        handle.write('\n')


def slugify(text: str) -> str:
    slug = re.sub(r'[^a-z0-9]+', '-', text.lower()).strip('-')
    return (slug or 'item')[:48]


def json_pretty(data: Any) -> str:
    return json.dumps(data, indent=2, ensure_ascii=False)


def codex_json(cwd: Path, schema_name: str, prompt: str) -> dict[str, Any]:
    schema = SCHEMA_DIR / schema_name
    out_file = TMP_DIR / f'{uuid.uuid4()}.json'
    cmd = [
        'codex', 'exec',
        '--dangerously-bypass-approvals-and-sandbox',
        '--cd', str(cwd),
        '--output-schema', str(schema),
        '-o', str(out_file),
        '--color', 'never',
        '--ephemeral',
        '-'
    ]
    result = run(cmd, cwd=REPO_ROOT, check=False, input_text=prompt)
    if result.returncode != 0:
        raise OrchestrationError(
            f'codex exec failed in {cwd}:\nPROMPT:\n{prompt}\nSTDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}'
        )
    try:
        return json.loads(out_file.read_text(encoding='utf-8'))
    finally:
        out_file.unlink(missing_ok=True)


def thinker_prompt(round_no: int, rejected: list[dict[str, str]], prior: list[RoundSummary]) -> str:
    rejected_text = json_pretty(rejected)
    prior_titles = [summary.accepted_idea for summary in prior if summary.accepted_idea != 'SKIPPED']
    return textwrap.dedent(f'''
    You are the Thinker agent for round {round_no} of a thesis-exact simplification campaign in /Volumes/src/mlf4.

    Mission:
    - Propose exactly one bounded simplification that is still needed in the current repository.
    - Preserve thesis-exact behavior from `papers/these-finale-english.txt`.
    - Use `papers/xmlf.txt` only if the thesis is silent.
    - Re-check the current repo state, code, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and `Bugs.md`.
    - Do not repeat already rejected ideas for this round or already accepted round themes.

    Already accepted titles in prior rounds:
    {json_pretty(prior_titles)}

    Rejected ideas this round:
    {rejected_text}

    Return only the schema fields. Keep the proposal high-value, bounded, and thesis-safe.
    ''').strip()


def verifier_initial_prompt(round_no: int, idea: dict[str, Any]) -> str:
    return textwrap.dedent(f'''
    You are the Verifier agent for round {round_no} in /Volumes/src/mlf4.

    Decide whether this proposed simplification is still needed right now.
    Use the primary thesis source `papers/these-finale-english.txt`, consult `papers/xmlf.txt` only if silent,
    and inspect the current repository plus `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and `Bugs.md`.

    Proposed idea:
    {json_pretty(idea)}

    Return only the schema fields. Set `gate` to `YES` only if the simplification is still needed, bounded,
    worthwhile, and thesis-safe.
    ''').strip()


def planner_plan_prompt(round_no: int, idea: dict[str, Any]) -> str:
    return textwrap.dedent(f'''
    You are the Planner agent for round {round_no} in /Volumes/src/mlf4.

    Produce a thesis-exact implementation plan for this accepted simplification idea.
    Requirements:
    - Primary source of truth: `papers/these-finale-english.txt`.
    - Use `papers/xmlf.txt` only if the thesis is silent.
    - Re-check current code and docs.
    - Keep the plan minimal, local, and warning-free.
    - Include exact files, concrete validation commands, acceptance/abort criteria, and a precise good-enough definition.
    - Prefer test-first slices when behavior protection is relevant.
    - Respect AGENTS guidance: update tests and docs in the same change when behavior/architecture expectations move.

    Accepted idea:
    {json_pretty(idea)}

    Return only the schema fields.
    ''').strip()


def worktree_path(round_no: int, idea_title: str) -> Path:
    return WORKTREE_BASE / f'orchestrator-round-{round_no:02d}-{slugify(idea_title)}'


def prepare_detached_worktree(round_no: int, idea_title: str) -> Path:
    path = worktree_path(round_no, idea_title)
    if path.exists():
        try:
            run(['git', 'worktree', 'remove', '--force', str(path)], cwd=REPO_ROOT, check=False)
        except Exception:
            pass
        shutil.rmtree(path, ignore_errors=True)
    run(['git', 'worktree', 'add', '--detach', str(path), BASE_BRANCH], cwd=REPO_ROOT)
    append_progress(f'Round {round_no}: created detached worktree at `{path}` for `{idea_title}`.')
    return path


def implementer_prompt(round_no: int, attempt: int, idea: dict[str, Any], plan: dict[str, Any], cwd: Path) -> str:
    return textwrap.dedent(f'''
    You are the Implementer agent for round {round_no}, attempt {attempt}, working only in `{cwd}`.

    Execute the current plan and nothing else.
    Constraints:
    - Preserve thesis-exact behavior from `papers/these-finale-english.txt`.
    - Use `papers/xmlf.txt` only if the thesis is silent.
    - Do not expand scope.
    - Do not commit.
    - Follow TDD discipline when the change needs behavior protection: add or update the narrowest failing regression first, verify it fails, then implement the minimal fix/refactor and rerun targeted validation.
    - Update tests/docs (`CHANGELOG.md`, `implementation_notes.md`) when the plan requires them.
    - Respect existing workspace changes outside your worktree.

    Idea:
    {json_pretty(idea)}

    Current plan:
    {json_pretty(plan)}

    Return only the schema fields after completing your implementation attempt.
    ''').strip()


def qa_prompt(round_no: int, attempt: int, idea: dict[str, Any], plan: dict[str, Any], cwd: Path) -> str:
    return textwrap.dedent(f'''
    You are the QA agent for round {round_no}, attempt {attempt}, working in `{cwd}`.

    Run the plan's required validation commands exactly as needed to assess this attempt. Use targeted commands first if listed,
    and include the full gate if listed. Return `gate=YES` only if every required command succeeds.

    Idea:
    {json_pretty(idea)}

    Plan tests and commands:
    {json_pretty(plan.get('tests_and_commands', []))}

    Return only the schema fields.
    ''').strip()


def planner_eval_prompt(round_no: int, attempt: int, idea: dict[str, Any], plan: dict[str, Any],
                        implementer_result: dict[str, Any], qa_result: dict[str, Any], cwd: Path) -> str:
    return textwrap.dedent(f'''
    You are the Planner agent for round {round_no}, attempt {attempt}, inspecting `{cwd}`.

    Decide whether the current implementation is good enough relative to the current plan.
    Inspect the current worktree diff and relevant files directly before answering.

    Idea:
    {json_pretty(idea)}

    Current plan:
    {json_pretty(plan)}

    Implementer report:
    {json_pretty(implementer_result)}

    QA report:
    {json_pretty(qa_result)}

    Return only the schema fields. Use `gate=YES` only if the implementation meets the plan's good-enough definition.
    ''').strip()


def planner_replan_prompt(round_no: int, attempt: int, idea: dict[str, Any], current_plan: dict[str, Any],
                          implementer_result: dict[str, Any], qa_result: dict[str, Any], planner_eval: dict[str, Any],
                          verifier_feedback: dict[str, Any] | None, repeated_blocker: bool, previous_delta: dict[str, Any] | None,
                          cwd: Path) -> str:
    return textwrap.dedent(f'''
    You are the Planner agent for round {round_no}, replanning after attempt {attempt}, inspecting `{cwd}`.

    Produce an updated full plan plus a `PLAN_DELTA` section.
    Requirements:
    - Preserve thesis-exact behavior from `papers/these-finale-english.txt`.
    - Use `papers/xmlf.txt` only if the thesis is silent.
    - Re-check the current worktree diff and failure signals directly.
    - The updated plan must materially differ from the previous plan when the prior strategy failed.
    - If there were two consecutive attempts with `feasible=NO`, `meaningful_diff=NO`, and the same blocker,
      you MUST either change strategy shape or declare the item blocked in the updated plan's abort criteria.

    Idea:
    {json_pretty(idea)}

    Current plan:
    {json_pretty(current_plan)}

    Planner evaluation:
    {json_pretty(planner_eval)}

    Implementer report:
    {json_pretty(implementer_result)}

    QA report:
    {json_pretty(qa_result)}

    Verifier feedback:
    {json_pretty(verifier_feedback)}

    Previous PLAN_DELTA:
    {json_pretty(previous_delta)}

    repeated_blocker={str(repeated_blocker)}

    Return only the schema fields.
    ''').strip()


def verifier_final_prompt(round_no: int, attempt: int, idea: dict[str, Any], plan: dict[str, Any],
                          qa_result: dict[str, Any], cwd: Path) -> str:
    return textwrap.dedent(f'''
    You are the Verifier agent for round {round_no}, final sanity gate after attempt {attempt}, inspecting `{cwd}`.

    Decide whether this simplification still preserves thesis-exactness and still looks worth keeping.
    Inspect the current worktree diff and relevant files directly.
    Use `papers/these-finale-english.txt` as primary source; use `papers/xmlf.txt` only if the thesis is silent.

    Idea:
    {json_pretty(idea)}

    Current plan:
    {json_pretty(plan)}

    QA report:
    {json_pretty(qa_result)}

    Return only the schema fields.
    ''').strip()


def integrator_prompt(round_no: int, idea: dict[str, Any], cwd: Path) -> str:
    slug = slugify(idea['title'])
    branch = f'codex/round-{round_no}-{slug}'
    commit_message = f'Simplify {idea["title"]}'
    return textwrap.dedent(f'''
    You are the Integrator agent for round {round_no}.

    Working setup:
    - Repo root: `{REPO_ROOT}`
    - Detached worktree with accepted changes: `{cwd}`
    - Base branch: `{BASE_BRANCH}`
    - Final branch to create: `{branch}`
    - Commit message: `{commit_message}`

    Required steps:
    1. In `{cwd}`, create branch `{branch}` from the current detached HEAD with the accepted uncommitted changes.
    2. Commit the accepted changes with the given clear commit message.
    3. In repo root `{REPO_ROOT}`, merge `{branch}` back into `{BASE_BRANCH}` locally.
    4. Report the feature commit hash and merge result.

    Constraints:
    - Do not edit or commit the orchestration task files under `tasks/todo/2026-03-09-simplification-orchestrator-run`.
    - Do not add unrelated changes.
    - Do not push.
    - Use the already-validated worktree state only.

    Return only the schema fields.
    ''').strip()


def cleanup_worktree(path: Path) -> None:
    if path.exists():
        run(['git', 'worktree', 'remove', '--force', str(path)], cwd=REPO_ROOT, check=False)
        shutil.rmtree(path, ignore_errors=True)


def git_default_branch() -> str:
    return BASE_BRANCH


def main() -> int:
    summaries: list[RoundSummary] = []
    update_task_plan_phase('complete', 'in_progress', 'pending')
    append_progress('Agent orchestration schemas and runner are ready; beginning round execution.')
    append_findings('## Per-Round Findings')

    for round_no in range(1, 11):
        append_progress(f'Round {round_no}: starting idea generation.')
        rejected: list[dict[str, str]] = []
        idea: dict[str, Any] | None = None
        initial_gate = 'NO'
        planner_summary = ''
        attempts_used = 0
        qa_final_gate = 'NO'
        verifier_final_gate = 'NO'
        commit_hash = ''
        merge_result = ''
        worktree: Path | None = None
        current_plan: dict[str, Any] | None = None
        previous_delta: dict[str, Any] | None = None
        previous_impl: dict[str, Any] | None = None
        round_blocked = False

        try:
            for proposal_idx in range(1, 6):
                idea = codex_json(REPO_ROOT, 'thinker-idea.schema.json', thinker_prompt(round_no, rejected, summaries))
                log_event(round_no, 'A', 'Thinker', idea['title'], 0, 'PROPOSED', idea['problem'], idea.get('likely_files', []), idea.get('required_validation', []), '')
                verification = codex_json(REPO_ROOT, 'verifier-gate.schema.json', verifier_initial_prompt(round_no, idea))
                initial_gate = verification['gate']
                log_event(round_no, 'A', 'Verifier', idea['title'], 0, verification['gate'], verification['reason'], idea.get('likely_files', []), [], '')
                if verification['gate'] == 'YES':
                    break
                rejected.append({'title': idea['title'], 'reason': verification['reason']})
                append_progress(f'Round {round_no}: rejected idea `{idea["title"]}` ({verification["reason"]}).')
            else:
                summaries.append(RoundSummary(round=round_no, accepted_idea='SKIPPED', verifier_initial_gate='NO', planner_summary='No idea survived 5 verifier rejections.', attempt_count=0, qa_final_gate='NO', verifier_final_gate='NO', commit_hash='-', merge_result='SKIPPED'))
                append_findings(f'### Round {round_no}\n- Status: SKIPPED after 5 rejected ideas.')
                append_progress(f'Round {round_no}: marked SKIPPED after 5 rejected ideas.')
                continue

            assert idea is not None
            current_plan = codex_json(REPO_ROOT, 'planner-plan.schema.json', planner_plan_prompt(round_no, idea))
            planner_summary = current_plan['problem_statement']
            log_event(round_no, 'B', 'Planner', idea['title'], 0, 'PLANNED', current_plan['problem_statement'], current_plan.get('exact_files_to_modify', []), current_plan.get('tests_and_commands', []), '')
            worktree = prepare_detached_worktree(round_no, idea['title'])

            for attempt in range(1, 11):
                attempts_used = attempt
                impl = codex_json(worktree, 'implementer-result.schema.json', implementer_prompt(round_no, attempt, idea, current_plan, worktree))
                log_event(round_no, 'C', 'Implementer', idea['title'], attempt, impl['feasible'], impl['change_summary'], impl.get('files_changed', []), [], '')

                qa = codex_json(worktree, 'qa-result.schema.json', qa_prompt(round_no, attempt, idea, current_plan, worktree))
                qa_final_gate = qa['gate']
                log_event(round_no, 'C', 'QA', idea['title'], attempt, qa['gate'], '; '.join(qa.get('failures_if_any', [])) or 'All required commands passed.', impl.get('files_changed', []), qa.get('commands_run', []), '')

                planner_eval = codex_json(worktree, 'planner-eval.schema.json', planner_eval_prompt(round_no, attempt, idea, current_plan, impl, qa, worktree))
                log_event(round_no, 'C', 'Planner', idea['title'], attempt, planner_eval['gate'], planner_eval['reason'], impl.get('files_changed', []), current_plan.get('tests_and_commands', []), '')

                if planner_eval['gate'] == 'YES' and qa['gate'] == 'YES':
                    verifier_final = codex_json(worktree, 'verifier-gate.schema.json', verifier_final_prompt(round_no, attempt, idea, current_plan, qa, worktree))
                    verifier_final_gate = verifier_final['gate']
                    log_event(round_no, 'D', 'Verifier', idea['title'], attempt, verifier_final['gate'], verifier_final['reason'], impl.get('files_changed', []), qa.get('commands_run', []), '')
                    if verifier_final['gate'] == 'YES':
                        integration = codex_json(REPO_ROOT, 'integrator-result.schema.json', integrator_prompt(round_no, idea, worktree))
                        commit_hash = integration['commit_hash']
                        merge_result = integration['merge_result']
                        log_event(round_no, 'E', 'Integrator', idea['title'], attempt, 'MERGED', merge_result, impl.get('files_changed', []), [], commit_hash)
                        summaries.append(RoundSummary(round=round_no, accepted_idea=idea['title'], verifier_initial_gate=initial_gate, planner_summary=planner_summary, attempt_count=attempts_used, qa_final_gate=qa_final_gate, verifier_final_gate=verifier_final_gate, commit_hash=commit_hash, merge_result=merge_result))
                        append_findings(textwrap.dedent(f'''\
                        ### Round {round_no}
                        - Accepted idea: `{idea['title']}`
                        - Planner summary: {planner_summary}
                        - Final gates: QA={qa_final_gate}, Verifier={verifier_final_gate}
                        - Commit: `{commit_hash}`
                        - Merge: {merge_result}
                        ''').strip())
                        append_progress(f'Round {round_no}: merged `{idea["title"]}` as `{commit_hash}` ({merge_result}).')
                        break

                    verifier_feedback = verifier_final
                else:
                    verifier_feedback = None

                repeated_blocker = False
                if previous_impl is not None:
                    repeated_blocker = (
                        previous_impl.get('feasible') == 'NO' and impl.get('feasible') == 'NO' and
                        previous_impl.get('meaningful_diff') == 'NO' and impl.get('meaningful_diff') == 'NO' and
                        previous_impl.get('blockers') == impl.get('blockers')
                    )

                replan = codex_json(
                    worktree,
                    'planner-replan.schema.json',
                    planner_replan_prompt(round_no, attempt, idea, current_plan, impl, qa, planner_eval, verifier_feedback, repeated_blocker, previous_delta, worktree)
                )
                log_event(round_no, 'C', 'Planner', idea['title'], attempt, 'REPLAN', replan['PLAN_DELTA']['what_failed'], replan.get('exact_files_to_modify', []), replan.get('tests_and_commands', []), '')

                if previous_delta and replan['PLAN_DELTA'] == previous_delta:
                    round_blocked = True
                    merge_result = 'BLOCKED: planner repeated prior PLAN_DELTA'
                    append_progress(f'Round {round_no}: blocked because planner repeated the prior PLAN_DELTA on attempt {attempt}.')
                    break

                current_plan = replan
                previous_delta = replan['PLAN_DELTA']
                previous_impl = impl

                blocked_for_round = any('blocked' in item.lower() for item in current_plan.get('abort_criteria', []))
                if repeated_blocker and blocked_for_round:
                    round_blocked = True
                    merge_result = 'BLOCKED: planner declared item blocked for this round'
                    append_progress(f'Round {round_no}: planner declared the item blocked after repeated blocker on attempt {attempt}.')
                    break
            else:
                merge_result = 'MAXIMUMRETRY'

            if not any(summary.round == round_no for summary in summaries):
                if worktree is not None:
                    cleanup_worktree(worktree)
                summaries.append(RoundSummary(round=round_no, accepted_idea=idea['title'], verifier_initial_gate=initial_gate, planner_summary=planner_summary or 'Accepted idea did not merge.', attempt_count=attempts_used, qa_final_gate=qa_final_gate, verifier_final_gate=verifier_final_gate, commit_hash=commit_hash or '-', merge_result=merge_result or ('BLOCKED' if round_blocked else 'MAXIMUMRETRY')))
                append_findings(textwrap.dedent(f'''\
                ### Round {round_no}
                - Accepted idea: `{idea['title']}`
                - Outcome: {merge_result or ('BLOCKED' if round_blocked else 'MAXIMUMRETRY')}
                - Attempts used: {attempts_used}
                - Final gates: QA={qa_final_gate}, Verifier={verifier_final_gate}
                ''').strip())
                outcome = merge_result or ('BLOCKED' if round_blocked else 'MAXIMUMRETRY')
                append_progress(f'Round {round_no}: ended without merge ({outcome}).')
        except Exception as exc:
            record_error(type(exc).__name__, f'round {round_no}', str(exc).splitlines()[0][:160])
            append_progress(f'Round {round_no}: orchestration error `{type(exc).__name__}`; marking round blocked.')
            log_event(round_no, 'ERROR', 'Orchestrator', idea['title'] if idea else '', attempts_used, 'ERROR', str(exc).splitlines()[0][:300], [], [], '')
            if worktree is not None:
                cleanup_worktree(worktree)
            summaries.append(RoundSummary(round=round_no, accepted_idea=idea['title'] if idea else 'SKIPPED', verifier_initial_gate=initial_gate, planner_summary=planner_summary or 'Orchestration error', attempt_count=attempts_used, qa_final_gate=qa_final_gate, verifier_final_gate=verifier_final_gate, commit_hash='-', merge_result='BLOCKED: orchestration error'))

    for index, summary in enumerate(summaries):
        if index + 1 < len(summaries):
            summary.next_candidate_area = summaries[index + 1].accepted_idea
        else:
            summary.next_candidate_area = 'n/a'

    update_task_plan_phase('complete', 'complete', 'in_progress')
    append_progress('All 10 rounds completed; compiling final summary.')

    summary_lines = []
    for summary in summaries:
        line = (
            f"{summary.round}. accepted_idea={summary.accepted_idea}; initial={summary.verifier_initial_gate}; "
            f"planner={summary.planner_summary}; attempts={summary.attempt_count}; qa={summary.qa_final_gate}; "
            f"verifier={summary.verifier_final_gate}; commit={summary.commit_hash}; merge={summary.merge_result}; "
            f"next={summary.next_candidate_area}"
        )
        summary_lines.append(line)

    append_findings('## Final Round Ledger')
    for line in summary_lines:
        append_findings(f'- {line}')

    update_task_plan_phase('complete', 'complete', 'complete')
    append_progress('Round ledger finalized; archiving task folder.')

    archive_dir = REPO_ROOT / 'tasks' / 'archive' / TASK_DIR.name
    if archive_dir.exists():
        shutil.rmtree(archive_dir)
    shutil.move(str(TASK_DIR), str(archive_dir))

    for line in summary_lines:
        print(line)
    print('FINAL STATUS: COMPLETED')
    return 0


if __name__ == '__main__':
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f'FINAL STATUS: FAILED: {exc}', file=sys.stderr)
        raise
