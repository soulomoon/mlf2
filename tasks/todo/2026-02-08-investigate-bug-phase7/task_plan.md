# Task Plan: Investigate Phase 7 TCLetTypeMismatch for `make`

## Goal
Trace why `make`'s let-scheme specializes to `... -> Int` while its elaborated RHS remains polymorphic, document likely root causes and fix approaches, and recommend a path forward.

## Current Phase
Phase 3

## Phases

### Phase 1: Requirements & Discovery
- [x] Understand user intent
- [x] Identify constraints and requirements
- [x] Document findings in findings.md
- **Status:** complete

### Phase 2: Planning & Structure
- [x] Define technical approach
- [x] Create project structure if needed
- [x] Document decisions with rationale
- **Status:** complete

### Phase 3: Implementation
- [x] Execute the plan step by step
- [x] Write code to files before executing
- [x] Test incrementally
- **Status:** in_progress

### Phase 4: Testing & Verification
- [ ] Verify all requirements met
- [ ] Document test results in progress.md
- [ ] Fix any issues found
- **Status:** pending

### Phase 5: Delivery
- [ ] Review all output files
- [ ] Ensure deliverables are complete
- [ ] Deliver to user
- **Status:** pending

## Key Questions
1. Where does let-scheme for `make` acquire a `... -> Int` instantiation while elaborated RHS remains polymorphic?
2. How do GammaPlan/ReifyPlan interplay with generalization/elaboration to enforce scheme vs polymorphic types?

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Use strict systematic-debugging flow (reproduce → compare patterns → single-hypothesis tests) before any durable fix | Prevents guesswork and isolates causes in a multi-phase pipeline |
| Treat code edits as temporary hypothesis probes and revert unless they improve bug behavior | Keeps workspace stable and avoids compounded regressions |
| Use internal pipeline dumps (`solvedClean` vs `solvedForGen`, `gaSolvedToBase`, trace logs) as primary evidence | The failure emerges from cross-module data-flow, not a single local function |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Catchup script missing ${CLAUDE_PLUGIN_ROOT}/scripts/session-catchup.py | 1 | Directory absent; noted and continuing |
| `cabal exec runghc` intermittently hid package module `MLF.API` | 1 | Switched reproducible runs to `cabal repl lib:mlf2` + `:l script` |
| Internal dump scripts initially failed due hidden modules / missing imports | 1 | Executed via `cabal repl lib:mlf2-internal` and fixed imports |

## Notes
- Update phase status as you progress: pending → in_progress → complete
- Re-read this plan before major decisions to keep the goal top of mind
- Log ALL errors so we do not repeat the same action
