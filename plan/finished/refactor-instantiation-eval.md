# Refactor plan: unify instantiation evaluation (applyInstantiation + checkInstantiation)

## Goal
Reduce duplication between `MLF.Elab.Inst.applyInstantiation` and `MLF.Elab.TypeCheck.checkInstantiation` by introducing a shared evaluator for `Instantiation` that encapsulates the case tree and fresh-name threading.

## Scope
- `src/MLF/Elab/Inst.hs`
- `src/MLF/Elab/TypeCheck.hs`

## Proposed abstraction
Introduce a helper in `MLF.Elab.Inst` (or new `MLF.Elab.Inst.Eval`) with a core fold:

```
-- skeleton only
foldInstantiation
  :: (Int -> ElabType -> Instantiation -> Either e (Int, ElabType))
  -> Int
  -> ElabType
  -> Instantiation
  -> Either e (Int, ElabType)
```

or a record of handlers:

```
 data InstEval e = InstEval
   { onBot   :: Int -> ElabType -> ElabType -> Either e (Int, ElabType)
   , onAbstr :: Int -> ElabType -> String -> Either e (Int, ElabType)
   , onIntro :: Int -> ElabType -> Either e (Int, ElabType)
   , onElim  :: Int -> ElabType -> Either e (Int, ElabType)
   , onInside :: Int -> ElabType -> Instantiation -> Either e (Int, ElabType)
   , onUnder  :: Int -> ElabType -> String -> Instantiation -> Either e (Int, ElabType)
   }
```

`applyInstantiation` uses a simple `InstEval ElabError` instance; `checkInstantiation` uses a `InstEval TypeCheckError` instance that can consult the `Env` for `InstAbstr` and rebind under `InstUnder`.

## Steps
1. Extract the common case tree into a new helper, with callbacks for the typecheck-specific cases (`InstAbstr` and `InstUnder` env handling).
2. Rewrite `applyInstantiation` to delegate to the helper (keeping its error texts intact).
3. Rewrite `checkInstantiation` to delegate to the same helper, threading `Env` in the callback closures.
4. Keep `renameInstBound` in one place (see separate plan), and reuse it from both helpers.

## Risks
- Error messages may change; keep the same string content and constructor use.
- Ensure `InstUnder` uses the same alpha-renaming behavior as before.

## Verification
- Run `cabal test` (or at least `mlf2-test` focusing on elaboration/typecheck specs if a full run is heavy).
