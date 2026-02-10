#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="$ROOT_DIR/tmp/direction-matrix/$(date +%Y%m%d-%H%M%S)"
mkdir -p "$OUT_DIR"

cat > /tmp/repro-bug-2026-02-06-002.hs <<'REPRO_EOF'
import qualified Data.Set as Set
import MLF.API

expr :: NormSurfaceExpr
expr =
  ELet "make" (ELam "x" (ELam "y" (EVar "x")))
    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
      (EApp (EVar "c1") (ELit (LBool True))))

main :: IO ()
main = do
  putStrLn "REPRO|baseline"
  print (runPipelineElab Set.empty expr)
  print (runPipelineElabChecked Set.empty expr)
REPRO_EOF

cat > /tmp/repro-bug-2026-02-06-002-trace.hs <<'TRACE_EOF'
import qualified Data.Set as Set
import MLF.API

expr :: NormSurfaceExpr
expr =
  ELet "make" (ELam "x" (ELam "y" (EVar "x")))
    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
      (EApp (EVar "c1") (ELit (LBool True))))

cfg :: PipelineConfig
cfg =
  defaultPipelineConfig
    { pcTraceConfig =
        defaultTraceConfig
          { tcBinding = True
          , tcPresolution = True
          , tcSolve = True
          , tcElab = True
          , tcNormalize = True
          , tcGeneralize = True
          }
    }

main :: IO ()
main = do
  putStrLn "TRACE|baseline"
  print (runPipelineElabWithConfig cfg Set.empty expr)
  print (runPipelineElabCheckedWithConfig cfg Set.empty expr)
TRACE_EOF

cabal repl lib:mlf2 <<TRACE_END > "$OUT_DIR/repro.out" 2>&1
:l $ROOT_DIR/scripts/bug-2026-02-06-002-diagnostics.hs
:main
:l /tmp/repro-bug-2026-02-06-002.hs
:main
:q
TRACE_END

cabal repl lib:mlf2 <<TRACE_END > "$OUT_DIR/trace.out" 2>&1
:l $ROOT_DIR/scripts/bug-2026-02-06-002-diagnostics.hs
:main --trace
:l /tmp/repro-bug-2026-02-06-002-trace.hs
:main
:q
TRACE_END

{
  cabal test mlf2-test --test-show-details=direct --test-options='--match="BUG-2026-02-06-002 thesis target"'
  cabal test mlf2-test --test-show-details=direct --test-options='--match="generalizes reused constructors via make const"'
  cabal test mlf2-test --test-show-details=direct --test-options='--match="redirected let-use sites keep polymorphic schemes"'
} > "$OUT_DIR/tests.out" 2>&1 || true

echo "$OUT_DIR"
