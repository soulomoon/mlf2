module MLF.Frontend.Program.Run
  ( Value (..),
    runProgram,
    prettyValue,
  )
where

import MLF.Elab.Pipeline (ElabTerm (..), Pretty (..), Ty (..), freeTypeVarsType, normalize, schemeFromType, typeCheck)
import MLF.Frontend.Program.Check (checkProgram)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ProgramError (..),
  )
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Frontend.Syntax.Program as ProgramSyntax

data Value
  = VLit Lit
  | VTerm ElabTerm
  deriving (Eq, Show)

runProgram :: ProgramSyntax.Program -> Either ProgramError Value
runProgram program = do
  checked <- checkProgram program
  pure (toValue (normalizeProgramTerm (programMainTerm checked)))

programMainTerm :: CheckedProgram -> ElabTerm
programMainTerm checked =
  foldr bindAll (EVar (checkedProgramMain checked)) allBindings
  where
    allBindings =
      [ binding
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]

    bindAll binding body =
      ELet
        (checkedBindingName binding)
        (schemeFromType (checkedBindingType binding))
        (checkedBindingTerm binding)
        body

normalizeProgramTerm :: ElabTerm -> ElabTerm
normalizeProgramTerm term =
  let termNorm = normalize term
      termSimplified = case termNorm of
        ELet v _ rhs (EVar bodyV)
          | v == bodyV -> rhs
        _ -> termNorm
      termUnderTyAbs =
        case termSimplified of
          ETyAbs v mbBound body ->
            let body' = normalizeProgramTerm body
                rebuilt = ETyAbs v mbBound body'
             in case typeCheck rebuilt of
                  Right (TForall _ _ bodyTy)
                    | v `notElem` freeTypeVarsType bodyTy -> body'
                  _ -> rebuilt
          _ -> termSimplified
      termStripped = stripUnusedTopTyAbs termUnderTyAbs
   in if termStripped == term
        then termStripped
        else normalizeProgramTerm termStripped

stripUnusedTopTyAbs :: ElabTerm -> ElabTerm
stripUnusedTopTyAbs term = case term of
  ETyAbs v mbBound body ->
    let body' = stripUnusedTopTyAbs body
        term' = ETyAbs v mbBound body'
     in case typeCheck term' of
          Right (TForall _ _ bodyTy)
            | v `notElem` freeTypeVarsType bodyTy -> body'
          _ -> term'
  ELam v ty body -> ELam v ty (stripUnusedTopTyAbs body)
  EApp f a -> EApp (stripUnusedTopTyAbs f) (stripUnusedTopTyAbs a)
  ELet v sch rhs body -> ELet v sch (stripUnusedTopTyAbs rhs) (stripUnusedTopTyAbs body)
  ETyInst e inst -> ETyInst (stripUnusedTopTyAbs e) inst
  ERoll ty body -> ERoll ty (stripUnusedTopTyAbs body)
  EUnroll body -> EUnroll (stripUnusedTopTyAbs body)
  _ -> term

toValue :: ElabTerm -> Value
toValue term = case term of
  ELit lit -> VLit lit
  other -> VTerm other

prettyValue :: Value -> String
prettyValue value = case value of
  VLit (LInt i) -> show i
  VLit (LBool b) -> if b then "true" else "false"
  VLit (LString s) -> show s
  VTerm term -> pretty term
