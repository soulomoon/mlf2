module MLF.Frontend.Program.Checked
  ( CheckedProgram,
    checkedProgramModules,
    checkedProgramMainResolvedVar,
    checkedProgramResolved,
    checkedProgramMain,
    mkCheckedProgram,
    mapCheckedProgramModules,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import MLF.Types.Elab
  ( ElabType,
    ElabTypeIdentityGap,
    elabTypeIdentityGaps,
    resolvedVarRuntimeName,
    xmlfTermTypeIdentityGaps,
  )
import MLF.Frontend.Program.Checked.Internal
import MLF.Frontend.Program.Types

checkedProgramModules :: CheckedProgram -> [CheckedModule]
checkedProgramModules = checkedProgramModulesInternal

checkedProgramMainResolvedVar :: CheckedProgram -> ResolvedVar
checkedProgramMainResolvedVar = checkedProgramMainResolvedVarInternal

checkedProgramResolved :: CheckedProgram -> ResolvedProgram
checkedProgramResolved = checkedProgramResolvedInternal

mkCheckedProgram :: ResolvedProgram -> [CheckedModule] -> ResolvedVar -> Either ProgramError CheckedProgram
mkCheckedProgram resolved modules mainResolved = do
  let checked =
        CheckedProgram
          { checkedProgramModulesInternal = modules,
            checkedProgramMainResolvedVarInternal = mainResolved,
            checkedProgramResolvedInternal = resolved
          }
  validateCheckedProgramIdentityPayloads checked
  pure checked

mapCheckedProgramModules :: ([CheckedModule] -> [CheckedModule]) -> CheckedProgram -> Either ProgramError CheckedProgram
mapCheckedProgramModules f checked =
  mkCheckedProgram
    (checkedProgramResolved checked)
    (f (checkedProgramModules checked))
    (checkedProgramMainResolvedVar checked)

checkedProgramMain :: CheckedProgram -> String
checkedProgramMain =
  resolvedVarRuntimeName . checkedProgramMainResolvedVar

validateCheckedProgramIdentityPayloads :: CheckedProgram -> Either ProgramError ()
validateCheckedProgramIdentityPayloads checked = do
  validateElabTypeIdentityPayload "program main reference" (resolvedVarType (checkedProgramMainResolvedVar checked))
  mapM_ validateCheckedModuleIdentityPayloads (checkedProgramModules checked)

validateCheckedModuleIdentityPayloads :: CheckedModule -> Either ProgramError ()
validateCheckedModuleIdentityPayloads checked = do
  mapM_ validateCheckedBindingIdentityPayloads (checkedModuleBindings checked)
  mapM_ validateDataInfoIdentityPayloads (Map.elems (checkedModuleData checked))
  mapM_ validateClassInfoIdentityPayloads (Map.elems (checkedModuleClasses checked))
  mapM_ validateInstanceInfoIdentityPayloads (checkedModuleInstances checked)
  validateModuleExportsIdentityPayloads (checkedModuleExports checked)

validateCheckedBindingIdentityPayloads :: CheckedBinding -> Either ProgramError ()
validateCheckedBindingIdentityPayloads binding = do
  validateTypeViewIdentityPayload owner (checkedBindingSourceTypeView binding)
  validateElabTypeIdentityPayload (owner ++ " reference") (resolvedVarType (checkedBindingResolvedVar binding))
  validateElabTypeIdentityPayload (owner ++ " result") (checkedBindingType binding)
  validateElabIdentityGaps (owner ++ " term") (xmlfTermTypeIdentityGaps (checkedBindingTerm binding))
  mapM_ validateDeferredObligationIdentityPayloads (Map.elems (checkedBindingDeferredObligations binding))
  where
    owner = "binding `" ++ checkedBindingName binding ++ "`"

validateConstraintInfoIdentityPayloads :: ConstraintInfo -> Either ProgramError ()
validateConstraintInfoIdentityPayloads constraint =
  mapM_
    (validateTypeViewIdentityPayload ("constraint `" ++ constraintDisplayClass constraint ++ "`"))
    (NE.toList (constraintTypeViews constraint))

validateConstructorInfoIdentityPayloads :: ConstructorInfo -> Either ProgramError ()
validateConstructorInfoIdentityPayloads ctor = do
  validateTypeViewIdentityPayload ("constructor `" ++ ctorName ctor ++ "`") (ctorTypeView ctor)
  mapM_
    (validateTypeViewIdentityPayload ("constructor owner shape `" ++ ctorName ctor ++ "`") . constructorShapeTypeView)
    (constructorOwnerShapes ctor)

validateDataInfoIdentityPayloads :: DataInfo -> Either ProgramError ()
validateDataInfoIdentityPayloads =
  mapM_ validateConstructorInfoIdentityPayloads . dataConstructors

validateMethodInfoIdentityPayloads :: MethodInfo -> Either ProgramError ()
validateMethodInfoIdentityPayloads method = do
  validateTypeViewIdentityPayload ("method `" ++ methodName method ++ "`") (methodTypeViewRaw method)
  mapM_ validateConstraintInfoIdentityPayloads (methodConstraintInfos method)

validateClassInfoIdentityPayloads :: ClassInfo -> Either ProgramError ()
validateClassInfoIdentityPayloads classInfo = do
  mapM_ validateConstraintInfoIdentityPayloads (classSuperclassInfos classInfo)
  mapM_ validateMethodInfoIdentityPayloads (Map.elems (classMethodsByIdentity classInfo))

validateValueInfoIdentityPayloads :: ValueInfo -> Either ProgramError ()
validateValueInfoIdentityPayloads valueInfo =
  case valueInfo of
    OrdinaryValue {valueConstraintInfos = constraints} -> do
      validateTypeViewIdentityPayload ("value `" ++ valueInfoRuntimeName valueInfo ++ "`") (ordinaryValueTypeView valueInfo)
      mapM_ validateConstraintInfoIdentityPayloads constraints
    ConstructorValue {valueCtorInfo = ctor} ->
      validateConstructorInfoIdentityPayloads ctor
    OverloadedMethod {valueMethodInfo = method} ->
      validateMethodInfoIdentityPayloads method

validateInstanceInfoIdentityPayloads :: InstanceInfo -> Either ProgramError ()
validateInstanceInfoIdentityPayloads instanceInfo = do
  mapM_
    (validateTypeViewIdentityPayload ("instance head `" ++ instanceClassName instanceInfo ++ "`"))
    (NE.toList (instanceHeadTypeViews instanceInfo))
  mapM_ validateConstraintInfoIdentityPayloads (instanceConstraintInfos instanceInfo)
  mapM_ validateValueInfoIdentityPayloads (Map.elems (instanceMethodsByIdentity instanceInfo))

validateEvidenceMethodIdentityPayloads :: String -> EvidenceMethod -> Either ProgramError ()
validateEvidenceMethodIdentityPayloads owner method = do
  validateTypeViewIdentityPayload owner (evidenceMethodTypeView method)
  mapM_ (validateElabTypeIdentityPayload owner . resolvedVarType) (evidenceMethodResolvedVar method)

validateEvidenceInfoIdentityPayloads :: EvidenceInfo -> Either ProgramError ()
validateEvidenceInfoIdentityPayloads evidence = do
  mapM_ (validateTypeViewIdentityPayload "evidence class argument") (NE.toList (evidenceTypeViews evidence))
  mapM_ (validateEvidenceMethodIdentityPayloads "evidence method") (Map.elems (evidenceMethodsByIdentity evidence))

validateDeferredMethodEvidenceIdentityPayloads :: DeferredMethodEvidence -> Either ProgramError ()
validateDeferredMethodEvidenceIdentityPayloads evidence = do
  mapM_ (validateTypeViewIdentityPayload "deferred method class argument") (NE.toList (deferredMethodEvidenceClassArgs evidence))
  validateEvidenceMethodIdentityPayloads "deferred evidence method" (deferredMethodEvidenceMethod evidence)

validateDeferredObligationIdentityPayloads :: DeferredProgramObligation -> Either ProgramError ()
validateDeferredObligationIdentityPayloads obligation =
  case obligation of
    DeferredMethod deferred -> do
      validateMethodInfoIdentityPayloads (deferredMethodInfo deferred)
      mapM_ (validateTypeViewIdentityPayload "deferred method result") (deferredMethodExpectedResult deferred)
      mapM_ validateDeferredMethodEvidenceIdentityPayloads (deferredMethodEvidence deferred)
      mapM_ validateEvidenceInfoIdentityPayloads (deferredMethodLocalEvidence deferred)
    DeferredConstructor deferred -> do
      validateConstructorInfoIdentityPayloads (deferredConstructorInfo deferred)
      validateTypeViewIdentityPayload "deferred constructor source" (deferredConstructorSourceTypeView deferred)
      validateTypeViewIdentityPayload "deferred constructor occurrence" (deferredConstructorOccurrenceTypeView deferred)
      mapM_ (validateTypeViewIdentityPayload "deferred constructor substitution") (typeBinderSubstViews (deferredConstructorInitialSubst deferred))
    DeferredCase deferred -> do
      validateDataInfoIdentityPayloads (deferredCaseDataInfo deferred)
      validateTypeViewIdentityPayload "deferred case scrutinee" (deferredCaseScrutineeTypeView deferred)
      validateTypeViewIdentityPayload "deferred case result" (deferredCaseResultTypeView deferred)

validateModuleExportsIdentityPayloads :: ModuleExports -> Either ProgramError ()
validateModuleExportsIdentityPayloads exports = do
  mapM_ validateValueInfoIdentityPayloads (Map.elems (exportedValuesByIdentity exports))
  mapM_ validateExportedTypeInfoIdentityPayloads (Map.elems (exportedTypesByIdentity exports))
  mapM_ validateClassInfoIdentityPayloads (Map.elems (exportedClassesByIdentity exports))
  where
    validateExportedTypeInfoIdentityPayloads info = do
      validateDataInfoIdentityPayloads (exportedTypeData info)
      mapM_ validateConstructorInfoIdentityPayloads (Map.elems (exportedTypeConstructorsByIdentity info))

validateTypeViewIdentityPayload :: String -> TypeView -> Either ProgramError ()
validateTypeViewIdentityPayload owner view =
  case typeViewIdentityGaps view of
    [] -> pure ()
    gaps ->
      Left
        ( ProgramPipelineError
            ("checked TypeView for " ++ owner ++ " is identity-incomplete: " ++ show gaps)
        )

validateElabTypeIdentityPayload :: String -> ElabType -> Either ProgramError ()
validateElabTypeIdentityPayload owner =
  validateElabIdentityGaps owner . elabTypeIdentityGaps

validateElabIdentityGaps :: String -> [ElabTypeIdentityGap] -> Either ProgramError ()
validateElabIdentityGaps _ [] =
  pure ()
validateElabIdentityGaps owner gaps =
  Left
    ( ProgramPipelineError
        ("checked ElabType for " ++ owner ++ " is identity-incomplete: " ++ show gaps)
    )
