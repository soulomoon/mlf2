{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Check.Cache.Key
    ( BuiltinPreludeCacheKey
    , builtinPreludeCacheKey
    )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

import MLF.Frontend.Program.Types
    ( ResolvedLocalSymbols (..)
    , ResolvedScope (..)
    , ResolvedSemanticModule (..)
    )
import MLF.Frontend.Symbol
    ( ResolvedSymbol
    , SymbolIdentityPayloadKey
    , SymbolSpelling
    , resolvedSymbolIdentity
    , resolvedSymbolSpelling
    , symbolIdentityPayloadKey
    )
import MLF.Frontend.Syntax
    ( Lit (..)
    , ResolvedSrcBound (..)
    , ResolvedSrcTy (..)
    , ResolvedTypeBinderRef
    , SrcKind (..)
    , TypeParam (..)
    , resolvedTypeBinderIdentity
    , resolvedTypeBinderName
    )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Frontend.TypeLevel (TypeFamilyDecl)
import MLF.Types.Identity
    ( LocalIdentity
    , LocalRef
    , TypeBinderIdentity
    , localRefDiscard
    , localRefIdentity
    , localRefName
    )

-- The Prelude checker observes both resolved identity and source/display
-- spelling. Keep those dimensions in one ordered, collision-free key instead
-- of routing through a hash or a derived rendering of the whole module.
data BuiltinPreludeCacheKey = BuiltinPreludeCacheKey
    String
    SymbolIdentityPayloadKey
    PreludeModuleSyntaxKey
    PreludeLocalSymbolsKey
    PreludeScopeKey
    PreludeScopeKey
    deriving (Eq, Ord)

data PreludeModuleSyntaxKey = PreludeModuleSyntaxKey
    String
    (Maybe [PreludeExportItemKey])
    [PreludeImportKey]
    [PreludeDeclKey]
    deriving (Eq, Ord)

data PreludeExportItemKey
    = PreludeExportValue PreludeSymbolKey
    | PreludeExportType PreludeExportTypeKey
    | PreludeExportTypeWithConstructors PreludeExportTypeKey
    deriving (Eq, Ord)

data PreludeExportTypeKey = PreludeExportTypeKey
    String
    [PreludeSymbolKey]
    deriving (Eq, Ord)

data PreludeImportKey = PreludeImportKey
    PreludeSymbolKey
    (Maybe String)
    (Maybe [PreludeExportItemKey])
    deriving (Eq, Ord)

data PreludeDeclKey
    = PreludeClassDecl PreludeClassDeclKey
    | PreludeInstanceDecl PreludeInstanceDeclKey
    | PreludeDataDecl PreludeDataDeclKey
    | PreludeTypeFamilyDecl TypeFamilyDecl
    | PreludeDefDecl PreludeDefDeclKey
    deriving (Eq, Ord)

data PreludeClassDeclKey = PreludeClassDeclKey
    PreludeSymbolKey
    [PreludeClassConstraintKey]
    [PreludeTypeParamKey]
    [PreludeFunctionalDependencyKey]
    [PreludeMethodSigKey]
    deriving (Eq, Ord)

data PreludeMethodSigKey = PreludeMethodSigKey
    PreludeSymbolKey
    PreludeConstrainedTypeKey
    deriving (Eq, Ord)

data PreludeInstanceDeclKey = PreludeInstanceDeclKey
    [PreludeClassConstraintKey]
    PreludeSymbolKey
    [PreludeSrcTypeKey]
    [PreludeMethodDefKey]
    deriving (Eq, Ord)

data PreludeMethodDefKey = PreludeMethodDefKey
    PreludeSymbolKey
    PreludeExprKey
    deriving (Eq, Ord)

data PreludeDataDeclKey = PreludeDataDeclKey
    PreludeSymbolKey
    [PreludeTypeParamKey]
    [PreludeConstructorDeclKey]
    [PreludeSymbolKey]
    deriving (Eq, Ord)

data PreludeConstructorDeclKey = PreludeConstructorDeclKey
    PreludeSymbolKey
    PreludeSrcTypeKey
    deriving (Eq, Ord)

data PreludeDefDeclKey = PreludeDefDeclKey
    PreludeSymbolKey
    PreludeConstrainedTypeKey
    PreludeExprKey
    deriving (Eq, Ord)

data PreludeClassConstraintKey = PreludeClassConstraintKey
    PreludeSymbolKey
    [PreludeSrcTypeKey]
    deriving (Eq, Ord)

data PreludeConstrainedTypeKey = PreludeConstrainedTypeKey
    [PreludeClassConstraintKey]
    PreludeSrcTypeKey
    deriving (Eq, Ord)

data PreludeFunctionalDependencyKey = PreludeFunctionalDependencyKey
    [String]
    [String]
    deriving (Eq, Ord)

data PreludeExprKey
    = PreludeVar PreludeValueRefKey
    | PreludeLit PreludeLitKey
    | PreludeLam PreludeParamKey PreludeExprKey
    | PreludeApp PreludeExprKey PreludeExprKey
    | PreludeLet PreludeLocalRefKey (Maybe PreludeSrcTypeKey) PreludeExprKey PreludeExprKey
    | PreludeAnn PreludeExprKey PreludeSrcTypeKey
    | PreludeCase PreludeExprKey [PreludeAltKey]
    deriving (Eq, Ord)

data PreludeParamKey = PreludeParamKey
    PreludeLocalRefKey
    (Maybe PreludeSrcTypeKey)
    deriving (Eq, Ord)

data PreludeAltKey = PreludeAltKey
    PreludePatternKey
    PreludeExprKey
    deriving (Eq, Ord)

data PreludePatternKey
    = PreludeCtorPattern PreludeSymbolKey [PreludePatternKey]
    | PreludeVarPattern PreludeLocalRefKey
    | PreludeWildcardPattern
    | PreludeAnnPattern PreludePatternKey PreludeSrcTypeKey
    deriving (Eq, Ord)

data PreludeValueRefKey
    = PreludeLocalValue PreludeLocalRefKey
    | PreludeGlobalValue PreludeSymbolKey
    deriving (Eq, Ord)

data PreludeSrcTypeKey
    = PreludeTypeVar PreludeTypeBinderKey
    | PreludeTypeArrow PreludeSrcTypeKey PreludeSrcTypeKey
    | PreludeTypeBase PreludeSymbolKey
    | PreludeTypeCon PreludeSymbolKey [PreludeSrcTypeKey]
    | PreludeTypeVarApp PreludeTypeBinderKey [PreludeSrcTypeKey]
    | PreludeTypeLambda PreludeTypeBinderKey PreludeSrcTypeKey
    | PreludeTypeApp PreludeSrcTypeKey PreludeSrcTypeKey
    | PreludeTypeForall PreludeTypeBinderKey (Maybe PreludeSrcTypeKey) PreludeSrcTypeKey
    | PreludeTypeMu PreludeTypeBinderKey PreludeSrcTypeKey
    | PreludeTypeBottom
    deriving (Eq, Ord)

data PreludeTypeParamKey
    = PreludeParsedTypeParam String PreludeKindKey
    | PreludeResolvedTypeParam PreludeTypeBinderKey PreludeKindKey
    deriving (Eq, Ord)

data PreludeKindKey
    = PreludeTypeKind
    | PreludeArrowKind PreludeKindKey PreludeKindKey
    deriving (Eq, Ord)

data PreludeLitKey
    = PreludeIntLit Integer
    | PreludeBoolLit Bool
    | PreludeStringLit String
    | PreludeCharLit Char
    deriving (Eq, Ord)

data PreludeSymbolKey = PreludeSymbolKey
    SymbolIdentityPayloadKey
    SymbolSpelling
    deriving (Eq, Ord)

data PreludeTypeBinderKey = PreludeTypeBinderKey
    TypeBinderIdentity
    String
    deriving (Eq, Ord)

data PreludeLocalRefKey = PreludeLocalRefKey
    LocalIdentity
    String
    Bool
    deriving (Eq, Ord)

data PreludeLocalSymbolsKey = PreludeLocalSymbolsKey
    (Map String [PreludeSymbolKey])
    (Map String [PreludeSymbolKey])
    (Map String [PreludeSymbolKey])
    deriving (Eq, Ord)

data PreludeScopeKey = PreludeScopeKey
    (Map String PreludeSymbolKey)
    (Map String PreludeSymbolKey)
    (Map String PreludeSymbolKey)
    (Map String PreludeSymbolKey)
    deriving (Eq, Ord)

builtinPreludeCacheKey :: ResolvedSemanticModule -> BuiltinPreludeCacheKey
builtinPreludeCacheKey resolvedModule =
    BuiltinPreludeCacheKey
        (resolvedSemanticModuleName resolvedModule)
        (symbolIdentityPayloadKey (resolvedSemanticModuleIdentity resolvedModule))
        (moduleSyntaxKey (resolvedSemanticModuleSyntax resolvedModule))
        (localSymbolsKey (resolvedSemanticModuleLocalSymbols resolvedModule))
        (scopeKey (resolvedSemanticModuleScope resolvedModule))
        (scopeKey (resolvedSemanticModuleExports resolvedModule))

moduleSyntaxKey :: P.ResolvedModuleSyntax -> PreludeModuleSyntaxKey
moduleSyntaxKey moduleSyntax =
    PreludeModuleSyntaxKey
        (P.moduleName moduleSyntax)
        (fmap (map exportItemKey) (P.moduleExports moduleSyntax))
        (map importKey (P.moduleImports moduleSyntax))
        (map declKey (P.moduleDecls moduleSyntax))

exportItemKey :: P.ResolvedExportItem -> PreludeExportItemKey
exportItemKey item =
    case item of
        P.ExportValue symbol -> PreludeExportValue (symbolKey symbol)
        P.ExportType ref -> PreludeExportType (exportTypeKey ref)
        P.ExportTypeWithConstructors ref ->
            PreludeExportTypeWithConstructors (exportTypeKey ref)

exportTypeKey :: P.ResolvedExportTypeRef -> PreludeExportTypeKey
exportTypeKey ref =
    PreludeExportTypeKey
        (P.resolvedExportTypeName ref)
        (map symbolKey (P.resolvedExportTypeSymbols ref))

importKey :: P.ResolvedImport -> PreludeImportKey
importKey import0 =
    PreludeImportKey
        (symbolKey (P.importModuleName import0))
        (P.importAlias import0)
        (fmap (map exportItemKey) (P.importExposing import0))

declKey :: P.ResolvedDecl -> PreludeDeclKey
declKey decl =
    case decl of
        P.DeclClass classDecl -> PreludeClassDecl (classDeclKey classDecl)
        P.DeclInstance instanceDecl -> PreludeInstanceDecl (instanceDeclKey instanceDecl)
        P.DeclData dataDecl -> PreludeDataDecl (dataDeclKey dataDecl)
        P.DeclTypeFamily familyDecl -> PreludeTypeFamilyDecl familyDecl
        P.DeclDef defDecl -> PreludeDefDecl (defDeclKey defDecl)

classDeclKey :: P.ResolvedClassDecl -> PreludeClassDeclKey
classDeclKey classDecl =
    PreludeClassDeclKey
        (symbolKey (P.classDeclName classDecl))
        (map classConstraintKey (P.classDeclSuperclasses classDecl))
        (map typeParamKey (NE.toList (P.classDeclParams classDecl)))
        (map functionalDependencyKey (P.classDeclFundeps classDecl))
        (map methodSigKey (P.classDeclMethods classDecl))

methodSigKey :: P.ResolvedMethodSig -> PreludeMethodSigKey
methodSigKey methodSig =
    PreludeMethodSigKey
        (symbolKey (P.methodSigName methodSig))
        (constrainedTypeKey (P.methodSigType methodSig))

instanceDeclKey :: P.ResolvedInstanceDecl -> PreludeInstanceDeclKey
instanceDeclKey instanceDecl =
    PreludeInstanceDeclKey
        (map classConstraintKey (P.instanceDeclConstraints instanceDecl))
        (symbolKey (P.instanceDeclClass instanceDecl))
        (map srcTypeKey (NE.toList (P.instanceDeclTypes instanceDecl)))
        (map methodDefKey (P.instanceDeclMethods instanceDecl))

methodDefKey :: P.ResolvedMethodDef -> PreludeMethodDefKey
methodDefKey methodDef =
    PreludeMethodDefKey
        (symbolKey (P.methodDefName methodDef))
        (exprKey (P.methodDefExpr methodDef))

dataDeclKey :: P.ResolvedDataDecl -> PreludeDataDeclKey
dataDeclKey dataDecl =
    PreludeDataDeclKey
        (symbolKey (P.dataDeclName dataDecl))
        (map typeParamKey (P.dataDeclParams dataDecl))
        (map constructorDeclKey (P.dataDeclConstructors dataDecl))
        (map symbolKey (P.dataDeclDeriving dataDecl))

constructorDeclKey :: P.ResolvedConstructorDecl -> PreludeConstructorDeclKey
constructorDeclKey constructorDecl =
    PreludeConstructorDeclKey
        (symbolKey (P.constructorDeclName constructorDecl))
        (srcTypeKey (P.constructorDeclType constructorDecl))

defDeclKey :: P.ResolvedDefDecl -> PreludeDefDeclKey
defDeclKey defDecl =
    PreludeDefDeclKey
        (symbolKey (P.defDeclName defDecl))
        (constrainedTypeKey (P.defDeclType defDecl))
        (exprKey (P.defDeclExpr defDecl))

classConstraintKey :: P.ResolvedClassConstraint -> PreludeClassConstraintKey
classConstraintKey constraint =
    PreludeClassConstraintKey
        (symbolKey (P.constraintClassName constraint))
        (map srcTypeKey (NE.toList (P.constraintTypes constraint)))

constrainedTypeKey :: P.ResolvedConstrainedType -> PreludeConstrainedTypeKey
constrainedTypeKey constrained =
    PreludeConstrainedTypeKey
        (map classConstraintKey (P.constrainedConstraints constrained))
        (srcTypeKey (P.constrainedBody constrained))

functionalDependencyKey :: P.FunctionalDependency -> PreludeFunctionalDependencyKey
functionalDependencyKey dependency =
    PreludeFunctionalDependencyKey
        (NE.toList (P.fundepDeterminers dependency))
        (NE.toList (P.fundepDetermined dependency))

exprKey :: P.ResolvedExpr -> PreludeExprKey
exprKey expression =
    case expression of
        P.EVar ref -> PreludeVar (valueRefKey ref)
        P.ELit literal -> PreludeLit (litKey literal)
        P.ELam param body -> PreludeLam (paramKey param) (exprKey body)
        P.EApp function argument -> PreludeApp (exprKey function) (exprKey argument)
        P.ELet ref annotation rhs body ->
            PreludeLet
                (localRefKey ref)
                (srcTypeKey <$> annotation)
                (exprKey rhs)
                (exprKey body)
        P.EAnn body annotation -> PreludeAnn (exprKey body) (srcTypeKey annotation)
        P.ECase scrutinee alternatives ->
            PreludeCase (exprKey scrutinee) (map altKey alternatives)

paramKey :: P.ResolvedParam -> PreludeParamKey
paramKey param =
    PreludeParamKey
        (localRefKey (P.paramName param))
        (srcTypeKey <$> P.paramType param)

altKey :: P.ResolvedAlt -> PreludeAltKey
altKey alternative =
    PreludeAltKey
        (patternKey (P.altPattern alternative))
        (exprKey (P.altExpr alternative))

patternKey :: P.ResolvedPattern -> PreludePatternKey
patternKey pattern0 =
    case pattern0 of
        P.PatCtor constructor patterns ->
            PreludeCtorPattern (symbolKey constructor) (map patternKey patterns)
        P.PatVar ref -> PreludeVarPattern (localRefKey ref)
        P.PatWildcard -> PreludeWildcardPattern
        P.PatAnn pattern1 annotation ->
            PreludeAnnPattern (patternKey pattern1) (srcTypeKey annotation)

valueRefKey :: P.ResolvedValueRef -> PreludeValueRefKey
valueRefKey ref =
    case ref of
        P.ResolvedLocalValue local -> PreludeLocalValue (localRefKey local)
        P.ResolvedGlobalValue global -> PreludeGlobalValue (symbolKey global)

srcTypeKey :: ResolvedSrcTy n v -> PreludeSrcTypeKey
srcTypeKey sourceType =
    case sourceType of
        RSTVar ref -> PreludeTypeVar (typeBinderKey ref)
        RSTArrow domain codomain ->
            PreludeTypeArrow (srcTypeKey domain) (srcTypeKey codomain)
        RSTBase symbol -> PreludeTypeBase (symbolKey symbol)
        RSTCon symbol arguments ->
            PreludeTypeCon (symbolKey symbol) (map srcTypeKey (NE.toList arguments))
        RSTVarApp ref arguments ->
            PreludeTypeVarApp (typeBinderKey ref) (map srcTypeKey (NE.toList arguments))
        RSTTyLam ref body -> PreludeTypeLambda (typeBinderKey ref) (srcTypeKey body)
        RSTTyApp function argument ->
            PreludeTypeApp (srcTypeKey function) (srcTypeKey argument)
        RSTForall ref bound body ->
            PreludeTypeForall
                (typeBinderKey ref)
                (srcTypeKey . unResolvedSrcBound <$> bound)
                (srcTypeKey body)
        RSTMu ref body -> PreludeTypeMu (typeBinderKey ref) (srcTypeKey body)
        RSTBottom -> PreludeTypeBottom

typeParamKey :: TypeParam -> PreludeTypeParamKey
typeParamKey param =
    case param of
        TypeParam name kind0 -> PreludeParsedTypeParam name (kindKey kind0)
        ResolvedTypeParam ref kind0 ->
            PreludeResolvedTypeParam (typeBinderKey ref) (kindKey kind0)

kindKey :: SrcKind -> PreludeKindKey
kindKey kind0 =
    case kind0 of
        KType -> PreludeTypeKind
        KArrow domain codomain -> PreludeArrowKind (kindKey domain) (kindKey codomain)

litKey :: Lit -> PreludeLitKey
litKey literal =
    case literal of
        LInt value -> PreludeIntLit value
        LBool value -> PreludeBoolLit value
        LString value -> PreludeStringLit value
        LChar value -> PreludeCharLit value

symbolKey :: ResolvedSymbol -> PreludeSymbolKey
symbolKey symbol =
    PreludeSymbolKey
        (symbolIdentityPayloadKey (resolvedSymbolIdentity symbol))
        (resolvedSymbolSpelling symbol)

typeBinderKey :: ResolvedTypeBinderRef -> PreludeTypeBinderKey
typeBinderKey ref =
    PreludeTypeBinderKey
        (resolvedTypeBinderIdentity ref)
        (resolvedTypeBinderName ref)

localRefKey :: LocalRef -> PreludeLocalRefKey
localRefKey ref =
    PreludeLocalRefKey
        (localRefIdentity ref)
        (localRefName ref)
        (localRefDiscard ref)

localSymbolsKey :: ResolvedLocalSymbols -> PreludeLocalSymbolsKey
localSymbolsKey localSymbols =
    PreludeLocalSymbolsKey
        (mapSymbolLists (resolvedLocalValues localSymbols))
        (mapSymbolLists (resolvedLocalTypes localSymbols))
        (mapSymbolLists (resolvedLocalClasses localSymbols))

scopeKey :: ResolvedScope -> PreludeScopeKey
scopeKey scope =
    PreludeScopeKey
        (Map.map symbolKey (resolvedScopeValues scope))
        (Map.map symbolKey (resolvedScopeTypes scope))
        (Map.map symbolKey (resolvedScopeClasses scope))
        (Map.map symbolKey (resolvedScopeModules scope))

mapSymbolLists :: Map String [ResolvedSymbol] -> Map String [PreludeSymbolKey]
mapSymbolLists =
    Map.map (map symbolKey)
