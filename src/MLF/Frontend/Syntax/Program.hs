{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MLF.Frontend.Syntax.Program
    ( ProgramPhase (..)
    , ModuleName
    , TypeName
    , ConstructorName
    , ClassName
    , MethodName
    , ValueName
    , SourcePosition (..)
    , SourceSpan (..)
    , ProgramSpanIndex (..)
    , LocatedProgram (..)
    , emptyProgramSpanIndex
    , appendProgramSpanIndex
    , ProgramF (..)
    , Program
    , ResolvedProgramSyntax
    , ModuleF (..)
    , Module
    , ResolvedModuleSyntax
    , ExportItemF (..)
    , ExportItem
    , ResolvedExportItem
    , ImportF (..)
    , Import
    , ResolvedImport
    , ClassConstraintF (..)
    , ClassConstraint
    , ResolvedClassConstraint
    , ConstrainedTypeF (..)
    , ConstrainedType
    , ResolvedConstrainedType
    , unconstrainedType
    , resolvedUnconstrainedType
    , DeclF (..)
    , Decl
    , ResolvedDecl
    , ClassDeclF (..)
    , ClassDecl
    , ResolvedClassDecl
    , MethodSigF (..)
    , MethodSig
    , ResolvedMethodSig
    , InstanceDeclF (..)
    , InstanceDecl
    , ResolvedInstanceDecl
    , MethodDefF (..)
    , MethodDef
    , ResolvedMethodDef
    , DataDeclF (..)
    , DataDecl
    , ResolvedDataDecl
    , ConstructorDeclF (..)
    , ConstructorDecl
    , ResolvedConstructorDecl
    , DefDeclF (..)
    , DefDecl
    , ResolvedDefDecl
    , ExprF (..)
    , Expr
    , ResolvedExpr
    , ParamF (..)
    , Param
    , ResolvedParam
    , AltF (..)
    , Alt
    , ResolvedAlt
    , PatternF (..)
    , Pattern
    , ResolvedPattern
    , ResolvedValueRef (..)
    , ResolvedExportTypeRef (..)
    , ProgramSrcType
    , ModuleRef
    , ValueRef
    , ExportValueRef
    , TypeRef
    , ExportTypeRef
    , ClassRef
    , ConstructorRef
    , refDisplayName
    , valueRefName
    , exportValueRefName
    , typeRefName
    , exportTypeRefName
    , classRefName
    , constructorRefName
    , moduleRefName
    , exportItemName
    , programSrcTypeToSrcType
    , programSrcTypeIdentityType
    , unresolveProgram
    , unresolveModule
    , unresolveDecl
    , unresolveExpr
    , unresolvePattern
    , unresolveConstrainedType
    , unresolveClassConstraint
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MLF.Frontend.Symbol
    ( ResolvedSymbol (..)
    , resolvedSymbolSpelling
    , symbolDisplayName
    )
import MLF.Frontend.Syntax
    ( Lit (..)
    , ResolvedSrcType
    , SrcType
    , resolvedSrcTypeIdentityType
    , resolvedSrcTypeToSrcType
    )

data ProgramPhase = Parsed | Resolved
    deriving (Eq, Show)

type ModuleName = String

type TypeName = String

type ConstructorName = String

type ClassName = String

type MethodName = String

type ValueName = String

data ResolvedValueRef
    = ResolvedLocalValue ValueName
    | ResolvedGlobalValue ResolvedSymbol
    deriving (Eq, Show)

data ResolvedExportTypeRef = ResolvedExportTypeRef
    { resolvedExportTypeName :: TypeName
    , resolvedExportTypeSymbols :: [ResolvedSymbol]
    }
    deriving (Eq, Show)

type family ProgramSrcType (p :: ProgramPhase) where
    ProgramSrcType 'Parsed = SrcType
    ProgramSrcType 'Resolved = ResolvedSrcType

type family ModuleRef (p :: ProgramPhase) where
    ModuleRef 'Parsed = ModuleName
    ModuleRef 'Resolved = ResolvedSymbol

type family ValueRef (p :: ProgramPhase) where
    ValueRef 'Parsed = ValueName
    ValueRef 'Resolved = ResolvedValueRef

type family ExportValueRef (p :: ProgramPhase) where
    ExportValueRef 'Parsed = ValueName
    ExportValueRef 'Resolved = ResolvedSymbol

type family TypeRef (p :: ProgramPhase) where
    TypeRef 'Parsed = TypeName
    TypeRef 'Resolved = ResolvedSymbol

type family ExportTypeRef (p :: ProgramPhase) where
    ExportTypeRef 'Parsed = TypeName
    ExportTypeRef 'Resolved = ResolvedExportTypeRef

type family ClassRef (p :: ProgramPhase) where
    ClassRef 'Parsed = ClassName
    ClassRef 'Resolved = ResolvedSymbol

type family ConstructorRef (p :: ProgramPhase) where
    ConstructorRef 'Parsed = ConstructorName
    ConstructorRef 'Resolved = ResolvedSymbol

class RefDisplay a where
    refDisplayName :: a -> String

instance RefDisplay String where
    refDisplayName = id

instance RefDisplay ResolvedSymbol where
    refDisplayName = symbolDisplayName . resolvedSymbolSpelling

instance RefDisplay ResolvedValueRef where
    refDisplayName ref =
        case ref of
            ResolvedLocalValue name -> name
            ResolvedGlobalValue symbol -> refDisplayName symbol

instance RefDisplay ResolvedExportTypeRef where
    refDisplayName = resolvedExportTypeName

valueRefName :: RefDisplay (ValueRef p) => ValueRef p -> String
valueRefName = refDisplayName

exportValueRefName :: RefDisplay (ExportValueRef p) => ExportValueRef p -> String
exportValueRefName = refDisplayName

typeRefName :: RefDisplay (TypeRef p) => TypeRef p -> String
typeRefName = refDisplayName

exportTypeRefName :: RefDisplay (ExportTypeRef p) => ExportTypeRef p -> String
exportTypeRefName = refDisplayName

classRefName :: RefDisplay (ClassRef p) => ClassRef p -> String
classRefName = refDisplayName

constructorRefName :: RefDisplay (ConstructorRef p) => ConstructorRef p -> String
constructorRefName = refDisplayName

moduleRefName :: RefDisplay (ModuleRef p) => ModuleRef p -> String
moduleRefName = refDisplayName

programSrcTypeToSrcType :: ProgramSrcType p ~ ResolvedSrcType => ProgramSrcType p -> SrcType
programSrcTypeToSrcType = resolvedSrcTypeToSrcType

programSrcTypeIdentityType :: ProgramSrcType p ~ ResolvedSrcType => ProgramSrcType p -> SrcType
programSrcTypeIdentityType = resolvedSrcTypeIdentityType

data SourcePosition = SourcePosition
    { sourceLine :: Int
    , sourceColumn :: Int
    }
    deriving (Eq, Ord, Show)

data SourceSpan = SourceSpan
    { sourceFile :: FilePath
    , sourceStart :: SourcePosition
    , sourceEnd :: SourcePosition
    }
    deriving (Eq, Ord, Show)

data ProgramSpanIndex = ProgramSpanIndex
    { spanModules :: Map ModuleName SourceSpan
    , spanImports :: Map ModuleName [SourceSpan]
    , spanImportAliases :: Map ModuleName [SourceSpan]
    , spanImportItems :: Map String [SourceSpan]
    , spanExportItems :: Map String [SourceSpan]
    , spanValues :: Map ValueName [SourceSpan]
    , spanTypes :: Map TypeName [SourceSpan]
    , spanConstructors :: Map ConstructorName [SourceSpan]
    , spanClasses :: Map ClassName [SourceSpan]
    }
    deriving (Eq, Show)

data LocatedProgram = LocatedProgram
    { locatedProgram :: Program
    , locatedProgramSpans :: ProgramSpanIndex
    }
    deriving (Eq, Show)

emptyProgramSpanIndex :: ProgramSpanIndex
emptyProgramSpanIndex =
    ProgramSpanIndex
        { spanModules = Map.empty
        , spanImports = Map.empty
        , spanImportAliases = Map.empty
        , spanImportItems = Map.empty
        , spanExportItems = Map.empty
        , spanValues = Map.empty
        , spanTypes = Map.empty
        , spanConstructors = Map.empty
        , spanClasses = Map.empty
        }

appendProgramSpanIndex :: ProgramSpanIndex -> ProgramSpanIndex -> ProgramSpanIndex
appendProgramSpanIndex left right =
    ProgramSpanIndex
        { spanModules = spanModules left `Map.union` spanModules right
        , spanImports = Map.unionWith (++) (spanImports left) (spanImports right)
        , spanImportAliases = Map.unionWith (++) (spanImportAliases left) (spanImportAliases right)
        , spanImportItems = Map.unionWith (++) (spanImportItems left) (spanImportItems right)
        , spanExportItems = Map.unionWith (++) (spanExportItems left) (spanExportItems right)
        , spanValues = Map.unionWith (++) (spanValues left) (spanValues right)
        , spanTypes = Map.unionWith (++) (spanTypes left) (spanTypes right)
        , spanConstructors = Map.unionWith (++) (spanConstructors left) (spanConstructors right)
        , spanClasses = Map.unionWith (++) (spanClasses left) (spanClasses right)
        }

newtype ProgramF (p :: ProgramPhase) = Program
    { programModules :: [ModuleF p]
    }

deriving instance Eq (ModuleF p) => Eq (ProgramF p)

deriving instance Show (ModuleF p) => Show (ProgramF p)

type Program = ProgramF 'Parsed

type ResolvedProgramSyntax = ProgramF 'Resolved

data ModuleF (p :: ProgramPhase) = Module
    { moduleName :: ModuleName
    , moduleExports :: Maybe [ExportItemF p]
    , moduleImports :: [ImportF p]
    , moduleDecls :: [DeclF p]
    }

deriving instance
    (Eq (ExportItemF p), Eq (ImportF p), Eq (DeclF p)) =>
    Eq (ModuleF p)

deriving instance
    (Show (ExportItemF p), Show (ImportF p), Show (DeclF p)) =>
    Show (ModuleF p)

type Module = ModuleF 'Parsed

type ResolvedModuleSyntax = ModuleF 'Resolved

data ExportItemF (p :: ProgramPhase)
    = ExportValue (ExportValueRef p)
    | ExportType (ExportTypeRef p)
    | ExportTypeWithConstructors (ExportTypeRef p)

deriving instance
    (Eq (ExportValueRef p), Eq (ExportTypeRef p)) =>
    Eq (ExportItemF p)

deriving instance
    (Show (ExportValueRef p), Show (ExportTypeRef p)) =>
    Show (ExportItemF p)

type ExportItem = ExportItemF 'Parsed

type ResolvedExportItem = ExportItemF 'Resolved

exportItemName :: (RefDisplay (ExportValueRef p), RefDisplay (ExportTypeRef p)) => ExportItemF p -> String
exportItemName item =
    case item of
        ExportValue name -> refDisplayName name
        ExportType name -> refDisplayName name
        ExportTypeWithConstructors name -> refDisplayName name

data ImportF (p :: ProgramPhase) = Import
    { importModuleName :: ModuleRef p
    , importAlias :: Maybe ModuleName
    , importExposing :: Maybe [ExportItemF p]
    }

deriving instance
    (Eq (ModuleRef p), Eq (ExportItemF p)) =>
    Eq (ImportF p)

deriving instance
    (Show (ModuleRef p), Show (ExportItemF p)) =>
    Show (ImportF p)

type Import = ImportF 'Parsed

type ResolvedImport = ImportF 'Resolved

data ClassConstraintF (p :: ProgramPhase) = ClassConstraint
    { constraintClassName :: ClassRef p
    , constraintType :: ProgramSrcType p
    }

deriving instance
    (Eq (ClassRef p), Eq (ProgramSrcType p)) =>
    Eq (ClassConstraintF p)

deriving instance
    (Show (ClassRef p), Show (ProgramSrcType p)) =>
    Show (ClassConstraintF p)

type ClassConstraint = ClassConstraintF 'Parsed

type ResolvedClassConstraint = ClassConstraintF 'Resolved

data ConstrainedTypeF (p :: ProgramPhase) = ConstrainedType
    { constrainedConstraints :: [ClassConstraintF p]
    , constrainedBody :: ProgramSrcType p
    }

deriving instance
    (Eq (ClassConstraintF p), Eq (ProgramSrcType p)) =>
    Eq (ConstrainedTypeF p)

deriving instance
    (Show (ClassConstraintF p), Show (ProgramSrcType p)) =>
    Show (ConstrainedTypeF p)

type ConstrainedType = ConstrainedTypeF 'Parsed

type ResolvedConstrainedType = ConstrainedTypeF 'Resolved

unconstrainedType :: SrcType -> ConstrainedType
unconstrainedType = ConstrainedType []

resolvedUnconstrainedType :: ResolvedSrcType -> ResolvedConstrainedType
resolvedUnconstrainedType = ConstrainedType []

data DeclF (p :: ProgramPhase)
    = DeclClass (ClassDeclF p)
    | DeclInstance (InstanceDeclF p)
    | DeclData (DataDeclF p)
    | DeclDef (DefDeclF p)

deriving instance
    (Eq (ClassDeclF p), Eq (InstanceDeclF p), Eq (DataDeclF p), Eq (DefDeclF p)) =>
    Eq (DeclF p)

deriving instance
    (Show (ClassDeclF p), Show (InstanceDeclF p), Show (DataDeclF p), Show (DefDeclF p)) =>
    Show (DeclF p)

type Decl = DeclF 'Parsed

type ResolvedDecl = DeclF 'Resolved

data ClassDeclF (p :: ProgramPhase) = ClassDecl
    { classDeclName :: ClassName
    , classDeclParam :: String
    , classDeclMethods :: [MethodSigF p]
    }

deriving instance Eq (MethodSigF p) => Eq (ClassDeclF p)

deriving instance Show (MethodSigF p) => Show (ClassDeclF p)

type ClassDecl = ClassDeclF 'Parsed

type ResolvedClassDecl = ClassDeclF 'Resolved

data MethodSigF (p :: ProgramPhase) = MethodSig
    { methodSigName :: MethodName
    , methodSigType :: ConstrainedTypeF p
    }

deriving instance Eq (ConstrainedTypeF p) => Eq (MethodSigF p)

deriving instance Show (ConstrainedTypeF p) => Show (MethodSigF p)

type MethodSig = MethodSigF 'Parsed

type ResolvedMethodSig = MethodSigF 'Resolved

data InstanceDeclF (p :: ProgramPhase) = InstanceDecl
    { instanceDeclConstraints :: [ClassConstraintF p]
    , instanceDeclClass :: ClassRef p
    , instanceDeclType :: ProgramSrcType p
    , instanceDeclMethods :: [MethodDefF p]
    }

deriving instance
    (Eq (ClassConstraintF p), Eq (ClassRef p), Eq (ProgramSrcType p), Eq (MethodDefF p)) =>
    Eq (InstanceDeclF p)

deriving instance
    (Show (ClassConstraintF p), Show (ClassRef p), Show (ProgramSrcType p), Show (MethodDefF p)) =>
    Show (InstanceDeclF p)

type InstanceDecl = InstanceDeclF 'Parsed

type ResolvedInstanceDecl = InstanceDeclF 'Resolved

data MethodDefF (p :: ProgramPhase) = MethodDef
    { methodDefName :: MethodName
    , methodDefExpr :: ExprF p
    }

deriving instance Eq (ExprF p) => Eq (MethodDefF p)

deriving instance Show (ExprF p) => Show (MethodDefF p)

type MethodDef = MethodDefF 'Parsed

type ResolvedMethodDef = MethodDefF 'Resolved

data DataDeclF (p :: ProgramPhase) = DataDecl
    { dataDeclName :: TypeName
    , dataDeclParams :: [String]
    , dataDeclConstructors :: [ConstructorDeclF p]
    , dataDeclDeriving :: [ClassRef p]
    }

deriving instance
    (Eq (ConstructorDeclF p), Eq (ClassRef p)) =>
    Eq (DataDeclF p)

deriving instance
    (Show (ConstructorDeclF p), Show (ClassRef p)) =>
    Show (DataDeclF p)

type DataDecl = DataDeclF 'Parsed

type ResolvedDataDecl = DataDeclF 'Resolved

data ConstructorDeclF (p :: ProgramPhase) = ConstructorDecl
    { constructorDeclName :: ConstructorName
    , constructorDeclType :: ProgramSrcType p
    }

deriving instance Eq (ProgramSrcType p) => Eq (ConstructorDeclF p)

deriving instance Show (ProgramSrcType p) => Show (ConstructorDeclF p)

type ConstructorDecl = ConstructorDeclF 'Parsed

type ResolvedConstructorDecl = ConstructorDeclF 'Resolved

data DefDeclF (p :: ProgramPhase) = DefDecl
    { defDeclName :: ValueName
    , defDeclType :: ConstrainedTypeF p
    , defDeclExpr :: ExprF p
    }

deriving instance
    (Eq (ConstrainedTypeF p), Eq (ExprF p)) =>
    Eq (DefDeclF p)

deriving instance
    (Show (ConstrainedTypeF p), Show (ExprF p)) =>
    Show (DefDeclF p)

type DefDecl = DefDeclF 'Parsed

type ResolvedDefDecl = DefDeclF 'Resolved

data ExprF (p :: ProgramPhase)
    = EVar (ValueRef p)
    | ELit Lit
    | ELam (ParamF p) (ExprF p)
    | EApp (ExprF p) (ExprF p)
    | ELet ValueName (Maybe (ProgramSrcType p)) (ExprF p) (ExprF p)
    | EAnn (ExprF p) (ProgramSrcType p)
    | ECase (ExprF p) [AltF p]

deriving instance
    (Eq (ValueRef p), Eq (ProgramSrcType p), Eq (ParamF p), Eq (AltF p)) =>
    Eq (ExprF p)

deriving instance
    (Show (ValueRef p), Show (ProgramSrcType p), Show (ParamF p), Show (AltF p)) =>
    Show (ExprF p)

type Expr = ExprF 'Parsed

type ResolvedExpr = ExprF 'Resolved

data ParamF (p :: ProgramPhase) = Param
    { paramName :: ValueName
    , paramType :: Maybe (ProgramSrcType p)
    }

deriving instance Eq (ProgramSrcType p) => Eq (ParamF p)

deriving instance Show (ProgramSrcType p) => Show (ParamF p)

type Param = ParamF 'Parsed

type ResolvedParam = ParamF 'Resolved

data AltF (p :: ProgramPhase) = Alt
    { altPattern :: PatternF p
    , altExpr :: ExprF p
    }

deriving instance
    (Eq (PatternF p), Eq (ExprF p)) =>
    Eq (AltF p)

deriving instance
    (Show (PatternF p), Show (ExprF p)) =>
    Show (AltF p)

type Alt = AltF 'Parsed

type ResolvedAlt = AltF 'Resolved

data PatternF (p :: ProgramPhase)
    = PatCtor (ConstructorRef p) [PatternF p]
    | PatVar ValueName
    | PatWildcard
    | PatAnn (PatternF p) (ProgramSrcType p)

deriving instance
    (Eq (ConstructorRef p), Eq (ProgramSrcType p)) =>
    Eq (PatternF p)

deriving instance
    (Show (ConstructorRef p), Show (ProgramSrcType p)) =>
    Show (PatternF p)

type Pattern = PatternF 'Parsed

type ResolvedPattern = PatternF 'Resolved

unresolveProgram :: ResolvedProgramSyntax -> Program
unresolveProgram (Program modules0) =
    Program (map unresolveModule modules0)

unresolveModule :: ResolvedModuleSyntax -> Module
unresolveModule mod0 =
    Module
        { moduleName = moduleName mod0
        , moduleExports = fmap (map unresolveExportItem) (moduleExports mod0)
        , moduleImports = map unresolveImport (moduleImports mod0)
        , moduleDecls = map unresolveDecl (moduleDecls mod0)
        }

unresolveExportItem :: ResolvedExportItem -> ExportItem
unresolveExportItem item =
    case item of
        ExportValue symbol -> ExportValue (refDisplayName symbol)
        ExportType ref -> ExportType (resolvedExportTypeName ref)
        ExportTypeWithConstructors ref -> ExportTypeWithConstructors (resolvedExportTypeName ref)

unresolveImport :: ResolvedImport -> Import
unresolveImport imp =
    Import
        { importModuleName = refDisplayName (importModuleName imp)
        , importAlias = importAlias imp
        , importExposing = fmap (map unresolveExportItem) (importExposing imp)
        }

unresolveDecl :: ResolvedDecl -> Decl
unresolveDecl decl =
    case decl of
        DeclClass classDecl -> DeclClass (unresolveClassDecl classDecl)
        DeclInstance instDecl -> DeclInstance (unresolveInstanceDecl instDecl)
        DeclData dataDecl -> DeclData (unresolveDataDecl dataDecl)
        DeclDef defDecl -> DeclDef (unresolveDefDecl defDecl)

unresolveClassDecl :: ResolvedClassDecl -> ClassDecl
unresolveClassDecl decl =
    ClassDecl
        { classDeclName = classDeclName decl
        , classDeclParam = classDeclParam decl
        , classDeclMethods = map unresolveMethodSig (classDeclMethods decl)
        }

unresolveMethodSig :: ResolvedMethodSig -> MethodSig
unresolveMethodSig sig =
    MethodSig
        { methodSigName = methodSigName sig
        , methodSigType = unresolveConstrainedType (methodSigType sig)
        }

unresolveInstanceDecl :: ResolvedInstanceDecl -> InstanceDecl
unresolveInstanceDecl decl =
    InstanceDecl
        { instanceDeclConstraints = map unresolveClassConstraint (instanceDeclConstraints decl)
        , instanceDeclClass = refDisplayName (instanceDeclClass decl)
        , instanceDeclType = resolvedSrcTypeToSrcType (instanceDeclType decl)
        , instanceDeclMethods = map unresolveMethodDef (instanceDeclMethods decl)
        }

unresolveMethodDef :: ResolvedMethodDef -> MethodDef
unresolveMethodDef def =
    MethodDef
        { methodDefName = methodDefName def
        , methodDefExpr = unresolveExpr (methodDefExpr def)
        }

unresolveDataDecl :: ResolvedDataDecl -> DataDecl
unresolveDataDecl decl =
    DataDecl
        { dataDeclName = dataDeclName decl
        , dataDeclParams = dataDeclParams decl
        , dataDeclConstructors = map unresolveConstructorDecl (dataDeclConstructors decl)
        , dataDeclDeriving = map refDisplayName (dataDeclDeriving decl)
        }

unresolveConstructorDecl :: ResolvedConstructorDecl -> ConstructorDecl
unresolveConstructorDecl decl =
    ConstructorDecl
        { constructorDeclName = constructorDeclName decl
        , constructorDeclType = resolvedSrcTypeToSrcType (constructorDeclType decl)
        }

unresolveDefDecl :: ResolvedDefDecl -> DefDecl
unresolveDefDecl decl =
    DefDecl
        { defDeclName = defDeclName decl
        , defDeclType = unresolveConstrainedType (defDeclType decl)
        , defDeclExpr = unresolveExpr (defDeclExpr decl)
        }

unresolveConstrainedType :: ResolvedConstrainedType -> ConstrainedType
unresolveConstrainedType ty =
    ConstrainedType
        { constrainedConstraints = map unresolveClassConstraint (constrainedConstraints ty)
        , constrainedBody = resolvedSrcTypeToSrcType (constrainedBody ty)
        }

unresolveClassConstraint :: ResolvedClassConstraint -> ClassConstraint
unresolveClassConstraint constraint =
    ClassConstraint
        { constraintClassName = refDisplayName (constraintClassName constraint)
        , constraintType = resolvedSrcTypeToSrcType (constraintType constraint)
        }

unresolveExpr :: ResolvedExpr -> Expr
unresolveExpr expr =
    case expr of
        EVar ref -> EVar (refDisplayName ref)
        ELit lit -> ELit lit
        ELam param body -> ELam (unresolveParam param) (unresolveExpr body)
        EApp fun arg -> EApp (unresolveExpr fun) (unresolveExpr arg)
        ELet name mbTy rhs body ->
            ELet name (fmap resolvedSrcTypeToSrcType mbTy) (unresolveExpr rhs) (unresolveExpr body)
        EAnn inner ty -> EAnn (unresolveExpr inner) (resolvedSrcTypeToSrcType ty)
        ECase scrutinee alts -> ECase (unresolveExpr scrutinee) (map unresolveAlt alts)

unresolveParam :: ResolvedParam -> Param
unresolveParam param =
    Param
        { paramName = paramName param
        , paramType = fmap resolvedSrcTypeToSrcType (paramType param)
        }

unresolveAlt :: ResolvedAlt -> Alt
unresolveAlt alt =
    Alt
        { altPattern = unresolvePattern (altPattern alt)
        , altExpr = unresolveExpr (altExpr alt)
        }

unresolvePattern :: ResolvedPattern -> Pattern
unresolvePattern pattern0 =
    case pattern0 of
        PatCtor ctor args -> PatCtor (refDisplayName ctor) (map unresolvePattern args)
        PatVar name -> PatVar name
        PatWildcard -> PatWildcard
        PatAnn inner ty -> PatAnn (unresolvePattern inner) (resolvedSrcTypeToSrcType ty)
