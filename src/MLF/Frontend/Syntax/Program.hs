{-# LANGUAGE DeriveTraversable #-}
module MLF.Frontend.Syntax.Program
    ( ModuleName
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
    , Program (..)
    , Module (..)
    , ExportItem (..)
    , Import (..)
    , ClassConstraint (..)
    , ConstrainedType (..)
    , unconstrainedType
    , Decl (..)
    , ClassDecl (..)
    , MethodSig (..)
    , InstanceDecl (..)
    , MethodDef (..)
    , DataDecl (..)
    , ConstructorDecl (..)
    , DefDecl (..)
    , Expr (..)
    , Param (..)
    , Alt (..)
    , Pattern (..)
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MLF.Frontend.Syntax (Lit (..), SrcType)

type ModuleName = String

type TypeName = String

type ConstructorName = String

type ClassName = String

type MethodName = String

type ValueName = String

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
        , spanValues = Map.unionWith (++) (spanValues left) (spanValues right)
        , spanTypes = Map.unionWith (++) (spanTypes left) (spanTypes right)
        , spanConstructors = Map.unionWith (++) (spanConstructors left) (spanConstructors right)
        , spanClasses = Map.unionWith (++) (spanClasses left) (spanClasses right)
        }

newtype Program = Program
    { programModules :: [Module]
    }
    deriving (Eq, Show)

data Module = Module
    { moduleName :: ModuleName
    , moduleExports :: Maybe [ExportItem]
    , moduleImports :: [Import]
    , moduleDecls :: [Decl]
    }
    deriving (Eq, Show)

data ExportItem
    = ExportValue ValueName
    | ExportType TypeName
    | ExportTypeWithConstructors TypeName
    deriving (Eq, Show)

data Import = Import
    { importModuleName :: ModuleName
    , importAlias :: Maybe ModuleName
    , importExposing :: Maybe [ExportItem]
    }
    deriving (Eq, Show)

data ClassConstraint = ClassConstraint
    { constraintClassName :: ClassName
    , constraintType :: SrcType
    }
    deriving (Eq, Show)

data ConstrainedType = ConstrainedType
    { constrainedConstraints :: [ClassConstraint]
    , constrainedBody :: SrcType
    }
    deriving (Eq, Show)

unconstrainedType :: SrcType -> ConstrainedType
unconstrainedType = ConstrainedType []

data Decl
    = DeclClass ClassDecl
    | DeclInstance InstanceDecl
    | DeclData DataDecl
    | DeclDef DefDecl
    deriving (Eq, Show)

data ClassDecl = ClassDecl
    { classDeclName :: ClassName
    , classDeclParam :: String
    , classDeclMethods :: [MethodSig]
    }
    deriving (Eq, Show)

data MethodSig = MethodSig
    { methodSigName :: MethodName
    , methodSigType :: ConstrainedType
    }
    deriving (Eq, Show)

data InstanceDecl = InstanceDecl
    { instanceDeclConstraints :: [ClassConstraint]
    , instanceDeclClass :: ClassName
    , instanceDeclType :: SrcType
    , instanceDeclMethods :: [MethodDef]
    }
    deriving (Eq, Show)

data MethodDef = MethodDef
    { methodDefName :: MethodName
    , methodDefExpr :: Expr
    }
    deriving (Eq, Show)

data DataDecl = DataDecl
    { dataDeclName :: TypeName
    , dataDeclParams :: [String]
    , dataDeclConstructors :: [ConstructorDecl]
    , dataDeclDeriving :: [ClassName]
    }
    deriving (Eq, Show)

data ConstructorDecl = ConstructorDecl
    { constructorDeclName :: ConstructorName
    , constructorDeclType :: SrcType
    }
    deriving (Eq, Show)

data DefDecl = DefDecl
    { defDeclName :: ValueName
    , defDeclType :: ConstrainedType
    , defDeclExpr :: Expr
    }
    deriving (Eq, Show)

data Expr
    = EVar ValueName
    | ELit Lit
    | ELam Param Expr
    | EApp Expr Expr
    | ELet ValueName (Maybe SrcType) Expr Expr
    | EAnn Expr SrcType
    | ECase Expr [Alt]
    deriving (Eq, Show)

data Param = Param
    { paramName :: ValueName
    , paramType :: Maybe SrcType
    }
    deriving (Eq, Show)

data Alt = Alt
    { altPattern :: Pattern
    , altExpr :: Expr
    }
    deriving (Eq, Show)

data Pattern
    = PatCtor ConstructorName [Pattern]
    | PatVar ValueName
    | PatWildcard
    | PatAnn Pattern SrcType
    deriving (Eq, Show)
