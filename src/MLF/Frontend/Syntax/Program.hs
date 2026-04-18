{-# LANGUAGE DeriveTraversable #-}
module MLF.Frontend.Syntax.Program
    ( ModuleName
    , TypeName
    , ConstructorName
    , ClassName
    , MethodName
    , ValueName
    , Program (..)
    , Module (..)
    , ExportItem (..)
    , Import (..)
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

import MLF.Frontend.Syntax (Lit (..), SrcType)

type ModuleName = String

type TypeName = String

type ConstructorName = String

type ClassName = String

type MethodName = String

type ValueName = String

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
    , importExposing :: Maybe [ExportItem]
    }
    deriving (Eq, Show)

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
    , methodSigType :: SrcType
    }
    deriving (Eq, Show)

data InstanceDecl = InstanceDecl
    { instanceDeclClass :: ClassName
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
    , defDeclType :: SrcType
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
    = PatCtor ConstructorName [ValueName]
    | PatVar ValueName
    | PatWildcard
    deriving (Eq, Show)
