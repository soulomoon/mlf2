module ResolvedSymbolSpec (spec) where

import qualified Data.Map.Strict as Map
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax (SrcTy (..))
import Test.Hspec

spec :: Spec
spec =
  describe "MLF.Program resolved symbol identities" $ do
    it "keeps imported value identity stable across unqualified and aliased spellings" $ do
      let unqualified = resolvedValueInfoSymbol (SymbolUnqualifiedImport "Lib") valueInfo
          qualified =
            resolvedValueInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              valueInfo {valueDisplayName = "L.answer"}

      sameResolvedSymbol unqualified qualified `shouldBe` True
      resolvedSymbolIdentity unqualified `shouldBe` resolvedSymbolIdentity qualified
      resolvedSymbolSpelling unqualified `shouldNotBe` resolvedSymbolSpelling qualified

    it "represents imported type and constructor aliases with the same semantic identities" $ do
      let typeUnqualified = resolvedDataInfoSymbol (SymbolUnqualifiedImport "Lib") "Token" tokenDataInfo
          typeQualified = resolvedDataInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Token" tokenDataInfo
          ctorUnqualified = resolvedConstructorInfoSymbol (SymbolUnqualifiedImport "Lib") "Some" tokenDataInfo someCtor
          ctorQualified =
            resolvedConstructorInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              "L.Some"
              tokenDataInfo
              someCtor {ctorName = "L.Some"}

      sameResolvedSymbol typeUnqualified typeQualified `shouldBe` True
      sameResolvedSymbol ctorUnqualified ctorQualified `shouldBe` True
      symbolOwnerIdentity (resolvedSymbolIdentity ctorQualified)
        `shouldBe` Just (SymbolOwnerType "Lib" "Token")

    it "represents imported class and method aliases with the same semantic identities" $ do
      let classUnqualified = resolvedClassInfoSymbol (SymbolUnqualifiedImport "Lib") eqClassInfo
          classQualified = resolvedClassInfoSymbol (SymbolQualifiedImport "Lib" "L") qualifiedEqClassInfo
          methodUnqualified = resolvedValueInfoSymbol (SymbolUnqualifiedImport "Lib") eqMethodValue
          methodQualified =
            resolvedValueInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              qualifiedEqMethodValue

      sameResolvedSymbol classUnqualified classQualified `shouldBe` True
      sameResolvedSymbol methodUnqualified methodQualified `shouldBe` True
      symbolOwnerIdentity (resolvedSymbolIdentity methodQualified)
        `shouldBe` Just (SymbolOwnerClass "Lib" "Eq")

    it "can model local declarations and module/import identities without changing semantic keys" $ do
      let local = resolvedValueInfoSymbol (SymbolLocal "Main") mainValueInfo
          importedModule = resolvedModuleSymbol (SymbolQualifiedImport "Lib" "L") "Lib" "L"

      resolvedSymbolIdentity local
        `shouldBe` SymbolIdentity SymbolValue "Main" "main" Nothing
      resolvedSymbolIdentity importedModule
        `shouldBe` SymbolIdentity SymbolModule "Lib" "Lib" Nothing
      symbolDisplayName (resolvedSymbolSpelling importedModule) `shouldBe` "L"

valueInfo :: ValueInfo
valueInfo =
  OrdinaryValue
    { valueDisplayName = "answer",
      valueRuntimeName = "Lib__answer",
      valueType = STBase "Int",
      valueConstraints = [],
      valueOriginModule = "Lib"
    }

mainValueInfo :: ValueInfo
mainValueInfo =
  OrdinaryValue
    { valueDisplayName = "main",
      valueRuntimeName = "Main__main",
      valueType = STBase "Int",
      valueConstraints = [],
      valueOriginModule = "Main"
    }

someCtor :: ConstructorInfo
someCtor =
  ConstructorInfo
    { ctorName = "Some",
      ctorRuntimeName = "Lib__Some",
      ctorType = STBase "Token",
      ctorForalls = [],
      ctorArgs = [],
      ctorResult = STBase "Token",
      ctorOwningType = "Token",
      ctorIndex = 0
    }

tokenDataInfo :: DataInfo
tokenDataInfo =
  DataInfo
    { dataName = "Token",
      dataModule = "Lib",
      dataParams = [],
      dataConstructors = [someCtor]
    }

eqMethodInfo :: MethodInfo
eqMethodInfo =
  MethodInfo
    { methodClassName = "Eq",
      methodClassModule = "Lib",
      methodName = "eq",
      methodRuntimeBase = "Lib__Eq__eq",
      methodType = STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")),
      methodConstraints = [],
      methodParamName = "a"
    }

qualifiedEqMethodInfo :: MethodInfo
qualifiedEqMethodInfo =
  eqMethodInfo {methodClassName = "L.Eq"}

eqClassInfo :: ClassInfo
eqClassInfo =
  ClassInfo
    { className = "Eq",
      classModule = "Lib",
      classParamName = "a",
      classMethods = Map.singleton "eq" eqMethodInfo
    }

qualifiedEqClassInfo :: ClassInfo
qualifiedEqClassInfo =
  ClassInfo
    { className = "L.Eq",
      classModule = "Lib",
      classParamName = "a",
      classMethods = Map.singleton "eq" qualifiedEqMethodInfo
    }

eqMethodValue :: ValueInfo
eqMethodValue =
  OverloadedMethod
    { valueDisplayName = "eq",
      valueMethodInfo = eqMethodInfo,
      valueOriginModule = "Lib"
    }

qualifiedEqMethodValue :: ValueInfo
qualifiedEqMethodValue =
  OverloadedMethod
    { valueDisplayName = "L.eq",
      valueMethodInfo = qualifiedEqMethodInfo,
      valueOriginModule = "Lib"
    }
