{-# LANGUAGE GADTs #-}

module ResolvedSymbolSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.Frontend.Program.Elaborate (lowerType, mkElaborateScope)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType, firstOrderTypeParam)
import Test.Hspec

spec :: Spec
spec = do
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

  describe "substituteTypeVar" $ do
    it "composes variable-headed type application heads with partially applied constructors" $
      substituteTypeVar "f" (STCon "Either" (STBase "Int" :| [])) (STVarApp "f" (STVar "a" :| []))
        `shouldBe` STCon "Either" (STBase "Int" :| [STVar "a"])

    it "composes variable-headed type application heads with partially applied variable heads" $
      substituteTypeVar "f" (STVarApp "g" (STBase "Int" :| [])) (STVarApp "f" (STVar "a" :| []))
        `shouldBe` STVarApp "g" (STBase "Int" :| [STVar "a"])

  describe "higher-kinded source type lowering" $ do
    it "composes variable-headed constructor fields with partially applied constructor arguments" $ do
      let scope = mkElaborateScope Map.empty (Map.singleton "Higher" higherDataInfo) Map.empty []
          lowered = lowerType scope (STCon "Higher" (STCon "Either" (STBase "Int" :| []) :| [STBase "Bool"]))
          expectedField = STCon "Either" (STBase "Int" :| [STBase "Bool"])
      lowered `shouldSatisfy` containsSrcType expectedField
      lowered `shouldSatisfy` (not . containsVarAppHead "f")

containsSrcType :: SrcType -> SrcType -> Bool
containsSrcType needle ty
  | needle == ty = True
  | otherwise =
      case ty of
        STArrow dom cod -> containsSrcType needle dom || containsSrcType needle cod
        STCon _ args -> any (containsSrcType needle) (toListNE args)
        STVarApp _ args -> any (containsSrcType needle) (toListNE args)
        STForall _ mb body -> maybe False (containsSrcType needle . unSrcBound) mb || containsSrcType needle body
        STMu _ body -> containsSrcType needle body
        STVar {} -> False
        STBase {} -> False
        STBottom -> False

containsVarAppHead :: String -> SrcType -> Bool
containsVarAppHead needle ty =
  case ty of
    STArrow dom cod -> containsVarAppHead needle dom || containsVarAppHead needle cod
    STCon _ args -> any (containsVarAppHead needle) (toListNE args)
    STVarApp name args -> name == needle || any (containsVarAppHead needle) (toListNE args)
    STForall _ mb body -> maybe False (containsVarAppHead needle . unSrcBound) mb || containsVarAppHead needle body
    STMu _ body -> containsVarAppHead needle body
    STVar {} -> False
    STBase {} -> False
    STBottom -> False

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

valueInfo :: ValueInfo
valueInfo =
  OrdinaryValue
    { valueDisplayName = "answer",
      valueInfoSymbol = SymbolIdentity SymbolValue "Lib" "answer" Nothing,
      valueRuntimeName = "Lib__answer",
      valueType = STBase "Int",
      valueIdentityType = STBase "Int",
      valueConstraints = [],
      valueConstraintInfos = [],
      valueOriginModule = "Lib"
    }

mainValueInfo :: ValueInfo
mainValueInfo =
  OrdinaryValue
    { valueDisplayName = "main",
      valueInfoSymbol = SymbolIdentity SymbolValue "Main" "main" Nothing,
      valueRuntimeName = "Main__main",
      valueType = STBase "Int",
      valueIdentityType = STBase "Int",
      valueConstraints = [],
      valueConstraintInfos = [],
      valueOriginModule = "Main"
    }

someCtor :: ConstructorInfo
someCtor =
  ConstructorInfo
    { ctorName = "Some",
      ctorInfoSymbol =
        SymbolIdentity
          SymbolConstructor
          "Lib"
          "Some"
          (Just (SymbolOwnerType "Lib" "Token")),
      ctorRuntimeName = "Lib__Some",
      ctorType = STBase "Token",
      ctorForalls = [],
      ctorArgs = [],
      ctorResult = STBase "Token",
      ctorOwningType = "Token",
      ctorOwningTypeIdentity = SymbolIdentity SymbolType "Lib" "Token" Nothing,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

tokenDataInfo :: DataInfo
tokenDataInfo =
  DataInfo
    { dataName = "Token",
      dataInfoSymbol = SymbolIdentity SymbolType "Lib" "Token" Nothing,
      dataModule = "Lib",
      dataTypeParams = [],
      dataParams = [],
      dataConstructors = [someCtor]
    }

higherCtor :: ConstructorInfo
higherCtor =
  ConstructorInfo
    { ctorName = "Higher",
      ctorInfoSymbol =
        SymbolIdentity
          SymbolConstructor
          "Lib"
          "Higher"
          (Just (SymbolOwnerType "Lib" "Higher")),
      ctorRuntimeName = "Lib__Higher",
      ctorType =
        STArrow
          (STVarApp "f" (STVar "a" :| []))
          (STCon "Higher" (STVar "f" :| [STVar "a"])),
      ctorForalls = [],
      ctorArgs = [STVarApp "f" (STVar "a" :| [])],
      ctorResult = STCon "Higher" (STVar "f" :| [STVar "a"]),
      ctorOwningType = "Higher",
      ctorOwningTypeIdentity = SymbolIdentity SymbolType "Lib" "Higher" Nothing,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

higherDataInfo :: DataInfo
higherDataInfo =
  DataInfo
    { dataName = "Higher",
      dataInfoSymbol = SymbolIdentity SymbolType "Lib" "Higher" Nothing,
      dataModule = "Lib",
      dataTypeParams = [firstOrderTypeParam "f", firstOrderTypeParam "a"],
      dataParams = ["f", "a"],
      dataConstructors = [higherCtor]
    }

eqMethodInfo :: MethodInfo
eqMethodInfo =
  MethodInfo
    { methodClassName = "Eq",
      methodInfoSymbol =
        SymbolIdentity
          SymbolMethod
          "Lib"
          "eq"
          (Just (SymbolOwnerClass "Lib" "Eq")),
      methodClassModule = "Lib",
      methodName = "eq",
      methodRuntimeBase = "Lib__Eq__eq",
      methodType = STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")),
      methodTypeIdentity = STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")),
      methodConstraints = [],
      methodConstraintInfos = [],
      methodTypeParam = firstOrderTypeParam "a",
      methodParamName = "a"
    }

qualifiedEqMethodInfo :: MethodInfo
qualifiedEqMethodInfo =
  eqMethodInfo {methodClassName = "L.Eq"}

eqClassInfo :: ClassInfo
eqClassInfo =
  ClassInfo
    { className = "Eq",
      classInfoSymbol = SymbolIdentity SymbolClass "Lib" "Eq" Nothing,
      classModule = "Lib",
      classTypeParam = firstOrderTypeParam "a",
      classParamName = "a",
      classMethods = Map.singleton "eq" eqMethodInfo
    }

qualifiedEqClassInfo :: ClassInfo
qualifiedEqClassInfo =
  ClassInfo
    { className = "L.Eq",
      classInfoSymbol = SymbolIdentity SymbolClass "Lib" "Eq" Nothing,
      classModule = "Lib",
      classTypeParam = firstOrderTypeParam "a",
      classParamName = "a",
      classMethods = Map.singleton "eq" qualifiedEqMethodInfo
    }

eqMethodValue :: ValueInfo
eqMethodValue =
  OverloadedMethod
    { valueDisplayName = "eq",
      valueInfoSymbol = methodInfoSymbolIdentity eqMethodInfo,
      valueMethodInfo = eqMethodInfo,
      valueOriginModule = "Lib"
    }

qualifiedEqMethodValue :: ValueInfo
qualifiedEqMethodValue =
  OverloadedMethod
    { valueDisplayName = "L.eq",
      valueInfoSymbol = methodInfoSymbolIdentity qualifiedEqMethodInfo,
      valueMethodInfo = qualifiedEqMethodInfo,
      valueOriginModule = "Lib"
    }
