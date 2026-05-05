module Parity.FrozenArtifacts (
    FrozenMetadata(..),
    FrozenAnchorResult(..),
    FrozenBaseline(..),
    baselineSchemaVersion,
    baselineFilePath,
    parityAnchorNames,
    mkMetadata,
    metadataFromRenderedJson,
    buildFrozenBaseline,
    renderFrozenBaselineJson
) where

import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, stripPrefix)
import qualified Data.Set as Set

import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution (computePresolution)
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Elab.Pipeline (defaultTraceConfig, prettyDisplay, runPipelineElab)
import MLF.Frontend.ConstraintGen (ConstraintResult(..), generateConstraints)
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Frontend.Syntax
    ( Expr(..)
    , Lit(..)
    , SrcTy(..)
    , SrcType
    , SurfaceExpr
    , mkSrcBound
    )
import qualified SolvedFacadeTestUtil as SolvedTest
baselineSchemaVersion :: Int
baselineSchemaVersion = 1

baselineFilePath :: FilePath
baselineFilePath = "test/golden/legacy-replay-baseline-v1.json"

data FrozenMetadata = FrozenMetadata
    { fmSchemaVersion :: Int
    , fmGeneratedOn :: String
    , fmSourceCommit :: String
    , fmAnchorNames :: [String]
    }
    deriving (Eq, Show)

data FrozenAnchorResult = FrozenAnchorResult
    { farName :: String
    , farCanonicalConstraint :: String
    , farOriginalConstraint :: String
    , farCanonicalMap :: [(Int, Int)]
    , farElaboratedType :: String
    }
    deriving (Eq, Show)

data FrozenBaseline = FrozenBaseline
    { fbMetadata :: FrozenMetadata
    , fbAnchors :: [FrozenAnchorResult]
    }
    deriving (Eq, Show)

mkMetadata :: String -> String -> FrozenMetadata
mkMetadata generatedOn sourceCommit =
    FrozenMetadata
        { fmSchemaVersion = baselineSchemaVersion
        , fmGeneratedOn = generatedOn
        , fmSourceCommit = sourceCommit
        , fmAnchorNames = parityAnchorNames
        }

parityAnchors :: [(String, SurfaceExpr)]
parityAnchors =
    let rhs = ELam "x" (ELam "y" (EVar "x"))
        schemeTy =
            mkForalls
                [ ("a", Nothing)
                , ("b", Just (STVar "a"))
                ]
                (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
        ann =
            STForall "a" Nothing
                (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
    in [ ("id", ELam "x" (EVar "x"))
       , ("id-app-int", EApp (ELam "x" (EVar "x")) (ELit (LInt 1)))
       , ("let-id", ELet "id" (ELam "x" (EVar "x")) (EVar "id"))
       , ( "let-poly-two-use"
         , ELet "id" (ELam "x" (EVar "x"))
                (ELet "_" (EApp (EVar "id") (ELit (LInt 1)))
                    (EApp (EVar "id") (ELit (LBool True))))
         )
       , ( "a6-bounded-alias-coercion"
         , ELet "c" (EAnn rhs schemeTy)
                (EAnn (EVar "c") ann)
         )
       ]

parityAnchorNames :: [String]
parityAnchorNames = map fst parityAnchors

buildFrozenBaseline :: FrozenMetadata -> Either String FrozenBaseline
buildFrozenBaseline metadata = do
    let expectedAnchors = parityAnchorNames
    if fmAnchorNames metadata /= expectedAnchors
        then
            Left
                ( "Frozen metadata anchor mismatch. expected="
                    ++ show expectedAnchors
                    ++ " actual="
                    ++ show (fmAnchorNames metadata)
                )
        else pure ()
    anchors <- traverse buildAnchorResult parityAnchors
    pure FrozenBaseline
        { fbMetadata = metadata
        , fbAnchors = anchors
        }

buildAnchorResult :: (String, SurfaceExpr) -> Either String FrozenAnchorResult
buildAnchorResult (label, expr) = do
    norm <- firstShowE (normalizeExpr expr)
    ConstraintResult { crConstraint = c0 } <- firstShowE (generateConstraints Set.empty norm)
    let c1 = normalize c0
    (cAcyclic, acyc) <- firstShowE (checkAcyclicity c1)
    pres <- firstShowE (computePresolution defaultTraceConfig acyc cAcyclic)
    solved <- firstShowE (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
    (_, ty) <- firstShowE (runPipelineElab Set.empty norm)
    pure FrozenAnchorResult
        { farName = label
        , farCanonicalConstraint = show (Solved.canonicalConstraint solved)
        , farOriginalConstraint = show (Solved.originalConstraint solved)
        , farCanonicalMap = canonicalMapPairs (Solved.canonicalMap solved)
        , farElaboratedType = prettyDisplay ty
        }

canonicalMapPairs :: IntMap.IntMap NodeId -> [(Int, Int)]
canonicalMapPairs uf =
    [ (k, nodeIdToInt nid)
    | (k, nid) <- IntMap.toAscList uf
    ]

nodeIdToInt :: NodeId -> Int
nodeIdToInt (NodeId n) = n

mkForalls :: [(String, Maybe SrcType)] -> SrcType -> SrcType
mkForalls binds body =
    foldr
        (\(name, mbBound) acc -> STForall name (fmap mkSrcBound mbBound) acc)
        body
        binds

firstShowE :: Show e => Either e a -> Either String a
firstShowE = either (Left . show) Right

metadataFromRenderedJson :: String -> Either String FrozenMetadata
metadataFromRenderedJson json = do
    schemaVersion <- extractIntField "schema_version" json
    generatedOn <- extractStringField "generated_on" json
    sourceCommit <- extractStringField "source_commit" json
    pure
        FrozenMetadata
            { fmSchemaVersion = schemaVersion
            , fmGeneratedOn = generatedOn
            , fmSourceCommit = sourceCommit
            , fmAnchorNames = parityAnchorNames
            }

extractStringField :: String -> String -> Either String String
extractStringField key text =
    case findAfter ("\"" ++ key ++ "\": \"") text of
        Nothing -> Left ("missing JSON string field: " ++ key)
        Just rest -> Right (takeWhile (/= '"') rest)

extractIntField :: String -> String -> Either String Int
extractIntField key text =
    case findAfter ("\"" ++ key ++ "\": ") text of
        Nothing -> Left ("missing JSON int field: " ++ key)
        Just rest ->
            let digits = takeWhile isDigit rest
            in if null digits
                then Left ("invalid JSON int field: " ++ key)
                else Right (read digits)

findAfter :: String -> String -> Maybe String
findAfter needle = go
  where
    go haystack =
        case stripPrefix needle haystack of
            Just rest -> Just rest
            Nothing ->
                case haystack of
                    [] -> Nothing
                    (_ : rest) -> go rest

renderFrozenBaselineJson :: FrozenBaseline -> String
renderFrozenBaselineJson baseline = renderJValue 0 (baselineToJson baseline) ++ "\n"

baselineToJson :: FrozenBaseline -> JValue
baselineToJson FrozenBaseline { fbMetadata = meta, fbAnchors = anchors } =
    JObject
        [ ("schema_version", JInt (fmSchemaVersion meta))
        , ("generated_on", JString (fmGeneratedOn meta))
        , ("source_commit", JString (fmSourceCommit meta))
        , ("anchors", JArray (map JString (fmAnchorNames meta)))
        , ("results", JArray (map anchorToJson anchors))
        ]

anchorToJson :: FrozenAnchorResult -> JValue
anchorToJson anchor =
    JObject
        [ ("name", JString (farName anchor))
        , ("canonical_constraint", JString (farCanonicalConstraint anchor))
        , ("original_constraint", JString (farOriginalConstraint anchor))
        , ("canonical_map", JArray (map pairToJson (farCanonicalMap anchor)))
        , ("elaborated_type", JString (farElaboratedType anchor))
        ]

pairToJson :: (Int, Int) -> JValue
pairToJson (a, b) = JArray [JInt a, JInt b]

data JValue
    = JObject [(String, JValue)]
    | JArray [JValue]
    | JString String
    | JInt Int

renderJValue :: Int -> JValue -> String
renderJValue indentLevel value =
    case value of
        JObject fields -> renderObject indentLevel fields
        JArray vals -> renderArray indentLevel vals
        JString s -> "\"" ++ escapeJson s ++ "\""
        JInt n -> show n

renderObject :: Int -> [(String, JValue)] -> String
renderObject indentLevel fields =
    let indent = replicate indentLevel ' '
        innerIndent = replicate (indentLevel + 2) ' '
        renderField (key, val) =
            innerIndent
                ++ "\""
                ++ escapeJson key
                ++ "\": "
                ++ renderJValue (indentLevel + 2) val
    in case fields of
        [] -> "{}"
        _ ->
            "{\n"
                ++ intercalate ",\n" (map renderField fields)
                ++ "\n"
                ++ indent
                ++ "}"

renderArray :: Int -> [JValue] -> String
renderArray indentLevel values =
    case values of
        [] -> "[]"
        _ ->
            let indent = replicate indentLevel ' '
                innerIndent = replicate (indentLevel + 2) ' '
                renderItem val = innerIndent ++ renderJValue (indentLevel + 2) val
            in "[\n"
                ++ intercalate ",\n" (map renderItem values)
                ++ "\n"
                ++ indent
                ++ "]"

escapeJson :: String -> String
escapeJson = concatMap escapeChar
  where
    escapeChar ch = case ch of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> [ch]
