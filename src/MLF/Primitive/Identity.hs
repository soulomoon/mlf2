module MLF.Primitive.Identity
  ( builtinModuleName,
    builtinTypeNames,
    builtinTypeIdentity,
    builtinTypeHeadIdentity,
    builtinValueIdentity,
    isBuiltinTypeName,
    normalizeBuiltinTypeReference,
  )
where

import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), symbolIdentityFromParts)
import MLF.Types.Unique (UniqueIdentity (..), uniqueIdentityStableName)

builtinModuleName :: String
builtinModuleName = "<builtin>"

builtinTypeNames :: Set String
builtinTypeNames =
  Map.keysSet builtinTypeUniqueIdentities

isBuiltinTypeName :: String -> Bool
isBuiltinTypeName =
  isBuiltinTypeNameRaw . canonicalBuiltinTypeReference

isBuiltinTypeNameRaw :: String -> Bool
isBuiltinTypeNameRaw = (`Set.member` builtinTypeNames)

normalizeBuiltinTypeReference :: String -> String
normalizeBuiltinTypeReference name =
  case stripPrefix (builtinModuleName ++ ".") name of
    Just builtinName
      | isBuiltinTypeNameRaw builtinName -> builtinName
    _ -> Map.findWithDefault name name builtinTypeStableNames

canonicalBuiltinTypeReference :: String -> String
canonicalBuiltinTypeReference name =
  case stripPrefix (builtinModuleName ++ ".") name of
    Just builtinName
      | isBuiltinTypeNameRaw builtinName -> builtinName
    _ -> name

builtinTypeIdentity :: String -> SymbolIdentity
builtinTypeIdentity name =
    symbolIdentityFromParts (builtinTypeUniqueIdentity canonical) SymbolType builtinModuleName canonical Nothing
  where
    canonical = canonicalBuiltinTypeReference name

builtinTypeHeadIdentity :: String -> Maybe SymbolIdentity
builtinTypeHeadIdentity name
  | isBuiltinTypeName canonical = Just (builtinTypeIdentity canonical)
  | otherwise = Nothing
  where
    canonical = canonicalBuiltinTypeReference name

builtinTypeUniqueIdentity :: String -> UniqueIdentity
builtinTypeUniqueIdentity name =
  Map.findWithDefault missingBuiltinIdentity name builtinTypeUniqueIdentities
  where
    missingBuiltinIdentity =
      error ("missing builtin identity for SymbolType `" ++ name ++ "`")

builtinTypeUniqueIdentities :: Map String UniqueIdentity
builtinTypeUniqueIdentities =
  Map.fromList
    [ ("Bool", UniqueIdentity (-100000)),
      ("Char", UniqueIdentity (-100001)),
      ("IO", UniqueIdentity (-100002)),
      ("IORef", UniqueIdentity (-100003)),
      ("Int", UniqueIdentity (-100004)),
      ("String", UniqueIdentity (-100005))
    ]

builtinTypeStableNames :: Map String String
builtinTypeStableNames =
  Map.fromList
    [ (uniqueIdentityStableName unique, name)
    | (name, unique) <- Map.toList builtinTypeUniqueIdentities
    ]

builtinValueIdentity :: String -> SymbolIdentity
builtinValueIdentity name =
  symbolIdentityFromParts (builtinValueUniqueIdentity canonical) SymbolValue builtinModuleName canonical Nothing
  where
    canonical = canonicalBuiltinValueReference name

normalizeBuiltinValueReference :: String -> String
normalizeBuiltinValueReference name =
  case stripPrefix (builtinModuleName ++ ".") name of
    Just builtinName
      | Map.member builtinName builtinValueUniqueIdentities -> builtinName
    _ -> name

canonicalBuiltinValueReference :: String -> String
canonicalBuiltinValueReference = normalizeBuiltinValueReference

builtinValueUniqueIdentity :: String -> UniqueIdentity
builtinValueUniqueIdentity name =
  Map.findWithDefault missingBuiltinIdentity name builtinValueUniqueIdentities
  where
    missingBuiltinIdentity =
      error ("missing builtin identity for SymbolValue `" ++ name ++ "`")

builtinValueUniqueIdentities :: Map String UniqueIdentity
builtinValueUniqueIdentities =
  Map.fromList
    [ ("__char_is_ascii_alpha", UniqueIdentity (-200000)),
      ("__char_is_ascii_alpha_num", UniqueIdentity (-200001)),
      ("__char_is_ascii_control", UniqueIdentity (-200002)),
      ("__char_is_ascii_hex_digit", UniqueIdentity (-200003)),
      ("__char_is_ascii_identifier_continue", UniqueIdentity (-200004)),
      ("__char_is_ascii_identifier_start", UniqueIdentity (-200005)),
      ("__char_is_ascii_line_break", UniqueIdentity (-200006)),
      ("__char_is_ascii_lower", UniqueIdentity (-200007)),
      ("__char_is_ascii_printable", UniqueIdentity (-200008)),
      ("__char_is_ascii_punctuation", UniqueIdentity (-200009)),
      ("__char_is_ascii_upper", UniqueIdentity (-200010)),
      ("__char_is_ascii_whitespace", UniqueIdentity (-200011)),
      ("__char_is_digit", UniqueIdentity (-200012)),
      ("__char_to_ascii_lower", UniqueIdentity (-200013)),
      ("__char_to_ascii_upper", UniqueIdentity (-200014)),
      ("__io_ap", UniqueIdentity (-200015)),
      ("__io_appendFile", UniqueIdentity (-200016)),
      ("__io_bind", UniqueIdentity (-200017)),
      ("__io_exitWith", UniqueIdentity (-200018)),
      ("__io_getArgs", UniqueIdentity (-200019)),
      ("__io_getLine", UniqueIdentity (-200020)),
      ("__io_map", UniqueIdentity (-200021)),
      ("__io_newIORef", UniqueIdentity (-200022)),
      ("__io_pure", UniqueIdentity (-200023)),
      ("__io_putStr", UniqueIdentity (-200024)),
      ("__io_putStrLn", UniqueIdentity (-200025)),
      ("__io_readFile", UniqueIdentity (-200026)),
      ("__io_readIORef", UniqueIdentity (-200027)),
      ("__io_writeFile", UniqueIdentity (-200028)),
      ("__io_writeIORef", UniqueIdentity (-200029)),
      ("__mlfp_and", UniqueIdentity (-200030)),
      ("__string_append", UniqueIdentity (-200031)),
      ("__string_char_at", UniqueIdentity (-200032)),
      ("__string_char_at_option", UniqueIdentity (-200033)),
      ("__string_compare", UniqueIdentity (-200034)),
      ("__string_contains", UniqueIdentity (-200035)),
      ("__string_contains_char", UniqueIdentity (-200036)),
      ("__string_drop", UniqueIdentity (-200037)),
      ("__string_ends_with", UniqueIdentity (-200038)),
      ("__string_equals", UniqueIdentity (-200039)),
      ("__string_from_bool", UniqueIdentity (-200040)),
      ("__string_from_char", UniqueIdentity (-200041)),
      ("__string_from_int", UniqueIdentity (-200042)),
      ("__string_from_list", UniqueIdentity (-200043)),
      ("__string_from_nat", UniqueIdentity (-200044)),
      ("__string_index_of", UniqueIdentity (-200045)),
      ("__string_index_of_char", UniqueIdentity (-200046)),
      ("__string_is_empty", UniqueIdentity (-200047)),
      ("__string_join", UniqueIdentity (-200048)),
      ("__string_length", UniqueIdentity (-200049)),
      ("__string_replace", UniqueIdentity (-200050)),
      ("__string_replace_char", UniqueIdentity (-200051)),
      ("__string_slice", UniqueIdentity (-200052)),
      ("__string_split", UniqueIdentity (-200053)),
      ("__string_split_char", UniqueIdentity (-200054)),
      ("__string_starts_with", UniqueIdentity (-200055)),
      ("__string_take", UniqueIdentity (-200056)),
      ("__string_to_ascii_lower", UniqueIdentity (-200057)),
      ("__string_to_ascii_upper", UniqueIdentity (-200058)),
      ("__string_to_list", UniqueIdentity (-200059))
    ]
