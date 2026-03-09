module MLF.Reify.Cache (
    ReifyMode(..),
    ReifyCache(..),
    emptyCache,
    cacheLookup,
    cacheInsert
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Types.Elab (ElabType)

data ReifyMode
    = ModeType
    | ModeTypeNoFallback
    | ModeBound
    deriving (Eq)

data ReifyCache = ReifyCache
    { cacheType :: IntMap.IntMap ElabType
    , cacheTypeNoFallback :: IntMap.IntMap ElabType
    , cacheBound :: IntMap.IntMap ElabType
    , cacheInProgress :: IntSet.IntSet
    }

emptyCache :: ReifyCache
emptyCache = ReifyCache IntMap.empty IntMap.empty IntMap.empty IntSet.empty

cacheLookup :: ReifyMode -> ReifyCache -> Int -> Maybe ElabType
cacheLookup mode cache key = case mode of
    ModeType -> IntMap.lookup key (cacheType cache)
    ModeTypeNoFallback -> IntMap.lookup key (cacheTypeNoFallback cache)
    ModeBound -> IntMap.lookup key (cacheBound cache)

cacheInsert :: ReifyMode -> Int -> ElabType -> ReifyCache -> ReifyCache
cacheInsert mode key ty cache = case mode of
    ModeType -> cache { cacheType = IntMap.insert key ty (cacheType cache) }
    ModeTypeNoFallback -> cache { cacheTypeNoFallback = IntMap.insert key ty (cacheTypeNoFallback cache) }
    ModeBound -> cache { cacheBound = IntMap.insert key ty (cacheBound cache) }
