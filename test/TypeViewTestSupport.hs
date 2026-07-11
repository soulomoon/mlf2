module TypeViewTestSupport
  ( mkTypeView,
    fixtureTypeView,
    setTypeViewDisplay,
    setTypeViewIdentity,
    setTypeViewTypes,
    setTypeViewHeadIdentities,
    setTypeViewBinderIdentities,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MLF.Frontend.Program.Types
  ( SymbolIdentity,
    TypeView,
    typeViewBinderIdentities,
    typeViewDisplay,
    typeViewFromProjections,
    typeViewHeadIdentities,
    typeViewIdentity,
  )
import MLF.Frontend.Syntax (SrcType)
import MLF.Types.Identity (TypeBinderIdentity)

mkTypeView :: SrcType -> SrcType -> TypeView
mkTypeView display identity =
  fixtureTypeView display identity Map.empty Map.empty

fixtureTypeView :: SrcType -> SrcType -> Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeView
fixtureTypeView display identity headIdentities binderIdentities =
  case typeViewFromProjections display identity headIdentities binderIdentities of
    Right view -> view
    Left err -> error ("invalid TypeView test fixture: " ++ show err)

setTypeViewDisplay :: SrcType -> TypeView -> TypeView
setTypeViewDisplay display view =
  setTypeViewTypes display (typeViewIdentity view) view

setTypeViewIdentity :: SrcType -> TypeView -> TypeView
setTypeViewIdentity identity view =
  setTypeViewTypes (typeViewDisplay view) identity view

setTypeViewTypes :: SrcType -> SrcType -> TypeView -> TypeView
setTypeViewTypes display identity view =
  fixtureTypeView
    display
    identity
    (typeViewHeadIdentities view)
    (typeViewBinderIdentities view)

setTypeViewHeadIdentities :: Map String SymbolIdentity -> TypeView -> TypeView
setTypeViewHeadIdentities headIdentities view =
  fixtureTypeView
    (typeViewDisplay view)
    (typeViewIdentity view)
    headIdentities
    (typeViewBinderIdentities view)

setTypeViewBinderIdentities :: Map String TypeBinderIdentity -> TypeView -> TypeView
setTypeViewBinderIdentities binderIdentities view =
  fixtureTypeView
    (typeViewDisplay view)
    (typeViewIdentity view)
    (typeViewHeadIdentities view)
    binderIdentities
