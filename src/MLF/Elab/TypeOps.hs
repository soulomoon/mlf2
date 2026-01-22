module MLF.Elab.TypeOps (
    splitForalls,
    stripForallsType,
    freeTypeVarsFrom,
    freeTypeVarsType,
    freeTypeVarsList,
    substTypeCapture,
    substTypeSimple,
    renameTypeVar,
    freshNameLike,
    freshTypeName,
    freshTypeNameFromCounter,
    alphaEqType,
    matchType,
    parseNameId,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    inlineBaseBoundsType
) where

import MLF.Reify.TypeOps (
    alphaEqType,
    freshNameLike,
    freshTypeName,
    freshTypeNameFromCounter,
    freeTypeVarsFrom,
    freeTypeVarsList,
    freeTypeVarsType,
    inlineBaseBoundsType,
    matchType,
    parseNameId,
    renameTypeVar,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    splitForalls,
    stripForallsType,
    substTypeCapture,
    substTypeSimple
    )
