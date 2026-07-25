module MLF.Frontend.Program.Checked
  ( CheckedProgram,
    checkedProgramModules,
    checkedProgramMainResolvedVar,
    checkedProgramResolved,
    checkedProgramMain,
    mkCheckedProgram,
    mapCheckedProgramModules,
  )
where

import MLF.Types.Elab (resolvedVarRuntimeName)
import MLF.Frontend.Program.Checked.Internal
import MLF.Frontend.Program.Types

checkedProgramModules :: CheckedProgram -> [CheckedModule]
checkedProgramModules = checkedProgramModulesInternal

checkedProgramMainResolvedVar :: CheckedProgram -> ResolvedVar
checkedProgramMainResolvedVar = checkedProgramMainResolvedVarInternal

checkedProgramResolved :: CheckedProgram -> ResolvedProgram
checkedProgramResolved = checkedProgramResolvedInternal

mkCheckedProgram :: ResolvedProgram -> [CheckedModule] -> ResolvedVar -> CheckedProgram
mkCheckedProgram resolved modules mainResolved =
  CheckedProgram
    { checkedProgramModulesInternal = modules,
      checkedProgramMainResolvedVarInternal = mainResolved,
      checkedProgramResolvedInternal = resolved
    }

mapCheckedProgramModules :: ([CheckedModule] -> [CheckedModule]) -> CheckedProgram -> CheckedProgram
mapCheckedProgramModules f checked =
  mkCheckedProgram
    (checkedProgramResolved checked)
    (f (checkedProgramModules checked))
    (checkedProgramMainResolvedVar checked)

checkedProgramMain :: CheckedProgram -> String
checkedProgramMain =
  resolvedVarRuntimeName . checkedProgramMainResolvedVar
