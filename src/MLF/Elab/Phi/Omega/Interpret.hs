{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Elab.Phi.Omega.Interpret
-- Description : Omega/Step interpretation flow for witness translation
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Public facade for the Omega\/Step interpretation flow used by the
-- Phi translation pipeline.
--
-- The full implementation lives in
-- "MLF.Elab.Phi.Omega.Interpret.Internal"; this module re-exports
-- the public entry point unchanged.
module MLF.Elab.Phi.Omega.Interpret
  ( phiWithSchemeOmega,
  )
where

import MLF.Elab.Phi.Omega.Interpret.Internal (phiWithSchemeOmega)
