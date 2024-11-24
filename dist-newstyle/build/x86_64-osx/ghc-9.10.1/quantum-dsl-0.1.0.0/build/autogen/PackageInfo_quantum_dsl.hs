{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_quantum_dsl (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "quantum_dsl"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Domain-Specific Language for Quantum Computing Algorithms"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
