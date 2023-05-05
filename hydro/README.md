# Hydro

This directory contains two sub-folders. The first is /HaskellHydro, which contains my attempt at Hydroflow code generation from Haskell by modeling channels as applicative arrows.
The second is /hydro, which is simply a blank hydroflow project for testing HaskellHydro generated outputs.

To view the Channel GADT and typeclass implementations, see /HaskellHydro/src/HydroArrow.hs. To see the Rust generator, see /HaskellHydro/src/RustGenerator.hs. 

To play around with and write your own Channels, see /HaskellHydro/src/Lib.hs.
