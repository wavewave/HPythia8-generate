Name:		HPythia8
Version:	$version$
Synopsis:	Haskell FFI binding to Pythia8 
Description: 	Pythia 8 is a particle physics monte carlo event generator written in C++. HPythia8 provides haskell Foreign Function Interface (FFI) to Pythia 8 library. 
Homepage:       http://ianwookim.org/HPythia8
License: 	LGPL-2.1
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Category:       Graphics
Tested-with:    GHC >= 7.4.1
Build-Type: 	Custom
cabal-version:  >=1.10
Extra-source-files: 
$cabalIndentation$CHANGES
$cabalIndentation$Config.hs
$csrcFiles$

Source-repository head
  type:     git
  location: https://github.com/wavewave/HPythia8, https://github.com/wavewave/HPythia8-generate


Library
  default-language: Haskell2010
  hs-source-dirs: src
  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans
  ghc-prof-options: -caf-all -auto-all
  Build-Depends:      base>4 && < 5
  Exposed-Modules:
$cabalIndentation$HPythia8
$cabalIndentation$HPythia8.TypeCast
$exposedModules$  
  Other-Modules:
$otherModules$
  extra-libraries:    stdc++
  Include-dirs:       csrc
  Install-includes:   
$includeFiles$
  C-sources:          
$cppFiles$

   


