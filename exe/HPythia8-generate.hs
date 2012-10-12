{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Executable  : HPythia8-generate
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generate source code for HPythia8  
--
-----------------------------------------------------------------------------

module Main where

main :: IO ()
main = putStrLn "Hello World" 

{- import System.IO
import System.Directory
import System.FilePath 
import System.Console.CmdArgs

import Text.StringTemplate hiding (render)

-- import HPythia8.Generate.ROOT
-- import HPythia8.Generate.ROOTAnnotate
-- import HPythia8.Generate.ROOTModule

import HPythia8.Generate.ROOTsmall
import HPythia8.Generate.ROOTAnnotatesmall
import HPythia8.Generate.ROOTModulesmall

import Bindings.Cxx.Generate.Generator.Driver
-- import Bindings.Cxx.Generate.Generator.Command hiding (config)
import Command

import Text.Parsec

import Bindings.Cxx.Generate.Config
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Code.Dependency
import Bindings.Cxx.Generate.Generator.ContentMaker 
import Bindings.Cxx.Generate.Code.Cabal
import Bindings.Cxx.Generate.Code.Cpp

import Distribution.Package
import Distribution.PackageDescription hiding (exposedModules)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version 

import Text.StringTemplate.Helpers

import Data.List 
import qualified Data.Map as M
import Data.Maybe

import Paths_HPythia8_generate
import qualified Paths_fficxx as F

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 

cabalTemplate :: String 
cabalTemplate = "HPythia8.cabal"

hprefix :: String 
hprefix = "HPythia8.Class"

pkgname :: String 
pkgname = "HPythia8"
-- cprefix :: String 
-- cprefix = "HPythia8"




mkCROOTIncludeHeaders :: Class 
                      -> [String] 
mkCROOTIncludeHeaders c = 
  case class_name c of
    "Deletable" -> [] 
    _ -> [(class_name c) ++ ".h"]



mkCabalFile :: FFICXXConfig -> Handle -> [ClassModule] -> IO () 
mkCabalFile config h classmodules = do 
  version <- getHPythia8Version config
  templateDir <- getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  let str = renderTemplateGroup 
              templates 
              [ ("version", version) 
              , ("csrcFiles", genCsrcFiles classmodules)
              , ("includeFiles", genIncludeFiles pkgname classmodules) 
              , ("cppFiles", genCppFiles classmodules)
              , ("exposedModules", genExposedModules hprefix classmodules) 
              , ("otherModules", genOtherModules hprefix classmodules)
              , ("cabalIndentation", cabalIndentation)
              ]
              cabalTemplate 
  hPutStrLn h str

getHPythia8Version :: FFICXXConfig -> IO String 
getHPythia8Version conf = do 
  let hrootgeneratecabal = fficxxconfig_scriptBaseDir conf </> "HPythia8-generate.cabal"
  gdescs <- readPackageDescription normal hrootgeneratecabal
  
  let vnums = versionBranch . pkgVersion . package . packageDescription $ gdescs 
  return $ intercalate "." (map show vnums)
--  putStrLn $ "version = " ++ show vnum



commandLineProcess :: HPythia8Generate -> IO () 
commandLineProcess (Generate conf) = do 
  putStrLn "Automatic HPythia8 binding generation" 
  str <- readFile conf 
  let config = case (parse fficxxconfigParse "" str) of 
                 Left msg -> error (show msg)
                 Right ans -> ans
  
  let workingDir = fficxxconfig_workingDir config 
      ibase = fficxxconfig_installBaseDir config
      cabalFileName = "HPythia8.cabal"

      (root_all_modules,root_all_classes_imports) = 
        mkAllClassModulesAndCIH (pkgname,mkCROOTIncludeHeaders) root_all_classes
  
  
  putStrLn "cabal file generation" 
  getHPythia8Version config
  withFile (workingDir </> cabalFileName) WriteMode $ 
    \h -> do 
      mkCabalFile config h root_all_modules 

  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 

  let cglobal = mkGlobal root_all_classes
      -- prefix = hprefix
   
  putStrLn "header file generation"
  writeTypeDeclHeaders templates cglobal workingDir pkgname root_all_classes_imports
  mapM_ (writeDeclHeaders templates cglobal workingDir pkgname) root_all_classes_imports

  putStrLn "cpp file generation" 
  mapM_ (writeCppDef templates workingDir) root_all_classes_imports

  putStrLn "RawType.hs file generation" 
  mapM_ (writeRawTypeHs templates workingDir hprefix) root_all_modules 

  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc templates workingDir hprefix) root_all_modules

  putStrLn "Interface.hs file generation" 
  mapM_ (writeInterfaceHs annotateMap templates workingDir hprefix) root_all_modules

  putStrLn "Cast.hs file generation"
  mapM_ (writeCastHs templates workingDir hprefix) root_all_modules

  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs annotateMap templates workingDir hprefix) root_all_modules

  putStrLn "module file generation" 
  mapM_ (writeModuleHs templates workingDir hprefix) root_all_modules

  putStrLn "HPythia8.hs file generation"
  writePkgHs (pkgname,hprefix) templates workingDir root_all_modules
  
  copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
  copyPredefined templateDir (srcDir ibase) pkgname
  mapM_ (copyCppFiles workingDir (csrcDir ibase) pkgname) root_all_classes_imports
  mapM_ (copyModule workingDir (srcDir ibase) hprefix pkgname) root_all_modules 
-}

