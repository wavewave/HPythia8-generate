{-# LANGUAGE DeriveDataTypeable #-}

module Command where

import System.Console.CmdArgs

data HPythia8Generate = Generate { config :: FilePath } 
            deriving (Show,Data,Typeable)

generate :: HPythia8Generate
generate = Generate { config = "HPythia8.conf" } 

mode :: HPythia8Generate
mode = modes [generate] 

