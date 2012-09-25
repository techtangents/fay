{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Fay.Compiler where

import Language.Fay                 (compileToplevelModule, compileViaStr,generateMapping)
import Language.Fay.Print
import Language.Fay.Types

import Control.Monad
import Language.Haskell.Exts.Syntax
import Paths_fay
import System.FilePath
import Text.Groom

-- | A result of something the compiler writes.
class Writer a where
  writeout :: a -> String -> IO ()

-- | Something to feed into the compiler.
class Reader a where
  readin :: a -> IO String

-- | Simple file writer.
instance Writer FilePath where
  writeout = writeFile

-- | Simple file reader.
instance Reader FilePath where
  readin = readFile

-- | Compile file program to…
compileFromTo :: CompileConfig -> FilePath -> FilePath -> IO ()
compileFromTo config filein fileout = do
  result <- compileFromToReturningStatus config filein fileout
  case result of
    Right () -> return ()
    Left err -> error . groom $ err

-- | Compile file program to…
compileFromToReturningStatus :: CompileConfig -> FilePath -> FilePath -> IO (Either CompileError ())
compileFromToReturningStatus config filein fileout = do
  result <- compileFile config { configFilePath = Just filein } filein fileout
  case result of
    Right (mapping,(top,bottom)) -> do
      writeFile fileout (top++bottom)
      when (configHtmlWrapper config) $
        writeFile (replaceExtension fileout "html") $ unlines [
            "<!doctype html>"
          , "<html>"
          , "  <head>"
          ,"    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
          , unlines . map ("    "++) . map makeScriptTagSrc $ configHtmlJSLibs config
          , "    " ++ makeScriptTagSrc relativeJsPath
          , "    </script>"
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"]
      when (configSourceMap config) $ do
        generateMapping (length (lines top)) filein fileout mapping >>= writeFile (fileout ++ ".map")
      return (Right ())
            where relativeJsPath = makeRelative (dropFileName fileout) fileout
                  makeScriptTagSrc :: FilePath -> String
                  makeScriptTagSrc = \s ->
                    "<script type=\"text/javascript\" src=\"" ++ s ++ "\"></script>"
    Left err -> return (Left err)

-- | Compile readable/writable values.
compileReadWrite :: (Reader r, Writer w) => CompileConfig -> r -> w -> IO ()
compileReadWrite config reader writer = do
  result <- compileFile config reader ""
  case result of
    Right (_,(top,bottom)) -> do
      writeout writer (top++bottom)
    Left err -> error . groom $ err

-- | Compile the given file.
compileFile :: (Reader r) => CompileConfig -> r -> FilePath -> IO (Either CompileError ([Mapping],(String,String)))
compileFile config filein fileout = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readin filein
  compileProgram config
                 raw
                 compileToplevelModule
                 (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)
                 fileout

  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable program.
compileProgram :: (Show from,Show to,CompilesTo from to)
               => CompileConfig -> String -> (from -> Compile to) -> String
               -> FilePath
               -> IO (Either CompileError ([Mapping],(String,String)))
compileProgram config raw with hscode fileout = do
  result <- compileViaStr config with hscode
  case result of
    Left err -> return (Left err)
    Right (ps@PrintState{..},state) ->
      return $ Right $ (psMapping
                       ,generate (concat (reverse psOutput))
                                 (stateExports state)
                                 (stateModuleName state))

  where generate jscode exports (ModuleName (clean -> modulename)) =
          (unlines (["//@ sourceMappingURL=" ++ fileout ++ ".map" | configSourceMap config ] ++
                    ["/** @constructor"
                    ,"*/"
                    ,"var " ++ modulename ++ " = function(){"
                    ,raw])
          ,unlines [jscode
                   ,"// Exports"
                   ,unlines (map printExport exports)
                   ,"// Built-ins"
                   ,"this._ = _;"
                   ,if configExportBuiltins config
                       then unlines ["this.$           = $;"
                                    ,"this.$fayToJs    = Fay$$fayToJs;"
                                    ,"this.$jsToFay    = Fay$$jsToFay;"
                                    ]
                       else ""
                   ,"};"
                   ,if not (configLibrary config)
                       then unlines [";"
                                    ,"var main = new " ++ modulename ++ "();"
                                    ,"main._(main.main);"
                                    ]
                       else ""
                   ])
        clean ('.':cs) = '$' : clean cs
        clean (c:cs)   = c : clean cs
        clean [] = []

-- | Print an this.x = x; export out.
printExport :: Name -> String
printExport name =
  printJSString (JsSetProp ":this"
                           (UnQual name)
                           (JsName (UnQual name)))

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x
