{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Traversable (forM)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Category ((>>>))
import Control.Arrow (second)
import Control.Monad.Trans.Except (runExceptT)
import System.FilePath (makeRelative, pathSeparator)
import System.FilePath.Glob (glob)
import System.Exit (exitFailure, ExitCode(..))
import System.Directory (getCurrentDirectory)
import System.Process (readProcessWithExitCode)
import qualified Language.PureScript as P
import Language.PureScript.Interactive (findNodeProcess)
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, parsePackageName)
import System.IO.UTF8 (readUTF8FileT)

import Language.PureScript.Docs.Doctest

dummy :: PackageName
Right dummy = parsePackageName "dummy"

inputFiles :: IO [FilePath]
inputFiles = glob "src/**/*.purs"

depsFiles :: IO [FilePath]
depsFiles = glob ".psc-package/psc-0.11.7/*/*/src/**/*.purs"

modulesDir :: FilePath
modulesDir = ".purs_doctest_modules" ++ pathSeparator : "node_modules"

indexFile :: FilePath
indexFile = ".purs_doctest_modules" ++ pathSeparator : "index.js"

main :: IO ()
main = do
  files <- (++) <$> inputFiles <*> depsFiles
  ms <- loadAllModules files >>= successOrExit

  egs <- collectExamples
  let examples = map examplesModule egs
  let entry = entryPointModule egs

  let allModules = map ("<internal>",) (entry : examples) ++ ms

  e <- liftIO . runMake $ make allModules
  putStrLn (show e)

collectExamples :: IO [Examples]
collectExamples = do
  input <- inputFiles
  deps <- map (dummy,) <$> depsFiles

  modules <- parseAndConvert input deps
  pure (map parseDoctests modules)

successOrExit :: Either P.MultipleErrors a -> IO a
successOrExit act =
  case act of
    Right x ->
      return x
    Left err -> do
      putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions err)
      exitFailure

parseAndConvert input deps =
  runExceptT (D.parseFilesInPackages input deps
             >>= uncurry D.convertModulesInPackage)
  >>= successOrExit

--- Copied from Language.PureScript.Interactive

-- | Load all modules.
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(FilePath, P.Module)])
loadAllModules files = do
  pwd <- getCurrentDirectory
  filesAndContent <- forM files $ \filename -> do
    content <- readUTF8FileT filename
    return (filename, content)
  return $ P.parseModulesFromFiles (makeRelative pwd) filesAndContent

-- | Pretty-print errors
printErrors :: MonadIO m => P.MultipleErrors -> m ()
printErrors errs = liftIO $ do
  pwd <- getCurrentDirectory
  putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

-- | Rebuild a module, using the cached externs data for dependencies.
rebuild
  :: [P.ExternsFile]
  -> P.Module
  -> P.Make (P.ExternsFile, P.Environment)
rebuild loadedExterns m = do
    externs <- P.rebuildModule buildActions loadedExterns m
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment (loadedExterns ++ [externs]))
  where
    buildActions :: P.MakeActions P.Make
    buildActions =
      (P.buildMakeActions modulesDir
                          filePathMap
                          Map.empty
                          False) { P.progress = const (return ()) }

    filePathMap :: Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = Map.singleton (P.getModuleName m) (Left P.RebuildAlways)

-- | Build the collection of modules from scratch. This is usually done on startup.
make
  :: [(FilePath, P.Module)]
  -> P.Make ([P.ExternsFile], P.Environment)
make ms = do
    foreignFiles <- P.inferForeignModules filePathMap
    externs <- P.make (buildActions foreignFiles) (map snd ms)
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs)
  where
    buildActions :: Map P.ModuleName FilePath -> P.MakeActions P.Make
    buildActions foreignFiles =
      P.buildMakeActions modulesDir
                         filePathMap
                         foreignFiles
                         False

    filePathMap :: Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = Map.fromList $ map (\(fp, m) -> (P.getModuleName m, Right fp)) ms

evaluate :: IO ()
evaluate = do
  writeFile indexFile "require('$Doctest')['main']();"
  process <- findNodeProcess -- maybe findNodeProcess (pure . pure) nodePath
  -- result <- traverse (\node -> readProcessWithExitCode node (nodeArgs ++ [indexFile]) "") process
  result <- traverse (\node -> readProcessWithExitCode node [indexFile] "") process
  case result of
    Just (ExitSuccess, out, _)   -> putStrLn out
    Just (ExitFailure _, _, err) -> putStrLn err
    Nothing                      -> putStrLn "Couldn't find node.js"
