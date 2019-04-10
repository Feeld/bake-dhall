{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main (main) where

import           BakeDhall                  (eval, evalWithValue, exprFromFile,
                                             exprFromTextPure, renderExpr)

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Archive.Tar.Entry    as Tar
import qualified Codec.Compression.BZip     as BZip
import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.List                  as List
import qualified Data.Yaml
import qualified Dhall
import           Options.Applicative.Simple
import           Protolude
import           System.Directory           (listDirectory)
import           System.FilePath
import           System.IO.Error            (userError)

main :: IO ()
main = Dhall.detailed $ evaluateCommand =<< parseCommand

data Command
  = Pack   PackOptions
  | Unpack   UnpackOptions
  | Template TemplateOptions
  | Evaluate

data UnpackOptions = UnpackOptions
  { outputYamlPath   :: FilePath
  , inputPackagePath :: FilePath
  , jsonConfigPath   :: FilePath
  }

data PackOptions = PackOptions
  { outputPackagePath    :: FilePath
  , inputTemplateDirPath :: FilePath
  }

data TemplateOptions = TemplateOptions
  { outputYamlPath  :: FilePath
  , templateDirPath :: FilePath
  , jsonConfigPath  :: FilePath
  }

schemaFilename :: FilePath
schemaFilename = "Config.dhall"

evaluateCommand :: Command -> IO ()
evaluateCommand (Template opts) = evaluateTemplate opts
evaluateCommand Evaluate        = evaluateStdin
evaluateCommand (Pack opts)     = evaluatePack opts
evaluateCommand (Unpack opts)   = evaluateUnpack opts

evaluateTemplate :: TemplateOptions -> IO ()
evaluateTemplate TemplateOptions{outputYamlPath,templateDirPath,jsonConfigPath} = do
  schemaExpr <- exprFromFile $ templateDirPath </> schemaFilename
  cfgValue <- withInputFileOrStdin jsonConfigPath $ \inH ->
    either (throwIO . userError) pure =<< (eitherDecode @Value . toS <$> LBS.hGetContents inH)
  withOutputFileOrStdout outputYamlPath $ \outH -> do
    paths <- sort . map (templateDirPath </>) <$> listDirectory templateDirPath
    forM_ paths $ \pth ->
      when (isOuputProducingPath schemaFilename (takeFileName pth)) $ do
        hPutStrLn stderr $ "Processing " <> pth
        yaml <- Data.Yaml.encode
            <$> (evalWithValue schemaExpr cfgValue =<< exprFromFile pth)
        hPutStrLn outH $ "---\n# Source: " <> toS (takeFileName pth) <> "\n" <> yaml

evaluateUnpack :: UnpackOptions -> IO ()
evaluateUnpack UnpackOptions{outputYamlPath,inputPackagePath,jsonConfigPath} =
  withInputFileOrStdin inputPackagePath $ \inH -> do
    eEntries <- Tar.foldEntries (\a -> fmap (a:)) (pure mempty) Left
              . Tar.read . BZip.decompress
            <$> LBS.hGetContents inH
    entries <- either (throwIO . userError . show) pure eEntries
    schemaEntry <- maybe (throwIO (userError "Config.yaml not found in archive")) pure
                  (List.find ((==schemaFilename) . Tar.entryPath) entries)
    schemaExpr <- either (throwIO . userError . toS) pure =<< (exprFromTextPure <$> entryText schemaEntry)
    cfgValue <- withInputFileOrStdin jsonConfigPath $ \cfgH ->
      either (throwIO . userError) pure =<< (eitherDecode @Value . toS <$> LBS.hGetContents cfgH)
    withOutputFileOrStdout outputYamlPath $ \outH ->
      forM_ (sortBy (compare `on` Tar.entryPath) entries) $ \entry -> do
        let pth = Tar.entryPath entry
        when (isOuputProducingPath schemaFilename (takeFileName pth)) $ do
          expr <- either (throwIO . userError . toS) pure =<< (exprFromTextPure <$> entryText entry)
          yaml <- Data.Yaml.encode <$> evalWithValue schemaExpr cfgValue expr
          hPutStrLn outH $ "---\n# Source: " <> toS (takeFileName pth) <> "\n" <> yaml

  where
  entryText :: Tar.Entry -> IO Text
  entryText Tar.Entry{entryContent=Tar.NormalFile x _} = pure (toS x)
  entryText e = throwIO $ userError $ "Expected " <> Tar.entryPath e <> " to be a normal file"

evaluatePack :: PackOptions -> IO ()
evaluatePack PackOptions{outputPackagePath,inputTemplateDirPath} = do
  paths <- map (inputTemplateDirPath </>) <$> listDirectory inputTemplateDirPath
  entries <- fmap catMaybes $ forM paths $ \pth ->
    if isPackablePath schemaFilename (takeFileName pth) then do
      hPutStrLn stderr $ "Processing " <> pth
      importedAndNormalized <- exprFromFile pth
      tarPath <- either (throwIO . userError) pure (Tar.toTarPath False (takeFileName pth))
      pure $ Just $Tar.fileEntry tarPath (toS (renderExpr importedAndNormalized))
    else pure Nothing
  withOutputFileOrStdout outputPackagePath (\h -> LBS.hPutStr h $ BZip.compress $ Tar.write entries)

evaluateStdin :: IO ()
evaluateStdin =
  putStrLn . Data.Yaml.encode =<< eval =<< exprFromFile "-"


isOuputProducingPath :: FilePath -> FilePath -> Bool
isOuputProducingPath schemaFile file
  | schemaFile==file           = False
isOuputProducingPath _ ('_':_) = False
isOuputProducingPath _ ('.':_) = False
isOuputProducingPath _ s       = takeExtension s == ".dhall"

isPackablePath :: FilePath -> FilePath -> Bool
isPackablePath schemaFile file =
  schemaFile==file || isOuputProducingPath schemaFile file




parseCommand :: IO Command
parseCommand =
  fmap snd $ simpleOptions "0.0.1" headerText description (pure ()) $ do
    addCommand
      "unpack"
      "Evaluate a packaged dhall template"
      Unpack
      ( UnpackOptions
        <$> outputYamlOption
        <*> inputPackageOption
        <*> jsonConfigOption
      )
    addCommand
      "pack"
      "Pack a dhall template"
      Pack
      ( PackOptions
        <$>  strOption
          (  help "Output archive file"
          <> long "output"
          <> short 'o'
          )
        <*> inputTemplateOption
      )
    addCommand
      "template"
      "Evaluate a dhall template dir"
      Template
      templateOptions
    addCommand
      "eval"
      "Evaluate stdin"
      (const Evaluate)
      (pure ())
  where
  headerText = "bake-dhall"
  description = "describe me"
  templateOptions =
    TemplateOptions
      <$> outputYamlOption
      <*> inputTemplateOption
      <*> jsonConfigOption
  inputTemplateOption =
    strOption
      (  help "Input template directory"
      <> long "template"
      <> short 't'
      <> value "."
      <> showDefault
      )
  inputPackageOption =
    strOption
      (  help "Input package path"
      <> long "input"
      <> short 'i'
      <> value "-"
      <> showDefault
      )
  outputYamlOption =
    strOption
      (  help "Output yaml file"
      <> long "output"
      <> short 'o'
      <> value "-"
      <> showDefault
      )
  jsonConfigOption =
    strOption
      (  help "Input json config"
      <> long "config"
      <> short 'c'
      )

withInputFileOrStdin :: FilePath -> (Handle -> IO a) -> IO a
withInputFileOrStdin "-" f = f stdin
withInputFileOrStdin fp  f = withFile fp ReadMode f

withOutputFileOrStdout :: FilePath -> (Handle -> IO a) -> IO a
withOutputFileOrStdout "-" f = f stdout
withOutputFileOrStdout fp  f = withFile fp WriteMode f
