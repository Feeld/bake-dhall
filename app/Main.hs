{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main (main) where

import           BakeDhall                  (ExprX, eval, evalWithValue,
                                             exprFromFile)

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Archive.Tar.Entry    as Tar
import qualified Codec.Compression.Lzma     as Lzma
import qualified Codec.Serialise
import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.List                  as List
import qualified Data.Yaml
import qualified Dhall
import qualified Dhall.Binary
import           Options.Applicative.Simple
import           Protolude
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
  { outputPackagePath :: FilePath
  , templateDirPath   :: FilePath
  }

data TemplateOptions = TemplateOptions
  { outputYamlPath  :: FilePath
  , templateDirPath :: FilePath
  , jsonConfigPath  :: FilePath
  }

schemaFilename :: FilePath
schemaFilename = "Config.dhall"

indexFilename :: FilePath
indexFilename = "index.dhall"

evaluateCommand :: Command -> IO ()
evaluateCommand (Template opts) = evaluateTemplate opts
evaluateCommand Evaluate        = evaluateStdin
evaluateCommand (Pack opts)     = evaluatePack opts
evaluateCommand (Unpack opts)   = evaluateUnpack opts

evaluateTemplate :: TemplateOptions -> IO ()
evaluateTemplate TemplateOptions{outputYamlPath,templateDirPath,jsonConfigPath} = do
  let indexPath = templateDirPath </> indexFilename
  schemaExpr <- exprFromFile $ templateDirPath </> schemaFilename
  cfgValue <- withInputFileOrStdin jsonConfigPath $ \inH ->
    either (throwIO . userError) pure =<< (eitherDecode @Value . toS <$> LBS.hGetContents inH)
  withOutputFileOrStdout outputYamlPath $ \outH -> do
    aesonValue <- evalWithValue schemaExpr cfgValue =<< exprFromFile indexPath
    forM_ (extractParts aesonValue) $ \part ->
      hPutStrLn outH $ "---\n" <> Data.Yaml.encode part

extractParts :: Data.Aeson.Value -> [Data.Aeson.Value]
extractParts (Data.Aeson.Array vs) = toList vs
extractParts x                     = [x]

evaluateUnpack :: UnpackOptions -> IO ()
evaluateUnpack UnpackOptions{outputYamlPath,inputPackagePath,jsonConfigPath} =
  withInputFileOrStdin inputPackagePath $ \inH -> do
    eEntries <- Tar.foldEntries (\a -> fmap (a:)) (pure mempty) Left
              . Tar.read . Lzma.decompress
            <$> LBS.hGetContents inH
    entries <- either (throwIO . userError . show) pure eEntries
    schemaEntry <- maybe (throwIO (userError $ schemaFilename <> " not found in archive")) pure
                  (List.find ((==schemaFilename) . Tar.entryPath) entries)
    indexEntry <- maybe (throwIO (userError $ indexFilename <> " not found in archive")) pure
                  (List.find ((==indexFilename) . Tar.entryPath) entries)
    expr <- exprFromEntry indexEntry
    schemaExpr <- exprFromEntry schemaEntry
    cfgValue <- withInputFileOrStdin jsonConfigPath $ \cfgH ->
      either (throwIO . userError) pure =<< (eitherDecode @Value . toS <$> LBS.hGetContents cfgH)
    withOutputFileOrStdout outputYamlPath $ \outH -> do
      aesonValue <- evalWithValue schemaExpr cfgValue expr
      forM_ (extractParts aesonValue) $ \part ->
        hPutStrLn outH $ "---\n" <> Data.Yaml.encode part

exprFromEntry :: MonadIO m => Tar.Entry -> m ExprX
exprFromEntry entry = do
  term <- throws . Codec.Serialise.deserialiseOrFail =<< entryBytes entry
  exprI <- throws $ Dhall.Binary.decodeExpression term
  either (throwIO . userError) pure $ traverse (const (Left "Import resolution disabled")) exprI

entryBytes :: MonadIO f => Tar.Entry -> f LBS.ByteString
entryBytes Tar.Entry{entryContent=Tar.NormalFile x _} = pure x
entryBytes e = throwIO $ userError $ "Expected " <> Tar.entryPath e <> " to be a normal file"

evaluatePack :: PackOptions -> IO ()
evaluatePack PackOptions{outputPackagePath,templateDirPath} = do
  entries <- forM [indexPath, schemaPath] $ \pth -> do
    importedAndNormalized <- exprFromFile pth
    tarPath <- either (throwIO . userError) pure (Tar.toTarPath False (takeFileName pth))
    pure $ Tar.fileEntry tarPath
         $ Codec.Serialise.serialise
         $ Dhall.Binary.encode importedAndNormalized
  withOutputFileOrStdout outputPackagePath (\h -> LBS.hPutStr h $ Lzma.compressWith compressOptions $ Tar.write entries)
  where
  indexPath  = templateDirPath </> indexFilename
  schemaPath = templateDirPath </> schemaFilename
  compressOptions = Lzma.defaultCompressParams
    { Lzma.compressLevelExtreme = True
    , Lzma.compressLevel = Lzma.CompressionLevel9
    }


evaluateStdin :: IO ()
evaluateStdin =
  putStrLn . Data.Yaml.encode =<< eval =<< exprFromFile "-"




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

throws :: (MonadIO m, Exception e) => Either e a -> m a
throws (Left  e) = throwIO e
throws (Right a) = pure a
