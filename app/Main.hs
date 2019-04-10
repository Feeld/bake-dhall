{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main (main) where

import           BakeDhall                  (eval, evalWithValue, exprFromFile)

import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Yaml
import qualified Dhall
import           Options.Applicative.Simple
import           Protolude
import           System.Directory           (listDirectory)
import           System.FilePath
import           System.IO.Error            (userError)
import           System.IO.Temp

main :: IO ()
main = Dhall.detailed $ evaluateCommand =<< parseCommand

data Command
  = Export   ExportOptions
  | Import   ImportOptions
  | Template TemplateOptions
  | Evaluate

data ImportOptions = ImportOptions
  { outputTemplateDirPath :: FilePath
  , packagePath           :: FilePath
  }

data ExportOptions = ExportOptions
  { outputPackagePath    :: FilePath
  , inputTemplateDirPath :: FilePath
  }

data TemplateOptions = TemplateOptions
  { outputYamlPath  :: FilePath
  , templateDirPath :: FilePath
  , jsonConfigPath  :: FilePath
  , schemaFilename  :: FilePath
  }


evaluateCommand :: Command -> IO ()
evaluateCommand (Template opts) = evaluateTemplate opts
evaluateCommand Evaluate        = evaluateStdin

evaluateTemplate :: TemplateOptions -> IO ()
evaluateTemplate TemplateOptions{outputYamlPath,templateDirPath,jsonConfigPath,schemaFilename} = do
  schemaExpr <- exprFromFile $ templateDirPath </> schemaFilename
  cfgValue <- withInputFileOrStdin jsonConfigPath $ \inH ->
    either (throwIO . userError) pure =<< (eitherDecode @Value . toS <$> LBS.hGetContents inH)
  withOutputFileOrStdout outputYamlPath $ \outH -> do
    paths <- map (templateDirPath </>) <$> listDirectory templateDirPath
    forM_ paths $ \pth ->
      when (isOuputProducingPath schemaFilename (takeFileName pth)) $ do
        hPutStrLn stderr $ "Processing " <> pth
        yaml <- Data.Yaml.encode
          <$> (evalWithValue schemaExpr cfgValue =<< exprFromFile pth)
        hPutStrLn outH $ "---\n# Source: " <> toS pth <> "\n" <> yaml

evaluateStdin :: IO ()
evaluateStdin =
  putStrLn . Data.Yaml.encode =<< eval =<< exprFromFile "-"


isOuputProducingPath :: FilePath -> FilePath -> Bool
isOuputProducingPath schemaFile file
  | schemaFile==file           = False
isOuputProducingPath _ ('_':_) = False
isOuputProducingPath _ ('.':_) = False
isOuputProducingPath _ s       = takeExtension s == ".dhall"




parseCommand :: IO Command
parseCommand =
  fmap snd $ simpleOptions "0.0.1" header description (pure ()) $ do
    addCommand
      "import"
      "Import an exported dhall template package"
      Import
      ( ImportOptions
        <$>  strOption
          (  help "Output template directory"
          <> long "output"
          <> short 'o'
          )
        <*> strOption
          (  help "Input package file"
          <> long "input"
          <> short 'i'
          )
      )
    addCommand
      "export"
      "Export a dhall template"
      Export
      ( ExportOptions
        <$>  strOption
          (  help "Output package file"
          <> long "output"
          <> short 'o'
          )
        <*>  strOption
          (  help "Input template directory"
          <> long "template"
          <> short 't'
          <> value "."
          <> showDefault
          )
      )
    addCommand
      "template"
      "Evaluate a dhall template dir"
      Template
      ( TemplateOptions
        <$>  strOption
          (  help "Output yaml file"
          <> long "output"
          <> short 'o'
          <> value "-"
          <> showDefault
          )
        <*>  strOption
          (  help "Input template directory"
          <> long "template"
          <> short 't'
          <> value "."
          <> showDefault
          )
        <*>  strOption
          (  help "Input json config"
          <> long "input"
          <> short 'i'
          <> value "-"
          <> showDefault
          )
        <*>  strOption
          (  help "Configuration type filename (relative to template dir)"
          <> long "schema"
          <> short 's'
          <> value "Config.dhall"
          <> showDefault
          )
      )
    addCommand
      "eval"
      "Evaluate stdin"
      (const Evaluate)
      (pure ())
  where
  header = "bake-dhall"
  description = "describe me"


withInputFileOrStdin :: FilePath -> (Handle -> IO a) -> IO a
withInputFileOrStdin "-" f = f stdin
withInputFileOrStdin fp  f = withFile fp ReadMode f

withOutputFileOrStdout :: FilePath -> (Handle -> IO a) -> IO a
withOutputFileOrStdout "-" f = f stdout
withOutputFileOrStdout fp  f = withFile fp WriteMode f
