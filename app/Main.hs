{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Main (main) where

import           BakeDhall                  (ExprX, applyExpr, evalExpr,
                                             exprFromBytes, exprFromFile,
                                             exprToBytes, toYamlDocuments)

import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.FileEmbed             (embedStringFile)
import qualified Dhall
import           Options.Applicative.Simple
import           Protolude
import           System.IO.Error            (userError)

main :: IO ()
main = Dhall.detailed $ evaluateCommand =<< parseCommand

data Command
  = Pack   PackOptions
  | Unpack   UnpackOptions
  | Evaluate EvaluateOptions

data UnpackOptions = UnpackOptions
  { outputYamlPath   :: FilePath
  , inputPackagePath :: FilePath
  , jsonConfigPath   :: Maybe FilePath
  }

data PackOptions = PackOptions
  { outputPackagePath :: FilePath
  , expressionPath    :: FilePath
  }

data EvaluateOptions = EvaluateOptions
  { outputYamlPath :: FilePath
  , expressionPath :: FilePath
  , jsonConfigPath :: Maybe FilePath
  }


parseCommand :: IO Command
parseCommand =
  fmap snd $ simpleOptions "0.0.1" headerText description (pure ()) $ do
    addCommand
      "unpack"
      "Apply a json argument to a binary serialized dhall function"
      Unpack
      unpackOptions
    addCommand
      "pack"
      "Resolve all imports, normalize and serialize a dhall function into a binary archive"
      Pack
      packOtions
    addCommand
      "evaluate"
      "Apply a json argument to a dhall function"
      Evaluate
      evaluateOptions
  where
  headerText = "bake-dhall"
  description = $(embedStringFile "app/description.txt")

  evaluateOptions =
    EvaluateOptions
      <$> outputYamlOption
      <*> inputExpressionOption
      <*> jsonConfigOption

  packOtions =
    PackOptions
      <$> outputArchiveOption
      <*> inputExpressionOption

  unpackOptions =
    UnpackOptions
      <$> outputYamlOption
      <*> inputPackageOption
      <*> jsonConfigOption

  inputExpressionOption =
    strOption
      (  help "Input expression path ('-' for stdin)"
      <> long "input"
      <> short 'i'
      <> value "-"
      <> showDefault
      )

  inputPackageOption =
    strOption
      (  help "Input package path ('-' for stdin)"
      <> long "input"
      <> short 'i'
      <> value "-"
      <> showDefault
      )

  outputYamlOption =
    strOption
      (  help "Output yaml file ('-' for stdin)"
      <> long "output"
      <> short 'o'
      <> value "-"
      <> showDefault
      )

  outputArchiveOption =
    strOption
      (  help "Output archive file"
      <> long "output"
      <> short 'o'
      )

  jsonConfigOption = optional $
    strOption
      (  help "Input json argument"
      <> long "config"
      <> short 'c'
      )


evaluateCommand :: Command -> IO ()
evaluateCommand (Evaluate opts) = evaluateEvaluate opts
evaluateCommand (Pack opts)     = evaluatePack opts
evaluateCommand (Unpack opts)   = evaluateUnpack opts

evaluateEvaluate :: EvaluateOptions -> IO ()
evaluateEvaluate EvaluateOptions{outputYamlPath,expressionPath,jsonConfigPath} =
  applyOrEval jsonConfigPath outputYamlPath =<< exprFromFile expressionPath

evaluatePack :: PackOptions -> IO ()
evaluatePack PackOptions{outputPackagePath,expressionPath} = do
  bytes <- exprToBytes <$> exprFromFile expressionPath
  withOutputFileOrStdout outputPackagePath (`BS.hPutStr` bytes)

evaluateUnpack :: UnpackOptions -> IO ()
evaluateUnpack UnpackOptions{outputYamlPath,inputPackagePath,jsonConfigPath} =
  withInputFileOrStdin inputPackagePath $ \inH -> do
    eExpr  <- exprFromBytes <$> BS.hGetContents inH
    case eExpr of
      Right ex -> applyOrEval jsonConfigPath outputYamlPath ex
      Left err -> throwIO err


applyOrEval :: Maybe FilePath -> FilePath -> ExprX -> IO ()
applyOrEval (Just jsonConfigPath) outputYamlPath expr =
  withInputFileOrStdin jsonConfigPath $ \cfgH ->
  withOutputFileOrStdout outputYamlPath $ \outH -> do
    cfgValue <- either (throwIO . userError) pure =<< (eitherDecode @Value <$> LBS.hGetContents cfgH)
    jValue <- either throwIO pure (applyExpr expr cfgValue)
    hPutStrLn outH $ toYamlDocuments jValue
applyOrEval Nothing outputYamlPath expr =
  withOutputFileOrStdout outputYamlPath $ \outH -> do
    jValue <- either throwIO pure (evalExpr expr)
    hPutStrLn outH $ toYamlDocuments jValue

withInputFileOrStdin :: FilePath -> (Handle -> IO a) -> IO a
withInputFileOrStdin "-" f = f stdin
withInputFileOrStdin fp  f = withFile fp ReadMode f

withOutputFileOrStdout :: FilePath -> (Handle -> IO a) -> IO a
withOutputFileOrStdout "-" f = f stdout
withOutputFileOrStdout fp  f = withFile fp WriteMode f
