{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main (main) where

import           BakeDhall                  (BakeError, ExprX, applyExpr,
                                             evalExpr, exprFromB64,
                                             exprFromBytes, exprFromFile,
                                             exprToBytes, toYamlDocuments)

import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as LBS
import           Data.FileEmbed             (embedStringFile)
import qualified Dhall
import qualified Network.Wai.Handler.Warp   as Warp
import           Options.Applicative.Simple hiding (argument)
import qualified Prelude
import           Protolude
import           Servant
import           System.Envy                hiding ((.=))
import           System.IO.Error            (userError)

main :: IO ()
main = Dhall.detailed $ evaluateCommand =<< parseCommand

data Command
  = Pack   PackOptions
  | Unpack   UnpackOptions
  | Evaluate EvaluateOptions
  | Serve

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
    addCommand
      "serve"
      "Launch an HTTP serve which evaluates packed expressions"
      (const Serve)
      (pure ())
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
evaluateCommand Serve           = startServer

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




--
-- Web service stuff
--

type EvaluatePackageApi =
     ReqBody '[JSON]                Request
  :> Post    '[JSON, YamlDocuments] Manifest

data YamlDocuments

data Request = Request
  { expression :: Base64Expr
  , argument   :: Maybe Value
  , token      :: AccessToken
  } deriving (Generic, FromJSON)

newtype Manifest = Manifest LBS.ByteString

instance ToJSON Manifest where
  toJSON (Manifest m) = object [ "manifest" .= toS @_ @Text (B64.encode (toS m)) ]

instance MimeRender YamlDocuments Manifest where
  mimeRender _ (Manifest lbs) = lbs

instance Accept YamlDocuments where
  contentType _ = "application/x-yaml"


newtype AccessToken = AccessToken Text
  deriving newtype (Eq, FromJSON)

instance FromEnv AccessToken where
  fromEnv = AccessToken . toS <$> env @Prelude.String "SECRET_TOKEN"


newtype Base64Expr = Base64Expr ExprX

instance FromJSON Base64Expr where
  parseJSON = withText "Base64Expr" (either (Prelude.fail . show) (pure . Base64Expr) . exprFromB64)

newtype Port = Port Warp.Port

instance FromEnv Port where
  fromEnv = do
    port <- env "PORT"
    case readMaybe port of
      Just p  -> pure (Port (fromInteger p))
      Nothing -> Prelude.fail "PORT must be a number"

evaluatePackageServer
  :: MonadError ServantErr m
  => AccessToken
  -> ServerT EvaluatePackageApi m
evaluatePackageServer secret Request{expression=Base64Expr expr,argument,token}
  | secret /= token = throwError $ err403 { errBody = "Invalid access token" }
  | otherwise = either (\e -> throwError $ err422 { errBody = show e }) pure (createManifest expr argument)

createManifest :: ExprX -> Maybe Value -> Either BakeError Manifest
createManifest expr (Just arg) = Manifest . toYamlDocuments <$> applyExpr expr arg
createManifest expr Nothing    = Manifest . toYamlDocuments <$> evalExpr expr

startServer :: IO ()
startServer = do
  Port port <- either (throwIO . userError) pure =<< decodeEnv
  secret <- either (throwIO . userError) pure =<< decodeEnv
  putStrLn @Text $ "Listening on port " <> show port
  Warp.run port (mkApplication secret)

mkApplication :: AccessToken -> Application
mkApplication = serve (Proxy @EvaluatePackageApi) . evaluatePackageServer
