{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Main (main, mainFor) where

import           System.IO.Error                  (userError)
import           Control.Lens
import qualified Control.Monad.Trans.State.Strict as StrictState
import           Data.Aeson
import qualified Data.ByteString.Base64           as B64
import           Data.Proxy
import qualified Data.Text.IO
import qualified Data.Yaml
import           Dhall                            (Inject, Interpret, Natural)
import qualified Dhall
import qualified Dhall.Context
import           Dhall.Core                       (Chunks (..), Expr (..),
                                                   normalizeWith)
import qualified Dhall.Import
import           Dhall.JSON                       (Conversion (..),
                                                   convertToHomogeneousMaps,
                                                   dhallToJSON)
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import           Protolude
import           System.FilePath                  (takeDirectory)

main :: IO ()
main = mainFor (Proxy @HttpServiceConfig)

data HttpServiceConfig = HttpServiceConfig
  { name        :: Text
  , namespace   :: Text
  , image       :: Text
  , port        :: Natural
  , replicas    :: Natural
  , hosts       :: [ Text ]
  , defaultHost :: Text
  , environment :: [ EnvVar ]
  } deriving (Show, Generic, Inject, Interpret, ToJSON, FromJSON)


data EnvVar = EnvVar
  { name  :: Text
  , value :: Text
  } deriving (Show, Generic, Inject, Interpret, ToJSON, FromJSON)



mainFor :: forall cfg. (FromJSON cfg, Inject cfg) => Proxy cfg -> IO ()
mainFor _ = Dhall.detailed $ do
  args <- getArgs
  case args of
    (cfgFile:files) -> do
      cfg <- either (throwIO . userError) pure
        =<< (eitherDecode @cfg . toS <$> readFile cfgFile)
      values <- traverse (evalWithConfig cfg) files
      forM_ (zip files values) $ \(f,v) -> do
        putStrLn @Text "---"
        putStrLn @Text $ "# from: " <> show f
        putStrLn (Data.Yaml.encode v)
    _ -> die "Usage: bake-dhall CONFIG_JSON FILE_1 ... FILE_N"

evalWithConfig
  :: forall config
   . Inject config
  => config
  -> FilePath
  -> IO Value
evalWithConfig cfg path = do
    let
      cfgDhall = Dhall.embed Dhall.inject cfg
      filename
        | path == "-" = "(stdin)"
        | otherwise   = path
      rootDirectory
        | path == "-" = "."
        | otherwise   = takeDirectory path
      withFile'
        | path == "-" = \f -> f stdin
        | otherwise   = withFile path ReadMode

    withFile' $ \hdl -> do
      text <- Data.Text.IO.hGetContents hdl

      parsedExpression <- case Dhall.Parser.exprFromText filename text of
        Left  err              -> throwIO err
        Right parsedExpression -> return parsedExpression

      let status = Dhall.Import.emptyStatus rootDirectory

      resolvedExpression <- StrictState.evalStateT (Dhall.Import.loadWith parsedExpression) status

      let appliedExpression = normalizeB64 (resolvedExpression `App` cfgDhall)

      case Dhall.TypeCheck.typeWith @Dhall.Parser.Src (startingContext (Proxy @config)) appliedExpression  of
        Left  err -> throwIO err
        Right _   -> return ()

      let convertedExpression = convertToHomogeneousMaps conversion appliedExpression
          conversion = Conversion "mapKey" "mapValue"

      case dhallToJSON convertedExpression of
        Left err -> throwIO err
        Right v  -> pure v

normalizeB64 :: Eq a => Expr s a -> Expr t a
normalizeB64 = normalizeWith (pure . b64normalizer)
  where
  b64normalizer :: Eq a => Expr s a -> Maybe (Expr s a)
  b64normalizer (App (Var "Text/toBase64") (normalizeB64 -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.encode . toS $ x)))
  b64normalizer (App (Var "Text/fromBase64") (normalizeB64 -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.decodeLenient . toS $ x)))
  b64normalizer _ =
    Nothing

startingContext
  :: forall a
   . Inject a
  => Proxy a -> Dhall.Context.Context (Expr Dhall.Parser.Src Dhall.TypeCheck.X)
startingContext _ = Dhall.Context.empty
                  & Dhall.Context.insert "Text/toBase64" textToTextType
                  & Dhall.Context.insert "Text/fromBase64" textToTextType
                  & Dhall.Context.insert "Config" configDhallType
  where
  textToTextType = Pi "_" Text Text
  configDhallType = Dhall.declared (Dhall.inject @a)
