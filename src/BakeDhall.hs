{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module BakeDhall (ExprX, exprFromFile, exprFromText, exprFromText', evalWithValue) where

import           JsonToDhall (defaultConversion, dhallFromJSON)

import           Control.Lens
import qualified Control.Monad.Trans.State.Strict as StrictState
import           Data.Aeson
import qualified Data.ByteString.Base64           as B64
import qualified Data.Text.IO
import qualified Dhall.Binary
import qualified Dhall.Context
import           Dhall.Core                       (Chunks (..), Expr (..),
                                                   normalizeWith)
import qualified Dhall.Import
import           Dhall.JSON                       (Conversion (..),
                                                   convertToHomogeneousMaps,
                                                   dhallToJSON, omitEmpty)
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import           Protolude
import           System.FilePath                  (takeDirectory)
import           System.IO.Error                  (userError)



type ExprX = Expr Dhall.Parser.Src Dhall.TypeCheck.X

evalWithValue
  :: ExprX
  -> Value
  -> ExprX
  -> IO Value
evalWithValue cfgTyExpr cfgValue funExpr = do
  cfgExpr <- case dhallFromJSON defaultConversion cfgTyExpr cfgValue of
    Right x -> pure x
    Left e  -> throwIO $ userError $ show e

  let appliedExpr = normalizeB64 (funExpr `App` cfgExpr)

  case Dhall.TypeCheck.typeWith @Dhall.Parser.Src (startingContext cfgTyExpr) appliedExpr  of
    Left  err -> throwIO err
    Right _   -> return ()

  let convertedExpression = convertToHomogeneousMaps conversion appliedExpr
      conversion = Dhall.JSON.Conversion "key" "value"

  case dhallToJSON convertedExpression of
    Left err -> throwIO err
    Right v  -> pure (omitEmpty v)

exprFromFile
  :: FilePath
  -> IO ExprX
exprFromFile path = withFile' $
  exprFromText' rootDirectory filename <=< Data.Text.IO.hGetContents
  where
  filename
    | path == "-" = "(stdin)"
    | otherwise   = path
  rootDirectory
    | path == "-" = "."
    | otherwise   = takeDirectory path
  withFile'
    | path == "-" = \f -> f stdin
    | otherwise   = withFile path ReadMode

exprFromText :: Text -> IO ExprX
exprFromText = exprFromText' "." "str"

exprFromText'
  :: FilePath
  -> FilePath
  -> Text
  -> IO ExprX
exprFromText' rootDirectory filename text = do
  parsedExpression <- case Dhall.Parser.exprFromText filename text of
    Left  err              -> throwIO err
    Right parsedExpression -> return parsedExpression

  let status = Dhall.Import.emptyStatus rootDirectory

  normalizeB64 <$> StrictState.evalStateT (Dhall.Import.loadWith parsedExpression) status

normalizeB64 :: Dhall.Binary.ToTerm a => Expr s a -> Expr t a
normalizeB64 = normalizeWith (pure . b64normalizer)
  where
  b64normalizer :: Dhall.Binary.ToTerm a => Expr s a -> Maybe (Expr s a)
  b64normalizer (App (Var "Text/toBase64") (normalizeB64 -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.encode . toS $ x)))
  b64normalizer (App (Var "Text/fromBase64") (normalizeB64 -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.decodeLenient . toS $ x)))
  b64normalizer _ =
    Nothing

startingContext
  :: ExprX -> Dhall.Context.Context ExprX
startingContext x = Dhall.Context.empty
                  & Dhall.Context.insert "Text/toBase64" textToTextType
                  & Dhall.Context.insert "Text/fromBase64" textToTextType
                  & Dhall.Context.insert "Config" x
  where
  textToTextType = Pi "_" Text Text
