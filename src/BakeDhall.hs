{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module BakeDhall (ExprX, exprFromFile, exprFromText, exprFromText', eval, evalWithValue) where

import           JsonToDhall                      (defaultConversion,
                                                   dhallFromJSON)

import           Control.Lens                     hiding (Const)
import qualified Control.Monad.Trans.State.Strict as StrictState
import           Data.Aeson
import qualified Data.ByteString.Base64           as B64
import qualified Data.Text.IO
import           Data.Yaml
import qualified Dhall.Binary
import qualified Dhall.Context
import           Dhall.Core                       (Chunks (..), Const (..),
                                                   Expr (..), denote,
                                                   internalError, normalizeWith)
import qualified Dhall.Import
import           Dhall.JSON                       (Conversion (..),
                                                   convertToHomogeneousMaps,
                                                   dhallToJSON, omitEmpty)
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import           Protolude                        hiding (Const)
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

  let appliedExpr = normalizeBake (funExpr `App` cfgExpr)

  case Dhall.TypeCheck.typeWith @Dhall.Parser.Src startingContext appliedExpr  of
    Left  err -> throwIO err
    Right _   -> return ()

  let convertedExpression = convertToHomogeneousMaps conversion appliedExpr
      conversion = Dhall.JSON.Conversion "mapKey" "mapValue"

  case dhallToJSON convertedExpression of
    Left err -> throwIO err
    Right v  -> pure (omitEmpty v)

eval :: ExprX -> IO Value
eval expr = do
  case Dhall.TypeCheck.typeWith @Dhall.Parser.Src startingContext expr of
    Left  err -> throwIO err
    Right _   -> return ()

  let convertedExpression = convertToHomogeneousMaps conversion expr
      conversion = Dhall.JSON.Conversion "mapKey" "mapValue"

  case dhallToJSON convertedExpression of
    Left err -> throwIO err
    Right v  -> pure (omitEmpty v)

exprFromFile
  :: FilePath
  -> IO ExprX
exprFromFile "-" =
  exprFromText' "." "(stdin)" =<< Data.Text.IO.hGetContents stdin
exprFromFile path = withFile path ReadMode $
  exprFromText' (takeDirectory path) path <=< Data.Text.IO.hGetContents

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

  normalizeBake <$> StrictState.evalStateT (Dhall.Import.loadWith parsedExpression) status

normalizeBake :: Dhall.Binary.ToTerm a => Expr s a -> Expr t a
normalizeBake = normalizeWith (pure . normalizer)
  where
  normalizer :: Dhall.Binary.ToTerm a => Expr s a -> Maybe (Expr s a)

  normalizer (App (Var "Text/toBase64") (normalizeBake -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.encode . toS $ x)))

  normalizer (App (Var "Text/fromBase64") (normalizeBake -> TextLit (Chunks [] x))) =
    case B64.decode (toS x) of
      Right s -> Just (Some (TextLit (Chunks [] (toS s))))
      Left _ -> Just (None `App` Text)

  normalizer (App (App (Var "fromJSON") tyExpr) (normalizeBake -> TextLit (Chunks [] str))) = do
    expr <- exprFromValue tyExpr =<< Data.Aeson.decode (toS str)
    pure $ Annot expr tyExpr

  normalizer (App (App (Var "toJSON") _) expr) =
    serializeValueWith (toS . Data.Aeson.encode) expr

  normalizer (App (App (Var "fromYAML") tyExpr) (normalizeBake -> TextLit (Chunks [] str))) = do
    expr <- exprFromValue tyExpr =<< either (const Nothing) Just (Data.Yaml.decodeEither' (toS str))
    pure $ Annot expr tyExpr

  normalizer (App (App (Var "toYAML") _) expr) =
    serializeValueWith (toS . Data.Yaml.encode) expr

  normalizer _ =
    Nothing

  serializeValueWith fun expr =
    let convertedExpression = convertToHomogeneousMaps conversion (const anX <$> expr)
        conversion = Dhall.JSON.Conversion "mapKey" "mapValue"
    in case dhallToJSON convertedExpression of
      Left _  -> Nothing
      Right v -> Just (TextLit (Chunks [] (fun (omitEmpty v))))

  exprFromValue tyExpr value =
    case dhallFromJSON defaultConversion (const anX <$> denote tyExpr) value of
      Right x -> Just (Dhall.TypeCheck.absurd <$> denote (Some x))
      Left _  -> Just (None `App` tyExpr)

  anX = Dhall.TypeCheck.X (internalError "absurd")

startingContext
  :: Dhall.Context.Context ExprX
startingContext = Dhall.Context.empty
                & Dhall.Context.insert "Text/toBase64" textToTextType
                & Dhall.Context.insert "Text/fromBase64" fromBase64Type
                & Dhall.Context.insert "fromJSON" deserializerType
                & Dhall.Context.insert "fromYAML" deserializerType
                & Dhall.Context.insert "toJSON" serializerType
                & Dhall.Context.insert "toYAML" serializerType
  where
  textToTextType = Pi "_" Text Text
  fromBase64Type = Pi "_" Text (Optional `App` Text)
  deserializerType = Pi "x" (Const Type) (Pi "_" Text (Optional `App` Var "x"))
  serializerType = Pi "x" (Const Type) (Pi "_" (Var "x") Text)
