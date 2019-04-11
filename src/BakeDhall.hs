{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module BakeDhall (
  ExprX
, exprFromFile
, exprFromText
, exprFromText'
, exprFromTextPure
, eval
, evalWithValue
, evalTest
, renderExpr
) where


import           JsonToDhall                           (defaultConversion,
                                                        dhallFromJSON)

import           Control.Lens                          hiding (Const)
import qualified Control.Monad.Trans.State.Strict      as StrictState
import           Data.Aeson
import qualified Data.ByteString.Base64                as B64
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import           Data.Yaml
import qualified Dhall.Binary
import qualified Dhall.Context
import           Dhall.Core                            (Chunks (..), Const (..),
                                                        Expr (..),
                                                        ReifiedNormalizer (..),
                                                        denote,
                                                        internalError,
                                                        normalizeWith)
import qualified Dhall.Import
import           Dhall.JSON                            (Conversion (..),
                                                        convertToHomogeneousMaps,
                                                        dhallToJSON, omitEmpty)
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import           Protolude                             hiding (Const)
import           System.FilePath                       (takeDirectory)
import           System.IO.Error                       (userError)

-- This module exports functions to evaluate dhall expressions with builtin
-- json/yaml support
--
-- $
-- >>> :set -XOverloadedStrings
--
-- Can evaluate a simple expression
-- >>> evalTest "1 + 2"
-- 3
--
-- Can convert a string to base64
-- >>> evalTest "Text/toBase64 \"foobar\""
-- "Zm9vYmFy"
--
-- Can decode a well-formed base64 string
-- >>> evalTest "Text/fromBase64 \"Zm9vYmFy\""
-- "foobar"
--
-- A non-well-formed base64 string produces a json null
-- >>> evalTest "Text/fromBase64 \"Zm9mFy\""
-- null
--
-- Can encode into json
-- >>> evalTest "toJSON (List Natural) [1 + 2 + 3, 5]"
-- "[6,5]"
--
-- Can decode a well-typed json
-- >>> evalTest "fromJSON {name:Text} ./examples/sampleConfig.json as Text"
-- {"name":"Some name"}
--
-- A non-well-typed json produces a json null
-- >>> evalTest "fromJSON {name:Natural} ./examples/sampleConfig.json as Text"
-- null
--
-- Can encode into yaml
-- >>> evalTest "toYAML (List Natural) [1 + 2 + 3, 5]"
-- "- 6\n- 5\n"
--
-- Can decode a well-typed yaml
-- >>> evalTest "fromYAML (List Natural) \"- 6\\n- 5\\n\""
-- [6,5]
--
-- A non well-typed yaml produces a json null
-- >>> evalTest "fromYAML (List (List Text)) \"- 6\\n- 5\\n\""
-- null


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

  typeCheck appliedExpr

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

evalTest :: Text -> IO ()
evalTest = putStrLn <=< fmap Data.Aeson.encode . eval <=< exprFromText

exprFromFile
  :: FilePath
  -> IO ExprX
exprFromFile "-" =
  exprFromText' "." "(stdin)" =<< Data.Text.IO.hGetContents stdin
exprFromFile path = withFile path ReadMode $
  exprFromText' (takeDirectory path) path <=< Data.Text.IO.hGetContents

exprFromText :: Text -> IO ExprX
exprFromText = exprFromText' "." "(string)"

exprFromText'
  :: FilePath
  -> FilePath
  -> Text
  -> IO ExprX
exprFromText' rootDirectory filename text = do
  parsedExpression <- case Dhall.Parser.exprFromText filename text of
    Left  err              -> throwIO err
    Right parsedExpression -> return parsedExpression

  let status = Dhall.Import.emptyStatus        rootDirectory
             & Dhall.Import.normalizer      .~ ReifiedNormalizer (pure . bakeNormalizer)
             & Dhall.Import.startingContext .~ startingContext

  resolved <- normalizeBake <$> StrictState.evalStateT (Dhall.Import.loadWith parsedExpression) status
  typeCheck resolved
  pure resolved


typeCheck :: MonadIO m => ExprX -> m ()
typeCheck expr =
  case Dhall.TypeCheck.typeWith @Dhall.Parser.Src startingContext expr of
    Left  err -> throwIO err
    Right _   -> return ()

exprFromTextPure
  :: Text
  -> Either Text ExprX
exprFromTextPure text = do
  expression <- case Dhall.Parser.exprFromText "(string)" text of
    Left  err  -> Left (show err)
    Right expr -> Right expr

  normal <- normalizeBake
        <$> traverse (const (Left "Import resolution disabled")) expression
  case Dhall.TypeCheck.typeWith @Dhall.Parser.Src startingContext normal of
    Left  err -> Left (show err)
    Right _   -> Right normal


normalizeBake :: Dhall.Binary.ToTerm a => Expr s a -> Expr t a
normalizeBake = normalizeWith (pure . bakeNormalizer)

bakeNormalizer :: Dhall.Binary.ToTerm a => Expr s a -> Maybe (Expr s a)
bakeNormalizer = normalizer
  where
  normalizer :: Dhall.Binary.ToTerm a => Expr s a -> Maybe (Expr s a)

  normalizer (App (Var "Text/toBase64") (normalizeBake -> TextLit (Chunks [] x))) =
    Just (TextLit (Chunks [] (toS . B64.encode . toS $ x)))

  normalizer (App (Var "Text/fromBase64") (normalizeBake -> TextLit (Chunks [] x))) =
    case B64.decode (toS x) of
      Right s -> Just (Some (TextLit (Chunks [] (toS s))))
      Left _  -> Just (None `App` Text)

  normalizer (App (App (Var "fromJSON") tyExpr) (normalizeBake -> TextLit (Chunks [] str))) =
    exprFromValue tyExpr =<< Data.Aeson.decode (toS str)

  normalizer (App (App (Var "toJSON") _) expr) =
    serializeValueWith (toS . Data.Aeson.encode) expr

  normalizer (App (App (Var "fromYAML") tyExpr) (normalizeBake -> TextLit (Chunks [] str))) =
    exprFromValue tyExpr =<< either (const Nothing) Just (Data.Yaml.decodeEither' (toS str))

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


renderExpr :: ExprX -> Text
renderExpr = Pretty.renderStrict . Pretty.layoutPretty opts . Pretty.pretty
  where
  opts :: Pretty.LayoutOptions
  opts =
    Pretty.defaultLayoutOptions
      { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
