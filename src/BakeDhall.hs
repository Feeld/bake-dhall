{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module BakeDhall (
  ExprX
, BakeError (..)
, exprFromFile
, exprFromText
, exprFromBytes
, exprFromB64
, applyExpr
, evalExpr
, exprToBytes
, exprToB64
, toYamlDocuments
, renderExpr
) where


import           JsonToDhall                           (CompileError,
                                                        defaultConversion,
                                                        dhallFromJSON)

import qualified Codec.Compression.Lzma                as Lzma
import qualified Codec.Serialise
import           Control.Lens                          hiding (Const)
import           Control.Monad.Catch
import qualified Control.Monad.Trans.State.Strict      as StrictState
import           Data.Aeson
import qualified Data.ByteString.Base64                as B64
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import           Data.Yaml
import qualified Dhall.Binary
import qualified Dhall.Context
import           Dhall.Core                            (Chunks (..), Const (..),
                                                        Expr (..),
                                                        ReifiedNormalizer (..),
                                                        denote, internalError,
                                                        normalizeWith)
import qualified Dhall.Import
import           Dhall.JSON                            (CompileError (..),
                                                        Conversion (..),
                                                        convertToHomogeneousMaps,
                                                        dhallToJSON, omitEmpty)
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import           GHC.Generics                          (Generic)
import           Protolude                             hiding (Const, TypeError,
                                                        catches)
import           System.FilePath                       (takeDirectory)

-- This module exports functions to evaluate dhall expressions with builtin
-- json/yaml support
--
-- $
-- >>> :set -XOverloadedStrings
--
-- Can evaluate a simple expression
-- >>> _evalTest "1 + 2"
-- 3
--
-- Can convert a string to base64
-- >>> _evalTest "Text/toBase64 \"foobar\""
-- "Zm9vYmFy"
--
-- Can decode a well-formed base64 string
-- >>> _evalTest "Text/fromBase64 \"Zm9vYmFy\""
-- "foobar"
--
-- A non-well-formed base64 string produces a json null
-- >>> _evalTest "Text/fromBase64 \"Zm9mFy\""
-- null
--
-- Can encode into json
-- >>> _evalTest "toJSON (List Natural) [1 + 2 + 3, 5]"
-- "[6,5]"
--
-- Can decode a well-typed json
-- >>> _evalTest "fromJSON {name:Text} ./examples/sampleConfig.json as Text"
-- {"name":"Some name"}
--
-- A non-well-typed json produces a json null
-- >>> _evalTest "fromJSON {name:Natural} ./examples/sampleConfig.json as Text"
-- null
--
-- Can encode into yaml
-- >>> _evalTest "toYAML (List Natural) [1 + 2 + 3, 5]"
-- "- 6\n- 5\n"
--
-- Can decode a well-typed yaml
-- >>> _evalTest "fromYAML (List Natural) \"- 6\\n- 5\\n\""
-- [6,5]
--
-- A non well-typed yaml produces a json null
-- >>> _evalTest "fromYAML (List (List Text)) \"- 6\\n- 5\\n\""
-- null


type ExprX = Expr Dhall.Parser.Src Dhall.TypeCheck.X

data BakeError
  = ImportResolutionDisabled
  | DeserialiseFailure Codec.Serialise.DeserialiseFailure
  | DecodingFailure Dhall.Binary.DecodingFailure
  | B64DecodeError Text

  | JsonToDhallError JsonToDhall.CompileError
  | DhallToJsonErorr Dhall.JSON.CompileError

  | TypeError (Dhall.TypeCheck.TypeError Dhall.Parser.Src Dhall.TypeCheck.X)
  | ParseError Dhall.Parser.ParseError
  | MissingImportsError Dhall.Import.MissingImports
  | MissingFileError Dhall.Import.MissingFile
  | MissingEnvironmentVariableError Dhall.Import.MissingEnvironmentVariable
  | CycleError Dhall.Import.Cycle
  | ReferentiallyOpaqueError Dhall.Import.ReferentiallyOpaque

  | NotALambda
  | NeedsArgument
  deriving (Show, Generic, Exception)

exprFromFile
  :: FilePath
  -> IO ExprX
exprFromFile "-" =
  either throwIO pure =<< exprFromText "." "(stdin)" =<< Data.Text.IO.hGetContents stdin
exprFromFile path = withFile path ReadMode $
  either throwIO pure <=< exprFromText (takeDirectory path) path <=< Data.Text.IO.hGetContents


exprFromText
  :: MonadIO m
  => FilePath
  -> FilePath
  -> Text
  -> m (Either BakeError ExprX)
exprFromText rootDirectory filename text = runExceptT $ do
  parsedExpression <-
    ExceptT $ pure
            $ first ParseError
            $ Dhall.Parser.exprFromText filename text

  let status = Dhall.Import.emptyStatus        rootDirectory
             & Dhall.Import.normalizer      .~ ReifiedNormalizer (pure . bakeNormalizer)
             & Dhall.Import.startingContext .~ startingContext

  resolved <-
    ExceptT $ liftIO
            $ catchDhallError
            $ StrictState.evalStateT (Dhall.Import.loadWith parsedExpression) status
  void $ ExceptT $ pure $ typeCheck resolved
  pure (normalizeBake resolved)


catchDhallError :: MonadCatch m => m a -> m (Either BakeError a)
catchDhallError io = (Right <$> io) `catches`
  [ Handler (pure . Left . ParseError)
  , Handler (pure . Left . TypeError)
  , Handler (pure . Left . MissingImportsError)
  , Handler (pure . Left . MissingFileError)
  , Handler (pure . Left . MissingEnvironmentVariableError)
  , Handler (pure . Left . CycleError)
  , Handler (pure . Left . ReferentiallyOpaqueError)
  ]

exprFromBytes :: ByteString -> Either BakeError ExprX
exprFromBytes bytes = do
  term <- first DeserialiseFailure
       $! Codec.Serialise.deserialiseOrFail
       $! Lzma.decompress
       $! toS bytes
  exprI <- first DecodingFailure $ Dhall.Binary.decodeExpression term
  traverse (const (Left ImportResolutionDisabled)) exprI

exprFromB64 :: Text -> Either BakeError ExprX
exprFromB64 = exprFromBytes
                    <=< first (B64DecodeError . toS)
                      . B64.decode . toS

exprToBytes :: ExprX -> ByteString
exprToBytes = toS
            . Lzma.compressWith compressOptions
            . Codec.Serialise.serialise
            . Dhall.Binary.encode
  where
  compressOptions = Lzma.defaultCompressParams
    { Lzma.compressLevelExtreme = True
    , Lzma.compressLevel = Lzma.CompressionLevel9
    }

applyExpr :: ExprX -> Value -> Either BakeError Value
applyExpr lamExpr@(Lam _ tyExpr _) cfgValue = do
  cfgExpr <- first JsonToDhallError $ dhallFromJSON defaultConversion tyExpr cfgValue
  evalExpr (lamExpr `App` cfgExpr)
applyExpr _ _ = Left NotALambda

evalExpr :: ExprX -> Either BakeError Value
evalExpr (normalizeBake -> Lam{}) = Left NeedsArgument
evalExpr (normalizeBake -> expr) = do
  void $ typeCheck expr
  omitEmpty
    <$> first DhallToJsonErorr (dhallToJSON $ convertToHomogeneousMaps dhallJsonConversion expr)

dhallJsonConversion :: Conversion
dhallJsonConversion = Dhall.JSON.Conversion "mapKey" "mapValue"

-- | Encodes an 'ExprX' as base64 text
--
-- >>> Right expr <- exprFromText "." "(string)" "{ foo=2, bar=[\"a\",\"b\"]}"
-- >>> either (const (pure ())) putStrLn (fmap renderExpr $ exprFromB64 $ exprToB64 expr)
-- { bar = [ "a", "b" ], foo = 2 }
exprToB64 :: ExprX -> Text
exprToB64 = toS . B64.encode . toS . exprToBytes


typeCheck :: ExprX -> Either BakeError (Expr Dhall.Parser.Src Dhall.TypeCheck.X)
typeCheck = first TypeError . Dhall.TypeCheck.typeWith @Dhall.Parser.Src startingContext


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
    let convertedExpression = convertToHomogeneousMaps dhallJsonConversion (const anX <$> expr)
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

toYamlDocuments :: Value -> LBS.ByteString
toYamlDocuments (Array vs) = LBS.fromChunks $ map (\p -> "---\n"<>Data.Yaml.encode p) $ toList vs
toYamlDocuments v = toS $ Data.Yaml.encode v

-- | This function is for doctests.
_evalTest :: Text -> IO ()
_evalTest text = do
  expr <- either throwIO pure =<< exprFromText "." "(string)" text
  case evalExpr expr of
    Right v  -> putStrLn (Data.Aeson.encode v)
    Left err -> print err
