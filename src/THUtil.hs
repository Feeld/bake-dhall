{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module THUtil where

import qualified Codec.Compression.Lzma     as Lzma
import qualified Codec.Serialise

import qualified Dhall
import           Dhall.Core (Expr)
import qualified Dhall.Binary
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import           Language.Haskell.TH.Syntax
import           Protolude
import qualified Prelude
import qualified System.IO

staticDhallExpressionAndType :: Text -> Q Exp
staticDhallExpressionAndType text = do
  (expression, expressionTy) <- runIO $ do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
    expr <- Dhall.inputExpr text
    case Dhall.TypeCheck.typeOf expr of
      Right ty -> pure (exprToStringL expr, exprToStringL ty)
      Left err -> die $ show err
  [|(deserializeExpr $(pure expression), deserializeExpr $(pure expressionTy))|]


exprToStringL :: Dhall.Binary.ToTerm a => Expr s a -> Exp
exprToStringL =
   LitE
 . StringL
 . toS
 . Lzma.compressWith compressOptions
 . Codec.Serialise.serialise
 . Dhall.Binary.encode
 where
  compressOptions = Lzma.defaultCompressParams
    { Lzma.compressLevelExtreme = True
    , Lzma.compressLevel = Lzma.CompressionLevel9
    }

deserializeExpr :: Prelude.String -> Expr s a
deserializeExpr str = either (Prelude.error . ("could not deserialize embedded dhall expr: "<>)) identity $ do
  term <- first show $ Codec.Serialise.deserialiseOrFail (Lzma.decompress $ toS str)
  exprI <- first show $ Dhall.Binary.decodeExpression term
  traverse (const (Left "Import resolution disabled")) exprI
