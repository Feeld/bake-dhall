{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main (main) where

import           BakeDhall       (evalWithValue, exprFromFile)
import           Data.Aeson
import qualified Data.Yaml
import qualified Dhall
import           Protolude
import           System.IO.Error (userError)

main :: IO ()
main = Dhall.detailed $ do
  args <- getArgs
  case args of
    (tyExprFile:cfgFile:files) -> do
      cfgValue <- either (throwIO . userError) pure
        =<< (eitherDecode @Value . toS <$> readFile cfgFile)
      tyExpr <- exprFromFile tyExprFile
      values <- traverse (evalWithValue tyExpr cfgValue <=< exprFromFile) files
      forM_ (zip files values) $ \(f,v) -> do
        putStrLn @Text "---"
        putStrLn @Text $ "# from: " <> show f
        putStrLn (Data.Yaml.encode v)
    _ -> die "Usage: bake-dhall CONFIG_SIG CONFIG_JSON FILE_1 ... FILE_N"
