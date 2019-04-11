{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module EmbeddedModules (replaceBuiltinModules, insertEmbeddedModuleTypes) where

import qualified Dhall.Context
import           Dhall.Core      (Expr (..), Var)
import           Dhall.TH        (staticDhallExpression)
import qualified Dhall.TypeCheck
import           Protolude       hiding (Const)

insertEmbeddedModuleTypes
  :: Dhall.Context.Context (Expr s Dhall.TypeCheck.X)
  -> Dhall.Context.Context (Expr s Dhall.TypeCheck.X)
insertEmbeddedModuleTypes
  = Dhall.Context.insert "Prelude"        preludeTy
  . Dhall.Context.insert "k8s/typesUnion" k8sUnionTy
  . Dhall.Context.insert "k8s/types"      k8sTypesTy
  . Dhall.Context.insert "k8s/defaults"   k8sDefaultsTy


replaceBuiltinModules :: Expr s1 a1 -> Maybe (Expr s2 a2)
replaceBuiltinModules e =
     replaceBuiltinModule "Prelude"        prelude  e
 <|> replaceBuiltinModule "k8s/typesUnion" k8sUnion e
 <|> replaceBuiltinModule "k8s/defaults"   k8sDefaults e
 <|> replaceBuiltinModule "k8s/types"      k8sTypes e

replaceBuiltinModule
  :: Dhall.Core.Var
  -> Expr s1 a1
  -> Expr s2 a2
  -> Maybe (Expr s1 a1)
replaceBuiltinModule name repl (Field (Var vName) n)
  | name == vName
  = Just (Field repl n)
replaceBuiltinModule name repl (Field e n) =
  flip Field n <$> replaceBuiltinModule name repl e
replaceBuiltinModule _ _ _ = Nothing

prelude :: Expr s a
prelude = $(staticDhallExpression "./dhall/Prelude.dhall")
preludeTy :: Expr s Dhall.TypeCheck.X
Right preludeTy = Dhall.TypeCheck.typeOf prelude

k8sUnion :: Expr s a
k8sUnion = $(staticDhallExpression "./dhall/typesUnion.dhall")
k8sUnionTy :: Expr s Dhall.TypeCheck.X
Right k8sUnionTy = Dhall.TypeCheck.typeOf k8sUnion

k8sTypes :: Expr s a
k8sTypes = $(staticDhallExpression "./dhall/types.dhall")
k8sTypesTy :: Expr s Dhall.TypeCheck.X
Right k8sTypesTy = Dhall.TypeCheck.typeOf k8sTypes

k8sDefaults :: Expr s a
k8sDefaults = $(staticDhallExpression "./dhall/defaults.dhall")
k8sDefaultsTy :: Expr s Dhall.TypeCheck.X
Right k8sDefaultsTy = Dhall.TypeCheck.typeOf k8sDefaults
