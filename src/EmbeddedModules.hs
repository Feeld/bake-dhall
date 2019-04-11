{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module EmbeddedModules (replaceBuiltinModules, insertEmbeddedModuleTypes) where

import           THUtil (staticDhallExpressionAndType)
import qualified Dhall.Context
import           Dhall.Core      (Expr (..), subst)
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


replaceBuiltinModules :: Expr s a -> Expr s a
replaceBuiltinModules =
   subst "Prelude"        prelude
 . subst "k8s/typesUnion" k8sUnion
 . subst "k8s/defaults"   k8sDefaults
 . subst "k8s/types"      k8sTypes


prelude :: Expr s a
preludeTy :: Expr s Dhall.TypeCheck.X
(prelude, preludeTy) = $(staticDhallExpressionAndType "./dhall/Prelude.dhall")

k8sUnion :: Expr s a
k8sUnionTy :: Expr s Dhall.TypeCheck.X
(k8sUnion, k8sUnionTy) = $(staticDhallExpressionAndType "./dhall/typesUnion.dhall")

k8sTypes :: Expr s a
k8sTypesTy :: Expr s Dhall.TypeCheck.X
(k8sTypes, k8sTypesTy) = $(staticDhallExpressionAndType "./dhall/types.dhall")

k8sDefaults :: Expr s a
k8sDefaultsTy :: Expr s Dhall.TypeCheck.X
(k8sDefaults, k8sDefaultsTy) = $(staticDhallExpressionAndType "./dhall/defaults.dhall")
