let k8s = ./types
let TypesUnion =
  < Deployment : k8s.Deployment
  | Service    : k8s.Service
  | Ingress    : k8s.Ingress
  >
let Config = ./Config.dhall
in \(cfg : Config) ->
  [ TypesUnion.Deployment (./deployment.dhall       cfg)
  , TypesUnion.Deployment (./deploymentSimple.dhall cfg)
  , TypesUnion.Service    (./service.dhall          cfg)
  , TypesUnion.Ingress    (./ingress.dhall          cfg)
  ] : List TypesUnion
