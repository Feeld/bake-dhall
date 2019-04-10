let Prelude = ./Prelude
let map = Prelude.`List`.map
let kv = Prelude.JSON.keyText
let types = ./types
let defaults = ./defaults
let Config = ./Config.dhall

let Service = { name : Text, host : Text, version : Text }

let services = [ { name = "foo", host = "foo.example.com", version = "2.3" } ]

let makeTLS
    : Service -> types.IngressTLS
    =     \(service : Service)
      ->  { hosts =
              [ service.host ]
          , secretName =
              Some "${service.name}-certificate"
          }

let makeRule
    : Service -> types.IngressRule
    =     \(service : Service)
      ->  { host =
              Some service.host
          , http =
              Some
              { paths =
                  [ { backend =
                        { serviceName =
                            service.name
                        , servicePort =
                            types.IntOrString.Int 80
                        }
                    , path =
                        None Text
                    }
                  ]
              }
          }

let mkIngress
    : List Service -> types.Ingress
    =     \(inputServices : List Service)
      ->  let annotations =
                [ kv "kubernetes.io/ingress.class" "nginx"
                , kv "kubernetes.io/ingress.allow-http" "false"
                ]

          let defaultService =
                { name =
                    "default"
                , host =
                    "default.example.com"
                , version =
                    " 1.0"
                }

          let ingressServices = inputServices # [ defaultService ]

          let spec =
                    defaults.IngressSpec
                //  { tls =
                        map Service types.IngressTLS makeTLS ingressServices
                    , rules =
                        map Service types.IngressRule makeRule ingressServices
                    }

          in      defaults.Ingress
              //  { metadata =
                          defaults.ObjectMeta
                      //  { name = "nginx", annotations = annotations }
                  , spec =
                      Some spec
                  }

in \(_ : Config) -> mkIngress services
