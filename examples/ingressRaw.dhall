-- Prelude imports
let map = (./_Prelude.dhall).`List`.map

-- dhall-kubernetes types and defaults
in let TLS     = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.extensions.v1beta1.IngressTLS.dhall
in let Rule    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.extensions.v1beta1.IngressRule.dhall
in let RuleVal = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.extensions.v1beta1.HTTPIngressRuleValue.dhall
in let Spec    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.extensions.v1beta1.IngressSpec.dhall
in let Ingress = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.extensions.v1beta1.Ingress.dhall
in let defaultIngress = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.extensions.v1beta1.Ingress.dhall
in let defaultMeta    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
in let defaultSpec    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.extensions.v1beta1.IngressSpec.dhall
in let IntOrString    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.apimachinery.pkg.util.intstr.IntOrString.dhall

-- Our Service type
in let Service = ./Config.dhall
in let GlobalConfig = { services : List Service }

-- A function to generate an ingress given a configuration
in let mkIngress : GlobalConfig -> Ingress =

  \(config : GlobalConfig) ->

  -- Given a service, make a TLS definition with their host and certificate
     let makeTLS = \(service : Service) ->
    { hosts = Some [ service.host ]
    , secretName = Some "${service.name}-certificate"
    }

  -- Given a service, make an Ingress Rule
  in let makeRule = \(service : Service) ->
    { host = Some service.host
    , http = Some
        { paths = [ { backend =
                        { serviceName = service.name
                        , servicePort = IntOrString.Int 80
                        }
                    , path = None Text
                    }
                  ]
        }
    }

  -- Nginx ingress requires a default service as a catchall
  in let defaultService =
    { name = "default"
    , host = "default.example.com"
    , version = " 1.0"
    }

  -- List of services
  in let services = config.services # [ defaultService ]

  -- Some metadata annotations
  -- NOTE: `dhall-to-yaml` will generate a record with arbitrary keys from a list
  -- of records where mapKey is the key and mapValue is the value of that key
  in let genericRecord = List { mapKey : Text, mapValue : Text }
  in let kv = \(k : Text) -> \(v : Text) -> { mapKey = k, mapValue = v }

  in let annotations = Some
    [ kv "kubernetes.io/ingress.class"      "nginx"
    , kv "kubernetes.io/ingress.allow-http" "false"
    ]

  -- Generate spec from services
  in let spec = defaultSpec //
    { tls   = Some (map Service TLS  makeTLS  services)
    , rules = Some (map Service Rule makeRule services)
    }

  in defaultIngress
    { metadata = defaultMeta
      { name = "nginx" } //
      { annotations = annotations }
    } //
    { spec = Some spec }


-- Here we import our example service, and generate the ingress with it
in \( serviceConfig : Config) -> mkIngress { services = [ serviceConfig ] }
