let types = ./types 
let defaults = ./defaults
let Config = ./Config.dhall

let kv = (./Prelude).JSON.keyText

let deployment
    : types.Deployment
    =     defaults.Deployment
      //  { metadata =
              defaults.ObjectMeta // { name = "nginx" }
          , spec =
              Some
              (     defaults.DeploymentSpec
                //  { replicas =
                        Some 2
                    , revisionHistoryLimit =
                        Some 10
                    , selector =
                            defaults.LabelSelector
                        //  { matchLabels = [ kv "app" "nginx" ] }
                    , strategy =
                        Some
                        (     defaults.DeploymentStrategy
                          //  { type =
                                  Some "RollingUpdate"
                              , rollingUpdate =
                                  { maxSurge =
                                      Some (types.IntOrString.Int 5)
                                  , maxUnavailable =
                                      Some (types.IntOrString.Int 0)
                                  }
                              }
                        )
                    , template =
                            defaults.PodTemplateSpec
                        //  { metadata =
                                    defaults.ObjectMeta
                                //  { name =
                                        "nginx"
                                    , labels =
                                        [ kv "app" "nginx" ]
                                    }
                            , spec =
                                Some
                                (     defaults.PodSpec
                                  //  { containers =
                                          [     defaults.Container
                                            //  { name =
                                                    "nginx"
                                                , image =
                                                    Some "nginx:1.15.3"
                                                , imagePullPolicy =
                                                    Some "Always"
                                                , ports =
                                                    [     defaults.ContainerPort
                                                      //  { containerPort = 80 }
                                                    ]
                                                , resources =
                                                    Some
                                                    { limits =
                                                        [ kv "cpu" "500m" ]
                                                    , requests =
                                                        [ kv "cpu" "10m" ]
                                                    }
                                                }
                                          ]
                                      }
                                )
                            }
                    }
              )
          }

in \(_ : Config) -> deployment
