-- Prelude imports
   let map = (./_Prelude.dhall).`List`.map

-- import dhall-kubernetes types and defaults
in let Deployment    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.apps.v1beta2.Deployment.dhall
in let Spec          = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.apps.v1beta2.DeploymentSpec.dhall
in let PodSpec       = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.core.v1.PodSpec.dhall
in let ContainerPort = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/types/io.k8s.api.core.v1.ContainerPort.dhall
in let defaultDeployment    = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.apps.v1beta2.Deployment.dhall
in let defaultMeta          = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
in let defaultSpec          = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.apps.v1beta2.DeploymentSpec.dhall
in let defaultTemplate      = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.core.v1.PodTemplateSpec.dhall
in let defaultPodSpec       = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.core.v1.PodSpec.dhall
in let defaultSelector      = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
in let defaultContainer     = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.core.v1.Container.dhall
in let defaultContainerPort = https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/default/io.k8s.api.core.v1.ContainerPort.dhall

{-

Here we import the Config type.
It's going to be the input to our mkDeployment function,
and contains the configuration for the Deployment.

-}
in let Config = ./Config.dhall


-- So here we define a function that outputs a Deployment
in let mkDeployment : Config -> Deployment =

  \(deployment : Config) ->

     let selector = Some [{ mapKey = "app", mapValue = deployment.name }]

  in let spec = defaultSpec
    { selector = defaultSelector // { matchLabels = selector }
    , template = defaultTemplate
      { metadata = defaultMeta
        { name = deployment.name } // { labels = selector }
      } //
      { spec = Some (defaultPodSpec
        { containers = [
          defaultContainer
            { name = deployment.name } //
            { image = Some "your-container-service.io/${deployment.name}:${deployment.version}"
            , imagePullPolicy = Some "Always"
            , ports = Some [(defaultContainerPort {containerPort = 8080})]
            }
          ]
        })
      }
    } //
    { replicas = Some 2
    , revisionHistoryLimit = Some 10
    }

  in defaultDeployment
    { metadata = defaultMeta { name = deployment.name }
    } //
    { spec = Some spec } : Deployment


in mkDeployment
