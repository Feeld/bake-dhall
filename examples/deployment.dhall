let config =
  https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/api/Deployment/default
  //
  { name = "nginx"
  , replicas = 2
  , containers =
    [ https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/api/Deployment/defaultContainer
      //
      { name = "nginx"
      , imageName = "nginx"
      , imageTag = "1.15.3"
      , port = [ 80 ] : Optional Natural
      }
    ]
  }

in \(_ : Config) -> https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/api/Deployment/mkDeployment config
