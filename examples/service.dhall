let config =
  https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/api/Service/default
  //
  { name = "nginx"
  , containerPort = 80
  }

in \(_ : Config) ->
  https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/69c85131d17816889311905aaacfcb621dcaf59c/api/Service/mkService config
