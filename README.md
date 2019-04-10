# bake-dhall

Evaluate a template dir:

```sh
bake-dhall template -t examples -c examples/sampleConfig.json
```

Pack a template dir into a portable archive

```sh
bake-dhall pack -t examples -o packed
```

Evaluate a packed archive dir:

```sh
bake-dhall unpack -i packed -c examples/sampleConfig.json
```
