# bake-dhall

Evaluate a template dir:

```sh
bake-dhall evaluate -i examples/index.dhall -c examples/sampleConfig.json
```

Pack a template dir into a portable archive

```sh
bake-dhall pack -i examples/index.dhall -o packed.xz
```

Evaluate a packed archive dir:

```sh
bake-dhall unpack -i packed.xz -c examples/sampleConfig.json
```
