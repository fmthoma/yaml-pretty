yaml-pretty
================================================================================

`yaml-pretty` is a Haskell library for layouting and pretty-printing YAML.
Objects and arrays are layed out as in-line blocks (JSON-style) if they fit
within one line, otherwise as multi-line blocks.

`yq` is a terminal frontend that accepts YAML on stdin and pretty-prints it. It
is particular useful for pretty-printing JSON documents (of which YAML is a
superset) without all the noise.

Usage examples:

    cat document.yaml | yq
    curl -s https://api.github.com/users/fmthoma | yq
