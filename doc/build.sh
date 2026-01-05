#!/bin/sh

cd "$(dirname "$0")" || exit 1

mkdir -p _build
cp ./*.ml ./_build/
cd _build || exit 1

FLAGS="-dlambda -S"

# shellcheck disable=SC2086
ocamlopt $FLAGS -o stack stack.ml 2>stack.lambda

# shellcheck disable=SC2086
ocamlopt $FLAGS -o heap heap.ml 2>heap.lambda

# Modify the generated assembly files to use a common label for comparison
sed -i 's/camlStack/camlProgram/g' stack.s
sed -i 's/camlHeap/camlProgram/g' heap.s

diff --color=always -y heap.s stack.s
# diff --color=always -y heap.lambda stack.lambda
