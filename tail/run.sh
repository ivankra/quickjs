#!/bin/bash
export PATH=$HOME/src/zoo/bench:$PATH
while :; do
  for cc in clang22 gcc16; do
    bench -a -r 100 -r zlib.js:10 qjs-$cc-o3-direct qjs-$cc-o3-master qjs-$cc-o3-tail
  done
  for cc in gcc14 gcc15 gcc16 clang19 clang20 clang21 clang22; do
    bench -a -r 50 -r zlib.js:5 qjs-$cc-direct qjs-$cc-master qjs-$cc-tail
  done
done
#while :; do bench -a -r 10 qjs{02..11}-???????; done
