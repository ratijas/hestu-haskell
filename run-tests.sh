#!/usr/bin/env bash

bin=${bin:-scheme-parser-exe}

find tests -name '*.yahaha' -print0 |\
    xargs -0 -n1 stack exec -- "$bin"
