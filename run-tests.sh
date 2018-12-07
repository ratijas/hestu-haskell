#!/usr/bin/env bash

bin=${bin:-hestu}

find tests -name '*.yahaha' -print0 |\
    xargs -0 -n1 stack exec -- "$bin"
