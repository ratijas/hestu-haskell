#!/usr/bin/env bash

find tests/ -print0 -name '*.yahaha' |\
xargs -0 -n 1 stack exec -- scheme-parser-exe