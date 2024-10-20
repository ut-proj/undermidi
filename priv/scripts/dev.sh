#!/bin/bash

rm -rf _build/*/lib/undermidi
rebar3 as undermidi repl
