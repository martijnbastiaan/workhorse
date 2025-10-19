#!/usr/bin/env bash
fourmolu --mode inplace $(git ls-files *.hs)
cabal-gild -i workhorse.cabal -o workhorse.cabal
