#!/bin/bash 
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" 
stack exec -- Pearls +RTS -p
