#!/bin/bash
export RUNHASKELL="stack exec runhaskell --"
export EXECUTABLE="stack exec -- minhs-1 "
./run_tests_cabal.sh $1 $2 $3 $4 $5
