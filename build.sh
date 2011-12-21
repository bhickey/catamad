#!/bin/bash
runhaskell Setup configure --prefix=$HOME --user
runhaskell Setup build
runhaskell Setup install
