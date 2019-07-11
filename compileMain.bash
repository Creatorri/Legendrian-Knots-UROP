#!/bin/sh

ghc -o bin/aug -threaded -O2 --make -odir bin/ -hidir bin/ -optc-o3 -optc-ffast-math Main.hs
