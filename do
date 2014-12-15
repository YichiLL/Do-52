#!/bin/bash
make
./compile $1 
mv Game.java MyPlayer.java runtime/
make clean
(cd runtime  && make run && make clean)
cd ..

