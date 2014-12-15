#!/bin/bash
make
./compile $1 
mv Game.java MyPlayer.java runtime/
(cd runtime && make run)
cd ..

