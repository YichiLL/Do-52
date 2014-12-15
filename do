#!/bin/bash

make
./compile $1 && run_java
make clean

function run_java {
	mv Game.java MyPlayer.java runtime/
	(cd runtime && make run && make clean)
	cd ..
}

