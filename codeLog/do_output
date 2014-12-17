#!/bin/bash

RunJava() {
	mv Game.java MyPlayer.java runtime/
	(cd runtime && make run && make clean)
}

make
./compile $1 && RunJava
make clean
