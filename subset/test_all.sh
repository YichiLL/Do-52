#!/bin/sh

DO_FIFTY_TWO = "./do_fifty_two"

ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

Usage() {
	echo "Usage: testall.sh [options] [.do files]"
	echo "-k 	Keep intermediate files"
	echo "-h 	Print this Help"
	exit 1
}

SignalError(){
	if[ $error -eq 0 ] ; then
		echo "FAILED"
		error=1
	fi
	echo "	$1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare(){
	generatedfiles="$generatedfiles $3"
	echo diff -b $1 $2 > $3 1>&2
	diff -b "$1" "$2" > 2>&1 || {
		SignalError "$1 differs"
		echo "FAILED $1 differs from $2" 1>&2
	}
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
		SignalError "$1 failed on $*"
		return 1
    }
}

# MORE STUFF...