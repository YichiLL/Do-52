#!/bin/sh

DO_FIFTY_TWO="./compile"
JAVA="javac"
RUNTIME="runtime/"
MAIN="main"

# Set time limit for all operations
ulimit -t 30

globallog=testLogic.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: run_tests.sh [options] [.do files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
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

# Run <args>
# Report the command, run it, and report any errors
RunFail() {
    echo $* 1>&2
    eval $* && {
    SignalErrorFail "$1 failed on $*"
    return 0
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.do//'`
    reffile=`echo $1 | sed 's/.do$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    javafile=`echo $basename |sed -e 's/^//g' -e 's/-/_/g'`
    ajavafile=`echo $javafile | perl -pe 's/\S+/\u$&/g'`

    newjavafile=`echo $ajavafile | perl -pe 's/([^ ])_([a-z])/\\1\\u\\2/g'`

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles="$generatedfiles tests/${newjavafile}.java tests/${basename}.diff tests/${basename}.out" &&
    Run "$DO_FIFTY_TWO" $1 &&
    Run "mv Game.java MyPlayer.java $RUNTIME" && 
    #Run "make clean" &&
    Run "cd runtime/" &&
    Run "javac -g MyPlayer.java" &&
    Run "javac -g Game.java" &&
    Run "java -cp . $MAIN >" ../tests/${basename}.out &&
    Run "cd .." &&
    #Run "$JAVA" tests/${newjavafile}.java ">" tests/${basename}.out
    Compare tests/${basename}.out tests/${basename}.gold tests/${basename}.diff
    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK - $basename succeeds"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}


shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test_*.do"
fi

for file in $files
do
    case $file in
    *test_*)
        Check $file 2>> $globallog
        ;;
    *)
        echo "unknown file type $file"
        globalerror=1
        ;;
    esac
done

exit $globalerror