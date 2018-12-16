#!/bin/sh

# Path to the LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the hippograph compiler.  Usually "./hippograph.native"
# Try "_build/hippograph.native" if ocamlbuild was unable to create a symbolic link.
HIPPOGRAPH="./hippograph.native"
#HIPPOGRAPH="_build/hippograph.native"

HPG=".hpg"
BASENAME="${1%$HPG}"

if [ "$#" -ne 1 ]; then
    echo "usage: ./run.sh filename.hpg"
	exit
fi

generatedfiles="$generatedfiles $BASENAME.ll $BASENAME.s $BASENAME.exe $BASENAME.out" &&
"$HIPPOGRAPH" "$1" > "$BASENAME.ll"
"$LLC" "-relocation-model=pic" "$BASENAME.ll" > "$BASENAME.s"
"$CC" "-o" "$BASENAME.exe" "$BASENAME.s" "graph.o"
"./$BASENAME.exe"
rm -f $generatedfiles