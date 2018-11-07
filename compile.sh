cat $1 | ./hippograph.native > source.llvm; llc source.llvm -o source.s; gcc source.s -o main; rm source.s source.llvm
