#!/bin/sh

function print_commands {
	echo "Commands:"

	echo "  compile: Compile a file"
	echo "  file"
	echo "  da: Debug analyzer"
	echo "  dd: Disable debug"
	echo "  dir: Debug IR"
	echo "  dp: Debug parser"
	echo "  dpc: Debug precompiler"
	echo "  dpp: Debug preparser"
	echo "  ds: Debug scanner"
	echo "  ed: Enable debug"
	echo "  exit: Exit to the REPL"
	echo "  file: Set a new value to the file"
	echo "  help: Print the help"
	echo "  rupc: Run until precompiler"
	echo "  rupp: Run until preparser"
}

function do_nothing {
	echo "do nothing for the moment."
}

CURRENT_FILE="./tests/mypkg/main.lily"

echo "Welcome to the debug REPL."

print_commands

echo

while [ 1 ]
do	
	echo -n "> "

	read input
	
	case $input in
		"compile")
			./build/Debug/lily compile $CURRENT_FILE
			;;
		"da")
			do_nothing
			;;
		"dd")
			cd patches && patch ../include/base/macros.h disable_debug.patch && cd ..
			;;
		"dir")
			do_nothing
			;;
		"dp")
			do_nothing
			;;
		"dpc")
			do_nothing
			;;
		"dpp")
			do_nothing
			;;
		"ds")
			do_nothing
			;;
		"ed")
			cd patches && patch ../include/base/macros.h enable_debug.patch && cd ..
			;;
		"exit")
			break
			;;
		"file")
			echo -n "file> "
			read file
			CURRENT_FILE=$file
			echo "A new file has been successfully configured."
			;;
		"help")
			print_commands
			;;		
		"rupc")
			do_nothing
			;;
		"rupp")
			do_nothing
			;;
		*)
			echo "error: bad command"
			echo "please consult the command help"
			;;
	esac
done
