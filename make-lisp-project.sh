#!/bin/bash


make_executable=0
make_library=0

function print_help()
{
    echo "example ..."
}


# get user argument: make-lisp-project [-bin|-lib] -n project-name file1 file2
while getopts ":hbln::" opt; do
    case $opt in
	h) print_help
	   exit 1
	   ;;
	b) make_executable=1
	   ;;
	l) make_library=1
	   ;;
	n) project_name="$OPTARG"
	   ;;    
    esac
done

project_files=("${@:4}")

if [ -d $project_name ]; then
    echo "Project directory already exists. Exit."
    exit 1
fi

mkdir $project_name
cd $project_name
mkdir tests libs

# creare README.md
echo "## Introduction
## How it works
## How to build
## How to test
## How to use
## Limitations
## Tested
## Contribution
## References" >> README.md

#echo $make_executable
#echo $project_name
#echo $project_files
