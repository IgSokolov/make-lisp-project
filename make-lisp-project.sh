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

# if [ -d $project_name ]; then
#     echo "Project directory already exists. Exit."
#     exit 1
# fi

# create directories
rm -r $project_name # rm after degug
mkdir $project_name
cd $project_name
mkdir tests libs

# create README.md
echo "## Introduction
## How it works
## How to build
## How to test
## How to use
## Limitations
## Tested
## Contribution
## References" > README.md

function depend_on_package_file()
{
    local list=""
    local prefix=$"\n\\t\t\b"
    local postfix=""
    for filename in "$@"
    do
        :
	list+=$prefix'(:file '\"$filename\"' :depends-on ("packages"))'
    done
    echo $list
}

function dependencies_for_api()
{
    list=
    for filename in "$@"
    do
        :
	list+=\"$filename\"$' '
    done
    echo $list
}

dep1=$(depend_on_package_file "${project_files[@]}")
dep2=$(dependencies_for_api "${project_files[@]}")
#echo -e $result

#create project_name.asd
echo -ne '(asdf:defsystem "'$project_name'"
  :description ""
  :author ""
  :licence ""
  :version ""
  :components ((:file "packages")'"$dep1"$"\n\\t\t\b"'(:file "api" :depends-on ("packages" '"$dep2"'))))'$'\n' > $project_name.asd



# (asdf:defsystem "cl-replica/test"
#   :description "Unit-tests for cl-replica"
#   :author ""
#   :licence ""
#   :version ""
#   :depends-on ("cl-replica")
#   :components ((:file "packages")
# 	       (:file "unit-tests")))  

## bins:

# (asdf:defsystem "daq-server"
#   :description "start/stop DAQ"
#   :author "Dr.-Ing. Igor Sokolov"
#   :licence "BSD"
#   :version "1.0.0"
#   :components ((:file "packages")
# 	       (:file "logging")
# 	       (:file "cli")
# 	       (:file "main" :depends-on ("packages" "logging" "cli")))
#   :depends-on (:unix-opts :cffi :pzmq :cl-ppcre)
#   :build-operation "program-op"
#   :build-pathname "daq-server"
#   :entry-point "daq-server.main:run")

#echo $make_executable
#echo $project_name
#echo $project_files
