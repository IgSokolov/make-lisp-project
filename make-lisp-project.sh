#!/bin/bash


make_executable=false
make_library=false

function print_help()
{
    echo "example ..."
}

########## parse cli args ############

# get user argument: make-lisp-project [-bin|-lib] -n project-name file1 file2
while getopts ":hbln::" opt; do
    case $opt in
	h) print_help
	   exit 1
	   ;;
	b) make_executable=true
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

######## create README #############

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


######## create project_name.asd ###########

function dependencies_on_package_file()
{
    local list=""
    local tab=$"\n\t\t\b"
    for filename in "$@"
    do
        :
	list+=$tab'(:file '\"$filename\"' :depends-on ("packages"))'
    done
    echo $list$tab
}

function dependencies_for_api()
{
    list=""
    for filename in "$@"
    do
        :
	list+=\"$filename\"$' '
    done
    echo $list
}

function make_asdf_appendix_for_executable()
{
    newline="\n\t\b\b\b\b\b\b"
    if $1; then
	echo "$newline:build-operation \"program-op\"
$newline:build-pathname \"$project_name\"
$newline:entry-point \"$project_name.main:run\")"
    else
	echo ""
    fi    
}

dep1=$(dependencies_on_package_file "${project_files[@]}")
dep2=$(dependencies_for_api "${project_files[@]}")
appendix_for_executables=$(make_asdf_appendix_for_executable $make_executable)

echo -ne '(asdf:defsystem "'$project_name'"
  :description ""
  :author ""
  :licence ""
  :version ""
  :components ((:file "packages")'"$dep1"'(:file "api" :depends-on ("packages" '"$dep2"')))'$appendix_for_executables')'$'\n' > $project_name.asd

echo ' ' >> $project_name.asd

echo '(asdf:defsystem "'$project_name'/u-test"
  :description "Unit tests for '$project_name'
  :author ""
  :licence ""
  :version ""
  :depends-on ('\"$project_name\"')
  :components ((:file "packages")
	       (:file "tests/unit-tests")))' >> $project_name.asd

echo ' ' >> $project_name.asd

echo '(asdf:defsystem "'$project_name'/i-test"
  :description "Integration tests for '$project_name'
  :author ""
  :licence ""
  :version ""
  :depends-on ('\"$project_name\"')
  :components ((:file "packages")
	       (:file "tests/integration-tests")))' >> $project_name.asd

######### create load file #########

function make_load_appendix_for_executable()
{    
    if $1; then
	echo "\n(asdf:make \"$project_name\")"
    else
	echo ""
    fi    
}

appendix_for_executables=$(make_load_appendix_for_executable $make_executable)

echo -ne '(require "asdf")
(asdf:load-system "'$project_name'")'$appendix_for_executables'
;;(asdf:load-system "'$project_name'/u-test") ;; to build unit-tests
;;(asdf:load-system "'$project_name'/i-test") ;; to build integrations-tests\n' > load.lisp

