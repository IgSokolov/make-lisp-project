#!/bin/bash

make_executable=false
make_library=false

function print_help()
{
    echo "To create library: ./make-lisp-project.sh -l -n test-project file1 file2"
    echo "To create binary:  ./make-lisp-project.sh -b -n test-project file1 file2"
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
	l) make_library=true
	   ;;
	n) project_name="$OPTARG"
	   ;;    
    esac
done

if $make_executable && $make_binary
then
    echo "Provide either -l (for library) or -b (for binary)"
    exit 1
fi

project_files=("${@:4}")

# if [ -d $project_name ]; then
#     echo "Project directory already exists. Exit."
#     exit 1
# fi

# create directories
rm -r $project_name # rm after degug
mkdir $project_name
cd $project_name
mkdir tests

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
    local tab=$"\n\t"
    for filename in "$@"
    do
        :
	list+=$tab'(:file '\"$filename\"' :depends-on ("packages"))'
    done
    echo -ne $list$tab
}

function dependencies_for_api_or_main()
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
    newline="\n\t"
    if $1; then
	echo "$newline:build-operation \"program-op\"
$newline:build-pathname \"$project_name\"
$newline:entry-point \"$project_name.main:run\")"
    else
	echo ")"
    fi    
}

function main_else_api()
{
    if $1; then
	echo "main"
    else
	echo "api"
    fi    
}

function add_cli_for_executable()
{
    if $1; then
	project_files+=("cli")
    fi    
}

add_cli_for_executable $make_executable
dep1=$(dependencies_on_package_file "${project_files[@]}")
dep2=$(dependencies_for_api_or_main "${project_files[@]}")
appendix_for_executables=$(make_asdf_appendix_for_executable $make_executable)
entry_point=$(main_else_api $make_executable)

echo -ne '(asdf:defsystem "'$project_name'"
  :description "tbd"
  :author "tbd"
  :licence "BSD"
  :version "1.0.0"
  :components ((:file "packages")'"$dep1"'(:file '\"$entry_point\"' :depends-on ("packages" '"$dep2"')))'$appendix_for_executables$'\n' > $project_name.asd

echo ' ' >> $project_name.asd

echo '(asdf:defsystem "'$project_name'/u-test"
  :description "Unit tests for '$project_name'"  
  :depends-on ('\"$project_name\"')
  :components ((:file "packages")
	       (:file "tests/unit-tests")))' >> $project_name.asd

echo ' ' >> $project_name.asd

echo '(asdf:defsystem "'$project_name'/i-test"
  :description "Integration tests for '$project_name'"  
  :depends-on ('\"$project_name\"')
  :components ((:file "packages")
	       (:file "tests/integration-tests")))' >> $project_name.asd

######### create load file #########
function make_load_appendix_for_executable()
{    
    if $1; then
	echo "\n;;(asdf:make \"$project_name\") ;; to make executable"
    else
	echo ""
    fi    
}

appendix_for_executables=$(make_load_appendix_for_executable $make_executable)

echo -ne '(require "asdf")
(push #P"'$(pwd)'/" asdf:*central-registry*)
(asdf:load-system "'$project_name'")'$appendix_for_executables'
;;(asdf:load-system "'$project_name'/u-test") ;; to build unit-tests
;;(asdf:load-system "'$project_name'/i-test") ;; to build integrations-tests\n' > load.lisp


######### create src files ############
inpackage_prefix="(in-package :"$project_name"."
function make_src_file()
{
    echo -ne $inpackage_prefix$1")\n\n" > $1.lisp
}

if $make_executable; then
    make_src_file main
else
    make_src_file api
fi

for filename in "${project_files[@]}"
do    
    make_src_file $filename
done


if $make_executable; then
# fill cli.lisp
echo "(defun parse-cli-args ()
  (values 1 2))" >> cli.lisp

# fill main.lisp
echo ";; Entry point of the program
(defun run()
  (handler-case
      (multiple-value-bind (arg) (parse-cli-args) ;; <- parse cli ang go
	(format t \"arg = ~a~%\" arg))
    ;; catch sigint
    (sb-sys:interactive-interrupt () (sb-ext:exit))))" >> main.lisp
# create & fill makefile
echo "build:
	sbcl --load \"load.lisp\"" > makefile
fi

cd tests
make_src_file unit-tests
make_src_file integration-tests
cd ..

######### package file ###############
defpackage_prefix="(defpackage :"$project_name"."

function make_use_clause()
{
    local list="(:use :cl"
    for filename in "$@"
    do
        :
	list+=" :$project_name."$filename
    done
    echo $list")"
}

function make_export_clause()
{
    local list="(:export"
    for filename in "$@"
    do
        :
	list+=" :"$filename
    done
    echo $list"))"
}

touch packages.lisp
function make_defpackage()
{
    local newline="\n\t"
    echo -ne $defpackage_prefix"$1"$newline >> packages.lisp
    echo -ne $2 >> packages.lisp
    if [[ $3 == '' ]]; then
	echo -ne ")\n\n" >> packages.lisp
    else 
	echo -ne $newline$3'\n\n' >> packages.lisp
    fi
}


if $make_executable; then
    defpackage_list=("${project_files[@]::${#project_files[@]}-1}")  # all but cli.lisp
else
    defpackage_list=("${project_files[@]}")
fi

for filename in "${defpackage_list[@]}"
do    
    make_defpackage $filename "$(make_use_clause)"
done

if $make_executable; then
    make_defpackage cli "$(make_use_clause "${project_files[@]::${#project_files[@]}-1}")" "$(make_export_clause "parse-cli-args")"
    make_defpackage main "$(make_use_clause "${project_files[@]}")" "$(make_export_clause "run")"    
else
    make_defpackage api "$(make_use_clause "${project_files[@]}")"
fi

if $make_executable; then    
    project_files+=("main")
else
    project_files+=("api")
fi

make_defpackage u-tests "$(make_use_clause "${project_files[@]}")"
make_defpackage i-tests "$(make_use_clause "${project_files[@]}")"
