## Introduction
This shell script creates a layout for a Lisp poject. 

## How to use
The script takes the following arguments:
- -b or -l to ..
- -n

### Make a library
To create the layout for a Lisp library, whose name is "my-library" and whose API depends on
file1.lisp and file2.lisp, execute in shell:
```
./make-lisp-project.sh -l -n my-library file1 file2
```
This produces a folder `my-library` with following content:
```
my-library/
├── api.lisp
├── file1.lisp
├── file2.lisp
├── load.lisp
├── my-library.asd
├── packages.lisp
├── README.md
└── tests
    ├── integration-tests.lisp
    └── unit-tests.lisp
```
The file `load.lisp` contains ASDF command to compile the library.

### Make an executable
To create the layout for a Lisp executable, whose name is "my-executable" and whose main.lisp depends on
file1.lisp and file2.lisp, execute in shell:
```
./make-lisp-project.sh -b -n my-executable file1 file2
```
This produces a folder `my-library` with following content:
```
my-executable/
├── cli.lisp
├── file1.lisp
├── file2.lisp
├── load.lisp
├── main.lisp
├── makefile
├── my-executable.asd
├── packages.lisp
├── README.md
└── tests
    ├── integration-tests.lisp
    └── unit-tests.lisp

```
The file `load.lisp` contains ASDF command to build the executable. Or just use the makefile.

## Tested
* GNU bash, version 4.2.46(2)-release (x86_64-redhat-linux-gnu)
* SBCL 2.1.9
