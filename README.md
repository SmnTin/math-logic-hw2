# math-logic-hw2

## Prerequisites

* [Stack](https://docs.haskellstack.org/en/stable/README/)
* [MiniSAT](http://minisat.se/)

## Quick start

Firstly, obtain a local copy of the code and move to the project root:
```console
$ git clone https://github.com/SmnTin/math-logic-hw2
$ cd math-logic-hw2
```

From the project root, where the `package.yaml` is located, run the following to build and test the app:
```console
$ stack update
$ stack build
$ stack test
```

To run the app execute:
```console
$ stack run queens 8
4 1
2 2
7 3
3 4
6 5
8 6
5 7
1 8
```
If it is possible to place the queens, you get 1-indexed coordinates of each queen. 

If it is impossible to place the queens, an empty output will be printed:
```console
$ stack run queens 2
$
```
