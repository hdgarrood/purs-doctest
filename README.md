# purs-doctest

Experimental support for doctests in PureScript. See `example/`

```
$ cd example
$ stack exec purs-doctest
Compiling $Doctest.Example
Compiling $Doctest.$Main
/home/harry/code/purs-doctest/example/.purs_doctest_modules/node_modules/$Doctest.$Main/index.js
Example
  silly
    1: PASS
    2: PASS
  interleave
    1: FAIL: Expected (1:2:3:4:5:6:Nil), got (1 : 2 : 3 : 4 : 5 : 6 : Nil)
    2: FAIL: Expected (1:2:3:4:5:6:Nil), got (1 : 2 : 3 : 4 : 5 : 6 : Nil)
2/4 tests passed.
```
