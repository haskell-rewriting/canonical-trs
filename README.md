# canonical-trs

## purpose

`canonical-trs` is a tool that takes a TRS and transforms it into a
canonical form which is invariant under permutation of rules and
renaming of function symbols.

Such a canonical form used to efficiently identify equivalent TRSs.

Note that the output *will* change between versions.

## example

```
# canonical-trs canonical-trs cops/1.trs
(COMMENT produced by canonical-trs 0.0)



(VAR
v0
v1)
(RULES
f0(f1(v0)
,v0) ->
f0(v0
,f1(v0))
f0(f2(v0)
,v0) ->
f0(v0
,f2(v0))
f3(v0
,v1) ->
v0
f3(v0
,v1) ->
f3(v0
,f1(v1))
f1(v0) ->
f2(v0))

```

## capabilities

* WST format https://www.lri.fr/~marche/tpdb/format.html
* XTC format http://termination-portal.org/wiki/TPDB_XML_Format

## requirements

all dependencies are available from hackage. http://hackage.haskell.org/

## wishlist

* support for HRS and CTRS
* support for permutation of function arguments (hard)
