haddocset [![Build Status](https://travis-ci.org/philopon/haddocset.svg?branch=master)](https://travis-ci.org/philopon/haddocset)
===
Generate docset of Dash from Haddock

similar projects
---
* [jfeltz/dash-haskell](https://github.com/jfeltz/dash-haskell)

Usage/Example
---

```
$ haddocset --help
Usage: haddocset [--hc-pkg CMD] [-t|--target DOCSET] [-q|--quiet] COMMAND

Available options:
  -h,--help                Show this help text
  --hc-pkg CMD             hc-pkg command (default: ghc-pkg)
  -t,--target DOCSET       output directory (default: haskell.docset)
  -q,--quiet               suppress output.

Available commands:
  create                   create new docset.
  list                     list package of docset.
  add                      add package to docset.

# create output.docset using global packages.
$ haddocset -t output create 
[1/5] Create Directory.
[2/5] Writing plist.
[3/5] Migrate Database.
    Global package directory: /usr/local/Cellar/ghc/7.6.3/lib/ghc-7.6.3/package.conf.d
    Global package count:     25
[4/5] Copy and populate Documents.
    array-0.4.0.1 ...
    base-4.6.0.1 ................................*****************************
    bin-package-db-0.0.0.0 .
    binary-0.5.1.1 ..
    bytestring-0.10.0.2 .....****
    Cabal-1.16.0 ................*****************
    containers-0.5.0.0 ......******
    deepseq-1.3.0.1 .
    directory-1.2.0.1 .
    filepath-1.3.0.1 ..*
    ghc-prim-0.3.0.0 .....********
    hoopl-3.9.0.0 ....****
    hpc-0.6.0.0 ..
    integer-gmp-0.5.0.0 .
    old-locale-1.0.0.5 .
    old-time-1.1.0.1 .
    pretty-1.1.1.0 .*
    process-1.1.0.2 .
    template-haskell-2.8.0.0 .....*********
    time-1.4.0.1 ...*
    unix-2.6.0.1 ..........********
[5/5] Create index.

# add transformers, mtl using cabal conf file.
$ haddocset -t output add path/to/transformers-0.3.0.0-hash.conf path/to/mtl-2.1.2-hash.conf
    transformers-0.3.0.0 .....**
    mtl-2.1.2 .......*

# add void, attoparsec using html directory which contain *.haddock file.
$ haddocset -t output add path/to/void-0.6.1/html to/attoparsec-0.11.2.1/html 
    void-0.6.1 .
    attoparsec-0.11.2.1 ...**

# show list of packages.
$ haddocset -t output list
array-0.4.0.1
attoparsec-0.11.2.1
base-4.6.0.1
bin-package-db-0.0.0.0
binary-0.5.1.1
bytestring-0.10.0.2
Cabal-1.16.0
containers-0.5.0.0
deepseq-1.3.0.1
directory-1.2.0.1
filepath-1.3.0.1
ghc-prim-0.3.0.0
hoopl-3.9.0.0
hpc-0.6.0.0
integer-gmp-0.5.0.0
mtl-2.1.2
old-locale-1.0.0.5
old-time-1.1.0.1
pretty-1.1.1.0
process-1.1.0.2
template-haskell-2.8.0.0
time-1.4.0.1
transformers-0.3.0.0
unix-2.6.0.1
void-0.6.1

$ open output.docset
```

Use with stack
---

```
$ stack exec -- haddocset -t target.docset create
$ stack build --haddock
$ stack exec -- haddocset -t target.docset add $(stack path --snapshot-pkg-db)/*.conf
$ stack exec -- haddocset -t target.docset add $(stack path --local-pkg-db)/*.conf 
```
Thanks: [@mohanzhang - jfeltz/dash-haskell#14](../../../../jfeltz/dash-haskell/issues/14#issuecomment-139656811) and [@nrolland - #30](../../pull/30)
