haddocset
============
Generate docset of Dash from Haddock

Usage/Example
============

```
% haddocset --help
Usage: haddocset [--hc-pkg CMD] [-t|--target DOCSET] [-q|--quiet] COMMAND

Available options:
  -h,--help                Show this help text
  --hc-pkg CMD             hc-pkg command (default: ghc-pkg)
  -t,--target DOCSET       output directory (default: haskell.docset)
  -q,--quiet               suppress output.

Available commands:
  create                   crate new docset.
  list                     list package of docset.
  add                      add package to docset.

% haddocset create # create docset using global database
[1/6] Create Directory.
[2/6] Writing plist.
[3/6] Migrate Database.
    Global package directory: /opt/ghc-7.6.3/lib/ghc-7.6.3/package.conf.d
    Global package count:     21
[4/6] Copy Documents.
..............................................................................................................................................................
[5/6] Populate database index.
.................................................................................................
[6/6] Create index.

% haddocset list # list up packages in docset
array-0.4.0.1
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
old-locale-1.0.0.5
old-time-1.1.0.1
pretty-1.1.1.0
process-1.1.0.2
template-haskell-2.8.0.0
time-1.4.0.1
unix-2.6.0.1

% haddocset add ./.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d/*.conf
/Users/philopon/mysrc/Haskell/haddocset/.cabal-sandbox/share/doc/x86_64-osx-ghc-7.6.3/primitive-0.5.2.1/html/primitive.haddock: openBinaryFile: does not exist (No such file or directory)
/Users/philopon/mysrc/Haskell/haddocset/.cabal-sandbox/share/doc/x86_64-osx-ghc-7.6.3/semigroups-0.12.2/html/semigroups.haddock: openBinaryFile: does not exist (No such file or directory)
/Users/philopon/mysrc/Haskell/haddocset/.cabal-sandbox/share/doc/x86_64-osx-ghc-7.6.3/vector-0.10.9.1/html/vector.haddock: openBinaryFile: does not exist (No such file or directory)
/Users/philopon/mysrc/Haskell/haddocset/.cabal-sandbox/share/doc/x86_64-osx-ghc-7.6.3/xhtml-3000.2.1/html/xhtml.haddock: openBinaryFile: does not exist (No such file or directory)

% haddocset list
array-0.4.0.1
attoparsec-0.11.2.1
base-4.6.0.1
base-unicode-symbols-0.2.2.4
bin-package-db-0.0.0.0
binary-0.5.1.1
blaze-builder-0.3.3.2
blaze-textual-0.2.0.8
bytestring-0.10.0.2
Cabal-1.16.0
conduit-1.0.15.1
containers-0.5.0.0
deepseq-1.3.0.1
direct-sqlite-2.3.11
directory-1.2.0.1
filepath-1.3.0.1
filesystem-conduit-1.0.0.1
ghc-paths-0.1.0.9
ghc-prim-0.3.0.0
haddock-2.13.2.1
hashable-1.2.1.0
hoopl-3.9.0.0
hpc-0.6.0.0
integer-gmp-0.5.0.0
lifted-base-0.2.2.0
mmorph-1.0.2
monad-control-0.3.2.3
mtl-2.1.2
nats-0.1.2
old-locale-1.0.0.5
old-time-1.1.0.1
optparse-applicative-0.7.0.2
pretty-1.1.1.0
process-1.1.0.2
resourcet-0.4.10
scientific-0.2.0.1
sqlite-simple-0.4.5.1
system-fileio-0.3.12
system-filepath-0.4.9
tagsoup-0.13.1
template-haskell-2.8.0.0
text-1.1.0.0
text-stream-decode-0.1.0.4
time-1.4.0.1
transformers-0.3.0.0
transformers-base-0.4.1
unix-2.6.0.1
unordered-containers-0.2.3.3
void-0.6.1

% open haskell.docset
```
