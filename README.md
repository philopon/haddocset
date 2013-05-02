haddocset
=========

Generate docset of Dash from Haddock

usage/example
----

    $ haddocset --help
    Usage: haddocset [OPTIONS]
    OPTIONS:
      -f       --force        force
      -l       --link         use symbolic link
      -o FILE  --output=FILE  output file
      -i FILE  --input=FILE   input *.haddock file
      -h       --help         show this message

    # generate single document
    $ haddocset -o http.docset -i $prefix/share/doc/HTTP-4000.2.8/html/HTTP.haddock
    
    # generate multiple / symlinked document
    $ haddocset -l -o haskell.docset -i $prefix/share/doc/HTTP-4000.2.8/html/HTTP.haddock -i $prefix/share/doc/unix-2.6.0.0/html/unix.haddock
   
    # overwrite
    $ haddocset -f -o haskell.docset -i $prefix/share/doc/HTTP-4000.2.8/html/HTTP.haddock
