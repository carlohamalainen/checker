checker
=======

Basic utility for computing and checking MD5SUMs on a directory of files. This is NOT production-ready code.

If necessary, update cabal:

    cabal update
    cabal install cabal-install

Then install:

    git clone git://github.com/carlohamalainen/checker.git
    cd checker
    cabal install

Compute missing checksums:

    checker --computemissing <dir>

Check existing checksums, reporting on any differences or missing checksums:

    checker --checkall <dir>

All this utility does is create a directory in the top level called .md5sums, and
then a duplicate directory structure is created with md5sums of the existing files. For example:

    $ find tmp
    tmp
    tmp/foo
    tmp/.md5sums
    tmp/.md5sums/foo.md5sum
    tmp/.md5sums/bar
    tmp/.md5sums/bar/b.md5sum
    tmp/.md5sums/bar/a.md5sum
    tmp/bar
    tmp/bar/b
    tmp/bar/a

