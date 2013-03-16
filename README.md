# checker

Basic utility for computing and checking MD5SUMs on a directory of files. This is NOT production-ready code.

## Installation

If necessary, install the Haskell platform and update cabal:

    sudo apt-get install haskell-platform
    cabal update
    cabal install cabal-install

Then install:

    git clone git://github.com/carlohamalainen/checker.git
    cd checker
    cabal install

## Local usage

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

## Comparing against S3

Using [s3cmd](http://s3tools.org/s3cmd) one can synchronise a local directory to 
Amazon S3. Suppose that the local directory tmp has been synchronised to

    s3://my-s3-name/tmp

then the following operations operate on the S3 store.

Update the local cache, `$HOME/.s3_lar`, of all MD5sum checksums for the S3 object store. Note that this 
does a recursive listing of **every** object in your S3 store.

    checker --update-s3-cache

Show files that are fully stored in S3 and also the local directory:

    checker --show-in-both                    <s3 url> <local path>

Show files that are present locally but not in S3:

    checker --show-in-local-but-not-s3        <s3 url> <local path>

Show files that are in S3 but not present locally:

    checker --show-in-s3-but-not-local        <s3 url> <local path>

Compare checksums of local vs S3 files, and report on those that do not match:

    checker --check-checksums-local-vs-s3     <s3 url> <local path>
    
For example:

    checker --check-checksums-local-vs-s3     s3://my-s3-name/tmp /home/user/tmp
