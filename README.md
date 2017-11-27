# "Ciphersaber" RC4 implementation
Copyright (c) 1997 Bart Massey

[This program is licensed under the "MIT License". Please
see the file `COPYING` in this distribution for license
terms.]

These are implementations of the "Ciphersaber" RC4 stream
cipher. See the file `ciphersaber.html` in this distribution
for the late-90s webpage describing Ciphersaber, or see
[the currently-available version](http://ciphersaber.gurus.org).
Maybe it's a little silly, but it's a lot of fun.

There are three implementations here:

* A C "reference" implementation I wrote about 1997
  is supplied for correctness purposes and as a performance
  baseline.  (I made some cosmetic changes to this code to
  get it to cleanly compile in 2012.) The code is kind of
  inelegant, but it works.

* I wrote a more interesting implementation in 2012 in
  Haskell, using Data.ByteString for the IO and an IOUArray
  for the RC4 state. It's kind of crufty, and was 14x slower
  than the C version in 2012, but it works correctly even on
  large files. (I fixed a bug in 2016, but it should not
  affect performance.)

* I did a third implementation in 2016 in Rust. It works,
  but various pieces still need to be pushed upstream before
  it is stable. I did some more work on it in late 2017.  It
  currently is (temporarily) UNIX-only.

* In 2016 I did a bunch of work/rework, adding "Ciphersaber 2"
  support (basically 20 repetitions of the key initialization
  to work around some known security issues) and support for
  reading the key from `/dev/tty` to all the implementations.

Enjoy.

    Bart Massey
    2017-11-27
