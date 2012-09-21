# "Ciphersaber" RC4 implementation
Copyright © 2012 Bart Massey

[This program is licensed under the "MIT License". Please
see the file `COPYING` in this distribution for license
terms.]

These are implementations of the "Ciphersaber" RC4 stream
cipher. See the file `ciphersaber.html` in this distribution
for the late-90s webpage describing Ciphersaber, or see [the
currently-available version](http://ciphersaber.gurus.org).
Maybe it's a little silly, but it's a lot of fun.

There are two implementations here. A C "reference"
implementation I wrote about 15 years ago is supplied for
correctness purposes and as a performance baseline.  (I made
some cosmetic changes to this code to get it to cleanly
compile in 2012.) The code is kind of inelegant, but it
works.

The more interesting implementation is in Haskell, using
Data.ByteString for the IO and an IOUArray for the RC4
state. It's kind of crufty, and currently 14x slower than
the C version, but it works correctly even on large files.

Enjoy.

    Bart Massey
    2012-09-21
