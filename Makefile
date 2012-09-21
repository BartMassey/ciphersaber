# Copyright © 2012 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

CC = gcc
CFLAGS = -Wall -O2
HC = ghc
HFLAGS = -Wall -O2 --make

all: ciphersaber-c ciphersaber-hs

ciphersaber-c: ciphersaber.c
	$(CC) $(CFLAGS) -o ciphersaber-c ciphersaber.c

ciphersaber-hs: ciphersaber.hs
	$(HC) $(HFLAGS) -o ciphersaber-hs ciphersaber.hs
