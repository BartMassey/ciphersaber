# Copyright © 2012 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

CC = gcc
CFLAGS = -Wall -O2
HC = ghc
#HFLAGS = -Wall -O2 --make
HFLAGS = -Wall -O2 -prof -fprof-auto --make

all: ciphersaber-c ciphersaber-hs

ciphersaber-c: ciphersaber.c
	$(CC) $(CFLAGS) -o ciphersaber-c ciphersaber.c

ciphersaber-hs: ciphersaber.hs
	$(HC) $(HFLAGS) -o ciphersaber-hs ciphersaber.hs

test: all
	cp /usr/share/dict/words /tmp/1.txt
	sh ./bigfile.sh 10 /tmp/1.txt >/tmp/10.txt
	sh ./bigfile.sh 10 /tmp/10.txt >/tmp/100.txt
	for i in 1 10 100; do \
	  echo "$$i:"; \
	  time ./ciphersaber-c -e hello </tmp/$$i.txt >/dev/null ; \
	  time ./ciphersaber-hs -e hello </tmp/$$i.txt >/dev/null ; \
	done
