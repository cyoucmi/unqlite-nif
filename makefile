FLAGS = -g 
##-O6
##-std=c99

## TODO:
ERL_ROOT = /usr/local/lib/erlang/erts-5.10.4

ECC = erlc

OUTDIR = ./
RCS = $(wildcard *.erl)
OBJS = $(patsubst %.erl,$(OUTDIR)/%.beam,$(RCS))

all:unqlite_nif.so $(OBJS) test

unqlite_nif.so:unqlite/unqlite.c unqlite/unqlite.h unqlite_nif.c
	gcc -o $@ $^ --shared -fpic $(FLAGS) -Wall -I $(ERL_ROOT)/emulator/beam -I $(ERL_ROOT)/include
	
%.beam:%.erl
	$(ECC) $^

clean: 
	rm  unqlite_nif.so *.beam
