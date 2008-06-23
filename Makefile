CFLAGS=-g -DDEBUG
OBJS=bcpu.o bdecode.o bemu.o
HEADERS=bcpu.h bdecode.h bemu.h x86.h opcodes.h

bcpu.o: $(HEADERS)
bdecode.o: $(HEADERS)
x86-test.o: $(HEADERS)

opcodes.h: insts.pl
	perl $< > $@

bemu: $(OBJS)
x86-test: x86-test.o

tags:
	etags *.[ch]

.PHONY: tags
