CFLAGS=-g -DDEBUG
OBJS=bcpu.o bdecode.o bemu.o
HEADERS=bcpu.h bdecode.h bemu.h

bcpu.o: $(HEADERS)
bdecode.o: $(HEADERS)

bemu: $(OBJS)
