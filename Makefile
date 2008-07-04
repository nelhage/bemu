CFLAGS=-m32 -O2 -g -Wall
ASFLAGS=-m32
LDFLAGS=-m32

SRCS=bemu.c bcpu.c bdecode.c bt.c bclock.c bconsole.c
ASMSRCS=bt_helper.S
OBJECTS=$(SRCS:.c=.o) $(ASMSRCS:.S=.o)
GEN_H=opcodes.h
HEADERS=$(SRCS:.c=.h) $(GEN_H) x86.h

BEMU=bemu
UASM=uasm/uasm

TESTS=sancheck litmus bench1 bench2 bench3 bench4 supervisor align qsort timer
TESTS:=$(TESTS:%=tests/%.bin)

$(BEMU): LDFLAGS += -lrt
$(BEMU): $(OBJECTS)

$(UASM): CFLAGS += -w
$(UASM):
uasm: $(UASM)

opcodes.h: insts.pl
	perl insts.pl > opcodes.h

$(OBJECTS): $(HEADERS)

clean:
	rm -f $(OBJECTS) $(BEMU)
	rm -f tests/*.bin tests/*.map tests/*.sym
	rm -f $(UASM) uasm/uasm.o $(GEN_H)

%.bin: %.uasm $(UASM)
	$(UASM) $<

run-%: tests/%.bin
	./bemu $(BEMUOPTS) $<

run-os: BEMUOPTS += -o clock,tty
run-lab8: BEMUOPTS += -o clock,tty
run-timer: BEMUOPTS += -o clock

test: $(TESTS) $(UASM) $(BEMU)
	./run-tests.sh

TAGS: $(SRCS) $(ASMSRCS) $(HEADERS)
	etags $^

tags: TAGS

check-syntax:
	$(CC) $(CCFLAGS) -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

.phony: CLEAN tags check-syntax uasm
