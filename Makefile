CFLAGS=-m32 -O2 -g -Wall
ASFLAGS=-m32
LDFLAGS=-m32

SRCS=bemu.c bcpu.c bdecode.c bt.c bclock.c bconsole.c
ASMSRCS=bt_helper.S
OBJECTS=$(SRCS:.c=.o) $(ASMSRCS:.S=.o)
HEADERS=$(SRCS:.c=.h)

BEMU=bemu
UASM=uasm/uasm

TESTS=sancheck litmus bench1 bench2 bench3 bench4 supervisor align qsort timer
TESTS:=$(TESTS:%=tests/%.bin)

$(BEMU): LDFLAGS += -lrt
$(BEMU): $(OBJECTS)

$(UASM): CFLAGS += -w

$(OBJECTS): $(HEADERS)

clean:
	rm -f $(OBJECTS) $(BEMU) tests/*.bin tests/*.map tests/*.sym $(UASM) uasm/uasm.o

%.bin: %.uasm $(UASM)
	$(UASM) $<

test: $(TESTS) $(UASM) $(BEMU)
	./run-tests.sh

TAGS: $(SRCS) $(ASMSRCS) $(HEADERS)
	etags $^

tags: TAGS

check-syntax:
	$(CC) $(CCFLAGS) -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

.phony: CLEAN tags check-syntax
