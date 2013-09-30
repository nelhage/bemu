ARCH=$(shell uname -m)
ifeq ($(ARCH),x86_64)
BITS=64
else ifeq ($(ARCH),i686)
BITS=32
else
$(error Unsupported architecture $(ARCH))
endif

CXXFLAGS=-m$(BITS) -O2 -g -Wall -pthread $(if $(DEBUG),-DDEBUG=$(DEBUG)) -DHOST_BITS=$(BITS)
ASFLAGS=-m$(BITS) -g
LDFLAGS=-m$(BITS) -pthread

MAKEVARS:=.makevars

BEMU := bemu
UASM := uasm/uasm

TESTS=sancheck litmus bench1 bench2 bench3 bench4 supervisor align qsort timer trap trap2 jmptab
TESTS_BIN=$(TESTS:%=tests/%.bin)

$(BEMU): $(DEPFILES)

instructions.h: insts.pl
	perl $< -cxx > $@

EXTRA_CLEAN := tests/*.bin tests/*.map tests/*.sym $(GEN_H)

%.bin: %.uasm $(UASM)
	$(UASM) $<

run-%: tests/%.bin
	./bemu $(BEMUOPTS) $<

run-os: BEMUOPTS += -o clock,tty
run-lab8: BEMUOPTS += -o clock,tty
run-timer: BEMUOPTS += -o clock

test: $(TESTS_BIN) $(UASM) $(BEMU)
	./run-tests.sh

TAGS: $(SRCS) $(ASMSRCS)
	etags $^

tags: TAGS

check-syntax:
	$(CC) $(CCFLAGS) -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

.phony: CLEAN tags check-syntax uasm

DIRS := . uasm

include Makefile.lib
-include $(DEPFILES)
