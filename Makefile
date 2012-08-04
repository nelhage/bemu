CXXFLAGS=-m32 -O2 -g -Wall -pthread $(if $(DEBUG),-DDEBUG=$(DEBUG))
ASFLAGS=-m32 -g
LDFLAGS=-m32 -pthread

SRCS=bemu.cpp bcpu.cpp bdecode.cpp bt.cpp bclock.cpp bconsole.cpp
ASMSRCS=bt_helper.S
OBJECTS=$(SRCS:.cpp=.o) $(ASMSRCS:.S=.o)
GEN_H=instructions.h
DEPFILES=$(SRCS:%.cpp=.%.d)

BEMU=bemu
UASM=uasm/uasm

TESTS=sancheck litmus bench1 bench2 bench3 bench4 supervisor align qsort timer trap jmptab
TESTS_BIN=$(TESTS:%=tests/%.bin)

all: $(BEMU) $(TESTS_BIN) $(DEPFILES)

$(BEMU): $(OBJECTS)
	$(CXX) -o $@ $(LDFLAGS) $(filter-out .config/%,$^)

$(UASM): CXXFLAGS += -w
$(UASM):
uasm: $(UASM)

$(OBJECTS): instructions.h .config/CXX .config/CPPFLAGS .config/CXXFLAGS
$(BEMU): .config/LDFLAGS
$(ASMSRCS:.S=.o): .config/ASFLAGS

instructions.h: insts.pl
	perl $< -cxx > $@

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

test: $(TESTS_BIN) $(UASM) $(BEMU)
	./run-tests.sh

TAGS: $(SRCS) $(ASMSRCS)
	etags $^

tags: TAGS

check-syntax:
	$(CC) $(CCFLAGS) -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

.phony: CLEAN tags check-syntax uasm

include Makefile.lib
-include $(DEPFILES)
