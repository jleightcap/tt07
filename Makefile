TOP = Fractran
ALL = src/$(TOP).v

CLASH_SRC = $(wildcard src/*.hs)
GHC_FLAGS += -isrc -Wall
CLASH_FLAGS += $(GHC_FLAGS) \
	       -fclash-clear -fclash-error-extra -fclash-compile-ultra -fclash-aggressive-x-optimization

# -Wno-WIDTH: 			clash-generates some verilog that doesen't pass this lint.
# -Wno-MULTITOP:		just linting all sources; don't care about multiple top-level modules.
VERILATOR_FLAGS += -Wall -Wno-WIDTH -Wno-MULTITOP

.PHONY: all
all: $(ALL)

$(ALL): $(CLASH_SRC)
	clash $(CLASH_FLAGS) src/$(TOP).hs --verilog
	sed '/timescale/d' verilog/$(TOP).topEntity/$(TOP).v > $@

.PHONY: lint
lint: $(TOP)
	verilator --lint-only $(VERILATOR_FLAGS) $(wildcard src/*.v)
	hlint $(CLASH_SRC)

.PHONY: test
test: $(TOP)
	runghc $(GHC_FLAGS) test/Test.hs
	$(MAKE) -C test/ && ! grep failure test/results.xml

.PHONY: clean
clean:
	$(MAKE) -C test/ clean
	rm -rf $(TOP) verilog/ src/*.hi src/*.o \
		test/__pycache__ test/results.xml runs/
