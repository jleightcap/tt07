CLASH_SRC = $(wildcard src/*.hs)
GHC_FLAGS += -isrc -Wall
CLASH_FLAGS += $(GHC_FLAGS) \
	       -fclash-clear -fclash-error-extra -fclash-compile-ultra -fclash-aggressive-x-optimization

# -Wno-WIDTH: 			clash-generates some verilog that doesen't pass this lint.
# -Wno-MULTITOP:		just linting all sources; don't care about multiple top-level modules.
VERILATOR_FLAGS += -Wall -Wno-WIDTH -Wno-MULTITOP

.PHONY: all
TOP ?= Fractran
OUT = src/$(TOP).v
all: $(OUT)

$(OUT): src/$(TOP).hs $(CLASH_SRC)
	clash $(CLASH_FLAGS) $< --verilog
	sed '/timescale/d' verilog/$(TOP).topEntity/$(TOP).v > $@

.PHONY: lint
lint: $(OUT)
	verilator --lint-only $(VERILATOR_FLAGS) $(wildcard src/*.v)
	hlint $(CLASH_SRC)

.PHONY: test
test: test-clash test-verilog

.PHONY: test-clash
test-clash: test/Main.hs
	@# NOTE: `runghc` doesn't pull in some libs automatically, hack around for now
	clash $(GHC_FLAGS) $< -o run && ./run && rm ./run

.PHONY: test-verilog
test-verilog: $(OUT)
	$(MAKE) -C test/ && ! grep failure test/results.xml

.PHONY: clean
clean:
	$(MAKE) -C test/ clean
	rm -rf $(OUT) verilog/ src/*.hi src/*.o \
		test/__pycache__ test/results.xml runs/
