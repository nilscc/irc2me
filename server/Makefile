BIN_SERVER = irc2mobile-server
BIN_TESTS  = backend-tests

BINARIES = $(BIN_SERVER) $(BIN_TESTS)

SRC_DIR = src

HS_SRCS = $(shell find $(SRC_DIR) -type f -name '*.hs')

.PHONY: all clean run

all: $(BINARIES)

clean:
	@cabal clean
	@-rm $(BINARIES)

run: $(BINARIES)
	./$(BIN_SERVER)

$(BINARIES): $(HS_SRCS)
	@cabal install --bindir=.
