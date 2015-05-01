#
# Targets
#

TARGETS = background background/chrome-handler login

SOURCES	= $(TARGETS:%=$(SRC_DIR)/%.ts)

#
# Compiler options
#

SRC_DIR   = src
BUILD_DIR = build

TSC 	  = tsc
TSC_FLAGS = -m amd --noImplicitAny --outDir $(BUILD_DIR) --sourceMap

#
# Main definition
#

.PHONY: all watch clean

all: main

main:
	@echo "Building files: $(SOURCES)"
	@$(TSC) $(TSC_FLAGS) $(SOURCES)

watch:
	@echo "Watching files: $(SOURCES)"
	@$(TSC) $(TSC_FLAGS) -w $(SOURCES)

clean:
	@echo "Cleaning up"
	@-rm -r $(BUILD_DIR)
