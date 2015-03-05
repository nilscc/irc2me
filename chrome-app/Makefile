#
# Targets
#

PAGES = connect main

TESTS = tests

#
# Compiler options
#

SRC_DIR   = src
BUILD_DIR = build

TSC 	  = tsc
TSC_FLAGS = -m amd --outDir $(BUILD_DIR) --sourceMap

#
# Main definition
#

.PHONY: all watch clean

all:
	@echo "Building files: $(SOURCES)"
	@$(TSC) $(TSC_FLAGS) $(SOURCES)

watch: | $(BUILD_DIR)
	@echo "Watching files: $(SOURCES)"
	@$(TSC) $(TSC_FLAGS) -w $(SOURCES)

clean:
	@echo "Cleaning up"
	@-rm -r $(BUILD_DIR)

# Build dependencies

SOURCES	= $(PAGES:%=$(SRC_DIR)/pages/%.ts) \
		  $(TESTS:%=$(SRC_DIR)/%.ts)
