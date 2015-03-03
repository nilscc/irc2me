#
# Targets
#

PAGES		= background

CLASSES 	= ChromeMessage \
			  ProtoStream \
			  Logger \
			  UIState \
			  Irc2me

DEFINITIONS = dcodeIO/ProtoBuf dcodeIO/Long dcodeIO/ByteBuffer \
			  chrome

MESSAGES	= proto/messages

#
# Compiler options
#

SRC_DIR   = src
BUILD_DIR = build

TSC 	  = tsc
TSC_FLAGS = -m amd --outDir $(BUILD_DIR)

#
# Main definition
#

.PHONY: all clean

all: $(PAGES:%=$(BUILD_DIR)/pages/%.js)

clean:
	@echo "Cleaning up"
	@-rm -r $(BUILD_DIR)

$(BUILD_DIR):
	@echo "Creating $(BUILD_DIR)"
	@mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.js: $(SRC_DIR)/%.ts | $(BUILD_DIR)
	@echo "Building $< to $@"
	@$(TSC) $(TSC_FLAGS) $<

# Build dependencies

TARGETS	= $(PAGES:%=pages/%) $(CLASSES) $(MESSAGES)
SOURCES = $(TARGETS:%=$(SRC_DIR)/%.ts)
BUILDS	= $(SOURCES:$(SRC_DIR)/%.ts=$(BUILD_DIR)/%.js)

$(BUILDS): $(SOURCES) $(DEFINITIONS:%=$(SRC_DIR)/libs/%.d.ts)
