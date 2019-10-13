SRC_DIR = graveyard
DEP_PACKAGES = memoize table-panel minikanren
BYTECODE_DIR = $(SRC_DIR)/compiled
GUI_PATH = $(SRC_DIR)/gui.rkt
BACKEND_PATH = $(SRC_DIR)/graveyard.rkt
SRC_FILES  = $(GUI_PATH) $(SRC_PATH)
BUILD_PATH = build
BUILD_BIN_PATH = $(BUILD_PATH)/bin
EXECUTABLE_PATH = $(BUILD_BIN_PATH)/graveyard
OSX_EXECUTABLE_PATH = $(EXECUTABLE_PATH).app
WINDOWS_EXECUTABLE_PATH = $(EXECUTABLE_PATH).exe
TARGET_DIR = $(BUILD_PATH)/target



run:
	racket $(GUI_SRC)

deps:
	raco pkg install $(DEP_PACKAGES)

clean:
	rm -rf $(BYTECODE_DIR)
	rm -rf $(BUILD_PATH)

compile_gui: $(SRC_FILES)
	raco make $(GUI_PATH)


executable: compile_gui
	mkdir -p $(BUILD_BIN_PATH)
	raco exe --gui -o $(EXECUTABLE_PATH) $(GUI_PATH)

build_osx: executable
	raco distribute $(TARGET_DIR) $(OSX_EXECUTABLE_PATH)

build_unix: executable
	raco distribute $(TARGET_DIR) $(EXECUTABLE_PATH)

build_windows: executable
	raco distribute $(TARGET_DIR) $(WINDOWS_EXECUTABLE_PATH)

.PHONY:  run deps clean
