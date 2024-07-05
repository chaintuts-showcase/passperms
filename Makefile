# This file contains a make script for the PassPerms application
#
# Author: Josh McIntyre
#

# This block defines makefile variables
CORE_FILES=src/passperms.f90
CORE_MOD_FILES=src/passpermslib.f90
PY_FILES=src/*.py

TEST_PY_FILES=tests/*.py
TEST_FILES=tests/*.f90
SRC_OBJ=*.o

BUILD_DIR=bin
BUILD_BIN=passperms
BUILD_TEST_BIN=test_passperms

CC=gfortran

# This rule builds the utility
build: $(CORE_FILES) $(PY_FILES)
	mkdir -p $(BUILD_DIR)
	$(CC) -o $(BUILD_DIR)/$(BUILD_BIN) $(CORE_MOD_FILES) $(CORE_FILES)
	rm *.mod
	cp $(PY_FILES) $(BUILD_DIR)

# This rule builds the utility
tests: $(TEST_FILES) $(TEST_PY_FILES)
	mkdir -p $(BUILD_DIR)
	$(CC) -o $(BUILD_DIR)/$(BUILD_TEST_BIN) $(CORE_MOD_FILES) $(TEST_FILES)
	rm *.mod
	cp $(TEST_PY_FILES) $(BUILD_DIR)

# This rule cleans the build directory
clean: $(BUILD_DIR)
	rm -r $(BUILD_DIR)/*
	rm -r -f $(BUILD_DIR)/.pytest_cache
	rmdir $(BUILD_DIR)
