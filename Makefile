# This file contains a make script for the PassPerms application
#
# Author: Josh McIntyre
#

# This block defines makefile variables
CORE_FILES=src/*.f90
PY_FILES=src/*.py

BUILD_DIR=bin
BUILD_BIN=passperms

CC=gfortran

# This rule builds the utility
build: $(CORE_FILES) $(PY_FILES)
	mkdir -p $(BUILD_DIR)
	$(CC) $(FLAGS) -o $(BUILD_DIR)/$(BUILD_BIN) $(CORE_FILES)
	cp $(PY_FILES) $(BUILD_DIR)

# This rule cleans the build directory
clean: $(BUILD_DIR)
	rm $(BUILD_DIR)/* 
	rmdir $(BUILD_DIR)
