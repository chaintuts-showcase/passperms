# This file contains a make script for the PassPerms application
#
# Author: Josh McIntyre
#

# This block defines makefile variables
CORE_FILES=src/*.f90

BUILD_DIR=bin
BUILD_BIN=passperms

CC=gfortran

# This rule builds the utility
build: $(SRC_FILES)
	mkdir -p $(BUILD_DIR)
	$(CC) $(FLAGS) -o $(BUILD_DIR)/$(BUILD_BIN) $(CORE_FILES)

# This rule cleans the build directory
clean: $(BUILD_DIR)
	rm $(BUILD_DIR)/* 
	rmdir $(BUILD_DIR)
