## General
____________

### Author
* Josh McIntyre

### Website
* jmcintyre.net

### Overview
* PassPerms demonstrates the relative security of password length over complexity

## Development
________________

### Git Workflow
* master for releases (merge development)
* development for bugfixes and new features

### Building
* make build
Build the utility
* make clean
Clean the build directory

### Features
* Calculate a table of total password permutations for a constant length and variable complexity
* Calculate a table of total password permutations for a constant complexity and variable lengths
* Output formatted tables

### Requirements
* Requires a terminal emulator

### Platforms
* Windows
* Linux
* MacOSX

## Usage
____________

### CLI Usage
* Run `passperms.exe -c/--complexities` to show variable complexity table
* Run `passperms.exe -l/--lengths` to show variable lengths table