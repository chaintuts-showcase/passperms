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
* Requires Python for Python version

### Platforms
* Windows
* Linux
* MacOSX

## Usage
____________

### CLI Usage
* Run `passperms.exe -c/--complexities` to show variable complexity table
* Run `passperms.exe -l/--lengths` to show variable lengths table
* Run with `--fast` to show pro password cracking cluster crack times
* Run with `--regular` to show consumer laptop crack times

### Python CLI Usage
* Run `python3 passpermspy.py --complexities` to show variable complexity table
* Run `python3 passpermspy.py --lengths` to show variable length table
* Run with `--fast` to show pro password cracking cluster crack times
* Run with `--regular` to show consumer laptop crack times