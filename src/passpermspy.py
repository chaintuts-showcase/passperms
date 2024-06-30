# This file contains a tool for calculating brute-force password cracking times
#
# Author: Josh McIntyre
#
import argparse

# Define module level constants

# Numerical (10), lowercase, upper lower, upper lower numerical (62), various amounts of symbols
COMPLEXITIES = [ 10, 26, 52, 62, 72, 82, 92 ]
LENGTH_SAMPLE = 8

# Lengths of the password in characters
LENGTHS = [ 8, 9, 10, 12, 14, 16, 20 ]
COMPLEXITY_SAMPLE = 62

# Password cracking constants
# 180 billion (giga) hashes per second - professional 25 GPU cluster, MD5 algorithm
# 7.652 billion (giga) hashes per second - consumer laptop with Nvidia GeForce 1050 Ti GPU
HASHPOWER_PRO = 180000000000.0
HASHPOWER_CONSUMER = 7652000000.0

SECONDS_PER_MINUTE = 60.0
SECONDS_PER_HOUR = 3600.0
SECONDS_PER_DAY = 86400.0
SECONDS_PER_YEAR = 31557600.0

VAR_LENGTH = "length"
VAR_COMPLEXITY = "complexity"
SPEED_FAST = "fast"
SPEED_SLOW = "slow"

# Formatting constants
DELIM_SHORT = "\t"
DELIM = "\t\t"
PRO_CRACK_HEADER = "Cracking w/ 180 GH/s, MD5 - 25 GPU professional cluster"
CONSUMER_CRACK_HEADER = "Cracking w/ 7.652 GH/s, MD5 - NVidia GeForce 1050 Ti consumer laptop"
ROW_HEADER_LENGTH = f"Length{DELIM}Combinations{DELIM}Time"
ROW_HEADER_COMPLEXITY = f"Complexity{DELIM_SHORT}Combinations{DELIM}Time"

SECONDS_LABEL = "seconds"
MINUTES_LABEL = "minutes"
HOURS_LABEL = "hours"
DAYS_LABEL = "days"
YEARS_LABEL = "years"
YEARS_SCIENTIFIC_SWITCH = 10000 # Switch to scientific notation if greater than N years

# Get permutations based on complexities and a given length
# Returns a list of total password permutations
def compute_complexities(length):

    results = [ complexity ** length for complexity in COMPLEXITIES ]
    return results
    
# Get permutations based on lengths for a given complexity
# Returns a list of total password permutations
def compute_lengths(complexity):

    results = [ complexity ** length for length in LENGTHS ]
    return results
    
# Compute crack times for a given a permutations list and hashes per second values
# Returns a list of total seconds to exhaust all possible passwords
def compute_cracktimes(perms, hashes_sec):

    results = [ perm / hashes_sec for perm in perms ]
    return results

# Format and output the results in a simple table format
def output_results(perms, calc_times, variable, speed):

    if variable == VAR_LENGTH:
        comb_header = f"Combinations for constant complexity {COMPLEXITY_SAMPLE}, variable lengths"
        row_header = ROW_HEADER_LENGTH
        bases = LENGTHS
    else:
        comb_header = f"Combinations for constant length {LENGTH_SAMPLE}, variable complexity"
        row_header = ROW_HEADER_COMPLEXITY
        bases = COMPLEXITIES

    if speed == SPEED_FAST:
        crack_header = PRO_CRACK_HEADER
    else:
        crack_header = CONSUMER_CRACK_HEADER

    rows = []
    for i in range(len(perms)):
    
        combs = perms[i]
        crack_time = calc_times[i]
        base = bases[i]

        if crack_time <= SECONDS_PER_MINUTE:
            crack_time_scaled = crack_time
            units = SECONDS_LABEL
        elif crack_time <= SECONDS_PER_HOUR:
            crack_time_scaled = crack_time / SECONDS_PER_MINUTE
            units = MINUTES_LABEL
        elif crack_time <= SECONDS_PER_DAY:
            crack_time_scaled = crack_time / SECONDS_PER_HOUR
            units = HOURS_LABEL
        elif crack_time <= SECONDS_PER_YEAR:
            crack_time_scaled = crack_time / SECONDS_PER_DAY
            units = DAYS_LABEL
        else:
            crack_time_scaled = crack_time / SECONDS_PER_YEAR
            units = YEARS_LABEL

        crack_time_str = f"{crack_time_scaled:.2e}" if (units == YEARS_LABEL and crack_time_scaled >= YEARS_SCIENTIFIC_SWITCH) else f"{crack_time_scaled:.5f}"
        row = f"{base}{DELIM}{combs:.2e}{DELIM}{crack_time_str} {units}"
        rows.append(row)

    print(comb_header)
    print(crack_header)
    print(row_header)
    for row in rows:
        print(row)
    
# The main entry point for the program
def main():

    # Fetch the directory from the command line args
    parser = argparse.ArgumentParser(description="Calculate password permutations and cracking times")
    parser.add_argument("--complexities", action="store_true", help="Calculate permutations for variable complexities")
    parser.add_argument("--lengths", action="store_true", help="Calculate permutations for variable lengths")
    parser.add_argument("--fast", action="store_true", help="Compute crack times for a professional cluster, 180 GH/s")
    parser.add_argument("--regular", action="store_true", help="Compute crack times for a consumer laptop, 7.625 GH/s")

    args = parser.parse_args()

    # Compute password permutations and crack times
    if args.complexities:
        variable = VAR_COMPLEXITY
        perms = compute_complexities(LENGTH_SAMPLE)
    else:
        variable = VAR_LENGTH
        perms = compute_lengths(COMPLEXITY_SAMPLE)
        
    if args.fast:
        speed = SPEED_FAST
    else:
        speed = SPEED_SLOW
    
    crack_times = compute_cracktimes(perms, HASHPOWER_PRO if speed == SPEED_FAST else HASHPOWER_CONSUMER)

    output_results(perms, crack_times, variable, speed)
    
if __name__ == "__main__":
    main()