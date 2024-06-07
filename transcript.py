def modify_csv(input_file, output_file):
    with open(input_file, 'r') as infile:
        lines = infile.readlines()
    
    modified_lines = []
    for i in range(0, len(lines), 2):  # Iterate over every alternate line
        time_stamp = lines[i].strip()  # Remove leading/trailing whitespace
        comment = lines[i+1].strip()  # Remove leading/trailing whitespace
        modified_lines.append(f"{time_stamp},{comment}")  # Combine time stamp and comment with a comma
    
    with open(output_file, 'w') as outfile:
        outfile.write('\n'.join(modified_lines))  # Write the modified lines

input_file = 'transcript.csv'
output_file = 'transcript_clean.csv'

modify_csv(input_file, output_file)
