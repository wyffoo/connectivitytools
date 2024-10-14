import csv

# Open the input and output CSV files
with open("topology.csv", "r") as in_file, open("tf.csv", "w", newline='') as out_file:
    # Create a CSV reader and writer
    reader = csv.reader(in_file, delimiter=';')
    writer = csv.writer(out_file)

    # Write the header row to the output file
    writer.writerow(["PNAME", "DVALUE"])

    # Iterate through the rows in the input file
    for row in reader:
        # Write only the desired fields to the output file
        writer.writerow([row[9], row[3]])
