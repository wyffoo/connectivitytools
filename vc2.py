import csv

# Define the input and output files
input_file = "data2.csv"
output_file = "output2.sql"

# Open the CSV file
with open(input_file, 'r') as csv_file:
    # Create a CSV reader
    csv_reader = csv.DictReader(csv_file, delimiter=';')

    # Open the SQL file
    with open(output_file, 'w') as sql_file:
        # For each row in the CSV file
        for row in csv_reader:
            # Write an INSERT statement to the SQL file
            sql_file.write(f"INSERT INTO giga.variables (name, description, value, unit, min_val, max_val) "
                           f"VALUES ('{row['PNAME']}', '{row['PDESC']}', {row['DVALUE']}, "
                           f"'{row['UNIT']}', {row['RSTART']}, {row['REND']});\n")
