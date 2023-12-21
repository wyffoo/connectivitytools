import csv

# Define the input and output files
input_file = "data.csv"
output_file = "output.sql"

# Open the CSV file
with open(input_file, 'r') as csv_file:
    # Create a CSV reader
    csv_reader = csv.DictReader(csv_file, delimiter=';')

    # Open the SQL file
    with open(output_file, 'w') as sql_file:
        # For each row in the CSV file
        for row in csv_reader:
            # Write an INSERT statement to the SQL file
            sql_file.write(f"INSERT INTO giga.variables (name, description, value) "
                           f"VALUES ('{row['PARAM']}', '{row['DESCRIPTION']}', {row['VALUE']});\n")
