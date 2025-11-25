tricot_nir->read.csv("/Users/ca384/Documents/ChinedoziRepo/Dissertation-Experiment2-Analysis-for-PYT-Wet-chemistry-NIRS/Data/All_NIRS/2020_2021NIRS/tricot/Fresh_roots_2020_2021_tricot_nirs .csv")

data <- data.frame(matrix(1:15, ncol = 3)) # Example data with 3 columns

# Assign column names
col_names <- paste0("col", 1:ncol(data)) # Generate column names

# Assign column names to the data frame
colnames(data) <- col_names

# Print the data frame with column names
print(data)
                        