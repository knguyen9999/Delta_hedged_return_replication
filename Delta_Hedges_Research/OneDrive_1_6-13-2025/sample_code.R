library(arrow)
library(tidyverse)


# Specify the path to your .parquet file
file_path <- "path_to_your_file/example.parquet"

# Read the .parquet file using the arrow package
data <- read_parquet(file_path)

data <- open_dataset(file_path)  |> 
  filter(row_number() <= 1000) |> 
  collect()



# Example tidyverse manipulation: filter rows where a column 'value' > 10
filtered_data <- data_tibble |>
  filter(value > 10)

# Print the filtered data
print(filtered_data)