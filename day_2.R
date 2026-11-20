# Advent of Code 2025



# Day 2 - Part 1 ---------------------------


day2_input <- readLines("day2_input.txt") 


# Split into elements and convert to tibble
input <- day2_input |> strsplit(split = ",") |> 
                       unlist() 

total <- 0


for (i in seq_along(input)) {

  str_range <- strsplit(input[i], split = "-") |> unlist()
  num_range <- as.numeric(str_range)
  
  num_seq <- seq(num_range[1], num_range[2])
  str_seq <- as.character(num_seq)
  
  str_lengths <- nchar(str_seq)
  
  # Get only the elements made up of repeated sequences of digits
  equal_halves <- ifelse(str_lengths %% 2 == 0, 
                         substr(str_seq, 1, nchar(str_seq) / 2) == substr(str_seq, 1 + (nchar(str_seq) / 2), nchar(str_seq)), 
                         FALSE)
  
  # Sum invalid IDs, ignoring ones with leading zeroes
  total <- total + sum(num_seq[equal_halves & substr(str_seq, 1, 1) != "0"])
  
}




# Day 2 - Part 2 ----------------------------------------------------------


# This time, any sequence of digits repeated >= 2 times is an invalid ID. 

total2 <- 0

for (j in seq_along(input)) {

  str_range <- strsplit(input[j], split = "-") |> unlist()
  num_range <- as.numeric(str_range)
  
  num_seq <- seq(num_range[1], num_range[2])
  str_seq <- as.character(num_seq)
  
  matches <- grepl("^(\\d+)\\1+$", str_seq)
  
  total2 <- total2 + sum(num_seq[matches])
  
}


