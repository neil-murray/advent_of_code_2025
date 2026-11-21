# Advent of Code 2025 


# Day 3 - Part 1 ----------------------------------------------------------

input <- readLines("day3_input.txt") |> strsplit(split = "") |> 
                                        lapply(as.numeric)

total_joltage <- 0

for (i in seq_along(input)) {
  
  bank <- input[[i]]
  bank_length <- length(bank)
  
  max_first_val <- which.max(bank[1:(bank_length - 1)])
  max_second_val <- max_first_val + which.max(bank[(max_first_val + 1):bank_length])
  
  joltage <- bank[max_first_val] * 10 + bank[max_second_val]
  
  total_joltage <- total_joltage + joltage
  
}





# Day 3 - Part 2 ----------------------------------------------------------



