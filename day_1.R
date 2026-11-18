library(tidyverse)


# Day 1 - Part 1 -----------------------------------------------------------


 
day1_input <- readLines("day1_input.txt")

# Convert raw input to integers.  Pos = "R", Neg = "L"
input_num <- ifelse(substr(day1_input, 1, 1) == "R", 
                    as.numeric(substr(day1_input, 2, nchar(day1_input))), 
                    -as.numeric(substr(day1_input, 2, nchar(day1_input))))

# Some rotations are large. For multiple rotations, get only the nett movement

input_mod <- input_num %% 100

# Set result to be initially 50
pt1_prev_pos <- 50

# Counter for number of times pointer lands on zero
pt1_total <- 0


for (i in seq_along(input_mod)) {

  pt1_curr_pos <- pt1_prev_pos + input_mod[i]

  if (pt1_curr_pos > 99) {

    pt1_curr_pos <- pt1_curr_pos - 100

  } else if (pt1_curr_pos < 0) {

    pt1_curr_pos <- pt1_curr_pos + 100

  }

  # Increments total if position is zero
  if (pt1_prev_pos == 0) {
    
    pt1_total <- pt1_total + 1
    
  }
  
  pt1_prev_pos <- pt1_curr_pos

}



# Day 1 - Part 2 ----------------------------------------------------------

# This time whenever the dial points at zero after a rotation or each time it passes zero 
# during a rotation, the total increments

get_zero_passes <- function(x) {
  
  prev_position <- 50   # starting position
  num_zero_passes <- 0  # counter for result
  # output <- list()
  
  
  for (i in seq_along(x)) {
    
    this_rotation <- x[i]
    
    unadjusted_position <- prev_position + this_rotation
    
    current_position <- unadjusted_position %% 100  # what the dial now reads
    
    
    if (unadjusted_position > 99) {
      
      num_rotations <- unadjusted_position %/% 100
      
    } else if (unadjusted_position <= 0) {
      
      if (prev_position == 0) {
        
        num_rotations <- (unadjusted_position %/% -100)  # if dial starts at zero, only count subsequent passes
        
      } else {
        
        num_rotations <- (unadjusted_position %/% -100) + 1 
        
      }
      
    } else {
      
      num_rotations <- 0
      
    }
    
    
    num_zero_passes <- num_zero_passes + num_rotations  # update result
    
    
    # output[[i]] <- list(prev_position = prev_position,
    #                     this_rotation = this_rotation,
    #                     unadjusted_position = unadjusted_position,
    #                     current_position = current_position,
    #                     num_rotations = num_rotations,
    #                     num_zero_passes = num_zero_passes)
    
    
    prev_position <- current_position  # prepares for next iteration
    
    
  }
  
  # return(output)
  
  return(num_zero_passes)
  
}



get_zero_passes(input_num)



# Make some test data

# The dial starts by pointing at 50.
# The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
# The dial is rotated L30 to point at 52.
# The dial is rotated R48 to point at 0.
# The dial is rotated L5 to point at 95.
# The dial is rotated R60 to point at 55; during this rotation, it points at 0 once.
# The dial is rotated L55 to point at 0.
# The dial is rotated L1 to point at 99.
# The dial is rotated L99 to point at 0.
# The dial is rotated R14 to point at 14.
# The dial is rotated L82 to point at 32; during this rotation, it points at 0 once.

example_data <- c(-68, -30, 48, -5, 60, -55, -1, -99, 14, -82)

example_output <- get_zero_passes(example_data)












