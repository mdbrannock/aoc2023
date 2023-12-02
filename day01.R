library(data.table)
library(here)
library(stringr)

# Part 1
dt <- fread(here('day01.txt'), header = F)

# Get numeric digits
dt[, digits := str_remove_all(V1, '[a-z]')]

# Get first and last
dt[, first := substr(digits, 1, 1)]
dt[, last := substr(digits, nchar(digits), nchar(digits))]

# Combine
dt[, together := as.numeric(paste0(first, last))]

# Sum
sum(dt$together)


# Part 2

# replace spelled out digits with numeric ones
digit_key = c(
  'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'
)
dt[, one := str_locate_all]
dt[, V2 := str_locate_all(V1, c(
  'one' = '1',
  'two' = '2',
  'three' = '3',
  'four' = '4',
  'five' = '5',
  'six' = '6',
  'seven' = '7',
  'eight' = '8',
  'nine' = '9'
))]

# Get numeric digits
dt[, digits := str_remove_all(V2, '[a-z]')]

# Get first and last
dt[, first := substr(digits, 1, 1)]
dt[, last := substr(digits, nchar(digits), nchar(digits))]

# Combine
dt[, together := as.numeric(paste0(first, last))]

# Sum
sum(dt$together)

# Wrong
52769
