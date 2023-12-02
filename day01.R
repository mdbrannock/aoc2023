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

# Find instances of spelled out digits
digit_key = c(
  'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'
)

# For each digit, find first and last instance
for(d in 1:9) {
  dig_loc = str_locate_all(dt$V1, paste0(d, '|', digit_key[d]))
  dig_loc = lapply(dig_loc, \(x) x[, 'start'])
  set(dt, j=paste0(digit_key[d], '_f'), value=sapply(dig_loc, min))
  set(dt, j=paste0(digit_key[d], '_l'), value=sapply(dig_loc, max))
}

# Get which number has minimum first instance and max last instance
row.fun <- function(..., fun=which.min) {
  x <- do.call(cbind, list(...))
  m <- apply(x, 1, fun)
  return(m)
}
dt[, first := row.fun(one_f, two_f, three_f, four_f, five_f, six_f, seven_f, eight_f, nine_f, fun=which.min)]
dt[, last  := row.fun(one_l, two_l, three_l, four_l, five_l, six_l, seven_l, eight_l, nine_l, fun=which.max)]

# Combine
dt[, together := as.numeric(paste0(first, last))]

# Sum
sum(dt$together)
