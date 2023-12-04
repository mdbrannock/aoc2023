library(data.table)
library(here)
library(stringr)

# Part 1
dt <- fread(here('day03.txt'), header = F)

m <- as.matrix(dt)

# Get coordinates for every number and every symbol
numbers <- lapply(1:nrow(dt), function(.x) {
  r <- dt$V1[.x]
  
  numbers <- str_extract_all(r, '[0-9]+')[[1]]
  num_x_pos <- str_locate_all(r, '[0-9]+')[[1]]

  return(data.table(
    num = numbers,
    x_start = num_x_pos[, 'start'],
    x_end = num_x_pos[, 'end'],
    y_start = .x,
    y_end = .x
  ))
}) %>% rbindlist()

numbers <- numbers[!is.na(num)]

symbols <- lapply(1:nrow(dt), function(.x) {
  r <- dt$V1[.x]
  
  symbols <- str_extract_all(r, '[^0-9\\.]')[[1]]
  sym_x_pos <- str_locate_all(r, '[^0-9\\.]')[[1]]
  
  return(data.table(
    sym = symbols,
    x_start = sym_x_pos[, 'start'] - 1,
    x_end = sym_x_pos[, 'end'] + 1,
    sym_y_start = .x - 1,
    sym_y_end = .x + 1
  ))
}) %>% rbindlist()

# Drop missing rows
symbols <- symbols[!is.na(sym)]

setkey(symbols, x_start, x_end)

# Overlapping join
dt2 <- foverlaps(numbers, symbols, by.x = c('x_start', 'x_end'), by.y = c('x_start', 'x_end'))

# Filter to only when y_num is within range
dt3 <- dt2[(y_start >= sym_y_start) & (y_start <= sym_y_end)]

# RIGHT ANSWER (but I disagree)
sum(as.numeric(dt3$num))

# WRONG ANSWER (that I think should be right)
# Get unique numbers
dt4 <- unique(dt3[, .(num, i.x_start, y_start)])
sum(as.numeric(dt4$num))


# Part 2

# Filter to * symbols
star_symbols <- symbols[sym == '*']

# Overlapping join
dt5 <- foverlaps(numbers, star_symbols, by.x = c('x_start', 'x_end'), by.y = c('x_start', 'x_end'))

# Filter to only when y_num is within range
dt6 <- dt5[(y_start >= sym_y_start) & (y_start <= sym_y_end)]

# Count by symbol coordinate how many numbers match
dt7 <- dt6[, .(num, n = .N), by=.(x_start, sym_y_start)]

# Filter
dt8 <- dt7[n == 2]

# Get product by group
dt9 <- dt8[, .(p = prod(as.numeric(num))), by=.(x_start, sym_y_start)]
sum(dt9$p)
