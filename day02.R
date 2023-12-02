library(data.table)
library(here)
library(stringr)

# Part 1
dt <- fread(here('day02.txt'), header = F, sep=':')

# Split games
games <- strsplit(dt$V2, '; ')

# Get counts of red, green, blue for each draw for each game
dt2 <- lapply(1:length(games), \(g) {
  game <- games[[g]]
  
  lapply(game, \(d) {

    green <- as.numeric(str_extract(d, '[0-9]+(?= green)'))
    red <- as.numeric(str_extract(d, '[0-9]+(?= red)'))
    blue <- as.numeric(str_extract(d, '[0-9]+(?= blue)'))
    
    return(data.table(id=g, green=green, red=red, blue=blue))
    
  }) %>% rbindlist()
}) %>% rbindlist()

# Fill NA with 0
setnafill(dt2, fill=0)

# Get max of each color for each game
dt3 <- dt2[, .(green = max(green), blue = max(blue), red = max(red)), by=id]

# Keep only games with no more than 12 reds, 13 greens, or 14 blues
dt4 <- dt3[(green <= 13) & (blue <= 14) & (red <= 12)]
sum(dt4$id)

# Part 2
# Get product or max for each color
dt3[, power := green * blue * red]
sum(dt3$power)
