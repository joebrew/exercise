library(dplyr)
source('global.R')

# Keep only bulk
bulk <- df %>% filter(bulk == 'yes')
