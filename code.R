library(gsheet)
library(tidyverse)
url <- 'https://docs.google.com/spreadsheets/d/1VSClXiF0ojKDIrTlHjPiYS9u8IVx1BnexgS3g6O2REY/edit?usp=sharing' 
df <- gsheet::gsheet2tbl(url = url)
