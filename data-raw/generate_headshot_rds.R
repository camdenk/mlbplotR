
library(tidyverse)

headshots <- read_csv("./data-raw/Player-IDs.csv")

saveRDS(headshots, "./data-raw/Player-IDs.rds")
