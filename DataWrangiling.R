library(tidyverse)
nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nfl_passes.csv")
colnames(nfl_passes)
table(nfl_passes$passer_player_name)
names(nfl_passes)
nfl_passes |>
  arrange(play_id)

count(nfl_passes)
view(nfl_passes)
