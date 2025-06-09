library(tidyverse)
library(kableExtra)



nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nfl_passes.csv")
colnames(nfl_passes)
table(nfl_passes$passer_player_name)
names(nfl_passes)
nfl_passes |>
  arrange(play_id)

count(nfl_passes)
view(nfl_passes)


names(nfl_passes)
summary(nfl_passes)


nfl2 <- nfl_passes |>
  select(passer_player_name, complete_pass) |>
  group_by(passer_player_name) |>
  summarize(total_pass = sum(complete_pass))

passes <- nfl_passes |>
  count(passer_player_name)

names(passes)[names(passes) == "n"] <- "total_passes"
passes$completed_passes <- nfl2$total_pass


nfl1 <- nfl_passes |>
  mutate(attempted_pass = sum(completed_passes)/ sum(total_passes))


#completion percentage per season for all teams.

ggplot(data = nfl_passes) +
  geom_bar(aes(x = interception)) +
  labs(title = "Whether passes were intercepted or not")


ggplot(data = nfl_passes) +
  geom_bar(aes(y = offense_formation)) +
  labs(title = "Offense Formation",
       y = "Offense Formation Type") 


passes |>
  ggplot(aes(x = total_passes)) +
  geom_histogram()

summary(passes$total_passes)

passes <- passes |>
  filter(total_passes >= 100) |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))


# Top 10 players with the highest completion rate in terms of passes
top_ten <- head(passes, 10) |> 
  select(passer_player_name, completion_percentage) |>
  rename("Player Name" = passer_player_name) 
  


# Making pretty table for top 10 players
top_ten |>
  kable(booktabs = TRUE,
        caption = "Caption in progress",
        col.names = c("Player Nmae", "Completion (%)")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#000080") |>
  column_spec(2, color = "black", background = "#ffffff") |>
  add_header_above(c("Top 10 NFL Players with the 
                     highest Completion Percentage" = 2),
                   background = "#CD2626", color = "white", bold = TRUE)



## Average Time to throw for each player 
avgtime <- nfl_passes |>
  group_by(passer_player_name) |>
  summarize(avgtime = mean(time_to_throw)) 

## Average yards gained 
avgyards <- nfl_passes |>
group_by(passer_player_name) |>
summarize(avg_yards_gained = mean(yards_gained))


##

  
  




