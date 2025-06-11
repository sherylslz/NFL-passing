
#===============================Packages========================================

library(tidyverse)
library(kableExtra)
library(sportyR)
library(ggplot2)




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

# creating `nfl2` with player name and their total completed passes
nfl2 <- nfl_passes |>
  dplyr::select(passer_player_name, complete_pass) |>
  group_by(passer_player_name) |>
  summarize(total_pass = sum(complete_pass))

#passes
passes1 <- nfl_passes |>
  count(passer_player_name)

names(passes)[names(passes) == "n"] <- "total_passes"
passes$completed_passes <- nfl2$total_pass

passes <- passes |>
  filter(total_passes >= 100) |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))


#################
nfl3 <- nfl_passes |>
  dplyr::select(target_player_name, complete_pass) |>
  group_by(target_player_name) |>
  summarize(total_pass = sum(complete_pass))

receiver_target <- nfl_passes |>
  count(target_player_name)

names(receiver_target)[names(receiver_target) == "n"] <- "total_passes"
receiver_target$completed_passes <- nfl3$total_pass

Targets_total <- receiver_target |>
  filter(total_passes) |>
  mutate(completion_percentage = completed_passes/ total_passes)

Targets <- receiver_target |>
  filter(total_passes >= 20) |>
  mutate(completion_percentage = completed_passes/ total_passes)

Top_targets <- Targets |>
  filter(total_passes >= 10) |>
  mutate(completion_percentage = completed_passes/ total_passes)


Top_targets <- Targets |> 
  select(target_player_name, completion_percentage,total_passes,completed_passes) |>
  rename('Player Name' = target_player_name) |>
  arrange(desc(total_passes))

Top_received = head(Top_targets, 10)


#######################Combos


nfl4 <- nfl_passes |>
  dplyr::select(target_player_name, passer_player_name, complete_pass) |>
  group_by(passer_player_name,target_player_name) |>
  count(passer_player_name,target_player_name)

nfl5 <- nfl_passes |>
  filter(complete_pass == "1") |>
  dplyr::select(target_player_name, passer_player_name, complete_pass) |>
  group_by(passer_player_name,target_player_name) |>
  count(passer_player_name,target_player_name)

nfl6 <- left_join(nfl4, nfl5, by = join_by(passer_player_name, target_player_name)) |>
  mutate(n.y = if_else(is.na(n.y), 0, n.y))

nfl8 <- nfl_passes |>
  filter(complete_pass == "1") |>
  dplyr::select(target_player_name, passer_player_name, complete_pass, touchdown) |>
  group_by(passer_player_name,target_player_name) |>
  count(passer_player_name,target_player_name)

Combos_total <- nfl6 |>
  filter(n.x >= 10) |>
  mutate(completion_percentage = n.y/ n.x)

#=====================================================================================

names(receiver_target)[names(receiver_target) == "n"] <- "total_passes"

nfl7 <- (nfl5$n/nfl4$n)
view(nfl7)
summarize(total_pass = sum(complete_pass))

Combos <- nfl_passes |>
  group_by(passer_player_name,target_player_name) |>
  count(passer_player_name,target_player_name)

  
names(Combos)[names(Combos) == "n"] <- "total_passes"
Combos$completed_passes <- nfl4$total_passes


Combos_totals <- Combos |>
  filter(total_passes) |>
  mutate(completion_percentage = completed_passes/ total_passes)

#======================================================================================


nfl1 <- nfl_passes |>
  mutate(attempted_pass = sum(completed_passes)/ sum(total_passes))


#============completion percentage per season for all teams=============================

# visualizing interception
ggplot(data = nfl_passes) +
  geom_bar(aes(x = interception)) +
  labs(title = "Whether passes were intercepted or not")

# visualizing offense formation
ggplot(data = nfl_passes) +
  geom_bar(aes(y = offense_formation)) +
  labs(title = "Offense Formation",
       y = "Offense Formation Type") 

# visualizing total passes (not very descriptive)
passes |>
  ggplot(aes(x = total_passes)) +
  geom_histogram()

summary(passes$total_passes)



#==========================Updated upstream=====================================

# Top 10 players with the highest completion rate in terms of passes
top_ten <- head(passes, 10) |> 
  dplyr::select(passer_player_name, completion_percentage) |>
  rename("Player Name" = passer_player_name) 

#===============================================================================

df = head(passes$passer_player_name, 10)

top_ten <- passes |> 
  dplyr::select(passer_player_name, completion_percentage) |>
  rename('Player Name' = passer_player_name)
  

#===============================================================================

df = head(top_ten, 10)

avgtime <- nfl_passes |>
  
  group_by(passer_player_name) |>
  summarize(avgtime = mean(time_to_throw))

#==============Making pretty table for top 10 players#==========================

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



#==Average Time to throw for each player========================================

avg_time <- nfl_passes |>
  group_by(passer_player_name) |>
  summarize(avg_time = mean(time_to_throw, na.rm = TRUE)) |>
  arrange(avg_time)


top_20_t <- head(avg_time, 20) |>
  mutate(avg_time = round(avg_time, 2))


#========Table displaying top 20 players with the lowest time to throw==========

top_20_t |>
  kable(booktabs = TRUE,
        caption = "Caption in progress",
        col.names = c("Player Name", "Time to Throw (sec)")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#000080") |>
  column_spec(2, color = "black", background = "#ffffff") |>
  add_header_above(c("Top 20 NFL Players with the 
                     lowest time to throw" = 2),
                   background = "#CD2626", color = "white", bold = TRUE)

#====Average yards gained========================================================

avg_yards <- nfl_passes |>
group_by(target_player_name) |>
summarize(avg_yards_gained = mean(yards_gained, na.rm = TRUE)) |>
arrange(desc(avg_yards_gained))

top_20 <- head(avg_yards, 20) |>
  mutate(avg_yards_gained = round(avg_yards_gained, 2))

#=========Table displaying top 20 players with the highest yards gained=========

top_20 |>
  kable(booktabs = TRUE,
        caption = "Caption in progress",
        col.names = c("Player Name", "Yard Gained")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#000080") |>
  column_spec(2, color = "black", background = "#ffffff") |>
  add_header_above(c("Top 20 NFL Players with the 
                     highest Average Yards Gained" = 2),
                   background = "#CD2626", color = "white", bold = TRUE)



#================Making a scatter plot to see relationship======================

ggplot(data = ds_2, aes(x = avg_yards_gained, y = completion_percentage)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", linewidth = 2) +
  labs(
    title = "Completion % vs. Time to Throw",
    x = "Average Time to Throw (sec)",
    y = "Completion Percentage %"
  ) +
  theme_minimal()


ggplot(data = ds_1, aes(x = avg_time, y = completion_percentage)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", linewidth = 2) +
  labs(
    title = "Completion % vs. Yards Gained",
    x = "Average Yards Gained",
    y = "Completion Percentage %"
  ) +
  theme_minimal()



completed_pass <- nfl_passes[which(nfl_passes$complete_pass == 1 & nfl_passes$passer_player_name=="L.Jackson"),]
completed_pass$target_x <- completed_pass$target_x-60
completed_pass$target_y <- completed_pass$target_y- (53.3/2)

intercepted_pass <- nfl_passes[which(nfl_passes$interception == 1 & nfl_passes$passer_player_name=="L.Jackson"),]
intercepted_pass$target_x <- intercepted_pass$target_x-60
intercepted_pass$target_y <- intercepted_pass$target_y- (53.3/2)


geom_football("nfl",display_range="in_bounds_only") +
  geom_point(data=completed_pass,x=completed_pass$target_x,y=completed_pass$target_y, colour = "blue")+
  geom_point(data=intercepted_pass,x=intercepted_pass$target_x,y=intercepted_pass$target_y,colour = "red")



# Wrong. X variable is categorical binary not numeric 
nfl_passes |> 
  ggplot(aes(x = complete_pass, y = yards_gained)) +
  geom_point(color = "darkred", size = 4, alpha = 0.5) +
  geom_smooth(method = "lm", linewidth = 2)

# Are players who throw more quickly more efficient and accurate which results in higher completion percentages and yards gained? 

