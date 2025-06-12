library(tidyverse)
library(kableExtra)
library(factoextra)



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


passes_2 <- nfl_passes |>
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


## Average QB touchdowns  




## Average Time to throw for each player 
avg_time <- nfl_passes |>
  group_by(passer_player_name) |>
  summarize(avg_time = mean(time_to_throw, na.rm = TRUE)) |>
  arrange(avg_time)


top_20_t <- head(avg_time, 20) |>
  mutate(avg_time = round(avg_time, 2))


# Table displaying top 20 players with the lowest time to throw

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

## Average yards gained 
avg_yards <- nfl_passes |>
group_by(passer_player_name) |>
summarize(avg_yards_gained = mean(yards_gained, na.rm = TRUE)) |>
arrange(desc(avg_yards_gained)) 

top_20 <- head(avg_yards, 20) |>
  mutate(avg_yards_gained = round(avg_yards_gained, 2))

## TD/attempt player data 

td_per_attempt <- nfl_passes |> 
group_by(passer_player_name) |> 
summarize(td_per_attempt = mean(touchdown, na.rm = TRUE)) 
arrange(desc("td_per_attempt"))  


##TD per attempt Histogram 
td_per_attempt |> 
  ggplot(aes(td_per_attempt))
  geom_histogram()
  
  
##
 



# Table displaying top 20 players with the highest yards gained

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



# General/Joint datasets

ds_1 <- passes |>
  left_join(avg_time, by = "passer_player_name")

ds_2 <- passes |>
  left_join(avgyards, by = "passer_player_name")

ds_3 <- ds_2 |> 
left_join(td_per_attempt, by = "passer_player_name")

ds_4 <- ds_1 |> 
left_join(td_per_attempt, by = "passer_player_name")  

ds_5 <- ds_1 |> 
  left_join(avg_yards, by = "passer_player_name")

ds_6 <- ds_3 |>
  left_join(inter_ds, ds_3, by = "passer_player_name")



# Making a scatter plot to see relationship

#Average time to throw on completion percentgage
ggplot(data = ds_1, aes(x = avg_time, y = completion_percentage )) +
  geom_point(color = "blue") +
  labs(
    title = "Completion % vs. Time to Throw",
    x = "Average Time to Throw (sec)",
    y = "Completion Percentage %"
  ) +
  theme_minimal()

#Completion Percentage by Average yards gained
ggplot(data = ds_2, aes(x = avg_yards_gained, y = completion_percentage)) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Completion % vs. Yards Gained",
    x = "Average Yards Gained",
    y = "Completion Percentage %"
  ) +
  theme_minimal()



#Completion percentage by Touch down per attempt 
ggplot(data = ds_3, aes(x = td_per_attempt , y = completion_percentage)) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Completion % vs. td_per_attempt ",
    x = "Touch Down Per Attempt ",
    y = "Completion Percentage %") 
     theme_minimal()
     
#Average Yards gained by touch down per attempt 
     ggplot(data = ds_3, aes(x = avg_yards_gained  , y = td_per_attempt ))+
       geom_point(color = "darkgreen") +
       labs(
         title = "Avg Yards Gained vs. td_per_attempt ",
         x = "Avg Yards Gained" ,
         y = "Touch Down Per Attempt") 
     theme_minimal()
     
## Time to throw by touch down per attempt 
     ggplot(data = ds_4, aes(x = avg_time , y = td_per_attempt ))+
       geom_point(color = "darkgreen") +
       labs(
         title = "avg_time vs. td_per_attempt ",
         x = "avg_time" ,
         y = "Touch Down Per Attempt") 
     theme_minimal()
     
## Time to throw by Average Yards gained 
     
     ggplot(data = ds_5, aes(x = avg_time , y = avg_yards_gained ))+
       geom_point(color = "darkgreen") +
       labs(
         title = "avg_time vs. avg_yards_gained ",
         x = "avg_time" ,
         y = "avg_yards_gained") 
     theme_minimal()
     

     

     
    # interception
     

inter_ds <- nfl_passes |>
  dplyr::select(interception, passer_player_name) |>
  group_by(passer_player_name) |>
  summarize(interception_total = mean(interception))



# ======================= [ CLUSTERING ] =======================================

## not the bets metric to judge the quality of a player

clust_nfl <- ds_6 |> 
  dplyr::select(completion_percentage, avg_yards_gained, td_per_attempt) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1)

ds_6 |>
  mutate(
    player_clusters = as.factor(clust_nfl$cluster)
  ) |>
  ggplot(aes(x = avg_yards_gained, y = completion_percentage,
             color = player_clusters, shape = player_clusters)) +
  geom_point(size = 5, alpha = 1) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") 
     




# ======================= [ SCALING ] =======================================

ds_6 <- ds_6 |>
  dplyr::mutate(
    std_completion_percentage = as.numeric(scale(completion_percentage, center = TRUE, scale = TRUE)),
    std_avg_yards_gained = as.numeric(scale(avg_yards_gained, center = TRUE, scale = TRUE))
  )

std_kmeans_cp <- ds_6 |> 
  dplyr::select(std_completion_percentage, std_avg_yards_gained) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1)

ds_6 |>
  mutate(
    player_clusters = as.factor(std_kmeans_cp$cluster)
  ) |>
  ggplot(aes(x = std_avg_yards_gained, y = std_completion_percentage,
             color = player_clusters)) +
  geom_point(size = 5, alpha = 1) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  coord_fixed()



# ======================= [ SCALING ] ==========================================


ds_features <- ds_6 |>
  mutate(log_completion = log(completion_percentage)) |> 
  dplyr::select(avg_yards_gained, log_completion, td_per_attempt, interception_total) |> 
  drop_na() 


std_ds_features <- ds_features |> 
  scale(center = TRUE, scale = TRUE)


kmeans_ds_features <- std_ds_features |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 4, nstart = 30) 


kmeans_ds_features |> 
  # need to pass in data used for clustering
  fviz_cluster(data = std_ds_features,
               geom = "point",
               ellipse = FALSE) +
  ggthemes::scale_color_colorblind() + 
    geom_point(size = 5, alpha = 1) + 
  theme_light()


ds_6 |>
  mutate(
    player_clusters = as.factor(std_kmeans_cp$cluster)
  ) |>
  ggplot(aes(x = avg_yards_gained, y = completion_percentage,
             color = player_clusters)) +
  geom_point(size = 5, alpha = 1) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  coord_fixed()


########################################################
Formation <- nfl_passes |>
  select(offense_formation, complete_pass) |>
  group_by(offense_formation) |>
  summarize(total_pass = sum(complete_pass))

Formation_p <- nfl_passes |>
  count(offense_formation)

names(Formation_p)[names(Formation_p) == "n"] <- "total_passes"
Formation_p$completed_passes <- Formation$total_pass

Formation_p <- Formation_p |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))



##Cluster data set 

cluster_data <- ds_features
cluster_data$cluster <- kmeans_ds_features$cluster


cluster_data <- ds_7 |>
left_join(cluster_data, ds_7, by = "avg_yards_gained")

cluster_data |> 
  ggplot(aes(x=completion_percentage, y=avg_yards_gained, color=cluster)) + 
  geom_point()+
  scale_color_calc()


 #Cluster Plot: Completion percentage by Avg Yards gained 
   cluster_data |> 
   ggplot(aes(x=completion_percentage, y=avg_yards_gained, color=cluster, 
                               label = passer_player_name)) + 
   geom_point(size = 5)+
   geom_text(hjust=0, vjust=0)
   
#Cluster Plot: Completion percentage by TD per attempt
  cluster_data |> 
  ggplot(aes(x=completion_percentage, y=td_per_attempt.x, color=cluster, 
                             label = passer_player_name)) + 
     geom_point(size = 5) + 
   geom_text(hjust=0, vjust=0)

 #Cluster Plot: Completion percentage by Int per atempt
 cluster_data |> 
   ggplot(aes(x=completion_percentage, y=interception_total, color=cluster,
                               label = passer_player_name)) + 
  geom_point(size = 5) + 
  geom_text(hjust=0, vjust=0)
  
 
 #hhhh
