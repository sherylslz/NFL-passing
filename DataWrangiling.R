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


########################################################
Formation <- nfl_passes |>
  dplyr::select(offense_formation, complete_pass) |>
  dplyr::group_by(offense_formation) |>
  dplyr::summarize(total_pass = sum(complete_pass))

Formation2 <- nfl_passes |>
  dplyr::select(offense_formation, complete_pass)

Formation_p <- nfl_passes |>
  count(offense_formation)

names(Formation_p)[names(Formation_p) == "n"] <- "total_passes"
Formation_p$completed_passes <- Formation$total_pass

Formation_p <- Formation_p |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))

#==========######################[ Plotting ]##################################
# ===========================[ Making the plot more aesthetically pleasing]=====

Formation_p_clean <-  Formation_p |>
  mutate(offense_formation = case_when(
    offense_formation %in% c("JUMBO", "PISTOL", "WILDCAT") ~ "Other",
    offense_formation %in% c("I_FORM") ~ "I_FORM",
    offense_formation %in% c("SHOTGUN") ~ "SHOTGUN",
    offense_formation %in% c("SINGLEBACK") ~ "SINGLEBACK",
    offense_formation %in% c("EMPTY") ~ "EMPTY",
    TRUE ~ "Other"
  ))

Formation_p_clean |>
  ggplot(aes(x = completion_percentage,
             y = reorder(offense_formation, completion_percentage))) +
  geom_col(fill = "navyblue", width = 0.6) +
  geom_text(aes(label = scales::percent(completion_percentage, accuracy = 0.1)),
            hjust = 1.1, color = "white", size = 4) +
  geom_text(aes(label = paste0("n = ", total_passes)),
            hjust = -0.1, size = 3.5, color = "#333333") +
  labs(
    title = "Completion Percentage by Offense Formation",
    subtitle = "completion rate and total count of pass attempts",
    x = "Completion Percentage",
    y = "Offense Formation"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#######################[ ROUTE TYPE PLOT ]#################################

route <- nfl_passes |>
  dplyr::select(route_ran, complete_pass) |>
  dplyr::group_by(route_ran) |>
  dplyr::summarize(total_pass = sum(complete_pass))

route2 <- nfl_passes |>
  dplyr::select(route_ran, complete_pass)

route_p <- nfl_passes |>
  count(route_ran)

names(route_p)[names(route_p) == "n"] <- "total_passes"
route_p$completed_passes <- route$total_pass

route_p_clean <- route_p |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))

# ====================================== plotting ==============================

route_p_clean |>
  ggplot(aes(x = completion_percentage,
             y = reorder(offense_route_grouped, completion_percentage))) +
  geom_col(fill = "navyblue", width = 0.6) +
  geom_text(aes(label = scales::percent(completion_percentage, accuracy = 0.1)),
            hjust = 1.1, color = "white", size = 4) +
  geom_text(aes(label = paste0("n = ", total_passes)),
            hjust = -0.1, size = 3.5, color = "#333333") +
  labs(
    title = "Completion Percentage by Offense Formation",
    subtitle = "completion rate and total count of pass attempts",
    x = "Completion Percentage",
    y = "Offense Formation"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )





#############HEXBIN GRAPH##################

Jalen_Hurts <- nfl_passes |>
  filter(passer_player_name == "J.Hurts")

Tua <- nfl_passes |>
  filter(passer_player_name == "T.Tagovailoa")

Tua |>
  ggplot(aes(target_x, target_y))+
  geom_hex(binwidth = c(1,1)) +
  scale_fill_gradient(low = "darkblue",
                      high = "gold")

Jalen_Hurts |>
  ggplot(aes(target_x, target_y))+
  geom_hex(binwidth = c(1,1)) +
  scale_fill_gradient(low = "darkblue",
                      high = "gold")


library(sportyR)
library(hexbin)
football_field <- geom_football("NFL", x_trans = -41.5)
fo +
  geom_hex(data = clark_shots, aes(x = shot_x, y = shot_y), binwidth = c(1, 1)) + 
  scale_fill_gradient(low = "midnightblue", high = "gold")

Top_QBs_new <- Top_QBs |>
  mutate(std_target_x = scale(target_x))

library(ggplot2)
library(cowplot)
#Hurts graph
geom_football("NFL") +
  stat_summary_hex(aes(x = target_x - 60,
                       y = target_y- (53.3/2),
                       z = interception,
                       group = -1), data = Jalen_Hurts,
                   binwidth = c(3,3),
                   fun = mean, color = 'black') +
  scale_fill_gradient(low = "darkblue",
                      high = "gold")





#Tua graph
geom_football("NFL") +
  stat_summary_hex(aes(x = target_x - 60,
                       y = target_y- (53.3/2),
                       z = complete_pass,
                       group = -1), data = Tua,
                   binwidth = c(3,3),
                   fun = mean, color = 'black') +
  scale_fill_gradient(low = "darkblue",
                      high = "gold")




completed_pass <- nfl_passes[which(nfl_passes$complete_pass == 1 & nfl_passes$passer_player_name=="L.Jackson"),]
completed_pass$target_x <- completed_pass$target_x-60
completed_pass$target_y <- completed_pass$target_y- (53.3/2)

intercepted_pass <- nfl_passes[which(nfl_passes$interception == 1 & nfl_passes$passer_player_name=="L.Jackson"),]
intercepted_pass$target_x <- intercepted_pass$target_x-60
intercepted_pass$target_y <- intercepted_pass$target_y- (53.3/2)
geom_football("nfl",display_range="in_bounds_only") +
  geom_point(data=completed_pass,x=completed_pass$target_x,y=completed_pass$target_y, colour = "blue")+
  geom_point(data=intercepted_pass,x=intercepted_pass$target_x,y=intercepted_pass$target_y,colour = "red")
>>>>>>> 7ebc262 (Update DataWrangiling.R)


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

 #Cluster Plot: Completion percentage by Int per attempt
 cluster_data |> 
   ggplot(aes(x=completion_percentage, y=interception_total, color=cluster,
                               label = passer_player_name)) + 
  geom_point(size = 5) + 
  geom_text(hjust=0, vjust=0)
 
<<<<<<< HEAD

##Gradient Joint Distribution Graph 
 
nfl_passes |>
  gradient_data <- nfl_passes

   group_by(offense_formation, route_ran) |>
   summarize(
     freq = mean(complete_pass), 
   ) |> 
   ggplot(aes(x = offense_formation, y = route_ran)) +
   geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = freq))
   scale_fill_gradient2()
   
   ###fefqwrf


=======
 #hhhh
 
Top_QB <- nfl_passes |>
  filter(passer_player_name == "J.Hurts" | passer_player_name == "T.Tagovailoa" |  passer_player_name == "P.Mahomes")


library(ggplot2)
Top_QB |>
   count(passer_player_name, complete_pass) |>
   ggplot(aes(x = passer_player_name, y = n, 
              fill = complete_pass)) +
  geom_col(position = "fill")

Top_QB |>
  count(offense_formation, passer_player_name) |>
  ggplot(aes(x = offense_formation, y = n, 
             fill = passer_player_name)) + 
  geom_col(position = "dodge")


Top_QB %>%
  ggplot(aes(x = passer_player_name, fill = target_player_name)) + 
  geom_bar() +
  facet_wrap(~ offense_formation)


##############Team Completion Percentage#######################
Teams <- nfl_passes |>
  count(posteam)

nfl12 <- nfl_passes |>
  select(posteam, complete_pass) |>
  group_by(posteam) |>
  summarize(total_pass = sum(complete_pass))

names(Teams)[names(Teams) == "n"] <- "total_passes"
Teams$completed_passes <- nfl12$total_pass

Teams <- Teams |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))

Top_5_Teams <- Teams2 |>
  head(5)
############################################################
nfl13 <- nfl_passes |>
  select(posteam, offense_formation,complete_pass) |>
  group_by(posteam, offense_formation) |>
  summarize(total_pass = sum(complete_pass))

Teams2 <- nfl_passes |>
  count(posteam,offense_formation)

names(Teams2)[names(Teams2) == "n"] <- "total_passes"
Teams2$completed_passes <- nfl13$total_pass

Teams2 <- Teams2 |>
  mutate(completion_percentage = completed_passes/ total_passes) |>
  arrange(desc(completion_percentage))

Top_5_Teams <- Teams2 |>
  head(5)

Top_5_Teams |>
  count(posteam, offense_formation) |>
  ggplot(aes(x = posteam, y = n, 
             fill = offense_formation)) + 
  geom_col(position = "dodge")


#######FORMATION VS. TEAMS##########
Teams3 <- nfl_passes |>
  filter(offense_formation == "EMPTY" |
           offense_formation == "I_FORM" |
           offense_formation == "JUMBO" |
           offense_formation == "PISTOL" |
           offense_formation == "SHOTGUN" |
           offense_formation == "SINGLEBACK",
         posteam == "SEA" |
           posteam == "PHI" |
           posteam == "CIN" |
           posteam == "NE" |
           posteam == "IND" |
           posteam == "ARI" |
           posteam == "LA" |
           posteam == "MIN" |
           posteam == "KC" |
           posteam == "JAX")


library(ggplot2)
Teams3 |>
  count(offense_formation, posteam) |>
  ggplot(aes(x = offense_formation, y = n, 
             fill = posteam)) +
  geom_col(position = "fill")

Top_QB |>
  count(offense_formation, passer_player_name) |>
  ggplot(aes(x = offense_formation, y = n, 
             fill = passer_player_name)) + 
  geom_col(position = "dodge")


Top_QB %>%
  ggplot(aes(x = passer_player_name, fill = target_player_name)) + 
  geom_bar() +
  facet_wrap(~ offense_formation)

# ======================== [ MOSAIC PLOT ]=====================================-

nfl_passes |> 
  dplyr::select(offense_formation, route_ran) |> 
  table() |> 
  mosaicplot(main = "Relationship between 
  offense formation and route ran")


