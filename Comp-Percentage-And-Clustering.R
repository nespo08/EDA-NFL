# Load all regular season passes from the 2021 regular season -------------

library(nflreadr)
library(ggplot2)
library(tidyverse)

# nfl_2021_data <- nflreadr::load_pbp(2021, file_type = "rds")
# 
# nfl_passing_plays <- nfl_2021_data %>%
#   filter(play_type == "pass", season_type == "REG", 
#          !is.na(epa), !is.na(posteam), posteam != "") %>%
#   select(# Player info attempting the pass:
#     passer_player_name, passer_player_id, posteam, 
#     # Info about the pass:
#     complete_pass, interception, yards_gained, touchdown, 
#     pass_location, pass_length, air_yards, yards_after_catch, epa, wpa,
#     shotgun, no_huddle, qb_dropback, qb_hit, sack,
#     # Context about the receiver:
#     receiver_player_name, receiver_player_id   ,
#     # Team context:
#     posteam, defteam, posteam_type, 
#     # Play and game context:
#     play_id, yardline_100, side_of_field, down, qtr, play_clock,
#     half_seconds_remaining, game_half, game_id,
#     home_team, away_team, home_score, away_score,
#     # Description of play
#     desc)

nfl_passing_plays <- read.csv("nfl-passing-plays-2021.csv")

# Preview data
head(nfl_passing_plays, n = 50)
colnames(nfl_passing_plays)

nfl_passing_plays <- as_tibble(nfl_passing_plays)


# Testing - Not Used For Hypotheses ---------------------------------------

JA_passing_plays <- filter(nfl_passing_plays, passer_player_name %in% 'J.Allen')
#tail(JA_passing_plays)

# Air yardage vs EPA, colored by pass location
nfl_passing_plays %>% 
  ggplot(aes(x = air_yards, y = epa, color = pass_location)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# Air yardage vs EPA for Josh Allen, colored by completions
JA_passing_plays %>% 
  ggplot(aes(x = air_yards, y = epa, color = complete_pass)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# Air yardage vs EPA for Josh Allen, colored by touchdowns
JA_passing_plays %>% 
  ggplot(aes(x = air_yards, y = epa, color = touchdown)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# Air yardage vs WPA for Josh Allen, colored by touchdowns
JA_passing_plays %>% 
  ggplot(aes(x = air_yards, y = wpa, color = touchdown)) +
  geom_point(alpha = 0.75) +
  scale_color_gradient(low="darkorange", high = "darkblue") +
  theme_bw()

# Air yardage vs WPA for NFL, colored by interceptions
nfl_passing_plays %>% 
  ggplot(aes(x = air_yards, y = wpa, color = interception)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# Total yardage vs YAC for NFL
nfl_passing_plays %>%
  ggplot(aes(x = yards_gained, y = yards_after_catch)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# Visualize independence with a mosaic plot

table("Pass Length" = JA_passing_plays$qb_hit,
      "Completion" = JA_passing_plays$interception) %>%
  mosaicplot(main = "Relationship between QB Hit and Interceptions?",
             shade = TRUE) # Shade by Pearson residuals (how far we differ from exp. independence)


# Hypothesis #3: At what yardage range do NFL QBs complete the most passes? Where are they the most accruate? -------------------------------------

library(patchwork)

# Convert 0s and 1s to TRUE/FALSE
nfl_passing_plays$complete_pass <- replace(nfl_passing_plays$complete_pass, 
                                           nfl_passing_plays$complete_pass == 0, 
                                           "FALSE")
nfl_passing_plays$complete_pass <- replace(nfl_passing_plays$complete_pass, 
                                           nfl_passing_plays$complete_pass == 1,
                                           "TRUE")

# Create density curve for air yardage, colored by completions
nfl_density_curve <- nfl_passing_plays %>%
  ggplot(aes(x=air_yards,
             color = complete_pass)) +
  geom_density() +
  geom_rug(alpha = 0.25) +
  scale_color_manual(values=c("FALSE"="red",
                              "TRUE"="green")) +
  theme_bw() +
  labs(x = "Pass Distance (in Yards)",
       y = "Number of Passes") +
  theme(legend.position = "blank")

# Create ECDF  for air yardage, colored by completions
nfl_ecdf <- nfl_passing_plays %>%
  ggplot(aes(x=air_yards,
             color = complete_pass)) +
  geom_rug(alpha = 0.25) +
  stat_ecdf() +
  scale_color_manual(values=c("FALSE"="red",
                              "TRUE"="green")) +
  theme_bw() +
  labs(x = "Pass Distance (in Yards)",
       y = "Proportion of Passes",
       color = "Pass Completed")

# Combine plots
nfl_density_curve + nfl_ecdf + plot_layout(guides = "collect")


# Clustering (Not Used for Presentation) --------------------------------------------------------------

# Group by team - yac vs ay
nfl_passing_plays %>%
  filter(!is.na(air_yards), !is.na(yards_after_catch)) %>%
  group_by(posteam) %>%
  summarize(yac = sum(yards_after_catch), ay = sum(air_yards)) %>%
  ggplot(aes(x=yac,y=ay)) +
  geom_point()

# Group by player - yac vs ay
nfl_passing_plays %>%
  filter(!is.na(air_yards), 
         !is.na(yards_after_catch)) %>%
  group_by(passer_player_name) %>%
  summarize(yac = sum(yards_after_catch), ay = sum(air_yards)) %>%
  ggplot(aes(x=yac,y=ay)) +
  geom_point()

# Group by team - ty vs epa
nfl_passing_plays %>%
  filter(!is.na(yards_gained)) %>%
  group_by(posteam) %>%
  summarize(ty = sum(yards_gained), epa = sum(epa)) %>%
  ggplot(aes(x=ty,y=epa)) +
  geom_point()


# Clustering (Example) ----------------------------------------------------

# Minimax clustering
library(protoclust)
library(ggdendro)

# Compute the distance (euclidean)
player_clust <- nfl_passing_plays %>%
  group_by(passer_player_name) %>%
  summarize(total_yards = sum(yards_gained), epa = sum(epa))

player_clust <- player_clust %>%
  filter(total_yards > 500) # Remove players with less than 500 total yds

player_clust_std <- player_clust %>%
  mutate(std_total_yards = as.numeric(scale(total_yards)),
                                      std_epa = as.numeric(scale(epa)))

player_dist <- dist(dplyr::select(player_clust_std, 
                    std_total_yards, 
                    std_epa))

nfl_protoclust <- protoclust(player_dist)

# Plot dendrogram
ggdendrogram(nfl_protoclust, 
             labels = FALSE,
             leaf_labels = FALSE,
             theme_dendro = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# Minimax clustering
minimax_player_clusters <- protocut(nfl_protoclust, k = 4)

player_clust_std %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>%
  ggplot(aes(x = std_total_yards, y = std_epa,
             color = player_clusters)) +
  geom_point(alpha = 0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Clustering with prototypes
nfl_prototypes <- player_clust_std %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>%
  slice(minimax_player_clusters$protos)

player_clust_std %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>%
  ggplot(aes(x = std_total_yards,
             y = std_epa, color = player_clusters)) +
  geom_point(data = mutate(nfl_prototypes,
                           player_clusters = as.factor(c(1,2,3,4))),
             size = 4) +
  geom_point(alpha = 0.5) +
  geom_label(data = nfl_prototypes, 
             aes(label = passer_player_name), 
             size = 4,
             show.legend = FALSE) +
  ggthemes::scale_color_colorblind() +
  labs(x = "Total Yards (Standardized)",
       y = "EPA (Standardized)",
       color = "QB Clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

# Create a table of the cluster statistics
data.frame("Clusters" = minimax_player_clusters$cl,
      "Names" = player_clust_std$passer_player_name) %>%
  arrange(minimax_player_clusters$cl)