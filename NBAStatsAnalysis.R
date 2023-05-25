#NBA Stats Analysis

library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(gganimate)
library(gapminder)
library(ggthemes)

getwd()
setwd("~/Projects/NBA_Analysis/NBA_Data")
rm(list = ls())
BasketballPlayers.data = read.csv("Player Totals.csv")

#Data cleaned by only including those from the NBA 
#Data also cleaned by removing data rows which are listed under Team "TOT" (Two Other Teams)
#Due to the fact that their stats in their respective teams were already listed 
#Meaning that those rows would cause inaccuracies in calculations
NBAPlayers.FullData = BasketballPlayers.data %>%
  filter(lg == "NBA", tm != "TOT")


#Number of Basketball Players in each NBA team throughout 2023
NBATeams.Players = BasketballPlayers.data %>% 
  select(player,season,lg,Teams = 'tm') %>%
  filter(lg == "NBA",season == 2023) %>%
  group_by(Teams) %>%
  count() %>%
  rename(Number_Of_Players = n) %>%
  arrange(desc(Number_Of_Players)) %>%
  ungroup()



Teams_MostPlayers = NBATeams.Players %>% head(5)
print(Teams_MostPlayers)
#As it can be seen there are 70 players on team TOT but TOT is not a team
#The 70 players listed under team TOT during the 2023 season, thay have played for at least 2 or more Teams thus being listed under TOT(Two Other Teams)
#As it can be noted, BRK,LAL,DAL,POR and SAS had the most number of players throughout the 2023 season
#These results would further allow me to determine that these teams made the most player trades and signings. 

NBA.Players = sum(NBATeams.Players$Number_Of_Players)
print(NBA.Players)
#There are a total of 679 NBA players who have been signed and played in the NBA throughout the 2023 season
#Given that there are a total of 30 NBA teams and each team is only allowed to have 15 players during the NBA Season, that would theoretically mean that there should only be 450 players 
#There are many variables to why there are far more players than expected
#First of the many reasons would be that players decide to retire mid way through the season
#Second, players could have been waived off by the team in order to create roster space for another player
#Third, players were transferred to their respective development teams in order to improve their skills in the NBA G-League.
#Fourth, the NBA may have banned or suspended the players due to the players breaking NBA rules and guidelines such as possession of weapon or substance abuse.


#Players Overall Total Stats 
NBAPlayer.totalStats = NBAPlayers.FullData %>%
  select(player_id, player,pos, g, gs, ast,stl,blk,tov,pf,pts) %>%
  group_by(player) %>%
  summarise(total_games = sum(g),total_gamesStarted = sum(gs),total_ast = sum(ast),
            total_steals = sum(stl),total_blocks = sum(blk),total_turnovers = sum(tov), 
            total_fouls = sum(pf), total_points = sum(pts), no_seasons = n()) %>%
  arrange(desc(total_points)) %>%
  ungroup()

ggplot(NBAPlayer.totalStats, aes(x = total_points, y = total_ast)) +
  geom_point()



GOATComparison = NBAPlayers.FullData %>%
  filter(player == "LeBron James" |player == "Michael Jordan" |player == "Kobe Bryant") %>%
  group_by(player) %>%
  arrange(experience) %>%
  mutate(TotalPoints = cumsum(pts), TotalAssists = cumsum(ast), TotalRebounds = cumsum(trb)) %>%
  ungroup()
  
  
ggplot(GOATComparison, aes(experience,TotalPoints,group = player, color = player, size = ast))+
  geom_point(alpha = 0.7) +
  labs(x = "Season", y = "Points", title = "Player Performance") +
  theme_minimal()

#As it can be seen Lebron James has scored the most Total Points as compared to the others
#But Michael Jordan Stopped Playing after his 15th Season and if we were to compare theyre total points

#Get average points per game over their whole career

  






