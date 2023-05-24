#NBA Stats Analysis

library(dplyr)
library(ggplot2)

getwd()
setwd("~/Projects/NBA_Analysis/NBA_Data")
rm(list = ls())
BasketballPlayers.data = read.csv("Player Totals.csv")
NBAPlayers.FullData = BasketballPlayers.data %>%
  filter(lg == "NBA")


#Number of Basketball Players in each NBA team throughout 2023
NBATeams.Players = BasketballPlayers.data %>% 
  select(player,season,lg,Teams = 'tm') %>%
  filter(lg == "NBA",season == 2023) %>%
  group_by(Teams) %>%
  count(Teams) %>%
  rename(Number_Of_Players = n) %>%
  arrange(desc(Number_Of_Players))


Teams_MostPlayers = NBATeams.Players %>% head(5)
print(Teams_MostPlayers)
#As it can be seen there are 70 players on team TOT but TOT is not a team
#The 70 players listed under team TOT during the 2023 season, thay have played for at least 2 or more Teams thus being listed under TOT(Two Other Teams)

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
  select(player_id, player, g, gs, ast,stl,blk,tov,pf,pts) %>%
  group_by(player) %>%
  summarise(total_games = sum(g),total_gamesStarted = sum(gs),total_ast = sum(ast),
            total_steals = sum(stl),total_blocks = sum(blk),total_turnovers = sum(tov), 
            total_fouls = sum(pf), total_points = sum(pts), no_seasons = n()) 
  
  
  






