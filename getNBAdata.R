library(tidyverse)
library(jsonlite)
library(rvest)
library(RSelenium)
library(wdman)
library(httr)
library(data.table)
##### Functions #####

getHeaders = function(webpage) {
  
  # The table headings
  # Not all of them are visible though
  tableHeader <- webpage %>% html_nodes("div.nba-stat-table__overflow") %>% html_nodes("table") %>% html_nodes("thead") %>% html_nodes("th")
  visibleHeaders <- list()
  
  if (length(tableHeader) == 0) {
    return(visibleHeaders)
  }
  
  # To extract the correct nodes, the ones that don't have "hidden" tags and that do have a "data-field" value are selected
  for (i in 1:length(tableHeader)) {
    if (is.na(tableHeader[[i]] %>% html_attr("hidden")) && !is.na(tableHeader[[i]] %>% html_attr("data-field"))) {
      visibleHeaders[[length(visibleHeaders) + 1]] <- tableHeader[[i]] %>% html_text() %>% str_trim()
    }
  }
  
  # The names of the accessible data
  visibleHeaders <- unlist(visibleHeaders)
  
  # The rank labels are not a part of this variable, until now
  visibleHeaders <- c("RANK", visibleHeaders)
  
  return(visibleHeaders)
  
}


getData = function(webpage, visibleHeaders) {
  
  # A list of the actual data values, mixed in with extra elements
  stats <- webpage %>% html_nodes("div.nba-stat-table__overflow") %>% html_nodes("tbody") %>% html_nodes("tr")
  
  # More filtering is needed
  extractedStats <- list()
  
  # The valid rows have an "index" attribute
  for (i in 1:length(stats)) {
    if (!is.na(stats[[i]] %>% html_attr("index"))) {
      extractedStats[[length(extractedStats) + 1]] <- stats[[i]] %>% html_nodes("td") %>% html_text() %>% str_trim()
    }
    
  }
  
  # The list will be converted into a matrix now
  extractedStats <- extractedStats %>% unlist() %>% matrix(ncol = length(visibleHeaders), byrow = TRUE)
  colnames(extractedStats) <- visibleHeaders %>% unlist()
  
  # If the variable should be a data frame with the numbers converted from strings into numerical values, 
  # then this code can be run
  extractedStats <- extractedStats %>% data.frame(stringsAsFactors = FALSE)
  
  for (i in 1:length(extractedStats)) {
    
    # Names that contain only characters would become "" when modified
    testVal <- extractedStats[1, i]
    testVal <- testVal %>% str_replace("[^0-9]+", "")
    
    if (testVal != "") { 
      extractedStats[i] <- sapply(extractedStats[i], as.numeric)
    }
  }
  
  return(extractedStats)
  
}

# Dynamic scraping is needed for this 

# server <- phantomjs(port = 4567L, verbose = FALSE)
# rd <- remoteDriver(browserName = "phantomjs", port = 4567L)

# Using Chrome
server <- chrome(port = 4567L, version = "80.0.3987.106", verbose = FALSE)
rd <- remoteDriver(browserName = "chrome", port = 4567L)



##### Team Data #####


#### Win Percentage ####

teamURL <- "https://stats.nba.com/teams/traditional/?sort=W_PCT&dir=-1&Season=2019-20&SeasonType=Regular%20Season"

# If we want totals instead of per game data, use this line:
# teamURL <- "https://stats.nba.com/teams/traditional/?sort=W_PCT&dir=-1&Season=2019-20&SeasonType=Regular%20Season&PerMode=Totals"


# Available data on this page:
# GP Games Played 
# W Wins 
# L Losses 
# WIN% Win Percentage 
# MIN Minutes Played 
# FGM Field Goalds Made 
# FGA Field Goals Attempted 
# FG% Field Goal Percentage 
# 3PM 3 Point Field Goals Made 
# 3PA 3 Point Field Goals Attempted 
# 3P% 3 Point Field Goals Percentage 
# FTM Free Throws Made 
# FTA Free Throws Attempted 
# FT% Free Throw Percentage 
# OREB Offensive Rebounds 
# DREB Defensive Rebounds 
# REB Rebounds 
# AST Assists 
# TOV Turnovers 
# STL Steals 
# BLK Blocks 
# BLKA Blocked Field Goal Attempts 
# PF Personal Fouls 
# PFD Personal Fouls Drawn 
# PTS Points
# +/- Plus Minus


rd$open(silent = TRUE)
rd$navigate(teamURL)

# The HTML data of the page
teamPage <- rd$getPageSource() %>% str_flatten() %>% read_html()

visibleHeaders_Team <- getHeaders(teamPage)

teamStats <- getData(teamPage, visibleHeaders_Team)
  


##### Player Data #####


# We can easily get a lot of player stats from this JSON
playerURL <- "https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=PerGame&Scope=S&Season=2019-20&SeasonType=Regular+Season&StatCategory=PTS"

# If totals are desired instead of per game stats, use this
# playerURL <- "https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=Totals&Scope=S&Season=2019-20&SeasonType=Regular+Season&StatCategory=PTS"

playerData <- fromJSON(playerURL, simplifyDataFrame = TRUE)

# The data in this JSON are:
# "PLAYER_ID": Unique ID to each player
# "RANK": Leaders in the value of StatCategory (PTS in this example)
# "PLAYER": Player First Names and Last Names
# "TEAM": Abbreviation for team namess
# "GP": Games played
# "MIN": Minutes played
# "FGM": Field goals made
# "FGA": Field goals attempted
# "FG_PCT": Field goal success percent
# "FG3M": 3-point field goals made
# "FG3A": 3-point field goals attempted
# "FG3_PCT": 3-point field goal success percent
# "FTM": Free throws made
# "FTA": Free throws attempted
# "FT_PCT": Free throw success percent
# "OREB": Offensive rebounds
# "DREB": Defensive rebounds
# "REB": Rebounds
# "AST": Assists
# "STL": Steals
# "BLK": Blocks
# "TOV": Turnovers
# "PTS": Points
# "EFF": Efficiency


#### Position-Based Comparisons ####

# If we want to incorporate their positions into the dataset, 
# we need to get that information via scraping

# Centers, Forwards, and Guards
urlCodes = c("C", "F", "G")

extractedPlayerStats = data.frame()
playerStats = data.frame()

for (i in 1:length(urlCodes)) {
  
  tempURL <- paste0("https://stats.nba.com/players/traditional/?PlayerPosition=", urlCodes[i], "&sort=PTS&dir=-1&SeasonType=Regular%20Season")
  rd$open(silent = TRUE)
  rd$navigate(tempURL)
  
  playerPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
  
  visibleHeaders_Player <- getHeaders(playerPage)
  
  tempStats <- getData(playerPage, visibleHeaders_Player) %>% 
    mutate("POSITION" = urlCodes[i])
  
  playerStats = rbind(playerStats, tempStats)

  # This dataset needs to be joined to playerData via the player names so that positions can be associated with the name
}

playerStats <- playerStats %>% 
  as.data.table(.) %>% 
  arrange(desc(PTS))

visibleHeaders = getHeaders(playerPage)
visibleHeaders = c(visibleHeaders, "POSITION")

