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


##### Set up Dynamic Scraping #####

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

extractedStats_Team <- getData(teamPage, visibleHeaders_Team)



##### Player Data #####


# We can easily get a lot of player stats from this JSON
playerURL <- "https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=PerGame&Scope=S&Season=2019-20&SeasonType=Regular+Season&StatCategory=PTS"

# If totals are desired instead of per game stats, use this
# playerURL <- "https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=Totals&Scope=S&Season=2019-20&SeasonType=Regular+Season&StatCategory=PTS"

playerData <- fromJSON(playerURL, simplifyDataFrame = TRUE)$resultSet$rowSet %>% data.frame(stringsAsFactors = FALSE)

names(playerData) <- fromJSON(playerURL, simplifyDataFrame = TRUE)$resultSet$headers

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
urlCodes <- c("C", "F", "G")

extractedPlayerStats <- data.frame()

# For each position, get their stats
for (i in 1:length(urlCodes)) {
  
  # Open the page
  tempURL <- paste0("https://stats.nba.com/players/traditional/?PlayerPosition=", urlCodes[i], "&sort=PTS&dir=-1&SeasonType=Regular%20Season")
  rd$open(silent = TRUE)
  rd$navigate(tempURL)
  
  # Wait a while
  Sys.sleep(5)
  
  playerPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
  
  # Get the headers
  visibleHeaders_Player <- getHeaders(playerPage)
  
  # Number of pages
  numPages <- playerPage %>% html_node("div.stats-table-pagination__info") %>% html_text() %>% str_split("of ") %>% unlist()
  numPages <- numPages[length(numPages)] %>% trimws() %>% as.numeric()
  
  # If numPages > 1, there's a "next" button that will need to be clicked
  if (numPages > 1) {
    nextButton <- rd$findElement("css selector", "a.stats-table-pagination__next")
  }
  
  for (j in 1:numPages) {
    
    if (j > 1) {
      nextButton$clickElement()
      Sys.sleep(5)
      playerPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
      nextButton <- rd$findElement("css selector", "a.stats-table-pagination__next")
    }
    
    extractedStats_Player <- getData(playerPage, visibleHeaders_Player)
    extractedStats_Player <- extractedStats_Player %>% mutate("POSITION" = urlCodes[i])
    
    extractedPlayerStats <- rbind(extractedPlayerStats, extractedStats_Player)
    
  }
  
}

# "POSITION" was created in the loop, so it should be added to visibleHeaders_Player
visibleHeaders_Player <- c(visibleHeaders_Player, "POSITION")


# This dataset needs to be joined to playerData 
# We can use the player names so that positions can be associated with the name
# We'll use left_join because extractedPlayerStats is much bigger than playerData

# First, we have to change the name of RANK because it's not the same as playerData's RANK
extractedPlayerStats <- extractedPlayerStats %>% rename(POS.RANK = RANK)

# Next, we need to convert columns that are classified as matricies into character classes
for (i in 1:ncol(extractedPlayerStats)) {
  extractedPlayerStats[[i]] <- extractedPlayerStats[[i]] %>% as.character()
}

# making new df so that we don't modify extractedPlayerStats after scraping so that we can 
# adjust as we need to without rescraping
playerStats <- extractedPlayerStats %>% 
  
  left_join(., playerData) %>% 
  
  # modifying a few cols that had 2 cols representing same value & to fill in NAs
  mutate(FG3_PCT = case_when(
    is.null(X3P.) ~ as.numeric(FG3_PCT) * 100,
    TRUE ~ as.numeric(X3P.))) %>% 
  
  mutate(`3PA` = case_when(
    is.null(X3PA) ~ as.numeric(FG3A),
    TRUE ~ as.numeric(X3PA))) %>% 
  
  mutate(`3PM` = case_when(
    is.null(X3PM) ~ as.numeric(FG3M),
    TRUE ~ as.numeric(X3PM))) %>% 
  
  mutate(FG_PCT = case_when(
    is.null(`FG.`) ~ as.numeric(FG_PCT) * 100,
    TRUE ~ as.numeric(`FG.`))) %>%
  
  mutate(FT_PCT = case_when(
    is.null(`FT.`) ~ as.numeric(FT_PCT) * 100,
    TRUE ~ as.numeric(`FT.`))) %>%
  
  # mutating numerics
  mutate_at(vars(-PLAYER, -TEAM, -POSITION), funs(as.numeric)) %>% 
  
  # making some point breakdowns for each player
  # 2PA and 2PM are two-pointers shot & made
  # 2(3)PT_ATT_PER calculates their 2(3) point attempts per game
  # PER_PTS_XX calculates the % of points per game from FT, 3P, and 2P 
  mutate(
    `2PA` = FGA - `3PA`,
    `2PM` = FGM - `3PM`,
    `FG2_PCT` = round((`2PM`/`2PA`)*100, 2), 
    `2PT_ATT_PER` = round((`2PA`/FGA)*100, 2),
    `3PT_ATT_PER` = round((`3PA`/FGA)*100, 2),
    PER_PTS_FT = round((FTM/PTS)*100, 2),
    PER_PTS_3P = round(((`3PM`*3)/PTS)*100, 2), 
    PER_PTS_2P = round(((`2PM`*2)/PTS)*100, 2)
    
  ) %>% 
  
  mutate(
    POSITION =
      case_when(
        POSITION == "G" ~"Guard", 
        POSITION == "C" ~"Center",
        TRUE ~"Forward")
  ) %>% 
  
  # sorting in a regular order, omitting duplic cols
  select(POS.RANK, PLAYER, TEAM, AGE, GP, W, L, MIN, PTS, FGM, FGA, FG_PCT,
         `2PA`, `2PM`, `FG2_PCT`, `3PA`, `3PM`, FG3_PCT, `2PT_ATT_PER`, `3PT_ATT_PER`, 
         FTM, FTA, FT_PCT, PER_PTS_FT, PER_PTS_2P, PER_PTS_3P, OREB, DREB, 
         REB, AST, TOV, STL, BLK, PF, FP, DD2, TD3, X..., POSITION, PLAYER_ID, RANK) %>% 
  
  arrange(desc(PTS))



cols <- RColorBrewer::brewer.pal(length(unique(playerStats$POSITION)), name = "Dark2")


playerStats <- playerStats %>% 
  mutate(color = factor(playerStats$POSITION, labels = cols))


require(teamcolors)
pal <- league_pal("nba") %>% 
  as.data.frame() %>% 
  mutate(
    TEAM= 
      c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
        "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
        "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"
      )
  ) %>% 
  rename(team_color = ".")

playerStats <- merge(playerStats, pal, by = "TEAM")

##### part 2 #####
# functions
getHeaders2 = function(webpage) {
  
  # The table headings
  # Not all of them are visible though
  tableHeader <- webpage %>% html_nodes("div.nba-stat-table__overflow") %>% html_nodes("table") %>% html_nodes("thead") %>% html_nodes("tr")
  visibleHeaders <- list()
  
  if (length(tableHeader) == 0) {
    return(visibleHeaders)
  }
  
  # To extract the correct nodes, the ones that don't have "class" tags are selected
  for (i in 1:length(tableHeader)) {
    if (is.na(tableHeader[[i]] %>% html_attr("class")) ) {
      visibleHeaders[[length(visibleHeaders) + 1]] <- tableHeader[[i]] %>% html_nodes("th") %>% html_text() %>% str_trim()
    }
  }
  
  # The names of the accessible data
  visibleHeaders <- unlist(visibleHeaders)
  
  return(visibleHeaders)
  
}

getData2 = function(webpage, visibleHeaders) {
  
  # A list of the actual data values, mixed in with extra elements
  stats <- webpage %>% html_nodes("div.nba-stat-table__overflow") %>% html_nodes("tbody") %>% html_nodes("tr")
  
  # More filtering is needed
  extractedStats <- list()
  
  # The valid rows have an "index" attribute and that do not have "class" tags 
  for (i in 1:length(stats)) {
    if (is.na(stats[[i]] %>% html_attr("class")) && !is.na(stats[[i]] %>% html_attr("index"))) {
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

# extract page number
page = function(webpage){
  pagenum = webpage %>% html_node("div.stats-table-pagination__info") %>% html_text() %>% str_split("of ") %>% unlist()
  pagenum = pagenum[length(pagenum)] %>% trimws() %>% as.numeric()
  return(pagenum)
}

# call server
server <- chrome(port = 1234L, version = "80.0.3987.106", verbose = FALSE)
rd <- remoteDriver(browserName = "chrome", port = 1234L)

shootingStats_year = data.frame()
shootingStats = data.frame()
year = c("2019-20","2018-19","2017-18","2016-17","2015-16")

for (i in 1:length(year)){
  shootingURL <- paste0("https://stats.nba.com/players/shooting/?Season=",year[i],"&SeasonType=Regular%20Season")
  rd$open(silent = TRUE)
  rd$navigate(shootingURL)
  # adjust accordingly to computer run-time
  Sys.sleep(15)
  shootingPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
  visibleHeaders_shooting <- getHeaders2(shootingPage)
  
  for (j in 1:page(shootingPage)){
    shootingPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
    temp_shootingStats <- getData2(shootingPage, visibleHeaders_shooting)
    temp_shootingStats_year = temp_shootingStats %>% mutate("YEAR" = year[i])
    shootingStats = rbind(shootingStats, temp_shootingStats_year)
    nextbutton<-rd$findElement("css","a.stats-table-pagination__next")
    nextbutton$clickElement()
    Sys.sleep(15)
  }
  
  shootingStats_year = shootingStats
  Sys.sleep(15)
}