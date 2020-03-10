library(tidyverse)
library(jsonlite)
library(rvest)
library(RSelenium)
library(wdman)
library(httr)
library(data.table)

# server <- chrome(port = 4567L, version = "80.0.3987.106", verbose = FALSE)
# rd <- remoteDriver(browserName = "chrome", port = 4567L)
# shootingURL <- "https://stats.nba.com/players/shooting/"
# shootingURL = "https://stats.nba.com/players/shooting/?Season=2019-20&SeasonType=Regular%20Season"
# rd$open(silent = TRUE)
# rd$navigate(shootingURL)

# functions
getHeaders = function(webpage) {
  
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

getData = function(webpage, visibleHeaders) {
  
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

# trial 1 works for 1 year
# loop for different page
# 
# shootingPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
# # temp
# visibleHeaders_shooting <- getHeaders(shootingPage)
# # temp
# # shootingStats <- getData(shootingPage, visibleHeaders_shooting)
# shootingStats = data.frame()
# 
# for(i in 1:page(shootingPage)){
#   shootingPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
#   # temp
#   # temp_visibleHeaders_shooting <- getHeaders(shootingPage)
#   temp_shootingStats <- getData(shootingPage, visibleHeaders_shooting)
#   shootingStats = rbind(shootingStats, temp_shootingStats)
#   nextbutton<-rd$findElement("css","a.stats-table-pagination__next")
#   nextbutton$clickElement()
#   Sys.sleep(5)
# }
# 
# # trial 2
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
  visibleHeaders_shooting <- getHeaders(shootingPage)
  
  for (j in 1:page(shootingPage)){
    shootingPage <- rd$getPageSource() %>% str_flatten() %>% read_html()
    temp_shootingStats <- getData(shootingPage, visibleHeaders_shooting)
    temp_shootingStats_year = temp_shootingStats %>% mutate("YEAR" = year[i])
    shootingStats = rbind(shootingStats, temp_shootingStats_year)
    nextbutton<-rd$findElement("css","a.stats-table-pagination__next")
    nextbutton$clickElement()
    Sys.sleep(5)
  }
  
  shootingStats_year = shootingStats
  Sys.sleep(3)
}
