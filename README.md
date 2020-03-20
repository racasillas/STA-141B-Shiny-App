# STA-141B-Shiny-App

### Intro:

The purpose of our application is to visualize NBA data to help contextualize trends and offer insights into player behavior based on
the position that they play. 

This repo contains an `app.R` file which is the primary application that we use. It works functionally 
as a stand-alone product by calling .txt files in from another repository of a group member, Aakash. However, shinyapps.io wasn't compatible
with the app due to the use of `shinydashboardplus`, so we stripped the app UI down a bit to lessen its complexity which can be found at 
`uglyApp.R.`  

The other files included in this repo are: `getNBAdat.R` (the script to scrape NBA data), `players.txt`, `shots.txt`, and `teams.txt`
(which are txt files from the web-scraped data to avoid rescraping every time the app is run).  

### Basics of the App:  

The app itself is made up of two tabs:   

1) The first tab offers in-depth visualization and filtering of our player database. The sidebar of this tab allows 
for many filtering components to search for things such as "players who are good shooters", "players who are good defenders", etc.
to better understand what type of players excel in each component of an NBA game. Its data is grouped by 
position-played for each player to help visualize distinct differences in the play-style of each position. It also offers
two summary tables to quantitatively express the average statistics of the filtered data. Its visualizations include a 
scatterplot (showing the points, rebounds, and assists of players), a pie chart (showing position representation in the filtered 
data), and a bar chart (showing team representation in the filtered data). Additionally, each color on the bar chart represents the
corresponding teams' primary color.

2) The second tab is moreso about the shooting characteristics of players over time. Its sidebar component offers the ability to 
filter the data by season and by player. The player options are reactive to the year selected, and are sorted by average field goal 
attempts per game. The body of this tab is composed of a summary row of the player, a bar chart of his shooting percentages and shot 
attempts by range, a table of his shooting percentages, and a timeseries plot of his average shot attempts per game by season played.  





