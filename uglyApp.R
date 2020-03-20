library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(glue)
library(data.table)
library(teamcolors)
library(shinyWidgets)

## Getting the Data


# URLs for getting the player and team data from GitHub

playerURL <- "https://raw.githubusercontent.com/A10theHero/technicallyADatabase/master/players.txt"

teamURL <- "https://raw.githubusercontent.com/A10theHero/technicallyADatabase/master/teams.txt"

shootingURL <- "https://raw.githubusercontent.com/A10theHero/technicallyADatabase/master/shots.txt"


###### Function #####

# Extracts from a URL

rawExtract <- function(url) {


  # Read in the raw file and remove the tab characters
  # Note: This introduces a new column that's just numbered according to the number of rows
  dat <- read_lines(url) %>% str_split(fixed("\t"))


  # Save the column names to a separate variable
  # Note: \" is still in these variables, but knowing their length is important
  dat.names <- dat[[1]]


  # Next, remove the \" characters
  dat <- dat %>%
    unlist() %>%
    str_remove_all('\"')


  # Update the colnames and remove them from dat
  dat.names <- dat[1:length(dat.names)]
  dat <- dat[(length(dat.names) + 1):length(dat)]


  # Convert the data into a matrix, with ncol = length(dat.names) + 1 (for the extra column introduced earlier)
  dat <- dat %>% matrix(ncol = length(dat.names) + 1, byrow = TRUE)


  # Remove that extra column
  dat <- dat[, 2:ncol(dat)]


  # Add the column names to dat
  colnames(dat) <- dat.names


  # Return the matrix as a data frame
  return(data.frame(dat, stringsAsFactors = FALSE))
}



##### Running the Function #####


playerStats <- rawExtract(playerURL)

# Next, we need to convert columns that are classified as matricies into character classes
for (i in 1:ncol(playerStats)) {
  playerStats[[i]] <- playerStats[[i]] %>% as.character()
}


# making new df so that we don't modify extractedPlayerStats after scraping so that we can
# adjust as we need to without rescraping
playerStats <- playerStats %>%
  # mutating numerics
  mutate_at(vars(-PLAYER, -TEAM, -POSITION, -color, -team_color), funs(as.numeric)) %>%
  rename(
    `2PA` = `X2PA`,
    `2PM` = `X2PM`,
    `3PA` = `X3PA`,
    `3PM` = `X3PM`,
    `2PT_ATT_PER` = `X2PT_ATT_PER`,
    `3PT_ATT_PER` = `X3PT_ATT_PER`
  )

playerStats <- playerStats %>%
  # sorting in a regular order, omitting duplic cols
  select(
    POS.RANK, PLAYER, TEAM, AGE, GP, W, L, MIN, PTS, FGM, FGA, FG_PCT,
    `2PA`, `2PM`, `FG2_PCT`, `3PA`, `3PM`, FG3_PCT, `2PT_ATT_PER`, `3PT_ATT_PER`,
    FTM, FTA, FT_PCT, PER_PTS_FT, PER_PTS_2P, PER_PTS_3P, OREB, DREB,
    REB, AST, TOV, STL, BLK, PF, FP, DD2, TD3, X..., POSITION, PLAYER_ID, RANK
  ) %>%

  arrange(desc(PTS))



cols <- RColorBrewer::brewer.pal(length(unique(playerStats$POSITION)), name = "Dark2")


playerStats <- playerStats %>%
  mutate(color = factor(playerStats$POSITION, labels = cols))


require(teamcolors)
pal <- league_pal("nba") %>%
  as.data.frame() %>%
  mutate(
    TEAM =
      c(
        "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
        "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
        "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"
      )
  ) %>%
  rename(team_color = ".")

playerStats <- merge(playerStats, pal, by = "TEAM")



teamStats <- rawExtract(teamURL)


shootingStats <- rawExtract(shootingURL)

shootingStats <- shootingStats %>%
  mutate_at(vars(-Player, -TEAM, -YEAR), funs(as.numeric))




# making the sidebar
# it uses menu items to make separate "tabs"
sidebar <-
  sidebarPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel("Players",
        icon = icon("basketball-ball")
      ),

      tabPanel("Shooting Stats",
        icon = icon("user-friends")
      )
    ),
    width = 250
  )

header <- headerPanel("NBA Visualization")


# creating the body
body <- uiOutput("body")


shinyApp(
  # making the UI a component of the header we created earlier, the sidebar we created earlier, and the body created below
  ui <- fluidPage(header,
    sidebar,
    body
  ),
  server = function(input, output, session) {

      
    # suppress warnings
    storeWarn <- getOption("warn")
    options(warn = -1)
    # body reactivity to input$tabs
    output$body <- renderUI({
      # render this output when they choose the first tab
      if (input$tabs == "Players") {
        sidebarLayout(
          sidebarPanel(width = 2,
            pickerInput(
              inputId = "xcol",
              label = "Choose teams",
              multiple = T,
              choices = sort(unique(playerStats$TEAM)),
              selected = playerStats$TEAM,
              options = list(
                `selected-text-format` = "count > 1",
                `actions-box` = TRUE
              ),
              choicesOpt = list(
                style = rep(("color: black; background: white;"), 100)
              )
            ),

            pickerInput(
              inputId = "good_shooter",
              label = "Filter for good shooters",
              multiple = TRUE,
              choices = list(
                `Overall Shooting` = c(
                  "Has above average FG%",
                  "Shoots more than avg"
                ),
                `3-point Specialists` = c(
                  "Shoots above avg % from 3",
                  "Shoots more 3s than avg"
                ),
                `2-point Specilists` = c(
                  "Shoots above avg % from 2",
                  "Shoots more 2s than avg"
                )
              ),
              selected = NULL,
              # allowing ability to select all and also rendering nicer ui "you've selected 3 things" instead of a list
              options = list(
                `selected-text-format` = "count > 1",
                `actions-box` = TRUE
              ),
              choicesOpt = list(
                style = rep(("color: black; background: white;"), 100)
              )
            ),
            pickerInput(
              inputId = "high_avgs",
              label = "Filter for elite performers",
              multiple = TRUE,
              choices = list(
                `Is in the 95th percentile for:` = c(
                  "Points",
                  "Rebounds",
                  "Assists",
                  "Steals",
                  "Blocks"
                )
              ),
              selected = NULL,
              options = list(
                `selected-text-format` = "count > 1",
                `actions-box` = TRUE
              ),
              choicesOpt = list(
                style = rep(("color: black; background: white;"), 100)
              )
            ),
            pickerInput(
              inputId = "threshold",
              label = "Filter for threshold check",
              multiple = TRUE,
              choices = list(
                `Thresholds` = c(
                  "Has played 50% of possible games",
                  "Team wins at least 50% of the games when player plays"
                )
              ),
              selected = NULL,
              options = list(
                `selected-text-format` = "count > 1",
                `actions-box` = TRUE
              ),
              choicesOpt = list(
                style = rep(("color: black; background: white;"), 100)
              )
            ),

            actionButton("update",
              label = "Update Data",
              icon = icon("rocket")
            )
          ),
          mainPanel(width = 10,
            # begin first fluid row, fluidRows define horizontal output
            fluidRow(
                column(9,
              box(
                solidHeader = TRUE,
                status = "primary",
                title = "Scatter Plot of Rebounds and Assists Relationship, Sized by Points ",
                plotlyOutput("scatter"),
                width = 12,
                height = "475px"
              )),
              column(
                  3,
                  box(
                      solidHeader = TRUE,
                      status = "primary",
                      title = "Summary Stats by Position",
                      # div(...) is another example of output we can do in a box
                      div(
                          DT::dataTableOutput("summarydata")
                      ),
                      # max width = 12, so since this is 12 it will span the whole body
                      width = 12,
                      height = "475px"
                  ) # end box
              )
            ),
            fluidRow(
              column(
                6,
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Position Representation",
                  plotlyOutput("piechart"),
                  width = 12,
                  height = "475px"
                )
              ),
              column(
                  6,
                  box(
                      solidHeader = TRUE,
                      status = "primary",
                      title = "Team Representation",
                      plotlyOutput("histogram"),
                      width = 12,
                      height = "475px"
                  )
              )

            ),
            # begin third fluid row
            fluidRow(
              box(
                solidHeader = TRUE,
                status = "primary",
                title = "Filtered Data",
                # div(...) is another example of output we can do in a box
                div(
                  DT::dataTableOutput("dataexample")
                ),
                # max width = 12, so since this is 12 it will span the whole body
                width = 12
              ) # end box
            )
          )
        ) # end third fluid row
      }

      # if users click second tab, this is the output that will be shown
      else if (input$tabs == "Shooting Stats") {
        # wellPanel creates the backing to contain all the fluidRow arguments
        sidebarLayout(
          sidebarPanel(
            selectInput("year",
              label = "Year:",
              choices = c(unique(shootingStats$YEAR)),
              multiple = FALSE
            ),

            selectInput("players", "Choose a Player:",
              choices = "",
              selected = c("2019-20")
            ),

            checkboxGroupInput(
              inputId = "checkBox",
              label = "Choose Shooting Range:",
              choices = c("Close Range", "Mid Range", "Deep Shot")
            ),

            actionButton(
              inputId = "update2",
              label = "Update Data",
              icon = icon("rocket")
            )
          ),
          # begin fluidrow
          mainPanel(
            fluidRow(
              infoBox(
                "Name",
                textOutput("playerNam"),
                width = 4,
                icon = icon("user"),
                color = "orange"
              ),
              infoBox(
                "Team",
                textOutput("playerTeam"),
                width = 4,
                icon = icon("users"),
                color = "olive"
              ),
              infoBox(
                "Shots Per Game",
                textOutput("playerShots"),
                width = 4,
                icon = icon("user-secret"),
                color = "blue"
              )
            ),
            fluidRow(
              # begin column of fluid row
              box(
                title = "",
                width = 12,
                # status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("shooting_plot")
              )
            ), # end fluidrow

            # begin next fluidRow
            fluidRow(
              dataTableOutput("shooting_percentages")
            )
          )
        )
      }
    }) # end output$body


    ## this is where the checkboxes come into play ##
    playerData <- eventReactive(input$update, {
      teams_selected <- as.list(input$xcol)
      playerStats %>%
        filter(TEAM %in% teams_selected) %>%
        filter(
          if ("Shoots above avg % from 3" %in% c(input$good_shooter) == TRUE) {
            FG3_PCT > sum(playerStats$`3PM`) / sum(playerStats$`3PA`) * 100
          }
          else {
            FG3_PCT == FG3_PCT
          }
        ) %>%
        filter(
          if ("Shoots more 3s than avg" %in% c(input$good_shooter) == TRUE) {
            `3PA` > mean(playerStats$`3PA`)
          }
          else {
            `3PA` == `3PA`
          }
        ) %>%
        filter(
          if ("Shoots above avg % from 2" %in% c(input$good_shooter) == TRUE) {
            FG2_PCT > sum(playerStats$`2PM`) / sum(playerStats$`2PA`) * 100
          }
          else {
            # FG2_PCT == FG2_PCT would remove some players because of NAs for FG2_PCT
            PLAYER == PLAYER
          }
        ) %>%
        filter(
          if ("Shoots more 2s than avg" %in% c(input$good_shooter) == TRUE) {
            `2PA` > mean(playerStats$`2PA`)
          }
          else {
            `2PA` == `2PA`
          }
        ) %>%
        filter(
          if ("Has above average FG%" %in% c(input$good_shooter) == TRUE) {
            FG_PCT > sum(playerStats$FGM) / sum(playerStats$FGA) * 100
          }
          else {
            FG_PCT == FG_PCT
          }
        ) %>%
        filter(
          if ("Shoots more than avg" %in% c(input$good_shooter) == TRUE) {
            FGA > mean(playerStats$FGA)
          }
          else {
            FGA == FGA
          }
        ) %>%
        filter(
          if ("Points" %in% c(input$high_avgs) == TRUE) {
            PTS >= quantile(playerStats$PTS, .95)
          }
          else {
            PTS == PTS
          }
        ) %>%
        filter(
          if ("Rebounds" %in% c(input$high_avgs) == TRUE) {
            REB >= quantile(playerStats$REB, .95)
          }
          else {
            REB == REB
          }
        ) %>%
        filter(
          if ("Assists" %in% c(input$high_avgs) == TRUE) {
            AST >= quantile(playerStats$AST, .95)
          }
          else {
            AST == AST
          }
        ) %>%
        filter(
          if ("Steals" %in% c(input$high_avgs) == TRUE) {
            STL >= quantile(playerStats$STL, .95)
          }
          else {
            STL == STL
          }
        ) %>%
        filter(
          if ("Blocks" %in% c(input$high_avgs) == TRUE) {
            BLK >= quantile(playerStats$BLK, .95)
          }
          else {
            BLK == BLK
          }
        ) %>%
        filter(
          if ("Has played 50% of possible games" %in% c(input$threshold) == TRUE) {
            GP > quantile(playerStats$GP, .50)
          }
          else {
            GP == GP
          }
        ) %>%
        filter(
          if ("Team wins at least 50% of the games when player plays" %in% c(input$threshold) == TRUE) {
            W / GP > .5
          }
          else {
            GP == GP
          }
        )
    })



    output$dataexample <- DT::renderDataTable({
      dataFilt <- playerData() %>%
        rename(
          `FG%` = FG_PCT, `2PT FG%` = FG2_PCT, `3PT FG%` = FG3_PCT,
          `FT%` = FT_PCT, `PPG% FROM FT` = PER_PTS_FT,
          `FGA% FROM 2` = `2PT_ATT_PER`, `FGA% FROM 3` = `3PT_ATT_PER`,
          `PPG% FROM 2` = PER_PTS_2P, `PPG% FROM 3` = PER_PTS_3P
        ) %>%
        filter(TEAM %in% as.list(input$xcol)) %>%
        arrange(desc(PTS)) %>%
        select(-`X...`, -`color`, -`team_color`)

      DT::datatable(
        data = dataFilt,
        escape = FALSE,
        options = list(
          scrollY = "250px",
          scrollX = TRUE
        ),
        class = "cell-border stripe"
      )
    })


    dataFilt2 <- eventReactive(input$update, {
      mydf <- playerData() %>%
        group_by(POSITION) %>%
        summarize(
          `N` = n(),
          `Points` = round(mean(PTS), 2),
          `Rebounds` = round(mean(REB), 2),
          `Assists` = round(mean(AST), 2),
          `Steals` = round(mean(STL), 2),
          `Blocks` = round(mean(BLK), 2),
          `FT%` = round(sum(FTM) / sum(FTA) * 100, 2),
          `FG%` = round(sum(FGM) / sum(FGA) * 100, 2),
          `2Pt%` = round(sum(`2PM`) / sum(`2PA`) * 100, 2),
          `3Pt%` = round(sum(`3PM`) / sum(`3PA`) * 100, 2),
          `PPG% From FTs` = round(sum(`FTM`) / sum(PTS) * 100, 2),
          `PPG% From 2s` = round(sum(`2PM` * 2) / sum(PTS) * 100, 2),
          `PPG% From 3s` = round(sum(`3PM` * 3) / sum(PTS) * 100, 2)
        )
      tmp <- as.data.frame(t(mydf[, -1]))
      colnames(tmp) <- mydf$POSITION
      as.data.frame(tmp)
    })
    output$summarydata <- DT::renderDataTable({
      DT::datatable(
        data = dataFilt2(),
        escape = FALSE,
        options = list(
          lengthMenu = c(10, 10, -1),
          scrollY = "360",
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        ),
        class = "cell-border stripe"
      )
    })

    output$scatter <- renderPlotly(
      if (nrow(playerData()) > 1) {
        fig <- plot_ly()
        for (i in unique(playerData()$POSITION)) {
          tempDat <- playerData() %>%
            filter(POSITION == i)
          fig <- add_trace(fig,
            data = tempDat,
            y = ~AST,
            x = ~REB,
            size = ~PTS,
            color = ~POSITION,
            text = paste(
              "Player: ", tempDat$PLAYER,
              "<br>Points: ", tempDat$PTS,
              "<br>Team: ", tempDat$TEAM
            ),
            hoverinfo = paste("Player", tempDat$PLAYER),
            type = "scatter",
            mode = "markers",
            marker = list(
              color = ~color,
              line = list(
                color = ~color,
                width = 2
              )
            )
          )
        }

        fig
      }
      else {
        fig <- plot_ly(
          type = "scatter",
          data = playerData(),
          x = ~REB,
          y = ~AST,
          size = ~PTS,
          text = paste(
            "Player: ", playerData()$PLAYER,
            "<br>Points: ", playerData()$PTS,
            "<br>Team: ", playerData()$TEAM
          ),
          hoverinfo = text,
          mode = "markers", marker = list(color = ~color)
        )
      }
    )

    output$piechart <- renderPlotly(
      plot_ly(
        type = "pie",
        data = playerData(),
        labels = ~`POSITION`,
        marker = list(
          colors = ~color,
          line = list(color = "#FFFFFF", width = 1)
        ),
        textinfo = "label+percent",
        insidetextfont = list(color = "#FFFFFF")
      )
    )
    dataHist <- eventReactive(
      input$update,
      {
        temp <- playerData() %>%
          group_by(TEAM) %>%
          # n_distinct to avoid repeats of players if they play > 1 position
          summarize(
            n = n_distinct(PLAYER),
            col = unique(team_color, TEAM)
          )
        temp$TEAM <- reorder(temp$TEAM, temp$n)
        temp
      }
    )

    output$histogram <- renderPlotly(
      plot_ly(dataHist(),
        x = ~TEAM,
        y = ~n,
        type = "bar",
        marker = list(color = ~col)
      ) %>%
        layout(
          yaxis = list(title = "Number of Players")
        )
    )
    ### end of making the output for the box with the time series ###






    ### below this is the output for the second tab ###

    # making the selections only have the info for the year chosen
    observeEvent(input$year, {
      dat <- shootingStats %>%
        mutate(TFGA = FGA + FGA.1 + FGA.2 + FGA.3 + FGA.4 + FGA.5) %>%
        filter(YEAR == input$year) %>%
        as.data.table() %>%
        arrange(desc(TFGA)) %>%
        select(Player)

      if (is.null(input$year)) {
        updateSelectInput(session, "players",
          choices = c("dat$Player")
        )
      }

      else {
        updateSelectInput(session, "players",
          choices = c(dat$Player)
        )
      }
    })


    shootingData <- eventReactive(input$update2, {
      shootingStats %>%
        filter(Player == input$players, YEAR == input$year) %>%
        mutate(TFGA = FGA + FGA.1 + FGA.2 + FGA.3 + FGA.4 + FGA.5)
    })


    output$playerNam <- renderText({
      (shootingData()$Player)
    })

    output$playerTeam <- renderText({
      (shootingData()$TEAM)
    })

    output$playerShots <- renderText({
      (shootingData()$TFGA)
    })

    # reactive to the second slider
    output$shooting_plot <- renderPlotly({
      data <- shootingData()

      data <- data.frame(
        x = c("LESS THAN 5FT.", "5-9 FT.", "10-14 FT.", "15-19 FT.", "20-24 FT.", "25-29 FT."),
        y = c(data$`FG.`, data$`FG..1`, data$`FG..2`, data$`FG..3`, data$`FG..4`, data$`FG..5`),
        z = c(data$`FGA`, data$`FGA.1`, data$`FGA.2`, data$`FGA.3`, data$`FGA.4`, data$`FGA.5`)
      )
      data$x <- factor(data$x, levels = c("LESS THAN 5FT.", "5-9 FT.", "10-14 FT.", "15-19 FT.", "20-24 FT.", "25-29 FT."))

      if (nrow(data) == 0) {
        return(NULL)
      }

      p <- data %>%
        plot_ly(
          x = ~x,
          y = ~y,
          type = "bar",
          text = paste(
            "Shots per game at this range:", data$z,
            "<br>Percentage:", data$y
          ),
          hoverinfo = "text",
          color = ~x
        ) %>%
        layout(
          title = "Shooting Percentages at Each Range (hover to see shot attempts)",
          xaxis = list(title = "Distance"),
          yaxis = list(title = "Shooting %")
        )
    })



    shootingPercentages <- eventReactive(input$update2, {
      dataset <- shootingStats %>%
        filter(YEAR == input$year) %>%
        filter(Player == input$players)
    })

    # reactive
    output$shooting_percentages <- isolate(renderDataTable(
      {
        if (nrow(shootingPercentages()) == 0) {
          return(NULL)
        }
        shootingPercentages() %>%
          summarize(
            `Close Range` = paste0(round((FGM + FGM.1) / (FGA + FGA.1) * 100, 2), "%"),
            `Mid Range` = paste0(round((FGM.2 + FGM.3) / (FGA.2 + FGA.3) * 100, 2), "%"),
            `Deep Shot` = paste0(round((FGM.4 + FGM.5) / (FGA.4 + FGA.5) * 100, 2), "%")
          ) %>%
          select(c(input$checkBox)) %>%
          matrix(nrow = 1, dimnames = list(NULL, c(input$checkBox))) %>%
          data.frame()
      },
      options = list(
        paging = F,
        searching = F,
        bInfo = F
      )
    ))
  } # close out server
) # close out shinyApp()
