library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(RSelenium)
library(wdman)
library(glue)


if (interactive()) {
  # making the header
  # there's a lot more we can do with the header, but this is just the basic format
  header <-
    dashboardHeaderPlus(
      title = tagList(
        span(
          class = "logo-lg", "NBA Visualization"
        ), # to remove text when collapsing sidebar
        img(src = "")
      ),
      titleWidth = 250
    ) # we can make an image in the header if we want to (like the NBA or something)
  
  # making the sidebar
  sidebar <- dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      
      # create first menu item (aka panel on the left)
      menuItem(
        "Players",
        # expandedName is used to call on later and make the output
        expandedName = "tab1",
        startExpanded = TRUE,
        # icon that displays when sidebar is collapsed
        icon = icon("basketball-ball"),
        tags$style(".fa-basketball-ball {color:#b14f43}"),
        # changing color of icons
        conditionalPanel(
          # show this panel when the tab is expanded
          "input$tabs == 'tab'",
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
                "Has played 75% of possible games",
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
          
          # trigger mechanism so we don't constantly load new data on each input change
          actionButton(
            inputId = "update",
            label = "Update Data",
            icon = icon("rocket")
          )
        ) # end conditional panel
      ), # end first menu item
      
      # makes the second "tab"
      menuItem(
        "Shooting Statistics",
        expandedName = "tab2",
        icon = icon("user-friends"),
        tags$style(".fa-user-friends {color:#3333f3}"),
        selectInput(
          inputId = "year",
          label = "Year: ",
          choices = c(unique(shootingStats_year$YEAR)),
          multiple = FALSE
        ),
        selectInput(
          inputId = "player_name", 
          label = "Player Name",
          choices = sort(unique(shootingStats_year$Player)),
          multiple = FALSE
        ),
        checkboxGroupInput(inputId = "checkBox",
                           label = "choose range: ",
                           choices = c("CLOSE", "MID", "FAR"))
      ) # end second tab
    )) # end sidebar creation
  
  # creating the body
  body <- dashboardBody(
    
    uiOutput("body"),
    
    # all of this is just color customization for the header
    # it can be whatever we want this is just filler for now
    tags$head(tags$style(
      HTML(
        "
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #4f6369;
                            }
                            
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #1d3b45;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #1d3b45;
                            }
                            
                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #1d3b45;
                            }
                            "
      )
    ))
  ) # end color customization
  
  
  shinyApp(
    # making the UI a component of the header we created earlier, the sidebar we created earlier, and the body created below
    ui = 
      
      dashboardPagePlus(header,
                        sidebar,
                        body,
                        skin = "blue"
      ),
    
    server = function(input, output, session) {
      # suppress warnings  
      storeWarn<- getOption("warn")
      options(warn = -1) 
      # body reactivity to input$tabs
      output$body <- renderUI({
        # if there is no tab selected, show this message
        if (is.null(input$sidebarItemExpanded)) {
          fluidRow(box(
            h4(
              "Please choose a tab (or whatever message we want to put here)"
            ),
            width = 12
          ))
        }
        # render this output when they choose the first tab
        else if (input$sidebarItemExpanded == "tab1") {
          wellPanel(
            # begin first fluid row, fluidRows define horizontal output
            fluidRow(
              column(
                9,
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Scatter Plot of Rebounds and Assists Relationship, Sized by Points ",
                  # any output can go in a box, but in this case I'm using plotlyOutput just to show it can show a graph
                  plotlyOutput("scatter"),
                  width = 12,
                  height = "475px"
                )
              ),
              column(
                3,
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Summary Stats by Position",
                  # div(...) is another example of output we can do in a box
                  div(
                    style = "overflow-x:scroll",
                    DT::dataTableOutput("summarydata")
                  ),
                  # max width = 12, so since this is 12 it will span the whole body
                  width = 12,
                  height = "475px"
                ) # end box
              )
            ), # end first fluid row
            # begin second fluidRow
            fluidRow(
              column(
                6,
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Position Representation",
                  # any output can go in a box, but in this case I'm using plotlyOutput just to show it can show a graph
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
                  # any output can go in a box, but in this case I'm using plotlyOutput just to show it can show a graph
                  plotlyOutput("histogram"),
                  width = 12,
                  height = "475px"
                )
              )
            ),
            # begin third fluid row
            fluidRow(
              box(
                # example of how to change box colors
                # just change the #82382f to whatever color you want
                tags$style(
                  HTML(
                    "
                      .box.box-solid.box-primary>.box-header {
                      color:#fff;
                      background:#82382f
                      }
                      .box.box-solid.box-primary{
                      border-bottom-color:#82382f;
                      border-left-color:#82382f;
                      border-right-color:#82382f;
                      border-top-color:#82382f;
                      }"
                  )
                ),
                solidHeader = TRUE,
                status = "primary",
                title = "Filtered Data",
                # div(...) is another example of output we can do in a box
                div(
                  style = "overflow-x:scroll",
                  DT::dataTableOutput("dataexample")
                ),
                # max width = 12, so since this is 12 it will span the whole body
                width = 12
              ) # end box
            ) # end third fluid row
          ) # end well panel
        }
        
        # if users click second tab, this is the output that will be shown
        else if (input$sidebarItemExpanded == "tab2") {
          # wellPanel creates the backing to contain all the fluidRow arguments
          wellPanel(
            # begin fluidrow
            fluidRow(
              infoBox(
                "Player's Name", uiOutput("orderNum"), width = 12
              )
            ),
            fluidRow(
              # begin column of fluid row
              box(title = "# of Different-Range Shooting",
                  width = 12,
                  # status = "warning", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("plot", height = 250)
              ),
            ), # end fluidrow
            
            # begin next fluidRow, mainly used to show how we can have different horizontal and vertical sections for output
            fluidRow(
              dataTableOutput("checkbox")
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
            if ("Has played 75% of possible games" %in% c(input$threshold) == TRUE) {
              GP > quantile(playerStats$GP, .75)
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
          arrange(desc(PTS))
        
        DT::datatable(
          data = dataFilt,
          escape = FALSE,
          options = list(
            lengthMenu = c(10, 10, -1),
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
                               "Player: ",  tempDat$PLAYER,
                               "<br>Points: ",  tempDat$PTS,
                               "<br>Team: ",  tempDat$TEAM
                             ),
                             hoverinfo = paste("Player", tempDat$PLAYER),
                             type = 'scatter', 
                             mode = 'markers',
                             marker = list(color = ~color,
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
            line = list(color = '#FFFFFF', width = 1)
          ),
          textinfo = "label+percent",
          insidetextfont = list(color = "#FFFFFF")
        )
      )
      dataHist <- eventReactive(
        input$update, {
          temp <- playerData() %>% 
            group_by(TEAM) %>% 
            # n_distinct to avoid repeats of players if they play > 1 position
            summarize(n = n_distinct(PLAYER),
                      col = unique(team_color, TEAM))
          temp$TEAM <- reorder(temp$TEAM, temp$n)
          temp
          
        })
      
      output$histogram <- renderPlotly(
        plot_ly(dataHist(),
                x = ~TEAM, 
                y = ~n, 
                type = "bar", 
                marker = list(color = ~col)) %>% 
          layout(
            yaxis =  list(title = "Number of Players")
          )
      ) 
      ### end of making the output for the box with the time series ###
      
      
      
      ### below this is the output for the second tab ###
      
      # reactive to the second slider
      output$orderNum <- renderText({
        input$player_name
      })
      
      # reactive to the second slider
      output$plot <- renderPlotly({
        data = shootingStats_year %>% 
          filter(YEAR == input$year) %>%
          filter(Player == input$player_name)
        if (nrow(data) == 0){
          return(NULL)
        }
        data %>% plot_ly(x = ~c("LESS THAN 5FT.", "5-9 FT.", "10-14 FT.", "15-19 FT.", "20-24 FT.", "25-29 FT."),
                         y = ~c(FG., FG..1, FG..2, FG..3, FG..4, FG..5), 
                         type = 'bar') %>%
          layout(title = 'Range Bar Chart',
                 xaxis = list(title = 'Distance'),
                 yaxis = list(title = '%'))
      })
      
      # reactive 
      output$checkbox = renderDataTable({
        dataset = filter(shootingStats_year, YEAR == input$year) %>% filter(Player == input$player_name)
        if (nrow(dataset) == 0){
          return(NULL)
        }
        dataset %>%
          summarize(
            CLOSE = (FGM+FGM.1)/(FGA+FGA.1),
            MID = (FGM.2+FGM.3)/(FGA.2+FGA.3),
            FAR = (FGM.4+FGM.5)/(FGA.4+FGA.5)
          ) %>%
          select(c(input$checkBox))
      }, options = list(paging = F, searching = F))
    } # close out server
  ) # close out shinyApp()
}