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
        "Menu item 2",
        expandedName = "tab2",
        icon = icon("user-friends"),
        tags$style(".fa-user-friends {color:#3333f3}"),
        sliderInput(
          inputId = "slider",
          label = "Some Filter",
          min = 0,
          max = 10,
          value = 0
        ),
        sliderInput(
          inputId = "slider2",
          label = "Some Filter 2",
          min = 0,
          max = 650,
          value = 0
        ),
        selectInput(
          inputId = "progress",
          label = "Progress",
          choices = c(
            "0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,
            "100%" = 100
          )
        )
      ) # end second tab
    )
  ) # end sidebar creation

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
              # begin first column, which will separate the fluidRow into vertical sections
              # since width  = 6, it will be half of the fluidRow
              column(
                6,
                infoBox(
                  "Number of Players", uiOutput("orderNum2"), "Subtitle",
                  icon = icon("users"), width = 12
                ),
                valueBox(
                  uiOutput("orderNum"), "Number of Teams",
                  icon = icon("credit-card"), width = 12,
                  href = "http://google.com"
                ),
                box(
                  title = "Progress example",
                  width = 12,
                  background = "red",
                  uiOutput("progress")
                )
                # the infoBox, valueBox, and box components above this are all in the same column, so they'll be stacked
                # the width of each is 12, so they will be the max width of the column (which is 6, so they'll all be half of the fluidRow)
              ), # end column

              # begin second column of fluid row, which will be the second vertical section
              column(
                6,
                box(
                  title = "Histogram box title",
                  width = 12,
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("plot", height = 275)
                )
              )
            ), # end fluidrow

            # begin next fluidRow, mainly used to show how we can have different horizontal and vertical sections for output
            fluidRow(
              box(
                title = "extra box",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h4("Everything above this is one 'fluidRow'. fluidRow will be how the boxes are grouped horizontally. 
                     The three smaller boxes to the left of the graph are grouped by 1 'column' of the fluidRow, and the
                     graph is the second column of the fluidRow. Columns can separate fluidRows vertically. Most shiny
                     elements have a max width of 12, so you can separate a fluidRow that is length 12 by 2 columns that 
                     are each 6 units wide, as above. The box that this text is in in length 12 so it's as wide as it
                     can be."),
                h4("These are just a few examples of how we can layout the app. Once we have the data, we can draw out 
                     how we want the app to look and which panels we want to have what data, and I can help design it from there")
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
            summarize(n = n(),
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

      # reactive to the first slider
      output$orderNum <- renderText({
        prettyNum(input$slider, big.mark = ",")
      })

      # reactive to the second slider
      output$orderNum2 <- renderText({
        prettyNum(input$slider2, big.mark = ",")
      })

      # reactive to the second slider
      output$plot <- renderPlotly({
        plot_ly(x = ~ rnorm(input$slider2), type = "histogram")
      })

      # reactive to the value for the "Progress"
      output$progress <- renderUI({
        iconName <- switch(input$progress,
          "100" = "ok",
          "0" = "remove",
          "road"
        )
        p("Current progress is: ", icon(iconName, lib = "glyphicon"))
      })
    } # close out server
  ) # close out shinyApp()
}
