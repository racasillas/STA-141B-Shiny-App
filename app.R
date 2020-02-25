library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)

if (interactive()) {
  # making the header
  # there's a lot more we can do with the header, but this is just the basic format
  header <-
    dashboardHeaderPlus(title = tagList(span(
      class = "logo-lg", "NBA Visualization"), # to remove text when collapsing sidebar
      img(src = ""))) # we can make an image in the header if we want to (like the NBA or something)
  
  # making the sidebar
  sidebar <- dashboardSidebar(sidebarMenu(
    id = 'tabs',
    
    # create first menu item (aka panel on the left)
    menuItem(
      "Summarizing Delays",
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
        selectInput(
          inputId = 'xcol',
          label ='X Variable', 
          choices = names(mtcars)
          ),
        selectInput(
          inputId = 'ycol',
          label = 'Y Variable', 
          choices = names(mtcars),
          selected = names(mtcars)[[2]]
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
        choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,
                    "100%" = 100)
    )
  ) # end second tab
  )) # end sidebar creation
  
  # creating the body
  body <- dashboardBody(uiOutput('body'),
                        
                        # all of this is just color customization for the header
                        # it can be whatever we want this is just filler for now
                        tags$head(tags$style(
                          HTML(
                            '
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
                            '
                          )
                          )))# end color customization
  
  
  shinyApp(
    # making the UI a component of the header we created earlier, the sidebar we created earlier, and the body created below
    ui = dashboardPagePlus(header,
                           sidebar,
                           body,
                           skin = 'blue'),
    
    server  = function(input, output, session) {
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
              gradientBox(
                title = "Gradient Box Example",
                icon = "fa fa-th",
                gradientColor = "teal", 
                boxToolSize = "sm", 
                footer = paste(
                "This is a gradient box. The box to the right (Box 2) is a normal box that reacts to when you 
                click Update Data on the left. The box below (Box 3) is a normal box too that contains 
                the data filtered when clicking Update Data. Boxes will primarily be how we format this app, 
                and they're very customizeable and can contain whatever we want (filtering, graphs, data, text, etc)."
                )
              ), # end box 
              box(
                solidHeader = TRUE,
                status = "primary",
                title = "Box 2 example",
                # any output can go in a box, but in this case I'm using plotlyOutput just to show it can show a graph
                plotlyOutput('timeseries'),
                width = 6) # end of box
            ),
            # begin second fluid row
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
                  title = "Box 3",
                  # div(...) is another example of output we can do in a box
                  div(div(style = 'overflow-x:scroll',
                          DT::dataTableOutput('dataexample'))),
                  # max width = 12, so since this is 12 it will span the whole body
                  width = 12
                  ) # end of box
            ) # end second fluid row
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
              column(6,
                infoBox(
                  "Number of Players", uiOutput("orderNum2"), "Subtitle", icon = icon("users"), width = 12
                  ),
                valueBox(
                  uiOutput("orderNum"), "Number of Teams", icon = icon("credit-card"), width = 12,
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
              column(6,
                     box(title = "Histogram box title",
                         width = 12,
                         status = "warning", solidHeader = TRUE, collapsible = TRUE,
                         plotlyOutput("plot", height = 250)
                     )
              )
                     
            ), # end fluidrow
            
            # begin next fluidRow, mainly used to show how we can have different horizontal and vertical sections for output
            fluidRow(
              box(title = "extra box",
                  width = 12,
                  status = 'primary', 
                  solidHeader = TRUE,
                  h4("Everything above this is one 'fluidRow'. fluidRow will be how the boxes are grouped horizontally. 
                     The three smaller boxes to the left of the graph are grouped by 1 'column' of the fluidRow, and the
                     graph is the second column of the fluidRow. Columns can separate fluidRows vertically. Most shiny
                     elements have a max width of 12, so you can separate a fluidRow that is length 12 by 2 columns that 
                     are each 6 units wide, as above. The box that this text is in in length 12 so it's as wide as it
                     can be."),
                  h4("These are just a few examples of how we can layout the app. Once we have the data, we can draw out 
                     how we want the app to look and which panels we want to have what data, and I can help design it from there"
                  )
              )
            )
  
          )
        }
      }) # end output$body
      
      
      
      ### begin testing api ###
      r <- GET(
        "https://free-nba.p.rapidapi.com/stats",
        add_headers(`X-RapidAPI-Key` = "aae7df4492msh5823be00af21410p188e17jsn6409df5b231e"),
        query = list(
          page = "0",
          `per_page` = "100"
        )
      )
      json <- content(r, as = "text")
      # fromJSON(json, flatten = TRUE)$data
      output$dataexample <- DT::renderDataTable({fromJSON(json, flatten = TRUE)$data})
      
      ### end testing api ###
      
      
      
      ### used for the dataTable at the bottom of the output for tab1 ###
      carsData <- eventReactive(input$update, {
        selectedDat <- mtcars %>% 
          select(input$xcol, input$ycol)
        selectedDat
      })
      
      # output$dataexample <- DT::renderDataTable({
      #   data = carsData()
      # })
      
      ### this is an example of making the output for the box with the time series (box 2) ###
      # 1) makes the data when the button is clicked with `x` and `y`
      # 2) renders the output based on `x` and `y` with output$timeseries
      x <- eventReactive(input$update, {
        mtcars[,input$xcol]
      })
      
      y <- eventReactive(input$update, {
        mtcars[,input$ycol]
      })
      
      output$timeseries <- renderPlotly(
        plot1 <- plot_ly(
          # x() is because we defined the function x above using eventReactive
          x = x(),
          # y() is because we defined the function y above using eventReactive
          y = y(), 
          type = 'scatter',
          mode = 'markers')
      )
      ### end of making the output for the box with the time series ###
      
      
      
      ### below this is the output for the second tab ###
      
      # reactive to the first slider
      output$orderNum <- renderText({
        prettyNum(input$slider, big.mark=",")
      })
      
      # reactive to the second slider
      output$orderNum2 <- renderText({
        prettyNum(input$slider2, big.mark=",")
      })
      
      # reactive to the second slider
      output$plot <- renderPlotly({
        plot_ly(x = ~rnorm(input$slider2), type= "histogram")
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
