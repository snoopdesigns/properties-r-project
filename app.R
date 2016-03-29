library(shiny)
library(shinydashboard)
library(googleVis)

source("main.r")

dataframe_complexes <- data.frame(matrix(ncol = 0, nrow = 0))
dataframe_apartments <- data.frame(matrix(ncol = 0, nrow = 0))

complexes_select = c("complex_name","complex_creator","complex_state","complex_location_exact","complex_location_accurate","complex_location_coords","complex_apartment_count")

status = c("NOT AVAILABLE", NA, NA)

ui <- dashboardPage(
  dashboardHeader(
    title = "Test dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Settings", tabName = "Settings", icon = icon("sliders")),
      menuItem("Complexes", tabName = "Complexes", icon = icon("folder"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Settings", 
        fluidRow(
          box(
            title = "Controls",solidHeader = TRUE,collapsible = TRUE,status="success",
            sliderInput("complex_count", "Number of complexes:", 1, 30, 1, 1),
            checkboxGroupInput("redownload_complexes", "Re-Download complexes",c("Yes" = "yes")),
            checkboxGroupInput("complexes_use_geocode", "Use geocode for complexes",c("Yes" = "yes")),
            checkboxGroupInput("complexes_exact_location", "Use accurate position for complexes",c("Yes" = "yes")),
            checkboxGroupInput("rooms", "Apartment type:",c("One-room" = "room1=1","Double-room" = "room2=1","3-room" = "room3=1","4-room" = "room4=1")),
            actionButton("actionButton", "Start analysis")
          ),
          box(
            title = "Status",solidHeader = TRUE,collapsible = TRUE,status="success",
            textOutput("status_text"),
            textOutput("complexes_text"),
            textOutput("apartments_text")
          )
        ),
        fluidRow(
          box(
            title = "Apartments",solidHeader = TRUE,collapsible = TRUE,status="info",
            dataTableOutput("ap_table_res"),
            width = "85%")
        )
      ),
      tabItem("Complexes",
        fluidRow(
          box(
            title = "Complexes map",solidHeader = TRUE,collapsible = TRUE,status="info",
            htmlOutput("complexes_map"),
            width = "85%",
            height = "85%")
        ),
        fluidRow(
          box(
            title = "Complexes",solidHeader = TRUE,collapsible = TRUE,status="info",
            dataTableOutput("complex_table_res"),
            width = "85%")
        )
      )
    )
  )
)

server <- function(input, output) { 
  output$status_text <- renderText(sprintf("Analysis status: %s",status[1]))
  output$complexes_text <- renderText(sprintf("Complexes: %s",status[2]))
  output$apartments_text <- renderText(sprintf("Apartments: %s",status[3]))
  
  output$ap_table_res <- renderDataTable({
    if (ncol(dataframe_complexes) > 0) {
      subset(dataframe_complexes, select = complexes_select)
    } else {
      data.frame(matrix(ncol = 0, nrow = 0))
    }
  })
  
  observeEvent(input$actionButton, {
    withProgress(message = 'Progress: ',value = 0, {
      
      incProgress(0, detail = "FETCH START")
      
      #update status
      status[1] <- "ANALYSING"
      status[2] <- NA
      status[3] <- NA
      output$status_text <- renderText(sprintf("Analysis status: %s",status[1]))
      output$complexes_text <- renderText(sprintf("Complexes: %s",status[2]))
      output$apartments_text <- renderText(sprintf("Apartments: %s",status[3]))
      
      if (is.null(input$redownload_complexes)) {
        print("Using complexes from file")
        dataframe_complexes <- read.csv("/home/dimka/PROJECTS/r-project/complexes.csv")
      } else {
        print("Redownloading complexes")
        if(is.null(input$complexes_use_geocode)) {
          cmpl_use_geocode <- FALSE
        } else {
          cmpl_use_geocode <- TRUE
        }
        if(is.null(input$complexes_exact_location)) {
          cmpl_exact_location <- FALSE
        } else {
          cmpl_exact_location <- TRUE
        }
        
        params <- vector(mode="numeric", length=0) # TODO
        for (complexes_i in 1:input$complex_count) {
          incProgress(1/(input$complex_count+1), detail = sprintf("FETCHING COMPLEX: %d", complexes_i))
          df_complexes_tmp <- fetch_complexes_page(params, complexes_i, cmpl_exact_location, cmpl_use_geocode)
          if (nrow(df_complexes_tmp) > 0) {
            dataframe_complexes <- rbind(df_complexes_tmp, dataframe_complexes)
          } else {
            break
          }
        }
        write.csv(dataframe_complexes, "/home/dimka/PROJECTS/r-project/complexes.csv")
      }
      incProgress(1/(input$complex_count+1), detail = "done")

      # fill tables
      output$complex_table_res <- renderDataTable({
        subset(dataframe_complexes, select = complexes_select)
      })

      # create complexes map
      # add tips column
      print(sprintf("Columns: %d, %d", nrow(dataframe_complexes),ncol(dataframe_complexes)))
      dataframe_complexes["map_tip"] <- ""
      dataframe_complexes <- transform(dataframe_complexes, map_tip = complex_name)
      dataframe_complexes <- transform(dataframe_complexes, map_tip = paste(map_tip,complex_location_exact,sep = "<br>"))
      dataframe_complexes <- transform(dataframe_complexes, map_tip = paste(map_tip,complex_closest_metro,sep = "<br>"))
      output$complexes_map <- renderGvis({
        gvisMap(dataframe_complexes, "complex_location_coords", "map_tip", options=list(
          mapType='normal', 
          enableScrollWheel=TRUE, 
          showTip=TRUE))
      })
      
      #update status
      status[1] <- "AVAILABLE"
      status[2] <- nrow(dataframe_complexes)
      status[3] <- nrow(dataframe_apartments)
      output$status_text <- renderText(sprintf("Analysis status: %s",status[1]))
      output$complexes_text <- renderText(sprintf("Complexes: %s",status[2]))
      output$apartments_text <- renderText(sprintf("Apartments: %s",status[3]))
    })
  })
}

shinyApp(ui, server)