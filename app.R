library(shiny)
library(shinydashboard)
library(googleVis)
library(DT)

# TODO LIST
# Add more accurate complex location: load not only the first apartment address, but from entire first page
# and select the longest address (assume it is more accurate)

source("utils/encoding.r")

source_utf8("utils/apartments_utils.r")
source_utf8("utils/common_utils.r")

setwd(".")

complexes_select = c(
  "complex_name",
  "complex_creator",
  "complex_state",
  "complex_ready_date",
  "complex_location",
  "complex_location_coords",
  "complex_closest_metro",
  "geocoding_accurate"
)
complexes_select_columns = c(
  "Название",
  "Застройщик",
  "Состояние",
  "Готовность",
  "Адрес",
  "Координаты",
  "Метро",
  "Точное положение?"
)

apartments_select = c(
  "complex_name",
  "apartment_selling_type",
  "apartment_building_type",
  "apartment_ready_date",
  "apartment_type",
  "apartment_total_area",
  "apartment_living_area",
  "apartment_kitchen_area",
  "apartment_floor_number",
  "apartment_floor_total",
  "apartment_has_balkony",
  "apartment_has_loggia",
  "apartment_price",
  "apartment_link"
)
apartments_select_columns = c(
  "Компл",
  "Тип",
  "Дом",
  "Гот",
  "Комн",
  "Общ",
  "Жил",
  "Кух",
  "Этаж",
  "Этаж вс",
  "Балк",
  "Лодж",
  "Цена",
  "W"
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Properties finder v0.01"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Download Data", tabName = "download_data", icon = icon("sliders")),
      menuItem("Complexes", tabName = "complexes_data", icon = icon("building")),
      menuItem("Apartments", tabName = "apartments_data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("download_data", 
        fluidRow(
          box(
            title = "Data Download Settings",solidHeader = TRUE,collapsible = TRUE,status="success",
            column(
              sliderInput("complexes_max_pages", "Complexes maximum pages:", 1, 30, 1, 1),
              sliderInput("apartment_max_pages", "Apartments maximum pages:", 1, 30, 1, 1),
              checkboxInput("complexes_accurate_location", "Use accurate position for complexes",TRUE),
              checkboxInput("complexes_use_geocode", "Use geocode for complexes",TRUE),
              checkboxGroupInput("apartments_type_filter", "Rooms type",c("1-комнатная" = "room1=1", "2-комнатная" = "room2=1", "Студия" = "room9=1")),
              actionButton("download", "Start downloading data"),
              width = 6
            ),
            column(
              textOutput("currentTime"),
              width = 6
            ),
            width = 12
          )
        )
      ),
      tabItem("complexes_data",
        fluidRow(
          box(
            title = "Complexes information",solidHeader = TRUE,collapsible = TRUE,status="info",
            htmlOutput("complexes_map"),
            HTML("<hr>"),
            dataTableOutput("complexes_table"),
            width = 12
          )
        )
      ),
      tabItem("apartments_data",
        fluidRow(
          box(
            title = "Apartments information",solidHeader = TRUE,collapsible = TRUE,status="info",
            column(
              plotOutput('apartments_plot1'),
              width = 6
            ),
            column(
              plotOutput('apartments_plot2'),
              width = 6
            ),
            HTML("<hr>"),
            dataTableOutput("apartments_table"),
            width = 12
          )
        )
      )
    )
  )
)

render_data <- function(output, input, rv) {
  
  output$apartments_plot1 <- renderPlot({
    rv$apartments
    barplot(WorldPhones[,"N.Amer"]*1000, 
            main="N.Amer",
            ylab="Number of Telephones",
            xlab="Year")
  })
  
  output$apartments_plot2 <- renderPlot({
    rv$apartments
    barplot(WorldPhones[,"N.Amer"]*1000, 
            main="N.Amer",
            ylab="Number of Telephones",
            xlab="Year")
  })
  
  output$complexes_table <- renderDataTable({
    rv$complexes
    dataframe_complexes <- load_dataframe("data/complexes.csv")
    if (ncol(dataframe_complexes) > 0) {
      datatable(
        subset(dataframe_complexes, select = complexes_select), 
        colnames = complexes_select_columns, 
        escape = FALSE,
        options = list(pageLength = 100)
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
  
  output$apartments_table <- renderDataTable({
    rv$apartments
    dataframe_complexes <- load_dataframe("data/complexes.csv")
    dataframe_apartments <- load_dataframe("data/apartments.csv")
    if(nrow(dataframe_apartments)>0 & nrow(dataframe_complexes)>0) {
      dataframe_apartments <- merge(dataframe_complexes, dataframe_apartments, by.x = "complex_id", by.y = "complex_id")
      dataframe_apartments <- transform(dataframe_apartments, apartment_link = sprintf('<a href="%s">*</a>', apartment_link))
    }
    if (ncol(dataframe_apartments) > 0) {
      datatable(
        subset(dataframe_apartments, select = apartments_select), 
        colnames = apartments_select_columns,  
        escape = FALSE,
        options = list(pageLength = 100)
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
    
  output$complexes_map <- renderGvis({
    rv$complexes
    dataframe_complexes <- load_dataframe("data/complexes.csv")
    if (ncol(dataframe_complexes) > 0) {
      dataframe_complexes["map_tip"] <- ""
      dataframe_complexes <- transform(dataframe_complexes, map_tip = paste(complex_name,complex_location,paste("Кол-во квартир:", complex_apartment_count,sep=" "),sep = "<br>"))
      gvisMap(dataframe_complexes, "complex_location_coords", "map_tip", options=list(
        mapType='normal', 
        enableScrollWheel=TRUE, 
        showTip=TRUE))
    }
  })
}

log_msg <- function(msg) {
  print(sprintf("[%s] %s", format(Sys.time(), "%D %X"), msg))
}

server <- function(input, output,session) { 
  
  rv <- reactiveValues(complexes = 0, apartments = 0)
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  render_data(output, input, rv)
  
  observeEvent(input$download, {
    log_msg(sprintf("apartments_type_filter: %s", input$apartments_type_filter))
    log_msg(sprintf("complexes_use_geocode: %s", input$complexes_use_geocode))
    log_msg(sprintf("complexes_accurate_location: %s", input$complexes_accurate_location))
    
    # forming request params vector
    params <- vector(mode="numeric", length=0)
    params <- append(params, input$apartments_type_filter)
      
    # Downloading complexes
    dataframe_complexes <- data.frame(matrix(ncol = 0, nrow = 0))
    progress <- shiny::Progress$new(session, min=0, max=1)
    progress$set(message = 'COMPLEXES', detail = 'downloading complexes...')
    dataframe_complexes <- APUTILS_download_complexes(log_msg, progress, input$complexes_max_pages, input$complexes_accurate_location, input$complexes_use_geocode, params)
    write_dataframe(dataframe_complexes, "data/complexes.csv")      
    progress$close()
    isolate(rv$complexes <- rv$complexes + 1)
    
    # Downloading apartments
    dataframe_apartments <- data.frame(matrix(ncol = 0, nrow = 0))
    progress <- shiny::Progress$new(session, min=0, max=1)
    progress$set(message = 'APARTMENTS', detail = 'downloading apartments...')
    dataframe_apartments <- APUTILS_download_apartments(log_msg, progress, dataframe_complexes["complex_id"], input$apartment_max_pages, params)
    write_dataframe(dataframe_apartments, "data/apartments.csv")  
    progress$close()
    isolate(rv$apartments <- rv$apartments + 1)
  })
}

shinyApp(ui, server)