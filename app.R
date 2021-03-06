library(shiny)
library(shinydashboard)
library(googleVis)
library(DT)
library(leaflet)
library(FNN)

# TODO LIST


source("utils/encoding.r")

source_utf8("utils/apartments_utils.r")
source_utf8("utils/common_utils.r")
source_utf8("utils/model_utils.r")

# Common configuration
setwd(".")
options(scipen=999)

complexes_select = c(
  "complex_name",
  "complex_creator",
  "complex_state",
  "complex_ready_date",
  "complex_location",
  "complex_location_coords",
  "complex_location_dist_to_center",
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
  "Дист.ц",
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
  "complex_location_dist_to_center",
  "complex_dist_to_metro",
  "apartment_closest_metro_dist_time",
  "apartment_closest_metro_dist_type",
  "apartment_link",
  "apartment_price_meter",
  "apartment_price"
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
  "Эт",
  "Эт в",
  "Б",
  "Л",
  "Ц",
  "Метр",
  "Метр вр",
  "Метр т",
  "W",
  "Ц кв",
  "Ц"
)

apartments_cluster_select = c(
  "complex_name",
  "apartment_building_type",
  "apartment_ready_date",
  "apartment_type",
  "apartment_total_area",
  "apartment_living_area",
  "apartment_kitchen_area",
  "apartment_floor_number",
  "apartment_floor_total",
  "apartment_is_first_floor",
  "apartment_is_last_floor",
  "apartment_has_balkony",
  "apartment_has_loggia",
  "complex_state",
  "complex_location_dist_to_center",
  "complex_dist_to_metro",
  "apartment_closest_metro_dist_time",
  "apartment_closest_metro_dist_type",
  "apartment_link",
  "apartment_price_meter",
  "apartment_price"
)

apartments_cluster_select_columns = c(
  "Комп",
  "Дом",
  "Гот",
  "Комн",
  "Общ",
  "Жил",
  "Кух",
  "Э",
  "Э вс",
  "Перв эт",
  "Посл эт",
  "Б",
  "Л",
  "Стр",
  "Ц",
  "М",
  "М вр",
  "М т",
  "W",
  "Ц кв",
  "Ц"
)

model_res_select = append(apartments_select, c("apartment_price_pred","percents"))
model_res_select_columns = append(apartments_select_columns, c("Ц п","Выг"))

ui <- dashboardPage(
  dashboardHeader(
    title = "Properties finder v0.01"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Download Data", tabName = "download_data", icon = icon("sliders")),
      menuItem("Complexes", tabName = "complexes_data", icon = icon("building")),
      menuItem("Apartments", tabName = "apartments_data", icon = icon("database")),
      menuItem("Modelling", tabName = "model_data", icon = icon("calculator")),
      menuItem("Data Statistics", tabName = "data_stats", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML(
              "#logTextBox{ 
              overflow-y: scroll; 
              overflow-x: hidden; 
              height:250px;}"
            )
      )
    ),
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
              checkboxGroupInput("apartments_type_filter", "Rooms type",c("1-комнатная" = "room1=1", "2-комнатная" = "room2=1", "Студия" = "room9=1"), selected = c("room1=1")),
              actionButton("download", "Start downloading data"),
              width = 6
            ),
            column(
              htmlOutput("dataStats"),
              width = 6
            ),
            width = 12
          ),
          box(
            title = "Processing logs",solidHeader = TRUE,collapsible = TRUE,status="success",
            column(
              div(id="logTextBox",htmlOutput("logOutput")),
              width = 12
            ),
            width = 12
          )
        )
      ),
      tabItem("complexes_data",
        fluidRow(
          box(
            title = "Complexes information",solidHeader = TRUE,collapsible = TRUE,status="info",
            leafletOutput("complexes_map"),
            HTML("<hr>"),
            dataTableOutput("complexes_table"),
            width = 12
          )
        )
      ),
      tabItem("apartments_data",
        fluidRow(
          box(
            title = "Apartments statistics",solidHeader = TRUE,collapsible = TRUE,status="info",
            column(
              plotOutput('apartments_plot1'),
              width = 6
            ),
            column(
              plotOutput('apartments_plot2'),
              width = 6
            ),
            column(
              sliderInput("apartment_price_filter", "Apartments price filter:", min = 1000000, max = 10000000, value = c(1500000, 3500000), step = 100000),
              width = 12
            ),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Apartments table",solidHeader = TRUE,collapsible = TRUE,status="info",
            dataTableOutput("apartments_table"),
            width = 12
          )
        )
      ),
      tabItem("model_data",
        fluidRow(
          box(
            title = "Data modelling",solidHeader = TRUE,collapsible = TRUE,status="info",
            column(
              sliderInput("model_threshold", "Minimum threshold:", 1, 30, 10, 1),
              sliderInput("model_max_distance", "Maximum distance:", 1000, 100000, 30000, 1000),
              sliderInput("model_price_filter", "Price filter:", min = 1000000, max = 10000000, value = c(1500000, 3500000), step = 100000),
              width = 4
            ),
            column(
              htmlOutput("model_stats"),
              HTML("<hr>"),
              actionButton("modelling", "Build model"),
              width = 2
            ),
            column(
              leafletOutput("model_res_map"),
              width = 6
            ),
            column(
              plotOutput("model_res_plot1"),
              width = 6
            ),
            column(
              plotOutput("model_res_plot2"),
              width = 6
            ),
            width = 12
          ),
          box(
            title = "Similar apartments",solidHeader = TRUE,collapsible = TRUE,status="info",
            column(
              sliderInput("model_cluster_k", "K value:", 1, 100, 5, 1),
              width = 6
            ),
            column(
              htmlOutput("model_cluster_selected"),
              width = 6
            ),
            column(
              dataTableOutput("model_res_table1"),
              width = 12
            ),
            width = 12
          ),
          box(
            title = "Model Table",solidHeader = TRUE,collapsible = TRUE,status="info",
            column(
              dataTableOutput("model_res_table"),
              width = 12
            ),
            width = 12
          )
        )
      ),
      tabItem("data_stats",
        fluidRow(
          box(
            column(
              plotOutput("data_stats_plot1"),
              width = 6
            ),
            column(
              plotOutput("data_stats_plot2"),
              width = 6
            ),
            column(
              sliderInput("data_stats_max_distance", "Maximum distance:", 1000, 100000, 30000, 1000),
              sliderInput("data_stats_max_distance_smooth", "Smoothing value:", 0.1, 1, 0.4, 0.1),
              width = 12
            ),
            width = 12
          )
        )
      )
    )
  )
)

render_logs <- function(output, input, session) {
  pollData <- reactivePoll(4000, session,
    checkFunc = function() {
      if (file.exists("data/log.txt"))
        file.info("data/log.txt")$mtime[1]
      else
        ""
    },
    valueFunc = function() {
      readLines("data/log.txt",encoding = "UTF-8")
    }
  )
  output$logOutput <- renderText({
    text <- pollData()
    text
  })
}

render_data <- function(output, input, rv) {
  
  # Rendering plain texts
  output$model_stats <- renderText({
    rv$model_res
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    dataframe_model_res <- COMMONUTILS_load_dataframe("data/model_res.csv")
    percents <- MODELUTILS_percent(as.numeric(nrow(dataframe_model_res) / nrow(dataframe_apartments)))
    model_error <- ifelse(nrow(dataframe_model_res) > 0, dataframe_model_res[1,]$predict_error, NA)
    res <- "<b>Model statistics:</b>"
    res <- paste(res, paste("Apartments amount", as.character(nrow(dataframe_apartments)), sep = ": "), sep = "<br>")
    res <- paste(res, sprintf("Used in modelling: %d (%s percents)", nrow(dataframe_model_res), percents), sep = "<br>")
    res <- paste(res, sprintf("Model error: %.1f percents", model_error), sep = "<br>")
  })
  
  output$dataStats <- renderText({
    rv$apartments
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    res <- "<b>Data statistics:</b>"
    res <- paste(res, paste("Complexes amount", as.character(nrow(dataframe_complexes)), sep = ": "), sep = "<br>")
    res <- paste(res, paste("Apartments amount", as.character(nrow(dataframe_apartments)), sep = ": "), sep = "<br>")
  })
  
  output$model_cluster_selected <- renderText({
    rv$model_res
    res <- sprintf("Selected: %s",str(input$model_res_table_rows_selected))
    res
  })
  
  # Rendering plots
  output$apartments_plot1 <- renderPlot({
    rv$apartments
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    dataframe_apartments <- dataframe_apartments[dataframe_apartments$apartment_price >= input$apartment_price_filter[1] & dataframe_apartments$apartment_price <= input$apartment_price_filter[2],]
    if(nrow(dataframe_apartments)>0) {
      plot(dataframe_apartments$apartment_price, dataframe_apartments$apartment_total_area, xlab="Price", ylab = "Total Area", main="Price VS Area")
    }
  })
  
  output$apartments_plot2 <- renderPlot({
    rv$apartments
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv", factors = TRUE)
    dataframe_apartments <- dataframe_apartments[dataframe_apartments$apartment_price >= input$apartment_price_filter[1] & dataframe_apartments$apartment_price <= input$apartment_price_filter[2],]
    if(nrow(dataframe_apartments)>0) {
      dataframe_apartments <- dataframe_apartments[!is.na(dataframe_apartments$apartment_closest_metro),]
      counts <- table(dataframe_apartments$apartment_closest_metro)
      par(las=2)
      par(oma=c(5,0,0,0))
      barplot(counts, main="Metro",ylab="Number of apartments")
    }
  })
  
  output$model_res_plot1 <- renderPlot({
    rv$model_res
    load("rf-res.data")
    varImpPlot(data.rf, type = 2, main = "Variable importance")
  })
  
  output$model_res_plot2 <- renderPlot({
    rv$model_res
    dataframe_model_res <- COMMONUTILS_load_dataframe("data/model_res.csv")
    dataframe_model_res <- dataframe_model_res[dataframe_model_res$complex_location_dist_to_center <= input$model_max_distance & dataframe_model_res$apartment_price >= input$model_price_filter[1] & dataframe_model_res$apartment_price <= input$model_price_filter[2],]
    plot(dataframe_model_res$apartment_price_pred, dataframe_model_res$apartment_price)
    par(new=TRUE, col="red")
    dependency <- lm(dataframe_model_res$apartment_price_pred ~ dataframe_model_res$apartment_price)
    abline(dependency) 
  })
  
  output$data_stats_plot1 <- renderPlot({
    rv$apartments
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    if(nrow(dataframe_apartments)>0 & nrow(dataframe_complexes)>0) {
      dataframe_apartments <- merge(dataframe_complexes, dataframe_apartments, by.x = "complex_id", by.y = "complex_id")
      dataframe_apartments <- dataframe_apartments[dataframe_apartments$complex_location_dist_to_center <= input$data_stats_max_distance,]
    }
    df_aggregated <- aggregate(apartment_price_meter ~ complex_location_dist_to_center, dataframe_apartments, function(x) median(x))
    plot(main = "Apartment Price vs. Distance to center", df_aggregated)
    df_median <- data.frame(complex_location_dist_to_center=df_aggregated$complex_location_dist_to_center, apartment_price_meter=predict(loess(apartment_price_meter ~ complex_location_dist_to_center, df_aggregated, span=input$data_stats_max_distance_smooth)))
    lines(df_median$complex_location_dist_to_center, df_median$apartment_price_meter, col="darkgreen", lwd=3)
  })
  
  output$data_stats_plot2 <- renderPlot({
    rv$apartments
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    if(nrow(dataframe_apartments)>0 & nrow(dataframe_complexes)>0) {
      dataframe_apartments <- merge(dataframe_complexes, dataframe_apartments, by.x = "complex_id", by.y = "complex_id")
      dataframe_apartments <- dataframe_apartments[dataframe_apartments$complex_location_dist_to_center <= input$data_stats_max_distance,]
    }
    max_dist <- max(dataframe_apartments$complex_location_dist_to_center, na.rm = TRUE)
    h <-hist(dataframe_apartments$complex_location_dist_to_center, breaks=20, xaxt='n', main = "Distance to center histogram")
    axis(side=1, at=seq(0, max_dist, 1000), labels=seq(0,max_dist,1000))
    xfit<-seq(min(dataframe_apartments$complex_location_dist_to_center),max(dataframe_apartments$complex_location_dist_to_center),length=40) 
    yfit<-dnorm(xfit,mean=mean(dataframe_apartments$complex_location_dist_to_center),sd=sd(dataframe_apartments$complex_location_dist_to_center)) 
    yfit <- yfit*diff(h$mids[1:2])*length(dataframe_apartments$complex_location_dist_to_center) 
    lines(xfit, yfit, col="blue", lwd=3)
    h
  })
  
  # Rendering data tables
  output$complexes_table <- renderDataTable({
    rv$complexes
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    if (nrow(dataframe_complexes) > 0) {
      datatable(
        subset(dataframe_complexes, select = complexes_select), 
        colnames = complexes_select_columns, 
        escape = FALSE,
        options = list(paging = FALSE,pageLength = 100)
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
  
  output$apartments_table <- renderDataTable({
    rv$apartments
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    dataframe_apartments <- COMMONUTILS_load_dataframe("data/apartments.csv")
    if(nrow(dataframe_apartments)>0 & nrow(dataframe_complexes)>0) {
      dataframe_apartments <- merge(dataframe_complexes, dataframe_apartments, by.x = "complex_id", by.y = "complex_id")
      dataframe_apartments <- transform(dataframe_apartments, apartment_link = sprintf('<a href="%s" target="_blank">*</a>', apartment_link))
    }
    if (ncol(dataframe_apartments) > 0) {
      datatable(
        subset(dataframe_apartments[dataframe_apartments$apartment_price >= input$apartment_price_filter[1] & dataframe_apartments$apartment_price <= input$apartment_price_filter[2],], select = apartments_select), 
        colnames = apartments_select_columns,  
        escape = FALSE,
        options = list(pageLength = 100)
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
  
  output$model_res_table <- renderDataTable({
    rv$model_res
    dataframe_model_res <- COMMONUTILS_load_dataframe("data/model_res.csv")
    dataframe_model_res <- transform(dataframe_model_res, apartment_link = sprintf('<a href="%s" target="_blank">*</a>', apartment_link))
    if (ncol(dataframe_model_res) > 0) {
      datatable(
        subset(dataframe_model_res[dataframe_model_res$complex_location_dist_to_center <= input$model_max_distance & dataframe_model_res$percents >= input$model_threshold & dataframe_model_res$apartment_price >= input$model_price_filter[1] & dataframe_model_res$apartment_price <= input$model_price_filter[2],], select = model_res_select),
        colnames = model_res_select_columns,  
        escape = FALSE,
        options = list(pageLength = 100),
        selection = 'single'
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
  
  output$model_res_table1 <- renderDataTable({
    rv$model_res
    dataframe_model_res <- COMMONUTILS_load_dataframe("data/model_res.csv")
    dataframe_model_res <- transform(dataframe_model_res, apartment_link = sprintf('<a href="%s" target="_blank">*</a>', apartment_link))
    if(!is.null(input$model_res_table_rows_selected)) {
      dataframe_query <- dataframe_model_res[as.numeric(input$model_res_table_rows_selected),]
      dataframe_nn_select <- MODELUTILS_run_knn(dataframe_model_res,dataframe_query,input$model_cluster_k)
      datatable(
        subset(dataframe_nn_select, select = apartments_cluster_select),
        colnames = apartments_cluster_select_columns,  
        escape = FALSE,
        options = list(pageLength = 5)
      )
    } else {
      datatable(data.frame(matrix(ncol = 0, nrow = 0)))
    }
  })
    
  # Rendering maps
  output$complexes_map <- renderLeaflet({
    rv$complexes
    dataframe_complexes <- COMMONUTILS_load_dataframe("data/complexes.csv")
    if (ncol(dataframe_complexes) > 0 & nrow(dataframe_complexes)) {
      dataframe_complexes["map_tip"] <- ""
      dataframe_complexes <- transform(dataframe_complexes, map_tip = paste(complex_name,complex_location,paste("Кол-во квартир:", complex_apartment_count,sep=" "),sep = "<br>"))
      m = leaflet(dataframe_complexes) %>% addTiles()
      m %>% addMarkers(
        lat = ~complex_location_coords_lat, 
        lng = ~complex_location_coords_lon, 
        popup = ~map_tip
        #radius = 6,
        #color = "navy",
        #stroke = FALSE, 
        #fillOpacity = 0.5
      )
    }
  })
  
  output$model_res_map <- renderLeaflet({
    rv$model_res
    dataframe_model_res <- COMMONUTILS_load_dataframe("data/model_res.csv")
    dataframe_model_res <- dataframe_model_res[dataframe_model_res$complex_location_dist_to_center <= input$model_max_distance & dataframe_model_res$percents >= input$model_threshold & dataframe_model_res$apartment_price >= input$model_price_filter[1] & dataframe_model_res$apartment_price <= input$model_price_filter[2],]
    if (ncol(dataframe_model_res) > 0 && nrow(dataframe_model_res)) {
      dataframe_model_res["map_tip"] <- ""
      dataframe_model_res <- transform(dataframe_model_res, map_tip = sprintf("%s<br>Цена: %d<br>Выгода: %s", complex_name, apartment_price, percents))
      m = leaflet(dataframe_model_res) %>% addTiles()
      m %>% addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        lat = ~complex_location_coords_lat, 
        lng = ~complex_location_coords_lon, 
        popup = ~map_tip,
        radius = 9,
        color = "red",
        stroke = FALSE, 
        fillOpacity = 0.9
      )
    } else {
      m = leaflet() %>% addTiles()
    }
  })
}

log_msg <- function(msg) {
  msg <- sprintf("<b>[%s]</b> %s", format(Sys.time(), "%D %X"), msg)
  print(msg)
  cat(msg, '<br>', file = "data/log.txt",append = TRUE)
}

server <- function(input, output,session) { 
  
  rv <- reactiveValues(complexes = 0, apartments = 0, model_res = 0)
  
  render_logs(output, input, session)
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
    progress <- shiny::Progress$new(session, min=0, max=input$complexes_max_pages)
    progress$set(message = 'COMPLEXES', detail = 'downloading complexes...')
    dataframe_complexes <- APUTILS_download_complexes(log_msg, progress, input$complexes_max_pages, input$complexes_accurate_location, input$complexes_use_geocode, params)
    COMMONUTILS_write_dataframe(dataframe_complexes, "data/complexes.csv")      
    progress$close()
    isolate(rv$complexes <- rv$complexes + 1)
    
    # Downloading apartments
    dataframe_apartments <- data.frame(matrix(ncol = 0, nrow = 0))
    progress <- shiny::Progress$new(session, min=0, max=nrow(dataframe_complexes))
    progress$set(message = 'APARTMENTS', detail = 'downloading apartments...')
    dataframe_apartments <- APUTILS_download_apartments(log_msg, progress, dataframe_complexes["complex_id"], input$apartment_max_pages, params)
    COMMONUTILS_write_dataframe(dataframe_apartments, "data/apartments.csv")  
    progress$close()
    isolate(rv$apartments <- rv$apartments + 1)
    
    log_msg("Done downloading data")
  })
  
  observeEvent(input$modelling, {
    progress <- shiny::Progress$new(session, min=0, max=10)
    progress$set(message = 'MODELLING', detail = 'preparing dataset...')   
    
    df_complexes <- load_dataframe("data/complexes.csv", factors = TRUE)
    df_apartments <- load_dataframe("data/apartments.csv", factors = TRUE)
    df_apartments <- merge(x = df_complexes, y = df_apartments, by = "complex_id", all = TRUE)
    
    dataframe_model_res <- MODELUTILS_run_model(log_msg, progress, df_apartments)
    COMMONUTILS_write_dataframe(dataframe_model_res, "data/model_res.csv") 
    
    progress$close()
    isolate(rv$model_res <- rv$model_res + 1)
    
    log_msg("Done modelling")
  })
}
log_msg("App started")
shinyApp(ui, server)
