packages <- c("shiny",
              "dplyr",
              "tidyr",
              "echarts4r",
              "lubridate",
              "bslib",
              "tools",
              "readxl")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE))
  install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))

ui = fluidPage(
  tags$head(
    HTML(
      "
     <script>
     var socket_timeout_interval
     var n = 0
     $(document).on('shiny:connected', function(event) {
     socket_timeout_interval = setInterval(function(){
     Shiny.onInputChange('count', n++)
     }, 15000)
     });
     $(document).on('shiny:disconnected', function(event) {
     clearInterval(socket_timeout_interval)
     });
     </script>
     "
    )
  ),
  textOutput("keepAlive"),
  
  
  titlePanel("Analysis from productiondata"),
  ## Sidebar
  
  fileInput("upload", 
            "Upload the xlsx file ",
            accept = ".xlsx"),
  actionButton("show", "Explaination"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        'lactations',"Laktations:",
        min = 1,max = 10,##na.rm = T),
        step = 1,
        value = c(1,3)
      ),
      sliderInput(
        'dil',"Days in milk:",
        min = 1,max = 500,
        step = 1,
        value = c(1,365)
      ),
      selectInput(
        inputId = 'trait', 
        label = "Trait", 
        choices = "productie",
        selected = 'productie',
        selectize = T,
        multiple = T
      ),
      dateInput("dateg",
                "Date from the scatterplot",
                value = Sys.Date(),
                min = floor_date(Sys.Date(), unit = "month"),
                max = Sys.Date()
      )
    ),
    mainPanel(
      fluidRow(column(6,echarts4rOutput("plotdil",height = "500px")),
               column(6,echarts4rOutput("plotdatum",height = "500px"))),
      fluidRow(echarts4rOutput("groteplot",height = "700px"))
    )))


server = function(input, output,session) {
  options(shiny.maxRequestSize=300*1024^2)
  cns <- c("id",
           "datum",
           "lactatie",
           "dim",
           "productie")
  observe({
    type_txt <- ifelse(input$type == "default", "notification", input$type)
    showNotification(
      "Invoer:
        id = animal,
        datum = date of the record,\n
        lactatie = lactationnumber,\n
        dim = days in milk,\n
        productie = production,\n
        the other columns are considered as other production variables,\n
        case of the columnnames does not matter\n",
      duration = NULL,
      id = "message"
    )
  }) |>
    bindEvent(input$show)
  
  data <- reactive({
    req(input$upload)
    d <- read_xlsx(path = input$upload$datapath) |> 
      rename_with(tolower) |> 
      filter(productie>0,dim>0)
    d
  })
  
  vns <- reactive({
    vns <- colnames(data())
    vns <- c('productie',vns[!vns%in%cns])
  })
  
  observe({
    updateSliderInput(session,"dil",
                      min = 1,
                      max = max(data()$dim,na.rm = T),
                      value = c(10,100)
    )
    updateSliderInput(session, 'lactations',
                      min = 1,
                      max = max(data()$lactatie,na.rm = T),
                      value = c(1,3)
                      
    )
    updateSelectInput(session, "trait",
                      selected = 'productie',
                      choices = vns())
    updateDateInput(session,'dateg',
                    value = max(data()$datum,na.rm = T),
                    min = min(data()$datum,na.rm = T),
                    max = max(data()$datum,na.rm = T))
  })
  
  
  dt <- reactive({
    data() |> 
      filter(productie>0&
               dim>0&
               lactatie>0&
               lactatie>=input$lactations[1]&
               lactatie<=input$lactations[2]&
               input$dil[1]<=dim&
               dim<input$dil[2]
      ) |>
      ungroup()|>
      pivot_longer(cols = vns(),names_repair = 'minimal')|> 
      filter(!is.na(value))
  })
  output$plotdil = renderEcharts4r({
    dt() |> 
      filter(name == "productie") |> 
      group_by(datum) |> 
      summarise(dim = mean(dim,na.rm = T),.groups = "drop") |> 
      e_charts(datum) |> 
      e_line(dim,symbol="none")
  })
  output$plotdatum = renderEcharts4r({
    dt() |>
      filter(name%in%input$trait[1]&
               datum==input$dateg) |>
      ungroup() |>
      ##group_by(datetime) |> 
      e_chart(dim) |>
      e_scatter(value,symbol_size = 5,bind = id) |>
      e_tooltip(
        formatter = htmlwidgets::JS("
            function(params){
              return('<strong> Cow' + params.name +
                  '</strong><br />dim: ' + params.value[0] +
                  '<br />value: ' + params.value[1])
                  }
            ")) 
  })
  output$groteplot <- renderEcharts4r({
    dt() |> 
      filter(name%in%input$trait) |> 
      group_by(name,datum) |> 
      summarise(value = mean(value,na.rm = T)) |> 
      e_charts(datum) |> 
      e_line(value,symbol="none") |> 
      e_tooltip(trigger = "axis") |> # tooltip
      e_connect_group("grp") |> 
      e_datazoom(x_index = 0, type = "slider") 
  })
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
}


shinyApp(ui,server,options = list(height = 500))



