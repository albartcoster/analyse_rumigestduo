library(shiny)
library(dplyr)
library(tidyr)
library(echarts4r)
library(lubridate)
library(bslib)
library(tools)
library(readxl)

ui = fluidPage(
    titlePanel("Analyse van Lelyrobotdata"),
    ## Sidebar

    fileInput("upload", 
              "Upload het xlsx bestand",
              accept = ".xlsx"),
    actionButton("show", "Uitleg"),
    
    sidebarLayout(
      sidebarPanel(
      sliderInput(
        'lactations',"Laktaties:",
        min = 1,max = 10,##na.rm = T),
        step = 1,
        value = c(1,3)
      ),
      sliderInput(
        'dil',"Laktatiedagen:",
        min = 1,max = 500,
        step = 1,
        value = c(1,365)
      ),
      selectInput(
        inputId = 'trait', 
        label = "Kenmerk", 
        choices = "productie",
        selected = 'productie',
        selectize = T,
        multiple = T
      ),
      dateInput("dateg",
                "Datum grafiek",
                value = Sys.Date(),
                min = floor_date(Sys.Date(), unit = "month"),
                max = Sys.Date()
      )
    ),
    mainPanel(
      ##fluidRow(column(6,echarts4rOutput("plotdil",height = "500px")),
      ##         column(6,echarts4rOutput("plotdatum",height = "500px"))),
      fluidRow(echarts4rOutput("groteplot",height = "700px"))
    )))

  
server = function(input, output,session) {
  options(shiny.maxRequestSize=30*1024^2)
    cns <- c("id",
           "datum",
           "lactatie",
           "dim",
             "productie")
    observe({
      type_txt <- ifelse(input$type == "default", "notification", input$type)
      showNotification(
        "Invoer:
        id = identificatie dier,
        datum = datum (zorg dat het goed in excel staat gespecificeerd),\n
        lactatie = lactatienummer,\n
        dim = lactatiedagen,\n
        productie = productie,\n
        de andere kolommen worden als afhankelijke variabelen ingelezen,\n
        hoofdletter of niet maakt niet uit\n",
        duration = NULL,
        id = "message"
      )
    }) |>
    bindEvent(input$show)
  
    data <- reactive({
      req(input$upload)
      d <- read_xlsx(path = input$upload$name) |> 
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
              return('<strong> Koe' + params.name +
                  '</strong><br />dil: ' + params.value[0] +
                  '<br />waarde: ' + params.value[1])
                  }
            ")) 
    })
    output$groteplot <- renderEcharts4r({
      dt() |> 
        filter(name%in%input$trait) |> 
        group_by(name,datum) |> 
        summarise(value = mean(value,na.rm = T),.groups = 'drop') |> 
        e_charts(datum) |> 
        e_line(value,symbol="none") |> 
        e_tooltip(trigger = "axis") |> # tooltip
        e_connect_group("grp") |> 
        e_datazoom(x_index = 0, type = "slider") 
    })
  }
    
    
shinyApp(ui,server,options = list(height = 500))

  

  