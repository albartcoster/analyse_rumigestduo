library(shiny)
library(dplyr)
library(tidyr)
library(echarts4r)
library(lubridate)
library(bslib)

load('data.Rda')

ui = fluidPage(
    titlePanel("Analyse van Lelyrobotdata"),
    ## Sidebar
    sidebarLayout(
      sidebarPanel(
      sliderInput(
        'lactations',"Laktaties:",
        min = 1,max = max(df$lactation,na.rm = T),
        step = 1,
        value = c(1,3)
      ),
      sliderInput(
        'dil',"Laktatiedagen:",
        min = 1,max = max(df$dim,na.rm = T),
        step = 1,
        value = c(1,365)
      ),
      selectInput(
        inputId = 'trait', 
        label = "Kenmerk", 
        choices = colnames(df)[-c(1:7)],
        selected = 'production',
        selectize = T,
        multiple = T
      ),
      dateInput("dateg",
                "Datum grafiek",
                value = max(df$datetime),
                min = min(df$datetime),
                max = max(df$datetime)
      )
    ),
    mainPanel(
      fluidRow(column(6,echarts4rOutput("plotdil",height = "500px")),
               column(6,echarts4rOutput("plotdatum",height = "500px"))),
      fluidRow(echarts4rOutput("groteplot",height = "700px"))
    )))
  
  server = function(input, output,session) {
    dt <- reactive({
       df |> 
        filter(production>0&
               dim>0&
               lactation>0&
                 lactation>=input$lactations[1]&
                 lactation<=input$lactations[2]&
                 input$dil[1]<=dim&
                 dim<input$dil[2]) |>
        ungroup()|>
        pivot_longer(cols = c(8:12,14:21),names_repair = 'minimal')|> 
        filter(!is.na(value))
        })
    output$plotdil = renderEcharts4r({
      dt() |> 
        filter(name == "production") |> 
        group_by(datetime) |> 
        summarise(dim = mean(dim,na.rm = T)) |> 
        e_charts(datetime) |> 
        e_line(dim,symbol="none") |> 
        e_group("grp")
    })
    output$plotdatum = renderEcharts4r({
      dt() |>
        filter(name%in%input$trait[1]&
                 datetime==input$dateg) |>
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
        group_by(name,datetime) |> 
        summarise(value = mean(value,na.rm = T)) |> 
        e_charts(datetime) |> 
        e_line(value,symbol="none") |> 
        e_tooltip(trigger = "axis") |> # tooltip
        e_connect_group("grp") |> 
        e_datazoom(x_index = 0, type = "slider") 
    })
  }
    
    
shinyApp(ui,server,options = list(height = 500))

  

  