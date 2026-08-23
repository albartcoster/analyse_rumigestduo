library("shiny")
library("tidyverse")
library("bslib")
library("ggplot2")
library("ggiraph")
library("bslib")
library("thematic")

load("data/data.Rdata")
##ddff <- dff %>% pivot_longer(cols = !c(datum,farm,week))

options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)

thematic_shiny()

# UI with bslib theme, pageNavbar, and bookmarking support
ui <- function(request){
  page_sidebar(
  title = "App analyse data Proef Rumigest DUO",
  theme = bs_theme(version = 5, bootswatch = "minty"), # Use bslib theme
  sidebar = sidebar(
    selectInput(
      inputId = 'farm',
      label = "Bedrijf",
      choices = unique(dff$farm),
      selected = unique(dff$farm),
      selectize = T,
      multiple = T
    ),
    ##renvbookmarkButton() # Adds the bookmark button
  ),
  navset_card_underline(
    id = "hoofd_tabs", # ID is verplicht zodat bookmarking het actieve tabblad onthoudt
    title = "Resultaten",
    full_screen = TRUE, # Maakt de gehele kaart (inclusief panelen) maximaliseerbaar
    
    # Panel 1: Tankdata
    nav_panel(
      id = "tankanalyse",
      title = "Tankanalyse",
      icon = icon("chart-line"),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          title = "Opties",
          position = "right",
          selectInput(
            inputId = 'vartanks',
            label = "Parameter",
            choices = unique(dff$name),
            selected = unique(dff$name[1]),
            selectize = T,
            multiple = T
          ),
        ),
        
      girafeOutput(outputId = "groteplot")
    )),
    
    # Panel 2: Voerefficiëntie
    nav_panel(
      id = 'voereff',
      title = "Data voeropname",
      icon = icon("chart-line"),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          title = "Opties",
          position = "right",
          selectInput(
            inputId = 'varfes',
            label = "Parameter",
            choices = unique(tabfe$name),
            selected = unique(tabfe$name[1]),
            selectize = T,
            multiple = T
          ),
          dateRangeInput(inputId = 'fedates',
                         "Begin en einddatum",
                         start = min(tabfe$date),
                         end = max(tabfe$date),
                         min = min(tabfe$date),
                         max = max(tabfe$date)
          )
        ),
        girafeOutput(outputId = "feplot")
      )),
    
    # Panel 3: Dagproducties
    nav_panel(
      id = 'productie',
      title = "Dagproductiedata",
      icon = icon("chart-line"),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          title = "Opties",
          position = "right",
          selectInput(
            inputId = 'varpres',
            label = "Parameter",
            choices = unique(prdata$name),
            selected = unique(prdata$name[1]),
            selectize = T,
            multiple = T
          ),
          dateRangeInput(inputId = 'prdates',
                         "Begin en einddatum",
                         start = min(prdata$date_time),
                         end = max(prdata$date_time),
                         min = min(prdata$date_time),
                         max = max(prdata$date_time)
          ),
          sliderInput(
            'lactations',"Laktaties:",
            min = 1,max = max(prdata$lactation),
            step = 1,
            value = c(3,8)
          ),
          sliderInput(
            'dil',"laktatiedagen:",
            min = 1,max = 500,
            step = 1,
            value = c(1,365)
          )
        ),
        girafeOutput(outputId = "prplot")
      )),
    )
  )}

  



server = function(input, output,session) {
  dt <- reactive({
    dff |> 
      filter(value>0,
             !is.na(value),
             farm%in%input$farm,
            name%in%input$vartanks)
    })
  
  dfe <- reactive({
    tabfe |> 
      filter(value>0,
             !is.na(value),
             user%in%input$farm,
             name%in%input$varfes,
             between(date,input$fedates[1],input$fedates[2]))
  })
  
  dpr <- reactive({
    prdata |> 
      mutate(week = floor_date(date_time,unit="week")) |> 
      filter(value>0,
            !is.na(value),
            farm%in%input$farm,
            name%in%input$varpres,
            between(date_time,input$prdates[1],input$prdates[2]),
            between(lactation,input$lactations[1],input$lactations[2]),
            between(dim,input$dil[1],input$dil[2])) |>
      group_by(farm,name,week) |>
      summarize(value = mean(value))
  })
  
  
  ## plot tanks
  output$groteplot <- renderGirafe({
    tplot <- dt()%>% 
      ggplot( mapping=aes(x= datum,
                          y = value,
                          color = farm,
                          tooltip=farm,
                          data_id=farm))+
      geom_point_interactive(size =0.3) + 
      geom_smooth_interactive(se = FALSE,span = 0.6,
                              hover_nearest = T) + 
      annotate(
        "rect",
        xmin = as.Date('2026-04-10'), xmax = as.Date('2026-06-01'),  # X range of shaded area
        ymin = -Inf, ymax = Inf,  # Full vertical range
        alpha = 0.4, fill = "grey"
      ) +
      labs(
        x = "Datum", y = "Parameter",
        color = "Bedrijf"
      ) +
      theme_bw() + 
      theme(legend.position = 'none') +
      facet_wrap(~ name, scales = "free_y",ncol =1) 
    
    girafe(
      ggobj = tplot,
      options = list(
        opts_hover(css = ''),
        opts_sizing(rescale = TRUE),
        opts_hover_inv(css = "opacity:0.1;"),
        width_svg = 5,   # Canvas width in inches
        height_svg = 5*length(input$var)  
      )
    )
  })
    
    ## plot voerkosten
    output$feplot <- renderGirafe({
      tplot <- dfe()%>% 
        ggplot( mapping=aes(x= date,
                            y = value,
                            color = user,
                            tooltip=user,
                            data_id=user))+
        geom_point_interactive(size =0.3) + 
        geom_smooth_interactive(se = FALSE,span = 0.6,
                                hover_nearest = T) + 
        annotate(
          "rect",
          xmin = as.Date('2026-04-10'), xmax = as.Date('2026-06-01'),  # X range of shaded area
          ymin = -Inf, ymax = Inf,  # Full vertical range
          alpha = 0.4, fill = "grey"
        ) +
        labs(
          x = "Datum", y = "Parameter",
          color = "Bedrijf"
        ) +
        theme_bw() + 
        theme(legend.position = 'none') +
        facet_wrap(~ name, scales = "free_y",ncol =1) 
      
      girafe(
        ggobj = tplot,
        options = list(
          opts_hover(css = ''),
          opts_sizing(rescale = TRUE),
          opts_hover_inv(css = "opacity:0.1;"),
          width_svg = 5,   # Canvas width in inches
          height_svg = 5*length(input$var)  
        )
      )})
      
      ## plot producties
      output$prplot <- renderGirafe({
        tplot <- dpr()%>% 
          ggplot( mapping=aes(x= week,
                              y = value,
                              color = farm,
                              tooltip=farm,
                              data_id=farm))+
          geom_line_interactive(size =0.3) + 
          annotate(
            "rect",
            xmin = as.Date('2026-04-10'), xmax = as.Date('2026-06-01'),  # X range of shaded area
            ymin = -Inf, ymax = Inf,  # Full vertical range
            alpha = 0.4, fill = "grey"
          ) +
          labs(
            x = "Datum", y = "Parameter",
            color = "Bedrijf"
          ) +
          theme_bw() + 
          theme(legend.position = 'none') +
          facet_wrap(~ name, scales = "free_y",ncol =1) 
        
        girafe(
          ggobj = tplot,
          options = list(
            opts_hover(css = ''),
            opts_sizing(rescale = TRUE),
            opts_hover_inv(css = "opacity:0.1;"),
            width_svg = 5,   # Canvas width in inches
            height_svg = 5 
          )
        )
    
  })
}


shinyApp(ui,server,enableBookmarking = "url",
         options = list(height = 500))



