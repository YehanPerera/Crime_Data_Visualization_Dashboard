library(bslib)
library(thematic)
library(forcats)
library(RColorBrewer)
library(pals)
library(shiny)
library(ggmap)
library(ggplot2)
library(dplyr)
library(plotly)
library(TSstudio)
library(xts)
library(hexbin)
library(bslib)
library(ggrepel)
library(shinyWidgets)

load(".RData")

api_key = Sys.getenv("API_KEY")
ggmap::register_google(key = api_key, write = TRUE)

thematic::thematic_on()

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)


ui <- fluidPage(
  theme = bs_theme(bootswatch = "cyborg"),
  
  #setBackgroundImage(src = "C:/Users/malis/Pictures/wallhaven/wallhaven-48vm9y.jpg"),
  # tags$body(
  # tags$img(
  #   src = "C:/Users/malis/Pictures/wallhaven/wallhaven-48vm9y.jpg",
  #   style = 'position: absolute'
  # )
  # ),
  
  titlePanel(h3("Los Angeles Crime Data Visualization", align = "center", style = "color : maroon;")),
  
  tabsetPanel(
    tabPanel("Mapping",
             
             fluidRow(
               tags$style(
                 "
      #crime {
        opacity: 0.3;
        border-style: solid;
        border-radius: 25px;
        padding: 20px;
      }
      #crime:hover {
        opacity: 1;
      }
      #tscrime {
      }
      #scatter , #density, #hex {
        padding: 10px;
        text-align: center;
        border-radius: 12px;
        width: 32%;
      }
      #tsarea {
        font-size: 13px;
        
      }
      #dateRange {
        padding-left: 110px;
        width: 400px;
      }
      "
               ),
               
               
               column(5,
                      #
                      plotOutput("bar")
                      
               ),
               column(7,
                      #
                      plotOutput("map"),
                      absolutePanel(id = "crime", class = "panel panel-default",
                                    width = 300, fixed = T, top = 300, left = 300,
                                    draggable = T, height = "auto",
                                    div(
                                      selectInput("select",
                                                  h5("Crime Type"),
                                                  choice = act,
                                                  selected = 1),
                                      actionButton("density", "density"),
                                      actionButton("scatter", "scatter"),
                                      actionButton("hex", "hex"),
                                      br(),
                                      br(),
                                      checkboxInput("pointers", "allow city pointer")
                                    )
                      )))),
    tabPanel("Time Series",
             fluidRow(
               column(12,
                      wellPanel(plotlyOutput("ts"), 
                                checkboxGroupButtons("check",
                                                     choices = choiceAct,
                                                     selected = 1),
                      ),
                      style = "padding : 10px;"
               ),
             ),
             fluidRow(
               # column(6,
               #       plotOutput("weapon"), style = "padding-top : 20px; padding-bottom : 20px"),
               #       column(6,
               #       plotOutput("premis"), style = "padding-top : 20px; padding-bottom : 20px"
               #       )
               # )
               column(12,
                      wellPanel(plotlyOutput("areats")),
                      style = "padding : 10px;"),
               absolutePanel(id = "tsarea", class = "panel panel-default",
                             fixed = F, height = "50", width = 300, left = 100, top =635,
                             selectInput("areats",
                                         h5("Areas"),
                                         choice = area,
                                         selected = 1)
               )
             ),
             fluidRow(
               column(6, 
                      dateRangeInput('dateRange',
                                     label = 'Input date range',
                                     min = min(myCrime$stdTime),
                                     start =min(myCrime$stdTime), 
                                     max = max(myCrime$stdTime),
                                     end = max(myCrime$stdTime)
                                     ),
                      style = "padding-top : 20px; padding-bottom : 20px;",
                      plotOutput("TSareamap")
                      ),
               column(6,
                      wellPanel(plotOutput("crimeBar")),
                      style = "padding-top : 190px; padding-bottom : 20px;",
                      )
             )
             ),
    tabPanel("Victim Demographics",
             fluidRow(
               column(6, plotOutput("areaMap"), 
                      style = "padding-top : 20px; padding-bottom : 100px;"
               ),
               column(6, wellPanel(plotOutput("raceBar")),
                      style = "padding-top : 100px; padding-bottom : 20px")
             ),
             fluidRow(
               column(6, wellPanel(plotOutput("genderPie"),
               ),
               style = "padding-top : 100px; padding-bottom : 20px"),
               column(6, wellPanel(plotOutput("ageHistogram")
               ),
               style = "padding-top : 100px; padding-bottom : 20px"
               ),  
               absolutePanel(
                 selectInput("selectArea",
                             h5("Area"),
                             choice = area,
                             selected = 1),
                 id = "tscrime", class = "panel panel-default",
                 top = 110, right = 30, width = 200, fixed = F,
                 draggable = F, height = "auto"),
               absolutePanel(selectInput("selectCrime",
                                         h5("Crime Type"),
                                         choice = act,
                                         selected = 1), 
                             id = "tscrime", class = "panel panel-default",
                             top = 110, right = 230, width = 200, fixed = F,
                             draggable = F, height = "auto"
               )
             )),
    tabPanel("Other",
             fluidRow(style = 'padding : 10px',
                      column(6,
                             absolutePanel(selectInput("selectTS",
                                                       h5("Crime Type"),
                                                       choice = act,
                                                       selected = 1),
                                           id = "tscrime", class = "panel panel-default",
                                           top = 110, right = 40, width = 200, fixed = T,
                                           height = "auto"),
                             plotOutput("weapon"), style = "padding-top : 100px; padding-bottom : 20px"),
                      column(6,
                             plotOutput("premis"), style = "padding-top : 100px; padding-bottom : 20px"
                      )
             )
    )
  ))


server <- function(input, output) {
  
  crimeReact <- reactive({
    myCrime %>%
      filter(crimeType == input$select) %>%
      filter(LON != 0 & LAT != 0) 
    #sample_frac(input$slider / 100)
  })
  
  weaponReact <- reactive({
    myCrime %>%
      filter(crimeType == input$selectTS) %>%
      group_by(`Weapon Desc`) %>%
      tally() %>%
      arrange(desc(n)) %>%
      na.omit() %>%
      top_n(7)
  })
  
  premisReact <- reactive({
    myCrime %>%
      filter(crimeType == input$selectTS) %>%
      group_by(`Premis Desc`) %>%
      tally() %>%
      arrange(desc(n)) %>%
      na.omit() %>%
      top_n(7)
  })
  
  v <- reactiveValues(type = NULL)
  
  observeEvent(input$density, {
    v$type = "density"
  })
  
  observeEvent(input$scatter, {
    v$type = "scatter"
  })
  
  observeEvent(input$hex, {
    v$type = "hex"
  })
  
  
  output$map <- renderPlot({
    colfunc = colorRampPalette(c("red3", "grey20"))
    myCols = colfunc(21)
    names(myCols) = levels(factor(names(table(crimeReact()$`AREA NAME`)), levels = names(sort(table(crimeReact()$`AREA NAME`), decreasing = T))))
    
    p = get_googlemap(center = "los Angeles") %>%
      ggmap()
    if (is.null(v$type)) {
      print(p)
    } else if (v$type == "scatter") {
      q = p +geom_point(data = crimeReact(),
                        aes(x = LON, y = LAT, color = `AREA NAME`), alpha = 0.25, size = 0.5) +
        scale_color_manual(values = myCols) +
        theme(legend.position = "none")
      print(q)
    } else if (v$type == "density") {
      q = p + stat_density2d(data = crimeReact(),
                             aes(x = LON, y = LAT, fill = ..level..), geom = "polygon", bins = 30, alpha = 0.25) +
        scale_fill_gradient(high = "red3", low = "grey20")
      print(q)
    } else if (v$type == "hex") {
      q = p + geom_hex(data = crimeReact(), 
                       aes(x = LON, y = LAT), binwidth = c(0.01, 0.01), alpha = 0.8) +
        scale_fill_gradient(high = "red3", low = "grey20")
      print(q)
    }
    print(input$pointers)
    if (input$pointers == T) {
      q + geom_point(data = areas, mapping = aes(x = LON_name, y = LAT_name), shape = 19, size = 3, color = "blue") +
        #geom_text(data = areas, mapping = aes(x = LON_name, y = LAT_name, label = `AREA NAME`), vjust = -1, color = "black", size = 4) +
        geom_label_repel(data = areas, aes(label = `AREA NAME`, x = LON_name, y = LAT_name),
                         fill = "white",
                         family = "sans",
                         box.padding = 0.5,
                         color = "grey22",
                         max.overlaps = Inf,
                         arrow = arrow(length = unit(0.01, "npc"))) 
    }
    
  },
  {width = 700},
  {height = 600}
  )
  
  output$bar <- renderPlot({
    colfunc = colorRampPalette(c("red3", "grey20"))
    
    myCols = colfunc(21)
    names(myCols) = levels(factor(names(table(crimeReact()$`AREA NAME`)), levels = names(sort(table(crimeReact()$`AREA NAME`), decreasing = T))))
    
    p <- ggplot(crimeReact(), aes(y = fct_rev(fct_infreq(`AREA NAME`)), fill = `AREA NAME`)) +
      geom_bar(stat = 'count') +
      #scale_fill_gradient(high = "red", low = "black") +
      scale_fill_manual(values = myCols) +
      theme(legend.position = "none", 
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 20))
    
    p
  },
  {width = 400},
  {height = 600})
  
  output$ts <- renderPlotly({
    if (is.null(input$check)) {
      ts_plot(ts(),
              slider = T,
              Xgrid = T, 
              Ygrid = T,
              title = "crime rate by type",
              Ytitle = "count") %>%
        layout(paper_bgcolor = "#2b2b2b",
               plot_bgcolor = "#2b2b2b",
               font = list(color = "white"))
    } else {
      ts_plot(mer[, as.integer(input$check)],
              slider = T,
              Xgrid = T, 
              Ygrid = T,
              title = "crime rate by type",
              Ytitle = "count") %>%
        layout(paper_bgcolor = "#2b2b2b",
               plot_bgcolor = "#2b2b2b",
               font = list(color = "white"))
    }
  })
  
  output$areats <- renderPlotly({
    ts_plot(fmer[, input$areats],
            slider = T,
            Xgrid = T, 
            Ygrid = T,
            title = "crime rate by area",
            color = "lightblue",
            Ytitle = "count") %>%
      layout(paper_bgcolor = "#2b2b2b",
             plot_bgcolor = "#2b2b2b",
             font = list(color = "white"))
  })
  
  output$weapon <- renderPlot({
    ggplot() +
      geom_bar(weaponReact(), mapping = aes(x = `Weapon Desc`, y = n, fill = `Weapon Desc`), stat = "identity") +
      theme(axis.text.x = element_blank(),
            text = element_text(color = "white")) +
      scale_fill_brewer(palette = "PuRd") +
      labs(title = "Top weapons used for each crime")
    
  })
  
  output$premis <- renderPlot({
    ggplot() +
      geom_bar(premisReact(), mapping = aes(x = `Premis Desc`, y = n, fill = `Premis Desc`), stat = "identity") +
      theme(axis.text.x = element_blank(),
            text = element_text(color = "white")) +
      scale_fill_brewer(palette = "Reds") + 
      labs(title = "Top premises each crime occured")
    
  })
  
  output$areaMap <- renderPlot({
    z = myCrime %>%
      filter(crimeType == input$selectCrime) %>%
      filter(`AREA NAME` == input$selectArea) %>%
      filter(LON != 0 & LAT != 0) %>%
      filter(!is.na(race))
    # sample_frac(input$slider / 100)
    gmap = get_googlemap(center = c(x = as.numeric(cood[cood$`AREA NAME` ==  input$selectArea, ][2]),
                                    y = as.numeric(cood[cood$`AREA NAME` == input$selectArea, ][3]))
                         , zoom = 13) %>%
      ggmap()
    
    gmap +
      geom_jitter(data = z,
                  aes(x = LON, y = LAT, color = race), width = 0.001, height = 0.001) +
      scale_color_manual(values = rev(as.vector(alphabet(20)))) +
      labs(title = "Mapping of the crime location") +
      theme(legend.position = "bottom",
            text = element_text(size = 15, color = "white"),
            legend.title = element_blank(),
            axis.text = element_text(size = 10)) 
    
  },
  {width = 600},
  {height = 550}) 
  
  output$raceBar <- renderPlot({
    z = myCrime %>%
      filter(crimeType == input$selectCrime) %>%
      filter(`AREA NAME` == input$selectArea) %>%
      filter(LON != 0 & LAT != 0) %>%
      filter(race != "unknown") %>%
      filter(!is.na(race))
    
    ggplot(z) +
      geom_bar(mapping = aes(x = fct_infreq(race), fill = race), stat = "count") +
      scale_fill_manual(values = rev(as.vector(alphabet(20)))) +
      labs(x = "race", title = "Victim count by race") +
      theme(text = element_text(color = "white", size = 15),
            axis.text.x = element_text(angle = 30, size = 10),
            axis.text = element_text(size = 10))
  })
  
  output$genderPie <- renderPlot({
    z = myCrime %>%
      filter(crimeType == input$selectCrime) %>%
      filter(`AREA NAME` == input$selectArea) %>%
      filter(LON != 0 & LAT != 0) %>%
      filter(`Vict Sex` == "F" | `Vict Sex` == "M") %>%
      filter(!is.na(race))
    
    ggplot(z) +
      geom_bar(mapping = aes(x = "", fill = `Vict Sex`), stat = "count", color = "white",
               width = 1) +
      coord_polar(theta = 'y') +
      scale_fill_manual(values = as.vector(alphabet(20))) +
      labs(title = "Victim count by gender") +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            text = element_text(size = 15, color = "white"))
    
  }) 
  
  output$ageHistogram <- renderPlot({
    z = myCrime %>%
      filter(crimeType == input$selectCrime) %>%
      filter(`AREA NAME` == input$selectArea) %>%
      filter(LON != 0 & LAT != 0) %>%
      filter(`Vict Age` != 0) %>%
      filter(!is.na(race))
    
    ggplot(z) +
      geom_histogram(mapping = aes(x = `Vict Age`), fill = "lightgreen", binwidth = 1) +
      labs(title = "Age distribution of victims") +
      theme(text = element_text(color = "white", size = 15), 
             axis.text = element_text(size = 10))
  })
  
  
  output$TSareamap <- renderPlot({
    z = myCrime %>%
      filter(`AREA NAME` == input$areats) %>%
      filter(stdTime >= input$dateRange[1] & stdTime <= input$dateRange[2]) %>%
      filter(LON != 0 & LAT != 0)
    
    tempcood = z %>%
      filter(LON != 0 & LAT != 0) %>%
      summarise_at(vars(LON, LAT), list(name = mean))
    
    print(z)
    print(as.numeric(tempcood[1]))
    # sample_frac(input$slider / 100)
    gmap = get_googlemap(center = c(x = as.numeric(tempcood[1] - 0.01),
                                    y = as.numeric(tempcood[2] + 0.01))
                         , zoom = 13) %>%
      ggmap()
    
    gmap +
      geom_jitter(data = z,
                  aes(x = LON, y = LAT, color = crimeType), width = 0.001, height = 0.001) +
      scale_color_manual(values = rev(as.vector(alphabet(20)))) +
      labs(title = "Mapping of the crime location") +
      theme(legend.position = "bottom",
            text = element_text(size = 15, color = "white"),
            legend.title = element_blank(),
            axis.text = element_text(size = 10)) 
    
  },
  {width = 600},
  {height = 550})
  
  output$crimeBar <- renderPlot({
    
    z = myCrime %>%
      filter(`AREA NAME` == input$areats) %>%
      filter(stdTime >= input$dateRange[1] & stdTime <= input$dateRange[2]) %>%
      filter(LON != 0 & LAT != 0)
    
    ggplot(z) +
      geom_bar(mapping = aes(x = fct_infreq(crimeType), fill = crimeType), stat = "count") +
      scale_fill_manual(values = rev(as.vector(alphabet(20)))) +
      labs(x = "crimeType", title = "Victim count by crime type") +
      theme(text = element_text(color = "white", size = 15),
            axis.text.x = element_text(angle = 30, size = 10),
            axis.text = element_text(size = 10))
  })
  
  
}








# Run the application 
shinyApp(ui = ui, server = server)

























