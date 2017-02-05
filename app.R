library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(DT) ;library(ggplot2)

ui <- shinyUI(fluidPage(
              tags$head(tags$style(
                HTML('
                     #radiobox {
                     background-color: rgba(255,255,255,0.8);
                     width: 160px;
                     padding: 10px;
                     box-shadow: 0 0 15px rgba(0,0,0,0.2);

                     }
                     
                     ')
  )),
  fluidRow(
  column(7, offset = 1,
         br(),
         div(h3("Hospitals in England")),
         div(h4("Interactive report showing relative mortality rates and staff recommendation levels in hospitals in England. Data sourced from nhs.publishmydata.com")),
         div(h4(textOutput("title"), align = "center"), style = "color:black"),
         div(h5(textOutput("period"), align = "center"), style = "color:black"),
         br())),
  fluidRow(
    column(7, offset = 1,
           tabsetPanel(
             tabPanel("Map",leafletOutput("map", height="600"),
                      br(),absolutePanel(id="radiobox", top = 50, right = 20, radioButtons("display", "Colour points by:",c("Mortality" = "mort","Staff Recommend" = "reco")))
                      ),
             tabPanel("Table",DT::dataTableOutput("table")),
             tabPanel("Chart",br(),plotOutput("distPlot"))
           )
    ),
    column(3,
           
           tabsetPanel(
             tabPanel("Filters",br(),
                      sliderInput("mortalityrange", 
                                  "Filter hospitals according to their relative mortality rate", 
                                  min = 0.6, 
                                  max = 1.2,
                                  step = 0.01,
                                  value = c(0.6,1.2)),
                      sliderInput("recorange", 
                                  "Filter hospitals according to the percentage of staff who would recommend the hospital", 
                                  min = 46, 
                                  max = 93, 
                                  value = c(46,93)),
                      div("Each hospital in England is shown as a point on the map to the left of this page. Clicking on the tabs at the top will also show a table of data and a scatterplot."),br(),div("The points on the map are coloured according to the value of one of the two indicators chosen for this example. You can choose which indicator to colour by by selecting from the two options at the top of the map."),br(),div("The sliders above allow for dynamic filtering of the data - you can use these to only show hospitals that meet certain criteria."),br(),div("As well as affecting the map, the sliders also filter the results shown in the table and chart. The sliders work together to filter the data. If you do not want to filter the hospitals by a particular indicator, leave the slider for that indicator at its fullest extent.")
             ),
             tabPanel("Features",br(),h4("Features and Points"),br(),div("The datasets were extracted from the datastore using the "),a("SPARQL Endpoint", href="http://nhs.publishmydata.com/sparql"), (". The full query can be found in "),a("this Gist", href="https://gist.github.com/northernjamie/bb5b3518fd462ee0641e4d6ebf465873"),br(),
                      br(),div("The data powering this tool is stored in a csv file on this webserver. It could easily be recoded to use PMD API to get data automatically from the linked datastore."),br(),div("Hospitals were geocoded by returning each hospital's postcode and geomatching with Office for National Statistics data, using R.",br(),div("The sliders allow for dynamic filtering of the data"),br(),div("Clicking a point on a map will show the associated data in a popup. These popups are hyperlinked back to the appropriate page on the linked datastore."),br(),div("Because the data is standardised in the datastore, it would be trivial to add additional hospital indicators."),br(),div("The data here appears to based on the Trust that the hospital belongs to, rather than individual hospitals. This would explain geographical clustering of data."))))
           
           
    ))))

server <- shinyServer(function(input, output, session) {
  
  
  
  lat <- 53.542788
  lng <- -2.144708
  zoom <- 6
  
  hospitaldata <- read.csv("hospital_georef_data.csv")
  
  selected <- reactive({
    subset(hospitaldata,
           mortalityvalue < input$mortalityrange[2] & mortalityvalue >= input$mortalityrange[1] & recommendvalue < input$recorange[2] & recommendvalue >= input$recorange[1])
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lat = lat, lng = lng, zoom = zoom)
    
  })
  
  observe({
    
    hospdata = selected()
    
    
    if(input$display == 'mort') {
      circlesize <- (~mortalityvalue*7);
      circlecolour <- hospdata$mortalityvalue;
      indurl <- hospdata$ind1url;
      obsurl <- hospdata$obs1url;
      legendtitle <- "Relative mortality rate";
      qpal <- colorNumeric(
        palette = "Spectral",
        domain = hospdata$mortalityvalue
      )
      
    }
    else { 
      circlesize <- (~recommendvalue/10);
      circlecolour <- hospdata$recommendvalue;
      indurl <- hospdata$ind2url;
      obsurl <- hospdata$obs2url;
      legendtitle <- paste("% of staff who would", "recommend this hospital", sep ="<br/>");
      qpal <- colorNumeric(
        palette = "Spectral",
        domain = hospdata$recommendvalue
      )
      
    }
    
    
    
    output$table <- DT::renderDataTable({
      data.frame(hospdata[c(7,11,14)])},
      options = list(order = list(2,'desc'))
    )
    
    
    leafletProxy("map") %>% 
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(data = hospdata,
                       radius = circlesize,
                       lng = ~X,
                       lat = ~Y,
                       opacity = 1,
                       fillOpacity = 1,
                       fillColor = ~qpal(circlecolour),
                       popup = ~paste("<strong><a href='",hospcode,"'>",hospname,"</a></strong><br/><br/><a href='",indurl,"'>",legendtitle,"</a>: <a href='",obsurl,"'>",circlecolour,"</a>"),
                       color = '#444444',
                       weight = 1) %>%
      addLegend(pal = qpal, values = circlecolour, opacity = 0.7,
                position = 'bottomleft', 
                title = paste0(legendtitle))
    
    
    output$distPlot <- renderPlot({
      
      
      ggplot(hospdata, aes(x=recommendvalue, y=mortalityvalue,label=hospname)) + geom_point(shape=21, size=6, color="blue",fill="red", alpha=0.3) + stat_smooth(method = "lm", col = "red") + ggtitle('Scatterplot Showing Relative Mortality vs Staff Recommending Hospital') + labs(x='% of staff respondents saying they would recommend the hospital', y='Relative mortality rate') + theme_bw()
      
      
    })
  })
  
  
  
})

shinyApp(ui, server)