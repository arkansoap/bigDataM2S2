source("funLib.r")


#User Interface
ui <- fluidPage(titlePanel("Refuge restroom for all"),
                tabsetPanel(
                  tabPanel("Présentation", "contents"),
                  tabPanel("Tableau", sidebarLayout(
                    sidebarPanel(
                      "contents",
                      selectInput(
                        "city",
                        label = "Choose a city",
                        choices = c("Tours", "London")
                      )
                    ),
                    mainPanel(textOutput("selected_city"),
                              dataTableOutput("table_city"))
                  )),
                  tabPanel("Carte", leafletOutput('map') ),
                  tabPanel("Statistiques")
                ))

#Server code (R)
server <- function(input, output) {
  output$selected_city <-
    renderText({
      paste("Vous avez selectionne" , input$city)
    })
  output$table_city <- renderDataTable({
    variable <- switch(
      input$city,
      "Tours" = list(lng="0.689797", lat="47.390185"),
      "London" = list(lat="51.509865", lng="-0.118092", per_page="100")
    )
    req_to_df(urlLoc, query = variable) %>% df_transfo()
  })
  output$map <- renderLeaflet({
    variable <- switch(
      input$city,
      "Tours" = "coord_Tours",
      "London" = "coord_London"
    )
    if (variable == "coord_Tours"){
      map = bigmap(0.689797,47.390185)
      df = req_to_df(urlLoc, query_Tours)
      map %>% addMarkers(data = df, lng = ~longitude, lat = ~latitude)
    }
    else if ( variable == "coord_London"){
      map = bigmap(-0.118092, 51.509865)
      df = req_to_df(urlLoc, query_Londres)
      map %>% addMarkers(data = df, lng = ~longitude, lat = ~latitude)
    }
  })
}

# to run app
shinyApp(ui = ui, server = server)