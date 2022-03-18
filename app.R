source("funLib.r")

#User Interface
ui <- fluidPage(
  titlePanel("Refuge restroom for all"),
  tabsetPanel(
    tabPanel("Présentation", "contents"),
    tabPanel("Tableau", sidebarLayout(
      sidebarPanel(
        selectInput(
          "city",
          label = "Choose a city",
          choices = c("Tours", "London")
        ),
        wellPanel(
          splitLayout(
            textInput("lng", label = 'lng', value = "2.349014" ),
            textInput("lat", label = 'lat', value = "48.864716")
          ),
          actionButton("coordOk", label = 'valider')
        ),
        wellPanel(
          checkboxInput("ada", "accessible"),
          checkboxInput("unisex", "unisexe"),
        )
      ),
      mainPanel(dataTableOutput("table_city2"),
                dataTableOutput("table_city"))
    )),
    tabPanel("Carte", leafletOutput('map')),
    tabPanel("Statistiques")
  )
)

#Server code (R)
server <- function(input, output) {
  output$table_city <- renderDataTable({
    ville_to_df(input$ada, input$unisex, input$city)
  })
  output$map <- renderLeaflet({
    display_map(input$ada, input$unisex, input$city)
  })
  df2 <- eventReactive(input$coordOk,{
    lat <- input$lat
    long <- input$lng
    query <- list(lng=long, lat=lat, per_page=100, ada=input$ada, unisex=input$unisex)
    df_trans <- req_to_df(urlLoc, query = query) 
  })
  output$table_city2 <- renderDataTable({df2()})
}

# to run app
shinyApp(ui = ui, server = server)