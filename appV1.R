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
            numericInput("lng", label = 'lng', 0),
            numericInput("lat", label = 'lat', 0)
          ),
          actionButton("coordOk", label = 'valider')
        ),
        wellPanel(
          checkboxGroupInput("para", "parametres",
                             choices = c("ada" = "TRUE", "unisex" = "TRUE")),
          actionButton("paraOk", label = 'valider')
        )
      ),
      mainPanel(textOutput("selected_city"),
                dataTableOutput("table_city"))
    )),
    tabPanel("Carte", leafletOutput('map')),
    tabPanel("Statistiques")
  )
)

#Server code (R)
server <- function(input, output) {
  #output$selected_city <-
  #  renderText({ input$para
  #})
  output$table_city <- renderDataTable({
    variable <- variable(input$city)
    coord <- coord_city(variable)
    query <- list(lng=coord[[1]], lat=coord[[2]], per_page=100)
    #query <- list(lng=coord[[1]], lat=coord[[2]],
    #              per_page=100, ada=input$para[1],
    #              unisex=input$para[2])
    df <- req_to_df(urlLoc, query = query) %>% df_transfo()
    })
  
  observeEvent({input$para[1];input$para[2];input$city},{
    variable <- variable(input$city)
    coord <- coord_city(variable)
    query <- list(lng=coord[[1]], lat=coord[[2]],
                  per_page=100, ada=str(input$para[1]),
                  unisex=str(input$para[2]))
    df <- req_to_df(urlLoc, query = query) 
  })
  
  
  output$map <- renderLeaflet({
    variable <- variable(input$city)
    display_map(variable)
  })
}

# to run app
shinyApp(ui = ui, server = server)