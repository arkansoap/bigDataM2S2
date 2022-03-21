source("funLib.r")

#User Interface
ui <- fluidPage(
  titlePanel("Refuge restroom for all"),
  tabsetPanel(
    tabPanel("Présentation", splitLayout(
      img(src = "logo.gif"),
      verticalLayout(
        h1("Toilettes LGBT Friendly"),
        h2("Objectif de l'application"),
        p("Cette application a pour but de répérer les toilettes LGBT Friendly proche de l'utilisateur."),
        p("Rendez-vous sur le site",a("Refuge restroom", href = "https://www.refugerestrooms.org/"), "pour plus de details.",br(), "Vous aurez la possibilite de laisser des commentaires et appreciation des lieux visites"),
        p("Pour aider à développer la base de données, vous pouvez proposer",
          a("de nouveaux lieux.", href = "https://www.refugerestrooms.org/restrooms/new?")),
        h2("Objectif pedagogique"),
        p("L'objectif est principalement pédagogique, il s'agissait d'apprendre à développer une",br(), "application Rshiny et découvrir le fonctionnement de certains packages"),
        br(),
        p("For an introduction and live examples, visit the ",
          a("Shiny homepage.", 
            href = "http://shiny.rstudio.com"))
        )
    )),
    tabPanel("Tableau", sidebarLayout(
      sidebarPanel(
        selectInput(
          "mode",
          label = "choix d'affichage par liste ou par coords",
          choices = c("liste", "coord")
        ),
        wellPanel(
          selectInput(
            "city",
            label = "Choose a city",
            choices = c("Tours", "London", "Berlin", "New-York")
          ),
          wellPanel(
            splitLayout(
              textInput("lng", label = 'lng', value = "2.349014" ),
              textInput("lat", label = 'lat', value = "48.864716")
            ),
            actionButton("coordOk", label = 'valider')
          )
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
    tabPanel("Statistiques", tabsetPanel(
      tabPanel("Répartition par pays",sidebarLayout(
        sidebarPanel(selectInput("choix",
                                 label = "Classement par pays ou zoom sur le pays sélectionné",
                                 choices = c("Pays", "USA", "Canada", "Australie", "Royaume-Uni", "France", "Allemagne", "Nouvelle-Zélande", "Suède", "Irlande")
        )),
        mainPanel(plotOutput("classement"))
      )),
      tabPanel("Répartition par date d'ajout", sidebarLayout(
        sidebarPanel(selectInput("choix2",
                                 label = "Evolution des ajouts par mois ou par année",
                                 choices = c("Mois", "Année"))),
        mainPanel(plotlyOutput("evolution"))
      )),
    ))
  )
)

#Server code (R)
server <- function(input, output) {
  
  dfCoord <- eventReactive(c(input$coordOk, input$ada, input$unisex),{
    lat <- input$lat
    long <- input$lng
    query <- list(lng=long, lat=lat, per_page=100, ada=input$ada, unisex=input$unisex)
    df_trans <- req_to_df(urlLoc, query = query)})
  
  observe({
    if (input$mode == "liste"){
      output$table_city <- renderDataTable({
        ville_to_df(input$ada, input$unisex, input$city)
      })
      output$map <- renderLeaflet({
        display_map(input$ada, input$unisex, input$city)})
      }
    else if (input$mode == "coord"){
      output$table_city <- renderDataTable({dfCoord()})
      output$map <- renderLeaflet({
        display_map2(ada = input$ada, unisex = input$unisex, lat = input$lat, long = input$lng)
      })
    }})
  
  observe({
    if (input$choix == "Pays"){
      output$classement <- renderPlot({
        res %>% group_by(country) %>% 
          mutate(highlight = (country == "US")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(country)),10), fill= highlight) +
          geom_bar()+coord_flip()+xlab("Pays")+ylab("")+ ylim(0, 41000)+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})
    } else if (input$choix == "USA"){
      output$classement <- renderPlot({subset(res,country=="US") %>% group_by(state) %>% 
          mutate(highlight = (state == "CA")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(state)),10), fill= highlight) +
          geom_bar()+coord_flip()+xlab("Etats")+ylab("")+ ylim(0, 20000)+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Canada"){
      output$classement <- renderPlot({subset(res,country=="CA") %>% group_by(state) %>%
          mutate(highlight = (state == "ON")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(state)),10), fill=highlight) +
          geom_bar()+coord_flip()+xlab("Etats")+ylab("")+ylim(0,1400)+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Australie"){
      output$classement <- renderPlot({subset(res,country=="AU") %>% group_by(state) %>% 
          mutate(highlight = (state =="VIC")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(state)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Etats")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Royaume-Uni"){
      output$classement <- renderPlot({subset(res,country=="GB") %>% group_by(city) %>% 
          mutate(highlight = (city=="LONDON")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+ ylim(0,1350)+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "France"){
      output$classement <- renderPlot({subset(res,country=="FR") %>% group_by(city) %>% 
          mutate(highlight= (city=="PARIS")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Allemagne"){
      output$classement <- renderPlot({subset(res,country=="DE") %>% group_by(city) %>% 
          mutate(highlight = (city == "BERLIN")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Nouvelle-Zélande"){
      output$classement <- renderPlot({subset(res,country=="NZ") %>% group_by(city) %>% 
          mutate(highlight= (city=="AUCKLAND")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Suède"){
      output$classement <- renderPlot({subset(res,country=="SE") %>% group_by(city) %>%
          mutate(highlight = (city == "STOCKHOLM")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
    else if (input$choix == "Irlande"){
      output$classement <- renderPlot({subset(res,country=="IE") %>% group_by(city) %>%
          mutate(highlight = (city == "DUBLIN")) %>% ggplot() +
          aes(x = fct_lump(fct_rev(fct_infreq(city)),10), fill = highlight) +
          geom_bar()+coord_flip()+xlab("Villes")+ylab("")+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                plot.subtitle = element_text(hjust = -0.55),
                plot.title = element_text(hjust = -0.5)) +
          geom_text(stat='count', aes(label=..count..), hjust = -0.1)+
          scale_fill_manual(values = c("gray75", "#415ff0"))})}
  })
  
  observe({
    if(input$choix2 == "Mois"){
      output$evolution <- renderPlotly({ggplotly(ggplot(res) +
                                                   geom_bar(aes(x = updated_at), 
                                                            position = "dodge", stat = "count") + 
                                                   scale_x_date(date_labels = "%m-%Y")+
                                                   xlab("Année") + ylab("Nombre d'ajouts"))})}
    else if(input$choix2 == "Année"){
      output$evolution <- renderPlotly({ggplotly(
        ggplot(res) + geom_bar(aes(x = year), position = "dodge", stat = "count") +
          scale_x_continuous(breaks=c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
          xlab("Année") + ylab("Nombre d'ajouts"))})}
    
  })
  
}

# to run app
shinyApp(ui = ui, server = server)