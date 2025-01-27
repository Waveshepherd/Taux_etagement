#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#Load packages

library(shiny)
library(bslib)
'%>%' <- dplyr::'%>%'

#Load data

load("../data_prepared/ROE_data.RData")

# Define UI for application that draws a histogram

ui <- page_fluid(
  # Application title
  titlePanel("Taux d'étagement à l'échelle d'un cours d'eau"),
  
  # Sélection des inputs
  inputPanel(
    # Selection du CE
    shiny::selectInput(
      "CE",
      "Choisir un cours d' eau :",
      choices = Info$nom_CE,
      selected = "l'yères"
    )
  ),
  
  # Titre de la première partie
  tags$h1("Situation du cours d'eau"),
  
  # Tableau des infos topographiques du cours d'eau
  tableOutput(outputId = "tab_info"),
  
  # Titre de la première partie
  tags$h1("Nombre d'ouvrages"),
  
  # Tableau des infos sur les ouvrages du CE
  tableOutput(outputId = "tab_ouv"),
  
  # Titre de la première partie
  tags$h1("Répartition des hauteurs de chute"),
  
  # Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur le CE
  plotOutput("CH"),
  
  # Titre de la première partie
  tags$h1("Répartition de l'état des ouvrage sur le cours principal"),
  
  # Tableau de la répartition de l'état des ouvrages principaux sur le CE
  tableOutput("tab_etat"),
  
  # Titre de la première partie
  tags$h1("Répartition des hauteurs de chute en fonction de l'état des ouvrages sur le cours principal"),
  
  # Tableau présentant la répartition des hauteurs de chutes par état ainsi que le TE
  tableOutput("tab_TE"),
  
  # Titre de la première partie
  tags$h1("Carte du cours d'eau"),
  
  # Tableau présentant la répartition des hauteurs de chutes par état ainsi que le TE
  leaflet::leafletOutput("map"),
  
  #Télécharger la fiche au format PDF
  downloadButton("downloadData", "Télécharger la fiche au format .pdf")
  
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  ### Création des objet réactifs ###
  
  #Le nom du CE
  CE <- reactive(input$CE)
  
  ##Tableau des infos topographiques du cours d'eau
  tab_info <- reactive({
    tab_info <- dplyr::filter(Info, Info$nom_CE == CE())
  })
  
  #Sélection des informations du tableau ROE_Normandie_H relatives à notre CE
  tab_CE <- reactive({
    tab_CE <-
      ROE_Normandie_H %>% dplyr::filter(nom_CE == CE()) %>%  sf::st_drop_geometry()
  })
  
  #Sélection des ouvrages situés sur le cours principal du CE
  ouv_p <- reactive({
    ouv_p <-
      tab_CE() %>% dplyr::filter(ouv_liaison == "ouvrage principal")
  })
  
  
  ### Création des tableaux de rendus
  
  #Tableau des infos topographiques du cours d'eau
  output$tab_info <- renderTable({
    tab_info <- Info %>%  dplyr::filter(Info$nom_CE == CE()) %>% dplyr::rename(    "Code de la Masse d'eau" ="code_ME",
                                                                               "Nom du cours d'eau"="nom_CE",
                                                                               "Longueur du cours d'eau (en km)"="longueur",
                                                                               "Altitute du point le plus en aval (en m)"="alt_av",
                                                                               "Altitude du point le plus en amont (en m)"="alt_am",
                                                                               "Dénivelé naturel (en m)"="deni_nat",
                                                                               "Pente naturelle (en ‰)"="pt_nat")
    
    
  })
  
  
  #Tableau résumé de la situation des ouvrages sur le CE
  output$tab_ouv <- renderTable({
    tab_ouv <-
      ouv_p() %>%
      dplyr::mutate(
        nb_ouv = nrow(tab_CE()),
        nb_ouvp = nrow(ouv_p()),
        d_ouvp = round(nrow(ouv_p()) / tab_info()$longueur, 2)
      ) %>%
      dplyr::select(nom_CE, nb_ouv, nb_ouvp, d_ouvp) %>%
      unique() %>% dplyr::rename("Nom du cours d'eau"="nom_CE",
                                 "Nombre d'ouvrages"="nb_ouv",
                                 "Nombre d'ouvrages sur le cours principal"="nb_ouvp",
                                 "Densité d'ouvarge sur le cours principal par km"="d_ouvp")
    
  })
  
  #Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur le CE
  output$CH <- renderPlot({
    
    tab_CE() %>% dplyr::filter(!is.na(hauteur)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = classe_hauteur, fill = etat), na.rm = TRUE) +
      ggplot2::geom_bar(ggplot2::aes(x = factor(
        classe_hauteur,
        level = c(
          "Inférieur à 0.5m",
          'De 0.5m à inférieur à 1m',
          'De 1m à inférieur à 1.5m',
          'De 1.5m à inférieur à 2m',
          'De 2m à inférieur à 3m',
          'De 3m à inférieur à 5m',
          'De 5m à inférieur à 10m',
          'Supérieur à 10m'
        )
      ))) +
      ggplot2::xlab("Intervals de hauteur de chute (m)") + ggplot2::ylab("Nombre d'ouvrages") +
      ggplot2::labs(fill = "État :")
    
  })
  
  #Tableau de la répartition de l'état des ouvrages principaux sur le CE
  output$tab_etat <- renderTable({
    
    tab_etat <- ouv_p() %>%
      dplyr::count(etat) %>%
      dplyr::mutate(prop = round(n / nrow(ouv_p()), 2)) %>%
      dplyr::rename(
        "État" = "etat",
        "Nombre d'ouvrages" = "n",
        "Proportion (en %)" = "prop"
      )
  })
  
  #Tableau présentant la répartition des hauteurs de chutes par état ainsi que le TE
  output$tab_TE <- renderTable({
    
    tab_TE <- ouv_p() %>%
      dplyr::filter(!is.na(hauteur)) %>% 
      dplyr::group_by(etat) %>% 
      dplyr::mutate(Hauteur_cumul = sum(hauteur)) %>% 
      dplyr::ungroup(etat) %>% 
      dplyr::select(etat, Hauteur_cumul) %>% 
      unique() %>% 
      dplyr::mutate(prop = round(Hauteur_cumul/sum(Hauteur_cumul)*100,2)) %>%
      dplyr::rename(
        "État" = "etat",
        "Hauteurs cumulées" = "Hauteur_cumul",
        "Rapport des hauteurs cumulées sur la pente naturelle (en %)" = "prop"
      )
  })
  
  
  output$map <- leaflet::renderLeaflet({
    
    tab_geo <-
      ROE_Normandie_H %>% dplyr::filter(nom_CE == CE()) %>%
      sf::st_as_sf(crs = 4326)
    
    pal <- leaflet::colorFactor(palette = c("blue","red","green"),
                               domain = tab_geo$etat)
    
    map <- leaflet::leaflet(tab_geo) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        popup = paste(
          "Code ROE : ",tab_CE()$ROE,"<br/>",
          "Nom de l'ouvrage : ",tab_CE()$Nom_ouvrage,"<br/>",
          "Type d'ouvrage et sous-type : ",tab_CE()$type," ",tab_CE()$sous_type,"<br/>",
          "État de l'ouvrage : ",tab_CE()$etat,"<br/>",
          "Hauteur de chute de l'ouvrage : ",tab_CE()$hauteur," m"
        ),
        radius = 6,
        color = ~pal(tab_geo$etat),
        group = "État des ouvrages",
        stroke = FALSE,
        fillOpacity = 0.8 
      )
  })
  
  output$downloadData <- downloadHandler(
    filename = "fiche_TE.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
    }
  )
  

}

# Run the application
shinyApp(ui = ui, server = server)
