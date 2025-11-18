##%####################################################%##
#                                                        #
##    Application de valorisation des BDD ROE et BDOE   ##
#                                                        #
##%####################################################%##

##----------------------------------------------------------------------------##
#### 1. Chargement des packages ####
##----------------------------------------------------------------------------##

library(shiny)
library(bslib)
'%>%' <- dplyr::'%>%'

##----------------------------------------------------------------------------##
#### 2. Chargement des données ####
##----------------------------------------------------------------------------##

load("../ap/data_prepared/ROE_data.RData")

##----------------------------------------------------------------------------##
#### 3. Front ####
##----------------------------------------------------------------------------##

ui <- page_fluid(
  
  # Titre de l'application
  titlePanel("Taux d'étagement à l'échelle des masses d'eau dites naturelles dans le PLAGEPOMI"),
  
  
  navset_tab(
    
    # Création des 2 onglets
    
    nav_panel("Tableau résumé région",

## 3.a. Onglet régions ##

    tags$h1("Situation à l'échelle de la région"),

    # Tableau des infos topographiques des linéaires de ME de la région
    DT::dataTableOutput(outputId = "tab_region")),
    
## 3.b. Onglet de la fiche résumé à l'échelle d'une ME ##

    nav_panel("Fiche masse d'eau",
  
  # Sélection des inputs

  inputPanel(
    # Selection de la ME
    shiny::selectInput(
      "ME",
      "Choisir un code masse d'eau :",
      choices = ME_L2_lin$cdeumassed,
      selected = "FRHR161"
    )
  ),
  
  # Titre de la partie : infos topographiques de la masse d'eau
  
  tags$h1("Situation de la masse d'eau"),
  
  # Tableau des infos topographiques de la masse d'eau
  
  tableOutput(outputId = "tab_info"),
  
  # Titre de la partie : infos sur les ouvrages du linéaire de la ME
  
  tags$h1("Nombre d'ouvrages"),
  
  # Tableau des infos sur les ouvrages du linéaire de la ME
  
  tableOutput(outputId = "tab_ouv"),
  
  # Titre de la partie : qui présente un barplot sur les classes de hauteurs 
  # de chute et de l'état de tous les ouvrages sur le linéaire de la ME
  
  tags$h1("Répartition des hauteurs de chute sur l'ensemble du linéaire de la ME"),
  
  # Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur le linéaire de la ME
  
  plotOutput("CH"),
  
  # Titre de la partie : répartition de l'état des ouvrages principaux sur le linéaire de la ME
  
  tags$h1("Hauteurs artificielles par état des ouvrages"),
  
  # Tableau de la répartition de l'état des ouvrages principaux sur le linéaire de la ME
  
  tableOutput("tab_h_arti"),
  
  # Titre de la partie : TE
  
  tags$h1("Taux d'étagement"),

  textOutput("text_TE"),
  
  # Titre de la partie : Carte présentant la répartition des ouvrages sur le linéaire de la ME avec les états
  
  tags$h1("Carte du cours d'eau"),
  
  # Carte présentant la répartition des ouvrages sur le linéaire de la ME avec les états
  
  leaflet::leafletOutput("map"),
  
  #Télécharger la fiche au format .docx
  
  downloadButton("your_output", "Télécharger la fiche au format .docx")
  
    )
  )
)


##----------------------------------------------------------------------------##
#### 4. Back ####
##----------------------------------------------------------------------------##

server <- function(input, output) {

## 4.a. Création des objet réactifs ##
  
  #Le code la ME
  ME <- reactive(input$ME)
  
  ##Tableau des infos topographiques du cours d'eau
  tab_info <- reactive({
    tab_info <- dplyr::filter(ME_L2_lin, ME_L2_lin$cdeumassed == ME())
  })
  
  #Sélection des informations du tableau ROE_Normandie_H relatives à notre ME
  tab_ME <- reactive({
    tab_ME <-
      ROE_Normandie_H %>% dplyr::filter(code_ME_valid == ME()) %>%  sf::st_drop_geometry()
  })
  
  #Sélection des ouvrages situés sur le cours principal du linéaire de la ME
  ouv_p <- reactive({
    ouv_p <-
      tab_ME() %>% dplyr::filter(ouv_liaison == "ouvrage principal")
  })
  
  #Sélection des informations de la ME qui nous intéresse
  
  tab_info_ME <- reactive({
    tab_info_ME <-
      ME_L2_lin %>% dplyr::filter(cdeumassed == ME())
  })
  
  #Création du tableau résumé des ME de la région
  
  tab_region <- reactive({
    tab_region <- 
      ME_L2_lin %>% dplyr::relocate("nb_ouvp","nb_ouv","H_cum" , .before = "TE"  ) %>% 
      dplyr::rename(    "Code de la Masse d'eau" ="cdeumassed",
                        "Nom de la masse d'eau"="nommassede",
                        "Longueur du cours d'eau (en km)"="longueur",
                        "Altitute du point le plus en aval (en m)"="aval",
                        "Altitude du point le plus en amont (en m)"="amont",
                        "Dénivelé naturel (en m)"="deni_nat",
                        "Pente moyenne (en ‰)"="pt_nat",
                        "Nombre d'ouvrages"="nb_ouv",
                        "Nombre d'ouvrages sur le cours principal"="nb_ouvp",
                        "Hauteurs artificielles cumulées"="H_cum",
                        "Taux d'étagement (en %)"="TE") 
  })    
  
## 4.a. Création des tableaux de rendus ##
  
  #Tableau résumé des ME de la région
  
  output$tab_region <- DT::renderDataTable(tab_region(),
                                           extensions=c("Buttons"),
                                           options = list(
                                             paging = TRUE,
                                             scrollX=TRUE,
                                             searching = TRUE,
                                             ordering = TRUE,
                                             dom = 'lfrtipB',
                                             pageLength = 10,
                                             lengthMenu = c(10, 20, 50, 100),
                                            buttons = list( 
                                              list(extend = 'csv',   filename =  paste("tx_etagement_normandie/", format(Sys.time(), '%Y_%m_%d'),".csv")),
                                              list(extend = 'excel', filename =  paste("tx_etagement_normandie/", format(Sys.time(), '%Y_%m_%d'),".xlsx"))))
) 
  
  
  #Tableau des infos topographiques du linéaire de ME
  
  output$tab_info <- renderTable({
    
    tab_info <-
      ME_L2_lin %>%  dplyr::filter(ME_L2_lin$cdeumassed == ME()) %>%
      dplyr::relocate("deni_nat","pt_nat", .before = "nb_ouvp"  ) %>%
      dplyr::select(cdeumassed:pt_nat) %>%
      dplyr::rename(
        "Code de la Masse d'eau" = 
          "cdeumassed",
        "Nom du cours d'eau" =
          "nommassede",
        "Longueur du cours d'eau (en km)" =
          "longueur",
        "Altitute du point le plus en aval (en m)" =
          "aval",
        "Altitude du point le plus en amont (en m)" =
          "amont",
        "Dénivelé naturel (en m)" =
          "deni_nat",
        "Pente moyenne (en ‰)" =
          "pt_nat"
      )
    
    
  })
  
  
  #Tableau résumé de la situation des ouvrages sur la ME
  
  output$tab_ouv <- renderTable({
    tab_ouv <-
      ouv_p() %>%
      dplyr::mutate(
        nb_ouv = nrow(tab_ME()),
        nb_ouvp = nrow(ouv_p()),
        d_ouvp = round(nrow(ouv_p()) / tab_info()$longueur, 2)
      ) %>%
      dplyr::select(code_ME_valid, nb_ouv, nb_ouvp, d_ouvp) %>%
      unique() %>% dplyr::rename("Code de la Masse d'eau" = "code_ME_valid",
                                 "Nombre d'ouvrages total"="nb_ouv",
                                 "Nombre d'ouvrages sur le cours principal"="nb_ouvp",
                                 "Densité d'ouvarge sur le cours principal par km"="d_ouvp")
    
  })
  
  #Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur la ME
  
  output$CH <- renderPlot({
      
    tab_ME() %>% dplyr::filter(!is.na(hauteur)) %>% 
      ggplot2::ggplot(ggplot2::aes(x = classe_hauteur, fill = etat)) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge=2)) +
      ggplot2::scale_fill_manual(values = c("Existant" = "#990000", "Détruit partiellement" = "#FFFF00",
                                            "Détruit entièrement" ="#000099")) +
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
      ggplot2::xlab("Intervalles de hauteur de chute (m)") + ggplot2::ylab("Nombre d'ouvrages") +
      ggplot2::ggtitle(label = "Répartition des hauteurs de chute en fonction de leurs classes et de l'état des ouvrages") +
      ggplot2::labs(fill = "État :", subtitle = sprintf(stringr::str_to_title(ME()))) +
      ggplot2::theme(plot.caption =  ggplot2::element_text(hjust = 0, margin = ggplot2::margin(0, 0, 0, 0, "pt")),
                     plot.margin = ggplot2::margin(0, 5.5, 5.5, 5.5, "pt")) +
      ggplot2::labs(caption = paste(sprintf(format(Sys.time(), '%d/%m/%Y')),
                                    "Sources : OFB, Geobs, DR Normandie",
                                    "Notes : Les ouvrages ne comportant pas de hauteurs de chutes on été retirés", sep = "\n"))
    
  })
  
  #Tableau de la répartition de l'état des ouvrages principaux sur la ME
  
  output$tab_h_arti <- renderTable({
        
        
    tab_count <- ouv_p() %>%  dplyr::count(etat)
        
    tab_h_arti <- ouv_p() %>%
          dplyr::group_by(etat) %>%
          dplyr::mutate(Hauteur_cumul = round(sum(hauteur, na.rm = TRUE), 0)) %>% 
          dplyr::ungroup(etat) %>%
          dplyr::select(etat, Hauteur_cumul) %>%
          unique() %>%
          dplyr::mutate(prop = round(Hauteur_cumul / sum(Hauteur_cumul) * 100, 0))
        
    tab_h_arti <- dplyr::left_join(tab_h_arti, tab_count)
        
    tab_h_arti <- tab_h_arti %>%
          dplyr::relocate(n, .before = 'Hauteur_cumul') %>%
          dplyr::arrange(etat) %>%
          dplyr::bind_rows(dplyr::summarise(.,
                                            across(where(is.numeric), sum),
                                            across(where(is.character), ~"Total"))) %>% 
          dplyr::rename(
            "État" = "etat",
            "Nombre d'ouvrages sur le cours principal" = "n",
            "Hauteurs cumulées (en m)" = "Hauteur_cumul",
            "Rapport des hauteurs cumulées sur la pente naturelle (en %)" = "prop"
          )
      
  })

  
  #Text présentant le TE
  
  output$text_TE <- renderText({
    
    paste("Le taux d'étagement de la masse d'eau : ", tab_info_ME()$nommassede," ayant pour code : ", tab_info_ME()$cdeumassed, " est de : ", tab_info_ME()$TE, "%.")
          
  })
  
  
  # Carte présentant la répartition des ouvrages sur le linéaire de ME avec les états
  
  output$map <- leaflet::renderLeaflet({
 
    # Préparation des points ROE sur la carte du linéaire de ME 
    
    tab_geo <-
      ROE_Normandie_H %>% dplyr::filter(code_ME_valid == ME()) %>%
      sf::st_as_sf(crs = 4326)
  
    tab_geo$etat <- as.factor(tab_geo$etat)  
    # Sélection du tracer du linéaire de ME
    
    ME_sf_NOR  <- ME_sf_NOR %>% 
      sf::st_as_sf(crs = 4326) %>% dplyr::filter(cdeumassed == ME()) 
    
    # Création de la palette de couleur à utiliser pour la légende
    
  pal <- leaflet::colorFactor(palette = c("#000099", "#FFFF00", "#990000"),
                                levels = c("Détruit entièrement", "Détruit partiellement", "Existant"),
                              na.color = 'grey')
    # Création de la carte
    
    map <- leaflet::leaflet(tab_geo) %>%
      
      # Ajout de la couche de base issue d'Open Street Map
      leaflet::addTiles() %>% 
      
      # Ajout de la couche d'ortophotographie
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery") %>% 
      
      # Ajout du linéaire de ME issu de la BDTOPO
      leaflet::addPolylines(data = ME_sf_NOR, color = "#56B4E9",
                            opacity = 0.8,
                            group = "Linéaire de la masse d'eau") %>%
      
      # Ajout des ROE présent sur le CE
      leaflet::addCircleMarkers(
        popup = paste(
          "Code ROE : ",tab_ME()$ROE,"<br/>",
          "Nom de l'ouvrage : ",tab_ME()$Nom_ouvrage,"<br/>",
          "Lien de l'ouvrage : ",tab_ME()$ouv_liaison,"<br/>",
          "Type d'ouvrage et sous-type : ",tab_ME()$type," ",tab_ME()$sous_type,"<br/>",
          "État de l'ouvrage : ",tab_ME()$etat,"<br/>",
          "Hauteur de chute de l'ouvrage : ",tab_ME()$hauteur," m"
        ),
        radius = 6,
        fillColor = ~pal(tab_geo$etat),
        fillOpacity = 1,
        stroke = TRUE,
        color = "black",
        weight = 2,
        opacity = 1,
        group = "ROE de la masse d'eau"
      ) %>%
      
      # Ajout de la légende 
      leaflet::addLegend("bottomleft", pal = pal, values = ~tab_geo$etat,
                         na.label = "NA",
                         title = "État des ouvrages",
                         opacity = 1
      ) %>% 
      
      # Ajout de l'échelle
      leaflet::addScaleBar(
        position = "bottomright",
        options = leaflet::scaleBarOptions(imperial = FALSE)
      ) %>%
      
      # Ajout de différentes couches 
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c("Open Street Map","ESRI World Imagery"),
        # Ajout d'une option qui permet d'afficehr le ROE ou non
        overlayGroups = c("ROE de la masse d'eau","Linéaire de la masse d'eau"),
        # Choix d'afficher en permanence le contrôle de l'affiche des couches
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # Ajout d'une minimap
      leaflet::addMiniMap(
        position = "topright",
        tiles = leaflet::providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      )
    
  })
  
  # Bouton pour télécharger la fiche rapport d'une ME au format .docx

  output$your_output <- downloadHandler(
    
    filename = function() {
      doc <- paste("tx_etagement_",ME(),"_", format(Sys.time(), '%Y_%m_%d'),".docx")
      doc
      
    },
    content = function(file) {
      tempReport <- normalizePath("template/template_ME_L2.Rmd")

  # Params
  params = list(CME_L2 = ME(),
                ME = tab_info()$nommassede)    
  
  rmarkdown::render(tempReport, output_file = file, output_format = 'word_document',
                    params = params,
                    envir = new.env(parent = globalenv())
  )
    }
  )
}

# Connection du font et du back
shinyApp(ui = ui, server = server)
