##%##############################################################%##
#                                                                  #
##    Application de visualisation des masse d'eau du PLAGEPOMI   ##
#                                                                  #
##%##############################################################%##

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
  titlePanel("Taux d'étagement à l'échelle des tronçons liste 2"),
  
  
  navset_tab(
    
    # Création des 2 onglets
    
    nav_panel("Tableau résumé région",
              
              ## 3.a. Onglet régions ##
              
              tags$h1("Situation à l'échelle de la région"),
              
              # Tableau des infos topographiques des tronçons liste 2 de la région
              DT::dataTableOutput(outputId = "tab_region")),
    
    ## 3.b. Onglet de la fiche résumé à l'échelle d'un CE ##
    
    nav_panel("Fiche tronçon liste 2",
              
              # Sélection des inputs
              
              inputPanel(
                # Selection du CE
                shiny::selectInput(
                  "CE",
                  "Choisir un tronçon liste 2 :",
                  choices = Info_L2$label_troncon_L2,
                  selected = "l'Yères_FR1333_FRHR161_76"
                )
              ),
              
              # Titre de la partie : infos topographiques du tronçon liste 2
              
              tags$h1("Situation du tronçon liste 2"),
              
              # Tableau des infos topographiques du tronçon liste 2
              
              tableOutput(outputId = "tab_info"),
              
              # Titre de la partie : infos sur les ouvrages du CE
              
              tags$h1("Nombre d'ouvrages"),
              
              # Tableau des infos sur les ouvrages du CE
              
              tableOutput(outputId = "tab_ouv"),
              
              # Titre de la partie : qui présente un barplot sur les classes de hauteurs 
              # de chute et de l'état de tous les ouvrages sur le CE
              
              tags$h1("Répartition des hauteurs de chute sur l'ensemble du tronçon liste 2"),
              
              # Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur le CE
              
              plotOutput("CH"),
              
              # Titre de la partie : répartition de l'état des ouvrages principaux sur le CE
              
              tags$h1("Hauteurs artificielles par état des ouvrages"),
              
              # Tableau de la répartition de l'état des ouvrages principaux sur le CE
              
              tableOutput("tab_h_arti"),
              
              # Titre de la partie : TE
              
              tags$h1("Taux d'étagement"),
              
              textOutput("text_TE"),
              
              # Titre de la partie : Carte présentant la répartition des ouvrages sur le CE avec les états
              
              tags$h1("Carte du tronçon liste 2"),
              
              # Carte présentant la répartition des ouvrages sur le CE avec les états
              
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
  
  #Le nom du tronçon L2
  CE <- reactive(input$CE)
  
  ##Tableau des infos topographiques du tronçon liste 2
  tab_info <- reactive({
    tab_info <- dplyr::filter(Info_L2, Info_L2$label_troncon_L2 == CE())
  })
  
  #Sélection des informations du tableau ROE_Normandie_H_L2 relatives à notre CE
  tab_CE <- reactive({
    tab_CE <-
      ROE_Normandie_H_L2 %>% dplyr::filter(label_troncon_L2 == CE()) %>%  sf::st_drop_geometry()
  })
  
  #Sélection des ouvrages situés sur le cours principal du CE
  ouv_p <- reactive({
    ouv_p <-
      tab_CE() %>% dplyr::filter(ouv_liaison == "ouvrage principal")
  })
  
  #Sélection des informations du CE qui nous intéresse
  
  tab_info_CE <- reactive({
    tab_info_CE <-
      Info_L2 %>% dplyr::filter(label_troncon_L2 == CE())
  })
  
  #Création du tableau résumé des CE de la région
  
  tab_region <- reactive({
    tab_region <- 
      Info_L2 %>% dplyr::relocate("nb_ouvp","nb_ouv", .before =  "H_cum") %>% 
      dplyr::select(!c(id_troncon_L2,DPT_noms,NomZone_L2_sandre,MEQ)) %>% 
      dplyr::rename(    "Code de la Masse d'eau" ="code_ME_valid",
                        "Nom du tronçon L2"="label_troncon_L2",
                        "Longueur du tronçon liste 2 (en km)"="longueur",
                        "Altitute du point le plus en aval (en m)"="alt_av",
                        "Altitude du point le plus en amont (en m)"="alt_am",
                        "Dénivelé naturel (en m)"="deni_nat",
                        "Pente moyenne (en ‰)"="pt_nat",
                        "Nombre d'ouvrages"="nb_ouv",
                        "Nombre d'ouvrages sur le cours principal"="nb_ouvp",
                        "Hauteurs artificielles cumulées"="H_cum",
                        "Taux d'étagement (en %)"="TE") 
  })    
  
  ## 4.a. Création des tableaux de rendus ##
  
  #Tableau résumé des CE de la région
  
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
                                               list(extend = 'excel', filename =  paste("tx_etagement_normandie/", format(Sys.time(), '%Y_%m_%d'),".docx"))))
  ) 
  
  
  #Tableau des infos topographiques du tronçon liste 2
  
  output$tab_info <- renderTable({
    
    tab_info <-
      Info_L2 %>%  dplyr::filter(Info_L2$label_troncon_L2 == CE()) %>%
      dplyr::select(code_ME_valid:pt_nat) %>%
      dplyr::rename(
        "Code de la Masse d'eau" = "code_ME_valid",
        "Nom du tronçon L2" =
          "label_troncon_L2",
        "Longueur du tronçon liste 2 (en km)" =
          "longueur",
        "Altitute du point le plus en aval (en m)" =
          "alt_av",
        "Altitude du point le plus en amont (en m)" =
          "alt_am",
        "Dénivelé naturel (en m)" =
          "deni_nat",
        "Pente moyenne (en ‰)" =
          "pt_nat"
      )
    
    
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
      dplyr::select(label_troncon_L2, nb_ouv, nb_ouvp, d_ouvp) %>%
      unique() %>% dplyr::rename("Nom du tronçon L2"="label_troncon_L2",
                                 "Nombre d'ouvrages total"="nb_ouv",
                                 "Nombre d'ouvrages sur le cours principal"="nb_ouvp",
                                 "Densité d'ouvarge sur le cours principal par km"="d_ouvp")
    
  })
  
  #Barplot des classes de hauteurs de chute et de l'état de tous les ouvrages sur le CE
  
  output$CH <- renderPlot({
    
    tab_CE() %>% dplyr::filter(!is.na(hauteur)) %>% 
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
      ggplot2::labs(fill = "État :", subtitle = sprintf(stringr::str_to_title(CE()))) +
      ggplot2::theme(plot.caption =  ggplot2::element_text(hjust = 0, margin = ggplot2::margin(0, 0, 0, 0, "pt")),
                     plot.margin = ggplot2::margin(0, 5.5, 5.5, 5.5, "pt")) +
      ggplot2::labs(caption = paste(sprintf(format(Sys.time(), '%d/%m/%Y')),
                                    "Sources : OFB, Geobs, DR Normandie",
                                    "Notes : Les ouvrages ne comportant pas de hauteurs de chutes on été retirés", sep = "\n"))
    
  })
  
  #Tableau de la répartition de l'état des ouvrages principaux sur le CE
  
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
    
    paste("Le taux d'étagement de la masse d'eau : ", tab_info_CE()$code_ME_valid, " est de : ", tab_info_CE()$TE, "%.")
    
  })
  
  
  # Carte présentant la répartition des ouvrages sur le CE avec les états
  
  output$map <- leaflet::renderLeaflet({
    
    # Préparation des points ROE sur la carte du CE 
    
    tab_geo <-
      ROE_Normandie_H_L2 %>% dplyr::filter(label_troncon_L2 == CE()) %>%
      sf::st_as_sf(crs = 4326)
    
    tab_geo$etat <- as.factor(tab_geo$etat)  
    # Sélection du tracer du CE 
    
    CE_L2_lin <- CE_L2_lin  %>% 
      sf::st_as_sf(crs = 4326) %>% dplyr::filter(label_troncon_L2 == CE()) 
    
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
      
      # Ajout du CE issu de la BDTOPO
      leaflet::addPolylines(data = CE_L2_lin, color = "#56B4E9",
                            opacity = 0.8,
                            group = "Tronçon liste 2") %>%
      
      # Ajout des ROE présent sur le CE
      leaflet::addCircleMarkers(
        popup = paste(
          "Code ROE : ",tab_CE()$ROE,"<br/>",
          "Nom de l'ouvrage : ",tab_CE()$Nom_ouvrage,"<br/>",
          "Lien de l'ouvrage : ",tab_CE()$ouv_liaison,"<br/>",
          "Type d'ouvrage et sous-type : ",tab_CE()$type," ",tab_CE()$sous_type,"<br/>",
          "État de l'ouvrage : ",tab_CE()$etat,"<br/>",
          "Hauteur de chute de l'ouvrage : ",tab_CE()$hauteur," m"
        ),
        radius = 6,
        fillColor = ~pal(tab_geo$etat),
        fillOpacity = 1,
        stroke = TRUE,
        color = "black",
        weight = 2,
        opacity = 1,
        group = "ROE du tronçon liste 2"
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
        overlayGroups = c("ROE du tronçon liste 2","Tronçon liste 2"),
        # Choix de d'afficher en permanence le contrôle de l'affiche des couches
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
  
  # Bouton pour télécharger le template sur un CE au format .docx
  
  output$your_output <- downloadHandler(
    
    filename = function() {
      doc <- paste("tx_etagement_",CE(),"_", format(Sys.time(), '%Y_%m_%d'),".docx")
      doc
      
    },
    content = function(file) {
      tempReport <- normalizePath("template/template_L2.Rmd")
      
      # Params
      params = list(CE = CE(),
                    DEP = unique(tab_CE()$dep_num),
                    CSE = tab_info()$NomZone_L2_sandre)    
      
      rmarkdown::render(tempReport, output_file = file, output_format = 'word_document',
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Connection du font et du back
shinyApp(ui = ui, server = server)
