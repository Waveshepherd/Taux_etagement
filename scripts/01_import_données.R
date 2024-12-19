##%######################################################%##
#                                                          #
###        Récupération des bases de données             ###
##     Nettoyage et homogénéisation des variables         ##
#                                                          #
##%######################################################%##

## Chargement des flux ##

#Données du flux geobs (temporaire)

ROE_Q <- read_delim(
  "data/ROE_REF_MONDE_28112024.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

#### Sélection des données utiles du flux Geobs pour la Normandie ######


ROE <- ROE_Q %>% filter(statut_nom != 'Gelé')
ROE_Normandie <-
  ROE_Q %>% dplyr::filter(
    dept_nom %in% c('MANCHE', 'CALVADOS', 'SEINE-MARITIME', 'ORNE', 'EURE'),
    statut_nom != 'Gelé'
  ) %>%
  dplyr::rename(
    'ROE' = 'identifiant_roe',
    'etat' = 'etat_nom',
    'Nom_ouvrage' = 'nom_principal',
    'type' = 'type_nom',
    'sous_type' = 'stype_nom',
    'fpi_1' = 'fpi_nom1',
    'fpi_2' = 'fpi_nom2',
    'fpi_3' = 'fpi_nom3',
    'fpi_4' = 'fpi_nom4',
    'fpi_5' = 'fpi_nom5',
    'emo1' = 'emo_nom1',
    'emo2' = 'emo_nom2',
    'emo3' = 'emo_nom3',
    'fnt' = 'fnt_nom1',
    'usage1' = 'usage_nom1',
    'usage2' = 'usage_nom2',
    'dep_nom' = 'dept_nom',
    'dep_num' = 'dept_code',
    'commune' = 'commune_nom',
    'code_ME' = 'masse_eau_code'
  ) %>%
  dplyr::select(
    'code_ME',
    'nom_topo',
    'nom_carthage',
    'ROE',
    'ouvrages_lies',
    'Nom_ouvrage',
    'etat',
    'type',
    'sous_type',
    'commune',
    'dep_nom',
    'dep_num',
    'fpi_1',
    'fpi_2',
    'fpi_3',
    'fpi_4',
    'fpi_5',
    'emo1',
    'emo2',
    'emo3',
    'fnt',
    'usage1',
    'usage2'
  )
  


## Choix de se baser la BD topo pour le nom des cours d'eau ##

ROE_Normandie <-
  ROE_Normandie %>% dplyr::mutate(
    nom_carthage = gsub("fleuve ", "", nom_carthage),
    nom_carthage = gsub("rivière ", "", nom_carthage),
    nom_carthage = case_when(!is.na(nom_carthage) ~ nom_carthage,
                             TRUE ~ 'NA'),
    nom_topo = case_when(is.na(nom_topo) ~ "NA",
                         nom_topo == 'NR' ~ 'NA',
                         TRUE ~ nom_topo),
    nom_topo = case_when(
      nom_carthage != 'NA' & nom_topo == 'NA' ~ nom_carthage,
      TRUE ~ nom_topo
    )
  ) %>%
  select(!(nom_carthage)) %>%
  dplyr::rename("nom_CE" = "nom_topo")

# Obtenir les obstacles principals et secondaire (à voir avec Benoît ou faire à partir des ouvrages liés)

###### Obtention des hauteurs de chute à partir d'une extraction en flux à partir de BDOE ######

#Données du flux geobs (temporaire)

Hauteur <- readr::read_delim("data/hauteur_chute_25112024.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

####Transfo_Hauteur#####

Hauteur <-
  Hauteur %>% dplyr::select("ouv_id", "hco_hauteur", "hco_date_mesure") %>%
  dplyr::rename('ROE' = 'ouv_id',
                'hauteur' = 'hco_hauteur',
                'date_mesure' = 'hco_date_mesure') %>%
  as.data.frame() 

#Ajout des hauteurs de chutes + modifictation des NA en date la plus ancienne possible#

ROE_Normandie_H <- dplyr::left_join(ROE_Normandie, Hauteur, join_by('ROE')) 

cond <- ROE_Normandie_H$date_mesure[i] < ROE_Normandie_H$date_mesure[i+1]
ROE_Normandie_H1 <-
  ROE_Normandie_H1 %>% dplyr::mutate(date_mesure = case_when(is.na(date_mesure) ~ as.Date("1970-01-01"),
                                                     TRUE ~ date_mesure)) 


##### Obtention des métriques du cours d'eau à partir de PHRYMO #####
