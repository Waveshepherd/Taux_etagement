##%######################################################%##
#                                                          #
###        Récupération des bases de données             ###
##     Nettoyage et homogénéisation des variables         ##
#                                                          #
##%######################################################%##

#### Chargement des BD ####

## Chargement du flux QGIS ##

#Délimitation de la zone dans laquelle les données sont récupérées (Normandie)

bbox_normandie <- 
  c(xmin = -1.944580,
    ymin = 48.133101,
    xmax = 1.966553,
    ymax = 50.145226) %>%
  sf::st_bbox(crs = 4326)

normandie_area <- 
  bbox_normandie %>%
  sf::st_as_sfc() %>%
  sf::st_sf()

#Chargement du flux

ROE_Normandie <- 
  r4geobs::get_geobs_data_wfs("REFERENTIEL_ROE_MONDE", normandie_area)

## Chargement du flux Geobs (pour les hauteurs) ##

bdoe <- r4geobs::get_bdoe_data(login = "augustin.soki-makilutila@ofb.gouv.fr",
                       mdp = 'Motdepasse20240802',
                       nom_dossier = "data/bdoe", 
                       lecture = TRUE)

#### Sélection des données utiles du flux Geobs pour la Normandie ######


ROE_Normandie <-
  ROE_Normandie %>% dplyr::filter(
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
  ) %>%
  as.data.frame()
  

## Choix de se baser la BD topo pour le nom des cours d'eau ##

ROE_Normandie <-
  ROE_Normandie %>% dplyr::mutate(
    nom_carthage = gsub("fleuve ", "", nom_carthage),
    nom_carthage = gsub("rivière ", "", nom_carthage),
    nom_carthage = dplyr::case_when(!is.na(nom_carthage) ~ nom_carthage,
                                    TRUE ~ 'NA'),
    nom_topo = dplyr::case_when(is.na(nom_topo) ~ "NA",
                                nom_topo == 'NR' ~ 'NA',
                                TRUE ~ nom_topo),
    nom_topo = dplyr::case_when(
      nom_carthage != 'NA' & nom_topo == 'NA' ~ nom_carthage,
      TRUE ~ nom_topo
    )
  ) %>%
  dplyr::select(!(nom_carthage)) %>%
  dplyr::rename("nom_CE" = "nom_topo")

# Obtenir les obstacles principals et secondaires (à voir avec Benoît ou faire à partir des ouvrages liés)

ROE_Normandie <-
  ROE_Normandie %>% tidyr::separate(ouvrages_lies, c('ouvl_1', 'ouvl_2', 'ouvl_3', 'ouvl_4', 'ouvl_5','ouvl_6', 'ouvl_7', 'ouvl_8'), "-")
  ROE_Normandie <-
  ROE_Normandie %>% dplyr::mutate(
    ouv_liaison = dplyr::case_when(is.na(ouvrages_lies) & ROE != across(starts_with('ROE')) ~ 'Principal',
                                   ROE == ouvrages_lies ~ "Secondaire",
                                   is.na(ouvrages_lies) & ROE == starts_with('ROE') ~ 'Secondaire',
                                   ROE != ouvrages_lies ~ "Principal",
                                   is.na(ouvrages_lies) ~ "Principal"
                                   ))

###### Ajout des hauteurs de chute à partir d'une extraction en flux provenant de BDOE ######

#Données du flux geobs (temporaire)

Hauteur <- readr::read_delim(
  "data/hauteur_chute.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

#Transfo_Hauteur#

Hauteur <-
  Hauteur %>% dplyr::select("ouv_id", "hco_hauteur", "hco_date_mesure") %>%
  dplyr::rename('ROE' = 'ouv_id',
                'hauteur' = 'hco_hauteur',
                'date_mesure' = 'hco_date_mesure') %>%
  as.data.frame()

#Ajout des hauteurs de chutes + modifictation des NA en date la plus ancienne possible + création des classes de hauteur de chute#

ROE_Normandie_H <-
  dplyr::left_join(ROE_Normandie, Hauteur, by = 'ROE')

ROE_Normandie_H$date_mesure <-
  as.character(ROE_Normandie_H$date_mesure)
ROE_Normandie_H <-
  ROE_Normandie_H %>% dplyr::mutate(date_mesure = dplyr::case_when(is.na(date_mesure) ~ "1970-01-01",
                                                                   TRUE ~ date_mesure)) %>%
  dplyr::group_by(ROE) %>%
  dplyr::filter(date_mesure == max(date_mesure)) %>%
  dplyr::filter(hauteur == min(hauteur)) %>%
  dplyr::ungroup(ROE) %>%
  dplyr::filter(!(is.na(hauteur))) %>%
  dplyr::distinct(ROE, .keep_all = TRUE) %>%
  dplyr::mutate(
    date_mesure =  dplyr::case_when(date_mesure == "1970-01-01" ~ "NA",
                                    TRUE ~ date_mesure),
    classe_hauteur = dplyr::case_when(
      hauteur < 1 / 2 ~ "Inférieur à 0.5m",
      hauteur  >= 1 / 2 & hauteur < 1 ~ 'De 0.5m à inférieur à 1m',
      hauteur >= 1 & hauteur < 1.5 ~ 'De 1m à inférieur à 1.5m',
      hauteur >= 1.5 & hauteur < 2 ~ 'De 1.5m à inférieur à 2m',
      hauteur >= 2 & hauteur < 3 ~ 'De 2m à inférieur à 3m',
      hauteur >= 3 & hauteur < 5 ~ 'De 3m à inférieur à 5m',
      hauteur >= 5 & hauteur < 10 ~ 'De 5m à inférieur à 10m',
      hauteur > 10 ~ 'Supérieur à 10m'
    )
  ) %>%
  dplyr::relocate(c('hauteur', 'classe_hauteur', 'date_mesure'), .after = 'Nom_ouvrage') %>%
  as.data.frame()

#dplyr::filter(date_mesure == max(date_mesure),
#hauteur == min(hauteur),
#!(is.na(hauteur))) %>%

#####Gérer les fautes d'orthographe####

ROE_Normandie_H <-
  ROE_Normandie_H %>% dplyr::mutate(nom_CE = dplyr::case_when(nom_CE == "l'hyères" ~ "l'yères",
                                                              TRUE ~ nom_CE))

##### Obtention des métriques du cours d'eau à partir de PHRYMO #####

Info_LB <- readr::read_delim(
  "data/usra_LB.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE,
  na = 'NA'
)
Info_NOR <- readr::read_delim(
  "data/usra_NOR.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE,
  na = 'NA'
)

Info <- dplyr::full_join(Info_LB, Info_NOR)

Info <-
  Info %>% dplyr::rename('nom_CE'='toponyme','alt_am' = 'zamont', 'alt_av' = 'zaval') %>% 
  dplyr::select(nom_CE, longueur, alt_av, alt_am) %>%
  dplyr::filter(alt_av != 0) %>%
  dplyr::group_by(nom_CE) %>% 
  dplyr::mutate(nom_CE = gsub("fleuve ","",nom_CE),
                nom_CE = gsub("rivière ","",nom_CE)) %>% 
  dplyr::mutate(longueur = sum(longueur)*0.001,
                alt_am = max(alt_am),
                alt_av = min(alt_av),
                deni_nat = alt_am - alt_av,
                pt_nat = deni_nat / longueur) %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  dplyr::ungroup(nom_CE) %>% 
  unique()

#Ajout des métriques de CE au tableau de données

ROE_Normandie_H <-
  dplyr::left_join(ROE_Normandie_H, Info, by = 'nom_CE')

