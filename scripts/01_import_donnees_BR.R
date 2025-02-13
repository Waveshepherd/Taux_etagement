##%######################################################%##
#                                                          #
###        Récupération des bases de données             ###
##     Nettoyage et homogénéisation des variables         ##
#                                                          #
##%######################################################%##

'%>%' <- dplyr::'%>%'

##----------------------------------------------------------------------------##
#### 1. Chargement des BD ####
##----------------------------------------------------------------------------##

## 1.a. Chargement du flux GEOBS ##

#Délimitation de la zone dans laquelle les données sont récupérées (Normandie)

bbox_normandie <-
  c(
    xmin = -1.944580,
    ymin = 48.133101,
    xmax = 1.966553,
    ymax = 50.145226
  ) %>%
  sf::st_bbox(crs = 4326)

normandie_area <-
  bbox_normandie %>%
  sf::st_as_sfc() %>%
  sf::st_sf()

#Chargement du flux

ROE_Normandie <- 
  r4geobs::get_geobs_wfs_data("REFERENTIEL_ROE_MONDE", normandie_area)


## 1.b. Chargement des données BDOE issues de Geobs (pour les hauteurs) ##

#Création dossier 'data' si inexistant
if (!file.exists("data")) {
  dir.create("data")
}

bdoe <-
  r4geobs::get_bdoe_data(
    login = Sys.getenv("GEOBS_LOGIN"),
    mdp = Sys.getenv("GEOBS_MDP"),
    nom_dossier = "data/bdoe",
    lecture = TRUE
  )

## 1.c. Chargement des données PHRYMO

Info_LB <- sf::read_sf(
  "//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/IG_METIER/CONTINUITE/PHRYMO/usra_LB.csv" 
)

Info_NOR <- sf::read_sf(
  "//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/IG_METIER/CONTINUITE/PHRYMO/usra_NOR.csv"
)

Info <- 
  dplyr::bind_rows(Info_LB, Info_NOR)

## 1.d. Chargement des données CE de la BD Topo (BDTopage)

CE_bdtopo <- 
  sf::read_sf(
    "data_prepared/CE_BDTOPO.gpkg"
  )

## 1.f. Chargement des données géographiques des tronçons "Liste 2" à partir du Sandre 

CE_L2_sandre <- 
  r4geobs::get_classCE_wfs(geo = bbox_normandie, liste = "Liste2")

## 1.g. Chargement des données issues du travail de récup sur les L2

load("data_prepared/L2_reclassif_ROE.RData") ## nom : L2_reclassif_ROE

## 1.h. Chargement des données sur les Masses d'eau (SN et LB)

BV_ME_SN_LB <- 
  sf::read_sf("data_prepared/BV_ME_SN_LB.gpkg")

##----------------------------------------------------------------------------##
#### 2. Sélection des données utiles du flux Geobs pour la Normandie ####
##----------------------------------------------------------------------------##

# nrow(ROE_Normandie) # 9801 lignes

ROE_Normandie <-
  ROE_Normandie %>%
  dplyr::mutate(dept_nom = dplyr::case_when(
    identifiant_roe %in% c('ROE14725', 
                           'ROE117117', 
                           'ROE119897',
                           'ROE119898') ~ 'MANCHE',
    identifiant_roe %in% c('ROE27825', 
                           'ROE21617') ~ 'CALVADOS',
    identifiant_roe %in% c('ROE54558',
                           'ROE88613') ~ 'SEINE-MARITIME',
    TRUE ~ dept_nom
  )) %>%
  dplyr::filter(
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

# nrow(ROE_Normandie) # 6110 lignes après filtrage

##----------------------------------------------------------------------------##
#### 3. Uniformisation des noms de cours d'eau ####
##----------------------------------------------------------------------------##

## Choix retenu: se baser sur la BD Topo pour le nom des cours d'eau ##

ROE_Normandie <-
  ROE_Normandie %>% 
  dplyr::mutate(
    nom_CE = dplyr::case_when(
      is.na(nom_topo) & is.na(nom_carthage) ~ NA,
      nom_topo == 'NR' & !is.na(nom_carthage) ~ nom_carthage,
      is.na(nom_topo) & !is.na(nom_carthage)~ nom_carthage,
      nom_topo == 'NR' ~ NA,
      TRUE ~ nom_topo
    )
  ) %>% 
  dplyr::mutate(
    # cas des rivières / garde rivière quand pronom "de|d'|du"
    nom_CE = gsub("^rivière(?! (de|d'|du)\\b)\\s*", "", nom_CE, perl = TRUE), 
    # cas de rivière quand situé en fin de nom
    nom_CE = gsub("canal des moulins rivière", "canal des moulins", nom_CE),
    # cas des fleuves quand en début de nom
    nom_CE = gsub("^fleuve\\s*","",nom_CE, perl = TRUE),
    # l'aure inférieure
    nom_CE = gsub("\\s*inférieure$", "", nom_CE),
    # cas de la soulle -> la soulles
    nom_CE = gsub("la soulle$", "la soulles", nom_CE),
    # cas de "ruisseau de neauphe sous essai" -> "ruisseau de neauphe-sous-essai"
    nom_CE = gsub("ruisseau de neauphe sous essai", "ruisseau de neauphe-sous-essai", nom_CE),
    # cas de l'hyères -> l'yères
    nom_CE = gsub("l'hyères", "l'yères", nom_CE)
  ) %>% 
  dplyr::relocate(nom_CE, .before = 'nom_topo')

## ROE_Normandie %>% filter(is.na(nom_CE)) %>% nrow() # 1027 lignes

## Raccrocher au nom de cours d'eau de la BDTOPO retéléchargé au préalable. ##
## --> l'accroche au CE dans Geobs n'est pas automatique ??? version de BDTOPO ??? ##

ROE_Normandie <-
  sf::st_join(
    ROE_Normandie,
    CE_bdtopo %>% 
      dplyr::select(toponyme), join = sf::st_nearest_feature
  ) %>% 
  dplyr::mutate(
    toponyme_c = tolower(toponyme),
    toponyme_c = gsub("^bras de\\s*", "", toponyme_c, perl = TRUE),
    toponyme_c = gsub("^bras du\\s*", "le ", toponyme_c, perl = TRUE),
    toponyme_c = gsub("^bras la\\s*", "la ", toponyme_c, perl = TRUE),
    toponyme_c = gsub("^rivière(?! (de|d'|du)\\b)\\s*", "", toponyme_c, perl = TRUE)
  ) %>%
  dplyr::mutate(
    nom_CE_valid = toponyme_c,
    nom_CE_valid = dplyr::case_when(is.na(toponyme_c) ~ nom_CE,
                              TRUE ~ nom_CE_valid)
  ) %>% 
  dplyr::select(-toponyme_c) %>% 
  dplyr::relocate(nom_CE_valid, toponyme, .after = 'nom_topo')

## cb de 'nom_CE' encore manquants ?
## ROE_Normandie %>% dplyr::filter(is.na(nom_CE_valid)) %>% nrow() # 38 lignes 🍾

##----------------------------------------------------------------------------##
#### 4. Obtenir les obstacles principaux et secondaires ####
##----------------------------------------------------------------------------##

ouvrages_lies_liste <-
  ouvrages_lies_liste <-
  ROE_Normandie %>%
  dplyr::filter(!is.na(ouvrages_lies)) %>%
  # rajout d'un début et fin de ligne pour éviter les erreurs de détection
  dplyr::mutate(ouvrages_lies = paste0("^", ouvrages_lies, "$")) %>% 
  dplyr::pull(ouvrages_lies) %>%
  gsub(" - ", "$|^", .) %>%
  paste(collapse = "|")

ROE_Normandie <-
  ROE_Normandie %>%
  dplyr::mutate(
    ouv_liaison = 
      dplyr::case_when(
        # si ouvrages liés renseignés et pas dans la liste des liés, alors ouvrages P
        !is.na(ouvrages_lies) & stringr::str_detect(ROE, ouvrages_lies_liste, negate = TRUE) ~ 'ouvrage principal',
        # si ouvragés liés vides NA et pas dans la liste des liés, alors ouvrages P (cas des ouvrages seuls !)
        is.na(ouvrages_lies) & stringr::str_detect(ROE, ouvrages_lies_liste, negate = TRUE) ~ 'ouvrage principal',
        # si ouvragés liés vides NA et présent dans liste des liés, alors ouvrages S
        stringr::str_detect(ROE, ouvrages_lies_liste, negate = FALSE) ~ 'ouvrage secondaire',
        # si ouvragés liés renseignés et présent dans liste des liés, alors ouvrages S (cas d'ouvrages liés à un autre lié secondaire)
        #!is.na(ouvrages_lies) & stringr::str_detect(ROE, ouvrages_lies_liste, negate = FALSE) ~ 'ouvrage secondaire',
        TRUE ~ 'statut à vérif'
      )
  ) %>%
  dplyr::relocate('ouv_liaison', .after = 'ouvrages_lies')


##----------------------------------------------------------------------------##
#### 5. Ajout hauteurs de chute à partir d'une extraction provenant de BDOE ####
##----------------------------------------------------------------------------##

# Données BDOE téléchargées sur Geobs

Hauteur <- bdoe$hauteur_chute

#Transfo_Hauteur#

Hauteur <-
  Hauteur %>% 
  dplyr::select("ouv_id", "hco_hauteur", "hco_date_mesure") %>%
  dplyr::rename('ROE' = 'ouv_id',
                'hauteur' = 'hco_hauteur',
                'date_mesure' = 'hco_date_mesure')

#Ajout des hauteurs de chutes + modification des NA en date la plus ancienne possible + création des classes de hauteur de chute#

ROE_Normandie_H <-
  dplyr::left_join(ROE_Normandie, Hauteur, by = 'ROE')

ROE_Normandie_H <-
  ROE_Normandie_H %>% 
  dplyr::mutate(date_mesure = dplyr::case_when(
    is.na(date_mesure) ~ as.Date("1970-01-01", "%Y-%m-%d"),
    TRUE ~ date_mesure
  )) %>%
  dplyr::group_by(ROE) %>%
  dplyr::filter(date_mesure == max(date_mesure)) %>%
  dplyr::filter(hauteur == min(hauteur) | is.na(hauteur)) %>%
  dplyr::ungroup(ROE) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    date_mesure = as.Date(gsub("1970-01-01", "NA", date_mesure)),
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
  dplyr::relocate(c('hauteur', 'classe_hauteur', 'date_mesure'), .after = 'Nom_ouvrage')


##----------------------------------------------------------------------------##
#### 6. Classement des cours d'eau - tronçons de catégorie Liste 1 & Liste 2 ###
##----------------------------------------------------------------------------##

ouvrage_L1_L2_bdoe <-
  bdoe$ouvrage %>% 
  dplyr::select(ROE = ouv_id, 
                ouv_liste1, 
                ouv_liste2, 
                ouv_date_liste) %>% 
  dplyr::mutate(
    ouv_liste1 = dplyr::case_when(ouv_liste1 == TRUE ~ "Liste1",
                                  ouv_liste1 == FALSE ~ "NON",
                                  TRUE ~ NA_character_),
    ouv_liste2 = dplyr::case_when(ouv_liste2 == TRUE ~ "Liste2",
                                  ouv_liste2 == FALSE ~ "NON",
                                  TRUE ~ NA_character_)
  )

ROE_Normandie_H <-
  ROE_Normandie_H %>% 
  dplyr::left_join(ouvrage_L1_L2_bdoe)

## sur ROE_Normandie_H -> nb ouv. L2 = 2935 / nb ouv. en NA = 2057 / nb ouv. en NON = 1118

## -> jointure avec le travail de récup des ROE orphelins L2
ROE_Normandie_H <- 
  ROE_Normandie_H %>% 
  dplyr::left_join(
    L2_reclassif_ROE %>% 
      dplyr::select(ROE, ouv_liste2_valid = ouv_liste2_c3) %>% 
      sf::st_drop_geometry()
  )

##----------------------------------------------------------------------------##
#### 7. Masses d'eau - Codes manquants ###
##----------------------------------------------------------------------------##

# --> 1910 ROE sans code ME ; soit env. 31%
# ROE_Normandie_H %>% 
#   sf::st_drop_geometry() %>% 
#   dplyr::mutate(code_ME_present = ifelse(is.na(code_ME), 'non', 'oui')) %>% 
#   dplyr::count(code_ME_present)

# verif_ROE_Normandie_H_code_ME <- 
#   sf::st_join(
#     ROE_Normandie_H %>% 
#       sf::st_transform(crs = 2154) %>% 
#       dplyr::select(code_ME, ROE, Nom_ouvrage),
#     BV_ME_SN_LB,
#     left = F) %>% 
#   dplyr::rename(code_ME_valid = CdEuMasseD) %>%
#   dplyr::mutate(
#     diff_code_ME = dplyr::case_when(toupper(code_ME) == code_ME_valid ~ 'egal',
#                                     TRUE ~ 'diff')
#   ) %>% 
#   sf::st_transform(crs = 4326)
# 
# verif_ROE_Normandie_H_code_ME %>% 
#   sf::st_drop_geometry() %>%
#   dplyr::count(diff_code_ME)
# 
# verif_ROE_Normandie_H_code_ME %>% 
#   dplyr::filter(diff_code_ME == 'diff' & !is.na(code_ME)) %>% 
#   nrow()

## -> 48 diff !!!

ROE_Normandie_H <-
  sf::st_join(
    ROE_Normandie_H %>%
      sf::st_transform(crs = 2154),
    BV_ME_SN_LB %>% 
      dplyr::select(code_ME_valid = CdEuMasseD),
    left = F) %>%
  dplyr::relocate('code_ME_valid', .after = 'code_ME') %>% 
  sf::st_transform(crs = 4326)

##----------------------------------------------------------------------------##
#### 8. Obtention des métriques du cours d'eau à partir de PHRYMO #####
##----------------------------------------------------------------------------##

# Reprojection des usra sur la bdtopo pour harmoniser les noms de cours d'eau

#Info_repro <-
  # sf::st_join(
  #   Info,
  #   CE_bdtopo %>% 
  #     dplyr::select(toponyme), join = sf::st_relate
  # )  %>% dplyr::mutate(
  #   toponyme_c = tolower(toponyme.y),
  #   toponyme_c = gsub("fleuve ", "", toponyme_c),
  #   toponyme_c = gsub("rivière ", "", toponyme_c),
  #   toponyme_c = gsub("^bras de\\s*", "", toponyme_c, perl = TRUE),
  #   toponyme_c = gsub("^bras du\\s*", "le ", toponyme_c, perl = TRUE),
  #   toponyme_c = gsub("^bras la\\s*", "la ", toponyme_c, perl = TRUE),
  #   toponyme_c = gsub("^rivière(?! (de|d'|du)\\b)\\s*", "", toponyme_c, perl = TRUE)
  # ) %>%
  # dplyr::rename(
  #   "nom_CE_valid" = "toponyme_c") %>% 
  # dplyr::relocate('nom_CE_valid',"toponyme.y", .after = 'toponyme.x') %>% 
  # sf::st_drop_geometry()

Info <-   
  Info %>% sf::st_drop_geometry() %>% 
  dplyr::rename('alt_am' = 'zamont',
                'alt_av' = 'zaval',
              "nom_CE_valid"="toponyme")  %>% 
  dplyr::select(nom_CE_valid, longueur, alt_av, alt_am) %>%
  dplyr::filter(alt_av != 0) %>%
  dplyr::group_by(nom_CE_valid) %>%
  dplyr::mutate(nom_CE_valid = gsub("fleuve ", "", nom_CE_valid),
                nom_CE_valid = gsub("rivière ", "", nom_CE_valid),
                nom_CE_valid = gsub("^bras de\\s*", "", nom_CE_valid, perl = TRUE),
                nom_CE_valid = gsub("^bras du\\s*", "le ", nom_CE_valid, perl = TRUE),
                nom_CE_valid = gsub("^bras la\\s*", "la ", nom_CE_valid, perl = TRUE),
                nom_CE_valid = gsub("^rivière(?! (de|d'|du)\\b)\\s*", "", nom_CE_valid, perl = TRUE)) %>%
  dplyr::mutate(
    longueur = sum(longueur) * 0.001,
    alt_am = max(alt_am),
    alt_av = min(alt_av),
    deni_nat = alt_am - alt_av,
    pt_nat = deni_nat / longueur
  ) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  dplyr::ungroup(nom_CE_valid) %>% 
  dplyr::distinct() %>% 
  #filtrer pour les cours d'eau qui nous intéressent 
  dplyr::filter(nom_CE_valid %in% ROE_Normandie_H$nom_CE_valid) 

#304/697 sans traitement approfondi
#469 /697 avec repojection sur bdtopo -> pb de repro 

#Obtention du code ME pour Info
tab_me <-
  ROE_Normandie_H %>%  sf::st_drop_geometry() %>%
  dplyr::mutate(nom_CE_valid = dplyr::case_when(nom_CE_valid == "NA" ~ NA,
                                                TRUE ~ nom_CE_valid)) %>% 
  tidyr::separate(col =  tidyr::matches("code_ME_valid"),
                  sep = '-',
                  c("code_ME_valid", "code_CE")) %>%
  dplyr::select("nom_CE_valid", "code_ME_valid") %>% 
  unique() %>%
  na.omit() %>%
  dplyr::group_by(nom_CE_valid) %>% 
  dplyr::summarise_all(~ paste(., collapse = ' - ')) 

Info <- dplyr::left_join(Info, tab_me, dplyr::join_by('nom_CE_valid'))

Info <- dplyr::relocate(Info,"code_ME_valid", .before =  "nom_CE_valid")

##---------------------------------------------------------------------------------##
#### 9. Calcul de différentes métriques sur la situation du CE ainsi que son TE #####
##---------------------------------------------------------------------------------##

#Calcul du nombre d'ouvrage sur CE, du nombre d'ouvarge sur le cours principal et de la hauteur de chute cuumulée de tous les ouvrages

tab_h <- ROE_Normandie_H %>%  sf::st_drop_geometry() %>% 
  dplyr::filter(hauteur != is.na(hauteur),
                nom_CE_valid != "NA") %>% 
  dplyr::group_by(nom_CE_valid) %>% 
  dplyr::add_count(nom_CE_valid, name = "nb_ouv") %>% 
  dplyr::filter(ouv_liaison == "ouvrage principal") %>% 
  dplyr::add_count(nom_CE_valid, name = "nb_ouvp") %>%
  dplyr::mutate(H_cum = sum(hauteur)) %>%
  dplyr::ungroup() %>% 
  dplyr::select("nom_CE_valid", "H_cum", "nb_ouv", "nb_ouvp") %>% 
  unique()

Info <- dplyr::left_join(Info, tab_h, dplyr::join_by('nom_CE_valid'))


#Calcul du TE

Info <- Info %>% dplyr::mutate(TE = round(H_cum/deni_nat*100,2)) %>% 
  dplyr::relocate("nb_ouvp","nb_ouv", .before =  "H_cum")

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##### Sauvegarder les objets utiles à la réalisation des prochains scripts ####
##----------------------------------------------------------------------------##
save(bbox_normandie, # a supprimer par la suite ?
     normandie_area, # a supprimer par la suite ?
     ROE_Normandie, # a supprimer par la suite ?
     ROE_Normandie_H,
     CE_bdtopo,
     Info, 
     file = "data_prepared/ROE_data.RData")
##----------------------------------------------------------------------------##