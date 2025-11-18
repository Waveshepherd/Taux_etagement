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

# Chargement du flux obtenu sur la page de Geobs (pour les informations sur les ROE)

ROE_Normandie <- 
  r4geobs::get_geobs_wfs_data("REFERENTIEL_ROE_MONDE", normandie_area)


## 1.b. Chargement des données BDOE issues de Geobs (pour les hauteurs) ##

bdoe <-
  r4geobs::get_bdoe_data(
    login = "",
    mdp = "",
    nom_dossier = "./ap/data_prepared/bdoe",
    lecture = TRUE
  )

## 1.c. Chargement des données PHRYMO

Info_LB <- vroom::vroom(
  "//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/IG_METIER/CONTINUITE/PHRYMO/usra_LB.csv" 
)

Info_NOR <- vroom::vroom(
  "//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/IG_METIER/CONTINUITE/PHRYMO/usra_NOR.csv"
)

Info <- 
  dplyr::bind_rows(Info_LB, Info_NOR)

## 1.d. Chargement des données CE de la BD Topo (BDTopage)

CE_bdtopo <- 
  sf::read_sf(
    "./ap/data_prepared/CE_BDTOPO.gpkg")

## 1.f. Chargement des données géographiques des tronçons "Liste 2" à partir du Sandre 

CE_L2_sandre <- 
  r4geobs::get_classCE_wfs(geo = bbox_normandie, liste = "Liste2")

## 1.g. Chargement des données issues du travail de récup sur les L2

load("./ap/data_prepared/L2_reclassif_ROE.RData") ## nom : L2_reclassif_ROE

## 1.h. Chargement des données sur les Masses d'eau (SN et LB)

BV_ME_SN_LB <- 
  sf::read_sf("./ap/data_prepared/BV_ME_SN_LB.gpkg")

## 1.i. Chargement des linéaires de CE classés liste 2 (?)

CE_L2_lin <- 
  sf::read_sf(
    "./ap/data_prepared/troncons_L2_NOR_verif_ME_dpt_vf.gpkg")

## 1.j. Chargement des données CE classés liste 2 buffer 50m (?)

CE_L2_buff <- 
  sf::read_sf(
    "./ap/data_prepared/CE_liste2_buff50.gpkg")

## 1.k. Chargement des tronçons usra L2
usra_L2 <- 
  sf::read_sf(
    "./ap/data_prepared/usra_sel_final_L2.gpkg")

## 1.l. Chargement des linéaires de ME

ME_sf <- sf::read_sf("ap/data_prepared/Lin_MasseDeau_SN_LB_EDL.gpkg")

## 1.m. Chargement des MNT1m pour les linéaires de ME_L2

load("ap/data_prepared/df_final_1.RData")

##----------------------------------------------------------------------------##
#### 2. Sélection des données utiles du flux Geobs pour la Normandie ####
##----------------------------------------------------------------------------##

# nrow(ROE_Normandie) # 9801 lignes

ROE_Normandie <-
  ROE_Normandie %>%
  # Repositionnement des ROE en bordure de departement
  dplyr::mutate(dept_nom = dplyr::case_when(
    identifiant_roe %in% c('ROE14725', 
                           'ROE117117', 
                           'ROE119897',
                           'ROE119898') ~ 'MANCHE',
    identifiant_roe %in% c('ROE27825', 
                           'ROE21617') ~ 'CALVADOS',
    identifiant_roe %in% c('ROE54558',
                           'ROE88613',
                           "ROE44037",
                           "ROE44039",
                           "ROE44047",
                           "ROE44050",
                           "ROE65934",
                           "ROE65935",
                           "ROE65936",
                           "ROE65937",
                           "ROE65938",
                           "ROE65939",
                           "ROE65940",
                           "ROE65941",
                           "ROE65941",
                           "ROE65928",
                           "ROE65927",
                           "ROE65925",
                           "ROE65895",
                           "ROE65893",
                           "ROE65890",
                           "ROE65892",
                           "ROE65894",
                           "ROE77785",
                           "ROE65891",
                           "ROE43804",
                           "ROE77788",
                           "ROE65889",
                           "ROE43762",
                           "ROE77193",
                           "ROE43753",
                           "ROE43748",
                           "ROE105804",
                           "ROE65882",
                           "ROE65883",
                           "ROE65860",
                           "ROE65857",
                           "ROE65855",
                           "ROE38669",
                           "ROE38651",
                           "ROE73112",
                           "ROE38684",
                           "ROE69743",
                           "ROE73110",
                           "ROE24085",
                           "ROE73111",
                           "ROE65840",
                           "ROE65846",
                           "ROE38634",
                           "ROE69746",
                           "ROE65832",
                           "ROE65831",
                           "ROE38609",
                           "ROE106644",
                           "ROE38312",
                           "ROE65829",
                           "ROE34410",
                           "ROE105786",
                           "ROE34408",
                           "ROE65826",
                           "ROE65825",
                           "ROE65827",
                           "ROE65824",
                           "ROE65822",
                           "ROE106647",
                           "ROE65823",
                           "ROE105785",
                           "ROE34342",
                           "ROE65819",
                           "ROE105783",
                           "ROE34340",
                           "ROE65820",
                           "ROE34335",
                           "ROE34300",
                           "ROE74241",
                           "ROE26142",
                           "ROE26133",
                           "ROE26149",
                           "ROE26145",
                           "ROE27601",
                           "ROE27625",
                           "ROE27641",
                           "ROE65951",
                           "ROE67727",
                           "ROE27592",
                           "ROE27551",
                           "ROE27567",
                           "ROE27071",
                           "ROE65949",
                           "ROE27085",
                           "ROE27098",
                           "ROE27586",
                           "ROE34412",
                           "ROE69761",
                           "ROE38351",
                           "ROE69746",
                           "ROE38599",
                           "ROE38609",
                           "ROE65832",
                           "ROE65831",
                           "ROE43795",
                           "ROE105805",
                           "ROE77784",
                           "ROE43872",
                           "") ~ 'SEINE-MARITIME',
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


##----------------------------------------------------------------------------##
#### 3. Uniformisation des noms de cours d'eau ####
##----------------------------------------------------------------------------##

## 3. a. Création du champ "nom_CE" qui se base sur les nom de CE issus de l'ancienne de la BD Topo  disponible sur Geobs ##

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

# ROE_Normandie %>% filter(is.na(nom_CE)) %>% nrow() # 1027 lignes

## 3. b. Création du champ "nom_CE_valid" qui se base sur les nom de CE issus de la version 2022 de la BD Topo ##

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

# cb de 'nom_CE' encore manquants ?
# ROE_Normandie %>% dplyr::filter(is.na(nom_CE_valid)) %>% nrow() # 38 lignes 🍾

##----------------------------------------------------------------------------##
#### 4. Obtenir les obstacles principaux et secondaires ####
##----------------------------------------------------------------------------##

# Création d'une liste contenant tous les ouvrages secondaires liées à un ouvrage principal 

ouvrages_lies_liste <-
  ouvrages_lies_liste <-
  ROE_Normandie %>%
  dplyr::filter(!is.na(ouvrages_lies)) %>%
  # rajout d'un début et fin de ligne pour éviter les erreurs de détection
  dplyr::mutate(ouvrages_lies = paste0("^", ouvrages_lies, "$")) %>% 
  dplyr::pull(ouvrages_lies) %>%
  gsub(" - ", "$|^", .) %>%
  paste(collapse = "|")

# Création du champ "ouv_liaison" pour déterminer si un ouvrage est principal ou secondaire 

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
      )) %>% 
      dplyr::mutate(ouv_liaison = dplyr::case_when(stringr::str_detect(toponyme, pattern = "^Bras", negate = FALSE) ~ "ouvrage secondaire",
                                                   TRUE ~ ouv_liaison)) %>% 
  dplyr::relocate('ouv_liaison', .after = 'ouvrages_lies')

# Modifications manuelles de certains ROE qui sont considérés comme "principals" par le code actuel mais qui ne sont pas sur le cours principal du CE

ROE_Normandie <- 
  ROE_Normandie %>% 
  dplyr::mutate(ouv_liaison = dplyr::case_when(ROE == "ROE46995" ~ "ouvrage secondaire",
                                               ROE == "ROE46993" ~ "ouvrage secondaire",
                                               ROE == "ROE44944" ~ "ouvrage secondaire",
                                               ROE == "ROE89260" ~ "ouvrage secondaire",
                                               ROE == "ROE95576" ~ "ouvrage secondaire",
                                               ROE == "ROE44942" ~ "ouvrage secondaire",
                                               ROE == "ROE44940" ~ "ouvrage secondaire",
                                               ROE == "ROE110869" ~ "ouvrage secondaire",
                                               ROE == "ROE90919" ~ "ouvrage secondaire",
                                               ROE == "ROE110866" ~ "ouvrage secondaire",
                                               ROE == "ROE44058" ~ "ouvragre principal",
                                               TRUE ~ ouv_liaison))


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

#Traitement spécifique de certains ouvrages dû à la méthode employée pour l'extrapolation des ouvrages classés L2

ROE_Normandie_H <-
  ROE_Normandie_H %>% 
  dplyr::mutate(ouv_liste2_valid = dplyr::case_when(ROE == "ROE44110" ~ "???",
                                                    TRUE ~ ouv_liste2_valid))

#Filter les ROE sur les tronçons L2
ROE_Normandie_H_L2 <- ROE_Normandie_H %>% dplyr::mutate(ouv_liste2_valid = dplyr::case_when(ouv_liste2 == "Liste2" ~ "Liste2",
                                                                               TRUE ~ ouv_liste2_valid)) %>% 
  dplyr::filter(ouv_liste2_valid == "Liste2")

# Récupérer le nom du tronçon L2 associé aux ROE

CE_L2_lin <- CE_L2_lin %>% sf::st_transform(crs = 4326)

ROE_Normandie_H_L2 <-
  sf::st_join(
    ROE_Normandie_H_L2,
    CE_L2_lin %>% 
      dplyr::select(label_troncon_L2, id_troncon_L2), join = sf::st_nearest_feature
  ) %>% 
  dplyr::relocate(label_troncon_L2, id_troncon_L2, .after = 'nom_carthage')

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

# Pour les tronçons L2
ROE_Normandie_H_L2 <-
  sf::st_join(
    ROE_Normandie_H_L2 %>%
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

#Calcul du nombre d'ouvrage sur CE, du nombre d'ouvarge sur le cours principal et de la hauteur de chute cumulée de tous les ouvrages

tab_h <- ROE_Normandie_H %>%  sf::st_drop_geometry() %>% 
  dplyr::group_by(nom_CE_valid) %>% 
  dplyr::add_count(nom_CE_valid, name = "nb_ouv") %>% 
  dplyr::filter(ouv_liaison == "ouvrage principal") %>% 
  dplyr::add_count(nom_CE_valid, name = "nb_ouvp") %>%
  dplyr::mutate(H_cum = sum(hauteur, na.rm = TRUE)) %>%
  dplyr::ungroup() %>% 
  dplyr::select("nom_CE_valid", "H_cum", "nb_ouv", "nb_ouvp") %>% 
  unique()

Info <- dplyr::left_join(Info, tab_h, dplyr::join_by('nom_CE_valid'))


#Calcul du TE

Info <- Info %>% dplyr::mutate(TE = round(H_cum/deni_nat*100,1)) %>% 
  dplyr::mutate(dplyr::across(3:6, round,0),
                dplyr::across(8:9, round,0),
                pt_nat = round(pt_nat,1)) %>% 
  dplyr::relocate("nb_ouv","nb_ouvp", .before =  "H_cum")

#Ajustements dernière minute

Info <- Info %>% dplyr::filter(!is.na(nom_CE_valid)) 
Info <- Info[order(Info$nom_CE_valid),]

ROE_Normandie_H <- ROE_Normandie_H %>% dplyr::filter(!is.na(nom_CE_valid))
ROE_Normandie_H <- ROE_Normandie_H[order(ROE_Normandie_H$nom_CE_valid),]

##---------------------------------------------------------------------------------##
#### 10. Limitation des données aux tronçons de CE classés L2 #####
##---------------------------------------------------------------------------------##


## 10. a. ROE ##
# 
# 
# CE_L2_lin <- CE_L2_lin %>% 
#   dplyr::mutate(
#     NomZone = tolower(NomZone),
#     NomZone = gsub("fleuve ", "", NomZone),
#     NomZone = gsub("rivière ", "", NomZone),
#     NomZone = gsub("^bras de\\s*", "", NomZone, perl = TRUE),
#     NomZone = gsub("^bras du\\s*", "le ", NomZone, perl = TRUE),
#     NomZone = gsub("^bras la\\s*", "la ", NomZone, perl = TRUE),
#     NomZone = gsub("^rivière(?! (de|d'|du)\\b)\\s*", "", NomZone, perl = TRUE))
# 
# ROE_Normandie_H_L2_filter <- ROE_Normandie_H_L2 %>% dplyr::filter(!is.na(NomZone))
# 
# #Buffer 50 m suffisant ?
# mapview::mapview(ROE_Normandie_H, zcol = "ouv_liste2_valid") + CE_L2_lin
# 
# #Voit que ceux qui sont exclu = en-dehors du tracé
#mapview::mapview(ROE_Normandie_H, zcol = "ouv_liste2", na.color = "green") + CE_L2_lin
# 
# ## 10. b. Info ##
# 


Info_L2 <- usra_L2 %>% sf::st_drop_geometry() %>% 
  dplyr::rename('alt_am' = 'zamont',
                'alt_av' = 'zaval') %>% 
  dplyr::select(id_troncon_L2, label_troncon_L2, longueur, alt_av, alt_am) %>%
  dplyr::group_by(id_troncon_L2) %>% 
  dplyr::filter(alt_av != 0) %>%
  dplyr::mutate(
    longueur = sum(longueur) * 0.001,
    alt_am = max(alt_am),
    alt_av = min(alt_av),
    deni_nat = alt_am - alt_av,
    pt_nat = deni_nat / longueur
  ) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  dplyr::ungroup(id_troncon_L2) %>%
  dplyr::distinct() 

#Obtention du code ME pour Info_L2
tab_me <-
  ROE_Normandie_H_L2 %>%  sf::st_drop_geometry() %>%
  dplyr::mutate(label_troncon_L2 = dplyr::case_when(label_troncon_L2 == "NA" ~ NA,
                                                TRUE ~ label_troncon_L2)) %>% 
  tidyr::separate(col =  tidyr::matches("code_ME_valid"),
                  sep = '-',
                  c("code_ME_valid", "code_CE")) %>%
  dplyr::select("label_troncon_L2", "code_ME_valid") %>% 
  unique() %>%
  na.omit() %>%
  dplyr::group_by(label_troncon_L2) %>% 
  dplyr::summarise_all(~ paste(., collapse = ' - ')) 

Info_L2 <- dplyr::left_join(Info_L2, tab_me, dplyr::join_by('label_troncon_L2'))

Info_L2 <- dplyr::relocate(Info_L2,"code_ME_valid", .before =  "label_troncon_L2")

#Calcul du nombre d'ouvrage sur CE, du nombre d'ouvarge sur le cours principal et de la hauteur de chute cuumulée de tous les ouvrages

tab_h <- ROE_Normandie_H_L2 %>%  sf::st_drop_geometry() %>%
  dplyr::group_by(id_troncon_L2) %>%
  dplyr::add_count(id_troncon_L2, name = "nb_ouv") %>%
  dplyr::filter(ouv_liaison == "ouvrage principal") %>%
  dplyr::add_count(id_troncon_L2, name = "nb_ouvp") %>%
  dplyr::filter(hauteur != is.na(hauteur),
                id_troncon_L2 != "NA") %>%
  dplyr::mutate(H_cum = sum(hauteur)) %>%
  dplyr::ungroup() %>%
  dplyr::select("label_troncon_L2", "H_cum", "nb_ouv", "nb_ouvp") %>%
  unique()

Info_L2 <- dplyr::left_join(Info_L2, tab_h, dplyr::join_by('label_troncon_L2'))

#Attention car 2 tronçons on le même nom mais ceux ne sont pas les mêmes

#Calcul du TE

Info_L2 <- Info_L2 %>% dplyr::mutate(TE = round(H_cum/deni_nat*100,2)) %>%
  dplyr::relocate("nb_ouvp","nb_ouv", .before =  "H_cum")

Info_L2 <- Info_L2 %>% dplyr::filter(!is.na(TE)) 
Info_L2 <- Info_L2[order(Info_L2$label_troncon_L2),]

ROE_Normandie_H_L2 <- ROE_Normandie_H_L2 %>% dplyr::filter(!is.na(label_troncon_L2))
ROE_Normandie_H_L2 <- ROE_Normandie_H_L2[order(ROE_Normandie_H_L2$label_troncon_L2),]
# 
# #Manque des tronçons
# mapview::mapview(Info_L2, color = "red") + CE_L2_lin + mapview::mapview(ROE_Normandie_H_L2_filter, color = "green")

# ## 10. b. Tronçons mis en qualités ##

Dep_CE <- CE_L2_lin %>%  sf::st_drop_geometry() %>% dplyr::select(DPT_noms, NomZone_L2_sandre, label_troncon_L2)
Info_L2 <- dplyr::left_join(Info_L2, Dep_CE, dplyr::join_by('label_troncon_L2'))

Info_L2 <- Info_L2 %>% 
  dplyr::mutate(MEQ = dplyr::case_when(label_troncon_L2 == "Bailly Bec_FR1302_FRHR165-G2220600_76" ~ "non",                                                               
                                                            label_troncon_L2 =="Bras de la Gièze_FR1528_FRHR336-I7030600_50" ~ "non",                                                         
                                                            label_troncon_L2 == "Bras du Moulin des Bois_FR1584_FRHR345-I8108000_50"~ "non",                                                  
                                                            label_troncon_L2 == "Canal des Moulins_FR1410_FRHR268_27" ~ "oui",                                                                 
                                                            label_troncon_L2 == "Cours d'Eau 01 de la Commune d'Appeville-Annebault_FR1605_FRHR268-H6234050_27" ~ "oui",                       
                                                            label_troncon_L2 == "Cours d'Eau 01 de la Commune de Beaussault_FR1294_FRHR162-G2011100_76" ~ "non",                               
                                                            label_troncon_L2 == "Cours d'Eau 01 de la Commune de Boulleville_FR1602_FRHR_T07-H6270650_27" ~ "oui",                             
                                                            label_troncon_L2 == "Cours d'Eau 01 de la Commune de Brionne_FR1606_FRHR268-H6200700_27" ~ "non",                                  
                                                            label_troncon_L2 == "Cours d'Eau 01 de la Commune de la Torpt_FR1610_FRHR270-H6266000_27" ~ "non",                                 
                                                            label_troncon_L2 == "Cours d'Eau 02 de Beaulieu_FR1479_FRHR302_61" ~ "non",                                                        
                                                            label_troncon_L2 == "Cours d'Eau 02 de la Commune de Fontaine-la-Soret_FR1607_FRHR268-H6200650_27" ~ "oui",                        
                                                            label_troncon_L2 == "Cours d'Eau 09 du Château_FR1551_FRHR321-I4557000_14" ~ "non",                                                
                                                            label_troncon_L2 == "Douet du Mieux_FR1447_FRHR277_14" ~ "non",                                                                    
                                                            label_troncon_L2 == "Douet_FR1441_FRHR277-I0409000_14" ~ "non",                                                                    
                                                            label_troncon_L2 == "Fontaine Saint-Pierre_FR1274_FRHR159-G0153000_76" ~ "oui",                                                    
                                                            label_troncon_L2 == "Fossé 08 de la Commune de Saint-Evroult-de-Montfort_FR1491_FRHR275-I0119000_61" ~ "non",                      
                                                            label_troncon_L2 == "l'Airon_FR1623_FRHR347_50" ~ "oui",                                                                          
                                                            label_troncon_L2 == "l'Airou_FR1525_FRHR337_50"  ~ "oui",                                                                         
                                                            label_troncon_L2 == "l'Algot_FR1430_FRHR284-I1380600_14" ~ "oui",                                                                 
                                                            label_troncon_L2 == "l'Allemagne_FR1520_FRHR343-I7719000_50"  ~ "oui",                                                            
                                                            label_troncon_L2 == "l'Ancre_FR1449_FRHR290_14" ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Andelle_FR1381_FRHR353_76" ~ "oui",                                                                        
                                                            label_troncon_L2 == "l'Aure_FR1588_FRHR320_14" ~ "non",                                                                            
                                                            label_troncon_L2 == "l'Aurette_FR1560_FRHR320-I4510600_14" ~ "non",                                                                
                                                            label_troncon_L2 == "l'Avre_FR1463_FRHR256_27" ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Ay_FR1530_FRHR335_50" ~ "non",                                                                              
                                                            label_troncon_L2 == "l'Eaulne_FR1253_FRHR163_76" ~ "non",                                                                          
                                                            label_troncon_L2 == "l'Eaulne_FR1401_FRHR165_76" ~ "non",                                                                          
                                                            label_troncon_L2 == "l'Epte_FR1349_FRHR239_27" ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Eure_FR1472_FRHR246A_27" ~ "oui",                                                                          
                                                            label_troncon_L2 == "l'Eure_FR1473_FRHR261_27" ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Odon_FR1540_FRHR309_14" ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Oir_FR1593_FRHR352_50"  ~ "oui",                                                                           
                                                            label_troncon_L2 == "l'Orbiquet_FR1499_FRHR276_14" ~ "oui",                                                                       
                                                            label_troncon_L2 == "l'Orne_FR1627_FRHR295_61"   ~ "oui",                                                                         
                                                            label_troncon_L2 == "l'Orne_FR1629_FRHR306_14"  ~ "oui",                                                                          
                                                            label_troncon_L2 == "l'Orne_FR1630_FRHR299A_61"  ~ "oui",                                                                         
                                                            label_troncon_L2 == "l'Yères_FR1333_FRHR161_76"  ~ "oui",                                                                         
                                                            label_troncon_L2 == "l'Yvie_FR1459_FRHR277-I0399000_14" ~ "non",                                                                   
                                                            label_troncon_L2 == "la Baize_FR1419_FRHR300_61" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Barges_FR1436_FRHR281-I1110600_61" ~ "non",                                                                
                                                            label_troncon_L2 == "la Bérence_FR1526_FRHR336-I7070600_50" ~ "non",                                                               
                                                            label_troncon_L2 == "la Béthune_FR1245_FRHR163_76" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Béthune_FR1352_FRHR163_76" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Béthune_FR1382_FRHR163_76" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Bouanne_FR1583_FRHR344-I8020600_50" ~ "non",                                                                
                                                            label_troncon_L2 == "la Bresle_FR1342_FRHR159_76" ~ "oui",                                                                        
                                                            label_troncon_L2 == "la Brévogne_FR1582_FRHR313-I4160600_14" ~ "oui",                                                              
                                                            label_troncon_L2 == "la Briante_FR0410911_FRGR1403_61" ~ "non",                                                                    
                                                            label_troncon_L2 == "la Calonne_FR1493_FRHR279_27" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Canche_FR1323_FRHR162-G2040600_76" ~ "non",                                                                
                                                            label_troncon_L2 == "la Commeauche_FR0410911_FRGR0474_61" ~ "non",                                                                 
                                                            label_troncon_L2 == "la Corbionne_FR0410911_FRGR0475_61" ~ "non",                                                                  
                                                            label_troncon_L2 == "la Coulandre_FR1478_FRHR301-I2371000_61" ~ "non",                                                             
                                                            label_troncon_L2 == "la Courteille_FR1505_FRHR301-I2360600_61" ~ "non",                                                            
                                                            label_troncon_L2 == "la Diane_FR1470_FRHR302-I2409000_14" ~ "non",                                                                 
                                                            label_troncon_L2 == "la Dives_FR1460_FRHR281_14" ~ "oui",                                                                          
                                                            label_troncon_L2 == "la Donnette_FR0410911_FRGR0475_61" ~ "non",                                                                   
                                                            label_troncon_L2 == "la Doquette_FR1527_FRHR336-I7049000_50" ~ "non",                                                              
                                                            label_troncon_L2 == "la Dorette_FR1518_FRHR285_14" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Douve_FR1542_FRHR326_50" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Drôme_FR1561_FRHR321_14" ~ "oui",                                                                          
                                                            label_troncon_L2 == "la Drôme_FR1578_FRHR316_14" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Druance_FR1498_FRHR303_14" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Durance_FR1433_FRHR302-I2404000_61" ~ "oui",                                                              
                                                            label_troncon_L2 == "la Durdent_FR1285_FRHR170_76" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Fontenelle_FR1620_FRHR264A-H5111500_76" ~ "oui",                                                           
                                                            label_troncon_L2 == "la Gine_FR1420_FRHR301-I2340600_61" ~ "non",                                                                  
                                                            label_troncon_L2 == "la Guerge_FR0410911_FRGR0022_50" ~ "non",                                                                     
                                                            label_troncon_L2 == "la Guigne_FR1556_FRHR307-I2549000_14" ~ "non",                                                                
                                                            label_troncon_L2 == "la Jambette_FR0410911_FRGR0477_61" ~ "non",                                                                   
                                                            label_troncon_L2 == "la Jeannette_FR1439_FRHR303_14" ~ "non",                                                                      
                                                            label_troncon_L2 == "la Joigne_FR1566_FRHR317-I4370600_50" ~ "oui",                                                               
                                                            label_troncon_L2 == "la Laize_FR1589_FRHR308_14" ~ "non",                                                                          
                                                            label_troncon_L2 == "la Lévrière_FR1321_FRHR238_27" ~ "oui",                                                                      
                                                            label_troncon_L2 == "la Lieure_FR1489_FRHR241-H3259000_27" ~ "oui",                                                               
                                                            label_troncon_L2 == "la Méline_FR1306_FRHR159-G0120600_76" ~ "non",                                                                
                                                            label_troncon_L2 == "la Morte Eure_FR1427_FRHR261_27" ~ "oui",                                                                   
                                                            label_troncon_L2 == "la Mue_FR1591_FRHR312_14" ~ "non",                                                                            
                                                            label_troncon_L2 == "la Paquine_FR1516_FRHR278_14" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Risle_FR1495_FRHR266_61" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Risle_FR1502_FRHR268_27"  ~ "oui",                                                                        
                                                            label_troncon_L2 == "la Rouvre_FR1444_FRHR301_61" ~ "non",                                                                         
                                                            label_troncon_L2 == "la Saâne_FR1255_FRHR168_76" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Saâne_FR1383_FRHR168_76" ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Saire_FR1536_FRHR333_50"  ~ "oui",                                                                        
                                                            label_troncon_L2 == "la Sarthe_FR0410911_FRGR0455A_61" ~ "non",                                                                    
                                                            label_troncon_L2 == "la Sarthe_FR0410911_FRGR0457_61" ~ "non",                                                                     
                                                            label_troncon_L2 == "la Scie_FR1353_FRHR167_76"  ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Sée_FR1522_FRHR344_50"  ~ "oui",                                                                          
                                                            label_troncon_L2 == "la Seine_FR1178_FRHR73A_76" ~ "non",                                                                          
                                                            label_troncon_L2 == "la Sélune_FR1562_FRHR348A_50" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Seulles_FR1534_FRHR311_14"  ~ "oui",                                                                      
                                                            label_troncon_L2 == "la Sienne_FR1529_FRHR336_50"  ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Sinope_FR1537_FRHR332_50" ~ "oui",                                                                        
                                                            label_troncon_L2 == "la Souleuvre_FR1581_FRHR315_14"  ~ "oui",                                                                    
                                                            label_troncon_L2 == "la Soulles_FR1523_FRHR341_50"  ~ "oui",                                                                      
                                                            label_troncon_L2 == "la Taute_FR1538_FRHR329_50"  ~ "oui",                                                                        
                                                            label_troncon_L2 == "la Touques_FR1497_FRHR275_14" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Valett_FR1247_FRHR170-G6000700_76" ~ "non",                                                                
                                                            label_troncon_L2 == "la Vanne_FR1524_FRHR339_50" ~ "non",                                                                          
                                                            label_troncon_L2 == "la Varenne_FR1626_FRHR164_76" ~ "oui",                                                                       
                                                            label_troncon_L2 == "la Vère_FR1406_FRHR305_61"  ~ "oui",                                                                         
                                                            label_troncon_L2 == "la Véronne_FR1455_FRHR268-H6249000_27" ~ "oui",                                                              
                                                            label_troncon_L2 == "la Vie_FR1501_FRHR284_14" ~ "oui",
                                                            label_troncon_L2 == "la Vienne_FR1354_FRHR168-G4100600_76" ~ "non",                                                                
                                                            label_troncon_L2 == "la Vilaine_FR1619_FRHR271-H7020600_27" ~ "oui",                                                              
                                                            label_troncon_L2 == "la Villette_FR0410911_FRGR1427_61" ~ "non",                                                                   
                                                            label_troncon_L2 == "la Vire_FR1590_FRHR317_50" ~ "oui",                                                                          
                                                            label_troncon_L2 == "la Virène_FR1586_FRHR313-I4110600_14" ~ "non",                                                                
                                                            label_troncon_L2 == "la Visance_FR1456_FRHR305-I2470600_61" ~ "non",                                                               
                                                            label_troncon_L2 == "le Beuvron_FR1563_FRHR350_50"  ~ "oui",                                                                      
                                                            label_troncon_L2 == "le Bieu_FR1568_FRHR344-I8--0250_50" ~ "non",                                                                  
                                                            label_troncon_L2 == "le Bourgel_FR1457_FRHR275-I0150600_61" ~ "non",                                                               
                                                            label_troncon_L2 == "le Chêne Galon_FR0410911_FRGR1593_61" ~ "non",                                                                
                                                            label_troncon_L2 == "le Cirieux_FR1428_FRHR277-I0280600_14" ~ "non",                                                               
                                                            label_troncon_L2 == "le Douet Tourtelle_FR1424_FRHR279-I0379000_27" ~ "oui",                                                      
                                                            label_troncon_L2 == "le Douet_FR1595_FRHR161-G1109000_76" ~ "non",                                                                 
                                                            label_troncon_L2 == "le Fouillebroc_FR1488_FRHR241-H3259500_27" ~ "oui",                                                          
                                                            label_troncon_L2 == "le Glanon_FR1549_FRHR344-I8040600_50" ~ "non",                                                                
                                                            label_troncon_L2 == "le Lembron_FR1486_FRHR301-I2360600_61" ~ "non",                                                               
                                                            label_troncon_L2 == "le Merderet_FR1541_FRHR327_50" ~ "non",                                                                       
                                                            label_troncon_L2 == "le Noireau_FR1451_FRHR302_61"  ~ "oui",                                                                      
                                                            label_troncon_L2 == "le Pre Dauge_FR1465_FRHR277-I0320600_14" ~ "non",                                                             
                                                            label_troncon_L2 == "le ruisseau de Culoiseau de la source jusqu'à la confluence avec la Corbionne_FR0410911__FRGR0475_61" ~ "non",
                                                            label_troncon_L2 == "le Sarthon_FR0410911_FRGR0465_61" ~ "non",                                                                    
                                                            label_troncon_L2 == "le Sébec_FR1411_FRHR268-H6254000_27" ~ "non",                                                                 
                                                            label_troncon_L2 == "le Sorson_FR1598_FRHR162-G2020600_76" ~ "non",                                                                
                                                            label_troncon_L2 == "le Thar_FR1521_FRHR343_50" ~ "oui",                                                                          
                                                            label_troncon_L2 == "le Thérain_FR1341_FRHR221_76" ~ "non",                                                                        
                                                            label_troncon_L2 == "le Trottebec_FR1611_FRHR334-I6530600_50" ~ "non",                                                             
                                                            label_troncon_L2 == "le Vieux Ruisseau_FR1569_FRHR306-I2529000_14" ~ "non",                                                        
                                                            label_troncon_L2 == "Rivière d'Angerville_FR1440_FRHR279-I0369000_27" ~ "oui",                                                    
                                                            label_troncon_L2 == "Rivière de Valmont_FR1372_FRHR171_76" ~ "oui",                                                               
                                                            label_troncon_L2 == "Ru de Saint-Evroult_FR1519_FRHR275-I0119000_61" ~ "non",                                                      
                                                            label_troncon_L2 == "Ruisseau d'Herbion_FR1580_FRHR306-I2507600_14" ~ "non",                                                       
                                                            label_troncon_L2 == "Ruisseau de Bactot_FR1550_FRHR308-I2575000_14" ~ "non",                                                       
                                                            label_troncon_L2 == "Ruisseau de Bully_FR1254_FRHR162-G2059000_76" ~ "non",                                                        
                                                            label_troncon_L2 == "Ruisseau de Chaumont_FR1426_FRHR275-I0130600_61" ~ "non",                                                     
                                                            label_troncon_L2 == "Ruisseau de Courtonnel_FR1446_FRHR276-I02-0410_14" ~ "non",                                                   
                                                            label_troncon_L2 == "Ruisseau de Ganzeville_FR1343_FRHR171-G7100600_76" ~ "oui",                                                   
                                                            label_troncon_L2 == "Ruisseau de l'Abbesse_FR1517_FRHR279-I0362000_27" ~ "non",                                                    
                                                            label_troncon_L2 == "Ruisseau de la Coignardière_FR0410911_FRGR0475_61" ~ "non",                                                   
                                                            label_troncon_L2 == "Ruisseau de la Corbie_FR1432_FRHR270_27" ~ "oui",                                                             
                                                            label_troncon_L2 == "Ruisseau de la Croix Blanche_FR1438_FRHR269_27" ~ "oui",                                                     
                                                            label_troncon_L2 == "Ruisseau de la Fontaine Maurice_FR1417_FRHR275-I0203000_14" ~ "non",                                          
                                                            label_troncon_L2 == "Ruisseau de la Freulette_FR1454_FRHR268_27" ~ "oui",                                                         
                                                            label_troncon_L2 == "Ruisseau de la Grande Vallée_FR1564_FRHR306-I2539000_14" ~ "non",                                             
                                                            label_troncon_L2 == "Ruisseau de la Vallée des Vaux_FR1579_FRHR306-I2509000_14" ~ "non",                                           
                                                            label_troncon_L2 == "Ruisseau de la Vitardiere_FR1331_FRHR159-G0109000_76" ~ "non",                                                
                                                            label_troncon_L2 == "Ruisseau de Monbayer_FR1413_FRHR302_61" ~ "non",                                                              
                                                            label_troncon_L2 == "Ruisseau de Saint-Laurent_FR1533_FRHR344-I8060600_50" ~ "non",                                                
                                                            label_troncon_L2 == "Ruisseau de Tourville_FR1431_FRHR268-H6254000_27" ~ "non",                                                    
                                                            label_troncon_L2 == "Ruisseau des Aumônes_FR1461_FRHR275-I0130600_61" ~ "non",                                                     
                                                            label_troncon_L2 == "Ruisseau des Godeliers_FR1609_FRHR270-H6266000_27" ~ "non",                                                   
                                                            label_troncon_L2 == "Ruisseau des Tanneries_FR1443_FRHR275_61" ~ "non",                                                            
                                                            label_troncon_L2 == "Ruisseau du Bec_FR1437_FRHR268-H6229000_27" ~ "oui",                                                         
                                                            label_troncon_L2 == "Ruisseau du Val Jouen_FR1608_FRHR270-H6265000_27" ~ "non",                                                    
                                                            label_troncon_L2 == "Ruisseau du Vauferment_FR1407_FRHR266-H6008000_61" ~ "non",                                                   
                                                            label_troncon_L2 == "Ruisseau du Vaunoy_FR1483_FRHR277-I0440600_14" ~ "non",                                                       
                                                            label_troncon_L2 == "Ruisseau du Vivier du Voeu_FR1351_FRHR162-G2020600_76" ~ "oui",                                              
                                                            label_troncon_L2 == "Ruisseau Saint-Christophe_FR1604_FRHR268-H6236000_27" ~ "oui",)) %>% 
  dplyr::filter(MEQ == "oui")

##----------------------------------------------------------------------------------------------------------------------##
#### 11. Obtention des métriques du cours d'eau à partir du MNT1m et travail pour obtenir les ME_L2 pour le PLAGEPOMI #####
##----------------------------------------------------------------------------------------------------------------------##

# Sélection des linéaires de masse d'eau uniquement présents en Normandie et dans un buffer autour.

ME_sf <- ME_sf %>% sf::st_transform(crs = 4326)

ME_sf_NOR <- ME_sf[lengths(sf::st_intersects(ME_sf, normandie_area %>% sf::st_buffer(dist = 100))) > 0,]

#Calcul du nombre d'ouvrage sur CE, du nombre d'ouvarge sur le cours principal et de la hauteur de chute cumulée de tous les ouvrages

tab_h <- ROE_Normandie_H_L2 %>%  sf::st_drop_geometry() %>%
  dplyr::rename("cdeumassed" = "code_ME_valid") %>% 
  dplyr::group_by(cdeumassed) %>%
  dplyr::add_count(cdeumassed, name = "nb_ouv") %>%
  dplyr::filter(ouv_liaison == "ouvrage principal") %>%
  dplyr::add_count(cdeumassed, name = "nb_ouvp") %>%
  dplyr::filter(hauteur != is.na(hauteur),
                cdeumassed != "NA") %>%
  dplyr::mutate(H_cum = sum(hauteur)) %>%
  dplyr::ungroup() %>%
  dplyr::select("cdeumassed", "H_cum", "nb_ouv", "nb_ouvp") %>%
  unique()

# Bascule du point aval et amont en format large pour calculer les métriques

df_final_1 <- df_final_1 %>% sf::st_drop_geometry() %>% tidyr::pivot_wider(names_from = coords, values_from = extraction_mnt) %>% 
  dplyr::left_join(tab_h, dplyr::join_by("cdeumassed")) %>% unique()

# Calcul du dénivelé et de la pente naturelle

df_final_1 <- df_final_1 %>%  
  dplyr::group_by(cdeumassed) %>% 
  dplyr::mutate(long = as.vector(long) * 0.001,
                deni_nat = start-end,
                pt_nat = deni_nat / long)%>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  dplyr::ungroup()%>%
  unique()

#Calcul du TE
df_final_1 <- do.call(data.frame, df_final_1)
df_final_1 <- tibble::remove_rownames(df_final_1)

ME_L2_lin <- df_final_1 %>%
  dplyr::relocate("nb_ouvp","nb_ouv", .before =  "H_cum") %>% dplyr::select(!LongueurTo) %>% dplyr::rename("longueur"="long",
                                                                                             "amont"="mnt_ign_1m",
                                                                                             "aval"="mnt_ign_1m.1",
                                                                                             "deni_nat"="mnt_ign_1m.2",
                                                                                             "pt_nat"="mnt_ign_1m.3") %>%
  dplyr::relocate("aval", .before =  "amont") %>% dplyr::mutate(TE = round(H_cum/deni_nat*100,2))

# Dep_CE <- CE_L2_lin %>%  sf::st_drop_geometry() %>% dplyr::select(DPT_noms, CdEuMasseD) %>%
#   dplyr::rename("cdeumassed" = "CdEuMasseD") %>%  unique()
# ME_L2_lin <- dplyr::left_join(ME_L2_lin, Dep_CE, dplyr::join_by('cdeumassed'))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##### Sauvegarder les objets utiles à la réalisation des prochains scripts ####
##----------------------------------------------------------------------------##
save(bbox_normandie, # a supprimer par la suite ?
     normandie_area, # a supprimer par la suite ?
     ROE_Normandie, # a supprimer par la suite ?
     ROE_Normandie_H,
     ROE_Normandie_H_L2,
     CE_bdtopo,
     CE_L2_lin,
     Info,
     Info_L2,
     ME_L2_lin,
     ME_sf_NOR,
     file = "./ap/data_prepared/ROE_data.RData")
##----------------------------------------------------------------------------##