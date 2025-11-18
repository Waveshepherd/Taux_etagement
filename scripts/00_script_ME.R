# install.packages(c("tidyverse","sf","happign", "lwgeom", "terra", "tabulapdf"))
#
# # install.packages("devtools")
# devtools::install_github("paul-carteron/happign")
library(tidyverse)
library(sf)
library(happign)
library(lwgeom)
library(terra)
library(tabulapdf)

load("./ap/data_prepared/df_final_1.RData")

'%>%' <- dplyr::'%>%'

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
  sf::st_sf() %>% 
  sf::st_transform(2154)

# Extraction des données d'altimétries à partir des métadonnées IGN

layers_metadata <- happign::get_layers_metadata("wms-r", "altimetrie")
dem_layer <- layers_metadata[2, 1] #ELEVATION.ELEVATIONGRIDCOVERAGE


# Récupérer le tableaux des 74 ME
tab_ME <-
  tabulapdf::extract_tables("./ap/data_prepared/2020_12_Bilan_RCE_OFB.pdf", pages = 5)[[1]] %>%
  dplyr::mutate(nom_me = gsub(x = stringr::str_extract(ID_FONCTIO, pattern = "^[^()]+"), pattern = " ", replacement = ""),
                nom_me = paste0('FR', nom_me))

## 1.l. Chargement des linéaires de ME

ME_sf <- sf::read_sf("ap/data_prepared/Lin_MasseDeau_SN_LB_EDL.gpkg")


ME_sf <- ME_sf %>% sf::st_transform(2154)

ME_sf_NOR <- ME_sf[lengths(sf::st_intersects(ME_sf, normandie_area %>% sf::st_buffer(dist = 100))) > 0,]

setdiff(tab_ME$nom_me, ME_sf_NOR$cdeumassed) ## il manque "FRHR336A" et "FRHR336B" qui sont regroupées en une seule et "FRHR348B" n'existe pas daéns ce référentiel

ME_sf_NOR_prio <-
  ME_sf_NOR %>%
  dplyr::filter(cdeumassed %in% tab_ME$nom_me | cdeumassed == "FRHR336") #FRHR336 est divisée en A et B dans le tableau mais pas dans le référentiel
length(unique(ME_sf_NOR_prio$cdeumassed))

## Permet de récupérer les coordonnées du point amont et aval de chaque cours d'eau ainsi que leurs longueur

ME_sf_NOR_prio_pts <-
  ME_sf_NOR_prio %>%
  dplyr::select(cdeumassed, nommassede, LongueurTo) %>%
  dplyr::group_by(cdeumassed) %>%
  dplyr::mutate(start = lwgeom::st_startpoint(geom),
         end = lwgeom::st_endpoint(geom),
         long = sf::st_length(geom)) %>%
  sf::st_drop_geometry() %>%
  dplyr::ungroup()  %>% 
  tidyr::pivot_longer(cols = c(start, end), names_to = "coords") %>%
  sf::st_as_sf()

## Récupération des données d'altimétries aux points aval et amont de chaque linéaire de ME  

df_final_1 <- NULL
for(i in 1:nrow(ME_sf_NOR_prio_pts)){
  
  print(glue::glue("row number {i}"))
  
  df_tp <- 
    ME_sf_NOR_prio_pts %>% 
    dplyr::slice(i) %>% 
    sf::st_buffer(5) %>% 
    dplyr::rename(geometry = value) %>% 
    dplyr::select(geometry) %>% 
    st_as_sf()
  
  tmp <- df_tp$geometry %>% st_as_sf() %>% dplyr::rename(geometry = x)

  mnt_1m <- happign::get_wms_raster(x = tmp,
                           filename = "scripts/mnt_ign_1m.tif",
                           layer = dem_layer,
                           overwrite = T,
                           res = 1,
                           crs = 2154,
                           rgb = FALSE)
  
  mnt_1m[mnt_1m<0] <- NA

  ext_tmp <-
  terra::extract(mnt_1m, 
                 ME_sf_NOR_prio_pts %>% 
                   dplyr::slice(i))
  
  df_tp2 <-
  ME_sf_NOR_prio_pts %>% 
    dplyr::slice(i) %>% 
    dplyr::mutate(extraction_mnt = ext_tmp[2]) 
  
  df_final_1 <- rbind(df_final_1, df_tp2)
}

setdiff(tab_ME$nom_me, df_final_1$cdeumassed)

save(df_final_1, file = "ap/data_prepared/df_final_1.Rdata")
