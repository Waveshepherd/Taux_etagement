##%######################################################%##
#                                                          #
####    Generer le fichier Rmarkdown au format docx     ####
#                                                          #
##%######################################################%##


load("./ap/data_prepared/ROE_data.RData")

##----------------------------------------------------------------------------##
#### 1. Générer les rapports par masse d'eau ####
##----------------------------------------------------------------------------##

ME_L2_lin_NA <- ME_L2_lin %>% dplyr::filter(is.na(TE)) #penser à supprimer le barplot du template avant de lancer le script
ME_L2_lin_nNA <- ME_L2_lin %>% dplyr::filter(!is.na(TE))

'%>%' <- dplyr::'%>%'

ME_L2_lin_nNA$cdeumassed[2:2] %>% 
  purrr::map(
    .f = function(x) {
      # dpt_tp <-
      #   Info_L2 %>% filter(label_troncon_L2 == x) %>% pull(DPT_noms, NomZone_L2_sandre)
      me_tp <-
        ME_L2_lin %>% dplyr::filter(cdeumassed == x) %>% dplyr::pull(nommassede)
      rmarkdown::render(input = "./ap/template/template_ME_L2.Rmd",
                        output_dir = "./output_MEl2", 
                        output_file = paste("tx_etagement_",me_tp,"_", format(Sys.time(), '%Y_%m_%d'),".docx"),
                        params = list(CME_L2 = x,
                                      ME = me_tp)
      )
    }
  )

##----------------------------------------------------------------------------##
#### 1. Générer les rapports par tronçons L2 ####
##----------------------------------------------------------------------------##
