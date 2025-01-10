##%######################################################%##
#                                                          #
####    Generer le fichier Rmarkdown au format html     ####
#                                                          #
##%######################################################%##




Info$nom_CE[2:4] %>% 
  purrr::map(
    .f = function(x) {
      rmarkdown::render(input = "asset/Backbone_TE_test.Rmd",
                        output_dir = "output", 
                        output_file = paste("Fichier_html", x),
                        params = list(CE = x)
      )
    }
  )


