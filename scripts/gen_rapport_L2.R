generate_report_L2 <- function(data, output_dir, id_t = NULL, generate_all_reports = FALSE) {

'%>%' <- dplyr::'%>%'

    # Render report using rmarkdown for each value in the column
    if (generate_all_reports) {
    
    
      # Check for NA values in the TE column
      na_values_in_TE <- sum(is.na(data$TE))
      
      if (na_values_in_TE > 0) {
        
        warning(paste("Removing", na_values_in_TE, "NA values from TE."))
        
        data <- data %>% dplyr::filter(!is.na(TE))
      }
    
    data %>%
      dplyr::pull("id_troncon_L2") %>%
      purrr::map(
        .f = function(x) {
          L2_tp <-
            data %>% 
            dplyr::filter(id_troncon_L2 == x) %>% 
            dplyr::pull(label_troncon_L2)
          
          dep <-
            data %>% 
            dplyr::filter(id_troncon_L2 == x) %>% 
            dplyr::pull(DPT_noms)
          
          cse <-
            data %>% 
            dplyr::filter(id_troncon_L2 == x) %>% 
            dplyr::pull(NomZone_L2_sandre)
          
          rmarkdown::render(
            input = "./ap/template/template_L2.Rmd",
            output_dir = output_dir,
            output_file = paste("tx_etagement_", L2_tp, "_", format(Sys.time(), '%Y_%m_%d'), ".docx"),
            params = list(CE = L2_tp,  DEP = dep, CSE = cse) 
          )
        }
      )
  } else {
    # Check for NA values in the TE column
    na_TE <- data %>% 
      dplyr::filter(id_troncon_L2 == id_t) %>% 
      dplyr::pull(TE) %>% 
      is.na() %>% 
      sum()
    
    if (na_TE > 0) {
      stop("Error: The TE column contains NA values. Please remove them before generating reports.")
    }
    
    # "id_t" must be specify when generate_all_reports = FALSE 
    if (is.null(id_t)) {
      stop("Error: id_t must be specified when generate_all_reports is FALSE.")
    }
    
    # Render report using rmarkdown for one unique value specified by the user
    L2_tp <-
      data %>% 
      dplyr::filter(id_troncon_L2 == id_t) %>% 
      dplyr::pull(label_troncon_L2)
    
    dep <-
      data %>% 
      dplyr::filter(id_troncon_L2 == id_t) %>% 
      dplyr::pull(DPT_noms)
    
    cse <-
      data %>% 
      dplyr::filter(id_troncon_L2 == id_t) %>% 
      dplyr::pull(NomZone_L2_sandre)
    
    rmarkdown::render(
      input = "./ap/template/template_L2.Rmd",
      output_dir = output_dir,
      output_file = paste("tx_etagement_", L2_tp, "_", format(Sys.time(), '%Y_%m_%d'), ".docx"),
      params = list(CE = L2_tp,  DEP = dep, CSE = cse) 
    
    )
  }
}


load("./ap/data_prepared/ROE_data.RData")


