generate_report_ME <- function(data, output_dir, cdme = NULL, generate_all_reports = FALSE) {
  
  '%>%' <- dplyr::'%>%'
  
  # Render report using rmarkdown for each value in the column
  if (generate_all_reports) {
    
    
    # Check for NA values in the TE column
    na_values_in_TE <- sum(is.na(data$TE))
    
    if (na_values_in_TE > 0) {
      
      warning(paste("Removing", na_values_in_TE, "NA values from TE"))
      
      data <- data %>% dplyr::filter(!is.na(TE))
    }
    
    data %>%
      dplyr::pull("cdeumassed") %>%
      purrr::map(
        .f = function(x) {
          me_tp <-
            data %>% 
            dplyr::filter(cdeumassed == x) %>% 
            dplyr::pull(nommassede)
          
          rmarkdown::render(
            input = "./ap/template/template_ME_L2.Rmd",
            output_dir = output_dir,
            output_file = paste("tx_etagement_", me_tp, "_", format(Sys.time(), '%Y_%m_%d'), ".docx"),
            params = list(CME_L2 = x, ME = me_tp)
          )
        }
      )
  } else {
    # Check for NA values in the TE column
    na_TE <- data %>% 
      dplyr::filter(cdeumassed == cdme) %>% 
      dplyr::pull(TE) %>% 
      is.na() %>% 
      sum()
    
    if (na_TE > 0) {
      stop("Error: The TE column contains NA values. Please remove them before generating reports.")
    }
    
    # "cdme" must be specify when generate_all_reports = FALSE 
    if (is.null(cdme)) {
      stop("Error: cdme must be specified when generate_all_reports is FALSE")
    }
    
    # Render report using rmarkdown for one unique value specified by the user
    me_tp <-
      ME_L2_lin %>% 
      dplyr::filter(cdeumassed == cdme) %>% 
      dplyr::pull(nommassede)
    
    rmarkdown::render(
      input = "./ap/template/template_ME_L2.Rmd",
      output_dir = output_dir,
      output_file = paste("tx_etagement_", me_tp, "_", format(Sys.time(), '%Y_%m_%d'), ".docx"),
      params = list(CME_L2 = cdme, ME = me_tp)
    )
  }
}


load("./ap/data_prepared/ROE_data.RData")



