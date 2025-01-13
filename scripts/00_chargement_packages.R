##%######################################################%##
#                                                          #
####              Chargement des packages               ####
#                                                          #
##%######################################################%##

# if (!require("renv")) install.packages("renv")

#### Fonctionnement de renv ------------------------------------
### Restaure les packages dans le lockfile :
# renv::restore()



### Restaure les packages dans le lockfile :

library(tidyverse)
#library(tidyr) <- fait partie de tidyverse
#library(plyr)
#library(tibble) <- fait partie de tidyverse
library(shiny)
library(rJava)
library(readxl)
#library(readr) <- fait partie de tidyverse
library(knitr)
#library(ggplot2) <- fait partie de tidyverse
library(ggrepel)
library(patchwork)
library(plotly)
#library(dplyr) <- fait partie de tidyverse
library(DT)
library(reactable)
library(scales)
library(rlang)
library(rvest)

library(r4geobs)
library(vroom)
