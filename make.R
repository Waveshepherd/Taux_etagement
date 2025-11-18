##%######################################################%##
#                                                          #
####               Lancement de tous les                ####
####           scripts pour réaliser l'étude            ####
#                                                          #
##%######################################################%##

##----------------------------------------------------------------------------##
#### 1. Chargement des packages ####
##----------------------------------------------------------------------------##

# A faire lors de la première utilisation du script

# if (!require("renv")) install.packages("renv")
# renv::restore()

source("scripts/00_chargement_packages.R")

##----------------------------------------------------------------------------##
#### 2. Chargement et transformation des données ####
##----------------------------------------------------------------------------##

#A exécuter pour créer les linéaires de masses d'eau obtenu par les MNT1m

# source("scripts/00_script_ME.R")

source("scripts/01_import_donnees.R")

##----------------------------------------------------------------------------##
#### 3. Génération de rapport(s) ####
##----------------------------------------------------------------------------##

## 3.a. Fonction pour générer les rapport sur les tronçons liste 2 ##

source("scripts/gen_rapport_L2.R")

# Fonction pour générer tous les rapports 
generate_report_L2(Info_L2, "./output_tl2", generate_all_reports = TRUE)

# Fonction pour générer un rapport sur un tronçon liste 2 précis
generate_report_L2(Info_L2, "./output_tl2", id_t = '64')

## 3.b. Fonction pour générer les rapport sur les masses d'eau ##

source("scripts/gen_rapport_ME.R")

# Fonction pour générer tous les rapports 
generate_report_ME(ME_L2_lin, "./output_MEl2", generate_all_reports = TRUE)

# Fonction pour générer un rapport sur une masse d'eau précise
generate_report_ME(ME_L2_lin, "./output_MEl2", cdme = 'FRHR261')

##----------------------------------------------------------------------------##
#### 4. Lancement des applications de visualisation ####
##----------------------------------------------------------------------------##

## 4.a. Lancement de l'application pour les tronçons L2 ##

shiny::runApp("ap/apL2.R")

## 4.b. Lancement de l'application pour les masse d'eau ##

shiny::runApp("ap/apME.R")
