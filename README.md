# Taux_etagement 💧

Ce travail a pour objectif de créer un outil de visualisation des tronçons de cours d’eau classés liste 2 et des 74 masses d’eau classées comme naturelles par le PLAGEPOMI pour la région Normandie. Ces entités doivent faire l’objet de rapports visant à évaluer l’indicateur « taux d’étagement ». Cet outil permet également de générer des rapports de façon automatisée. 

## 1.	Import et formatage des données ⚙

### a) Accès aux données

Voir (Source_de_donnes.xlsx)

Les différentes données sont disponibles sur le réseau commun en suivant ce lien :"\\\\ad.intra\\dfs\\COMMUNS\\REGIONS\\nor\\DR\OFB\\Obstacles_écoulement\\Augustin_Mise_en_qualité_ROE\\Taux_etagement\\ap\\data_prepared

### b) Formatage des données

Elles sont ensuites transformées en suivant les étapes suivantes :

*	Etapes 1 à 8 : formater les données issues de Geobs pour les rendre exploitables

*	Etapes 9 à 11 : construction des tableaux comprenant les métriques des cours d’eau, tronçons L2 ou masses d’eau PLAGEPOMI calculées à partir des données issues de PHRYMO ou du MNT1m

-	9 et 10 : les données des métriques de cours d’eau ont été calculées à partir des données issues de PHRYMO. 

-	11 : les données des métriques de cours d’eau ont été calculées à partir des données MNT1m.

*Le travail aillant été initialement effectué sur les cours d’eau, il comprend de ce fait une partie d’harmonisation du nom des cours d’eau qui n’est pas nécessaire pour l’utilisation de l’application de visualisation et la génération de rapport. Cependant, il a été conservé au cas où il aurait une utilité future. 

## 2. Utilisation de l'outil 💻️

Voir (Utilisation de l'outil)

## 3.	Les axes d’amélioration ↗️

-	Créer un référentiel avec des linéaires qui correspondent aux linéaires des tronçons de cours d’eau classés liste 2 comprenant les bras de ces tronçons. Les linéaires existants n’existent qu’à titre indicatif mais ne reflètent pas la réalité de l’hydromorphisme des cours d’eau ce qui les rend inexploitables. 
 
-	Affiner ce linéaire pour qu’il comprenne un cours principal par tronçon ainsi que le reste de ses bras secondaires. Cela pour régler le problème des ouvrages identifiés comme principaux car ils ne sont reliés à aucun autre ouvrage mais qui sont sur un bras secondaire et donc de ce fait à considérer comme ouvrage secondaire.

-	Créer un linéaire des 74 masses d’eau classées comme naturelles dans le PLAGEPOMI car il n’existe aucun référentiel pour ces dernières dont le tracer est différent de celui des masse d’eau de la DCE.  

-	Cela pourrait également solutionner la difficulter qu’est d’obtenir les métriques : point amont, point aval et longueur du cours d’eau. Car aucun moyen n’a été identifié pour les obtenir de façon précises. En effet, l’obtention des ce métriques a été tester de 2 manières : l’aggrégation tronçons usra utilisés dans le PHRYMO et le calcul des MNT à un 1m du point aval et amont.

-	Les usra se sont révélés correctes dans la majorité des cas mais un nombre non négligeable ne correspond absolument pas aux tracés des tronçons de cours d’eau.

-	Le calcul des MNT a été utilisé pour les masses d’eau et il s’est révélé être également imprécis car les linéaires utilisés ne correspondent pas aux linéaires des masses d’eau définies par le PLAGEPOMI qui pour certaines sont uniques à ce référentiel.
 L’une des solutions envisagées serait de se référer aux linéaires créés spécialement pour les tronçons et les masses d’eau afin de se baser sur les MNT. Une autre solution serait de créer un tableau manuellement avec les informations requises. 
 
 ## 4.	R
 
 Il a été réalisé sous la version suivante de R : 4.4.2 et las packages suivants :

- gtable_0.3.6
- renv_1.1.4
- dplyr_1.1.4
- compiler_4.4.2
- zip_2.3.2
- tidyselect_1.2.1
- Rcpp_1.0.14 
- stringr_1.5.1
- leaflet_2.2.2 
- scales_1.3.0
- yaml_2.3.10 
- fastmap_1.2.0
- ggplot2_3.5.2  
- R6_2.6.1
- generics_0.1.3
- openxlsx_4.2.8
- classInt_0.4-11
- sf_1.0-20
- knitr_1.50
- htmlwidgets_1.6.4
- tibble_3.2.1
- units_0.8-7
- munsell_0.5.1
- DBI_1.2.3
- pillar_1.10.2
- rlang_1.1.6
- stringi_1.8.7
- xfun_0.52
- cli_3.6.5
- magrittr_2.0.3
- class_7.3-23
- crosstalk_1.2.1
- digest_0.6.37
- grid_4.4.2
- rstudioapi_0.17.1
- lifecycle_1.0.4
- vctrs_0.6.5
- KernSmooth_2.23-26
- proxy_0.4-27
- evaluate_1.0.3
- glue_1.8.0
- e1071_1.7-16
- colorspace_2.1-1
- rmarkdown_2.29
- tools_4.4.2
- pkgconfig_2.0.3
- htmltools_0.5.8.1

