library(tidyverse)
library(magrittr)
library(sf)
library(data.table)
library(COGugaison)
library(stringr)
library(scales)
library(ggplot2)

#####################
## shapefile communes

if (dir.exists("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18")) {
  # importer shape des communes France métro
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
  
} else {
  url_comm <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z"
  download.file(url_comm, destfile = "/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  system("7z x -o/tmp /tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  # importer shape des communes France métro
  
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
}


library(COGugaison)
# communes avec maillages supra-communaux correspondants
comm_supra <- merge(comm, table_supracom_2016, by.x = "CODGEO", by.y = "CODGEO", all.x = TRUE)
comm.s <- comm %>% st_simplify( preserveTopology = FALSE, dTolerance = 10)
CV <- comm_supra %>% dplyr::select(CV) %>% group_by(CV) %>% summarize()
CV.s <- CV %>% st_simplify( preserveTopology = FALSE, dTolerance = 1000)
DEP <- comm_supra %>% dplyr::select(DEP) %>% group_by(DEP) %>% summarize()
DEP.s <- DEP %>% st_simplify( preserveTopology = FALSE, dTolerance = 1000)

############################@
#### DATA AGRESTE

######## population 2011

url_data <- "https://www.insee.fr/fr/statistiques/fichier/2044743/base-cc-evol-struct-pop-2010.zip"
download.file(url_data, destfile = "/tmp/base-cc-evol-struct-pop-2010.zip")
system("7z x -o/tmp /tmp/base-cc-evol-struct-pop-2010.zip")

COMM_POP_2010 <- read_xls("/tmp/base-cc-evol-struct-pop-2010.xls", sheet = "COM_2010", skip = 5)  %>%
  select(CODGEO,P10_POP)

#vérification du COG
COG_akinator(vecteur_codgeo=COMM_POP_2010[,1],donnees_insee=T)
# conversion des données communales en géographie 2011
COMM_POP_2010_COG2011 <-  changement_COG_varNum(table_entree=COMM_POP_2010,annees=c(2012:2011),agregation=T,libgeo=T,donnees_insee=T)
# correspondance communes 2011 cantons 2016
COMM_COG2011_CV2016 <- changement_COG_typo(table_entree = table_supracom_2016 %>% select(CODGEO, CV), annees = c(2016:2011),methode_fusion = "methode_difference", typos = "CV")
# aggrégation en maille cantons
CORRESP_CV2011_CV2016 <- 
  COMM_POP_2010_COG2011 %>% 
  left_join(table_supracom_2011 %>% select(CODGEO, CV) %>% rename(CV_2011 = CV), by = c("CODGEO" = "CODGEO")) %>%
  left_join(COMM_COG2011_CV2016 %>% select(CODGEO, CV) %>% rename(CV_2016 = CV), by = c("CODGEO" = "CODGEO")) 

PASSAGE_CV2011_CV2016 <- CORRESP_CV2011_CV2016 %>%
  group_by(CV_2011, CV_2016) %>%
  summarise(POP = sum(P10_POP)) %>%
  left_join(CORRESP_CV2011_CV2016 %>% 
              group_by(CV_2011) %>%
              summarise(POP = sum(P10_POP)),
            by = "CV_2011") %>%
  mutate(ratio = POP.x / POP.y)
  
CV2016_data_pop_elevage <-
  PASSAGE_CV2011_CV2016 %>% select(CV_2011, CV_2016, ratio) %>%
  left_join(CV_data_pop_elevage, by =c("CV_2011"= "CV")) %>%
  mutate_if(is.numeric, funs(.*ratio)) %>%
  select(-ratio) %>%
  group_by(CV_2016) %>%
  summarise_if(is.numeric, funs(sum))

######################
#### DATA SURFACE EAU

#https://diffusion.shom.fr/multiproduct/customer/units/
# http://www.data.gouv.fr/fr/datasets/bd-topo-r-hydrographie/

# téléchargement bd topo hydrographie
url_bdtopohydro <- "https://wxs-telechargement.ign.fr/cty7e9rigf3x9e9hxpgx073e/telechargement/inspire/BDTOPO-FRANCE-HYDROGRAPHIE-PACK_04-2017$BDTOPO_2-2_HYDROGRAPHIE_SHP_LAMB93_FRAN_2017-03-31/file/BDTOPO_2-2_HYDROGRAPHIE_SHP_LAMB93_FRAN_2017-03-31.7z"
download.file(url_bdtopohydro, destfile = "/tmp/BDTOPO_2-2_HYDROGRAPHIE_SHP_LAMB93_FRAN_2017-03-31.7z")
system("7z x -o/tmp /tmp/BDTOPO_2-2_HYDROGRAPHIE_SHP_LAMB93_FRAN_2017-03-31.7z")
# importer shape 
BDTOPO_surfaceeau <- st_read( dsn = "/tmp/BDTOPO_2-2_HYDROGRAPHIE_SHP_LAMB93_FRAN_2017-03-31/BDTOPO/1_DONNEES_LIVRAISON_2017-04-00266/BDT_2-2_SHP_LAMB93_FRAN_ED171",  "SURFACE_EAU") %>%   st_set_crs(NA)  %>% st_set_crs( 2154)


# téléchargement trait de cote
url_TCH <- "http://services.data.shom.fr/public/download/prepackageGroup/TCH-PACK_DL/prepackage/TCH_FRA_V2/file/TCH_FRA_V2.7z"
download.file(url_TCH, destfile = "/tmp/TCH_FRA_V2.7z")
system("7z x -o/tmp /tmp/TCH_FRA_V2.7z")
# importer shape
TCH_fra <- st_read( dsn = "/tmp/TCH_FRA_V2/Shapefile",  "TCH")  %>%   st_set_crs(NA)  %>% st_set_crs( 2154)
TCH_fra_zt <- st_buffer(TCH_fra, dist = 50)


# couche unique 
BDTOPO_surfaceeau <- BDTOPO_surfaceeau  %>%  select(NATURE) %>% mutate(NATURE = "EAU")
TCH_fra_zt <- TCH_fra_zt  %>% select(Source) %>% rename(NATURE = Source) %>% mutate(NATURE = "EAU")
ZONE_EAU <- rbind(BDTOPO_surfaceeau, TCH_fra_zt) %>% group_by(NATURE) %>% summarise(nb = n())
# finalement BD TOPO HYDRO non utilisée

COMM_ZONE_EAU <-
  comm %>% 
  mutate(area = as.numeric(st_area(geometry))) %>%
  left_join(comm %>% 
              st_intersection(TCH_fra_zt) %>%
              mutate(area = as.numeric(st_area(geometry))) %>%
              group_by(CODGEO) %>%
              summarize(area_ZONE_EAU = sum(area)) %>%
              as.data.frame() %>%
              select(CODGEO,area_ZONE_EAU ), by = "CODGEO") %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(pct_area_ZONE_EAU = area_ZONE_EAU / area)


## OSO occupationd du sol


http://www.cesbio.ups-tlse.fr/multitemp/?p=10104

# fonction pour calculer stats par commune
stats_comm <- function(id_dep){
  # url_dep <- paste0("http://osr-cesbio.ups-tlse.fr/echangeswww/TheiaOSO/vecteurs_2016/departement_",id_dep,".zip")
  # download.file(url_dep, destfile = paste0("/tmp/departement_",id_dep,".zip"))
  # system(paste0("7z x -o/tmp /tmp/departement_",id_dep,".zip"))
  # # lecture et nettoyage du shp
  OSO_dep <- st_read(paste0("/tmp/departement_",id_dep,".shp") , stringsAsFactors = F) %>% st_transform(crs = 2154)
  OSO_dep <- OSO_dep %>% st_buffer(dist = 0)
  #intersection avec communes
  intersect_dep <- st_intersection(st_buffer(OSO_dep,dist = 0), st_buffer(comm,dist = 0) )
  # stats par commune
  COMM_OSO_dep <- intersect_dep %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    as.data.frame() %>%
    dplyr::select(CODGEO, Classe, area) %>%
    group_by(CODGEO,Classe) %>% summarise(area = sum(area))
  # 
  #suppression des fichiers temporaires
  file.remove(paste0("/tmp/departement_",id_dep,".dbf"))
  file.remove(paste0("/tmp/departement_",id_dep,".prj"))
  file.remove(paste0("/tmp/departement_",id_dep,".shp"))
  file.remove(paste0("/tmp/departement_",id_dep,".shx"))
  file.remove(paste0("/tmp/departement_",id_dep,".zip"))
}

id_dep <- c(seq(from = 10, to =95, by = 1),'2A','2B','01','02','03','04','05','06','07','08','09')
COMM_OSO <- purrr::map(id_dep, stats_comm) %>%
  reduce(rbind)

COMM_OSO <- fread( file = "./clc/COMM_OSO.csv", verbose = F, colClasses=c(CODGEO="character", Classe="character"))
                   
# culture ete:11
# culture hiver:12
# foret feuillus:31
# foret coniferes:32
# pelouses:34
# landes ligneuses:36
# urbain dense:41
# urbain diffus:42
# zones ind et com:43
# surfaces routes:44
# surfaces minerales:45
# plages et dunes:46
# eau:51
# glaciers ou neige: 53
# prairies:211
# vergers:221
# vignes:222


### visu ggplot de chacune des catégories

CV2016_OS_long <-
  COMM_OSO %>% # ungroup() %>%
  left_join(table_supracom_2016 %>% select(CODGEO, CV), by ="CODGEO") %>% 
  group_by(CV, Classe) %>%
  summarize(area = sum(area)) %>%
  mutate(pct_area = area / sum(area))
  
ggplot() +
  geom_sf(data = merge(CV.s, CV2016_OS_long, by ="CV") %>% filter(Classe %in% '36') %>%
            mutate(pct_area.cl = cut(pct_area, breaks = quantile(pct_area, probs=seq(0,1, length  = 10), na.rm=TRUE))),
          aes( fill=pct_area.cl), color = NA) +
  scale_fill_manual( name = "", values = colorRampPalette(rev(brewer.pal(10, "BrBG")) )(10)) +
  geom_sf(data =DEP, color = "grey65", fill = NA ,size = 0.2) +
  thm +            
  coord_sf(crs = st_crs(2154)) 

# indicateurs synthétiques eau et forets

CV2016_OS_appli <-
  COMM_OSO %>% 
  left_join(table_supracom_2016 %>% select(CODGEO, CV), by ="CODGEO") %>% 
  mutate(Classe.s = ifelse(Classe %in% c('31','32'), "OS_forets", ifelse(Classe %in% c('51'), "OS_eau", NA))) %>%
  group_by(CV, Classe.s) %>%
  summarize(area = sum(area)) %>%
  mutate(pct_area = area / sum(area)) %>%
  filter(!is.na(Classe.s)) %>%
  select(-area) %>%
  spread(Classe.s, pct_area) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  left_join( 
  COMM_ZONE_EAU %>% as.data.frame() %>%
  left_join(table_supracom_2016 %>% select(CODGEO, CV), by ="CODGEO") %>% # ungroup() %>%
    group_by(CV) %>%
    summarize(area = sum(area), area_ZONE_EAU=sum(area_ZONE_EAU)) %>%
    mutate(pct_area_ZONE_EAU = area_ZONE_EAU / area) %>%
    select(CV, pct_area_ZONE_EAU), by = "CV") %>%
  mutate(pct_EAU = pct_area_ZONE_EAU + OS_eau) %>%
  rename(pct_FORETS = OS_forets)
  
  
    
#######################
##### DATA LOGEMENTS

library(readxl)

url_data <- "https://www.insee.fr/fr/statistiques/fichier/2863604/BTX_TD_LOG1_2014.zip"
download.file(url_data, destfile = "/tmp/BTX_TD_LOG1_2014.zip")
system("7z x -o/tmp /tmp/BTX_TD_LOG1_2014.zip")

CV_LOG_RP2014 <- read_xls("/tmp/BTX_TD_LOG1_2014.xls", sheet = "COM", skip =10) %>%
  left_join(table_supracom_2016 %>% select(CODGEO, CV), by ="CODGEO") %>%
  group_by(CV) %>%
  summarise_if(is.numeric, funs (sum)) %>%
  mutate(LOGS_TOT =  select(., contains("ACHL")) %>% rowSums()) %>%
  mutate(LOGS_ACHL16111 =  select(., contains("ACHL16111")) %>% rowSums()) %>%
  select(CV, LOGS_TOT, LOGS_ACHL16111) %>%
  mutate(pct_LOGS_LOGS_ACHL16111 = LOGS_ACHL16111/LOGS_TOT)



  
###################
# DATA declivité

# cf script carto_alti_decli
CV_alti.df

###################
# DATA pluie


# valeur moyenne par commune à partir des rasters

# 1. nb jours de pluie
# conversion en spatialpixeldf et rasterlayer
krigeage_NORRR1MM_TOT_m_1000m_rst <- raster(SpatialPixelsDataFrame(points=krigeage_NORRR1MM_TOT_m_1000m_df[c("x", "y")],     data=krigeage_NORRR1MM_TOT_m_1000m_df[c("var1.pred")],proj4string = CRS("+init=epsg:2154")), layer=1, values=TRUE)
# calcul de la valeur moyenne du raster sur l'emprise de chaque commune
CV_moy_val <- raster::extract(krigeage_NORRR1MM_TOT_m_1000m_rst, as(CV, 'Spatial'), method='simple', small=FALSE, fun=mean, na.rm=TRUE) %>%
  as.data.frame() %>% set_colnames("val_NORRR1MM_TOT_m")
# récupération des infos attributaires
CV_indics_an_NORRR1MM_TOT_m <- as(CV, 'Spatial') %>% as.data.frame() %>% 
  rownames_to_column(var = "id_n") %>%
  left_join(CV_moy_val %>%
              mutate(id_n = rownames(CV_moy_val)),
            by = 'id_n')


# 2. quantité pluie 
# conversion en spatialpixeldf et rasterlayer
krigeage_NORRR_TOT_1000m_rst <- raster(SpatialPixelsDataFrame(points=krigeage_NORRR_TOT_1000m_df[c("x", "y")],     data=krigeage_NORRR_TOT_1000m_df[c("var1.pred")],proj4string = CRS("+init=epsg:2154")),
                                       layer=1, values=TRUE)
# calcul de la valeur moyenne du raster sur l'emprise de chaque canton
CV_moy_val <- raster::extract(krigeage_NORRR_TOT_1000m_rst, as(CV, 'Spatial'), method='simple', small=FALSE, fun=mean, na.rm=TRUE) %>%
  as.data.frame() %>% set_colnames("val_NORRR_TOT")
# récupération des infos attributaires
CV_indics_an_NORRR_TOT <- as(CV, 'Spatial') %>% as.data.frame() %>%
  rownames_to_column(var = "id_n") %>%
  left_join(CV_moy_val %>%
              mutate(id_n = rownames(CV_moy_val)),
            by = 'id_n')

CV_indics_an_TOT_m <- CV_indics_an_NORRR1MM_TOT_m %>% dplyr::select(-id_n) %>% left_join(CV_indics_an_NORRR_TOT %>% dplyr::select(CV,val_NORRR_TOT), by ="CV" )

#export data pour rmd
fwrite(CV_indics_an_TOT_m, file = "./data/CV_indics_an_TOT_m.csv", verbose = F)

#####################
### grille de densité
library(janitor)
library(stringi)

url_data <- "https://www.insee.fr/fr/statistiques/fichier/2114627/grille_densite_2016.zip"
download.file(url_data, destfile = "/tmp/grille_densite_2016.zip")
system("7z x -o/tmp /tmp/grille_densite_2016.zip")

# méthode d'aggrégation selon insee : https://www.insee.fr/fr/information/2114627

GRILLE_DENSITE_COMM <- read_xls("/tmp/grille_densite_2016.xls", sheet = "grille_densite_2016", col_types ="text")  %>%
  clean_names() %>%
 mutate(depcom = stri_sub( paste0("0",depcom),-5,-1 )) %>%
  as.data.frame() %>%
  mutate(pmun_2010 = as.numeric(pmun_2010)) %>%
  mutate_at(.vars = 6:9,
            .funs = funs(as.numeric(gsub("%", "", .))) ) %>%
  left_join(table_supracom_2016 %>% select(CODGEO, CV), by =c("depcom" = "CODGEO")) %>%
  mutate_at(.vars = 6:9,
            .funs = funs((./100)*pmun_2010) ) %>%
  group_by(CV) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  mutate_at(.vars = 3:6, funs(./pmun_2010 )) %>%
  mutate(typo_CV_densite = ifelse(part_pop_dense_1>0.5, "TD",NA)) %>%
  mutate(typo_CV_densite = ifelse(part_pop_dense_1+part_pop_intermediaire_2>0.5 & is.na(typo_CV_densite), "D",typo_CV_densite)) %>%
  mutate(typo_CV_densite = ifelse(part_pop_tres_peu_dense_4>0.5 & is.na(typo_CV_densite), "TPD",typo_CV_densite)) %>%
  mutate(typo_CV_densite = ifelse(part_pop_peu_dense_3+part_pop_tres_peu_dense_4>0.5 & is.na(typo_CV_densite), "PD",typo_CV_densite)) 

GRILLE_DENSITE_COMM %>% group_by(typo_CV_densite) %>% summarise(nb =n())

#############################
# finalisation du fichier et scores

CV_data_appli <-
  GRILLE_DENSITE_COMM %>% select(CV, typo_CV_densite) %>%
  filter(!substr(CV,1,2) %in% '97') %>%
  left_join(CV_LOG_RP2014, by ="CV") %>%
  left_join(libelles_supracom_2016 %>%
              mutate(LIBGEO = iconv(LIBGEO,  "ISO_8859-2", "UTF-8")) %>%
              mutate(LIBGEO =gsub("\\s*\\([^\\)]+\\)","",as.character(LIBGEO))) %>%
              filter(NIVGEO %in% 'CV'), by =c("CV" ="CODGEO")) %>%
  left_join(CV2016_data_pop_elevage %>% 
              mutate(grossesbetes =total_bovins + total_equides + total_ovins*0.3 + total_caprins*0.3) %>%
              select(CV_2016, grossesbetes), by =c("CV" ="CV_2016")) %>%
  left_join(CV_alti.df, by =c("CV" ="CV")) %>%
  left_join(CV_indics_an_TOT_m, by =c("CV" ="CV")) %>%
  left_join(CV2016_OS_appli %>% select(CV, pct_EAU, pct_FORETS), by ="CV")

CV_data_appli.geo <- 
  CV.s %>%
  left_join(CV %>% mutate(superficie_km2 = as.numeric(st_area(.)/1000000)) %>% as.data.frame() %>% select(CV,superficie_km2 ), by ="CV") %>%
  left_join(CV_data_appli, by ="CV") %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(densite_grossesbetes = grossesbetes/superficie_km2 ) %>%
  mutate(score_vieuxlogements = ntile(pct_LOGS_LOGS_ACHL16111,5)-1) %>%
  mutate(score_grossesbetes = ntile(densite_grossesbetes,5)-1) %>%
  mutate(score_pluie = ntile(val_NORRR1MM_TOT_m,5)-1) %>%
  mutate(score_declivite= ntile(ecart_type,5)-1) %>%
  mutate(score_OS_eau = ntile(pct_EAU,5)-1) %>%
  mutate(score_OS_forets= ntile(pct_FORETS,5)-1) 

#  cantons en polygones et données séparément
st_write(CV_data_appli.geo %>% select(CV), dsn = "./appli/data/CV_appli.geo.shp", layer = "CV_appli.geo.shp", driver = "ESRI Shapefile")
fwrite(CV_data_appli.geo %>% as.data.frame() %>% select(-geometry), file = "./appli/data/CV_data_appli.csv", verbose = F)

# centroides
CV_data.geo_ctr <- CV_data_appli.geo %>% as.data.frame() %>%
  cbind(  do.call(rbind, st_geometry( CV_data_appli.geo %>% st_centroid() %>% st_transform(4326))) %>% set_colnames(c("x","y")) ) %>%
  mutate(LIBGEO_conv = conv_accents(LIBGEO))


fwrite(CV_data.geo_ctr  %>% filter(!typo_CV_densite %in% 'TD') %>% select(-geometry) , file = "./appli/data/CV_data.geo_ctr.csv", verbose = F)

st_write(DEP.s %>% select(DEP), dsn = "./appli/data/DEP_appli.geo.shp", layer = "DEP_appli.geo.shp", driver = "ESRI Shapefile")


# + OCCUPATION DU SOL


### indicateurs totaux
FR_data_appli.geo <- 
  CV_data_appli.geo %>% as.data.frame() %>% select(-geometry) %>%
  mutate(PAYS = "FRA") %>%
  group_by(PAYS) %>%
  summarise_if(is.numeric,funs(sum))

