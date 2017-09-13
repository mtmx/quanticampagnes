library(data.table)
library(tidyverse)
library(magrittr)
require(lubridate)
library(tmaptools)
library(sf)
library(rgeos)
# doc sf https://github.com/riatelab/intro_sf
library(devtools)
#install_github("riatelab/cartography")
#install_github("tidyverse/ggplot2")
library(devtools)
library(cartography)
library(raster)
library(SpatialPosition)
library(stringr)
library(viridis)

## stockage dans dossier temporaire
#tmp <- tempdir()
url_data <- "https://www.data.gouv.fr/s/resources/indices-mensuels-de-precipitations-et-nombre-de-jours-de-precipitations-issus-du-modele-aladin-pour-la-periode-de-reference/20150930-201506/Precip_REF_mensuel.txt"
download.file(url_data, destfile = "/tmp/Precip_REF_mensuel.txt")

Precip_REF_mois <- fread( "/tmp/Precip_REF_mensuel.txt",
                          sep = ";",
                          dec= ".",
                          stringsAsFactors = FALSE,
                          header= F) %>% 
  dplyr::select(-V15) %>%
  set_colnames(c("Point","Latitude","Longitude","Contexte","Integration","Mois","NORPAV","NORPINT","NORRR","NORPFL90","NORRR1MM","NORPXCWD","NORPN20MM","NORPXCDD"))

#temperature par mois
#https://www.data.gouv.fr/s/resources/indices-mensuels-de-temperature-et-nombre-de-jours-de-temperature-issus-du-modele-aladin-pour-la-periode-de-reference/20150930-200335/Tempe_REF_mensuel.txt

#export data pour rmd
fwrite(Precip_REF_mois, file = "./data/Precip_REF_mois.csv", verbose = F)

url_data <- "https://www.data.gouv.fr/s/resources/indices-mensuels-de-precipitations-et-nombre-de-jours-de-precipitations-issus-du-modele-aladin-pour-la-periode-de-reference/20150930-201506/Precip_REF_mensuel.txt"
download.file(url_data, destfile = "/tmp/Precip_REF_mensuel.txt")


Temp_REF_mois <- fread( "https://www.data.gouv.fr/s/resources/indices-mensuels-de-temperature-et-nombre-de-jours-de-temperature-issus-du-modele-aladin-pour-la-periode-de-reference/20150930-200335/Tempe_REF_mensuel.txt",
                        sep = ";",
                        dec= ".",
                        stringsAsFactors = FALSE,
                        header= F
                        #colClasses=list(character=c("REGION2016","REGION","IRIS","IRAN","ILT","ILETUD","HLML","DNAI","DEPT","ARM","CANTVILLE","CAN_ZZZZZZZZZ")) 
) %>% 
  dplyr::select(-V15) %>%
  set_colnames(c("Point","Latitude","Longitude","Contexte","Integration","Mois","NORPAV","NORPINT","NORRR","NORPFL90","NORRR1MM","NORPXCWD","NORPN20MM","NORPXCDD"))


#description 
# Date d'extraction : 25/09/2015 - 08h54 loc.
#-----------------------------------------------------------------------------
# Producteur : CNRM
# Experience : CNRM2014
# Modele     : ALADIN
# Scenario :
#     REF_RCP : référence
#-----------------------------------------------------------------------------
# Horizons :
#     REF : Référence
#-----------------------------------------------------------------------------
# Type d'indice : saisonnier
# (1 - hiver, 2 - printemps, 3 - été, 4 - automne)
#-----------------------------------------------------------------------------
# Indices : 
#     NORPAV : Intégration de Précipitations journalières moyennes (mm/jour)
#     NORPINT : Intégration de Précipitation moyenne les jours pluvieux (mm/jour)
#     NORRR : Intégration de Cumul de précipitation (mm)
#     NORPFL90 : Intégration de Fraction des précipitations journalières intenses (%)
#     NORRR1MM : Intégration de Nombre de jours de pluie (jour)
#     NORPXCWD : Intégration de Nombre maximum de jours pluvieux consécutifs (jour)
#     NORPN20MM : Intégration de Nombre de jours de fortes précipitations (jour)
#     NORPXCDD : Intégration de Période de sécheresse (jour)

Precip_REF_Mois.jourspluie <- 
  Precip_REF_mois %>%
  dplyr::select(Point, Mois,NORRR1MM) %>%
  mutate(Mois = paste0("NORRR1MM_",Mois)) %>%
  spread(Mois, NORRR1MM) %>%
  mutate(NORRR1MM_TOT = rowSums(.[grep("NORRR1MM_", names(.))], na.rm = TRUE) ) %>%
  mutate(NORRR1MM_2 = NORRR1MM_2 * (31/28.25),
            NORRR1MM_4 = NORRR1MM_4 * (31/30),
            NORRR1MM_6 = NORRR1MM_6 * (31/30),
            NORRR1MM_9 = NORRR1MM_9 * (31/30),
            NORRR1MM_11 = NORRR1MM_11 * (31/30))


Precip_REF_Mois.qtepluie <- 
  Precip_REF_mois %>%
  dplyr::select(Point, Mois,NORRR) %>%
  mutate(Mois = paste0("NORRR_",Mois)) %>%
  spread(Mois, NORRR) %>%
  mutate(NORRR_TOT = rowSums(.[grep("NORRR_", names(.))], na.rm = TRUE) ) %>%
  mutate(NORRR_2 = NORRR_2 * (31/28.25),
         NORRR_4 = NORRR_4 * (31/30),
         NORRR_6 = NORRR_6 * (31/30),
         NORRR_9 = NORRR_9 * (31/30),
         NORRR_11 = NORRR_11 * (31/30))

# points stations
Precip.geo <- Precip_REF_mois %>% distinct(Point,Latitude,Longitude) %>%
  st_as_sf( coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(Precip.geo, crs = 2154) 

# vérifier geometrie et projection
plot(st_geometry(Precip.geo))
st_crs(x = Precip.geo)

# reférentiel GEOFLA janvier 2017 IGN
url_data <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z"
download.file(url_data, destfile = "/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
system("7z x -o/tmp /tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")

comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) 

# générer couche des cantons
library(COGugaison)
# communes avec maillages supra-communaux correspondants
comm_supra <- merge(comm, table_supracom, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)
# création des contours cantons
CV <- comm_supra %>% dplyr::select(CV) %>% group_by(CV) %>% summarize()
# contours cantours avec géometrie simplifiée
CV_s <- CV %>% st_simplify( preserveTopology = FALSE, dTolerance = 10)
# contours france avec zone tampon 20 km
FR <- comm %>% st_union()
FRzt20km <- FR %>% st_buffer( dist = 20000)
#contours départements simplifiés
DEP <- comm_supra %>% dplyr::select(DEP) %>% group_by(DEP) %>% summarize() 
DEP.s <- DEP %>% st_simplify( preserveTopology = FALSE, dTolerance = 10)

DEP_sp <- spTransform(as(DEP, 'Spatial') , CRS("+init=epsg:2154")) 

#########################################
###### krigeage

# grille 1000m sp
st_bbox(FR)

grd_1000m <- expand.grid(x=seq(from=79038.19, to=1262434.11, by=1000), y=seq(from=6026557.99, to=7130478.28, by=1000))
coordinates(grd_1000m) <- ~ x+y
proj4string(grd_1000m) <- CRS("+init=epsg:2154") 
FR_sp <- spTransform(as(FR, 'Spatial'), crs(grd_1000m))
grd_1000m <- grd_1000m[FR_sp ]
gridded(grd_1000m) <- TRUE

# data points
pts_meteo.Mois.jourspluie <- Precip.geo %>% merge(Precip_REF_Mois.jourspluie, by = "Point") %>% merge(Precip_REF_Mois.qtepluie, by = "Point") 
  
# conversion sf vers sp juste pour gstat  
pts_meteo.Mois.jourspluie.sp <- as(pts_meteo.Mois.jourspluie, 'Spatial')
pts_meteo.Mois.jourspluie.sp <- spTransform(pts_meteo.Mois.jourspluie.sp, crs(grd_1000m))
# variogram
# source méthodo krigeage : http://dumas.ccsd.cnrs.fr/dumas-00520260/document

library(gstat)
library(spdplyr)
vgm<-variogram(NORRR1MM_TOT~1,pts_meteo.Mois.jourspluie.sp,cutoff=60000)
plot(vgm,pch=3,type="o")
# ajustement modèle
vgm.fit<-fit.variogram(vgm,vgm(psill=100,model="Exp",range=60000))
# graphique du variogramme ajusté avec le modèle paramétré
plot(vgm,vgm.fit)

############

# estimation de valeur moyenne mensuelle sur l'année
pts_meteo.Mois.jourspluie.sp <- pts_meteo.Mois.jourspluie.sp %>%
  mutate(NORRR1MM_TOT_m = NORRR1MM_TOT /12)

# jours de pluie
krigeage_NORRR1MM_TOT_m_1000m_df <- krige(NORRR1MM_TOT_m~1,
                                        pts_meteo.Mois.jourspluie.sp,
                                        grd_1000m,
                                        vgm.fit,
                                        nmax = 50)  %>% as.data.frame()
# quantité de pluie
krigeage_NORRR_TOT_1000m_df <- krige(NORRR_TOT~1,
                                          pts_meteo.Mois.jourspluie.sp,
                                          grd_1000m,
                                          vgm.fit,
                                          nmax = 50)  %>% as.data.frame()


##########
# cartes ggplot

# parametrage du thème ggplot
thm <- 
  theme(legend.position="right",
        legend.text=element_text(size=8),
        legend.title=element_text(size=9),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),#element_text(size=9, color = "black"),
        axis.title.y=element_blank(), #element_text(size=9, color = "grey",face="italic"),
        axis.ticks=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major.y=element_line(colour = 'grey80', linetype = 'dotdash', size = 0.1),
        panel.grid.major.x=element_line(colour = 'grey80', linetype = 'dotdash', size = 0.1),#element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.key.width = unit(0.4, "cm"),
        strip.text.y = element_text(size = 8, colour = "black", angle = 0),
        strip.text.x = element_text(size = 8, colour = "black", angle = 0),
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=9,face="italic"),
        plot.caption=element_text(size=6,colour = "grey20")) 

#export data pour rmd
fwrite(krigeage_NORRR_TOT_1000m_df, file = "./data/krigeage_NORRR_TOT_1000m_df.csv", verbose = F)
fwrite(krigeage_NORRR1MM_TOT_m_1000m_df, file = "./data/krigeage_NORRR1MM_TOT_m_1000m_df.csv", verbose = F)


# carte nb jours pluie
ggplot() +
  geom_tile(data = krigeage_NORRR1MM_TOT_m_1000m_df, aes(x=x, y=y, fill=var1.pred), color = NA) +
  scale_fill_viridis(option="magma", direction = -1, name = "", limits = c(4, 15)) +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Nombre de jours de pluie par mois (en moyenne)",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  theme(legend.position = c(0.9, 0.5)) +
  coord_sf(crs = st_crs(2154))

# carte quantité pluie
ggplot() +
  geom_tile(data = krigeage_NORRR_TOT_1000m_df, aes(x=x, y=y, fill=var1.pred), color = NA) +
  scale_fill_viridis(option="viridis", direction = -1, name = "", limits = c(500, 2500)) +
  #geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Quantité de pluie par an",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  coord_sf(crs = st_crs(2154))


#######################
# données raster par mois

# fonction : sortie krigeage par mois en dataframe
sortie_data_krigeage <- function(nn,mm,var)
{
  # estimation des valeurs
  krigeage <- krige(get(paste0(var,"_",nn))~1,pts_meteo.Mois.jourspluie.sp,grd_1000m,vgm.fit,nmax = 50)
  
  # conversion en df
  krigeage_df <- krigeage %>%
    as.data.frame() %>%
    mutate(MOIS_n = as.numeric(nn),
           MOIS_t = mm,
           indic = var,
           var1.pred.q10 = ntile(var1.pred,10),
           var1.pred.q5 = ntile(var1.pred,20))
  
  #sortie df en dur
  assign( paste0("krigeage_",var,"_1000m_",nn,"_df"), krigeage_df, envir = .GlobalEnv )
  
}

n <- c('1','2','3','4','5','6','7','8','9','10','11','12')
m <- c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre')
#n <- c('1')
#m <- c('Janvier')
#v <- c("NORRR1MM","NORRR")
mapply(sortie_data_krigeage,nn =n, mm =m, var = "NORRR1MM") 
mapply(sortie_data_krigeage,nn =n, mm =m, var = "NORRR") 

# aggrégation dans un seul dataframe
krigeage_1000m_df_mois <-
  mget(ls(pattern="krigeage_NORRR1MM_1000m_\\d+_df|krigeage_NORRR_1000m_\\d+_df")) %>%
  bind_rows()
#export data pour rmd
fwrite(krigeage_1000m_df_mois, file = "./data/krigeage_1000m_df_mois.csv", verbose = F)
#krigeage_1000m_df_mois <- fread(file = "./data/krigeage_1000m_df_mois.csv", verbose = F)

# suppression des df temporaires
rm(list = ls(pattern="krigeage_NORRR1MM_1000m_\\d+_df|krigeage_NORRR_1000m_\\d+_df"))

###############################@
####### animation GIF
library(gganimate)

krigeage_1000m_df_mois_test <- krigeage_1000m_df_mois %>%  slice(1:5000)


krigeage_1000m_df_mois_edit <- krigeage_1000m_df_mois_test %>%
  mutate(id_x_y = paste0(x,"_",y)) %>%
  filter(indic %in% 'NORRR') %>%
  arrange(id_x_y, MOIS_n) %>%
  select(var1.pred,MOIS_n,id_x_y, MOIS_t) %>%
  rename(x=var1.pred,time=MOIS_n,id=id_x_y) %>%
  mutate(ease="linear")

krigeage_1000m_df_mois_tween <- tween_elements(krigeage_1000m_df_mois_edit,
                                  "MOIS_n", "id", "ease", nframes = 300) %>%
  mutate( id_x_y = .group) %>%
  left_join(gapminder, by=c("country","year","continent")) %>%
  rename(population = pop.x)

p2 <- ggplot(gapminder_tween,
             aes(x=x, y=y, frame = .frame)) +
  geom_point(aes(size=population, color=continent),alpha=0.8) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_x_log10(labels=comma)

# carte nb jours pluie
p <-
ggplot() +
  geom_tile(data = krigeage_1000m_df_mois %>% filter(indic %in% 'NORRR') %>% filter(MOIS_n < 3), aes(x=x, y=y, fill=var1.pred, frame = MOIS_n), color = NA) +
  scale_fill_viridis(option="magma", direction = -1, name = "", limits = c(1, 25)) +
 # geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Nombre de jours de pluie par mois (en moyenne): ",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  theme(legend.position = c(0.9, 0.5)) +
  coord_sf(crs = st_crs(2154))

gganimate(p)

########################
# mois ou il pleut le plus

krigeage_moismax_df <-
  krigeage_1000m_df_mois %>%
 # filter(indic %in% 'NORRR') %>%
  mutate(N = paste0(x,"_",y)) %>%
  # dplyr::select(-x,-y, -var1.var) %>%
  group_by(N, indic) %>%
  filter(var1.pred == max(var1.pred))  %>%
  rename(MOIS_n_max = MOIS_n,MOIS_t_max = MOIS_t,var1.pred_max = var1.pred) %>%
  mutate(MOIS_t_max = factor(MOIS_t_max, levels = c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre')) )


# code couleur mois
mois_cols <- data.frame( classe=c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre'), 
                         col_hex = c( "#0A2164","#1369A2", "#0099A9","#009780","#67B784","#CBDF7C",
                                      "#FFE200","#DB9815", "#E57B25","#F0522D","#912E0F","#33004B"))

# mois max jours de pluie
ggplot() +
  geom_tile(data = krigeage_moismax_df %>% filter(indic %in% 'NORRR1MM'), aes(x=x, y=y, fill=MOIS_t_max), color = NA) +
  scale_fill_manual(values =  c( "#0A2164","#1369A2", "#0099A9","#009780","#67B784","#CBDF7C",
                                 "#FFE200", "#E57B25","#F0522D","#912E0F","#33004B"), name = "") +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Mois avec un maximum de jours de pluie",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  coord_sf(crs = st_crs(2154))

# mois max quantité de pluie
ggplot() +
  geom_tile(data = krigeage_NORRR_moismax_df %>% filter(indic %in% 'NORRR'), aes(x=x, y=y, fill=MOIS_t_max), color = NA) +
  scale_fill_manual(values =  c( "#0A2164","#1369A2","#009780","#67B784","#CBDF7C",
                                 "#FFE200", "#E57B25","#F0522D","#912E0F","#33004B"), name = "") +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Mois avec un maximum de quantité de pluie",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  coord_sf(crs = st_crs(2154))

########################
# valeur moyenne par commune à partir des rasters

# 1. nb jours de pluie
# conversion en spatialpixeldf et rasterlayer
krigeage_NORRR1MM_TOT_m_1000m_rst <- raster(SpatialPixelsDataFrame(points=krigeage_NORRR1MM_TOT_m_1000m_df[c("x", "y")],     data=krigeage_NORRR1MM_TOT_m_1000m_df[c("var1.pred")],proj4string = CRS("+init=epsg:2154")), layer=1, values=TRUE)
# calcul de la valeur moyenne du raster sur l'emprise de chaque commune
COMM_moy_val <- raster::extract(krigeage_NORRR1MM_TOT_m_1000m_rst, as(comm, 'Spatial'), method='simple', small=FALSE, fun=mean, na.rm=TRUE) %>%
  as.data.frame() %>% set_colnames("val_NORRR1MM_TOT_m")
# récupération des infos attributaires
COMM_indics_an_NORRR1MM_TOT_m <- as(comm, 'Spatial') %>% as.data.frame() %>% 
  rownames_to_column(var = "id_n") %>%
  left_join(COMM_moy_val %>%
              mutate(id_n = rownames(COMM_moy_val)),
            by = 'id_n')


# 2. quantité pluie 
# conversion en spatialpixeldf et rasterlayer
krigeage_NORRR_TOT_1000m_rst <- raster(SpatialPixelsDataFrame(points=krigeage_NORRR_TOT_1000m_df[c("x", "y")],     data=krigeage_NORRR_TOT_1000m_df[c("var1.pred")],proj4string = CRS("+init=epsg:2154")),
                                         layer=1, values=TRUE)
# calcul de la valeur moyenne du raster sur l'emprise de chaque canton
COMM_moy_val <- raster::extract(krigeage_NORRR_TOT_1000m_rst, as(comm, 'Spatial'), method='simple', small=FALSE, fun=mean, na.rm=TRUE) %>%
  as.data.frame() %>% set_colnames("val_NORRR_TOT")
# récupération des infos attributaires
COMM_indics_an_NORRR_TOT <- as(comm, 'Spatial') %>% as.data.frame() %>%
  rownames_to_column(var = "id_n") %>%
  left_join(COMM_moy_val %>%
              mutate(id_n = rownames(COMM_moy_val)),
            by = 'id_n')

COMM_indics_an_TOT_m <- COMM_indics_an_NORRR1MM_TOT_m %>% dplyr::select(-id_n) %>% left_join(COMM_indics_an_NORRR_TOT %>% dplyr::select(INSEE_COM,val_NORRR_TOT), by ="INSEE_COM" )

#export data pour rmd
fwrite(COMM_indics_an_TOT_m, file = "./data/COMM_indics_an_TOT_m.csv", verbose = F)
#COMM_indics_an_TOT_m <- fread( file = "./data/COMM_indics_an_TOT_m.csv", verbose = F)

#######################
# carte bivariée annuelle

# df raster avec infos jours pluie et quantité
krigeage_TOT_m_1000m_df <-
  krigeage_NORRR1MM_TOT_m_1000m_df %>% rename(var1.pred.NORRR1MM_TOT_m = var1.pred) %>%
  left_join(krigeage_NORRR_TOT_1000m_df %>% dplyr::select(x,y,var1.pred) %>% rename(var1.pred.NORRR_TOT = var1.pred), by = c('x','y')) %>%
  # calcul des quantiles 
  mutate(var1.pred.NORRR1MM_TOT_m.q = ntile(var1.pred.NORRR1MM_TOT_m,20),
         var1.pred.NORRR_TOT.q = ntile(var1.pred.NORRR_TOT,20)) 
  

#carte
ggplot() +
  geom_point(data = krigeage_TOT_m_1000m_df, aes(x=x, y=y, color=atan(var1.pred.NORRR1MM_TOT_m.q/var1.pred.NORRR_TOT.q),alpha=var1.pred.NORRR_TOT.q+var1.pred.NORRR1MM_TOT_m.q),  shape = 15, size = 0.01) +
  scale_color_viridis(option = 'inferno') +
  #scale_fill_distiller(palette = "Spectral")  +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Quantité et fréquence de pluie par an",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm + theme(legend.position = "none") +
  coord_sf(crs = st_crs(2154))

#### contours grandes villes (prefectures)
comm_prefs_80khabs <- comm %>%
  mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                        ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                               ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
  group_by(CODGEO) %>%
  # summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>%
  summarize(NOM_COM = first(NOM_COM),
            NOM_COM_M = first(NOM_COM_M),
            NOM_REG = first(NOM_REG),
            STATUT = first(STATUT),
            POPULATION = sum(POPULATION)) %>%
  #sélection des préfectures et communes > 80 000 habitants hors IDF
  filter(STATUT %in% c("Capitale d'état", "Préfecture de région","Préfecture")|POPULATION > 80000 & !NOM_REG %in% 'ILE-DE-FRANCE')

# export rmd
st_write(comm_prefs_80khabs, dsn = "./data/comm_prefs_80khabs.shp", layer = "comm_prefs_80khabs.shp", driver = "ESRI Shapefile")


# stats communes
# calcul des quantiles 
COMM_indics_an_TOT_m <- COMM_indics_an_TOT_m %>%
  mutate(val_NORRR_TOT_m = val_NORRR_TOT /12) %>%
  mutate(val_NORRR1MM_TOT_m.q = ntile(val_NORRR1MM_TOT_m,20),
         val_NORRR_TOT_m.q = ntile(val_NORRR_TOT_m,20)) 

# graphique non interactif

ggplot()+
  geom_point(data=COMM_indics_an_TOT_m,aes(x=val_NORRR1MM_TOT_m,y=val_NORRR_TOT_m,
                                           color=atan(val_NORRR1MM_TOT_m.q/val_NORRR_TOT_m.q),
                                           alpha=val_NORRR_TOT_m.q+val_NORRR1MM_TOT_m.q), size = 0.2)+
   geom_label_repel(data=COMM_indics_an_TOT_m %>% right_join(comm_prefs_80khabs %>% dplyr::select(CODGEO) %>% as.data.frame(), by = c("INSEE_COM" = "CODGEO")),
                   aes(x=val_NORRR1MM_TOT_m,y=val_NORRR_TOT_m,
                                                 label=as.character(NOM_COM)), size = 1.5,segment.color = NA,force = 3) +
   scale_color_viridis(option = 'inferno') +
  labs(x = "Nombre de jours de pluie (moyenne mensuelle)", y = "Quantité de pluie en mm (moyenne mensuelle)",
       title = "Quantité et fréquence de pluie par an",
       subtitle = "Moyenne sur période de référence : 1976-2005",
       caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr") +
  annotate("text", x = 6, y = 45, colour = "grey",fontsize=8, fontface="italic", label = "Il pleut peu et rarement") +
  annotate("text", x = 11, y = 55, colour = "grey",fontsize=8, fontface="italic", label = "Il pleut peu mais souvent") +
  annotate("text", x = 7, y = 110, colour = "grey",fontsize=8, fontface="italic", label = "Il pleut beaucoup mais rarement") +  
  annotate("text", x = 11, y = 170, colour = "grey",fontsize=8, fontface="italic", label = "Il pleut beaucoup et souvent") +  
  annotate("text", x = 6.5, y = 180, colour = "black",fontsize=10, fontface="bold", label = "Passez sur les points pour afficher les villes") +  
  

  thm + theme(legend.position = "none", 
              axis.text.x=element_text(size=9, color = "grey"),
              axis.title.x=element_text(size=9, color = "black"),
              axis.text.y=element_text(size=9, color = "grey"),
              axis.title.y=element_text(size=9, color = "black",angle=90)) 

library(ggrepel)
library(ggiraph)

# graphique interactif
gg <-
ggplot()+
  geom_point(data=COMM_indics_an_TOT_m,aes(x=val_NORRR1MM_TOT_m,y=val_NORRR_TOT_m,
                                           color=atan(val_NORRR1MM_TOT_m.q/val_NORRR_TOT_m.q),
                                           alpha=val_NORRR_TOT_m.q+val_NORRR1MM_TOT_m.q), size = 0.2)+

  geom_point_interactive(data = COMM_indics_an_TOT_m %>% right_join(comm_prefs_80khabs %>% dplyr::select(CODGEO) %>% as.data.frame(), by = c("INSEE_COM" = "CODGEO")),
                         aes(x= val_NORRR1MM_TOT_m, y = val_NORRR_TOT_m,tooltip = NOM_COM, data_id = INSEE_COM),shape = 21,colour = "grey",fill = "white", alpha =0.8, size =2) + 
  scale_color_viridis(option = 'inferno') +
  labs(x = "Nombre de jours de pluie (moyenne mensuelle)", y = "Quantité de pluie (en mm)",
    title = "Quantité et fréquence de pluie par an",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm + theme(legend.position = "none", 
              axis.text.x=element_text(size=9, color = "grey"),
              axis.title.x=element_text(size=9, color = "black"),
              axis.text.y=element_text(size=9, color = "grey"),
              axis.title.y=element_text(size=9, color = "black",angle=90)) 


# style du popup
tooltip_css <- "background-color:white;padding:2px;font-size: 120%;color: black;opacity:0.2"

a <-
ggiraph(code = {print(gg)},
        #width_svg = 20, 
        tooltip_extra_css = tooltip_css,
        tooltip_offx = 0, tooltip_offy = 0,
        zoom_max = 1,
        hover_css = "{fill:orange;r:6px;}")


pushViewport(viewport(layout = grid.layout(1, 2)))
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


krigeage_TOT_m_1000m_df_tot <- krigeage_TOT_m_1000m_df %>%
  expand(var1.pred.NORRR1MM_TOT_m,var1.pred.NORRR_TOT)

ggplot()+
  geom_tile(data=krigeage_TOT_m_1000m_df_tot,aes(x=var1.pred.NORRR1MM_TOT_m,y=var1.pred.NORRR_TOT,
                                                 fill=atan(var1.pred.NORRR1MM_TOT_m.q/var1.pred.NORRR_TOT.q),
                                                 alpha=var1.pred.NORRR_TOT.q+var1.pred.NORRR1MM_TOT_m.q))+
  #geom_text(alpha=1)+
  scale_fill_viridis() +
  geom_text_repel(data = DEP_indics_an_TOT_m,aes(val_NORRR1MM_TOT_m, val_NORRR_TOT_m, label = as.character(DEP)))

#export data pour rmd
fwrite(krigeage_TOT_m_1000m_df, file = "./data/krigeage_TOT_m_1000m_df.csv", verbose = F)



############ gig avec gganimate


library(gganimate)
pp <-
  # carte nb jours pluie
  ggplot() +
  geom_point(data = krigeage_1000m_df_mois %>% filter(indic %in% 'NORRR1MM'), aes(x=x, y=y, color=var1.pred, frame = MOIS_t), size = 0.01) +
  scale_color_viridis(option="magma", direction = -1, name = "", limits = c(4, 22)) +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  #geom_point(data = krigeage_nbjoursprecip_kri1000m_mois_df %>% filter(MOIS_t %in% 'Janvier') %>% filter(var1.pred.NORRR1MM_TOT_m.q5 ==1), aes(x=x, y=y), color = "black", size = 0.02) +
  #geom_sf(data = st_cast(st_as_sf(sgdf_poly), "MULTIPOLYGON"), fill = NA, size = 0.2, color = "red") +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Nombre de jours de pluie en ",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  theme(legend.position = c(0.9, 0.5)) +
  coord_sf(crs = st_crs(2154))

a <- gganimate(pp, interval = .5)

ppp <-
  # carte quantité pluie
  ggplot() +
  geom_point(data = krigeage_1000m_df_mois %>% filter(indic %in% 'NORRR'), aes(x=x, y=y, color=var1.pred, frame = MOIS_t), size = 0.01) +
  scale_color_viridis(option="magma", direction = -1, name = "", limits = c(500, 2500)) +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  #geom_point(data = krigeage_nbjoursprecip_kri1000m_mois_df %>% filter(MOIS_t %in% 'Janvier') %>% filter(var1.pred.NORRR1MM_TOT_m.q5 ==1), aes(x=x, y=y), color = "black", size = 0.02) +
  #geom_sf(data = st_cast(st_as_sf(sgdf_poly), "MULTIPOLYGON"), fill = NA, size = 0.2, color = "red") +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Quantité de pluie en ",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  theme(legend.position = c(0.9, 0.5)) +
  coord_sf(crs = st_crs(2154))

b <- gganimate(ppp, interval = .5)


pushViewport(viewport(layout = grid.layout(1, 2)))
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

##### !!!
et animation smooth
http://blog.revolutionanalytics.com/2017/05/tweenr.html



# fonction pour data communes par mois
sortie_data_krigeage_communes <- function(nn,mm, var)
{
  # sélection 
  krigeage_df <- krigeage_1000m_df_mois %>%
    filter(MOIS_n == as.numeric(nn) & indic %in% var)
  # conversion en spatialpixel et rasterlayer
  krigeage_pix <-   SpatialPixelsDataFrame(points=krigeage_df[c("x", "y")],     data=krigeage_df[c("var1.pred")],proj4string = CRS("+init=epsg:2154"))
                         
  krigeage_raster <- raster(krigeage_pix, layer=1, values=TRUE)
  # calcul de la valeur moyenne du raster sur l'emprise de chaque canton
  COMM_moy_val <- raster::extract(krigeage_raster, as(comm, 'Spatial'), method='simple', small=FALSE, fun=mean, na.rm=TRUE) %>%
    as.data.frame() %>% set_colnames("val")
  # récupération des infos attributaires
  COMM_indics_raster <- as(comm, 'Spatial') %>% as.data.frame() %>% dplyr::select(INSEE_COM) %>%
    rownames_to_column(var = "id_n") %>%
    left_join(COMM_moy_val %>%
                mutate(id_n = rownames(COMM_moy_val)),
              by = 'id_n')  %>%
    mutate(MOIS_n = as.numeric(nn),
           MOIS_t = mm,
           indic = var)
  #sortie data df en dur
  assign( paste0("COMM_indics_",var,"_TOT_1000m_",nn,"_df"), COMM_indics_raster, envir = .GlobalEnv )

}

n <- c('1','2','3','4','5','6','7','8','9','10','11','12')
m <- c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre')
mapply(sortie_data_krigeage_communes,nn =n, mm =m, var = "NORRR1MM") 
mapply(sortie_data_krigeage_communes,nn =n, mm =m, var = "NORRR") 

# aggrégation dans un seul dataframe
COMM_indics_TOT_m_mois_df <-
  mget(ls(pattern="COMM_indics_NORRR1MM_TOT_1000m_\\d+_df|COMM_indics_NORRR_TOT_1000m_\\d+_df")) %>%
  bind_rows()
#export data pour rmd
fwrite(COMM_indics_TOT_m_mois_df, file = "./data/COMM_indics_TOT_m_mois_df.csv", verbose = F)
COMM_indics_TOT_m_mois_df <- fread( file = "./data/COMM_indics_TOT_m_mois_df.csv", verbose = F)

# suppression des df temporaires
rm(list = ls(pattern="COMM_indics_NORRR1MM_TOT_1000m_\\d+_df|COMM_indics_NORRR_TOT_1000m_\\d+_df"))

###############
## graphique interactif de comparaison entre villes

# comparaison des villes

# stats sur villes de plus de 100k habitants à partir du raster


VILLES_indics_TOT_m_mois_df <-
COMM_indics_TOT_m_mois_df %>% 
  right_join(comm_prefs_80khabs %>% dplyr::select(CODGEO,NOM_COM) %>% as.data.frame(), by = c("INSEE_COM" = "CODGEO"))  %>% 
  #dplyr::select(INSEE_COM,val, MOIS_t) %>%
  spread(indic, val) %>%
  mutate(MOIS_t = factor(MOIS_t, levels = c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre')) ) %>%
  mutate(ttip = paste(
    "<strong>", NOM_COM,"</strong>"," en ",MOIS_t, ": <br />",
   format(round(as.numeric(NORRR1MM), 0), nsmall=0, big.mark=" "), " jours de pluie<br />",
   format(round(as.numeric(NORRR), 0), nsmall=0, big.mark=" "), " mm de pluie<br />",
   "soit ",format(round(as.numeric(NORRR/NORRR1MM), 0), nsmall=0, big.mark=" "), " mm de pluie par jour de pluie<br />"))
  

ppp <-
  # graphique villes quantité pluie par mois
  ggplot() +
  geom_line_interactive(data = VILLES_indics_TOT_m_mois_df , aes(MOIS_t, NORRR1MM, group = as.factor(NOM_COM),tooltip = NOM_COM, data_id = NOM_COM), alpha = 0.6, color = "grey90", size = 0.5) +
  
  geom_point_interactive(data = VILLES_indics_TOT_m_mois_df , aes(x= MOIS_t, y=NORRR1MM,  size = NORRR/NORRR1MM, color = NORRR,tooltip = ttip, data_id = NOM_COM),  alpha = 0.7, shape = 20) +
  scale_size_continuous(range = c(0.5,5), name = "Quantité de pluie\nmoyenne par jour\nde pluie (en mm)") +
  scale_color_viridis(option="inferno", direction = -1, name = "Quantité de pluie\ntotale par moi\n(en mm)") +
  scale_y_continuous(limits = c(0, 20)) +
  #scale_colour_manual(values = c("grey")) +
  labs(
    y = "Nombre de jours de pluie", x = "",
    title = "Quantité et fréquence de pluie par mois",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +  #guides(colour = "none") +
  theme(        legend.text=element_text(size=6),
                legend.title=element_text(size=7),
              axis.text.x=element_text(size=7, color = "grey",angle=30),
              axis.title.x=element_text(size=9, color = "black"),
              axis.text.y=element_text(size=9, color = "grey"),
              axis.title.y=element_text(size=9, color = "black",angle=90)) 


tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:10px 20px 10px 20px;"
#tooltip_css <- "background-color:white;padding:2px;font-size: 120%;color: black;opacity:0.2"
#tooltip_css <- "background-color:black;padding:2px;font-size: 80%;color: black;opacity:0.2"

ggiraph(code = {print(ppp)},
        #width_svg = 20, 
        tooltip_extra_css = tooltip_css,
        tooltip_offx = 0, tooltip_offy = -25,
        zoom_max = 1,
        width = 1, height = 4,
        hover_css = "color:red;stroke:red")

##########
# version ggplot en raster par mois avec facet

ggplot() +
  geom_tile(data = krigeage_nbjoursprecip_kri1000m_mois_df, aes(x=x, y=y, fill=var1.pred), color = NA) +
  scale_fill_viridis(option="magma", direction = -1, name = "") +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
  labs(
    title = "Nombre de jours de pluie par mois",
    subtitle = "Moyenne sur période de référence : 1976-2005",
    caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
  ) +   
  thm +            
  coord_sf(crs = st_crs(2154)) +
  facet_wrap(~ MOIS_n, ncol=3) 





########################
#### animation gif : version avec sortie png

######## sortie des png par mois

plosortie <- function(nn,mm)
{
plot <-
  ggplot() +
    geom_tile(data = get(paste0("krigeage_nbjoursprecip_kri1000m_",nn,"_df")), aes(x=x, y=y, fill=var1.pred), color = NA) +
    scale_fill_viridis(option="magma", direction = -1, name = "", limits = c(1, 24)) +
    geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +
    labs(
      title = paste0("Nombre de jours de pluie en ",mm),
      subtitle = "Moyenne sur période de référence : 1976-2005",
      caption = "Source : Meteo France, modèle Aladin-Climat / data.gouv.fr"
    ) +   
    thm +            
    coord_sf(crs = st_crs(2154))
  
  ggsave(paste0("./img/nbjoursprecip_kri_",nn,"_v1.png"), width = 20, height = 20, units = "cm")

}


n <- c('1','2','3','4','5','6','7','8','9','10','11','12')
m <- c('Janvier',"Février", "Mars","Avril",  "Mai",  "Juin","Juillet",  'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre')
mapply(plosortie,nn =n, mm =m) 

library(magick)

importpng <- function(nn)
{
  img <- image_scale(image_read(paste0("./img/nbjoursprecip_kri_",nn,"_v1.png")), "800")
  assign( paste0("png_nbjoursprecip_kri_",nn), img, envir = .GlobalEnv )
}

mapply(importpng,nn =n) 

frames <- image_morph(c(png_nbjoursprecip_kri_1, png_nbjoursprecip_kri_2,png_nbjoursprecip_kri_3,
                        png_nbjoursprecip_kri_4, png_nbjoursprecip_kri_5,png_nbjoursprecip_kri_6,
                        png_nbjoursprecip_kri_7, png_nbjoursprecip_kri_8,png_nbjoursprecip_kri_9,
                        png_nbjoursprecip_kri_10, png_nbjoursprecip_kri_11,png_nbjoursprecip_kri_12,png_nbjoursprecip_kri_1), frames = 10)

#frames <- image_morph(pattern="png_nbjoursprecip_kri_", frames = 10)

image_animate(frames)

rm(list=ls(pattern="krigeage_nbjoursprecip_kri5000m_"))


