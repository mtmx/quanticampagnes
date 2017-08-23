
# chargement librairies
options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(tidyverse)
library(magrittr)
library(COGugaison)
library(data.table)
library(sf)
library(janitor)
library(stringi)
library(ggplot2)
library(forcats)
library(devtools)

# data cheptels

# source : https://stats.agriculture.gouv.fr/disar/
# recherche tableau référencé 064_RA_003ACANTON : Cheptels et unités gros bétail en 2010
# afficher champ 'Liste géographique (cantons définition 2010)' (dans option 'navigateur')
# exporter en xls
# etre très patient...

CANTONS_animaux_2010 <- loadWorkbook("./data/CANTONS_animaux_2010_v2.xlsx", create = TRUE)  %>%
  readWorksheet( "data" ,header = TRUE, startRow = 1 ) %>%
  select(Indicateur,Canton, Exploitation, Cheptel_tete)  %>%
  mutate(Canton = substr(Canton,1,4),
         Exploitation= as.numeric(gsub(",",".",Exploitation)),
         Cheptel_tete = as.numeric(gsub(",",".",Cheptel_tete))) %>%
  filter(!Indicateur %in% c('Elevages (total hors apiculture)(1)','Herbivores(1)' ,'Granivores(1)','Apiculture'))

# suppression accents
CANTONS_animaux_2010 <- CANTONS_animaux_2010 %>%
  mutate(Indicateur = stri_trans_general(Indicateur,"Latin-ASCII"))

#CANTONS_animaux_2010 %>% distinct(Indicateur)
liste_cantons <- CANTONS_animaux_2010 %>% distinct(Canton) %>% mutate(CV_agreste = 1)
COG_akinator(vecteur_codgeo=liste_cantons[,1],donnees_insee=F)


######### maillages supra comm 2011
# verif que les cantons sont bien de 2011
#https://www.insee.fr/fr/information/2028028

# téléchargement et import de la table communale avec cantons, COG 2011
# (normalement un cas géré par COGugaison, mais millésime non dispo le package pour l'instant)
tmp <- tempdir()
url_data <- "https://www.insee.fr/fr/statistiques/fichier/2028028/table-appartenance-geo-communes-11.zip"
download.file(url_data, destfile = "/tmp/table-appartenance-geo-communes-11.zip")
system("7z x -o/tmp /tmp/table-appartenance-geo-communes-11.zip")

TABCOMM_COG2011 <- loadWorkbook("/tmp/table-appartenance-geo-communes-11.xls", create = TRUE)  %>%
  readWorksheet( "Liste_COM" ,header = TRUE, startRow = 6 )  %>%
  select(CODGEO,CV, DEP) %>%
  left_join(loadWorkbook("/tmp/table-appartenance-geo-communes-11.xls", create = TRUE)  %>%
              readWorksheet( "Niv_supracom" ,header = TRUE, startRow = 6 ) %>%
              filter(NIVGEO %in% 'CV'), by = c("CV" ="CODGEO"))

# liste des cantons 2011
liste_CV <- TABCOMM_COG2011 %>% distinct(CV) %>% mutate(CV_cog2011 = 1)

#téléchargement et import du shape communes COG 2011 avec arrondissements PLM et correction géometries

url_data <- "https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2011_GEOFLA_1-1_SHP_LAMB93_FR-ED111/file/GEOFLA_1-1_SHP_LAMB93_FR-ED111.7z"
download.file(url_data, destfile = "/tmp/GEOFLA_1-1_SHP_LAMB93_FR-ED111.7z")
system("7z x -o/tmp /tmp/GEOFLA_1-1_SHP_LAMB93_FR-ED111.7z")

comm2011 <- st_read("/tmp/GEOFLA_1-1_SHP_LAMB93_FR-ED111/GEOFLA/1_DONNEES_LIVRAISON_2013-12-00225/GEOFLA_1-1_SHP_LAMB93_FR-ED111/COMMUNES/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
  mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                        ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                               ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
  group_by(CODGEO) %>%
  summarize(NOM_COMM = first(NOM_COMM),
            STATUT = first(STATUT)) %>% 
  st_buffer(dist = 0)

# constitution des contours cantons 2011
comm_supra <- merge(comm2011, TABCOMM_COG2011, by.x = "CODGEO", by.y = "CODGEO", all.x = TRUE)
DEP <- comm_supra %>%  select(DEP) %>% group_by(DEP) %>% summarize() 
DEP.s <- DEP %>% st_simplify( preserveTopology = FALSE, dTolerance = 10)
CV <- comm_supra %>%  select(CV,LIBGEO) %>% group_by(CV) %>% summarize(LIBGEO = first(LIBGEO)) 


######## population 2011

url_data <- "https://www.insee.fr/fr/statistiques/fichier/2044743/base-cc-evol-struct-pop-2010.zip"
download.file(url_data, destfile = "/tmp/base-cc-evol-struct-pop-2010.zip")
system("7z x -o/tmp /tmp/base-cc-evol-struct-pop-2010.zip")

COMM_POP_2010 <- loadWorkbook("/tmp/base-cc-evol-struct-pop-2010.xls", create = TRUE)  %>%
  readWorksheet( "COM_2010" ,header = TRUE, startRow = 6 )  %>%
  select(CODGEO,P10_POP)

#vérification du COG
COG_akinator(vecteur_codgeo=COMM_POP_2010[,1],donnees_insee=T)
# conversion des données communales en géographie 2011
COMM_POP_2010_COG2011 <-  changement_COG_varNum(table_entree=COMM_POP_2010,annees=c(2012:2011),agregation=T,libgeo=T,donnees_insee=T)
# aggrégation en maille cantons
CV_POP_2010 <- 
  COMM_POP_2010_COG2011 %>% left_join(TABCOMM_COG2011, by = c("CODGEO" = "CODGEO")) %>%
  group_by(CV) %>% summarise(P10_POP = sum(P10_POP))




################# données sur intégralité des cantons, en largeur
CV_data_pop_elevage <-
  CV_POP_2010 %>% left_join(
    CANTONS_animaux_2010 %>%
      group_by(Canton, Indicateur) %>%
      summarise(Cheptel_tete = sum(Cheptel_tete)) %>%
      select(Canton, Indicateur,Cheptel_tete) %>%
      spread( Indicateur,  Cheptel_tete) %>%
      janitor::clean_names() %>%
      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))), by = c('CV'= 'canton')) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  filter(!substr(CV,1,2) %in% '97')

# et en longueur
CV_data_pop_elevage_lng <-
  CV_data_pop_elevage %>%
  gather("type_pop", "nb", 2:15)

# type de population la plus présente par canton
CV_data_pop_elevage_max <-
  CV_data_pop_elevage_lng %>%
  filter(type_pop %in% c('P10_POP','total_bovins','total_caprins','total_equides','total_ovins','total_porcins')) %>%
  group_by(CV) %>%
  filter(nb == max(nb))  %>%
  rename(type_pop_max = type_pop,nb_max = nb)


# données full
CV_data <- merge(CV, CV_data_pop_elevage %>% left_join( CV_data_pop_elevage_max, by ="CV"), by ="CV")

########################
### totaux cheptels france
FR_data_pop_elevage <- CV_data_pop_elevage_lng %>% group_by(type_pop) %>% summarise(nb = sum(nb))

##### cartes à points

# création de tous les objets points
creation_pts <- function(type_pop,ratio)
{
  #ratio <- enquo(ratio)
  CV_pts <- st_sample(CV_data , CV_data[[paste0(type_pop)]] / ratio)  %>%
    as.data.frame  %>%
    mutate(type_pop = paste0(type_pop) , ratio = ratio) %>%
    st_as_sf
  
  #sortie data points en dur
  assign( paste0("CV_pts_",type_pop), CV_pts, envir = .GlobalEnv )
  
}

#CV_data %>% as.data.frame() %>% colnames()
type_pop <- c('P10_POP', 'total_porcins', 'total_bovins','lapines_meres','total_caprins','total_equides','total_ovins','volailles')
ratio <- c(10000, 2000, 1000,1000,1000,200,1000,50000)
#ratio <- c(50000, 50000, 50000,50000,50000,50000,50000,50000)
mapply(creation_pts,type_pop =type_pop, ratio =ratio) 

# dans un seul objet
CV_pts <- rbind(CV_pts_P10_POP, CV_pts_total_porcins, CV_pts_total_bovins, CV_pts_lapines_meres,
                CV_pts_total_caprins,CV_pts_total_equides,CV_pts_total_ovins,CV_pts_volailles)

# dataframe (ne sert pas finalement)
CV_pts_df <- CV_pts %>% as.data.frame() %>% cbind( do.call(rbind, st_geometry(CV_pts)) %>% set_colnames(c("x","y"))) %>% select(-geometry)


# ordre facteur et renommage
CV_pts <- CV_pts %>%
  mutate(type_pop = factor(type_pop,levels = c( 'total_bovins','total_equides','total_porcins', 'total_caprins','total_ovins','lapines_meres','volailles','P10_POP'))) %>%
  mutate( type_pop_lab = fct_recode(type_pop,
                                    "Bovins (veaux, vaches, génisses, boeufs...)" = 'total_bovins',
                                    "Equidés (chevaux, anes, mulets...)" = 'total_equides',
                                    "Porcins (cochons, truies, porcelets)" ='total_porcins',
                                    "Caprins (chèvres, boucs, chevreaux)" = 'total_caprins',
                                    "Ovins (moutons, brebis, agneaux)" = 'total_ovins',
                                    "Lapines-mères" = 'lapines_meres',
                                    "Volailles" = 'volailles' ,
                                    'Population humaine' = 'P10_POP'))


# polygones à partir des points pour un meilleur controle du rendu
# ne sert pas finalement
CV_pts_poly <- CV_pts %>%  st_buffer(dist = 500) %>% st_cast( "MULTIPOLYGON")  %>% st_simplify()


# parametrage du thème ggplot
thm <- 
  theme(legend.position="right",
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
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
        legend.key.width = unit(0.2, "cm"),
        strip.text.y = element_text(size = 5, colour = "black", angle = 0),
        strip.text.x = element_text(size = 5, colour = "black", angle = 0),
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=9,face="italic"),
        plot.caption=element_text(size=6,colour = "grey20")) 

# export data
st_write(CV_pts_poly, dsn = "./data/CV_pts_poly.shp", layer = "CV_pts_poly.shp", driver = "ESRI Shapefile")
st_write(CV_pts, dsn = "./data/CV_pts.shp", layer = "CV_pts.shp", driver = "ESRI Shapefile")
st_write(DEP.s, dsn = "./data/DEP.s.shp", layer = "DEP.s.shp", driver = "ESRI Shapefile")

