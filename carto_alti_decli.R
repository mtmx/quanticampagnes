
library(tidyverse)
library(magrittr)
library(sf)
library(data.table)

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


# couches cartos supra communales
library(COGugaison)
# communes avec maillages supra-communaux correspondants
comm_supra <- merge(comm, table_supracom_2016, by.x = "CODGEO", by.y = "CODGEO", all.x = TRUE)
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


# téléchargement de la BD ALTI

url_comm <- "https://wxs-telechargement.ign.fr/jvam1hsjm11u8voorw81v2xb/telechargement/prepackage/BDALTI-75M_PACK_FXX_2017-07-11$BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21/file/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21.7z"
download.file(url_comm, destfile = "/tmp/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21.7z")
system("7z x -o/tmp /tmp/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21.7z")

library(raster)
# fonction pour lire et convertir asc en df
transf_asc <- function(x){
  r.df <- as.data.frame(rasterToPoints(raster(x), spatial = TRUE)) %>% set_colnames(c('alti','x','y'))
  alti_sf = st_as_sf(r.df, coords = c("x", "y"), crs = 2154)
  alti_sf_comm <- st_join(alti_sf, comm %>% select(CODGEO)) %>% as.data.frame() %>% select(-geometry)
  alti_sf_comm <- alti_sf_comm %>% filter(!is.na(CODGEO))
}
# test de la fonction sur un fichier asc
test <- transf_asc(paste0("./temp/BDALTIr_2-0_MNT_EXT_0075_6825_LAMB93_IGN69_20110929.asc"))

# liste des fichiers à convertir
files <- dir(path = "/tmp/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21/BDALTIV2/1_DONNEES_LIVRAISON_2017-06-00068/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE/",pattern = "*.asc")

# dataframe avec toutes les altis
alti.df <- files %>% 
  map(function(x) {
    transf_asc(paste0("/tmp/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2017-06-21/BDALTIV2/1_DONNEES_LIVRAISON_2017-06-00068/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE/", x))
  }) %>%
  reduce(rbind)

# export data pour sauvegarde
fwrite(alti.df, file = "./data/alti_df.csv", verbose = F)
alti_df <- fread(file = "./data/alti_df.csv", verbose = F, colClasses = c("numeric","character"))  %>% as.data.frame()

#############
# calcul des stats par commune 
COMM_alti.df <-
  alti_df %>%
  group_by(CODGEO) %>% 
  summarise(moyenne = mean(alti),
            ecart_type = sd(alti),
            min = min(alti),
            max = max(alti),
            D1 = quantile(alti, prob=0.1),
            D9 = quantile(alti, prob=0.9),
            C05 = quantile(alti, prob=0.05),
            C95 = quantile(alti, prob=0.95)) %>%
  mutate(ecart_D1_D9 = D9-D1,
         ecart_C95_C05 = C95-C05,
         ecart_minmax = max - min) %>% 
  left_join(comm %>% as.data.frame() %>% select(CODGEO, NOM_COMM,STATUT,POPULATION,superficie_ha), by ="CODGEO")

# export
fwrite(COMM_alti.df, file = "./data/COMM_alti.df.csv", verbose = F)
COMM_alti.df <- fread(file = "./data/COMM_alti.df.csv", verbose = F, colClasses = c("character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character","character","numeric","numeric"))  %>% as.data.frame()

#############
# calcul des stats par cantons 2016 
CV_alti.df <-
  alti_df %>%
  left_join(table_supracom_2016 %>% select(CODGEO,CV), by = "CODGEO") %>%
  group_by(CV) %>% 
  summarise(moyenne = mean(alti),
            ecart_type = sd(alti),
            min = min(alti),
            max = max(alti),
            D1 = quantile(alti, prob=0.1),
            D9 = quantile(alti, prob=0.9),
            C05 = quantile(alti, prob=0.05),
            C95 = quantile(alti, prob=0.95)) %>%
  mutate(ecart_D1_D9 = D9-D1,
         ecart_C95_C05 = C95-C05,
         ecart_minmax = max - min) 

fwrite(CV_alti.df, file = "./data/COMM_alti.df.csv", verbose = F)

#############
## graphique exemples de 3 communes même altitude
COMM_exemple_alti <- alti_df %>% filter( CODGEO %in% c('45123', '2B050','76069') )
COMM_exemple_alti_indics <- COMM_alti.df %>% filter( CODGEO %in% c('45123', '2B050','76069') )

# export
fwrite(COMM_exemple_alti, file = "./data/COMM_exemple_alti.csv", verbose = F)
COMM_exemple_alti <- fread(file = "./data/COMM_exemple_alti.csv", verbose = F, colClasses = c("numeric","character")) 
COMM_exemple_alti_indics <- COMM_alti.df %>% filter( CODGEO %in% c('45123', '2B050','76069') )
COMM_exemple_alti <- COMM_exemple_alti %>% left_join(COMM_exemple_alti_indics, by = "CODGEO")

ggplot(COMM_exemple_alti, aes(x = paste0(NOM_COMM," (",substr(CODGEO,1,2),")") , y = alti)) +
  geom_dotplot(binaxis = "y", stackdir = "center",binwidth = 1, method = "histodot", colour="grey40" ) +
  labs(x = "", y = "ALTITUDE (en mètres)",
       title = "Exemples de trois communes ayant la même altitude moyenne",
       subtitle = "Nombre de points de mesure sur le territoire de la commune",
       caption = "Source : IGN BD ALTI / GEOFLA") +
  annotate("text", x = 1.75, y = 400, colour = "black",size=3, fontface="italic", label = "Moyenne : 100 m") +
  annotate("text", x = 1.75, y = 380, colour = "black",size=3, fontface="italic", label = "Ecart-type : 107 m") + 
  annotate("text", x = 1.75, y = 360, colour = "black",size=3, fontface="italic", label = "Minimum : 0 m") +
  annotate("text", x = 1.75, y = 340, colour = "black",size=3, fontface="italic", label = "Maximum : 620 m") +
  
  annotate("text", x = 1, y = 300, colour = "black",size=3, fontface="italic", label = "Moyenne : 100 m") +
  annotate("text", x = 1, y = 280, colour = "black",size=3, fontface="italic", label = "Ecart-type : 52 m") + 
  annotate("text", x = 1, y = 260, colour = "black",size=3, fontface="italic", label = "Minimum : 3 m") +
  annotate("text", x = 1, y = 240, colour = "black",size=3, fontface="italic", label = "Maximum : 161 m") +
  
  annotate("text", x = 3, y = 200, colour = "black",size=3, fontface="italic", label = "Moyenne : 100 m") +
  annotate("text", x = 3, y = 180, colour = "black",size=3, fontface="italic", label = "Ecart-type : 1 m") + 
  annotate("text", x = 3, y = 160, colour = "black",size=3, fontface="italic", label = "Minimum : 99 m") +
  annotate("text", x = 3, y = 140, colour = "black",size=3, fontface="italic", label = "Maximum : 103 m") 

##### sortie png des deux cartes
library(cartography)

# palette de couleurs
cols <- carto.pal(pal1 = "green.pal",
                  n1 = 10,
                  pal2 = "sand.pal", 
                  n2 = 10) 
# export png
png("./img/img_decli/Ecart_type.png", width=7, height=6, units="cm", res=250)
par(mar = c(0,0,1.2,0))
choroLayer(x = merge(comm, COMM_alti.df, by = "CODGEO", all.x = TRUE) , 
           var = "ecart_type", 
           # breaks = bks,
           nclass = 20, method = "quantile",
           col = cols,  
           border = NA, 
           # lwd = NA, 
           legend.pos = "topright", 
           legend.title.txt = "Valeur\n(discrétisation\npar quantiles)", 
           legend.values.rnd = 0, 
           add = F) 
plot(DEP, border = "grey40", col = NA,lwd = 0.5, add = TRUE)
text(x = 939168, y = 7128487, labels = "ECART-TYPE", cex = 1, adj = 0)
# titre
layoutLayer(title = "Altitude : ECART-TYPE par commune (en mètres)", author = "", 
            sources = "IGN : BD ALTI / GEOFLA", frame = TRUE, col = "grey", 
            scale = NULL,coltitle = "black",
            south = F) 
dev.off()

# carte moyenne
png("./img/img_decli/Moyenne.png", width=7, height=6, units="cm", res=250)
par(mar = c(0,0,1.2,0))
choroLayer(x = merge(comm, COMM_alti.df, by = "CODGEO", all.x = TRUE) , 
           var = "moyenne", 
           # breaks = bks,
           nclass = 20, method = "quantile",
           col = cols,  
           border = NA, 
           # lwd = NA, 
           legend.pos = "topright", 
           legend.title.txt = "Valeur\n(discrétisation\npar quantiles)", 
           legend.values.rnd = 0, 
           add = F) 
plot(DEP, border = "grey40", col = NA,lwd = 0.5, add = TRUE)
text(x = 939168, y = 7128487, labels = "MOYENNE", cex = 1, adj = 0)
# titre
layoutLayer(title = "Altitude : MOYENNE par commune (en mètres)", author = "", 
            sources = "IGN : BD ALTI / GEOFLA", frame = TRUE, col = "grey", 
            scale = NULL,coltitle = "black",
            south = F) 
dev.off()

#########
## menu déroulant images
library(stringr)
library(bsselectR)
img_decli <- paste0(list.files("img_decli", full.names = TRUE))
names(img_decli) <- str_replace_all(img_decli, 
                                    c("\\.png" = "", 
                                      "img_decli/" = ""))

bsselect(img_decli, type = "img",
         live_search = TRUE, show_tick = TRUE,frame_height = "30", frame_width = "100%", height = 10)

########
# scatter plot toutes communes

library(scatterD3)
COMM_alti.df <- COMM_alti.df %>%
  mutate(type_commune = ifelse(STATUT %in% c("Capitale d'état", "Préfecture de région","Préfecture")|POPULATION > 50000  , "Grandes villes","Autres communes"))


scatterD3(data = COMM_alti.df, x = ecart_type, y = moyenne, 
          #lab = CODGEO,
          tooltip_text = paste(
            "<strong>", COMM_alti.df$NOM_COMM,"</strong>", " (", substr(COMM_alti.df$CODGEO,1,2),") <br />",
            " Moyenne : ", format(round(as.numeric(COMM_alti.df$moyenne), 0), nsmall=0, big.mark=" "), " mètres<br />",
            " Ecart-type : ",format(round(as.numeric(COMM_alti.df$ecart_type), 0), nsmall=0, big.mark=" "), " mètres<br />"),
          
           # fixed = TRUE,
          # left_margin = 80,
          col_var = type_commune,
          colors = c("Grandes villes" = "#C02942", "Autres communes" = "#f2d4d9"),
          col_lab = "Type de communes",
          #colors = "grey",
          point_opacity =0.5,
          axes_font_size = "120%",
          legend_font_size = "14px",
          caption = "Zoomer et passer sur les points pour afficher les caractéristiques de chaque commune. 
          Cliquer sur la catégorie 'Grandes villes' pour n'afficher que celles-ci",
          xlim = c(0,900),ylim = c(0,3000),
          #col_var = cyl,
          size_var = superficie_ha,
          size_lab = "Superficie",
          #size_range = c(100,300),
          hover_size = 4,
          width = 800, height = 600,
          xlab = "Ecart-type d'altitude (en mètres)", ylab = "Moyenne d'altitude (en mètres)"#, col_lab = "Cylinders",
          #symbol_lab = "Manual transmission"
)


########## gif 

library(magick)
# import png
Ecart_type <- image_read("./img/img_decli/Ecart_type.png")
Moyenne <- image_read("./img/img_decli/Moyenne.png")

# import png avec bonnes dimensions
Ecart_type <- image_scale(image_read("./img/img_decli/Ecart_type.png"), "800")
Moyenne <- image_scale(image_read("./img/img_decli/Moyenne.png"), "800")

frames <- c(Ecart_type,Moyenne)
# animation
animation <- image_animate(image_join(frames), fps = 0.5)
print(animation)

# export gif
image_write(animation, "./img/img_decli/carto_anim_alti.gif")
