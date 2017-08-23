#####
library(cartography)

# grille 20000m
grd_20000m <- getGridLayer(CV_data %>% st_simplify(preserveTopology = TRUE, dTolerance = 1000),
                           cellsize = 20000 * 20000, type = "regular", var =c('P10_POP','total_bovins','total_caprins','total_equides','total_ovins','total_porcins'))

# type de pop max par cellule

grd_data_pop_elevage_lng <-
  grd_20000m %>%
  select(id,c('P10_POP','total_bovins','total_caprins','total_equides','total_ovins','total_porcins'),-geometry ) %>%
  gather("type_pop", "nb", 2:7)

grd_data_pop_elevage_max <-
grd_data_pop_elevage_lng %>%
  #filter(type_pop %in% c('P10_POP','total_bovins','total_caprins','total_equides','total_ovins','total_porcins')) %>%
  group_by(id) %>%
  filter(nb == max(nb))  %>%
  rename(type_pop_max = type_pop,nb_max = nb)

# type de cheptels plus nombreux que la population humaine en longueur
grd_data_pop_elevage_max_lng <-
  grd_data_pop_elevage_lng %>%
  filter(type_pop %in% c('P10_POP','total_bovins','total_caprins','total_equides','total_ovins','total_porcins')) %>%
  left_join(grd_data_pop_elevage_lng %>% as.data.frame() %>% filter(type_pop %in% c('P10_POP')) %>% select(-type_pop, -geometry) %>% rename(nb_pop = nb), by = "id") %>%
  group_by(id) %>%
  filter(nb > nb_pop)  %>%
  select(-nb,-nb_pop) %>%
  mutate(type_pop2 = type_pop) %>%
  as.data.frame() %>%
  select(-geometry)

# et en largeur
grd_data_pop_elevage_max_lrg <- grd_20000m %>%
  select(id) %>% 
  left_join(grd_data_pop_elevage_max2 %>%
              spread(type_pop, type_pop2) , by = "id") %>%
  mutate(typo_supPOP = ifelse(is.na(total_bovins) & is.na(total_caprins) & is.na(total_ovins) & is.na(total_porcins), "POP",
                              paste0(total_bovins, "_",total_caprins, "_",total_ovins, "_",total_porcins))) %>%
  mutate(typo_supPOP = gsub("_NA","", typo_supPOP)) %>%
  mutate(typo_supPOP = gsub("NA_","", typo_supPOP)) %>%
  mutate(typo_supPOP = gsub("total_","", typo_supPOP)) %>%
  mutate(typo_supPOP_lib = ifelse(typo_supPOP %in% 'POP' , "",
                                  paste0("+ de ",typo_supPOP, " que d'humains") )) %>%
  mutate(typo_supPOP_lib = gsub("_"," que d'humains, + de ", typo_supPOP_lib)) %>%
  mutate(typo_supPOP_lib = gsub(",","<br>", typo_supPOP_lib)) %>%
  mutate(typo_supPOP_lib = gsub("'","&#39", typo_supPOP_lib)) 

  

# df des centroides des cellules
grd_data_ctr <- grd_20000m %>% as.data.frame() %>% cbind(  do.call(rbind, st_geometry( grd_20000m %>% st_centroid()  )) %>% set_colnames(c("lon","lat")) ) %>%
  left_join(grd_data_pop_elevage_max %>% as.data.frame() %>% select(-geometry), by ="id") %>%
  left_join(grd_data_pop_elevage_max_lrg %>% as.data.frame() %>% select(id, typo_supPOP, typo_supPOP_lib), by ="id") %>%
  mutate(type_pop_max = ifelse(type_pop_max %in% 'total_bovins', "bovins",
                               ifelse(type_pop_max %in% 'total_porcins', "porcins",
                                      ifelse(type_pop_max %in% 'total_bovins', "bovins",
                                             ifelse(type_pop_max %in% 'total_caprins', "caprins",
                                                    ifelse(type_pop_max %in% 'total_ovins', "ovins",'humains'))))))




## image logo
library("ggplot2")
#source("https://www.bioconductor.org/biocLite.R")
#biocLite("EBImage")
#install.packages("ggimage")
library(ggimage)

grd_data_ctr_img <- grd_data_ctr %>%
  mutate(img = ifelse(type_pop_max %in% 'bovins', "http://www.pngall.com/wp-content/uploads/2016/03/Cow-PNG-11.png",
                      ifelse(type_pop_max %in% 'porcins', "http://www.pngall.com/wp-content/uploads/2016/03/Pig-Transparent.png", #"./logos/cochon.png", 
                             ifelse(type_pop_max %in% 'caprins', "http://www.pngall.com/wp-content/uploads/2016/06/Goat-Free-PNG-Image.png",
                                    ifelse(type_pop_max %in% 'ovins', "http://www.pngall.com/wp-content/uploads/2016/03/Sheep.png",
                                           ifelse(type_pop_max %in% 'equides', "http://pngimg.com/uploads/horse/horse_PNG2558.png","http://www.alloprof.qc.ca/BV/PublishingImages/pages/a0306/a306i29.png")))))) %>%
  mutate(tip = paste0(
                      #"<style> div.leaflet-popup-content {width:auto!important;}</style>",
                      "<img src = ",paste0('"' ,img,'"'), " height=\"50\"width=\"60\">",
                      "<br>","<b>","<font size=1.5 color=black>" ,"Plus gros cheptel : ", type_pop_max,"</b>","</font>", "<br>",
                      "<font size=1.5 color=black>" , typo_supPOP_lib,"</font>", "<br>")) %>%
  mutate(tip_1 = paste0(type_pop_max,
    "<b>","<font size=2.5 color=black>" ,"Plus gros cheptel : ", type_pop_max,"</b>","</font>", "<br>",
    "<font size=2.5 color=black>" , typo_supPOP_lib,"</font>", "<br>")) %>%
  mutate(tip_img = paste0("<img src = ",paste0('"' ,img,'"'), " height=\"50\"width=\"60\">")) %>%
  mutate(tip_2 = paste0("<b>","<font size=1.5 color=black>" ,"Cheptel le plus nombreux : ", type_pop_max,"</b>","</font>", "<br>")) %>%
  tbl_df() %>%
  select(-geometry)


# carto finale

ggplot() +
  #geom_sf(data =  st_cast(CV_pts_poly_2 , "MULTIPOLYGON") , aes( fill =type_pop_lab),color = NA, alpha = 0.8) + 
  geom_image(data = grd_data_ctr_img ,aes(x= lon, y = lat,image=img), size=.014) + 
  coord_sf(crs = st_crs(2154)) +
  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey85", fill = NA ,size = 0.1) +
  thm +
  # annotations
   labs(
    title = "Cheptels",
    subtitle = "par canton",
    caption = "Source : Ministère de l'agriculture / Recensement agricole 2010"
  )

# avec ggiraph

#install_github("davidgohel/ggiraph")
library(ggiraph)

# fonction pour gérer accents
conv_accents <- function(x) {
  x <- gsub(pattern = "è", replacement = "&egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&ccedil;", x = x)
  
  x <- gsub(pattern = "è", replacement = "&Egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&Eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&Ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&Euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&Icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&Iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&Ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&Uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&Ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&Agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&Acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&Ccedil;", x = x)
  x <- gsub(pattern = "'", replacement = "&apos;", x = x)
  
  return(x)
}

# style du popup
#tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0;width:50px;height:10px"
tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

# 
my_gg <-
  ggplot() +
  #geom_sf(data =  st_cast(CV_pts_poly_2 , "MULTIPOLYGON") , aes( fill =type_pop_lab),color = NA, alpha = 0.8) + 

  geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey65", fill = NA ,size = 0.2) +

  geom_image(data = grd_data_ctr_img ,aes(x= lon, y = lat,image=img), size=.02) + 
  geom_point_interactive(data = grd_data_ctr_img,
                         aes(x= lon, y = lat,
                             tooltip = tip,
                             data_id = id),shape = 15,colour = "white", alpha =0.1, size =5) + 
  #coord_sf(crs = st_crs(2154)) +
  #geom_sf(data = st_cast(DEP.s, "MULTIPOLYGON"), color = "grey85", fill = NA ,size = 0.1) +
  thm +
  # annotations
  labs(
    title = "Cheptel le plus nombreux",
    subtitle = "(humains, porcins, bovins, caprins ou ovins)",
    caption = "Source : Ministère de l'agriculture / Recensement agricole 2010"
  )


# ggiraph
ggiraph(code = {print(my_gg)},
       # height_svg = 5, 
      #  width_svg = 15,
      width = 1,
        tooltip_extra_css = tooltip_css,
        tooltip_offx = -40, tooltip_offy = -30,
        zoom_max = 4,
        hover_css = "{fill:orange;r:6px;}")


# export data
fwrite(grd_data_ctr_img, file = "./data/grd_data_ctr_img.csv", verbose = F)


