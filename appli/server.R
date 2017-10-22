library(shiny)
library(ggplot2)
# pb https://github.com/dkahle/ggmap/issues/144
# install_github("hadley/ggplot2@v2.2.0")

library(magrittr)
library(ggiraph)
library(scales)
library(data.table)
library(sf)
library(tidyverse)
library(ggmap) #install_github("dkahle/ggmap")

function(input,output){
  
  # import shp pour emprise fonds de carte
  DEP_geo <- st_read("./data/DEP_appli.geo.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) 
  
  # centroides en dataframes 
  CV_data.geo_ctr <- fread( file = "./data/CV_data.geo_ctr.csv", verbose = F) %>%
    mutate_at(vars(starts_with("score")),funs(as.numeric))

  # fonds de carte stamen
  
   nc_map = get_map(location = unname(st_bbox(DEP_geo %>% st_buffer(dist = 50000) %>% st_transform(4326))), maptype = "terrain-background", zoom =7) 
  
  #############
  ### ggiraph points
  #library(ggiraph)
  # style du popup
  tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"
  
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
  
  ###
  # filtre du dataframe à afficher
  filteredData <- reactive({
    CV_data.geo_ctr %>% filter(score_grossesbetes >= input$filtre_grossesbetes[1] & 
                                 score_grossesbetes <= input$filtre_grossesbetes[2] &
                                 score_vieuxlogements >= input$filtre_vieuxlogements[1] & 
                                 score_vieuxlogements <= input$filtre_vieuxlogements[2] &
                                 score_pluie >= input$filtre_pluie[1] & 
                                 score_pluie <= input$filtre_pluie[2] &
                                 score_declivite >= input$filtre_declivite[1] & 
                                 score_declivite <= input$filtre_declivite[2] &
                                 score_OS_eau >= input$filtre_eau[1] & 
                                 score_OS_eau <= input$filtre_eau[2]  &
                                 score_OS_forets >= input$filtre_forets[1] & 
                                 score_OS_forets <= input$filtre_forets[2]) 
  })
  
  
  output$ggcarte <- renderggiraph({
    
    filteredData<-filteredData()
    
    mygg <- 
      #ggplot() +
      ggmap(nc_map) +
      geom_point_interactive(data = filteredData, aes(x= x, y = y, size = superficie_km2,alpha = 0.5,stroke = 0.5,
                                                      tooltip = paste0(
                                                        #"<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                        #"<img src = ",paste0('"' ,img,'"'), " height=\"50\"width=\"60\">",
                                                        "<b>","<font size=2.5 color=black>" , LIBGEO_conv,"</b>"," (",substr(CV,1,2),")", "</font>", "<br>"),
                                                      data_id = CV), color = "black", fill = "grey", shape = 21 ) +
      scale_size(range = c(0.8,3)) +
      thm + theme(legend.position="none") 
    
    # ggiraph
    ggiraph(code = {print(mygg)},
            #selection_type = "multiple", 
            # height_svg = 50, 
            #  width_svg = 50,
            #width = 0.7,
            #height = 5,
            tooltip_extra_css = tooltip_css,
            tooltip_offx = 0, tooltip_offy = -25,
            zoom_max = 4,
            hover_css = "{fill:orange;r:6px;}")
    
    
    
  })
  
}