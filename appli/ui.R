
library(shiny)
library(ggplot2)
library(ggiraph)

# thx https://stackoverflow.com/a/31066997

JScode <-
  "$(function() {
setTimeout(function(){
var vals = [0];
var powStart = 5;
var powStop = 2;
for (i = powStart; i >= powStop; i--) {
var val = Math.pow(10, -i);
val = parseFloat(val.toFixed(8));
vals.push(val);
}
$('#filtre_grossesbetes').data('ionRangeSlider').update({'values':['très peu',  'un peu', 'moyennement', 'pas mal', 'beaucoup']})
$('#filtre_vieuxlogements').data('ionRangeSlider').update({'values':['très peu',  'un peu', 'moyennement', 'pas mal', 'beaucoup']})
$('#filtre_pluie').data('ionRangeSlider').update({'values':['rarement','un peu','assez souvent','souvent','très souvent']})
$('#filtre_declivite').data('ionRangeSlider').update({'values':['tout plat','plutôt plat','un peu vallonné','vallonné','montagneux']})
$('#filtre_forets').data('ionRangeSlider').update({'values':['très peu',  'un peu', 'moyennement', 'pas mal', 'beaucoup']})
$('#filtre_eau').data('ionRangeSlider').update({'values':['très peu',  'un peu', 'moyennement', 'pas mal', 'beaucoup']})
}, 5)})"

 fluidPage(
   tags$head(tags$script(HTML(JScode))),
   tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
   # Application title
   #titlePanel("Cartographie des cantons :"),
   
   
   sidebarLayout(
     sidebarPanel(
       width = 3,
       tags$head(tags$style(type="text/css", 
                            ".test_type {color: black;
                            font-size: 6px; 
                            font-style: italic;}"
       )
       ),
       div(class="test_type",
           width = 3,
           # h2("Filtres"),
           # 
           # br(),
           # fluidRow( 
           #   column(3, img(height = 25, width = 25, src = "vache_2.png")),
           #   column(9, p("filtre_vaches_2")) ),
           
           img(src="vache_2.png", height = 25, width = 40, align = "right"),
           img(src="mouton.png", height = 8, width = 10, align = "right"),
           sliderInput("filtre_grossesbetes", label = h5("Belles bêtes"), ticks = F,
                       min = 0, max = 4, value = c(2, 4)) ,
           #br(),
           img(src="vieille_maison_2.png", height = 25, width = 40, align = "right"),
           sliderInput("filtre_vieuxlogements", label = h5("Vieilles pierres"),ticks = F,
                       min = 0, max = 4, value = c(2, 4)),
           #br(),
           img(src="pluie.png", height = 25, width = 30, align = "right"),
           sliderInput("filtre_pluie", label = h5("Pluie"),ticks = F,
                       min = 0, max = 4, value = c(2, 4)) ,
           #br(),
           img(src="montagne.png", height = 25, width = 30, align = "right"),
           sliderInput("filtre_declivite", label = h5("Déclivité du relief"),ticks = F,
                       min = 0, max = 4, value = c(2, 3)),
           #br(),
           img(src="arbre.png", height = 30, width = 40, align = "right"),
           sliderInput("filtre_forets", label = h5("Forêts"),ticks = F,
                       min = 0, max = 4, value = c(1, 4)) ,
           #br(),
           img(src="etang.png", height = 25, width = 30, align = "right"),
           sliderInput("filtre_eau", label = h5("Présence d'eau"),ticks = F,
                       min = 0, max = 4, value = c(1, 3)))), 
     mainPanel(
       ggiraphOutput("ggcarte", width = 600, height = 500)
     )
       )
 )
