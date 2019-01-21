# Cargamos poligonos del shpaefile
library(rgdal)
poligons<- readOGR("data/Comarques/shapefiles_catalunya_comarcas.shp",
                   "shapefiles_catalunya_comarcas",
                   use_iconv = TRUE, # Sin este paramtro da errores en los accentos para el catalan
                   encoding = "UTF-8")

# Para visualizar datos contenidos en el archivo
head(poligons@data)
poligons@data$nom_comar

# Podemos operar en ellos por ejemplo para *actualizar* la denominación de la "Val d'Aran" por Aran
library(tidyverse)
poligons@data$nom_comar <- as.character(poligons@data$nom_comar)
poligons@data$nom_comar[poligons@data$nom_comar=="Val d'Aran"] <- "Aran"
poligons@data$nom_comar <- sort(poligons@data$nom_comar)
poligons@data$nom_comar <- as.factor(poligons@data$nom_comar)


# Cargamos los datos que queremos representar
load(file = "data/dades.RData")


### Creamos una columna en los datos comun con los polígonos
dades$id <- poligons@data$comarca


# Ahora podemos juntos los dos objetos
mapa <- merge(poligons, dades, by.x = "comarca", by.y= "id")

# Dibujamos el mapa
library(tmap)
mp1 <- tm_shape(mapa) + 
  tm_fill("2012", # Columna a pintar
          palette = "-RdYlGn")+
  tm_layout(frame = FALSE)+
  tm_style("white")

mp1
tmap_save(mp1, filename = "images/2012.png")

# ...però cuando queremos comparar este mapa con el del año 2014...
# Creamos el mapa 2014
mp2 <- tm_shape(mapa) + 
    tm_fill("2014", # Columna a pintar
          palette = "-RdYlGn")+
  tm_layout(frame = FALSE)+
tmap_save(mp2, filename = "images/2014.png")

# Los ploteamos uno junto al lado del otro
library(grid)
library(gridExtra)
library(ggplot2)

png(filename = "images/Mp1Mp2.png")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(mp1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(mp2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
dev.off()

# Los valores y colores de la escala nos distorcionan poder comparar los mapas. 
#Asignamos los rangos y *fijamos* el estilo para cada año

map1 <- tm_shape(mapa) + 
  tm_fill("2012", 
          breaks = c(25, 50, 75, 100, 125, 150, 175, 200), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

map2 <-tm_shape(mapa) + 
  tm_fill("2013", breaks = c(25, 50, 75, 100, 125, 150, 175, 200), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

map3 <- tm_shape(mapa) + 
  tm_fill("2014", breaks = c(25, 50, 75, 100, 125, 150, 175, 200), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE, legend.outside=TRUE, legend.outside.position="bottom")





png(filename = "images/Map1Map2Map3.png")
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,3)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))

dev.off()

### PER A LA LLEGENDA
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))grid_arrange_shared_legend(map1, map2, map3, map4, map5)