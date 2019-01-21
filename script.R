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
load(file = "data/dades.Rdata")

### Creamos una columna en los datos comun con los polígonos
dades$id <- poligons@data$comarca


# Ahora podemos juntos los dos objetos
mapa <- merge(poligons, dades, by.x = "comarca", by.y= "id")


library(tmap)
tm_shape(mapa) + 
  tm_fill("2012", breaks = c(50, 100,150, 200, 250, 300, 350, 400, 450,500), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

map2 <-tm_shape(mapa) + 
  tm_fill("2013", breaks = c(50, 100,150, 200, 250, 300, 350, 400, 450,500), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

map3 <-tm_shape(mapa) + 
  tm_fill("2014", breaks = c(50, 100,150, 200, 250, 300, 350, 400, 450,500), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

map4 <-tm_shape(mapa) + 
  tm_fill("2015", breaks = c(50, 100,150, 200, 250, 300, 350, 400, 450,500), palette = "-RdYlGn", style = "fixed")+
  tm_layout(legend.show = FALSE, frame = FALSE)

tm_shape(mapa) + 
  tm_fill("2016", breaks = c(50, 100,150, 200, 250, 300, 350, 400, 450,500), palette = "-RdYlGn", style = "fixed", title)+
  tm_layout(title = "Expedients x 10.000 habitants",frame = FALSE, legend.outside = TRUE)+
  tm_legend(... = FALSE)

# 


library(grid)
library(gridExtra)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,3)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(map4, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map5, vp=viewport(layout.pos.col = 2, layout.pos.row =2))


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