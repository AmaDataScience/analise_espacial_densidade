pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando a base de dados
load("bd_cnpj")

# Observando a classe do objeto 'bd_cnpj':
class(bd_cnpj)

# Observando as variaveis da base de dados 'bd_cnpj':
bd_cnpj %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Criando um objeto do tipo sf a partir de 'bd_cnpj':
sf_bd_cnpj <- st_as_sf(x = bd_cnpj, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

# Observando a classe do objeto sf_bd_cnpj:
class(sf_bd_cnpj)

# Plotando o objeto sf_bd_cnpj de forma espacial:
tm_shape(shp = sf_bd_cnpj) + 
  tm_dots(size = 1)

# Adicionando uma camada de um mapa do Leafleet que considere a bounding box do 
# objeto sf_bd_cnpj:
tmap_mode("view")

tm_shape(shp = sf_bd_cnpj) + 
  tm_dots(col = "deepskyblue4", 
          border.col = "black", 
          size = 0.2, 
          alpha = 0.8)

#tmap_mode("plot") #Para desativar as camadas de mapas on-line

# Carregando um shapefile do estado de Sco Paulo
shp_saopaulo <- readOGR("shapefile_estadoSP", "estado_sp")

# Explorando mais profundamente o nosso shapefile do estado de Sco Paulo:
shp_saopaulo@proj4string

# Datum: South American Datum 1969; Projegco: Transverse Mercator; Zona: 23S. 
# Conferir Datum

# Visualizagco grafica do objeto shp_saopaulo:
tm_shape(shp = shp_saopaulo) + 
  tm_borders()

# Combinando o objeto shp_saopaulo com o objeto sf_bd_cnpj:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_bd_cnpj) + 
  tm_dots(col = "cnae", 
          size = 0.2)
# Conferir col = 'cnae'

# O buffering i uma ticnica para se medir distbncias para fora de um dado ponto
# geografico.

# A aplicagco da ticnica de buffering pode ser feita com o uso da fungco 
# gBuffer(), do pacote rgeos:
buffer_shoppings <- gBuffer(spgeom = sf_shoppings,
                            width = 1500,
                            byid = TRUE)

# A fungco gBuffer() nco funciona com objetos do tipo sf. Para utiliza-la,
# precisaremos converter o objeto sf_bd_cnpj para o tipo spatial points (sp).

# Para tanto, primeiramente, precisaremos isolar as coordenadas de longitude e 
# de latitude do data frame original 'bd_cnpj':
coordenadas_bd_cnpj <- cbind(bd_cnpj$longitude,
                             bd_cnpj$latitude)

coordenadas_bd_cnpj

# Depois, utilizaremos a fungco SpatialPoints() para criar um objeto do tipo sp:
sp_bd_cnpj <- SpatialPoints(coords = coordenadas_bd_cnpj,
                              proj4string = CRS("+proj=longlat"))

# Uma plotagem basica:
plot(sp_shoppings)

# A fungco SpatialPoints() nco permite a existencia concomitante de um data 
# SpatialPointsDataFrame() que quebra essa lsgica e permite a existjncia de uma 
# base de dados atrelada a um objeto de classe sp.

tmap_mode("plot")

tm_shape(shp = sp_bd_cnpj) + 
  tm_dots(size = 1)

# Vamos tentar aplicar a fungco gBuffer() mais uma vez:
buffer_bd_cnpj <- gBuffer(spgeom = sp_bd_cnpj,
                            width = 1500,
                            byid = TRUE)

# Dessa vez, o erro foi diferente. Alim de exigir um objeto de classe sp,
# a fungco gBuffer() exige que o objeto se oriente com distbncias euclidianas.
# Nosso atual objeto se orienta de forma geodisica.
bd_cnpj_UTM <- spTransform(x = sp_bd_cnpj,
                             CRSobj = CRS("+init=epsg:22523"))

# Visualizando o resultado:
tm_shape(shp = bd_cnpj_UTM) + 
  tm_dots(size = 1)

# Agora sim, poderemos aplicar a fungco gBuffer():
buffer_bd_cnpj <- gBuffer(spgeom = bd_cnpj_UTM, 
                            width = 1500, 
                            byid = TRUE)

# Plotagem do objeto buffer_bd_cnpj:
tm_shape(shp = buffer_bd_cnpj) + 
  tm_borders()

tmap_mode("view")

tm_shape(shp = buffer_bd_cnpj) + 
  tm_borders()

# Combinando os objetos shp_saopaulo, sf_bd_cnpj e buffer_bd_cnpj:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_bd_cnpj) + 
  tm_dots(col = "regiao", 
          size = 0.02) +
  tm_shape(buffer_bd_cnpj) + 
  tm_borders(col = "black") 

# A ticnica de buffer union combina aqueles outputs da ticnica de buffering que,
# por ventura, se encontrem.
buffer_union <- gUnaryUnion(spgeom = buffer_bd_cnpj)


tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_bd_cnpj) + 
  tm_dots(col = "cnae", 
          size = 0.02) +
  tm_shape(shp = buffer_union) + 
  tm_borders(col = "black") + 
  tm_fill(col = "gray",
          alpha = 0.5) 

# A ticnica de kernel densities calcula a densidade da presenga de pontos de
# interesse em determinada area geografica.

# O primeiro passo sera criar um objeto sp com a base de dados atrelada a ele:
bd_cnpj_sp_df <- SpatialPointsDataFrame(data = bd_cnpj,
                                          coords = coordenadas_bd_cnpj,
                                          proj4string = CRS("+proj=longlat"))


# Note como a fungco SpatialPointsDataFrame() permite a existjncia de um data
# frame junto a nosso objeto de classe sp:
bd_cnpj_sp_df@data

# Para o calculo das kernel densities, podemos utilizar a fungco kernelUD():
bd_cnpj_dens <- kernelUD(xy = bd_cnpj_sp_df,
                           h = "href",
                           grid = 1000,
                           boundary = NULL)

plot(bd_cnpj_dens)

# Para estabelecer as zonas com maior densidade, propomos o seguinte:
zona1 <- getverticeshr(x = bd_cnpj_dens, percent = 20) 
zona2 <- getverticeshr(x = bd_cnpj_dens, percent = 40) 
zona3 <- getverticeshr(x = bd_cnpj_dens, percent = 60) 
zona4 <- getverticeshr(x = bd_cnpj_dens, percent = 80)

tmap_options(check.and.fix = TRUE) 

tm_shape(shp = shp_bd_cnpj) + 
  tm_fill(col = "gray90") + 
  tm_borders(col = "white", alpha = 0.5) + 
  tm_shape(shp = bd_cnpj_sp_df) + 
  tm_dots(col = "regiao", size = 0.25) + 
  tm_shape(zona1) + 
  tm_borders(col = "firebrick4", lwd = 2.5) +
  tm_fill(alpha = 0.4, col = "firebrick4") + 
  tm_shape(zona2) + 
  tm_borders(col = "firebrick3", lwd = 2.5) + 
  tm_fill(alpha = 0.3, col = "firebrick3") + 
  tm_shape(zona3) + 
  tm_borders(col = "firebrick2", lwd = 2.5) + 
  tm_fill(alpha = 0.2, col = "firebrick2") +
  tm_shape(zona4) + 
  tm_borders(col = "firebrick1", lwd = 2.5) + 
  tm_fill(alpha = 0.1, col = "firebrick1")

tmap_mode("plot")

# FIM ---------------------------------------------------------------------
