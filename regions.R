##########Creation de cartes pour les reseaux locaux de services (rls) du Quebec#############
###(Merci a Philippe Hamel pour son aide sur ce projet : https://philippehamel.github.io/)###

##packages necessaires
library(dplyr)
library(ggmap)
library(maptools)
library(rgdal)
library(readxl)
library(scales)
library(pdftools)
library(stringr)
# S'il y a erreur, peut requérir l'installation des packages suivants 
#install.packages("rgeos", type="source")
#install.packages("rgdal", type="source")

##direction de travail
setwd("/Users/nict/Documents/github/Carte_regions")
getwd()


#telechargement des donnees demographiques
ifelse(!file.exists("demographie.pdf"),{
    url <- "http://www.stat.gouv.qc.ca/statistiques/profils/panorama-regions-2016.pdf"
    download.file(url, dest="demographie.pdf", mode="wb")
    "file now loaded"},
    "file already loaded")


#chargement du fichier ".shp" et transformation de dataframe --> ce fichier est 
#disponible au centre GeoStats de la bilbiotheque de l'Universite Laval; ce fichier 
#contient de l'information spatial sur les regions adminsitratives du Quebec
shp <- readOGR(dsn = "./regions_admin", layer = "sda_regio_s_poly")
shape <- fortify(shp, region = "RES_CO_REG")
shape$id <- as.integer(shape$id)


#dataframe permettant d'inserer les numeros et noms de regions a la carte
shp.centroids <- data.frame(long = coordinates(shp)[, 1], 
                            lat = coordinates(shp)[, 2]) 
shp.centroids[, "RES_CO_REG"] <- as.integer(shp@data[,"RES_CO_REG"])
shp.centroids[, "RES_NM_REG"] <- shp@data[,"RES_NM_REG"]

#retrait des doublons de la Cote-Nord
shp.centroids <- shp.centroids[-c(1,2,3,21),]

#ajustement des centres de Laval et Montreal pour une visualisation plus adequate
shp.centroids$lat[shp.centroids$RES_CO_REG==13] <- shp.centroids$lat[shp.centroids$RES_CO_REG==13] + 10000
shp.centroids$long[shp.centroids$RES_CO_REG==13] <- shp.centroids$long[shp.centroids$RES_CO_REG==13] - 19000
shp.centroids$lat[shp.centroids$RES_CO_REG==06] <- shp.centroids$lat[shp.centroids$RES_CO_REG==06] - 15000
shp.centroids$long[shp.centroids$RES_CO_REG==06] <- shp.centroids$long[shp.centroids$RES_CO_REG==06] - 10000


##chargement et traitement du fichier PDF de demographie
texte <- pdf_text("demographie.pdf")
table <- as.data.frame(strsplit(texte[25],"\n"))
table <- as.data.frame(table[c(6,27:43),])

regions <- str_sub(table[,1], start = 1, end = 30)
regions <- as.data.frame(gsub(" ", "", regions))
regions <- regions[-1,]
names(regions) <- "regions.adm"
regions <- gsub("–", "-", regions); regions

age.moyen <- str_sub(table[,1], start= -4)
age.moyen <- as.data.frame(as.numeric(gsub(",", ".", age.moyen)))
age.moyen <- age.moyen[-1,]; age.moyen
names(age.moyen) <- "age.moy"

age.median <- str_sub(table[,1], start = -10, end = -7)
age.median <- as.data.frame(as.numeric(gsub(",", ".", age.median)))
age.median <- age.median[-1,]; age.median
names(age.median) <- "age.med"

prop.65p <- str_sub(table[,1], start = -30, end = -26)
prop.65p <- as.data.frame(as.numeric(gsub(",", ".", prop.65p)))
prop.65p <- prop.65p[-1,]
prop.65p <- prop.65p/100
names(prop.65p) <- "prop.65p"

regions.adm <- cbind.data.frame(num = 1:length(regions), regions, age.moyen, age.median, prop.65p)

#mise en commun des donnees
data <- left_join(x = shape, y = regions.adm,
                  by = c("id" = "num"),
                  copy = TRUE)

#carte des regions administratives
pdf("regions_qc.pdf",width=8.5,height=8)
ggplot() +
    geom_polygon(data = data, aes(x = long, y = lat,
                                  group = group, fill = prop.65p), 
                 color = "black", size = 0.25) +
    scale_fill_distiller(palette = "Blues",
                         labels = percent,
                         direction = 1) +
                         labs(fill = "Proportion") +
    scale_x_continuous(limits = c(min(data$long), max(data$long) + 100000))+
    theme_nothing(legend = T) +
    theme(plot.title = element_text(hjust = 0.5, vjust=-2)) +
    ggtitle("Proportion de la population âgée de 65 ans et plus \npar région administrative (2015)") +
    geom_text(data = shp.centroids,
              aes(label = RES_CO_REG, x = long, y = lat, group = RES_CO_REG), size = 3)+
    annotate("text", x=max(data$long)-350000,
             y=max(data$lat)-43000*(1:17),
             label=paste(unique(data$regions)),
             size=3.5, hjust=0) +
    annotate("text", x=max(data$long)-350000,
             y=max(data$lat)-43000*(1:17),
             label=paste(unique(data$id), " : "),
             size=3.5, hjust=1) 
dev.off()




