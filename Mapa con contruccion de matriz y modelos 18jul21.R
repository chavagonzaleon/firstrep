

#install.packages(c("rgdal","rgeos","zoo","rasterVis","dismo","aspace","maptools","rgbif"))
#install.packages(c("RColorBrewer","latticeExtra"))
#install.packages("viridisLite")
#install.packages("hexbin")splancs
#install.packages("splancs")
#install.packages("Formula")
#install.packages("ggplot2")
#install.packages("gtable")
#install.packages("scales")
#install.packages("munsell")
#install.packages("colorspace")
#install.packages("glue")
#install.packages("ellipsis")
#install.packages("vctrs")
#install.packages("Hmisc")base64enc
#install.packages("base64enc")
#

                 #,"zoo","rasterVis","dismo","aspace","maptools","rgbif"))

### crear rasters e imagenes además modelar con prec y temp ademas de altitud.
library(sp)
library(sf)

library(raster)
library(maps)

library(rgdal)
library(rgeos)
library(zoo)
library(rasterVis)
#library(gtools)
#library(maxent)
library(dismo)
#library(rJava)
library(maptools)
library(spatstat)

#install.packages("rgbif",dependencies=T)
#install.packages("geoaxe",dependencies=T)
#install.packages("lazyeval",dependencies=T)
#install.packages("whisker",dependencies=T)
# "aoi" "httr" xml2
library(rgbif)

#install.packages("htmlTable",dependencies=T)

#install.packages("checkmate",dependencies=T)
#install.packages("backports",dependencies=T)
#install.packages("htmlwidgets’",dependencies=T)
#install.packages("knitr’",dependencies=T)

#paquetes descargados
#htmlTable htmltools xfun rstudioapi shapefiles
library(Hmisc)

#install.packages("shapefiles’",dependencies=T)
#shapefiles

library(aspace)
#library(ggplot2)

#system("defaults write org.R-project.R force.LANG en_US.UTF-8")

###########AQUI EMPIEZAN LOS PUNTOS DEL RISCAL


#Generando archivos con tracks y waypoints, aqui se cargarían datos nuevos i.e. faltan los reclutas de 2021 

archive_tracksGPX<-list.files(c("~/Documents/Posdoc Helechos/GPS info/","~/Documents/Posdoc Helechos/GPS info 2/","~/Documents/Posdoc Helechos/GPS info 3/","~/Documents/Posdoc Helechos/GPS total incompleto"),full.names=T,pattern="Track")

#archive_tracksGPX<-list.files("~/Documents/Posdoc Helechos/GPS info 3/",full.names=T,pattern="Track")

archive_rioGPX<-list.files(c("~/Documents/Posdoc Helechos/GPS info/","~/Documents/Posdoc Helechos/GPS info 2/","~/Documents/Posdoc Helechos/GPS info 3/","~/Documents/Posdoc Helechos/GPS total incompleto/"),full.names=T,pattern="RIO")

archive_tracksPLOTS<-list.files(c("~/Documents/Posdoc Helechos/GPS info/","~/Documents/Posdoc Helechos/GPS info 2/","~/Documents/Posdoc Helechos/GPS info 3/","~/Documents/Posdoc Helechos/GPS total incompleto/"),full.names=T,pattern="PLOT")

archive_waypointsGPX<-list.files(c("~/Documents/Posdoc Helechos/GPS total incompleto/"),full.names=T,pattern="Waypoints")

#CREANDO UNA FUNCION PARA SACAR LOS ARCHIVOS TIPO GPX DE UN COMPILADO lo saque de internet
listofgps<-function(filesgps,layers){
xall<-list()
for(i in 1:length(filesgps)){ 
xall[[i]]<-readOGR(filesgps[i], layer=layers)
}
return(xall)
}

#### esto tarda
listtracks<-listofgps(archive_tracksGPX,"tracks")
listwaypoints<-listofgps(archive_waypointsGPX,"waypoints")
listrio<-listofgps(archive_rioGPX,"tracks")
listplots<-listofgps(archive_tracksPLOTS,"tracks")

############ aqui los guardaré para no tener que sacarlos con la funcion , ya qye tarda mucho.
list.save(listtracks,file="~/Documents/listtracks.RData")
save(listwaypoints,file="~/Documents/listwaypoints.RData")
save(listrio,file="~/Documents/listrio.RData")
save(listplots,file="~/Documents/listplots.RData")


############ los cargamoooos
load("~/Documents/listtracks.RData")
load("~/Documents/listwaypoints.RData")
load("~/Documents/listrio.RData")
load("~/Documents/listplots.RData")

##########

#COMBINANDO LOS ELEMENTOS DE LA LISTA EN UN SOLO ARCHIVO SPATIAL
#DF= dataframe
ALLwaypoints<-do.call(bind,listwaypoints)
DFwaypoints<-data.frame(ALLwaypoints)
head(DFwaypoints)
DFwaypoints$name[1:500]
DFwaypoints$name[500:1500]

ALLtracks<-do.call(bind,listtracks)
DFtracks<-data.frame(ALLtracks)

RIO<-do.call(bind,listrio)

ALLplots<-do.call(bind,listplots)
DFplots<-data.frame(ALLplots)


plot(ALLplots,axes=T,main="plot tracks")
abline(v=-96.995,col=2)
abline(h=19.479,col=2)



##### 
names(ALLplots)
ALLplots$name

#sacando los puntos de los tracks  esto espero sirva para medir el area muestreada.
datcoordi<-0
for(i in 1:107){
datcoord<-data.frame(coordinates(ALLtracks[i,]))
datcoordi<-rbind(datcoordi,datcoord)
}
pointstracks<-datcoordi[-1,]

riscalY<-which(pointstracks[,2]<19.483 & pointstracks[,2]>19.479)

pointstracks2<-pointstracks[riscalY,]
    
riscalX<-which(pointstracks2[,1] > -97 & pointstracks2[,1]< - 96.995 )
  
pointstracks<-pointstracks2[riscalX,]

ponintsarea<-pointstracks 
    
plot(ponintsarea,pch=20,cex=1,col=rgb(1,0,0,.01),xlim=c(-97.001,-96.995),ylim=c(19.4795,19.483),main="El Riscal Tracks",axes=F,xlab="",ylab="")

axis(1,cex.axis=.7)
axis(2,cex.axis=.7,las=1)

points(ALLwaypoints,cex=1,col=rgb(0,1,0,.2),pch=20)
points(ALLwaypoints,cex=.1,col=rgb(0,0,0,.5),pch=20)

#PARA PROYECTAR LA INFO EN UNA capa de altitud tenemos que poner la misma proyeccion

CRS.new <- CRS("+proj=utm +zone=14 +ellps=GRS80 +units=m +no_defs")
CRSlambertwgs84<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

proj4string(ALLwaypoints)

coordsPointsArea<-coordinates(ponintsarea)
class(coordsPointsArea)
#proj4string(riscal)<-CRSlambertwgs84
ndat<-length(ponintsarea[,1])
dat<-data.frame(rep(0,ndat))

Area.puntos<-SpatialPointsDataFrame(ponintsarea,data = dat ,  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(Area.puntos,pch=".")
axis(1)
axis(2)

library("GISTools")
Area.puntos.new<-spTransform(Area.puntos, CRS.new)
plot(Area.puntos.new,axes=T,add=F,col=4,pch=".")

ALLwaypoints.new<-spTransform(ALLwaypoints, CRS.new)
points(ALLwaypoints.new,col=3,cex=5,pch=".")

dat<-data.frame(rep(0,length(ALLwaypoints)))

Waypoints<-SpatialPointsDataFrame(coordinates(ALLwaypoints),data = dat ,  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Waypoints.new<-spTransform(Waypoints, CRS.new)

plot(Waypoints.new)

#por aqui hacemos el kernel de los tracks mas adelante haremos el de los waypoints
data(newhaven)
breach.dens = kde.points(breach,lims=tracts)
level.plot(breach.dens)

PArea<-kde.points(Area.puntos.new,h=30,n=100)
level.plot(PArea)
plot(PArea)
names(PArea)
sum(PArea$kde)
PAreaRas<-raster(PArea)

values(PAreaRas)<-as.numeric(values(PAreaRas)>2e-5)

dev.off()
plot(PAreaRas)
points(Waypoints.new,pch=20,col=rgb(0,0,0,.2))

polygon<-rasterToPolygons(PAreaRas,function(x){x>3e-5&x<8e-5})

plot(polygon,main=c("El Area de los tracks puede servir para estimar la superficie"," mas adelante checaremos con los waypoints"))
gArea(polygon)

RIO.new<-spTransform(RIO, CRS.new)
plot(RIO.new,axes=T,add=T,col=4)

ALLwaypoints.new<-spTransform(ALLwaypoints, CRS.new)
points(ALLwaypoints.new,col=3,cex=5,pch=".")

proj4string(ALLwaypoints)
proj4string(ALLwaypoints.new)

ALLtracks.new<-spTransform(ALLtracks, CRS.new)
plot(ALLtracks.new,add=F,col=rgb(1,0,0,.5))

as.data.frame(ALLwaypoints.new)

hist(coordinates(ALLwaypoints.new)[,1],breaks=100)
hist(coordinates(ALLwaypoints)[,2],breaks=100)

#xlim=c(19.44,19.55)

##la info de los way points del riscal
nriscal.new<-which(coordinates(ALLwaypoints.new)[,1] > 709000&coordinates(ALLwaypoints.new)[,1]< 710400)

nriscalY<-which(coordinates(ALLwaypoints)[,2]>19.47&coordinates(ALLwaypoints)[,2]<19.49)

nriscalX<-which(coordinates(ALLwaypoints)[,1] > -97.005&coordinates(ALLwaypoints)[,1]< -96.99)

#sin transformacion 
plot(nriscalX,nriscalY)
length(nriscalY)
length(nriscalX)

nriscal<-nriscalX

plot(nriscal,nriscalY,col=2)

length(nriscal)
length(nriscalY)


which(ALLwaypoints$name[nriscal]=="A377")

library(raster)
library(rgdal)
library(sf)
library(sp)

pts.riscal<-coordinates(ALLwaypoints.new[nriscal,])

plot(pts.riscal,pch=20,cex=.1,col=rgb(0,1,0,.3),xlim=c(),main="El Riscal Tracks",axes=T,xlab="",ylab="")

dataAf2020<-read.table("~/Documents/A.firma censo 2020 plantilla .csv", sep=",",header=TRUE,row.names=NULL)


dataAf2019<-read.table("~/Documents/A.firma censo 2019 plantilla.csv", sep=",",header=TRUE,row.names=NULL)

###############################



#######################
ALLwaypoints.new$name

#dataGPS2021<-as.data.frame(ALLwaypoints.new,coordinates(ALLwaypoints))
#write.csv(dataGPS2021,"~/Documents/Posdoc Helechos/GPSDATA_contodo2020.csv")
########

#######CAPA DE ELEVACIONES 1:20000
layers<-ogrListLayers("~/Downloads/702825550660_s")
summary(layers)

ogrListLayers("~/Downloads/702825550660_s")

ogrInfo("~/Downloads/702825550660_s")
file.exists("~/spatial files/702825550660_s/curva_nivel20_l.dbf")

elev1<-readOGR(dsn=path.expand("~/spatial files/702825550660_s"),layer="curva_nivel20_l",encoding="ESRI Shapefile",require_geomType="wkbLineString")

file.exists("~/spatial files/702825550660_s/salto_agua20_p.shp")

elev2<-st_read("~/spatial files/702825550660_s",layer="curva_nivel20_l")

x1<-st_read("~/spatial files/702825550660_s")

x2<-st_read(dsn="~/Downloads/702825550660_s",layer="cuerpo_agua20_a")
x3<-st_read(dsn="~/Downloads/702825550660_s",layer="referencia_g20_p")
x4<-st_read(dsn="~/Downloads/702825550660_s",layer="estruc_eleva20_p")
x5<-st_read(dsn="~/Downloads/702825550660_s",layer="manantial20_p")
x6<-st_read(dsn="~/Downloads/702825550660_s",layer="corriente_ag20_l")


#elev1<-readOGR("~/Downloads/702825550660_s",layer=layers[33],require_geomType="wkbMultiPoint")
#agua1<-readOGR("~/Downloads/702825550660_s",layer=layers[14])
#corrients<-readOGR("~/Downloads/702825550660_s")
#library(sf)
summary(elev2)
dim(elev1)

summary(agua1)

elevras<-raster(elev1)

plot(elev1,col=rgb(0,0,0,.1),xlim=c(710000,720000),add=F)
#scalebar(10000)
scalebar(10000,label=c("10 kilometers"), xy=c(710000, 2144000), type='line', divs=1)
#plot(x1,col=rgb(1,0,1,1),xlim=c(710000,720000),add=T)
#plot(x2,col=rgb(1,0,0,1),xlim=c(710000,720000),add=T)
#plot(x6,col=rgb(1,0,0,1),xlim=c(710000,720000),add=F)

axis(1,cex.axis=.5)
axis(2,cex.axis=.5,las=1)

extent(elev1)
abline(v=709896.9,col=2,lwd=1)
#riscalelev<-crop(elev1,extent(709900,710400,2155100,2155700))

points(709918.3,2155500,col=2,pch=20)


### super importante para el mapa o los mapas finales
riscalelev<-crop(elev1,extent(709900,710400,2155100,2155500))
extent(riscalelev)
#par(fig = c(0,1,0,1))
#dev.off()
plot(riscalelev,add=T,col=(rainbow(10)),main="")

plot(riscalelev,add=F,col=(gray.colors(10)),main="")
axis(1,cex.axis=.7)
axis(2,cex.axis=.7,las=1)
title(c("Alsophila firma","en el Riscal"),font.main=3)

abline(h=c(2155500,2155400),lty=2,col=rgb(0,0,0,.2))

scalebar(100,label=c("100 m"), xy=c(709900, 2155100), type='line', divs=1)

#plot(ALLtracks.new[-1*plotsmessy,],axes=T,add=T,col=rgb(1,.3,0,.2))
#plot(ALLtracks.new,axes=T,add=T,col=rgb(0,0,0,.1))

plot(RIO.new,axes=T,add=T,col=rgb(0,.9,1,1),lwd=.8)
#plot(RIO.new,axes=T,add=T,col=rgb(0,0,1,1),lwd=1)


cbind(colnames(dataAf2019),colnames(dataAf2020))


tronco2019<-dataAf2019$tronco

troncostrue<-which(tronco2019>0)
which(tronco2019==0)
table(tronco2019)
#plot(table(tronco2019))
tronco2019*0
tronco2019[157]<-6
tronco2019[363]<-1
#plot((table(log(tronco2019))),typ="p",lwd=.5)

tronco2019[140]

T_5cm_menor<-which(tronco2019<6)
T_6cm_15cm<-which(tronco2019<16&tronco2019>5)
T_16cm_50cm<-which(tronco2019<51&tronco2019>15)
T_51cm_200cm<-which(tronco2019<201&tronco2019>50)
T_201cm_5m<-which(tronco2019<501&tronco2019>200)
T_mayor5m<-which(tronco2019>500)

CatRows<-list(T_5cm_menor,T_6cm_15cm,T_16cm_50cm,T_51cm_200cm,T_201cm_5m,T_mayor5m)

lapply(list(T_5cm_menor,T_6cm_15cm,T_16cm_50cm,T_51cm_200cm,T_201cm_5m,T_mayor5m),length)


coordAf2019<-cbind(dataAf2019$long,dataAf2019$lat)

#dev.off()

library(tmap)



######### funcion para jitter 


jitty<-function(coordsXY){
 jitlong=0
  jitlat=0
  for(i in 1:length(coordsXY[,1])){

    jitX<-rnorm(1,mean=coordsXY[,1][i],sd=1)

  jitY<-rnorm(1,mean=coordsXY[,2][i],sd=1)

  jitlong<-c(jitlong,jitX)
    jitlat<-c(jitlat,jitY)

  }
  
return(cbind(jitlong,jitlat)[-1,])
}
####### termina funcion
# usamos funcion
coordsXY<-coordAf2019[T_5cm_menor,]
coordjitted<-jitty(coordsXY)

text(coordjitted,dataAf2019$Individuo[T_5cm_menor],cex=.3,col=rgb(0,0,0,.4))




#xmin(ALLwaypoints.new)
#points(710020,2155310,cex=3,col=rgb(1,0,0,1),pch=20)
#points(710260,2155320,cex=2,col=rgb(0,0,0,1),pch=20)

#ALLwaypoints.new$name

#####
library(prettymapr)
addnortharrow(pos="topright",scale=.5,padin=c(.5,.2),lwd=.5)
#lines(c(1552000,1553150),c(777500,777500),lty=1,lwd=1)
#text(1552500,778000,c("1 km"),cex=.5)

############# aqui iria loop para todas las clases

pdf("~/Documents/Mapa A.firma categorias",width=12,height=12)

for(i in 1:6){
rowsplot<-unlist(CatRows[i])

plot(riscalelev,add=F,col=(gray.colors(10)),main="",xlim=c(709900,710400))
text(rep(710370,5),c(2155180,2155215,2155240,2155260,2155290),c("1530 msnm","1540 msnm","1550 msnm","1560 msnm","1570 msnm"),cex=.42,font=3,pos=4)
axis(1,cex.axis=.7)
axis(2,cex.axis=.7,las=1)
title(c("Alsophila firma en el Riscal",cbind("Categoria",i)),font.main=3)
clip(709900, 710400, 2155100, 2155500)
abline(h=seq(2155100,2155500,by=10),lty=3,col=rgb(0,0,0,.1),lwd=.5)
abline(v=seq(709900,710400,by=10),lty=3,col=rgb(0,0,0,.1),lwd=.5)

abline(h=c(2155600,2155500,2155400,2155300,2155200,2155100),lty=2,col=rgb(0,0,0,.2))
abline(v=c(709800,709900,710000,710100,710200,710300,710400,710500),lty=2,col=rgb(0,0,0,.2))

#plot(riscalelev,add=T,col=(gray.colors(10)),main="")
plot(RIO.new,axes=T,add=T,col=rgb(0,.9,1,1),lwd=.8)
#plot(RIO.new,axes=T,add=T,col=rgb(0,0,1,1),lwd=1)

points(coordAf2019[rowsplot,],cex=tronco2019[rowsplot]/max(tronco2019[rowsplot]),col=rgb(0,1,0,.5),pch=19)
points(coordAf2019[rowsplot,],cex=0.05,col=rgb(0,0,0,.5),pch=19)

coordsXY<-coordAf2019[rowsplot,]
coordjitted<-jitty(coordsXY)

text(coordjitted,dataAf2019$Individuo[rowsplot],cex=.3,col=rgb(0,0,0,.4))

scalebar(100,label=c("100 m"), xy=c(709900, 2155150), type='line', divs=1,cex=.5)
addnortharrow(pos="topright",scale=.3,padin=c(.5,.2),lwd=.5)

}

text(dataAf2019[140,c(5,6)],dataAf2019$Individuo[140],cex=.3,col=rgb(1,0,0))

dev.off()

plot(c(1,2,3,4,5,6),c(5,15,70,200,500,900),typ="h",lwd=7,col="darkgreen",axes=F,xlab= "Categoria",ylab="Centimetros")
axis(1)
axis(2,cex.axis=.7,label=c(5,15,50,200,500,"> 500"), at=c(-5,35,80,220,500,800),las=1)

############# analisis del crecimiento del tronco

dim(dataAf2020)
dim(dataAf2019)

dataAf2020B<-(dataAf2020)[,1:11]


names(dataAf2019)
names(dataAf2020B)

ID<-dataAf2019$Individuo
dataAf2020$Individuo

cbind(dataAf2019$Individuo[-182],dataAf2020$Individuo[1:371])

h2019<-dataAf2019$h.tallo[-182]
h2020<-dataAf2020$h.referencia[1:371]

plot(h2019,h2020)

Tronco2019<-dataAf2019$Tronco.T[-182]
Tronco2019<-Tronco2019-h2019
hist(Tronco2019)
Tronco2019[157]<-1

Tronco2020<-dataAf2020$Tronco.T.cm.[1:371]
Tronco2020<-Tronco2020-h2020
hist(Tronco2020)

cbind(Tronco2019,Tronco2020)
plot(Tronco2019,Tronco2020,typ="n")
text(Tronco2019,Tronco2020,ID,cex=.5)
abline(a=0,b=1,col=rgb(0,0,0,.5),lty=2)
model<-lm(Tronco2020~Tronco2019)
summary(model)
abline(model,col=rgb(1,0,0,.5))

Deltah<-(Tronco2020)-(Tronco2019)
max(Deltah,na.rm=T)

CrecTronco<-log(Tronco2020)-log(Tronco2019)


mean(CrecTronco,na.rm=T)
exp(mean(CrecTronco,na.rm=T))
exp(max(CrecTronco,na.rm=T))
plot(log(Tronco2019),(CrecTronco),typ="n",)
text(log(Tronco2019),(CrecTronco),ID,cex=.5)
abline(v=log(c(5,15,50,200,500)),col=3)

text(log(c(2,8,30,100,300,800)),rep(-2,5),c("cat 1","cat 2","cat 3","cat 4","cat 5","cat 6"),cex=1,col=3,font=4)



################## comienza el mapa para campo calculando cuadrantes

riscalelev

ALLwaypoints$name[nfuera1]
ALLwaypoints$name[nfuera2]


corteY<-seq(from=min(coordinates(ALLwaypoints.new)[c(nfuera1,nfuera2),2]),to=max(coordinates(ALLwaypoints.new)[c(nfuera1,nfuera2),2]),length.out=3)

abline(h=c(corteY))

seq(from=min(coordinates(ALLwaypoints.new)[,1]),to=max(coordinates(ALLwaypoints.new)[,1]),length.out=9)




##################

#par(new=T)
#par(fig=c(0,1,0,1))
#par(mfrow=c(1,1))
#par(mar=c(1,1,1,1),oma=c(15,14,1,1))
#plot(1,1,typ="l",axes=F)

#par(fig = c(0.06,0.37, 0.2, .8), new = T)  
hist(dataAf[,3],main="",cex.main=.8,cex.axis=.6,las=1,ylab="",xlab="",xaxt="n",col=rgb(0,1,0,1),box="white",breaks=20,col.axis=1,font.axis=2)

axis(1,cex.axis=.6,pos=2,tick=F,col.axis=1,font.axis=2,las=1)

#dev.off()

#x<- rnorm(100)  
#y <- rbinom(100, 1, 0.5)
#par(fig = c(0,1,0,1))
#hist(x)  
#par(fig = c(0.07,0.5, 0.5, 1), new = T)  
#boxplot(x ~ y)  


extentinfo<-as.vector(extent(riscalelev))
extentinfo[1]<-709880
rastere<-raster(extent(extentinfo),res=10)#raster extension es el  raster marco

elevraster<-rasterize(riscalelev,rastere)# algo mal no funciona

plot(elevraster)


#for (i in 1:4){ 
  infop<-rbind( data.frame(coordinates(RIO.new)[[1]]),data.frame(coordinates(RIO.new)[[2]]),data.frame(coordinates(RIO.new)[[3]]),data.frame(coordinates(RIO.new)[[4]]),data.frame(coordinates(RIO.new)[[5]]))
  
 # }


infop


distance<-distanceFromPoints(rastere,infop)
#dev.off()
plot(distance,main="distance to river",add=F)
points(infop,pch=".",col=rgb(0,0,1,1))
#points(ALLwaypoints.new,cex=1,col=rgb(0,1,0,.4),pch=20)
points(dataAf[,c(4,5)],cex=.1,col=rgb(1,0,0,.5),pch=20)

riscalelev

plot(elevraster,add=T)
elevpoints<-as(riscalelev, "SpatialPointsDataFrame")
points(elevpoints,pch=".")

#modelin with gstats empieza ejemplo para aprender
#modelaremos la capa topografica que antes era shapefile ahora es set de putos
library(gstat)
library(rgdal)
library(tmap)
library(spatstat)
library(maptools)

#####
subset(riscalelev =="ContourLine") 

values(elevpoints)
variogram(elevpoints)
distanceras<-distance
values(distanceras)<-0
variogram(distance)

th<-as(dirichlet(as.ppp(elevpoints)), "SpatialPolygons")
plot(th,add=F)

proj4string(th) <- proj4string(elevpoints)

crs(distanceras)<-"+proj=utm +zone=14 +ellps=GRS80 +units=m +no_defs"

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z    <- over(th, elevpoints, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

plot(th.spdf)
# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(elevpoints, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object


proj4string(grd) <- proj4string(elevpoints)


# Interpolate the grid cells using a power value of 2 (idp=2.0)
#model.idw <- idw(altura ~ 1,elevpoints, newdata=grd, idp=2.0)

#2,10

modelgstat<-gstat(formula=altura~1,locations=elevpoints ,set=list(idp=2),nmax=10)

elevraster<-interpolate(distanceras,modelgstat)

plot(elevraster,main="2,10")

points(elevpoints,main="1",pch=".")

#library(raster)
# Derive derive additional terrain vars from DEM using terrain fn.
derived_vars <- terrain(elevraster, opt = c('slope', 'roughness', 'aspect'))
slope <- derived_vars[["slope"]]
roughness <- derived_vars[["roughness"]]
aspect <- derived_vars[["aspect"]]

plot(derived_vars[[2]])

points(ALLwaypoints.new,cex=.1,col=rgb(0,0,0,.5),pch=20)

##############################################################3





#############################################################3

#writeRaster(derived_vars[[2]], "~/Documents/Posdoc Helechos/raster_pendiente_riscal")
#writeRaster(elevraster, "~/Documents/Posdoc Helechos/raster_elev_riscal")
#writeRaster(distance, "~/Documents/Posdoc Helechos/raster_distrio_riscal")

#
plot_elev<- raster("~/Documents/Posdoc Helechos/raster_elev_riscal.grd")
plot_slope<- raster("~/Documents/Posdoc Helechos/raster_pendiente_riscal.grd")
plot_disrio<- raster("~/Documents/Posdoc Helechos/raster_distrio_riscal.grd")

plot(plot_elev)

colorRampPalette(c("red","blue","gray"))(10)

plot(plot_slope,add=F,col=colorRampPalette(c("gray","gold","firebrick"))(10))

plot(plot_disrio,add=F,col=colorRampPalette(c("gray90","gray20"))(100))

dataAfirma<-read.csv("~/Documents/Censo Demografia 2019 Alsophila tests.csv")

elevgpsAf<-dataAfirma$coords.x1
longAf<-dataAfirma$coords.x2
latAf<-dataAfirma$elev
IndAf<-dataAfirma$Individuo

#plot(ALLwaypoints.new,cex=1,col=rgb(0,0,0,.5),pch=20)

points(data.frame(longAf,latAf),cex=.5,col=rgb(0,0,0,1),pch=20)

distrioAf<-extract(plot_disrio,data.frame(longAf,latAf))
elevrasterAf<-extract(plot_elev,data.frame(longAf,latAf))
slopeAf<-extract(plot_slope,data.frame(longAf,latAf))


plot(distrioAf,elevgpsAf)
plot(distrioAf,elevrasterAf)
plot(elevgpsAf,elevrasterAf)
plot(distrioAf,slopeAf)
plot(elevgpsAf,slopeAf)

summary(lm(elevgpsAf~elevrasterAf))


TroncoAf<-dataAfirma$Tronco.T
plot(log(distrioAf),log(TroncoAf))
plot((distrioAf),(TroncoAf))
plot((slopeAf),(TroncoAf))

TroncoAf_1<-subset(TroncoAf,distrioAf<=1)
hist(TroncoAf_1,breaks=20,xlim=c(0,1000),ylim=c(0,.02),prob=T)

TroncoAf_5<-subset(TroncoAf,distrioAf<=5 &distrioAf>1 )
hist(TroncoAf_5,breaks=20,xlim=c(0,1000),ylim=c(0,.02),prob=T)

TroncoAf_10<-subset(TroncoAf,distrioAf<=10 &distrioAf>5 )
hist(TroncoAf_10,breaks=20,xlim=c(0,1000),ylim=c(0,.02),prob=T)

TroncoAf_20<-subset(TroncoAf,distrioAf<=20 &distrioAf>10 )
hist(TroncoAf_20,breaks=20,xlim=c(0,1000),ylim=c(0,.02),prob=T)

TroncoAf_60<-subset(TroncoAf,distrioAf<=60 &distrioAf>20 )
hist(TroncoAf_60,breaks=20,xlim=c(0,1000),ylim=c(0,.02),prob=T)

TroncoAf_120<-subset(TroncoAf,distrioAf>60 )
hist(TroncoAf_120,breaks=5,xlim=c(0,1000),ylim=c(0,.02),prob=T)


dataAfirma2<-read.csv("~/Documents/Censo Demografia 2020 Alsophila tests DATOS.csv")

elevgpsAf2<-dataAfirma2$coords.x1
longAf2<-dataAfirma2$coords.x2
latAf2<-dataAfirma2$elev
IndAf2<-dataAfirma2$Individuo

TroncoAf2<-dataAfirma2$Tronco.T

TroncoAf[361:length(TroncoAf2)]<-NA

plot(log(TroncoAf),log(TroncoAf2))
plot((TroncoAf),(TroncoAf2))

which(TroncoAf>0)
Afpoints<-which(TroncoAf2>0)

plot(TroncoAf[Afpoints],TroncoAf2[Afpoints],pch=1)
abline(0,1,col=2)

plot(TroncoAf[Afpoints],TroncoAf2[Afpoints],xlim=c(0,20),ylim=c(0,30))
abline(0,1,col=2)

mean(TroncoAf2-TroncoAf,na.rm=T)

crecTronco<-TroncoAf2-TroncoAf

hist(crecTronco,breaks=100)
hist(TroncoAf2,breaks=100)

#points(data.frame(longAf,latAf),cex=.5,col=rgb(0,0,0,1),pch=20)

plot(data.frame(longAf2[Afpoints],latAf2[Afpoints]),cex=.2,col=rgb(1,0,0,1),pch=20)
length((longAf2[Afpoints]))

N1<-which(distrioAf<=1)

N5<-which(distrioAf<=5 &distrioAf>1)

N10<-which(distrioAf<=10 &distrioAf>5)

N20<-which(distrioAf<=20 &distrioAf>10)

N60<-which(distrioAf<=60 &distrioAf>20)

Nmas<-which(distrioAf>60)

plot(TroncoAf[Afpoints],TroncoAf2[Afpoints],pch=1)
#abline(0,1,col=2)

points(TroncoAf[N1],TroncoAf2[N1],pch=20,cex=.5)
points(TroncoAf[N5],TroncoAf2[N5],pch=20,cex=.5,col=2)
points(TroncoAf[N10],TroncoAf2[N10],pch=20,cex=.5,col=3)
points(TroncoAf[N20],TroncoAf2[N20],pch=20,cex=.5,col=4)
points(TroncoAf[N60],TroncoAf2[N60],pch=20,cex=.5,col="yellow")
points(TroncoAf[Nmas],TroncoAf2[Nmas],pch=20,cex=.5,col=5)


Tronco1<-c(TroncoAf[N1],TroncoAf[N5],TroncoAf[N10],TroncoAf[N20],TroncoAf[N60],TroncoAf[Nmas])
Tronco2<-c(TroncoAf2[N1],TroncoAf2[N5],TroncoAf2[N10],TroncoAf2[N20],TroncoAf2[N60],TroncoAf2[Nmas])

dist_rio_class<-c(rep("1metro",length(TroncoAf[N1])),rep("5metro",length(TroncoAf[N5])),rep("10metro",length(TroncoAf[N10])),rep("20metro",length(TroncoAf[N20])),rep("60metro",length(TroncoAf[N60])),rep("mas60metro",length(TroncoAf[Nmas])))

Norder<-c(N1,N5,N10,N20,N60,Nmas)

data.frame(dist_rio_class,Tronco1,Tronco2)

crecimiento<-Tronco2[Afpoints]-Tronco1[Afpoints]

which(crecimiento< (-50))
which(crecimiento>50)
#c(114,140)

distrioAf
plot(distrioAf[Afpoints][c(-114,-140)],crecimiento[c(-114,-140)],xlab="Dist a rio metros",ylab="Crecimiento cm/año",xlim=c(0,150))
abline(h=0,col="gray",lty=2)

#plot(as.factor(dist_rio_class),log(crecTronco[1:371]))

growth<-(crecimiento[c(-114,-140)])
distoriver<-distrioAf[Afpoints][c(-114,-140)]
ltronco<-TroncoAf[Afpoints][c(-114,-140)]

Nmenosde50<-which(distoriver>50)
Nmasde50<-which(distoriver>50)

mod1<-glm(growth[-Nmenosde50]~distoriver[-Nmenosde50])
anova(mod1,test="Chi")
summary(mod1)      

pred<-predict(mod1,newdata=data.frame(distoriver=seq(1,120,1)),se.fit=T,type="response")


plot(distrioAf[Afpoints][c(-114,-140)],crecimiento[c(-114,-140)],xlab="Dist a rio metros",ylab="Crecimiento cm/año",xlim=c(0,150))
abline(h=0,col="gray",lty=2)
points(seq(1,120,1),pred$fit,col=3,typ="l")
points(seq(1,120,1),pred$fit+pred$se.fit,col=3,typ="l",lty=2)
points(seq(1,120,1),pred$fit-pred$se.fit,col=3,typ="l",lty=2)

mod2<-glm(growth~ltronco)
anova(mod2,test="Chi")
summary(mod2)

plot(growth~ltronco)
abline()


