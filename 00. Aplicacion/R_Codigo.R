##########
#
# LECTURA DE DATOS 
#
##########

library(readxl)

# BasedeDatosBruta <- read_excel("/Volumes/Macintosh HD/Users/fabiancolomavelez/Documents/06. DockerBCK/11. TrabajoGrupalMD008y009/01. Data/BasedeDatosBruta.xlsx")
BasedeDatosBruta <- read_excel("/home/jovyan/work/11. TrabajoGrupalMD008y009/01. Data/BasedeDatosBruta.xlsx")

###################################################################################
####################################################################################
# PRIMER PREPROCESO
####################################################################################
###################################################################################

base <- BasedeDatosBruta[,-1]

head(base)

variable <- names(base)

base$LAND.AREA.km2. <- as.numeric(base$LAND.AREA.km2.)
base$WHITE.ALONE.. <- as.numeric(base$WHITE.ALONE..)
base$OTHER.RACES.. <- as.numeric(base$OTHER.RACES..)
base$HIGH.SCHOOL.GRADUATE..25.. <- as.numeric(base$HIGH.SCHOOL.GRADUATE..25..)


summary(base)

vecchar <- c()
vecnum <- c()

for(i in 1:length(variable)){
  
  if(is.character(base[,i][[1]])){
    
    vecchar <- c(vecchar,i)
    
  }else{
    
    vecnum <- c(vecnum,i)
    
  }
  
}

##########
#
# RECODIFICACIÓN DE LOS VALORES NULOS: VARIABLES NUM - 99
#
##########

for(i in 2:length(vecnum)){
  
  v <- base[, vecnum[i]]
  
  v[v==99] <- NA
  
  base[, vecnum[i]] <- v
  
}

summary(base[,vecnum])

##########
#
# Tratamiento de Valores nulos, Usando la metodologia KNN
#
##########

# variables a tratar: 5 6 11 16

library(class)

compl <- c(4, 22, 23, 24, 25, 26, 27, 28, 29)

attach(base)

# Llenamos los valores nulos segun el vecino mas cercano

variable[vecnum[2]]
aux = base[,-vecnum[2]]

aux1 = aux[!is.na(MONTH),compl]
aux2 = aux[is.na(MONTH),compl]
kk <- knn(aux1,aux2,MONTH[!is.na(MONTH)])
MONTH[is.na(MONTH)] = as.numeric(levels(kk))
base[,vecnum[2]] = MONTH

#variable 2 de 4
compl<- c(compl,vecnum[2])

variable[vecnum[3]]
aux = base[,-vecnum[3]]

aux1 = aux[!is.na(DAY),compl]
aux2 = aux[is.na(DAY),compl]
kk<- knn(aux1,aux2,DAY[!is.na(DAY)])
DAY[is.na(DAY)]= as.numeric(kk)
base[,vecnum[3]]=DAY

#variable 3 de 4
compl <- c(compl,vecnum[3])

variable[vecnum[4]]
aux = base[,-vecnum[4]]

aux1 = aux[!is.na(VICAGE),compl]
aux2 = aux[is.na(VICAGE),compl]
kk <- knn(aux1,aux2,VICAGE[!is.na(VICAGE)])
VICAGE[is.na(VICAGE)] = as.numeric(kk)
base[,vecnum[4]] =VICAGE

#variable 4 de 4
compl<- c(compl,vecnum[4])

variable[vecnum[5]]
aux = base[,-vecnum[5]]

aux1 = aux[!is.na(ACCUAGE),compl]
aux2 = aux[is.na(ACCUAGE),compl]
kk <- knn(aux1,aux2,ACCUAGE[!is.na(ACCUAGE)])
ACCUAGE[is.na(ACCUAGE)] = as.numeric(kk)
base[,vecnum[5]] = ACCUAGE


summary(base[,vecnum[2:5]])

##########
#
# RECODIFICACIÓN DE LOS VALORES NULOS: VARIABLES CAT - UNKNOWN/UNKN
#
##########

base$KILLER[base$KILLER=='UNKN']<-"UNKN.KILLER"
base$VICTIM[base$VICTIM=='UNKN']<-"UNKN.VICTIM"
base$HOUR[base$HOUR=='UNKNOWN']<-"UNKN.HOUR"
base$WEEKDAY[base$WEEKDAY=='UNKNOWN']<-"UNKN.WEEKDAY"
base$VICRACE[base$VICRACE=='UNKNOWN']<-"UNKN.VICRACE"
base$VICSEX[base$VICSEX=='UNKNOWN']<-"UNKN.VICSEX"
base$VICOCCUP[base$VICOCCUP=='UNKNOWN']<-"UNKN.VICOCCUP"
base$VICCOND[base$VICCOND=='UNKNOWN']<-"UNKN.VICCOND"
base$ACCURACE[base$ACCURACE=='UNKNOWN']<-"UNKN.ACCURACE"
base$ACCUSEX[base$ACCUSEX=='UNKNOWN']<-"UNKN.ACCUSEX"
base$ACCUOCCU[base$ACCUOCCU=='UNKNOWN']<-"UNKN.ACCUOCCU"
base$ACCUCOND[base$ACCUCOND=='UNKNOWN']<-"UNKN.ACCUCOND"
base$RELATION[base$RELATION=='UNKNOWN']<-"UNKN.RELATION"
base$CAUSE[base$CAUSE=='UNKNOWN']<-"UNKN.CAUSE"
base$WEAPON[base$WEAPON=='UNKNOWN']<-"UNKN.WEAPON"
base$LOCATION[base$LOCATION=='UNKNOWN']<-"UNKN.LOCATION"

# write.table(base, file = "/Volumes/Macintosh HD/Users/fabiancolomavelez/Documents/06. DockerBCK/11. TrabajoGrupalMD008y009/01. Data/baseClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
write.table(base, file = "/home/jovyan/work/11. TrabajoGrupalMD008y009/01. Data/baseClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

baseclean <- base
Basebruta <- BasedeDatosBruta

rm(aux)
rm(aux1)
rm(aux2)
rm(base)
rm(BasedeDatosBruta)
rm(v)

rm(ACCUAGE)
rm(compl)
rm(DAY)
rm(i)
rm(kk)

rm(MONTH)
rm(VICAGE)


###################################################################################
####################################################################################
# FIN - PRIMER PREPROCESO
####################################################################################
###################################################################################



## table(c2, dd$VICRACE)
## table(c2, dd$ACCURACE)
## table(c2, dd$WEAPON)
## table(dd$ACCURACE, dd$WEAPON)
## table(c2, dd$COUNTY)
## table(dd$OTHER.RACES..)
## table(dd$OTHER.RACES..,dd$COUNTY)
## table(dd$WHITE.ALONE..,dd$COUNTY)
##  .... 

###################################################################################
####################################################################################
# CLUSTER Y FACTORIZACIÓN DE VARIABLES
#################################################################################### # nolint
################################################################################### # nolint

dd <- baseclean


dd$COUNTY <- as.factor(dd$COUNTY)
dd$VICTIM <- as.factor(dd$VICTIM)
dd$KILLER <- as.factor(dd$KILLER)
dd$HOUR <- as.factor(dd$HOUR)
dd$WEEKDAY <- as.factor(dd$WEEKDAY)
dd$VICRACE <- as.factor(dd$VICRACE)
dd$VICSEX <- as.factor(dd$VICSEX)
dd$VICOCCUP <- as.factor(dd$VICOCCUP)
dd$VICCOND <- as.factor(dd$VICCOND)
dd$ACCURACE <- as.factor(dd$ACCURACE)
dd$ACCUSEX <- as.factor(dd$ACCUSEX)
dd$ACCUOCCU <- as.factor(dd$ACCUOCCU)
dd$ACCUCOND <- as.factor(dd$ACCUCOND)
dd$RELATION <- as.factor(dd$RELATION)
dd$CAUSE <- as.factor(dd$CAUSE)
dd$WEAPON <- as.factor(dd$WEAPON)
dd$LOCATION <- as.factor(dd$LOCATION)

library(cluster)

#set a list of numerical var

dcon <- data.frame(dd$YEAR,dd$MONTH,dd$DAY,dd$VICAGE,dd$ACCUAGE,dd$POPULATION,dd$NUM.WOMEN,dd$NUM.MEN,dd$LAND.AREA.km2.,dd$WHITE.ALONE..,dd$OTHER.RACES..,dd$PERCAPITA.MONEY.INCOME..,dd$HIGH.SCHOOL.GRADUATE..25..)

#dissimilarity matrix

actives <- c(1:30)

dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

h1 <- hclust(dissimMatrix,method="ward.D2")  
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/Cluster_Arbol.jpg')
plot(h1)
dev.off()

c2 <- cutree(h1,5)

#class sizes 
table(c2)

###################################################################################
####################################################################################
# FIN - CLUSTER Y FACTORIZACIÓN DE VARIABLES
####################################################################################
###################################################################################

###################################################################################
####################################################################################
# AGRUPACIÓN DE VARIABLES CATEGORICAS - SEGÚN PROFILING
####################################################################################
###################################################################################

dd <- baseclean



#Agrupación de la variable VICOCUP

names(dd[,12])


dd[dd[,12]=="WAITER",12]<-"BAR"
dd[dd[,12]=="BARTENDER",12]<-"BAR"
dd[dd[,12]=="DISHWASHER",12]<-"BAR"
dd[dd[,12]=="BAR OWNER",12]<-"BAR"
dd[dd[,12]=="MINER",12]<-"LABORER"
dd[dd[,12]=="RR WORKER",12]<-"LABORER"
dd[dd[,12]=="HARNESS MAKER",12]<-"LABORER"
dd[dd[,12]=="MECHANIC",12]<-"LABORER"
dd[dd[,12]=="CARPENTER",12]<-"LABORER"
dd[dd[,12]=="FARMER",12]<-"RURAL"
dd[dd[,12]=="RANCHER",12]<-"RURAL"
dd[dd[,12]=="COWBOY",12]<-"GOV"
dd[dd[,12]=="RANCH/FARM HAND",12]<-"RURAL"
dd[dd[,12]=="SHEEP HERDER",12]<-"RURAL"
dd[dd[,12]=="LAWMAN",12]<-"LAWYERS"
dd[dd[,12]=="LAWYER",12]<-"LAWYERS"
dd[dd[,12]=="LEGISLATOR",12]<-"LAWYERS"
dd[dd[,12]=="JUDGE/JUSTICE",12]<-"LAWYERS"
dd[dd[,12]=="PIMP",12]<-"PROSTITUTE"
dd[dd[,12]=="NURSE",12]<-"GOV"
dd[dd[,12]=="DOCTOR",12]<-"GOV"
dd[dd[,12]=="DRUGGIST",12]<-"SERVICE"
dd[dd[,12]=="TEACHER",12]<-"GOV"
dd[dd[,12]=="SAILOR",12]<-"GOV"
dd[dd[,12]=="SOLDIER",12]<-"GOV"
dd[dd[,12]=="OTHER TRADESMAN",12]<-"SERVICE"
dd[dd[,12]=="PEDDLER",12]<-"SERVICE"
dd[dd[,12]=="MERCHANT",12]<-"SERVICE"
dd[dd[,12]=="STORE CLERK",12]<-"SERVICE"
dd[dd[,12]=="BARBER",12]<-"SERVICE"
dd[dd[,12]=="FISHERMAN",12]<-"SERVICE"
dd[dd[,12]=="BLACKSMITH",12]<-"SERVICE"
dd[dd[,12]=="COOK",12]<-"SERVICE"
dd[dd[,12]=="BUTCHER",12]<-"SERVICE"
dd[dd[,12]=="BAR",12]<-"SERVICE"
dd[dd[,12]=="WATCHMAN",12]<-"SERVICE"
dd[dd[,12]=="PROSTITUTE",12]<-"SERVICE"
dd[dd[,12]=="TEAMSTER",12]<-"SERVICE"
dd[dd[,12]=="TAILOR",12]<-"SERVICE"
dd[dd[,12]=="PORTER",12]<-"SERVICE"
dd[dd[,12]=="LAUNDRY",12]<-"SERVICE"
dd[dd[,12]=="HOTEL KEEPER",12]<-"SERVICE"
dd[dd[,12]=="ASYLUM INMATE",12]<-"OTHER"
dd[dd[,12]=="PRISON INMATE",12]<-"OTHER"
dd[dd[,12]=="BOOKKEEPER",12]<-"GOV"
dd[dd[,12]=="DOMESTIC",12]<-"OTHER"
dd[dd[,12]=="HOUSEWIFE",12]<-"OTHER"
dd[dd[,12]=="BOAT HAND",12]<-"GOV"
dd[dd[,12]=="EDITOR/PUBLISHER",12]<-"OTHER"
dd[dd[,12]=="MINISTER",12]<-"GOV"
dd[dd[,12]=="PREACHER",12]<-"GOV"
dd[dd[,12]=="STEAMBOAT RUNNER",12]<-"GOV"
dd[dd[,12]=="GAMBLER",12]<-"OTHER"


#Agrupacion de la variable ACCUOCCUP
names(dd[,17])

dd[dd[,17]=="WAITER",17]<-"BAR"
dd[dd[,17]=="BARTENDER",17]<-"BAR"
dd[dd[,17]=="DISHWASHER",17]<-"BAR"
dd[dd[,17]=="BAR OWNER",17]<-"BAR"
dd[dd[,17]=="MINER",17]<-"LABORER"
dd[dd[,17]=="RR WORKER",17]<-"LABORER"
dd[dd[,17]=="HARNESS MAKER",17]<-"LABORER"
dd[dd[,17]=="MECHANIC",17]<-"LABORER"
dd[dd[,17]=="CARPENTER",17]<-"LABORER"
dd[dd[,17]=="FARMER",17]<-"RURAL"
dd[dd[,17]=="RANCHER",17]<-"RURAL"
dd[dd[,17]=="COWBOY",17]<-"GOV"
dd[dd[,17]=="RANCH/FARM HAND",17]<-"RURAL"
dd[dd[,17]=="SHEEP HERDER",17]<-"RURAL"
dd[dd[,17]=="LAWMAN",17]<-"LAWYERS"
dd[dd[,17]=="LAWYER",17]<-"LAWYERS"
dd[dd[,17]=="JUDGE/JUSTICE",17]<-"LAWYERS"
dd[dd[,17]=="PIMP",17]<-"PROSTITUTE"
dd[dd[,17]=="DOCTOR",17]<-"GOV"
dd[dd[,17]=="SAILOR",17]<-"GOV"
dd[dd[,17]=="SOLDIER",17]<-"GOV"
dd[dd[,17]=="OTHER TRADESMAN",17]<-"SERVICE"
dd[dd[,17]=="PEDDLER",17]<-"SERVICE"
dd[dd[,17]=="MERCHANT",17]<-"SERVICE"
dd[dd[,17]=="STORE CLERK",17]<-"SERVICE"
dd[dd[,17]=="BARBER",17]<-"SERVICE"
dd[dd[,17]=="FISHERMAN",17]<-"SERVICE"
dd[dd[,17]=="BLACKSMITH",17]<-"SERVICE"
dd[dd[,17]=="COOK",17]<-"SERVICE"
dd[dd[,17]=="BUTCHER",17]<-"SERVICE"
dd[dd[,17]=="BAR",17]<-"SERVICE"
dd[dd[,17]=="WATCHMAN",17]<-"SERVICE"
dd[dd[,17]=="PROSTITUTE",17]<-"SERVICE"
dd[dd[,17]=="TEAMSTER",17]<-"SERVICE"
dd[dd[,17]=="TAILOR",17]<-"SERVICE"
dd[dd[,17]=="PORTER",17]<-"SERVICE"
dd[dd[,17]=="LAUNDRY",17]<-"SERVICE"
dd[dd[,17]=="DEALER",17]<-"SERVICE"
dd[dd[,17]=="HOTEL KEEPER",17]<-"SERVICE"
dd[dd[,17]=="ASYLUM INMATE",17]<-"OTHER"
dd[dd[,17]=="PRISON INMATE",17]<-"OTHER"
dd[dd[,17]=="HOUSEWIFE",17]<-"OTHER"
dd[dd[,17]=="BOAT HAND",17]<-"OTHER"
dd[dd[,17]=="EDITOR/PUBLISHER",17]<-"OTHER"
dd[dd[,17]=="STEAMBOAT RUNNER",17]<-"OTHER"
dd[dd[,17]=="GAMBLER",17]<-"OTHER"
dd[dd[,17]=="CLERICAL",17]<-"GOV"
dd[dd[,17]=="STUDENT",17]<-"OTHER"
dd[dd[,17]=="SHOEMAKER",17]<-"SERVICE"
dd[dd[,17]=="GARDENER",17]<-"SERVICE"
dd[dd[,17]=="WELLS FARGO GRD",17]<-"SERVICE"
dd[dd[,17]=="BAKER",17]<-"SERVICE"
dd[dd[,17]=="BOOKKEEPER",17]<-"GOV"

#Agrupacion de la variable LOCATION

names(dd[,22])

dd[dd[,22]=="BRIDGE",22]<-"STREET"
dd[dd[,22]=="PARK",22]<-"STREET"
dd[dd[,22]=="RAILROAD",22]<-"STREET"
dd[dd[,22]=="COUNTRY ROAD",22]<-"STREET"
dd[dd[,22]=="WHARF",22]<-"STREET"
dd[dd[,22]=="CEMETARY",22]<-"GOV"
dd[dd[,22]=="DESERT",22]<-"NATURE"
dd[dd[,22]=="LAKE",22]<-"NATURE"
dd[dd[,22]=="MOUNTAINS",22]<-"NATURE"
dd[dd[,22]=="RIVER",22]<-"NATURE"
dd[dd[,22]=="OCEAN",22]<-"NATURE"
dd[dd[,22]=="WOODS",22]<-"NATURE"
dd[dd[,22]=="BEACH",22]<-"NATURE"
dd[dd[,22]=="INDIAN RESERVE",22]<-"NATURE"
dd[dd[,22]=="FARM",22]<-"RURAL"
dd[dd[,22]=="STABLE OR CORRAL",22]<-"RURAL"
dd[dd[,22]=="CABIN (RURAL)",22]<-"RURAL"
dd[dd[,22]=="RANCH",22]<-"RURAL"
dd[dd[,22]=="BANK",22]<-"GOV"
dd[dd[,22]=="ASYLUM",22]<-"GOV"
dd[dd[,22]=="ROOMING HOUSE",22]<-"HOUSE"
dd[dd[,22]=="SAW MILL",22]<-"OTHER"
dd[dd[,22]=="STORE",22]<-"OTHER"
dd[dd[,22]=="OTHER BUILDING",22]<-"HOUSE"
dd[dd[,22]=="OTHER BUSINESS",22]<-"OTHER"
dd[dd[,22]=="MISSION",22]<-"OTHER"
dd[dd[,22]=="SHIP",22]<-"OTHER"
dd[dd[,22]=="BLACKSMITH SHOP",22]<-"OTHER"
dd[dd[,22]=="OTHER HOUSE",22]<-"OTHER"
dd[dd[,22]=="SALOON",22]<-"RELAX"
dd[dd[,22]=="BROTHEL",22]<-"RELAX"
dd[dd[,22]=="RESTAURANT",22]<-"RELAX"
dd[dd[,22]=="DANCE HALL",22]<-"RELAX"
dd[dd[,22]=="HOTEL",22]<-"RELAX"
dd[dd[,22]=="JAIL",22]<-"GOV"
dd[dd[,22]=="PRISON",22]<-"GOV"
dd[dd[,22]=="COURTHOUSE",22]<-"GOV"
dd[dd[,22]=="STATE BUILDING",22]<-"GOV"
dd[dd[,22]=="MINE",22]<-"GOV"
dd[dd[,22]=="HOSPITAL",22]<-"GOV"
dd[dd[,22]=="VICTIM'S HOME",22]<-"HOUSE"
dd[dd[,22]=="ACCUSED HOME",22]<-"HOUSE"


#Agrupacion de la variable WEAPON

names(dd[,21])

dd[dd[,21]=="GUN UNKNOWN",21]<-"UNKN.WEAPON"
dd[dd[,21]=="POISON",21]<-"DRUGS"
dd[dd[,21]=="RIFLE",21]<-"SHOTGUN"
dd[dd[,21]=="HAND GUN",21]<-"SHOTGUN"
dd[dd[,21]=="AXE",21]<-"KNIFE"
dd[dd[,21]=="KICKED",21]<-"NOWEAPON"
dd[dd[,21]=="STRANGLED",21]<-"NOWEAPON"
dd[dd[,21]=="FISTS",21]<-"NOWEAPON"
dd[dd[,21]=="HANGING",21]<-"NOWEAPON"
dd[dd[,21]=="DROWNED",21]<-"NOWEAPON"
dd[dd[,21]=="THROWN DOWN",21]<-"NOWEAPON"
dd[dd[,21]=="BLUNT INSTRUMENT",21]<-"KNIFE"
dd[dd[,21]=="SHARP INSTRUMENT",21]<-"KNIFE"

dd[dd[,20]=="BRAWL",20]<-"DISPUTE"
dd[dd[,20]=="DOMESTIC DISPUTE",20]<-"DISPUTE"
dd[dd[,20]=="QUARREL",20]<-"DISPUTE"
dd[dd[,20]=="KILLED BY POLICE",20]<-"POLICE"
dd[dd[,20]=="KILLED POLICE",20]<-"POLICE"



# class(dd[,20])

names(dd)

write.table(dd, file = "/Volumes/Macintosh HD/Users/fabiancolomavelez/Documents/06. DockerBCK/11. TrabajoGrupalMD008y009/01. Data/baseClean_Agrupado.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

rm(dd)
###################################################################################
####################################################################################
# FIN - AGRUPACIÓN DE VARIABLES CATEGORICAS - SEGÚN PROFILING
####################################################################################
###################################################################################



###################################################################################
####################################################################################
# Analisis ACP
####################################################################################
###################################################################################

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

# Porcentaje de variabilidad explicada :

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

# Variabilidad acumulada
jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_VarianzaExplicada.jpg')
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
dev.off()



percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

# Seleccionamos 5 componentes, ya que tienen explican un 80% de la variabilidad del set de datos

nd = 5

# guardamos los componentes


Psi = pc1$x[,1:nd]

# Guardamos el nombre de las variables y de los casos.

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # para representar el sentido de las variables

# PLOT OF INDIVIDUALS

#select your axis
eje1<-1
eje2<-2

jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_proyección.jpg')

plot(Psi[,eje1],Psi[,eje2])
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="grey")
axis(side=3, pos= 0, labels = F, col="grey")
axis(side=2, pos= 0, labels = F, col="grey")
axis(side=4, pos= 0, labels = F, col="grey")

dev.off()



#library(rgl)
#plot3d(Psi[,1],Psi[,2],Psi[,3])



#######
#Projection of variables -- Explicar bien esta parte !!!!!!!!!!!!!

Phi = cor(dcon,Psi)

#select your axis

X<-Phi[,eje1]
Y<-Phi[,eje2]

jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_Dirección_1.jpg',width = 720, height = 720, units = "px", pointsize = 10, quality = 100)

plot(Psi[,eje1],Psi[,eje2],type="n", xlim = c(-1.5,1.5), ylim = c(-1.5,1.5))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=1.20)

dev.off()

jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_Dirección_1_Dots.jpg' ,width = 720, height = 720, units = "px", pointsize = 10, quality = 100)

plot(Psi[,eje1],Psi[,eje2], xlim = c(-2.5,4), ylim = c(-2.5,4))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.1,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=1.20)

dev.off()




jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_CON_COUNTRY_.jpg',width = 720, height = 720, units = "px", pointsize = 17, quality = 100)
# PROYECTAREMOS LAS PRINCIPALES VARIABLES EN EL RESULTADO DEL PCA REALIZADO.

plot(Psi[,eje1],Psi[,eje2],pch=19,)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#select your qualitative variable

varcat<-dd$COUNTY
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 

#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="red", cex=1)

dev.off()





# weapon and location vs county
jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_CON_COUNTY_LOCATION_WEAPON.jpg',width = 720, height = 720, units = "px", pointsize = 17, quality = 100)
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
varcat<-dd$COUNTY
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="red", cex=0.6)

varcat<-dd$LOCATION
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="black", cex=0.6)

varcat<-dd$WEAPON
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.6)
dev.off()


#weapon vs relation 
jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_RELACION_ARMA.jpg',width = 720, height = 720, units = "px", pointsize = 17, quality = 100)
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

varcat<-dd$RELATION
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="red", cex=0.6)

varcat<-dd$WEAPON
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.6)
dev.off()

#weapon vs accucond
jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_ARMA_ACU_COND.jpg',width = 720, height = 720, units = "px", pointsize = 17, quality = 100)
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

varcat<-dd$ACCUCOND
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="red", cex=0.6)

varcat<-dd$WEAPON
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.6)
dev.off()

#weapon vs viccond
jpeg('/home/jovyan/work/11. TrabajoGrupalMD008y009/02. Resultados/ACP_arma_vic_cond.jpg',width = 720, height = 720, units = "px", pointsize = 17, quality = 100)
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

varcat<-dd$VICCOND
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="red", cex=0.6)

varcat<-dd$WEAPON
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.6)
dev.off()






###################################################################################
####################################################################################
# FIN - Analisis ACP
####################################################################################
###################################################################################


###################################################################################
####################################################################################
# BIVARIANTE 
####################################################################################
###################################################################################


###################################################################################
####################################################################################
# FIN - BIVARIANTE 
####################################################################################
###################################################################################