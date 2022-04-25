##########
#
# LECTURA DE DATOS 
#
##########

dades = read.csv("/home/jovyan/work/11. TrabajoGrupalMD008y009/01. Data/baseClean.csv", sep = ";")

###################################################################################
####################################################################################
# Analisis Bivariante - 2 vs 2
####################################################################################
###################################################################################

summary(dades$WEAPON)
summary(dades$LOCATION)
Summary(dades$VICOCCUP)
summary(dades$ACCUOCCU)

K<-dim(dades)[2]
par(ask=TRUE)

P<-c2
nc<-length(levels(as.factor(P)))
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
      print(paste("An?lisi per classes de la Variable:", names(dades)[k]))

      boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
      
      barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
      abline(h=mean(dades[[k]]))
      legend(0,mean(dades[[k]]),"global mean",bty="n")
      print("Estad?stics per groups:")
      for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
      o<-oneway.test(dades[,k]~P)
      print(paste("p-valueANOVA:", o$p.value))
      kw<-kruskal.test(dades[,k]~P)
      print(paste("p-value Kruskal-Wallis:", kw$p.value))
      pvalk[,k]<-ValorTestXnum(dades[,k], P)
      print("p-values ValorsTest: ")
      print(pvalk[,k])      
      }else{
   #qualitatives
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
   #   print("Cross-table")
   #   print(table)
      rowperc<-prop.table(table,1)

   colperc<-prop.table(table,2)
   #  print("Distribucions condicionades a files")
   # print(rowperc)
   
   marg <- table(as.factor(P))/n
   print(append("Categories=",levels(dades[,k])))
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }

   #with legend
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   
   #condicionades a classes
   print(append("Categories=",levels(dades[,k])))
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   
   #with legend
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   
   #amb variable en eix d'abcisses
   marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }

      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
   #condicionades a columna 
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   paleta<-rainbow(length(levels(as.factor(P))))
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
   
   #with legend
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
   legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)

      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )

      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
   #diagrames de barres adosades
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )

      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
   
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
   
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
   }
}#endfor

∫
###################################################################################
####################################################################################
# FIN - Analisis Bivariante - 2 vs 2
####################################################################################
###################################################################################

###################################################################################
####################################################################################
# Test Chi/Anova por pantalla
####################################################################################
###################################################################################

# Chi cuadrado

variable<- names(base)

#comenzamos viendo que variables son numericas y cuales son de caracteres
vecchar<- c()
vecnum<- c()

for(i in 1:length(variable)){

  if(is.character(base[,i][[1]])){
    vecchar<- c(vecchar,i)
  }else{
    vecnum<- c(vecnum,i)
  }
  
}
v2<- vecnum
vecnum<- vecnum[2:5]


for (i in 1:length(colnames(variable[vecchar])))
{
    print(paste0("Chi^2 para la variable: ", colnames(dades)[i]))
    print(table(dades[,i],dades[,'Variable_principal']))
    print(chisq.test(table(dades[,i],dades[,'Variable_principal'])))
}


# ANOVA 

for (i in 1:length(colnames(variable[vecnum])))
{
    anova <- aov(var_con[,i] ~ Variable_principal, data = dades)
    print(paste0("ANOVA para la variable: ", colnames(var_con)[i]))
    print(summary(anova))
}


###################################################################################
####################################################################################
# Test Chi/Anova por pantalla
####################################################################################
###################################################################################