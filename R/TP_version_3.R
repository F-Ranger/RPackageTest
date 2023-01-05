install.packages("pdfetch")
install.packages("quantmod")

library("pdfetch")
library("quantmod")

#Extraction

dataFinaExtract <-function(date_debut,date_fin,NomIndice){
  return(pdfetch_YAHOO(NomIndice, from=as.Date(date_debut), to = as.Date(date_fin)))
}

"Du 1er jan 2021 au 1er jan 2022"
DJI <- (dataFinaExtract(NomIndice = "^DJI",date_debut = "2021-12-01",date_fin = "2022-01-01"))
colnames(DJI) <- c("open","high","low","close","adj_close","volume")

#version data frame pour la regression linéaire et logistique

df_DJI <- data.frame((dataFinaExtract(NomIndice = "^DJI",date_debut = "2021-01-01",date_fin = "2022-01-01")))
colnames(df_DJI) <- c("open","high","low","close","adj_close","volume")

#Creer une colonne direction pour le up et down (1=up et 0=down)

dji_open<- df_DJI$open

direction[dji_open> Lag(dji_open,1)] <- 1

direction[dji_open< Lag(dji_open,1)] <- 0

direction <- 1

df_DJI <- cbind(df_DJI,direction)

#Feature_engin

feature_engin <- function(nomInd,annee){
  cbind(
        periodReturn(Cl(nomInd),period='daily',subset=annee),
        periodReturn(Cl(nomInd),period='weekly',subset=annee),
        periodReturn(Cl(nomInd),period='monthly',subset=annee),
        periodReturn(Cl(nomInd),period='yearly',subset=annee),
        
        apply.daily(diff(log(Cl(DJI)))[-1], FUN = sum),
        apply.weekly(diff(log(Cl(DJI)))[-1], FUN = sum),
        apply.monthly(diff(log(Cl(DJI)))[-1], FUN = sum),
        apply.yearly(diff(log(Cl(DJI)))[-1], FUN = sum),
        
        apply.weekly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.weekly(Cl(nomInd),FUN = function(x){min(x)}),
        apply.weekly(Cl(nomInd),FUN = function(x){max(x)}),
        apply.weekly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.weekly(Cl(nomInd),FUN = function(x){median(x)}),
        apply.weekly(Vo(nomInd),FUN = function(x){mean(x)}),
      
        apply.monthly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.monthly(Cl(nomInd),FUN = function(x){min(x)}),
        apply.monthly(Cl(nomInd),FUN = function(x){max(x)}),
        apply.monthly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.monthly(Cl(nomInd),FUN = function(x){median(x)}),
        apply.monthly(Vo(nomInd),FUN = function(x){mean(x)}),
        
        apply.yearly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.yearly(Cl(nomInd),FUN = function(x){min(x)}),
        apply.yearly(Cl(nomInd),FUN = function(x){max(x)}),
        apply.yearly(Cl(nomInd),FUN = function(x){mean(x)}),
        apply.yearly(Cl(nomInd),FUN = function(x){median(x)}),
        apply.yearly(Vo(nomInd),FUN = function(x){mean(x)}))
        
  }

resultat_DJI <- feature_engin(DJI,'2021::')

colnames(resultat_DJI) <- c("Rendement quotidien",
                             "Rendement hebdomadaire",
                              "Rendement mensuel",
                              "Rendement annuel",
                              
                              "log return quotidien",
                              "log return hebdo",
                              "log return mensuel",
                              "log return annuel",
                            
                              "Prix cloture moyen de la semaine",
                              "Prix cloture min de la semaine",
                              "Prix cloture max de la semaine",
                              "Prix cloture moyen de la semaine",
                              "Médiane du prix de cloture de la semaine",
                              "Volume moyen de la semaine",
                              
                              "Prix cloture moyen du mois",
                              "Prix cloture min du mois",
                              "Prix cloture max du mois",
                              "Prix cloture moyen du mois",
                              "Médiane du prix de cloture du mois",
                              "Volume moyen du mois",
                              
                              "Prix cloture moyen de l'année",
                              "Prix cloture min de l'année",
                              "Prix cloture max de l'année",
                              "Prix cloture moyen de l'année",
                              "Médiane du prix de cloture de l'année",
                              "Volume moyen de l'année")

#Modélisation

#regression linéaire

set.seed(3035)
x1<-1:100
s<-sample(x1,252*0.7,replace=T,prob=rep(0.01,100))
s

View(df_DJI)

str(df_DJI)
summary(df_DJI$open)

library(ggplot2)
ggplot(df_DJI, aes(x=close))+
  geom_histogram(color="black",fill="pink",alpha=.5)

ggplot(df_DJI,aes(x =close, y=open)) + geom_point() + geom_smooth(method = "lm")
ggplot(df_DJI,aes(x =close, y=volume)) + geom_point() + geom_smooth(method = "lm")
ggplot(df_DJI,aes(x =high, y=low)) + geom_point() + geom_smooth(method = "lm")
ggplot(df_DJI,aes(x =close, y=low)) + geom_point() + geom_smooth(method = "lm")
ggplot(df_DJI,aes(x =close, y=high)) + geom_point() + geom_smooth(method = "lm")

cor(df_DJI[c("open","high","low","close","adj_close","volume")])

library(psych)
pairs.panels(df_DJI[c("open","high","low","close","adj_close","volume")])

DJI_LM <- lm(close~., data=DJI)
DJI_LM

LogDJI_LM <- lm(log(close)~., data=DJI)
LogDJI_LM

summary(DJI_LM)
summary(LogDJI_LM)
#regression logistique

glm.fits <- glm(direction~open+high+low+close+adj_close+volume,
                data=df_DJI,
                family=binomial)

summary(glm.fits)

glm.probs=predict(glm.fits,type="response")
glm.probs[1:5]

glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(df_DJI)
table(glm.pred,direction)






