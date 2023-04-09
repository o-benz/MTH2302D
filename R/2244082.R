mondata
#On set les variables
sales <- mondata$Sales
price <- mondata$Price
advertising <- mondata$Advertising
region <- mondata$Region

#Phase 1)
#a)
hist(sales, col = "blue", main = "Histogramme des ventes", xlab="Nombre de sièges vendus (en milliers)",ylab="Fréquence")
boxplot(sales, horizontal=T,xlab="Nombre de sièges vendus (en milliers)", ylab = "Distribution", col = "green", main = "Diagramme de Tukey des ventes")
qqnorm(sales, main="Droite de Henry des Ventes", ylab="Quantiles de l'échantillon", xlab="Quantiles Théoriques")
qqline(sales)
shapiro.test(sales)
#Tableau Descriptif 
summary(sales)          
sd(sales)               #Écart-type
t.test(sales)$conf.int  #Intervalle de confiance

#b)
windows()
layout(matrix(1:2, 1, 2))
hist(sales[region=="1"], col="lightblue", main=paste("Urbain"),
     xlab="Nombre de sièges vendus en milliers",ylab="Fréquences")
hist(sales[region=="0"], col="lightgreen",main=paste("Rural"),
     xlab="Nombre de sièges vendus en milliers",ylab="Fréquences")
boxplot(sales ~ region, col=c("lightblue", "lightgreen"), 
        main="Comparaison des ventes par région", xlab="Région", ylab="Ventes (en milliers)")
#Stats Urbain
summary(sales[region == 1])       
var(sales[region==1])                 #Variance
sd(sales[region == 1])                #Écart-type
t.test(sales[region == 1])$conf.int   #Intervalle de confiance
#Stats Urbain
summary(sales[region == 0])       
var(sales[region==0])                 #Variance
sd(sales[region == 0])                #Écart-type
t.test(sales[region == 0])$conf.int   #Intervalle de confiance
#Test d’hypothèses sur l’égalité des variances pour les deux groupes
var.test(sales[region == 0], sales[region == 1])
#Test d’hypothèses sur l’égalité des moyennes pour les deux groupes
t.test(sales[region == 0], sales[region == 1])

#Phase 2: Recherche du meilleur modèle
#c)
#Modèle 1:
#Tableau des coefficients de régression
reg1 <- lm(sales ~ price)
summary(reg1)
#Tableau d'analyse de la variance
anova (reg1)
#Nuages de points
plot (price,sales, main="Nuage de points du modèle 1", xlab="Prix", 
      ylab="Nombre de sièges vendus en milliers", col="red")
abline (reg1, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg1)
#Intervalle de confiance des coefficients de régression
confint (reg1)

#Modèle 2: 
#Tableau des coefficients de régression
reg2 <- lm(log(sales)~log(price))
summary(reg2)
#Tableau d'analyse de la variance
anova (reg2)
#Nuages de points
par (mfrow=c(1,1))
plot (log(price), log(sales), main="Nuage de points du modèle 2", xlab="Prix", 
      ylab="Nombre de sièges vendus en milliers", col="red") 
abline (reg2, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg2, col="purple")
#Intervalle de confiance des coefficients de régression
confint (reg2)

#Modèle 3:
#Tableau des coefficients de régression
reg3 <- lm(log(sales)~price)
summary(reg3)
#Tableau d'analyse de la variance
anova(reg3)
#Nuages de points
par (mfrow=c(1,1))
plot (price, log(sales), main="Nuage de points du modèle 3", xlab="Prix", 
      ylab="Nombre de sièges vendus en milliers", col="red")
abline (reg3, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg3, col="purple")
#Intervalle de confiance des coefficients de régression
confint (reg3)



#Modèle 4: 
#Tableau des coefficients de régression
reg4 <- lm(sales~advertising)
summary(reg4)
#Tableau d'analyse de la variance
anova (reg4)
#Nuages de points
par (mfrow=c(1,1))
plot (advertising,sales, main="Nuage de points du modèle 4", xlab="Publicité", 
      ylab="Nombre de sièges vendus en milliers", col="red")
abline (reg4, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg4, col="purple")
#Intervalle de confiance des coefficients de régression
confint (reg4)

#Modèle 5: 
#Tableau des coefficients de régression
reg5 <- lm(log(sales)~log(8+advertising))
summary(reg5)
#Tableau d'analyse de la variance
anova (reg5)
#Nuages de points
par (mfrow=c(1,1))
plot (log(8+advertising),log(sales), main="Nuage de points du modèle 5", 
      xlab="Publicité", ylab="Nombre de sièges vendus en milliers", col="red") 
abline (reg5, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg5, col="purple")
#Intervalle de confiance des coefficients de régression
confint (reg5)

#Modèle 6: 
#Tableau des coefficients de régression
reg6 <- lm(log(sales)~advertising)
summary(reg6)
#Tableau d'analyse de la variance
anova (reg6)
#Nuages de points
par (mfrow=c(1,1))
plot (advertising, log (sales), main="Nuage de points du modèle 6", 
      xlab="Publicité", ylab="Nombre de sièges vendus en milliers", col="red") 
abline (reg6, col="blue")
#Analyse des résidus
par (mfrow=c(2,2))
plot (reg6, col="purple")
#Intervalle de confiance des coefficients de régression
confint (reg6)

#d)
interprev <- data.frame(price=118, advertising=12, region=0)
predict(reg1, interprev, interval="prediction")
