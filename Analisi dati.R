fit <- lm(viscosity ~ scale(moisture) + scale(protein) + scale(ash), data=ice_cream)
summary(fit) # show results


#56 è 


#PER FARE PLOT RESIDUI
x11()
hist(fit$residuals, col = "red", freq = F, xlab="Density", ylab= "Residuals")       # cambia main = ""
curve(dnorm(x,0, sd(fit$residuals)), lwd= 2, add= TRUE) #confronta con una distribuzione normale

#UNA VOLTA CHE SI APRE LA FINESTRA GRAFICA --> ESPORTA POSTSCRIPT --> .EPS è COMODO PER LATEX OPPURE QUELLO CHE VOLETE

dev.off()
plot(fit$fitted.values, fit$residuals)
abline(h = 0)



plot(ice_cream$ash, fit$residuals)
plot(ice_cream$protein, fit$residuals)
plot(ice_cream$moisture, fit$residuals)

#per migliorare il modello si vede dai 3 grafici precedenti che i residui 
# plottati rispetto ad ash hanno un comportamento "strano"... si abbassano molto 
# nella zona centrale e sono alti ai lati. Questo suggerisce che si potrebbe provare a migliorare il modello introducendo un variabile ash^2



fit.1 <- lm(viscosity ~ scale(moisture) + scale(protein) + scale(ash) + scale(ash^2), data=ice_cream)
summary(fit.1) # show results


#Già solamente questo fa passare la devianza spiegata da 0.37 a 0.8

#Vediamo se migliora anche il comporamento dei residui

x11()
hist(fit.1$residuals, col = "red", freq = F, xlab="Density", ylab= "Residuals")       # cambia main = ""
curve(dnorm(x,0, sd(fit.1$residuals)), lwd= 2, add= TRUE) #confronta con una distribuzione normale


##iOLTRE A VERIFICARE OMOSCHEDASTICITà ( CHE SIGNIFICA " VARIANZA UGUALE" E SI INTENDE CHE LA VARIANZA è COSTANTE AL VARIARE DEI FITTED VALUES IN QUESTO CASO SPECIFICO) VERIFICA MEDIA RESIDUI SIA ZERO E ABBIANO DISTRIBUZIONE NORMALE

mean(fit.residuals)

dev.off()


x11()
par(mfrow=c(2,1))
plot(fit.1$fitted.values, fit.1$residuals, main="Modello con ash^2", ylim=c(-40,40))
abline(h = 0)
plot(fit$fitted.values, fit$residuals, main="Modello con ash e basta", ylim=c(-40,40))
abline(h = 0)

#Per apprezzare il miglioramento lasciare ylim costante.. si nota che
# 1) residui più schiacciati intorno a zero
# 2) problema nella parte centrale risolto quasi del tutto


#IN AGGIUNTA, PER VEDERE SE IL MODELLO PUò ESSERE ANCORA MIGLIORATO, PROVARE AD INTRODURRE INTERAZIONI



fit.2 <- lm(viscosity ~ scale(moisture) + scale(protein) + scale(ash) + scale(ash^2)
            +scale(protein):scale(ash), data=ice_cream)

summary(fit.2) # show results

#Un'interazione che ha portato vantaggi è stata quella di protein con ash... se scoprite il significato delle variabili vi aiuto a dare un'interpretazione ai coefficienti stimati






     