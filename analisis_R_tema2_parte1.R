#Especificamos el directorio de trabajo
setwd("C:/master EAE/")

# Cargamos los datos del ejercicio
library(readxl)
barometro_octubre22 <- read_excel("barometro_octubre22.xlsx")
cotiz_men_estaciones <- read_excel("cotiz_men_estaciones.xlsx")
#"Pegamos" las variables de los dataframes en el espacio de trabajo para poder referirnos a ellas directamente.
attach(barometro_octubre22)
attach(cotiz_men_estaciones)


library(readxl)
cotiz_men_estaciones <- read_excel("cotiz_men_estaciones.xlsx")
View(cotiz_men_estaciones)


## Modelo Lineal Simple

eq1 <- lm(IBE.Cl ~ IBE.Hi)
eq1

library(ggplot2)
ggplot(data=cotiz_men_estaciones, aes(y=IBE.Cl,x=IBE.Hi))+geom_point()
#Si queremos a?adir la recta de regresi?n, se hace con smooth y la opci?n lineal

ggplot(data=cotiz_men_estaciones, aes(y=IBE.Cl,x=IBE.Hi))+geom_point()+geom_smooth(method="lm",se=F)
#comparar con el modelo de la cotizaci?n con el IBEX

ggplot(data=cotiz_men_estaciones, aes(y=IBE.Cl,x=IBEX.Cl))+geom_point()

#Si queremos a?adir la recta de regresi?n, se hace con smooth y la opci?n lineal

ggplot(data=cotiz_men_estaciones, aes(y=IBE.Cl,x=IBEX.Cl))+geom_point()+geom_smooth(method="lm",se=F)

lIBE.Cl <- log(IBE.Cl)
lIBEX.Cl <- log(IBEX.Cl)
lm(lIBE.Cl ~ lIBEX.Cl)

lm(log(IBE.Cl) ~ log(IBEX.Cl))


## Modelo Lineal General


eq2 <- lm(FCC.Cl ~ IBEX.Cl+Euri)
eq2

#Los residuos de la regresi?n se obtienen del modelo:

residuos <- eq2$residuals
plot(residuos)
plot(residuos,type="overplotted")
abline(h=0)

plot(eq2)


par(mfrow=c(2,2))
plot(eq2)
par(mfrow=c(1,1))

## Heterocedasticidad

library(lmtest)
bptest(eq2)

bptest(eq2, ~I(IBEX.Cl*Euri)+I(IBEX.Cl^2)+I(Euri^2))

##Autocorrelaci?n

library(lmtest)

bgtest(eq2, order = 3)
bgtest(eq2, order = 2)
bgtest(eq2, order = 1)


## Multicolinealidad

#Dibujo de correlaciones
subdatos <- cbind(IBE.Cl,IBEX.Cl,Euri)
library(corrplot)
corrplot(cor(subdatos))
#Ahora, con los valores
corrplot(cor(subdatos),method="number", order="hclust", type="lower")
#Ahora mezclado todo.
corrplot.mixed(cor(subdatos), lower = "number", upper = "circle")

#estad?stico para multiolinealidad
library(car)
vif(eq2)

#Contraste de hip?tesis en los resultados del modelo

summary(eq2)

#Intervalos de confianza de los parametros
confint(eq2, level = 0.95)

#Predicci?n
predict(object = eq2, newdata = data.frame(IBEX.Cl=c(9200) ,Euri= c(3)), interval = "prediction", level = 0.90)


## Contraste de Hip?tesis generales

library(car)
linearHypothesis(eq2, "IBEX.Cl + Euri=1")

linearHypothesis(eq2, c("IBEX.Cl=0.5" , "IBEX.Cl + Euri=1"))


#Contrste por sumas residuales

mod_srs <- lm(IBE.Cl ~ IBEX.Cl+Euri)
mod_srr <- lm(IBE.Cl ~ I(IBEX.Cl+Euri))

anova(mod_srr,mod_srs)


## Modelo lineal general con Factores

verano.fac <- as.factor(verano)
otono.fac <- as.factor(otoño)


eq3.fac <- lm(IBE.Cl ~ IBEX.Cl+verano.fac)
summary(eq3.fac)


#Para el caso de varios factores, vamos a trabajar con los datos del barometro del CIS

barometro_octubre22 <- read_excel("C:/master EAE/barometro_octubre22.xlsx")
attach(barometro_octubre22)


preo.covid.fac <- as.factor(preo.covid)
                            
levels(preo.covid.fac) <- c("Mucho", "Bastante", "Regular", "Poco", "Nada")

eq4.fac <- lm(edad ~ preo.covid.fac)
summary(eq4.fac)              
              
preo.covid.ord <- ordered(preo.covid.fac,levels=c("Nada", "Poco", "Regular", "Bastante","Mucho"))

head(preo.covid)
head(preo.covid.fac)
head(preo.covid.ord)
                      


eq4.fac <- lm(edad ~ preo.covid.ord)
summary(eq4.fac)

#Si queremos cambiar el valor de referencia 

preo.covid.fac.2 <- relevel(preo.covid.fac,ref="Regular")

eq4.fac.noord.1 <- lm(edad ~ preo.covid.fac)
summary(eq4.fac.noord.1)
eq4.fac.noord.2 <- lm(edad ~ preo.covid.fac.2)
summary(eq4.fac.noord.2)
  
  
#comparamos estimacion sin factor 

eq5 <- lm(edad ~ preo.covid)
summary(eq5)


##Consideraciones Finales

library(GGally)
subdatos.frame <- as.data.frame(subdatos)
ggpairs(subdatos.frame, lower = list(continuous = "smooth"), diag = list(continuous = "bar"), axisLabels = "none")



