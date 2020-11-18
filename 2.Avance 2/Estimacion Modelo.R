

library(dplyr)
library(stats4)
library(bbmle)
library(glmnet)




#base<- read.csv('tumores.csv', header = T)
base <- read.csv(paste0(here::here("Avance 2"),"/tumores.csv"))

# LIMPIANDO Y ALISTANDO BASE

# Informacion de las variables numericas 

# a) radius (mean of distances from center to points on the perimeter) 
# b) texture (standard deviation of gray-scale values) 
# c) perimeter 
# d) area 
# e) smoothness (local variation in radius lengths) 
# f) compactness (perimeter^2 / area - 1.0) 
# g) concavity (severity of concave portions of the contour) 
# h) concave points (number of concave portions of the contour) 
# i) symmetry 
# j) fractal dimension ("coastline approximation" - 1)


base$diagnosis<-(base$diagnosis=="M")*1
base<-base[,-1]
#base<-base[,c(1,22:31)]
base<-base[,1:11]

attach(base)


# beta0=-7.35
# beta1=-2.05
# beta2=0.38
# beta3=-0.07
# beta4=0.039
# beta5=76.43
# beta6=-1.46
# beta7=8.46
# beta8=66.82
# beta9=16.27
# beta10=-68.33


#betas va ser un vector con los 31 valores de los coeficientes 
LL <- function(beta0,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10) {
  n<-nrow(base)
  #con funcion enlace logit
  x<-model.matrix(diagnosis~.,base)
  xb<-beta0+beta1*radius_worst+beta2*texture_worst+beta3*perimeter_worst+beta4*area_worst+beta5*smoothness_worst+beta6*compactness_worst+beta7*concavity_worst+beta8*concave.points_worst+beta9*symmetry_worst+beta10*fractal_dimension_worst
  
  
  ## Aquí cambié el cálculo pra obtener los valores entre 0 y 1 
  xb_g = 1/(1+exp(-xb))
  
  vero1 = suppressWarnings(dbinom(base$diagnosis,
                                  size = 1,
                                  prob = xb_g, 
                                  log=FALSE))
  parte1<- -sum(log(vero1))
  
  # Usando validaci?n cruzada para determinar el mejor Lambda
  sal.cv<-cv.glmnet(x[,-1],base$diagnosis,alpha=1)
  
  #Aquí cambié el lambda basado en un artículo que me encontré en donde recomiendan usas el de 1 error estandar en lugar del mínimo
  lambda<-sal.cv$lambda.1se
  
  parte2<-abs(beta1)+abs(beta2)+abs(beta3)+abs(beta4)+abs(beta5)+abs(beta6)+abs(beta7)+abs(beta8)+abs(beta9)+abs(beta10)

  
  
  vero= parte1+(n*lambda*parte2)
  
}




mod1 <- mle(LL, start = list(beta0=-2.05,
                             beta1=0.11,
                             beta2=0.01,
                             beta3=0.01,
                             beta4=-0.05,
                             beta5=2.18,
                             beta6=-0.513,
                             beta7=0.22,
                             beta8=1.73,
                             beta9=0.61,
                             beta10=2.13  
                               ))
summary(mod1)


mod2 <- mle2(LL, start = list(beta0=-2.05,
                              beta1=0.11,
                              beta2=0.01,
                              beta3=0.01,
                              beta4=-0.05,
                              beta5=2.18,
                              beta6=-0.513,
                              beta7=0.22,
                              beta8=1.73,
                              beta9=0.61,
                              beta10=2.13  
                              ))
summary(mod2)





#funcion glmnet

x<-model.matrix(diagnosis~.,base)
y<-base$diagnosis

n<-nrow(base)


mod = glm(y~x[,-1], family = binomial)

lasso.mod<-glmnet(x,y,alpha=1)

# Validaci?n Cruzada
sal.cv<-cv.glmnet(x[,-1],y,alpha=1) 
plot(sal.cv)

mejor.lambda<-sal.cv$lambda.1se

coef<-coef(lasso.mod)[,which(lasso.mod$lambda==mejor.lambda)]
coef

plot(lasso.mod,"lambda", label=TRUE)
abline(v = log(mejor.lambda), col="blue", lwd=4, lty=3)




