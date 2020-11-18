
library(glmnet)
library(dplyr)
library(tidyr)
library(stats4)
library(bbmle)
library(ggplot2)
library(car)

load("hogar.Rdata")


LL <- function(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) {
  n<-nrow(hogar)
  x<-model.matrix(pobre~.,hogar)
  
  betas = c(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
  xb = x%*%betas
  xb_g = 1/(1+exp(-xb))
  vero1 = suppressWarnings(dbinom(hogar$pobre,
                                  size = 1,
                                  prob = xb_g, 
                                  log=FALSE))
  parte1<- -sum(log(vero1))
  parte2<-sum(abs(betas))
  
  vero= parte1+(n*lambda*parte2)
}

# Usando validacion cruzada para determinar el mejor Lambda
x<-model.matrix(pobre~.,hogar)
sal.cv<-cv.glmnet(x[,-1],hogar$pobre,alpha=1, family = binomial)
#El lambda de 1 error estandar
lambda<-sal.cv$lambda.1se

## Valores iniciales a partir de la salida de glmnet
mod1 <- mle(LL, start = list(    b0= -1,
                                 b1= 0.38,
                                 b2= -0.32,
                                 b3= -0.32,
                                 b4= 0.65,
                                 b5= 0.0000000001,
                                 b6= -0.005,
                                 b7= 0.0000000001,
                                 b8= -0.35,
                                 b9= 0.05,
                                 b10= 0.0000000001
))
summary(mod1)


mod2 <- mle2(LL, start = list(
  b0= -1,
  b1= 0.38,
  b2= -0.32,
  b3= -0.32,
  b4= 0.65,
  b5= 0.0000000001,
  b6= -0.005,
  b7= 0.0000000001,
  b8= -0.35,
  b9= 0.05,
  b10= 0.0000000001
))

summary(mod2)
colnames(hogar)[-1]



# Mean square error
MSE <- function(Pred,Real) {
  N<-length(Real)
  ss<-(1/N)*sum((Real-Pred)^2)
  return(ss)
}


#lasso
x<-model.matrix(pobre~.,hogar)
coef_las<-summary(mod2)@coef[,1]

pred_las<-1/(1+exp(-(x %*% coef_las)))
pred_las_class<-ifelse(pred_las>mean(hogar$pobre),1,0)
MSE(pred_las_class,hogar$pobre)

# sensiblidad
MC1<-table(hogar$pobre,pred_las_class)
MC1

pp1 <- round(diag(MC1)[2]/rowSums(MC1)[2],4)
pp1

pn1<- round(diag(MC1)[1]/rowSums(MC1)[1],4)
pn1


#curva ROC
pred <- ROCR::prediction(pred_las,hogar$pobre )  
auc <- ROCR::performance(pred, "auc")
auc <- attributes(auc)$y.values[[1]]

perf <- ROCR::performance(pred,"tpr","fpr")
plot(perf,col="red",main=paste("AUC: ",round(auc,2)))
segments(0,0,1,1,col='black')
grid()  




#regresion logistica
mod_log <- glm(pobre~., family = binomial(link = "logit"), data=hogar)
summary(mod_log)

pred_log<-predict(mod_log,hogar[,-1],type="response")
pred_log_class<-ifelse(pred_log>mean(hogar$pobre),1,0)
MSE(pred_log_class,hogar$pobre)

# sensiblidad
MC<-table(hogar$pobre,pred_log_class)
MC

pp <- round(diag(MC)[2]/rowSums(MC)[2],4)
pp

pn<- round(diag(MC)[1]/rowSums(MC)[1],4)
pn



#curva ROC
pred <- ROCR::prediction(pred_log,hogar$pobre )  
auc <- ROCR::performance(pred, "auc")
auc <- attributes(auc)$y.values[[1]]

perf <- ROCR::performance(pred,"tpr","fpr")
plot(perf,col="red",main=paste("AUC: ",round(auc,2)))
segments(0,0,1,1,col='black')
grid()  





####Cantidad de variables seleccionadas según el lambda 

# Usando validacion cruzada para determinar el mejor Lambda
n<-nrow(hogar)
x<-model.matrix(pobre~.,hogar)
sal.cv<-cv.glmnet(x[,-1],hogar$pobre,alpha=1, family = binomial)
#El lambda de 1 error estandar
lambda_tot<-sal.cv[["lambda"]]

mat1<-matrix(rep(0,length(lambda_tot)*1),
             nrow=length(lambda_tot),ncol = 1,byrow = T)


for (i in 1:length(lambda_tot)) {
  
  lambda<-lambda_tot[i]
  
  mod2 <- mle2(LL, start = list(
    b0= -1,
    b1= 0.38,
    b2= -0.32,
    b3= -0.32,
    b4= 0.65,
    b5= 0.0000000001,
    b6= -0.005,
    b7= 0.0000000001,
    b8= -0.35,
    b9= 0.05,
    b10= 0.0000000001
  ))
  
  smod<-summary(mod2)
  mat1[i,]<-sum(1*(smod@coef[,4]<0.05))
  
}


df4<-data.frame(lambda=lambda_tot,
                Cant_Variables=mat1[,1])

df4<-df4 %>% 
  mutate(Corte_lambda=ifelse(lambda==sal.cv$lambda.1se,"lambda.1se",ifelse(
    lambda==sal.cv$lambda.min,"lambda.min","Otros"  )))


ggplot(data = df4,aes(lambda,Cant_Variables)) + 
  geom_point(aes(colour=Corte_lambda))+
  ylim(0,10)+
  ylab("Cantidad Variables")+
  theme_bw()+
  scale_color_manual(values=c("orange", "red", "dark gray")) 




########## simulaciones ##################

#1. Potencia de la prueba de los coeficientes

x<-model.matrix(pobre~.,hogar)
sal.cv<-cv.glmnet(x[,-1],hogar$pobre,alpha=1, family = binomial)
#El lambda de 1 error estandar
lambda<-sal.cv$lambda.1se

muestras<-c(50,100,1000)

#matrices que guardan los resultados de la potencia
tam1<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam2<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam3<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)

#matrices que guardan los resultados de las estimaciones lasso
tam1_es<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam2_es<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam3_es<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)

#matrices que guardan los resultados de las estimaciones logistica
tam1_es_log<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam2_es_log<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)
tam3_es_log<-matrix(rep(0,11*1000),nrow=1000,ncol = 11,byrow = T)


for(i in 1:length(muestras)){
  for (j in 1:1000) {
    #muestra con reemplazo
    index<-sample(1:nrow(hogar),muestras[i], replace=TRUE)
    hogar_temp<- hogar[index,]
    n<-nrow(hogar_temp)
    x<-model.matrix(pobre~.,hogar_temp)
    
    LL_temp <- function(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) {
  
      betas = c(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
      xb = x%*%betas
      xb_g = 1/(1+exp(-xb))
      vero1 = suppressWarnings(dbinom(hogar_temp$pobre,
                                      size = 1,
                                      prob = xb_g, 
                                      log=FALSE))
      parte1<- -sum(log(vero1))
      parte2<-sum(abs(betas))
      
      vero= parte1+(n*lambda*parte2)
    }
    
  
    mod2 <- mle2(LL_temp, start = list(
      b0= -1,
      b1= 0.38,
      b2= -0.32,
      b3= -0.32,
      b4= 0.65,
      b5= 0.0000000001,
      b6= -0.005,
      b7= 0.0000000001,
      b8= -0.35,
      b9= 0.05,
      b10= 0.0000000001
    ))
    
    smod<-summary(mod2)
    
    mod_log <- glm(pobre~., family = binomial(link = "logit"), data=hogar_temp)
    
    # extraer valores
    if(i==1){
      tam1[j,]<- 1*(smod@coef[,4]<0.05)
      tam1_es[j,]<- smod@coef[,1]
      tam1_es_log[j,]<- coef(mod_log)
    }
      
    if(i==2){
      tam2[j,]<- 1*(smod@coef[,4]<0.05)
      tam2_es[j,]<- smod@coef[,1]
      tam2_es_log[j,]<- coef(mod_log)
    }
    
    if(i==3){
      tam3[j,]<- 1*(smod@coef[,4]<0.05)
      tam3_es[j,]<- smod@coef[,1]
      tam3_es_log[j,]<- coef(mod_log)
    }
  }
}

#resumen para potencia
mean1<-apply(tam1,2,function(x) mean(x,na.rm = TRUE))
mean2<-apply(tam2,2,function(x) mean(x,na.rm = TRUE))
mean3<-apply(tam3,2,function(x) mean(x,na.rm = TRUE))

coeficientes<-c("b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10")
df<-data.frame(coeficientes=c(coeficientes,coeficientes,coeficientes),
               Muestra=c(rep(muestras[1],length(coeficientes)),rep(muestras[2],length(coeficientes)),rep(muestras[3],length(coeficientes))),
               Potencia=c(mean1,mean2,mean3))
#grafico potencia
df$coeficientes<-factor(df$coeficientes, levels = c("b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10"))

ggplot(data = df,aes(coeficientes,Potencia)) + 
  geom_point()+
  ylim(0,1)+
  theme_bw() + 
  facet_wrap(~Muestra, ncol =3) 


#2. Histogramas de estimacion de coeficientes


hist(tam1_es[,2])
hist(tam1_es_log[,2])







#3. Análisis de residuos de deviancia (linealidad y normalidad)


x<-model.matrix(pobre~.,hogar)
sal.cv<-cv.glmnet(x[,-1],hogar$pobre,alpha=1, family = binomial)
#lambda<-sal.cv$lambda.1se
#lambda<-sal.cv$lambda.min
lambda<-0.07

#50 , 100, 1000
muestras<-1000

#muestra con reemplazo
index<-sample(1:nrow(hogar),muestras, replace=TRUE)
hogar_temp<- hogar[index,]
n<-nrow(hogar_temp)
x<-model.matrix(pobre~.,hogar_temp)

LL_temp <- function(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) {
  
  betas = c(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
  xb = x%*%betas
  xb_g = 1/(1+exp(-xb))
  vero1 = suppressWarnings(dbinom(hogar_temp$pobre,
                                  size = 1,
                                  prob = xb_g, 
                                  log=FALSE))
  parte1<- -sum(log(vero1))
  parte2<-sum(abs(betas))
  
  vero= parte1+(n*lambda*parte2)
}


mod2 <- mle2(LL_temp, start = list(
  b0= -1,
  b1= 0.38,
  b2= -0.32,
  b3= -0.32,
  b4= 0.65,
  b5= 0.0000000001,
  b6= -0.005,
  b7= 0.0000000001,
  b8= -0.35,
  b9= 0.05,
  b10= 0.0000000001
))

smod<-summary(mod2)
coef_las<-smod@coef[,1]
pred_las<-1/(1+exp(-(x %*% coef_las)))
pred_las_class<-ifelse(pred_las>mean(hogar$pobre),1,0)

res<-hogar_temp$pobre-pred_las_class
signo.res=recode(res,"lo:0=-1;0:hi=1") #extrae el signo


#logverosimilitud

#modelo observado
vero1 = suppressWarnings(dbinom(hogar_temp$pobre,
                                size = 1,
                                prob = pred_las, 
                                log=FALSE))
parte1<-log(vero1)
parte2<-sum(abs(coef_las))
log.veros.observado= parte1+(n*lambda*parte2)


#modelo saturado

#Patrones de covariancia
data_tm<-hogar_temp %>% group_by(ninnos,cocina_elec,banno,sin_educ,
                                 urbano,renta,sani_alcant,casadx,dependencia,
                                 casa_propia) %>% 
  summarise(
    y=sum(pobre),
    ene=n()
  )


prob<-data_tm$y/data_tm$ene

vero1_2 = suppressWarnings(dbinom(hogar_temp$pobre,
                                size = 1,
                                prob = prob, 
                                log=FALSE))
parte1_2<-log(vero1_2)
log.veros.saturado= parte1_2+(n*lambda*parte2)


#residuos de deviancia
res.dev=signo.res*(2*(log.veros.saturado-log.veros.observado))^.5



plot(res.dev~pred_las,xlab="Predichos",ylab = "Residuos Deviancia",main="",
     pch=19,xlim=c(0,1),ylim=c(-2,3))
grid()

qqPlot(res.dev,ylab = "Residuos Deviancia",pch=19,ylim = c(-2,3))
grid()










