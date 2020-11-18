

n=200

beta0=3
beta1=1.5
beta2=0.2
beta3=0.01
beta4=2
beta5=0.06
beta6=2.3
beta7=0.4

x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
x5 = rnorm(n)
x6 = rnorm(n)
x7 = rnorm(n)

x=cbind(x1,
        x2,
        x3,
        x4,
        x5,
        x6,
        x7)

e = rnorm(n,0,2)

xb<-beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+beta5*x5+beta6*x6+beta7*x7+e

y = rbinom(n,1,1/(1+exp(-xb)))

LL <- function(beta0,beta1,beta2,beta3,beta4,beta5,beta6,beta7) {
  #n<-nrow(base)
  #con funcion enlace logit
  #x<-model.matrix(diagnosis~.,base)
  xb<-beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+beta5*x5+beta6*x6+beta7*x7
  
  
  ## Aquí cambié el cálculo pra obtener los valores entre 0 y 1 
  xb_g = 1/(1+exp(-xb))
  
  vero1 = suppressWarnings(dbinom(y,
                                  size = 1,
                                  prob = xb_g, 
                                  log=FALSE))
  parte1<- -sum(log(vero1))
  
  # Usando validaci?n cruzada para determinar el mejor Lambda
  sal.cv<-cv.glmnet(x=x,y=y,alpha=1)
  
  #Aquí cambié el lambda basado en un artículo que me encontré en donde recomiendan usas el de 1 error estandar en lugar del mínimo
  lambda<-sal.cv$lambda.1se
  
  parte2<-abs(beta1)+abs(beta2)+abs(beta3)+abs(beta4)+abs(beta5)+abs(beta6)+abs(beta7)
  

  vero= parte1+(n*lambda*parte2)
  
}

mod2 <- mle2(LL, start = list(beta0=-5,
                              beta1=2,
                              beta2=0.5,
                              beta3=0.5,
                              beta4=2,
                              beta5=0.5,
                              beta6=2,
                              beta7=3
))
summary(mod2)




