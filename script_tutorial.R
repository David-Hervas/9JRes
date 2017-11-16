########Prerrequisitos
#Usuarios de Windows: 
#Instalar Rtools (https://cran.r-project.org/bin/windows/Rtools/)

#Todos los usuarios
install.packages("brms")
install.packages("clickR")

####Carga de paquetes
library(brms)
library(clickR)

##Ejemplo básico con datos sencillos
descriptive(mtcars)
mtcars <- fix.factors(mtcars)
descriptive(mtcars)

#Modelo lineal mediante lm
mod1 <- lm(mpg ~ hp + disp + gear + am, data=mtcars)
summary(mod1)  #Summary estándar de R
summary1 <- report(mod1)
summary1$coefficients
summary1$upper.int

#Gráfico de los coeficientes
par(mar=c(5, 8, 3, 2))
plot(summary1)


#Alternativa bayesiana a la función lm

sink("lmbayes.stan")
cat("
    data {
    int N; //the number of observations
    int K; //the number of columns in the model matrix
    real y[N]; //the response
    matrix[N,K] X; //the model matrix
    }
    parameters {
    vector[K] beta; //the regression parameters
    real sigma; //the standard deviation
    }
    transformed parameters {
    vector[N] linpred;
    linpred = X*beta;
    }
    model {  
    beta[1] ~ uniform(-500, 500); 
    
    for(i in 2:K)
    beta[i] ~ uniform(-50, 50);//prior for the slopes following Gelman 2008
    
    y ~ normal(linpred,sigma);
    }
    ")
sink()

#load libraries
library(rstan)
library(coda)
#the model
X <- model.matrix(~ hp + disp + gear + am, data=mtcars)
y <- mtcars$mpg

m_norm<-stan(file="lmbayes.stan",data = list(N=dim(mtcars)[1],K=6,y=y,X=X),pars = c("beta","sigma"))
m_norm
round(coef(mod1),2)

#buffff!!!! Qué complicado!!!!

#Alternativa con brms
mod1b <- brm(mpg ~ hp + disp + gear + am, data=mtcars)
report(mod1b)
#Eso es otra cosa!

#Podemos hacer (casi) cualquier cosa que haríamos sobre el código en STAN
#Cambiar previas
mod1c <- brm(mpg ~ hp + disp + gear + am, data=mtcars, prior=c(set_prior("normal(0, 10)", class="b")), sample_prior = TRUE)
report(mod1c)

#Previas específicas para cada coeficiente
mod1d <- brm(mpg ~ hp + disp + gear + am, data=mtcars, prior=c(set_prior("normal(0, 10)", class="b", coef="hp"),
                                                               set_prior("student_t(10, 0, 1)", class="b", coef="disp")))
report_mod1d <- report(mod1d)

report_mod1d$Rhat_max

#Podemos paralelizar de forma sencilla
mod1d <- brm(mpg ~ hp + disp + gear + am, data=mtcars, prior=c(set_prior("normal(0, 10)", class="b", coef="hp"),
                                                               set_prior("student_t(10, 0, 1)", class="b", coef="disp")), cores=4)
report_mod1d <- report(mod1d)


#Podemos contrastar hipótesis de manera muy flexible
hypothesis(mod1c, "disp > 0", class = "b")
hypothesis(mod1c, "disp < 0", class = "b")
hypothesis(mod1c, "disp < -0.1", class = "b")
hypothesis(mod1c, "disp > hp", class = "b")

#Podemos visualizar el efecto de las variables con gráficas de efectos marginales
marginal_effects(mod1c)

#Especialmente útiles para visualizar interacciones
mod1e <- brm(mpg ~ hp *gear + disp + am, data=mtcars, prior=c(set_prior("normal(0, 10)", class="b")), sample_prior = TRUE)
report(mod1e)
marginal_effects(mod1e)

#Tenemos acceso a innumerables familias de distribuciones
# gaussian(link = "identity")
# student(link = "identity", link_sigma = "log", link_nu = "logm1")
# bernoulli(link = "logit")
# binomial(link = "logit")
# poisson(link = "log")
# Gamma(link = "inverse")
# negbinomial(link = "log", link_shape = "log")
# lognormal(link = "identity", link_sigma = "log")
# skew_normal(link = "identity", link_sigma = "log", link_alpha = "identity")
# exponential(link = "log")
# weibull(link = "log", link_shape = "log")
# frechet(link = "log", link_nu = "logm1")
# gen_extreme_value(link = "identity", link_sigma = "log", link_xi = "log1p")
# exgaussian(link = "identity", link_sigma = "log", link_beta = "log")
# wiener(link = "identity", link_bs = "log", link_ndt = "log", link_bias = "logit")
# beta(link = "logit", link_phi = "log")
# von_mises(link = "tan_half", link_kappa = "log")
# asym_laplace(link = "identity", link_sigma = "log", link_quantile = "logit")
# hurdle_poisson(link = "log")
# hurdle_negbinomial(link = "log", link_shape = "log", link_hu = "logit")
# hurdle_gamma(link = "log", link_shape = "log", link_hu = "logit")
# hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit")
# zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit")
# zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit")
# zero_inflated_poisson(link = "log", link_zi = "logit")
# zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit")
# zero_inflated_binomial(link = "logit", link_zi = "logit")
# categorical(link = "logit")
# cumulative(link = "logit", link_disc = "log", threshold = c("flexible", "equidistant"))
# sratio(link = "logit", link_disc = "log", threshold = c("flexible", "equidistant"))
# cratio(link = "logit", link_disc = "log", threshold = c("flexible", "equidistant"))
# acat(link = "logit", link_disc = "log", threshold = c("flexible", "equidistant"))

#Y por supuesto, podemos hacer un completo diagnóstico del modelo
plot(mod1c) #Diagnóstico básico
launch_shinystan(mod1c)  #Diagnóstico avanzado


######Ejemplos

#Modelo de regresión logística con separación perfecta
#Carga de datos
datos1 <- read.csv2("base1.csv")
names(datos1)
datos1 <- nice_names(datos1)
names(datos1)

descriptive(datos1)

datos1 <- fix.numerics(datos1)
descriptive(datos1)

datos1 <- fix.factors(datos1)
descriptive(datos1)

#Modelo predictivo para la variable RoboD
mod2 <- glm(robod ~ tad + rs02m, family="binomial", data=datos1[datos1$dia==1,])
report(mod2)

#Escalamos?
datos1$tad_sc <- scale(datos1$tad)[,1]
datos1$rs02m_sc <- scale(datos1$rs02m)[,1]

mod2 <- glm(robod ~ tad_sc + rs02m_sc, family="binomial", data=datos1[datos1$dia==1,])
report(mod2)

#Y en bayesiano?
mod2b <- brm(robod ~ tad_sc + rs02m_sc, family="bernoulli", data=datos1[datos1$dia==1,], prior=set_prior("normal(0, 10)", class="b"))
report(mod2b)

#Pero hay paquetes para hacer lo mismo sin estadística bayesiana...
library(brglm)
mod2c <- brglm(robod ~ tad_sc + rs02m_sc, data=datos1[datos1$dia==1,])
summary(mod2c)

#El problema es la falta de flexibilidad. Resulta que el estudio es longitudinal, queremos un modelo
#de medidas repetidas. En bayesiano es muy fácil, se añade al modelo y ya está

mod3 <- brm(robod ~ tad_sc + rs02m_sc + dia + (dia|id), family="bernoulli", data=datos1, prior=set_prior("normal(0, 10)", class="b"))
report(mod3)


######Modelo de regresión de percentiles
datos2 <- read.csv2("base2.csv")

names(datos2)
descriptive(datos2)

datos2 <- datos2[datos2$age<50 & datos2$week<44,]
descriptive(datos2)

plot(mca_pi ~ week, data=datos2)

#Estamos interesados en determinar valores por encima de lo normal. Podemos estimar el percentil 95
library(quantreg)
mod4 <- rq(mca_pi ~ week, data=datos2, tau=0.95)
report(mod4)  #Bug en clickR
summary(mod4, se="rank")


#Y en bayesiano
mod4b <- brm(bf(mca_pi ~ week, quantile=0.95), data=datos2, family="asym_laplace")
report(mod4b)


#pero volvemos a tener que las observaciones no son independientes. Añadimos factor aleatorio.
#Vamos a meter una previa para la desviación estándar del factor aleatorio y otra para el coeficiente
#de la variable week
mod4c <- brm(bf(mca_pi ~ week + (1|id), quantile=0.95), data=datos2, family="asym_laplace",
             prior=c(set_prior("cauchy(0,2)", class = "sd"),
                     set_prior("student_t(5, 0, 10)", class = "b")), cores=4)
report(mod4c)

##############Modelo de regresión ordinal
