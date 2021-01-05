# RDD: Sharp

cat("\014")
rm(list=ls())
dev.off();dev.off();
set.seed(2020)

library(lattice)




#########
#  Primer Ejemplo General
#########


# datos simulados
data.1<-data.frame(
        x=1:20,
        y=(c(10,15,20,25,30,35,40,45,50,55, 60,65,70,75,80,85,90,95,100,105)),
        z=(factor(rep(0:1,each=10)))
        ) 


# desordenar y
data.1$y<-jitter(data.1$y,factor=12)


# graficar: Existe un tipo de relacion entre x e y pero que cambie con el tratamiento (z)?
dev.off();dev.off();
xyplot(y~x,data.1,
       xlab="Tiempo",
       ylab="Puntaje", 
       main="Progresion hasta la Intervencion",
       groups=z)

# Veamos con mas detalle
xyplot(y~x,data.1,
       xlab="Tiempo",
       ylab="Puntaje", 
       main="Progresion hasta la Intervencion",
       pch=c(19,17),
       groups=z, 
       col=c("blue","green"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1)
)

# Supongamos que el corte es 10.5
corte = 10.5



# Regresion
analysis1 <- lm(y~I(x-corte)*z,
                data.1
                )

# Vean que ocupamos el operador "I" (de "Isolate", aislar). "I" aisla todo lo que esta dentro de "I". En este caso, la sustraccion.
summary(analysis1)
# Z no tiene un efecto causal 

#########
#  Cambio en el intercepto
#########

library (lattice)
data.2 <- data.frame(
        x=1:20,
        y=c(5,10,15,20,25,30,35,40,45,50, 100,105,110,115,120,125,130,135,140,145),
        z=(factor(rep(0:1,each=10)))
)

# desordenar y
data.2$y<-jitter(data.2$y,factor=10)

# plot
dev.off();dev.off();
xyplot(y~x,data.2,
       xlab="Tiempo",
       ylab="Puntaje", 
       main="Progresion hasta la Intervencion",
       groups=z)

# Veamos con mas detalle
xyplot(y~x,data.2,
       xlab="Tiempo",
       ylab="Puntaje", 
       main="Progresion hasta la Intervencion",
       pch=c(19,17),
       groups=z, 
       col=c("blue","green"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1)
)

analysis2<-lm(y~I(x-corte)*z,data.2)
summary(analysis2)

# Si estamos interesados en observar el intercepto, debemos observar el efecto y significancia de pi asociado a la variable de intervencion z (en este caso pi=46.459 es ***).


#########
#  Cambio en el slope/pendiente
#########

data.3 <- data.frame(
        x=1:20,
        y=c(50:59,60,65,70,75,80,85,90,95,100, 105),
        z=(factor(rep(0:1,each=10))))


data.3$y<-jitter(data.3$y,factor=10)

# plot
dev.off();dev.off();
xyplot(y~x,data.3,
       xlab="Tiempo",
       ylab="Puntaje", 
       main="Progresion hasta la Intervencion",
       groups=z)

# plot 2
xyplot(y~x,data.3,
       xlab="Tiempo",
       ylab="Puntaje",
       main="Progresion hasta la Intervencion Scenario 3",
       pch=c(19,17), 
       groups=z,
       col=c("maroon","grey"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1))

# Reg
analysis3<-lm(y~I(x-corte)*z,data.3) 
summary(analysis3)

# I(x - corte):z1 es positivo y estadisticamente significativo. Tenemos evidencia para sugerir que existe un cambio de intercepto y pendiente.
