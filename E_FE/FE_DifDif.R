### R code from vignette source 'FE_DifDif.Rnw'

###################################################
### code chunk number 1: setup
###################################################
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(knitr)
set.seed(2020)
options(scipen=9999999)
if (!require("pacman")) install.packages("pacman"); library(pacman) 


###################################################
### code chunk number 2: p:d
###################################################
p_load(AER, plm, stargazer)
data(Fatalities)
head(Fatalities)[1:2]


###################################################
### code chunk number 3: f:e1
###################################################
p_load(AER, plm)
data("Fatalities")
head(Fatalities)


###################################################
### code chunk number 4: f:e2
###################################################
model <- plm(fatal ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within") # "pooling", "within", "between", "random" "fd", or "ht"
summary(model)


###################################################
### code chunk number 5: us:map
###################################################
# instalar librerias
p_load(ggplot2,dplyr,maps)
# declarar datos
all_states <- map_data("state") 
# plot
ggplot(all_states, 
       aes(x=long, y=lat, group = group)) + 
  geom_polygon(fill="grey", colour = "white") +
  geom_polygon(fill="green", data = filter(all_states, region %in% c("pennsylvania"))) +
  geom_polygon(fill="red", data = filter(all_states, region %in% c("new jersey"))
               ) +
  theme_bw() +
  xlab("Longitud") + 
  ylab("Latitud")


###################################################
### code chunk number 6: dif:dif
###################################################
plot(c(0, 1), c(6, 8), 
     type = "p",
     ylim = c(5, 12),
     xlim = c(-0.3, 1.3),
     main = "El Estimador de Difference in Difference",
     xlab = "Periodo",
     ylab = "Desempleo",
     col = "steelblue",
     pch = 20,
     xaxt = "n",
     yaxt = "n")
#
axis(1, at = c(0, 1), labels = c("Antes", "Despues"))
axis(2, at = c(0, 13))
# treatment
points(c(0, 1, 1), c(7, 9, 11), 
       col = "darkred",
       pch = 20)
# 
lines(c(0, 1), c(7, 11), col = "darkred")
lines(c(0, 1), c(6, 8), col = "steelblue")
lines(c(0, 1), c(7, 9), col = "darkred", lty = 2)
lines(c(1, 1), c(9, 11), col = "black", lty = 2, lwd = 2)
# 
text(1, 10, expression(tau), cex = 0.8, pos = 4)
text(0, 5.5, "Control antes (PA)", cex = 0.8 , pos = 4)
text(0, 6.8, "Treated antes (NJ)", cex = 0.8 , pos = 4)
text(1, 7.9, "Control despues (PA)", cex = 0.8 , pos = 4)
text(1, 11.1, "Treated despues (NJ)", cex = 0.8 , pos = 4)


###################################################
### code chunk number 7: dif:dif:Data
###################################################
n <- 200
# definir tau
TEffect <- 4
# generar dummy de treatment 
z <- c(rep(0, n/2), rep(1, n/2))
# simular pre y post treatment en la y
y_pre <- 7 + rnorm(n)
y_pre[1:n/2] <- y_pre[1:n/2] - 1
y_post <- 7 + 2 + TEffect * z + rnorm(n)
y_post[1:n/2] <- y_post[1:n/2] - 1 
#
p_load(scales) # para usar alpha abajo (colores)
pre <- rep(0, length(y_pre[z==0]))
post <- rep(1, length(y_pre[z==0]))
# t=1
plot(jitter(pre, 0.6), 
     y_pre[z == 0], 
     ylim = c(0, 16), 
     col = alpha("steelblue", 0.3),
     pch = 20, 
     xlim = c(-0.5, 1.5),
     ylab = "Y",
     xlab = "Periodo",
     xaxt = "n",
     main = "Simulacion del DID Estimator")
axis(1, at = c(0, 1), labels = c("Antes", "Despues"))
# treatment t=1
points(jitter(pre, 0.6), 
       y_pre[z == 1], 
       col = alpha("darkred", 0.3), 
       pch = 20)
# control t=2
points(jitter(post, 0.6),
       y_post[z == 0], 
       col = alpha("steelblue", 0.5),
       pch = 20)
# treatment t=2
points(jitter(post, 0.6), 
       y_post[z == 1], 
       col = alpha("darkred", 0.5),
       pch = 20)
# lineas
lines(c(0, 1), c(mean(y_pre[z == 1]), mean(y_post[z == 1])), col = "darkred")
lines(c(0, 1), c(mean(y_pre[z == 0]), mean(y_post[z == 0])), col = "steelblue")
lines(c(0, 1), c(mean(y_pre[z == 1]), mean(y_post[z == 0]) + 
(mean(y_pre[z == 1])-mean(y_pre[z == 0]))), col = "darkred", lty = 2) 


###################################################
### code chunk number 8: dif:dif:Data:2
###################################################
mean(y_post[z == 1]) - mean(y_pre[z == 1]) - 
(mean(y_post[z == 0]) - mean(y_pre[z == 0]))


###################################################
### code chunk number 9: dif:dif:Data:3
###################################################
lm(I(y_post - y_pre) ~ z) # I significa isolate, que aisla


###################################################
### code chunk number 10: output
###################################################
knitr::purl('FE_DifDif.Rnw')
Stangle('FE_DifDif.Rnw')


