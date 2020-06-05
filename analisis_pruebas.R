#Funciones
moda <- function(x, useNA = "no") {          
  xtabla <- table(x, useNA = useNA)
  names(xtabla[xtabla == max(xtabla)])
}
library("RColorBrewer")
#Abrimos los datos
datos_escala <- read.csv("datos_escala.csv", fileEncoding = "UTF-8")
factores <- c("Muy frío a frío","Frío a neutral","Neutral a caliente","Caliente a muy caliente","Muy caliente a dolor")
factores <- factor(factores, levels = factores)
paleta_factores <- rev(brewer.pal(n = 5, name = 'RdBu'))

#Hacemos un print resumiendo los datos medios y las varianzas de cada dato
print("Temperatura media paso muy frío a frío")
print(c("Temperatura medida", mean(datos_escala$Treal.MF2F, na.rm = TRUE),"Std Err", sd(datos_escala$Treal.MF2F, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_escala$Test.MF2F,na.rm = TRUE), "Std Err", sd(datos_escala$Test.MF2F, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_escala$tres.MF2F,na.rm = TRUE), "Std Err", sd(datos_escala$tres.MF2F, na.rm = TRUE)))
print(c("Moda de la confianza",moda(datos_escala$Conf.MF2F, useNA = "no")[[1]]))

print("Temperatura media paso frío a neutral")
print(c("Temperatura medida", mean(datos_escala$Treal.F2N,na.rm = TRUE),"Std Err", sd(datos_escala$Treal.F2N, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_escala$Test.F2N,na.rm = TRUE), "Std Err", sd(datos_escala$Test.F2N, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_escala$tres.F2N,na.rm = TRUE), "Std Err", sd(datos_escala$tres.F2N, na.rm = TRUE)))
print(c("Moda de la confianza",moda(datos_escala$Conf.F2N, useNA = "no")))

print("Temperatura media paso neutral a caliente")
print(c("Temperatura medida", mean(datos_escala$Treal.N2C,na.rm = TRUE),"Std Err", sd(datos_escala$Treal.N2C, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_escala$Test.N2C,na.rm = TRUE), "Std Err", sd(datos_escala$Test.N2C, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_escala$tres.N2C,na.rm = TRUE), "Std Err", sd(datos_escala$tres.N2C, na.rm = TRUE)))
print(c("Moda de la confianza",moda(datos_escala$Conf.N2C, useNA = "no")))

print("Temperatura media paso caliente a muy caliente")
print(c("Temperatura medida", mean(datos_escala$Treal.C2MC,na.rm = TRUE),"Std Err", sd(datos_escala$Treal.C2MC, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_escala$Test.C2MC,na.rm = TRUE), "Std Err", sd(datos_escala$Test.C2MC, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_escala$tres.C2MC,na.rm = TRUE), "Std Err", sd(datos_escala$tres.C2MC, na.rm = TRUE)))
print(c("Moda de la confianza",moda(datos_escala$Conf.C2MC, useNA = "no")))

print("Temperatura media paso muy caliente a dolor")
print(c("Temperatura medida", mean(datos_escala$Treal.MC2D,na.rm = TRUE),"Std Err", sd(datos_escala$Treal.MC2D, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_escala$Test.MC2D,na.rm = TRUE), "Std Err", sd(datos_escala$Test.MC2D, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_escala$tres.MC2D,na.rm = TRUE), "Std Err", sd(datos_escala$tres.MC2D, na.rm = TRUE)))
print(c("Moda de la confianza",moda(datos_escala$Conf.MC2D, useNA = "no")))

#Vamos a relacionar o presentar variables interesantes
#Transformamos las medias de las columnas de cada dato en vectores
medias_escala <- unname(colMeans(datos_escala, na.rm = TRUE, dims = 1))
temperaturas_escala_reales <- cbind(datos_escala$Treal.MF2F,datos_escala$Treal.F2N,datos_escala$Treal.N2C,datos_escala$Treal.C2MC,datos_escala$Treal.MC2D)
temperaturas_escala_estimadas <- cbind(datos_escala$Test.MF2F,datos_escala$Test.F2N,datos_escala$Test.N2C,datos_escala$Test.C2MC,datos_escala$Test.MC2D)
tiempo_respuesta_escala <- cbind(datos_escala$tres.MF2F,datos_escala$tres.F2N,datos_escala$tres.N2C,datos_escala$tres.C2MC,datos_escala$tres.MC2D)
confianza_escala <- c(moda(datos_escala$Conf.MF2F, useNA = "no")[[1]],moda(datos_escala$Conf.F2N, useNA = "no"),moda(datos_escala$Conf.N2C, useNA = "no"),moda(datos_escala$Conf.C2MC, useNA = "no"),moda(datos_escala$Conf.MC2D, useNA = "no"))
#Diferencia entre la temperatura sentida y la estimada
dif_temp <- (temperaturas_escala_estimadas - temperaturas_escala_reales)

#Gráfico tipo box and whiskers con la media de la temperatura sentida
boxplot(temperaturas_escala_reales, main="Temperatura registrada de la sensación termoceptiva frente de\n a los puntos de cambio de la escala cualitativa", ylab="Temperatura (ºC)", xlab="Puntos frontera en la escala cualitativa de temperaturas", ylim=c(-10,70), yaxp  = c(-10, 70, 16), xaxt = "n", col=paleta_factores)
axis(1, at=1:5, labels=factores, cex.axis=0.8)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura media","Límite hasta el dolor", "Límite hasta neutral"),col=c("black","black","red","green"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=43,col="red",lty=2)
abline(h=17,col="red",lty=2)
abline(h=30,col="green",lty=2)
abline(h=36,col="green",lty=2)
points(colMeans(temperaturas_escala_reales, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con la media de la temperatura declarada
boxplot(temperaturas_escala_estimadas, main="Temperatura cuantitativa estimada en la sensación termoceptiva por los sujetos\n frente a los puntos de cambio de la escala cualitativa", ylab="Temperatura (ºC)", xlab="Puntos frontera en la escala cualitativa de temperaturas", yaxp  = c(-10, 70, 16), ylim=c(-10,70), xaxt = "n", col=paleta_factores)
axis(1, at=1:5, labels=factores, cex.axis=0.8)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura media","Límite hasta el dolor", "Límite hasta neutral"),col=c("black","black","red","green"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=43,col="red",lty=2)
abline(h=17,col="red",lty=2)
abline(h=30,col="green",lty=2)
abline(h=36,col="green",lty=2)
points(colMeans(temperaturas_escala_estimadas, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con la anomalía entre respuestas
boxplot(dif_temp, main="Diferencia entre temperatura recogida por el sistema\n y la temperatura estimada para el mismo estímulo termocepctivo", ylab="Temperatura (ºC)", xlab="Escala cuantitativa", ylim=c(-30,30), xaxt = "n", col=paleta_factores)
axis(1, at=1:5, labels=factores, cex.axis=0.8)
abline(h=0,lty=2)
legend("topleft",legend=c("Mediana de la anomalía","Anomalía media"),col="black",lty=c(1,0),lwd=c(2,1), pch=c(NA,17), pt.cex=2)
points(colMeans(dif_temp, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con la media del tiempo de respuesta
boxplot(tiempo_respuesta_escala, main="Tiempo de respuesta en la identificación de los puntos\n de cambio entre categorías en la escala cualitativa", ylab="Tiempo (s)", xlab="Puntos frontera en la escala cualitativa de temperaturas", xaxt = "n", col=paleta_factores)
axis(1, at=1:5, labels=factores, cex.axis=0.8)
legend("topleft",legend=c("Mediana del tiempo","Tiempo medio"),col="black",lty=c(1,0),lwd=c(2,1), pch=c(NA,17), pt.cex=2)
points(colMeans(tiempo_respuesta_escala, na.rm=TRUE), pch = 17, cex=2)

#Gráfico de anomalía frente a tiempo ¿hay relación?
plot(colMeans(tiempo_respuesta_escala, na.rm=TRUE), colMeans(dif_temp, na.rm=TRUE),main="Anomalía entre sensación y conceptualización\n frente al tiempo de respuesta usado", ylab="Anomalía (ºC)", xlab="Tiempo (s)", ylim=c(-25,15), cex=2, pch=4,lwd=2)
ajuste_t_dif <- lm(colMeans(dif_temp, na.rm=TRUE) ~ colMeans(tiempo_respuesta_escala, na.rm=TRUE))
abline(ajuste_t_dif,lwd=2,lty=5)
#Datos del ajuste lineal
print(summary(ajuste_t_dif))
legend("topright",legend=c("Anomalía media en T","Ajuste lineal con el tiempo medio"),col=c("black","black"),lty=c(0,5), pch=c(4,NA), pt.cex = 2, lwd=2)

#Gráfico del nivel de confianza
plot(factores,confianza_escala, main="Nivel de confianza modal en la reespuesta", xlab="Escala cualitativa", ylab="Nivel de confianza", xaxt = "n", yaxt = "n", ylim=c(1,4), type="h", col="grey60", lwd=40, lend=1)
axis(1, at=1:5, labels=factores, cex.axis=0.8)
axis(2, at=1:4, labels=c("Muy poco seguro","Poco seguro","Algo seguro","Muy seguro"), cex.axis=0.8)
