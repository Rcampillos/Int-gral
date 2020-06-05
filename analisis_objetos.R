#Funciones
moda <- function(x, useNA = "no") {          
  xtabla <- table(x, useNA = useNA)
  names(xtabla[xtabla == max(xtabla)])
}
library("RColorBrewer")
#Lectura datos
datos_sensacion <- read.csv("datos_sensacion.csv", fileEncoding = "UTF-8")
#Introducimos temperaturas de los objetos y algunas variables de limites
temp_Al_caliente <- 40
temp_Al_frio <- 12
temp_madera_caliente <- 34
temp_madera_fria <- 18

factores <- c("Muy frío","Frío","Neutral","Caliente","Muy caliente")
factores <- factor(factores, levels = factores)
paleta_factores <- rev(brewer.pal(n = 5, name = 'RdBu'))
objeto <- c("Frío", "Caliente")
objeto <- factor(objeto, levels = objeto)
paleta_objeto <- rev(c("#EF8A62", "#67A9CF"))

#Hacemos un print resumiendo los datos medios y las varianzas de cada dato
print("Temperatura de aluminio caliente")
print(c("Temperatura medida", mean(datos_sensacion$Treal.Al.C, na.rm = TRUE),"Std Err", sd(datos_sensacion$Treal.Al.C, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_sensacion$Test.Al.C,na.rm = TRUE), "Std Err", sd(datos_sensacion$Test.Al.C, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_sensacion$t.Al.C,na.rm = TRUE), "Std Err", sd(datos_sensacion$t.Al.C, na.rm = TRUE)))
print("Rango cualitativo asignado")
print(table(datos_sensacion$Rango.Al.C)/sum(table(datos_sensacion$Rango.Al.C)))
print(c("Moda de la confianza",moda(datos_sensacion$Conf.Al.C, useNA = "no")))

print("Temperatura de aluminio frío")
print(c("Temperatura medida", mean(datos_sensacion$Treal.Al.F, na.rm = TRUE),"Std Err", sd(datos_sensacion$Treal.Al.F, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_sensacion$Test.Al.F,na.rm = TRUE), "Std Err", sd(datos_sensacion$Test.Al.F, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_sensacion$t.Al.F,na.rm = TRUE), "Std Err", sd(datos_sensacion$t.Al.F, na.rm = TRUE)))
print("Rango cualitativo asignado")
print(table(datos_sensacion$Rango.Al.F)/sum(table(datos_sensacion$Rango.Al.F)))
print(c("Moda de la confianza",moda(datos_sensacion$Conf.Al.F, useNA = "no")))

print("Temperatura de madera caliente")
print(c("Temperatura medida", mean(datos_sensacion$Treal.Mad.C, na.rm = TRUE),"Std Err", sd(datos_sensacion$Treal.Mad.C, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_sensacion$Test.Mad.C,na.rm = TRUE), "Std Err", sd(datos_sensacion$Test.Mad.C, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_sensacion$t.Mad.C,na.rm = TRUE), "Std Err", sd(datos_sensacion$t.Mad.C, na.rm = TRUE)))
print("Rango cualitativo asignado")
print(table(datos_sensacion$Rango.Mad.C)/sum(table(datos_sensacion$Rango.Mad.C)))
print(c("Moda de la confianza",moda(datos_sensacion$Conf.Mad.C, useNA = "no")))

print("Temperatura de madera fría")
print(c("Temperatura medida", mean(datos_sensacion$Treal.Mad.F, na.rm = TRUE),"Std Err", sd(datos_sensacion$Treal.Mad.F, na.rm = TRUE)))
print(c("Temperatura estimada", mean(datos_sensacion$Test.Mad.F,na.rm = TRUE), "Std Err", sd(datos_sensacion$Test.Mad.F, na.rm = TRUE)))
print(c("Tiempo de respuesta", mean(datos_sensacion$t.Mad.F,na.rm = TRUE), "Std Err", sd(datos_sensacion$t.Mad.F, na.rm = TRUE)))
print("Rango cualitativo asignado")
print(table(datos_sensacion$Rango.Mad.F)/sum(table(datos_sensacion$Rango.Mad.F)))
print(c("Moda de la confianza",moda(datos_sensacion$Conf.Mad.F, useNA = "no")))

#Datos Madera
temp_objeto_real_Mad <- cbind(datos_sensacion$Treal.Mad.F,datos_sensacion$Treal.Mad.C)
temp_objeto_estimada_Mad <- cbind(datos_sensacion$Test.Mad.F,datos_sensacion$Test.Mad.C)
t_respuesta_Mad <- cbind(datos_sensacion$t.Mad.F,datos_sensacion$t.Mad.C)

#Datos Aluminio
temp_objeto_real_Al <- cbind(datos_sensacion$Treal.Al.F,datos_sensacion$Treal.Al.C)
temp_objeto_estimada_Al <- cbind(datos_sensacion$Test.Al.F,datos_sensacion$Test.Al.C)
t_respuesta_Al <- cbind(datos_sensacion$t.Al.F,datos_sensacion$t.Al.C)

#Gráfico tipo box and whiskers con las medidas de aluminio
boxplot(temp_objeto_real_Al, main="Temperatura registrada por el dispositivo equivalente a la sensación termoceptiva\n de una pieza de aluminio en dos estados térmicos", ylab="Temperatura (ºC)", xlab="Estados cualitativos de temperatura", sub="Marcados en discontínuo las temperaturas reales del objeto", ylim=c(10,50), xaxt = "n", col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura media","Temp. caliente", "Temp. frío"),col=c("black","black","red","blue"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=temp_Al_caliente,col="red",lty=2)
abline(h=temp_Al_frio,col="blue",lty=2)
points(colMeans(temp_objeto_real_Al, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con las medidas de madera
boxplot(temp_objeto_real_Mad, main="Temperatura registrada por el dispositivo equivalente a la sensación termoceptiva\n de una pieza de madera en dos estados térmicos", ylab="Temperatura (ºC)", xlab="Estados cualitativos de temperatura", sub="Marcados en discontínuo las temperaturas reales del objeto", ylim=c(10,50), xaxt = "n", col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura media","Temp. caliente", "Temp. frío"),col=c("black","black","red","blue"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=temp_madera_caliente,col="red",lty=2)
abline(h=temp_madera_fria,col="blue",lty=2)
points(colMeans(temp_objeto_real_Mad, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con las medidas de aluminio
boxplot(temp_objeto_estimada_Al, main="Temperatura cuantitativa estimada por los sujetos sobre la sensación termoceptiva\n de una pieza de aluminio en dos estados térmicos", ylab="Temperatura (ºC)", xlab="Estados cualitativos de temperatura", sub="Marcados en discontínuo las temperaturas reales del objeto", ylim=c(-10,50), xaxt = "n", col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura cuantitativa media","Temp. caliente", "Temp. frío"),col=c("black","black","red","blue"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=temp_Al_caliente,col="red",lty=2)
abline(h=temp_Al_frio,col="blue",lty=2)
points(colMeans(temp_objeto_estimada_Al, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tipo box and whiskers con las medidas de madera
boxplot(temp_objeto_estimada_Mad, main="Temperatura cuantitativa estimada por los sujetos sobre la sensación termoceptiva\n de una pieza de madera en dos estados térmicos", ylab="Temperatura (ºC)", xlab="Estados cualitativos de temperatura", sub="Marcados en discontínuo las temperaturas reales del objeto", ylim=c(-10,50), xaxt = "n", col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana de la temperatura","Temperatura media","Temp. caliente", "Temp. frío"),col=c("black","black","red","blue"),lty=c(1,0,2,2),lwd=c(2,1,1,1),pch=c(NA,17,NA,NA),pt.cex = 2)
abline(h=temp_madera_caliente,col="red",lty=2)
abline(h=temp_madera_fria,col="blue",lty=2)
points(colMeans(temp_objeto_estimada_Mad, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tiempos de respuesta por material Al
boxplot(t_respuesta_Al, main="Tiempo de respuesta sobre la sensación de una pìeza de Al", ylab="Tiempo (s)", xlab="Categoría de temperatura", xaxt = "n", ylim=c(0,50), col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana del tiempo","Tiempo medio"),col=c("black","black"),lty=c(1,0),lwd=c(2,1),pch=c(NA,17),pt.cex = 2)
points(colMeans(t_respuesta_Al, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tiempos de respuesta por material Madera
boxplot(t_respuesta_Mad, main="Tiempo de respuesta sobre la sensación de una pìeza de Madera", ylab="Tiempo (s)", xlab="Categoría de temperatura", xaxt = "n", ylim=c(0,50), col=paleta_objeto)
axis(1, at=1:2, labels=objeto)
legend("topleft",legend=c("Mediana del tiempo","Tiempo medio"),col=c("black","black"),lty=c(1,0),lwd=c(2,1),pch=c(NA,17),pt.cex = 2)
points(colMeans(t_respuesta_Mad, na.rm=TRUE), pch = 17, cex=2)

#Gráfico tiempos de respuesta por categoría
boxplot(cbind(t_respuesta_Al[,1],t_respuesta_Mad[,1]), main="Tiempo de respuesta sobre la sensación de una pìeza fría", ylab="Tiempo (s)", xlab="Categoría de temperatura", xaxt = "n", ylim=c(0,50), col=c("azure2","burlywood"))
axis(1, at=1:2, labels=c("Aluminio","Madera"))
legend("topleft",legend=c("Mediana del tiempo","Tiempo medio"),col=c("black","black"),lty=c(1,0),lwd=c(2,1),pch=c(NA,17),pt.cex = 2)
points(colMeans(cbind(t_respuesta_Al[,1],t_respuesta_Mad[,1]), na.rm=TRUE), pch = 17, cex=2)
#Gráfico tiempos de respuesta por categoría
boxplot(cbind(t_respuesta_Al[,2],t_respuesta_Mad[,2]), main="Tiempo de respuesta sobre la sensación de una pìeza caliente", ylab="Tiempo (s)", xlab="Categoría de temperatura", xaxt = "n", ylim=c(0,50), col=c("azure2","burlywood"))
axis(1, at=1:2, labels=c("Aluminio","Madera"))
legend("topleft",legend=c("Mediana del tiempo","Tiempo medio"),col=c("black","black"),lty=c(1,0),lwd=c(2,1),pch=c(NA,17),pt.cex = 2)
points(colMeans(cbind(t_respuesta_Al[,2],t_respuesta_Mad[,2]), na.rm=TRUE), pch = 17, cex=2)

#Gráfico categorización en rangos cualitativos
a<- table(factor(datos_sensacion$Rango.Mad.F, levels = factores))/sum(table(datos_sensacion$Rango.Mad.F))
b<- table(factor(datos_sensacion$Rango.Mad.C, levels = factores))/sum(table(datos_sensacion$Rango.Mad.C))
c<- table(factor(datos_sensacion$Rango.Al.F, levels = factores))/sum(table(datos_sensacion$Rango.Al.F))
d<- table(factor(datos_sensacion$Rango.Al.C, levels = factores))/sum(table(datos_sensacion$Rango.Al.C))

barplot(as.matrix(cbind(c,a,b,d))*100, main="Categorías cualitativas asignadas a cada objeto", ylab="Porcentaje (%)", xlab="Objeto", xaxt = "n", col=paleta_factores)
legend("top", fill = paleta_factores, legend = factores, horiz = TRUE, inset = c(0,1.1), xpd = TRUE)
axis(1, at=1:4, labels=c("Aluminio frío","Madera fría","Madera caliente","Aluminio caliente"))

