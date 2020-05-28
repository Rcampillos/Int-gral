# Datos calibración de equilibrio
t <-c(0:30)
temp <-c(24.69, 25.06, 26.19, 27.69, 29.00, 30.25, 31.12, 31.87, 32.44, 32.50, 32.75, 33.00, 33.31, 33.38, 33.56, 33.56, 33.44, 33.31, 33.31, 33.25, 33.13, 33.06, 33.00, 32.94, 32.84, 32.88, 32.88, 32.44, 32.81, 32.88, 32.63)
dt <- 2/60
dtemp <- 0.5
plot(t,temp, main="Evolución de la temperatura", sub="Condiciones: 25ºC ambientales, HR 30-40%", xlab="Tiempo (minutos)", ylab="Temperatura (ºC)",ylim=c(22,34))
arrows(x0=t-dt, y0=temp, x1=t+dt, y1=temp, code=3, angle=90, length=0.01)
arrows(x0=t, y0=temp-dtemp, x1=t, y1=temp+dtemp, code=3, angle=90, length=0.01)
abline(h=32.8, lty=2)
abline(v=20, lty=2)
legend("bottomright", legend=c("T. en el centro"),col="black", pch=1, cex=0.8)
# Datos de calibración t_amb=25ºC HR 40%
lectura <- c(908, 832, 739, 640, 535, 401, 307, 208, 145)
temp_cal <- c(13.4, 19.12, 22.75, 26.62, 30.62, 34.38, 39.94, 45.81, 52.70)
#Errores datos
delta_t <- 0.5
delta_l <- 5
#Ajuste a polinomio grado 3, escogido tras ver datos
#Mejora el ajuste en los bordes al tener efecto más acusado
modelo_poli <- lm(temp_cal ~ poly(lectura, 3, raw=TRUE))
#Gráfica de datos y errores
plot(lectura,temp_cal, main="Curva de calibración del gradiente de temperatura", sub="Condiciones: 25ºC ambientales, HR 30-40%", xlab="Lectura sensor lineal (Unidades)", ylab="Temperatura (ºC)", ylim=c(10,60))
arrows(x0=lectura-delta_l, y0=temp_cal, x1=lectura+delta_l, y1=temp_cal, code=3, angle=90, length=0.01)
arrows(x0=lectura, y0=temp_cal-delta_t, x1=lectura, y1=temp_cal+delta_t, code=3, angle=90, length=0.01)
#Gráfica modelo y print datos
lines(lectura,predict(modelo_poli),col="green")
print("Datos ajuste polinómico")
print(summary(modelo_poli))
#Ajuste a un modelo lineal simple
modelo_lin <- lm(temp_cal ~ poly(lectura, 1, raw=TRUE))
#Gráfica modelo y print datos
lines(lectura,predict(modelo_lin),col="red")
print("Datos ajuste lineal")
print(summary(modelo_lin))
legend("topright", legend=c("Datos calibración", "Ajuste lineal", "Ajuste poli. grad. 3"),col=c("black", "red", "green"), lty=c(0,1,1), pch=c(1,NaN,NaN), cex=0.8)