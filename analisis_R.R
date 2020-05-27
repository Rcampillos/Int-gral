lectura <- c(915,825,723,623,515,390,288,190,110)
dd <- 0.5
dt <- 0.05
temp <- c(10.25,14.25,17.44,20.31,23.19,27.44,32.00,39.75,48.75)
temp2 <- c(10.25,15.80,18.30,20.70,23.30,29.12,34.00,40.70,45.70)
tempm <- seq(9)
for (value in seq(9)){
  media <- (temp[value]+temp2[value])/2
  tempm[value] <- media}

modelo <- lm(tempm ~ poly(lectura, 3, raw=TRUE))
plot(lectura,tempm)
arrows(x0=lectura-0.5, y0=tempm-0.005, x1=lectura+0.5, y1=tempm+0.05, code=3, angle=90, length=0.05)
lines(lectura,predict(modelo),col="green")
print(summary(modelo))
