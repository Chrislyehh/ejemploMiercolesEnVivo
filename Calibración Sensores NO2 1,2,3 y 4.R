library(dplyr)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("dplyr")
library(tidyr)
library(readr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
#install.packages("hms")
library(hms)
library(openair)
library(tidyverse)
library(xts)
library(stringr)
#install.packages("writexl")
library(writexl)
library(zoo)
#install.packages("imputeTS")


#### Extraction of data ####

C0 <- read.table("C:/Users/UDD/Desktop/Analisis de datos/Pruebas Alphasense/Calibracion/NO2/Sensores NO2 1_2_3_4/0ppb",
                 header = FALSE, sep = ";")
C1 <- read.table("C:/Users/UDD/Desktop/Analisis de datos/Pruebas Alphasense/Calibracion/NO2/Sensores NO2 1_2_3_4/80ppb",
                 header = FALSE, sep = ";")
C2 <- read.table("C:/Users/UDD/Desktop/Analisis de datos/Pruebas Alphasense/Calibracion/NO2/Sensores NO2 1_2_3_4/350ppb",
                 header = FALSE, sep = ";")
C3 <- read.table("C:/Users/UDD/Desktop/Analisis de datos/Pruebas Alphasense/Calibracion/NO2/Sensores NO2 1_2_3_4/500ppb",
                 header = FALSE, sep = ";")


C0 <- C0[,c("V2","V4","V6","V8","V10","V12","V14","V16")]
C1 <- C1[,c("V2","V4","V6","V8","V10","V12","V14","V16")]
C2 <- C2[,c("V2","V4","V6","V8","V10","V12","V14","V16")]
C3 <- C3[,c("V2","V4","V6","V8","V10","V12","V14","V16")]


colnames(C0) <- c("1_OP1","1_OP2","2_OP1","2_OP2","3_OP1","3_OP2","4_OP1","4_OP2")
colnames(C1) <- c("1_OP1","1_OP2","2_OP1","2_OP2","3_OP1","3_OP2","4_OP1","4_OP2")
colnames(C2) <- c("1_OP1","1_OP2","2_OP1","2_OP2","3_OP1","3_OP2","4_OP1","4_OP2")
colnames(C3) <- c("1_OP1","1_OP2","2_OP1","2_OP2","3_OP1","3_OP2","4_OP1","4_OP2")



Diferencia <- function(col1,col2){
  c1 = (col1-col2)
  return(c1)
}

C0$dif_1 <-Diferencia(C0$`1_OP1`,C0$`1_OP2`)
C0$dif_2 <-Diferencia(C0$`2_OP1`,C0$`2_OP2`)
C0$dif_3 <-Diferencia(C0$`3_OP1`,C0$`3_OP2`)
C0$dif_4 <-Diferencia(C0$`4_OP1`,C0$`4_OP2`)

C1$dif_1 <-Diferencia(C1$`1_OP1`,C1$`1_OP2`)
C1$dif_2 <-Diferencia(C1$`2_OP1`,C1$`2_OP2`)
C1$dif_3 <-Diferencia(C1$`3_OP1`,C1$`3_OP2`)
C1$dif_4 <-Diferencia(C1$`4_OP1`,C1$`4_OP2`)

C2$dif_1 <-Diferencia(C2$`1_OP1`,C2$`1_OP2`)
C2$dif_2 <-Diferencia(C2$`2_OP1`,C2$`2_OP2`)
C2$dif_3 <-Diferencia(C2$`3_OP1`,C2$`3_OP2`)
C2$dif_4 <-Diferencia(C2$`4_OP1`,C2$`4_OP2`)

C3$dif_1 <-Diferencia(C3$`1_OP1`,C3$`1_OP2`)
C3$dif_2 <-Diferencia(C3$`2_OP1`,C3$`2_OP2`)
C3$dif_3 <-Diferencia(C3$`3_OP1`,C3$`3_OP2`)
C3$dif_4 <-Diferencia(C3$`4_OP1`,C3$`4_OP2`)


##### Calcular Promedios ####

p0_1 <- mean(C0$dif_1, na.rm = TRUE)
p1_1 <- mean(C1$dif_1, na.rm = TRUE)
p2_1 <- mean(C2$dif_1, na.rm = TRUE)
p3_1 <- mean(C3$dif_1, na.rm = TRUE)

p0_2 <- mean(C0$dif_2, na.rm = TRUE)
p1_2 <- mean(C1$dif_2, na.rm = TRUE)
p2_2 <- mean(C2$dif_2, na.rm = TRUE)
p3_2 <- mean(C3$dif_2, na.rm = TRUE)

p0_3 <- mean(C0$dif_3, na.rm = TRUE)
p1_3 <- mean(C1$dif_3, na.rm = TRUE)
p2_3 <- mean(C2$dif_3, na.rm = TRUE)
p3_3 <- mean(C3$dif_3, na.rm = TRUE)

p0_4 <- mean(C0$dif_4, na.rm = TRUE)
p1_4 <- mean(C1$dif_4, na.rm = TRUE)
p2_4 <- mean(C2$dif_4, na.rm = TRUE)
p3_4 <- mean(C3$dif_4, na.rm = TRUE)

Calibracion <- data.frame(
  referencia = c(0,0.0638,0.364,0.534),
  señal_1 = c(p0_1,p1_1,p2_1,p3_1),
  señal_2 = c(p0_2,p1_2,p2_2,p3_2),
  señal_3 = c(p0_3,p1_3,p2_3,p3_3),
  señal_4 = c(p0_4,p1_4,p2_4,p3_4)
)


#### Gráfico de Curvas #### 

plot(Calibracion$referencia, Calibracion$señal_1, type = "line", col = "blue", 
     ylim = range(c(Calibracion$señal_1, Calibracion$señal_2)), ylab = "Señal Sensores", xlab = "Concentración NO2")
lines(Calibracion$referencia, Calibracion$señal_2, col = "red")
lines(Calibracion$referencia, Calibracion$señal_3, col = "green")
lines(Calibracion$referencia, Calibracion$señal_4, col = "brown")
legend("bottomright", legend = c("Sensor 1", "Sensor 2", "Sensor 3", "Sensor 4"), col = c("blue", "red", "green","brown"), lty = 3)


######################################## Ajuste de curva sensores ############################
# Ajuste lineal para sensor1
modelo1 <- lm(referencia ~ señal_1, data = Calibracion)
summary(modelo1)

# Ajuste lineal para sensor2
modelo2 <- lm(referencia ~ señal_2, data = Calibracion)
summary(modelo2)

# Mostrar ecuaciones y R2
coef1 <- coef(modelo1)
r2_1 <- summary(modelo1)$r.squared
cat("Sensor 1:/n")
cat("Ecuacion: concentracion =", coef1[1], "+", coef1[2], "* sensor1/n")
cat("R² =", r2_1, "/n/n")

coef2 <- coef(modelo2)
r2_2 <- summary(modelo2)$r.squared
cat("Sensor 2:/n")
cat("Ecuacion: concentracion =", coef2[1], "+", coef2[2], "* sensor2/n")
cat("R² =", r2_2, "/n")

ggplot(Calibracion, aes(x = referencia, y = señal_1)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Ajuste lineal - Sensor 5",
       x = "Concentración NO",
       y = "Sensor 3") +
  theme_minimal()

ggplot(Calibracion, aes(x = referencia, y = señal_2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Ajuste lineal - Sensor 6",
       x = "Concentración NO",
       y = "Sensor 4") +
  theme_minimal()
