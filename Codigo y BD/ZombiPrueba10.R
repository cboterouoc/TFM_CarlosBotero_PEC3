
# UOC - Trabajo de Fin de Master
# Empresas Zombi de Colombia

# Carlos Alberto Botero velez
# 21/03/2021

# Objetivo
# El objetivo principal del presente TFM es el de predecir la población de empresas Zombi Colombianas,
# con base en las partidas contables registradas de 13.357 empresas durante los años 2017, 2018 y 2019.

# Alcance
# años 2017, 2018 y 2019

# Limpieza de datos:
# * Errores sintácticos
# * Normalizar/estandarizar variables
# * Valores atípicos (outliers)
# * Valores perdidos (missing)


library(car)
library(caret)         # Para particionar datos
library(caTools)
library(class)
library(data.table)
library(descr)
library(devtools)
library(doBy)
library(doParallel)
library(dplyr)        # select filter mutate ...
library(dummies)
library(e1071)
library(effects)
library(factoextra)
library(FactoMineR)
library(fdth)         # Para tablas de distribución y frecuencias
library(funModeling)
library(gbm)          # basic implementation
library(GGally)
library(ggeffects)    # efectos en modelos de regresion
library(ggfortify)
library(ggplot2)      # Gráficas
library(ggpubr)
library(ggrepel)
library(gmodels)
library(h2o)          # a java-based platform
library(haven)        # datos en formato .dta
library(highcharter)
library(Hmisc)
library(ISLR)
library(kableExtra)
library(kknn)
library(knitr)         # Para ver tablas mas amigables en formato html markdown
library(lime)          # model visualization
library(lme4)
library(lubridate)
library(MASS)
library(modeest)
library(moments)
library(mosaic)
library(MuMIn)
library(naivebayes)
library(openintro)
library(openxlsx)
library(parallel)
library(party)
library(partykit)
library(pbkrtest)
library(pdp)           # model visualization
library(PerformanceAnalytics)
library(plotly)
library(plotrix)
library(plyr)
library(pROC)
library(psych)
library(purrr)
library(readr)
library(readxl)
library(reshape)       # Para renombrar columnas en caso de necesitarse
library(RLRsim)
library(ROCR)
library(ROSE)
library(rpart)
library(rpart.plot)
library(rsample)      # data splitting 
library(RUnit)
library(scales)       # Para escalar datos
library(sjPlot)       # tablas de regresion
library(sm)
library(tableone)
library(tables)       # Presentación de tablas
library(tidyverse)    # varias
library(tree)
library(tseries)
library(vcd)
library(writexl)
library(xgboost)      # a faster implementation of gbm

install.packages("xlsx")
getwd()


# **************************************************************************************************************
#
# 3.    Diseño e implementación del TFM
# 3.1.  Análisis de la estructura de los datos y la información de la base de datos
# 3.1.1	Recolección y descripción de datos iniciales
#
# ******************************************************************************
# CRISP_DM - 2. Fase de comprensión de los datos
# ******************************************************************************

# Datos de la empresa - Variables exógenas y endógenas

DatosEmpresa <- openxlsx::read.xlsx("DatosEmpresa.xlsx")
sapply(DatosEmpresa, function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 
DatosEmpresa <- DatosEmpresa %>% drop_na()
View(DatosEmpresa)

# Identificar el tipo de variables (numéricas o categóricas)

str(DatosEmpresa)

# Como se puede apreciar las variables Tipología, CIIU, FJ, Tamaño, Departamento
# y Localidad son categóricas.

DatosEmpresa$Tipologia    <- as.factor(DatosEmpresa$Tipologia)
DatosEmpresa$CIIU         <- as.factor(DatosEmpresa$CIIU)
DatosEmpresa$FJ           <- as.factor(DatosEmpresa$FJ)
DatosEmpresa$Tamaño       <- as.factor(DatosEmpresa$Tamaño)
DatosEmpresa$Departamento <- as.factor(DatosEmpresa$Departamento)
DatosEmpresa$Localidad    <- as.factor(DatosEmpresa$Localidad)

sapply(DatosEmpresa, function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 

DatosEmpresa01 <- DatosEmpresa

# DatosEmpresa01 - El código se divide en categoría y código

DatosEmpresa01$CIIUCat <- as.factor(substr(DatosEmpresa01$CIIU, 1, 1))
DatosEmpresa01$CIIUCod <- as.numeric(substr(DatosEmpresa01$CIIU, 2, 5))
str(DatosEmpresa01)

# ******************************************************************************
# Agrupamiento de datos 01 : Empleados
# ******************************************************************************

nclass.scott(DatosEmpresa01$Empleados)
k <- nclass.Sturges(DatosEmpresa01$Empleados)
k
nclass.FD(DatosEmpresa01$Empleados)

# calculamos la amplitud

amplitud <- diff(range(DatosEmpresa01$Empleados))/k
amplitud
amplitud <- 2450

# calcular los extremos de los intervalos y las marcas de clase

m <- min(DatosEmpresa01$Empleados)
L <- m-0.05+amplitud*(0:k)
L
marcas <- (L[0:k]+L[1:k+1])/2
marcas
Empleados <- c("1", "3", "6", "8", "11", "13", "15",
               "18", "20", "23", "25", "28", "30", "33",
               "35")

largo.cut <- cut(DatosEmpresa01$Empleados, breaks = L, labels = Empleados, right = FALSE)
largo.cut
class(largo.cut)
DatosEmpresa01$Empleados <- largo.cut

str(DatosEmpresa01)

# ******************************************************************************
# Agrupamiento de datos 02 : CIIUCod
# ******************************************************************************

nclass.scott(DatosEmpresa01$CIIUCod)
k <- nclass.Sturges(DatosEmpresa01$CIIUCod)
k
nclass.FD(DatosEmpresa01$CIIUCod)

# calculamos la amplitud

amplitud <- diff(range(DatosEmpresa01$CIIUCod))/k
amplitud
amplitud <- 633

# calcular los extremos de los intervalos y las marcas de clase

m <- min(DatosEmpresa01$CIIUCod)
L <- m-0.05+amplitud*(0:k)
L
marcas <- (L[0:k]+L[1:k+1])/2
marcas
CIIUCod <- c("4", "10", "16", "23", "29", "35", "42",
             "48", "54", "61", "67", "73", "80", "86",
             "92")

largo.cut <- cut(DatosEmpresa01$CIIUCod, breaks = L, labels = CIIUCod, right = FALSE)
largo.cut
class(largo.cut)
DatosEmpresa01$CIIUCod <- largo.cut
str(DatosEmpresa01)


# ******************************************************************************
# Agrupamiento de datos 03 : AnioConstitucion
# ******************************************************************************

nclass.scott(DatosEmpresa01$AnioConstitucion)
k <- nclass.Sturges(DatosEmpresa01$AnioConstitucion)
k
nclass.FD(DatosEmpresa01$AnioConstitucion)

# calculamos la amplitud

amplitud <- diff(range(DatosEmpresa01$AnioConstitucion))/k
amplitud
amplitud <- 9

# calcular los extremos de los intervalos y las marcas de clase

m <- min(DatosEmpresa01$AnioConstitucion)
L <- m-0.05+amplitud*(0:k)
L
marcas <- (L[0:k]+L[1:k+1])/2
marcas
AnioConstitucion <- c("1", "2", "3", "4", "5", "6", "7",
                      "8", "9", "10", "11", "12", "13", "14",
                      "15")

largo.cut <- cut(DatosEmpresa01$AnioConstitucion, breaks = L, labels = AnioConstitucion, right = FALSE)
largo.cut
class(largo.cut)
DatosEmpresa01$AnioConstitucion <- largo.cut
str(DatosEmpresa01)


# ******************************************************************************
# Funcion para convertir las variables tipo factor a tipo numerico
# ******************************************************************************

indx <- sapply(DatosEmpresa01, is.factor)
DatosEmpresa01[indx] <- lapply(DatosEmpresa01[indx], function(x){ 
  listOri <- unique(x)
  listMod <- seq_along(listOri)
  res <- factor(x, levels=listOri)
  res <- as.numeric(res)
  return(res)
}
)

str(DatosEmpresa01)

# Eliminar filas con nulos en una columna concreta

sapply(DatosEmpresa01, function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 
DatosEmpresa01 <- DatosEmpresa01 %>% drop_na()
str(DatosEmpresa01)


# ******************************************************************************
# Leer datos de 2017, 2018 y 2019 e identificar el tipo de variables
# ******************************************************************************

ConsolidadoDatos <- openxlsx::read.xlsx("Integrado_17_18_19.xlsx")
View(ConsolidadoDatos)

# ******************************************************************************
# Se ajustan los campos null con ceros
# ******************************************************************************

ConsolidadoDatos[is.na(ConsolidadoDatos)] <- 0

# Dados los anteriores ajustes a los datos se acutaliza la base de datos original

write_xlsx(ConsolidadoDatos,"D:/CABV/MasterUOC/TFM/TFM UOC/PEC3/Datos Entrega\\ConDatosV.xlsx")
ConDatosV <- ConsolidadoDatos


# ******************************************************************************
#
# CRISP_DM 2. Fase de comprensión de los datos
# 3.1.2	Exploración de datos - Histogramas de frecuencias
#
# ******************************************************************************

# Histogramas de frecuencias para la variable DatosEmpresa01$Tipologia
# ******************************************************************************

plot(x = DatosEmpresa01$Tipologia, main = "Distribución de Tipología de Empresas",
     xlab = "", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "yellow"))

# Como se aprecia en la gráfica, la mayor concentración de empresas se centra en la
# tipología Sociedad Comercial/Industrial


# Histogramas de frecuencias para la variable DatosEmpresa01$FJ
# ******************************************************************************

plot(x = DatosEmpresa01$FJ, las = 2, main = "Distribución forma jurídica y actividad de la empresa",
     xlab = "", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey", "yellow"))

# Para el caso de la forma jurídica y la actividad de la empresa, la mayor concentración
# se evidencia en la "sociedad por acciones simplificada - SAS" y en segundo lugar
# la "Sociedad anónima".


# Histogramas de frecuencias para la variable DatosEmpresa01$Tamaño
# ******************************************************************************

plot(x = DatosEmpresa01$Tamaño, las = 2, main = "Distribución Tamaño de la Empresa",
     xlab = "Tamaño", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey", "yellow"))

# Con respecto al tamaño de la empresas de la base de datos, un importante porcentaje
# se centra en las empresas medianas - MD y las grandes - GR.


# Histogramas de frecuencias para la variable DatosEmpresa01$Departamento
# ******************************************************************************

plot(x = DatosEmpresa01$Departamento, las = 2, main = "Distribución por Departamento",
     xlab = "", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey", "yellow"))

# La concentración de las empresas se encuentra en Bogotá, Antioquia y Valle.


# Histogramas de frecuencias para las variables Departamento, Tamaño y FJ
# ******************************************************************************

DepartTamanio <- data.frame(DatosEmpresa01$Departamento, DatosEmpresa01$Tamaño, DatosEmpresa01$FJ)

ggplot(DepartTamanio, aes(x=DatosEmpresa01.Tamaño, fill=DatosEmpresa01.Departamento))+
  geom_bar(position = "dodge")

ggplot(DepartTamanio, aes(x=DatosEmpresa01.Tamaño, fill=DatosEmpresa01.FJ))+
  geom_bar(position = "dodge")


# Distribución por Número de Empleados
# ******************************************************************************

datos <- as.numeric(DatosEmpresa01$Empleados)

# Haciendo uso de la fórmula de Sturges se calcula del número de clases para construir
# el Histograma de los datos

CantEmpresas   <- 13357
Sturges        <- round(1 + (3.322)*log10(CantEmpresas))
freq.tab       <- function(data, n.int) {
  raw.tab      <- binning(data, nbins = n.int)
  tab          <- list()
  tab$intleft  <- raw.tab$breaSturgess[1:n.int]
  tab$intright <- raw.tab$breaSturgess[2:(n.int+1)]
  tab$Smc      <- raw.tab$x
  tab$Sfreq    <- raw.tab$table.freq
  tab$freqrel  <- raw.tab$table.freq / length(data)
  tab$freqacum <- cumsum(raw.tab$table.freq / length(data))
  return(tab)
}

# Se eliminan los datos nulos

CantEmpleados <- na.omit(datos)
freq.tab(CantEmpleados, 15)
df <- freq.tab(CantEmpleados, 15)

# Se generan clases para la cantidad de empleados

ClasesEmpleados <- cut(datos, breaks = 100*(0:10))

plot(x = ClasesEmpleados, las = 2, main = "Distribución por Número de Empleados",
     xlab = "", ylab = "Frecuencia",
     legend = T,
     col = c("royalblue", "seagreen", "purple", "grey", "yellow"))


# Consolidado de Datos para Activo, Total pasivo + patrimonio, y Utilidad neta
# ******************************************************************************

plot(x = ConsolidadoDatos$AC, las = 2, main = "Activo",
     xlab = "", ylab = "", 
     col = c("royalblue", "seagreen", "purple", "green", "blue"))

plot(x = ConsolidadoDatos$P, las = 2, main = "Total pasivo + patrimonio",
     xlab = "", ylab = "", 
     col = c("royalblue", "seagreen", "purple", "green", "blue"))

plot(x = ConsolidadoDatos$R, las = 2, main = "Utilidad neta",
     xlab = "", ylab = "", 
     col = c("royalblue", "seagreen", "purple", "green", "blue"))


# Histogramas de frecuencias para la variable ACL11M PROPIEDAD DE INVERSIÓN
# ******************************************************************************

sapply(DatosEmpresa01,   function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 
sapply(ConsolidadoDatos, function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 

ACL11M    <- data.frame(DatosEmpresa01)
str(ACL11M)
ACL11MsNA <- data.frame(ConsolidadoDatos$ID, ConsolidadoDatos$ANIO, ConsolidadoDatos$ACL11M)

names (ACL11MsNA) = c("ID", "ANIO", "ACL11M")
names (ACL11MsNA)

ACL11MsNA <- merge (ACL11MsNA, ACL11M, by = "ID")
ACL11MsNA$CIIU <- as.character.numeric_version(ACL11MsNA$CIIU)

CIIU <- (ACL11MsNA$CIIU)
ACL11MsNA$CatCIIU <- as.character(substr(CIIU, 1, 1))
ACL11MsNA$TipCIIU <- as.numeric(substr(CIIU, 2, 5))

ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="A", "A - Agricultura, ganadería, caza, silvicultura y pesca")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="B", "B - Explotación de minas y canteras")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="C", "C - Industrias manufactureras")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="D", "D - Suministro de electricidad, gas")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="E", "E - Distribución de agua")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="F", "F - Construcción")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="G", "G - Comercio al por mayor y al por menor")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="H", "H - Transporte y almacenamiento")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="I", "I - Alojamiento y servicios de comida")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="J", "J - Información y comunicaciones")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="K", "K - Actividades financieras y de seguros")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="L", "L - Actividades inmobiliarias")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="M", "M - Actividades profesionales, científicas y técnicas")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="N", "N - Actividades de servicios administrativos y de apoyo")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="O", "O - Administración pública y defensa")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="P", "P - Educación")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="Q", "Q - Actividades de atención de la salud humana")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="R", "R - Actividades artísticas, de entretenimiento y recreación")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="S", "S - Otras actividades de servicios")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="T", "T - Actividades de los hogares individuales en calidad de empleadores")
ACL11MsNA$CatCIIU <- replace(ACL11MsNA$CatCIIU, ACL11MsNA$CatCIIU=="U", "U - Actividades de organizaciones y entidades extraterritoriales")

ggplot(ACL11MsNA, aes(x=Tamaño, fill=CatCIIU))+
  geom_bar(position = "dodge")

ggplot(ACL11MsNA, aes(x=ANIO, fill=CatCIIU))+
  geom_bar(position = "dodge")

# RAGE52 57% - Gastos de ventas
# Se realiza un analisis para determinar el tipo de empresa y poder concluir si
# efectivamente no requiere tener información relacionada con Gastos de ventas.


# Histogramas de frecuencias para la variable RAGE52 GASTOS DE VENTAS
# ******************************************************************************

RAGE52    <- data.frame(DatosEmpresa01)
RAGE52sNA <- data.frame(ConsolidadoDatos$ID, ConsolidadoDatos$ANIO, ConsolidadoDatos$RAGE52)

names (RAGE52sNA) = c("ID", "ANIO", "RAGE52")
names (RAGE52sNA)

RAGE52sNA      <- merge (RAGE52sNA, RAGE52, by = "ID")
RAGE52sNA$CIIU <- as.character.numeric_version(RAGE52sNA$CIIU)

CIIU <- (RAGE52sNA$CIIU)
RAGE52sNA$CatCIIU <- as.character(substr(CIIU, 1, 1))
RAGE52sNA$TipCIIU <- as.numeric(substr(CIIU, 2, 5))

RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="A", "A - Agricultura, ganadería, caza, silvicultura y pesca")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="B", "B - Explotación de minas y canteras")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="C", "C - Industrias manufactureras")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="D", "D - Suministro de electricidad, gas")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="E", "E - Distribución de agua")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="F", "F - Construcción")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="G", "G - Comercio al por mayor y al por menor")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="H", "H - Transporte y almacenamiento")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="I", "I - Alojamiento y servicios de comida")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="J", "J - Información y comunicaciones")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="K", "K - Actividades financieras y de seguros")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="L", "L - Actividades inmobiliarias")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="M", "M - Actividades profesionales, científicas y técnicas")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="N", "N - Actividades de servicios administrativos y de apoyo")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="O", "O - Administración pública y defensa")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="P", "P - Educación")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="Q", "Q - Actividades de atención de la salud humana")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="R", "R - Actividades artísticas, de entretenimiento y recreación")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="S", "S - Otras actividades de servicios")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="T", "T - Actividades de los hogares individuales en calidad de empleadores")
RAGE52sNA$CatCIIU <- replace(RAGE52sNA$CatCIIU, RAGE52sNA$CatCIIU=="U", "U - Actividades de organizaciones y entidades extraterritoriales")

ggplot(RAGE52sNA, aes(x=Tamaño, fill=CatCIIU))+
  geom_bar(position = "dodge")

ggplot(RAGE52sNA, aes(x=ANIO, fill=CatCIIU))+
  geom_bar(position = "dodge")


# **************************************************************************************************************
#
# 3.    Diseño e implementación del TFM
# 3.1.  Análisis de la estructura de los datos y la información de la base de datos
# 3.1.3	Verificación de la calidad de los datos
#
# ******************************************************************************
# CRISP_DM - 2. Fase de comprensión de los datos
# ******************************************************************************

# Identificar outliers y Valores Perdidos
# ******************************************************************************

# Para identificarlos se divide la base de datos en los cuatro tamaños de las empresas GR, MC, MD PQ

EmpTamano <- data.frame(DatosEmpresa01)

EmpTamano$Tamaño    <- as.character(EmpTamano$Tamaño)

EmpGR <- EmpTamano[EmpTamano$Tamaño == "GR", ]
EmpMC <- EmpTamano[EmpTamano$Tamaño == "MC", ]
EmpMD <- EmpTamano[EmpTamano$Tamaño == "MD", ]
EmpPQ <- EmpTamano[EmpTamano$Tamaño == "PQ", ]

EmpGRConsolidada <- merge (EmpGR, ConsolidadoDatos, by = "ID")
EmpMCConsolidada <- merge (EmpMC, ConsolidadoDatos, by = "ID")
EmpMDConsolidada <- merge (EmpMD, ConsolidadoDatos, by = "ID")
EmpPQConsolidada <- merge (EmpPQ, ConsolidadoDatos, by = "ID")


# ******************************************************************************
# Análisis para Micro empresas - MC
# ******************************************************************************

EmpMCConsolidada$CIIUCat <- as.character(substr(EmpMCConsolidada$CIIU, 1, 1))
EmpMCConsolidada$CIIUCod <- as.numeric(substr(EmpMCConsolidada$CIIU, 2, 5))

# De las categorias del CIIU se elimina la categoría Z dado que no corresponde

EmpMCConsolidada <- filter(EmpMCConsolidada, EmpMCConsolidada$CIIUCat != "Z")

# Se generan tres poblaciones de datos de MC según los rangos de categorias

MC.RSM <- filter(EmpMCConsolidada, EmpMCConsolidada$CIIUCat %in% c("A", "B", "C"))
MC.RSC <- filter(EmpMCConsolidada, EmpMCConsolidada$CIIUCat %in% c("G"))
MC.RSS <- filter(EmpMCConsolidada, EmpMCConsolidada$CIIUCat %in% 
                   c("D", "E", "F", "H", "I", "J", "K", "L", "M",
                     "N", "O", "P", "Q", "R", "S", "T", "U"))

# Se evaluan los topes por rangos para determinar posibles outliers

MC.RSM.Sin.Outliers <- MC.RSM[MC.RSM$RAIE <= 781253000, ]
MC.RSS.Sin.Outliers <- MC.RSS[MC.RSS$RAIE <= 1093750000, ]
MC.RSC.Sin.Outliers <- MC.RSC[MC.RSC$RAIE <= 1484360964, ]

dev.off()       # Desactivamos todas las ventanas gráficas o dispositivos
x11()           # Abrimos el primer dispositivo
matrix (c ( 1:3 ), nrow = 3, byrow = TRUE)
layout (matrix (c ( 1:3 ), nrow = 3, byrow = TRUE))
layout.show(3) # Muestra las cuatro particiones

boxplot(MC.RSM.Sin.Outliers$RAIE, horizontal = TRUE, main = "MC - Sector manufacturero - RSM")
stripchart(MC.RSM.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "blue")

boxplot(MC.RSS.Sin.Outliers$RAIE, horizontal = TRUE, main = "MC - Sector de servicios - RSS")
stripchart(MC.RSS.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(MC.RSC.Sin.Outliers$RAIE, horizontal = TRUE, main = "MC - Sector comercio - RSC")
stripchart(MC.RSC.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "green")


# ******************************************************************************
# Análisis para Empresas Pequeñas - PQ
# ******************************************************************************

EmpPQConsolidada$CIIUCat <- as.character(substr(EmpPQConsolidada$CIIU, 1, 1))
EmpPQConsolidada$CIIUCod <- as.numeric(substr(EmpPQConsolidada$CIIU, 2, 5))

# De las categorias del CIIU se elimina la categoría Z dado que no corresponde

EmpPQConsolidada <- filter(EmpPQConsolidada, EmpPQConsolidada$CIIUCat != "Z")

# Se generan tres poblaciones de datos de PQ según los rangos de categorias

PQ.RSM <- filter(EmpPQConsolidada, EmpPQConsolidada$CIIUCat %in% c("A", "B", "C"))
PQ.RSC <- filter(EmpPQConsolidada, EmpPQConsolidada$CIIUCat %in% c("G"))
PQ.RSS <- filter(EmpPQConsolidada, EmpPQConsolidada$CIIUCat %in% 
                   c("D", "E", "F", "H", "I", "J", "K", "L", "M",
                     "N", "O", "P", "Q", "R", "S", "T", "U"))

# Se evaluan los topes por rangos para determinar posibles outliers

PQ.RSM.Sin.Outliers <- PQ.RSM[PQ.RSM$RAIE <  781253000 && PQ.RSM$RAIE <=  6796814000, ]
PQ.RSS.Sin.Outliers <- PQ.RSS[PQ.RSS$RAIE < 1093750000 && PQ.RSM$RAIE <=  4374967000, ]
PQ.RSC.Sin.Outliers <- PQ.RSC[PQ.RSC$RAIE < 1484360964 && PQ.RSM$RAIE <= 14296734576, ]

dev.off()       # Desactivamos todas las ventanas gráficas o dispositivos
x11()           # Abrimos el primer dispositivo
matrix (c ( 1:3 ), nrow = 3, byrow = TRUE)
layout (matrix (c ( 1:3 ), nrow = 3, byrow = TRUE))
layout.show(3) # Muestra las cuatro particiones

boxplot(PQ.RSM.Sin.Outliers$RAIE, horizontal = TRUE, main = "PQ - Sector manufacturero - RSM")
stripchart(PQ.RSM.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "blue")

boxplot(PQ.RSS.Sin.Outliers$RAIE, horizontal = TRUE, main = "PQ - Sector de servicios - RSS")
stripchart(PQ.RSS.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(PQ.RSC.Sin.Outliers$RAIE, horizontal = TRUE, main = "PQ - Sector comercio - RSC")
stripchart(PQ.RSC.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "green")


# ******************************************************************************
# Análisis para Empresas Medianas - MD
# ******************************************************************************

EmpMDConsolidada$CIIUCat <- as.character(substr(EmpMDConsolidada$CIIU, 1, 1))
EmpMDConsolidada$CIIUCod <- as.numeric(substr(EmpMDConsolidada$CIIU, 2, 5))

# De las categorias del CIIU se elimina la categoría Z dado que no corresponde

EmpMDConsolidada <- filter(EmpMDConsolidada, EmpMDConsolidada$CIIUCat != "Z")

# Se generan tres poblaciones de datos de MD según los rangos de categorias

MD.RSM <- filter(EmpMDConsolidada, EmpMDConsolidada$CIIUCat %in% c("A", "B", "C"))
MD.RSC <- filter(EmpMDConsolidada, EmpMDConsolidada$CIIUCat %in% c("G"))
MD.RSS <- filter(EmpMDConsolidada, EmpMDConsolidada$CIIUCat %in% 
                   c("D", "E", "F", "H", "I", "J", "K", "L", "M",
                     "N", "O", "P", "Q", "R", "S", "T", "U"))

# Se evaluan los topes por rangos para determinar posibles outliers

MD.RSM.Sin.Outliers <- MD.RSM[MD.RSM$RAIE <  6796814000 && MD.RSM$RAIE <= 57577549000, ]
MD.RSS.Sin.Outliers <- MD.RSS[MD.RSS$RAIE <  4374967000 && MD.RSM$RAIE <= 16015475000, ]
MD.RSC.Sin.Outliers <- MD.RSC[MD.RSC$RAIE < 14296734576 && MD.RSM$RAIE <= 71620010352, ]

dev.off()       # Desactivamos todas las ventanas gráficas o dispositivos
x11()           # Abrimos el primer dispositivo
matrix (c ( 1:3 ), nrow = 3, byrow = TRUE)
layout (matrix (c ( 1:3 ), nrow = 3, byrow = TRUE))
layout.show(3) # Muestra las cuatro particiones

boxplot(MD.RSM.Sin.Outliers$RAIE, horizontal = TRUE, main = "MD - Sector manufacturero - RSM")
stripchart(MD.RSM.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "blue")

boxplot(MD.RSS.Sin.Outliers$RAIE, horizontal = TRUE, main = "MD - Sector de servicios - RSS")
stripchart(MD.RSS.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(MD.RSC.Sin.Outliers$RAIE, horizontal = TRUE, main = "MD - Sector comercio - RSC")
stripchart(MD.RSC.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "green")


# ******************************************************************************
# Análisis para Empresas Grandes - GR
# ******************************************************************************

EmpGRConsolidada$CIIUCat <- as.character(substr(EmpGRConsolidada$CIIU, 1, 1))
EmpGRConsolidada$CIIUCod <- as.numeric(substr(EmpGRConsolidada$CIIU, 2, 5))

# De las categorias del CIIU se elimina la categoría Z dado que no corresponde

EmpGRConsolidada <- filter(EmpGRConsolidada, EmpGRConsolidada$CIIUCat != "Z")

# Se generan tres poblaciones de datos de GR según los rangos de categorias

GR.RSM <- filter(EmpGRConsolidada, EmpGRConsolidada$CIIUCat %in% c("A", "B", "C"))
GR.RSC <- filter(EmpGRConsolidada, EmpGRConsolidada$CIIUCat %in% c("G"))
GR.RSS <- filter(EmpGRConsolidada, EmpGRConsolidada$CIIUCat %in% 
                   c("D", "E", "F", "H", "I", "J", "K", "L", "M",
                     "N", "O", "P", "Q", "R", "S", "T", "U"))

# Se evaluan los topes por rangos para determinar posibles outliers

GR.RSM.Sin.Outliers <- GR.RSM[GR.RSM$RAIE < 57577549000, ]
GR.RSS.Sin.Outliers <- GR.RSS[GR.RSS$RAIE < 16015475000, ]
GR.RSC.Sin.Outliers <- GR.RSC[GR.RSC$RAIE < 71620010352, ]

dev.off()       # Desactivamos todas las ventanas gráficas o dispositivos
x11()           # Abrimos el primer dispositivo
matrix (c ( 1:3 ), nrow = 3, byrow = TRUE)
layout (matrix (c ( 1:3 ), nrow = 3, byrow = TRUE))
layout.show(3) # Muestra las cuatro particiones

boxplot(GR.RSM.Sin.Outliers$RAIE, horizontal = TRUE, main = "GR - Sector manufacturero - RSM")
stripchart(GR.RSM.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "blue")

boxplot(GR.RSS.Sin.Outliers$RAIE, horizontal = TRUE, main = "GR - Sector de servicios - RSS")
stripchart(GR.RSS.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "red")

boxplot(GR.RSC.Sin.Outliers$RAIE, horizontal = TRUE, main = "GR - Sector comercio - RSC")
stripchart(GR.RSC.Sin.Outliers$RAIE, method  = "jitter", pch = 19, add = TRUE, col = "green")



# **************************************************************************************************************
#
# 4.    Modelamiento
# 4.3.	Cálculo del índice de cobertura de la tasa de interés
#       ICRit = RA (resultado antes de impuestos) / RAGXFI (gastos financieros)
#
# ******************************************************************************
# CRISP_DM - 3. Fase de preparación de los datos
# 4.3.	Cálculo del índice de cobertura de la tasa de interés
#
# ******************************************************************************
# Identificación de Empresas Zombi de la población total
# ******************************************************************************
#
# cargar_datos
# ******************************************************************************

zombi <- ConDatosV

# with(zombi, table(RA, RAGXFI))
# ******************************************************************************

'%ni%' <- Negate('%in%')
excluir <- zombi [zombi$zombi != 1 | is.na(zombi$zombi) ,]$ID
sub_conjunto <- zombi [zombi$ID %ni% excluir,]

# Tautología para RA y RAGXFI
# ******************************************************************************

zombi$nuevoZombi <- ""
zombi [ zombi$RA <  0  & zombi$RAGXFI == 0  , ]$nuevoZombi <- 0
zombi [ zombi$RA <  0  & zombi$RAGXFI >  0  , ]$nuevoZombi <- 1
zombi [ zombi$RA == 0  & zombi$RAGXFI == 0  , ]$nuevoZombi <- 0
# zombi [ zombi$RA == 0  & zombi$RAGXFI <  0  , ]$nuevoZombi <- 0
# zombi [ zombi$RA == 0  & zombi$RAGXFI >  0  , ]$nuevoZombi <- 1
zombi [ zombi$RA >  0  & zombi$RAGXFI == 0  , ]$nuevoZombi <- 0
# zombi [ zombi$RA >  0  & zombi$RAGXFI <  0  , ]$nuevoZombi <- 0
zombi [ zombi$RA >  0  & zombi$RAGXFI >  0  & zombi$RA > zombi$RAGXFI  , ]$nuevoZombi <- 0
zombi [ zombi$RA >  0  & zombi$RAGXFI >  0  & zombi$RA < zombi$RAGXFI  , ]$nuevoZombi <- 1

str(zombi)
zombi$nuevoZombi <- as.numeric(zombi$nuevoZombi)
names(zombi)[50] <- "zombi"

ConDatosV        <- zombi
str(ConDatosV)
write_xlsx(zombi,"D:/CABV/MasterUOC/TFM/TFM UOC/PEC3/Datos Entrega\\ConDatosV.xlsx")


# 4.4.	Análisis de los datos relacionados con las empresas Zombi
# ****************************************************************************************************
# 4.5.1 Estimación del modelo ANOVA
# ****************************************************************************************************

Zombi.Anova <- merge (DatosEmpresa01, ConDatosV, by = "ID")

str(Zombi.Anova)

borrar <- c("ID", "ANIO", "CIIU", "CIIUCod", "RA", "RAGXFI", "Departamento")
anova <- Zombi.Anova[ , !(names(Zombi.Anova) %in% borrar)]

anova <- aov(zombi ~ ., data = anova)
summary(anova)

# ******************************************************************************
#                                                                                          BASE ANOVA
# c("Tamaño", "CIIUCat", "ACC11H", "ACC114",
#   "ACL11Q", "ACL11R", "ACL118", "PS", "PSC", "PSC224", "PSC228",
#   "PSL228", "PTT132", "PTT136","RAE",  "RAGE", "RAXFI", "zombi")]
# ******************************************************************************
#
# 4.5.4 Escalada de datos
#
# Normalización de Datos por la diferencia para la BD ConDatosV1
#
# ******************************************************************************

# cargar_datos

zombi <- ConDatosV

str(zombi)

# zombi <- EmpresasSNZombi

IDzombi <- zombi$ID
Zzombi  <- zombi$zombi

# escalar_variables

rm(zombi02)
class(zombi[,1])
class(zombi['ID'])
escalarRango <- function(x) ((x-mean(x))/(max(x)-min(x)))
vzombi01 <- setNames(as.data.frame(escalarRango(zombi[,1])),colnames(zombi)[1])
vzombi02 <- setNames(as.data.frame(escalarRango(zombi[,2])),colnames(zombi)[2])
vzombi03 <- setNames(as.data.frame(escalarRango(zombi[,3])),colnames(zombi)[3])
vzombi04 <- setNames(as.data.frame(escalarRango(zombi[,4])),colnames(zombi)[4])
vzombi05 <- setNames(as.data.frame(escalarRango(zombi[,5])),colnames(zombi)[5])
vzombi06 <- setNames(as.data.frame(escalarRango(zombi[,6])),colnames(zombi)[6])
vzombi07 <- setNames(as.data.frame(escalarRango(zombi[,7])),colnames(zombi)[7])
vzombi08 <- setNames(as.data.frame(escalarRango(zombi[,8])),colnames(zombi)[8])
vzombi09 <- setNames(as.data.frame(escalarRango(zombi[,9])),colnames(zombi)[9])
vzombi10 <- setNames(as.data.frame(escalarRango(zombi[,10])),colnames(zombi)[10])
vzombi11 <- setNames(as.data.frame(escalarRango(zombi[,11])),colnames(zombi)[11])
vzombi12 <- setNames(as.data.frame(escalarRango(zombi[,12])),colnames(zombi)[12])
vzombi13 <- setNames(as.data.frame(escalarRango(zombi[,13])),colnames(zombi)[13])
vzombi14 <- setNames(as.data.frame(escalarRango(zombi[,14])),colnames(zombi)[14])
vzombi15 <- setNames(as.data.frame(escalarRango(zombi[,15])),colnames(zombi)[15])
vzombi16 <- setNames(as.data.frame(escalarRango(zombi[,16])),colnames(zombi)[16])
vzombi17 <- setNames(as.data.frame(escalarRango(zombi[,17])),colnames(zombi)[17])
vzombi18 <- setNames(as.data.frame(escalarRango(zombi[,18])),colnames(zombi)[18])
vzombi19 <- setNames(as.data.frame(escalarRango(zombi[,19])),colnames(zombi)[19])
vzombi20 <- setNames(as.data.frame(escalarRango(zombi[,20])),colnames(zombi)[20])
vzombi21 <- setNames(as.data.frame(escalarRango(zombi[,21])),colnames(zombi)[21])
vzombi22 <- setNames(as.data.frame(escalarRango(zombi[,22])),colnames(zombi)[22])
vzombi23 <- setNames(as.data.frame(escalarRango(zombi[,23])),colnames(zombi)[23])
vzombi24 <- setNames(as.data.frame(escalarRango(zombi[,24])),colnames(zombi)[24])
vzombi25 <- setNames(as.data.frame(escalarRango(zombi[,25])),colnames(zombi)[25])
vzombi26 <- setNames(as.data.frame(escalarRango(zombi[,26])),colnames(zombi)[26])
vzombi27 <- setNames(as.data.frame(escalarRango(zombi[,27])),colnames(zombi)[27])
vzombi28 <- setNames(as.data.frame(escalarRango(zombi[,28])),colnames(zombi)[28])
vzombi29 <- setNames(as.data.frame(escalarRango(zombi[,29])),colnames(zombi)[29])
vzombi30 <- setNames(as.data.frame(escalarRango(zombi[,30])),colnames(zombi)[30])
vzombi31 <- setNames(as.data.frame(escalarRango(zombi[,31])),colnames(zombi)[31])
vzombi32 <- setNames(as.data.frame(escalarRango(zombi[,32])),colnames(zombi)[32])
vzombi33 <- setNames(as.data.frame(escalarRango(zombi[,33])),colnames(zombi)[33])
vzombi34 <- setNames(as.data.frame(escalarRango(zombi[,34])),colnames(zombi)[34])
vzombi35 <- setNames(as.data.frame(escalarRango(zombi[,35])),colnames(zombi)[35])
vzombi36 <- setNames(as.data.frame(escalarRango(zombi[,36])),colnames(zombi)[36])
vzombi37 <- setNames(as.data.frame(escalarRango(zombi[,37])),colnames(zombi)[37])
vzombi38 <- setNames(as.data.frame(escalarRango(zombi[,38])),colnames(zombi)[38])
vzombi39 <- setNames(as.data.frame(escalarRango(zombi[,39])),colnames(zombi)[39])
vzombi40 <- setNames(as.data.frame(escalarRango(zombi[,40])),colnames(zombi)[40])
vzombi41 <- setNames(as.data.frame(escalarRango(zombi[,41])),colnames(zombi)[41])
vzombi42 <- setNames(as.data.frame(escalarRango(zombi[,42])),colnames(zombi)[42])
vzombi43 <- setNames(as.data.frame(escalarRango(zombi[,43])),colnames(zombi)[43])
vzombi44 <- setNames(as.data.frame(escalarRango(zombi[,44])),colnames(zombi)[44])
vzombi45 <- setNames(as.data.frame(escalarRango(zombi[,45])),colnames(zombi)[45])
vzombi46 <- setNames(as.data.frame(escalarRango(zombi[,46])),colnames(zombi)[46])
vzombi47 <- setNames(as.data.frame(escalarRango(zombi[,47])),colnames(zombi)[47])
vzombi48 <- setNames(as.data.frame(escalarRango(zombi[,48])),colnames(zombi)[48])
vzombi49 <- setNames(as.data.frame(escalarRango(zombi[,49])),colnames(zombi)[49])
vzombi50 <- setNames(as.data.frame(escalarRango(zombi[,50])),colnames(zombi)[50])

zombi02 <- cbind(as.data.frame(lapply(ls(pattern="vzombi\\d+"),get)))
rm(list=ls(pattern="vzombi\\d+"))
zombi02$zombi <- zombi$zombi
zombi02$IDZ   <- IDzombi
borrar <- c("ID")
zombi02 <- zombi02[ , !(names(zombi02) %in% borrar)]
names(zombi02)[50] <- "ID"

str(zombi02)

# escalar_variables_comprobar

t(as.data.frame(apply(zombi,2,function(x) cbind(mean(x),sd(x),max(x),min(x)))))
t(as.data.frame(apply(zombi02,2,function(x) cbind(mean(x),sd(x),max(x),min(x)))))

# contar_valores_zombi

aggregate(ID~zombi, zombi02, length)

# grabar_datos_escalados

saveRDS(zombi02, file="DatosEscalados.rds")
savehistory()

# ******************************************************************************
# zombi02
# ******************************************************************************

# ******************************************************************************
# Media de la Base ConsolidadoDatos
# ******************************************************************************

zombi03 <- summaryBy(zombi02 ~ ID, data=zombi02, FUN=mean)


str(zombi03)

names(zombi03)[50] <- "zombi"
zombi03$zombi <- replace(zombi03$zombi, zombi03$zombi == "0.333333333333333", "0")
zombi03$zombi <- replace(zombi03$zombi, zombi03$zombi == "0.666666666666667", "0")
zombi03$zombi <- as.numeric(zombi03$zombi)

table(zombi03$zombi)

# ******************************************************************************
#
# Normalización de Datos para la BD Datos Empresa - DatosEmpresa01
#
# ******************************************************************************

# ******************************************************************************
# # Almacenar el DF DatEmpNum
# ******************************************************************************


DatEmpNum <- DatosEmpresa01
str(DatEmpNum)
# cargar_datos

IDzombiE  <- DatEmpNum$ID

# escalar_variables

str(DatEmpNum)

rm(DatEmpNum02)
class(DatEmpNum[,1])
class(DatEmpNum['ID'])
escalarRango <- function(x) ((x-mean(x))/(max(x)-min(x)))
vDatEmpNum01 <- setNames(as.data.frame(escalarRango(DatEmpNum[,1])),colnames(DatEmpNum)[1])
vDatEmpNum02 <- setNames(as.data.frame(escalarRango(DatEmpNum[,2])),colnames(DatEmpNum)[2])
vDatEmpNum03 <- setNames(as.data.frame(escalarRango(DatEmpNum[,3])),colnames(DatEmpNum)[3])
vDatEmpNum04 <- setNames(as.data.frame(escalarRango(DatEmpNum[,4])),colnames(DatEmpNum)[4])
vDatEmpNum05 <- setNames(as.data.frame(escalarRango(DatEmpNum[,5])),colnames(DatEmpNum)[5])
vDatEmpNum06 <- setNames(as.data.frame(escalarRango(DatEmpNum[,6])),colnames(DatEmpNum)[6])
vDatEmpNum07 <- setNames(as.data.frame(escalarRango(DatEmpNum[,7])),colnames(DatEmpNum)[7])
vDatEmpNum08 <- setNames(as.data.frame(escalarRango(DatEmpNum[,8])),colnames(DatEmpNum)[8])
vDatEmpNum09 <- setNames(as.data.frame(escalarRango(DatEmpNum[,9])),colnames(DatEmpNum)[9])
vDatEmpNum10 <- setNames(as.data.frame(escalarRango(DatEmpNum[,10])),colnames(DatEmpNum)[10])
vDatEmpNum11 <- setNames(as.data.frame(escalarRango(DatEmpNum[,11])),colnames(DatEmpNum)[11])


DatEmpNum02 <- cbind(as.data.frame(lapply(ls(pattern="vDatEmpNum\\d+"),get)))
rm(list=ls(pattern="vDatEmpNum\\d+"))
DatEmpNum02$DatEmpNum <- DatEmpNum$DatEmpNum
DatEmpNum02$IDZ   <- IDzombiE

borrar <- c("ID")
DatEmpNum02 <- DatEmpNum02[ , !(names(DatEmpNum02) %in% borrar)]
names(DatEmpNum02)[11] <- "ID"

# escalar_variables_comprobar

t(as.data.frame(apply(DatEmpNum,2,function(x) cbind(mean(x),sd(x),max(x),min(x)))))
t(as.data.frame(apply(DatEmpNum02,2,function(x) cbind(mean(x),sd(x),max(x),min(x)))))

# contar_valores_DatEmpNum

aggregate(ID~Tipologia, DatEmpNum02, length)

# grabar_datos_escalados

saveRDS(DatEmpNum02, file="DatosEscalados.rds")
savehistory()

str(zombi03)


# ******************************************************************************
# Integración de las BD zombi03 (medias) y DatEmpNum02
# ******************************************************************************

borrar <- c("ID.mean", "ANIO.mean", "RA.mean", "RAGXFI.mean")
zombi03 <- zombi03[ , !(names(zombi03) %in% borrar)]

zombi04 <- merge (DatEmpNum02, zombi03, by = "ID")

str(zombi04)

borrar <- c("CIIU", "Localidad")
zombi04 <- zombi04[ , !(names(zombi04) %in% borrar)]

ZombiDef <- zombi04


# ******************************************************************************
# Cálculo de Ratios financieros básicos
# Indicadores de Rentabilidad
# ******************************************************************************

# Solvencia
ZombiDef$Solvencia <- (ZombiDef$AC.mean / ZombiDef$PS.mean)

# Endeudamiento
ZombiDef$Endeudamiento <- (ZombiDef$PS.mean / ZombiDef$PT.mean)

# Fondo Maniobra
ZombiDef$FondoMan <- (ZombiDef$ACC.mean - ZombiDef$PSC.mean)

# Margen bruto = (RAI (total ingresos) - RAGE60 (costo de ventas)) / RAI (total ingresos)
ZombiDef$MarBrut <- ((ZombiDef$RAI.mean - ZombiDef$RAGE60.mean) / ZombiDef$RAI.mean)

# Margen neto = R (utilidad neta) / RAIE (ingresos operacionales))
ZombiDef$MarNeto <- (ZombiDef$R.mean / ZombiDef$RAIE.mean)

# Margen operacional = RAE (resultados operacionales) / RAIE (ingresos operacionales)
ZombiDef$MarOper <- (ZombiDef$RAE.mean / ZombiDef$RAIE.mean)

# Rendimiento sobre el patrimonio (ROE) = R/PT
ZombiDef$RentROE <- (ZombiDef$R.mean / ZombiDef$PT.mean)

# Rendimiento sobre los activos (ROA) = R / AC
ZombiDef$RentROA <- (ZombiDef$R.mean / ZombiDef$AC.mean)

# (ingresos operacionales)
ZombiDef$CtaXCob <- (ZombiDef$PS.mean / ZombiDef$AC.mean)

# ZombiDef$zombi <- as.factor(ZombiDef$zombi)
# ZombiDef$zombi <- ifelse(ZombiDef$zombi==0,"No","Yes")


# *******************************************************************************************************************
# 4.5.2 Analizar la multicolinealidad
# *******************************************************************************************************************

str(ZombiDef)

VIF <- ZombiDef[, c(-1) ]

str(VIF)

borrar <- c("CIIUCod", "ACL.mean", "P.mean", "PSL.mean", "PT.mean", "RAGX.mean",
            "RAIX.mean", "FondoMan")
VIF <- VIF[ , !(names(VIF) %in% borrar)]

str(VIF) # 55

VIF.lm <- lm(zombi ~ .,  na.action=na.exclude, data=VIF)
summary(VIF.lm)

coef(VIF.lm)

tmp <- confint(VIF.lm)
tmp
tmp[,2]-tmp[,1]

vif(VIF.lm)
apply(VIF,2,mean)

# ******************************************************************************
# Reducción 01 de algunas varias no significativas
# ******************************************************************************

borrar <- c("AC.mean", "ACC.mean", "ACC113.mean", "ACC114.mean","ACC118.mean",
            "ACC211.mean",  "PS.mean", "PSC.mean",
            "PSC225.mean", "PTT131.mean", "PTT136.mean", "R.mean", "RAE.mean",  "RAG.mean",
            "RAGE.mean", "RAGE60.mean", "RAI.mean",
            "RAIE.mean", "RAIXFI.mean", "RAX.mean", "RIII.mean", "RAXFI.mean", "MarNeto",
            "MarOper")


VIF <- VIF[ , !(names(VIF) %in% borrar)]


VIF.lm <- lm(zombi ~. ,  na.action=na.exclude, data=VIF)
summary(VIF.lm)

coef(VIF.lm)

tmp <- confint(VIF.lm)
tmp
tmp[,2]-tmp[,1]

vif(VIF.lm)
apply(VIF,2,mean)


# ******************************************************************************
# Reducción 02 de algunas varias no significativas
# ******************************************************************************

borrar <- c("ACL11R.mean", "ACL115.mean")
VIF <- VIF[ , !(names(VIF) %in% borrar)]


VIF.lm <- lm(zombi ~. ,  na.action=na.exclude, data=VIF)
summary(VIF.lm)

coef(VIF.lm)

tmp <- confint(VIF.lm)
tmp
tmp[,2]-tmp[,1]

vif(VIF.lm)
apply(VIF,2,mean)


# ******************************************************************************
# Reducción 03 de algunas varias no significativas
# ******************************************************************************

borrar <- c("ACC11H.mean", "PSC223.mean", "Endeudamiento", "RentROE")
VIF <- VIF[ , !(names(VIF) %in% borrar)]


VIF.lm <- lm(zombi ~. ,  na.action=na.exclude, data=VIF)
summary(VIF.lm)

coef(VIF.lm)

tmp <- confint(VIF.lm)
tmp
tmp[,2]-tmp[,1]

vif(VIF.lm)
apply(VIF,2,mean)

str(VIF)


# ************************************************************************************************************
# ************************************************************************************************************
# PCA - Análisis De Componentes Principales ZombiDef - Zombi.Ent - Zombi.Tes
# 4.5.5 Identificación de variables mediante el análisis de componentes principales - PCA
# ************************************************************************************************************
# ************************************************************************************************************


ZombiDefP <- ZombiDef[, c("Tamaño", "CIIUCat", "ACC11H.mean", "ACC114.mean", "ACL11Q.mean", "ACL11R.mean",
                          "ACL118.mean", "PS.mean", "PSC.mean", "PSC224.mean", "PSC228.mean", "PSL228.mean", "PTT132.mean",
                          "PTT136.mean","RAE.mean", "RAGE.mean", "RAIXFI.mean", "zombi")]


str(ZombiDef)


# ZombiDefP <- ZombiDef[, c("Tamaño", "CIIUCat", "ACL11Q.mean", "ACL118.mean", "PSC224.mean",
#                           "PSC228.mean", "PSL12J.mean", "PSL228.mean", "PTT132.mean",
#                           "Solvencia", "MarBrut", "RentROA", "RentROA", "zombi")]

str(ZombiDefP)
sapply(ZombiDefP,     function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 

res.pca <- PCA(ZombiDefP, graph = FALSE)
print(res.pca)

# Visualización e interpretación
# Autovalores / Varianzas

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Círculo de correlación
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Contribución de variables a PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
# Contribución de variables a  PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 40)


res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Descripcion de dimension 1
res.desc$Dim.1


# ******************************************************************************
# 4.5.6 Identificación de variables predictoras
# 1. Conjunto de datos de entrenamiento
# ******************************************************************************

NewZombi <- ZombiDef[, c("PS.mean", "PSC.mean", "RAGE.mean", "RAE.mean", "PTT136.mean", "zombi")]

str(Zombi.Ent)

ZombiEnt <- createDataPartition(NewZombi$zombi, p=0.7, list = FALSE)
Zombi.Ent <- NewZombi[ZombiEnt,]
nrow(Zombi.Ent)

sapply(Zombi.Ent, function(x)sum(is.na(x)))     # ver el resumen de cuantos faltan 


# ******************************************************************************
# 2. Conjunto de datos de validación - test
# ******************************************************************************

Zombi.Tes <- NewZombi[-ZombiEnt,]
nrow(Zombi.Tes)

colnames(Zombi.Tes)

dataset <- Zombi.Ent
str(dataset)

table(dataset$zombi)

# ******************************************************************************
# Para aplicar la Validación Cruzada vamos a hacer uso de la función createFolds
# del paquete caret, y luego entrenaremos cada modelo sobre k = 10 subconjuntos 
# haciendo uso de la función lapply:
# ******************************************************************************

folds <- createFolds(Zombi.Ent$zombi, k = 10)

# ******************************************************************************
# https://rpubs.com/rdelgado/405322
# Regresion Logistica
# ******************************************************************************

cvRegresionLogistica <- lapply(folds, function(x){
  training_fold <- Zombi.Ent
  test_fold     <- Zombi.Tes
  regresionRL  <- glm(zombi ~ ., family = binomial, data = training_fold, control = glm.control (maxit = 50))
  y_pred <- predict(regresionRL, type = 'response', newdata = test_fold)
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
  y_pred <- factor(y_pred, levels = c("0", "1"), labels = c("NoZombi", "Zombi"))
  cm <- table(test_fold$zombi, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionRegresionLogistica <- mean(as.numeric(cvRegresionLogistica))


# ******************************************************************************
# Random Forest
# ******************************************************************************

Zombi.Ent03 <- Zombi.Ent
Zombi.Ent03$zombi <- as.factor(Zombi.Ent03$zombi)

Zombi.Tes03 <- Zombi.Tes
Zombi.Tes03$zombi <- as.factor(Zombi.Tes03$zombi)

str(Zombi.Tes03)
table(Zombi.Tes03$zombi)

library(randomForest)
cvRandomForest  <- lapply(folds, function(x){
  training_fold <- Zombi.Ent03
  test_fold     <- Zombi.Tes03
  regresionRF   <- randomForest(zombi ~ ., data = training_fold, ntree = 250)
  y_pred <- predict(regresionRF, newdata = test_fold)
  cm <- table(test_fold$zombi, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionRandomForest <- mean(as.numeric(cvRandomForest))

# ******************************************************************************
# Confusion Matrix and Statistics -  Random Forest
# ******************************************************************************

regresionRF <- randomForest(zombi ~ ., data = Zombi.Ent03, ntree = 250)
y_pred      <- predict(regresionRF, newdata = Zombi.Tes03)
cm          <- table(Zombi.Tes03$zombi, y_pred)
cm

medida <- confusionMatrix(y_pred, Zombi.Tes03$zombi)
medida


## ****************************************************************************************************
# 4.5.7 k-fold Cross Validation
## ****************************************************************************************************

str(Zombi.Ent)
Zombi.Cross <- Zombi.Ent

Zombi.Cross$zombi <- as.factor(Zombi.Cross$zombi)

str(Zombi.Cross)

data(Zombi.Cross)

# definir el control de entrnamiento
trainControl <- trainControl(method="cv", number=10)

# evaluar el modelo
fit <- train(zombi ~ ., data=Zombi.Cross, trControl=trainControl, method="nb")

# mostrar resultados
print(fit)



## ****************************************************************************************************
# 4.5.8 RMSE y R2
## ****************************************************************************************************

library(caret)

# cargar datos
data(Zombi.Ent)

# preparar metodo
trainControl <- trainControl(method="cv", number=6)
set.seed(7)
fit <- train(zombi ~ ., data=Zombi.Ent, method="lm", metric="RMSE", trControl=trainControl)

# mostrar resultados
print(fit)


# ****************************************************************************************************
# 4.5.9.2 Logistic Regression
# ****************************************************************************************************

library(mlbench)

# cargar datos
data(Zombi.Ent)

# fit modelo
fit <- glm(zombi ~ ., data=Zombi.Ent, family=binomial(link='logit'))

# mostrar resultados
print(fit)

# predicciones
probabilities <- predict(fit, Zombi.Ent[,1:5], type='response')
predictions <- ifelse(probabilities > 0.5,'pos','neg')

# resumen de la presición
table(predictions, Zombi.Ent$zombi)


# ******************************************************************************
# 4.5.9.1 Random Forest
# ******************************************************************************

# default RF model

m1 <- randomForest(formula = zombi ~ ., data  = Zombi.Ent)
m1


# 5. Comparar el rendimiento de los algoritmos de aprendizaje automático

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)

# CART
set.seed(7)
fit.cart <- train(zombi ~ ., data=Zombi.Ent, method="rpart",
                  trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(zombi ~., data=Zombi.Ent, method="knn", trControl=trainControl)

# Random Forest
set.seed(7)
fit.rf <- train(zombi ~., data=Zombi.Ent, method="rf", trControl=trainControl)

# collect resamples
results <- resamples(list(CART=fit.cart, KNN=fit.knn, RF=fit.rf))


summary(results)


# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)



