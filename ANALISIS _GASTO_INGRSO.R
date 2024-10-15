# Passos a seguir para el análisis del gasto e ingreso del gobierno.

options(scipen = 999)

# Para describir el comportamiento de una ST en sus coomponentes
# Tendencia, Ciclo, Estacionalidad e Irregularidad se emplean:
#Librería TTR

install.packages('TTR')
library('TTR')

# Trabajamos con series de gastos e ingresos del gobierno entre 1993 y 2023

gasto_t <- ts(seriest$GASTO,
             start = c(1993,1), #FECHA DE INICIO 
             frequency = 12) #PERIODO TOTAL EN MESES
ingreso_t<-  ts(seriest$INGRESO,
                start = c(1993,1), #FECHA DE INICIO 
                frequency = 12)

ts.plot(gasto_t)
ts.plot(ingreso_t)

# Utilizar el metoo de la media movil 
# Consiste en contruir promedios, calacular promedios cada X periodos.
# Media movil de orden X, x siendo los periodos en los que se sacará un promedio.

#GASTO

gasto_tsma12<-SMA(gasto_t, n = 12) #A un año 
gasto_tsma24<-SMA(gasto_t, n = 24) #A dos años
gasto_tsma36<-SMA(gasto_t, n = 36) #A tres años 

#INGRESO

ingreso_tsma12<-SMA(ingreso_t, n = 12) #A un año 
ingreso_tsma24<-SMA(ingreso_t, n = 24) #A dos años
ingreso_tsma36<-SMA(ingreso_t, n = 36) #A tres años 

#graficamos las dos:

#GASTO

ts.plot(gasto_t, gasto_tsma12, gasto_tsma24,gasto_tsma36,
        col = c('black', 'red', 'blue', 'green'), 
        lwd = c(1, 2, 2, 2)) #La media movil tiene la finalidad de suavizar la curva.

#INGRESO

ts.plot(ingreso_t, ingreso_tsma12, ingreso_tsma24,ingreso_tsma36,
        col = c('black', 'red', 'blue', 'green'), 
        lwd = c(1, 2, 2, 2))

#Descomponer la serie 
install.packages('seasonal')
install.packages("seasonalview")
library('seasonal')
library('seasonalview')


#GASTO
gasto_tseas<- seas(gasto_t, x11 = '') #Serie desestacionalizada, ya no tiene componentes estacionales. Es la serie suavizada.

plot(gasto_tseas)

plot(gasto_tseas$data, 
     type = 'l')
#INGRESO
ingreso_tseas<- seas(ingreso_t, x11 = '') #Serie desestacionalizada, ya no tiene componentes estacionales. Es la serie suavizada.

plot(ingreso_tseas)

plot(ingreso_tseas$data, 
     type = 'l')

#Hacemos data frame a los objetps de igae_tseas
gral_tseas <- data.frame(gasto_tseas, ingreso_tseas)

View(gral_tseas)

head(gral_tseas)

#cambiamos nombres 

names(gral_tseas)<- c('g_date', 
                     'g_des', 
                     'g_est', 
                     'g_est_adj', 
                     'g_tend', 
                     'g_irreg',
                     'g_fac_adj',
                     'i_date', 
                     'i_des', 
                     'i_est', 
                     'i_est_adj', 
                     'i_tend', 
                     'i_irreg',
                     'i_fac_adj')

head(gral_tseas) #Data frame con nuevos nombres 


# Graficar ambas series temporales en una sola gráfica
plot.ts(cbind(gral_tseas$g_igae_des, gral_tseas$i_igae_des),
        col = c('black', 'red'), 
        lwd = 2, 
        legend = TRUE)


#Exportar data a Excel 

getwd()

write.csv(gral_tseas, 
          file = 'gral_i_g_desestacionalizado.csv')

#Otra forma de descomponer 

des_gasto<- decompose(gasto_t)
plot(des_gasto$figure, 
    type = 'b',
     xlab = 'mes',
     ylab = 'indice',
     col = 'darkblue',
     las = 1, 
    lwd = 2)
grid()

des_ingreso<- decompose(ingreso_t)
plot(des_ingreso$figure, 
     type = 'b',
     xlab = 'mes',
     ylab = 'indice',
     col = 'darkblue',
     las = 1, 
     lwd = 2)
grid()

#Análisis gráfico 
install.packages('forecast')
library('ggplot2')
library('forecast')

#GASTO
ggseasonplot(gasto_t, 
             year.labels = TRUE, 
             year.labels.left = TRUE) + 
  ylab('GASTO') + 
  xlab('Month') + 
 ggtitle('DESGLOCE DE GASTO');

#INGRESO
ggseasonplot(ingreso_t, 
             year.labels = TRUE, 
             year.labels.left = TRUE) + 
  ylab('INGRESO') + 
  xlab('Month') + 
  ggtitle('DESGLOCE DE INGRESO');

#GRAFICO POLAR

#GASTO
ggseasonplot(gasto_t, 
             polar = TRUE) + 
  ylab('GASTO') + 
  ggtitle('Desgloce de GASTO')

#INGRESO
ggseasonplot(ingreso_t, 
             polar = TRUE) + 
  ylab('INGRESO') + 
  ggtitle('Desgloce de INGRESO')
 
#Filtro Holdrick - prescott (HP)

install.packages('mFilter')
library(mFilter)

# Para el caso de HP se define el siguiente parametro:
#Lambda para ajustar el valor de la serie;

#lambda = 100 -> si los datos son anuales
#lambda = 1600 -> si los datos son trimestrales 
#lambda = 14400 -> si los valoes son mensuales

#GASTO
gasto_t_hp <- hpfilter(gral_tseas$g_des,
                      freq = 14400)
plot(gasto_t_hp)


fgastohp<- ts(gasto_t_hp$trend,
              start = c(1993,1),
              freq = 12)

ts.plot(fgastohp)


#INGRESO
ingreso_t_hp <- hpfilter(gral_tseas$i_des,
                       freq = 14400)
plot(gasto_t_hp)

fingresohp<- ts(ingreso_t_hp$trend,
              start = c(1993,1),
              freq = 12)

ts.plot(fingresohp)


#Transformamos el filtro HP en data frame

gral_t_hp <- data.frame(gasto_t_hp$trend,
                        gasto_t_hp$cycle,
                        ingreso_t_hp$trend,
                        ingreso_t_hp$cycle)
head(gral_t_hp)

# Filtro christiano Fritzgerald 

# GASTO

gasto_t_cf <- cffilter(gral_tseas$g_des)

plot(gasto_t_cf)

fgastocf<- ts(gasto_t_cf$trend,
              start = c(1993,1),
              freq = 12)

ts.plot(fgastocf)

#INGRESO

ingreso_t_cf <- cffilter(gral_tseas$i_des)

plot(ingreso_t_cf)

fingresocf<- ts(ingreso_t_cf$trend,
              start = c(1993,1),
              freq = 12)

ts.plot(fingresocf)


#Lo hacemos un data frame

gral_t_cf <- data.frame(gasto_t_cf$trend, 
                        gasto_t_cf$cycle,
                        ingreso_t_cf$trend, 
                        ingreso_t_cf$cycle)

# Comparamos filtros 

#GASTO 

ts.plot(fgastocf,fgastohp,
        col = c('green', 'red'),
        ldw = 2)

#INGRESO

ts.plot(fingresohp, fingresocf,
        col = c('green', 'red'),
        ldw = 2)

