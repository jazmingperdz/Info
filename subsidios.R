# Universidad Autonoma de Nuevo Leon
# Facultad de Economia
# Series INFONAVIT
## Subsidios y otros apoyos a la vivienda.

# Borrando la memoria 
rm(list=ls(all=TRUE))

# Cambiando el directorio 
setwd("C:/Users/monto/Desktop/INFONAVIT/Info")

# Instalando Librerias
install.packages("foreign")
install.packages("psych")
install.packages("questionr")
install.packages("modeest")

# Cargando las librerias necesarias.
library(psych)
library(base)          
library(foreign)       
library(questionr)     
library(ggplot2)
library(stats)
library(dplyr)
library(modeest)

# Cargando series 
numero=read.csv("NumeroSubs.csv")
monto=read.csv("MontoSubs.csv")
#Asignación de etiquetas de columnas

montosubs= rename(monto,  fecha = Glosario , Total.nacional = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit. , Vivienda.existente=Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.para.créditos.Línea.II..destinados.para.la.adquisición.de.vivienda.existente., Crédito.tradicional = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.al.producto.Crédito.Tradicional..para.adquirir.una.vivienda..nueva.o.existente..para.construir.en.terreno.propio..reparar..ampliar.o.mejorar.la.existente.o.para.pagar.pasivos., Línea.II.vivienda.nueva = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.para.créditos.Línea.II..destinados.para.la.adquisición.de.vivienda.nueva., Línea.II.vivienda.existente = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.para.créditos.Línea.III..destinados.para.la.construcción.de.vivienda., Línea.III.construcción = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.para.créditos.Línea.III..destinados.para.la.construcción.de.vivienda., Vivienda.Económica = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..para.Vivienda.Económica., Vivienda.Popular = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..para.Vivienda.Popular., Vivienda.Tradicional = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..para.Vivienda.Tradicional., Vivienda.Media = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..para.Vivienda.Media., Vivienda.Residencial = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..para.Vivienda.Residencial., Otros = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...como.complemento.a.un.crédito.Infonavit..sin.calsificación.de.vivienda.,
Hasta.20.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado.,
De.21.a.25.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..1, De.26.a.30.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..2,
De.31.a.35.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..3,
De.36.a.40.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..4,
De.41.a.45.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..5,
De.46.a.50.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..6,
De.51.a.55.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..7,
De.56.a.60.años = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..8, De.61.a.65.años =Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..9, de66.años.o.más = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.grupos.de.edad.del.derechohabiente.acreditado..10,
Otross = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi..y.que.no.cuentan.con.la.edad.del.derechohabiente.identificada.,
Aguascalientes = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito.,
Baja.California = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..1,
Baja.California.Sur = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..2,
Campeche = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..3,
Coahuila = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..4,
Colima = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..5,
Chiapas = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..6,
Chihuahua = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..7,
CDMX = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..8,
Durango = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..9,
México = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..10,
Guanajuato = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..11, Guerrero = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..12, Hidalgo = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..13, Jalisco = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..14, Michoacán =  Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..15,  Morelos = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..16, Nayarit = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..17, Nuevo.León = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..18, 
Oaxaca = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..19,
Puebla = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..20,
Querétaro = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..21,
Quintana.Roo = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..22,
San.Luis.Potosí = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..23,
Sinaloa = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..24,
Sonora = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..25,
Tabasco = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..26,
Tamaulipas = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..27,
Tlaxcala = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..28, 
Veracruz = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..29, 
Yucatán = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..30,
Zacatecas = Monto.de.subsidio.del.gobierno.federal.otorgado.por.la.Comisión.Nacional.de.Vivienda..Conavi...por.entidad.federativa.de.originación.del.crédito..31) 
montosub=montosubs[c(-1,-2,-3,-4,-5),]
montosub=montosub[,c(-(13:39))]

#Cálculo simple de estadíticos descriptivos
min = min(montosub$Total.nacional)
mediana = median.default(montosub$Total.nacional, na.rm = FALSE)
media=mean(montosub$Total.nacional)
moda = mfv(montosub$Total.nacional)
max = max(montosub$Total.nacional, na.rm = FALSE)

estadis_descrip = c(min, mediana, moda, max)
nombres <- c("Mínimo", "Mediana", "Moda", "Máximo")
descr2 <- as.data.frame(rbind(nombres, estadis_descrip))

write.csv2(descr2, file = "Subsidios.csv")
