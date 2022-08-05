# SCRIPT GUIA BASICO PARA ELABORAR MODELOS DE DOSIS RESPUESTA
# Autor: Andrés N. Martin
# 
# Lista de comandos de posible uso, elegir las que sean necesarias
# para correr linea por linea, cambiar las variables por las propias

# Elegir mirror para los paquetes (el más cercano)
chooseCRANmirror()

# Instalar paquetes "drc" y "lattice" (lattice ya está por defecto en gral)
library(drc)
library(lattice)

# Ver en que directorio estan los datos cargados
dir()
getwd()
# Cerciorarse que se esta trabajando en el mismo directorio en el que R carga los datos

# Seteo del directorio de trabajo
setwd("/path/to/nnn")

# ayuda
?operadorQueQuiera

## CARGA DE DATOS
# Desde un .csv (inconveniente segun compatibilidad de caracteres)
d <- read.csv("datos.csv", header = TRUE, sep = ";")
# de un archivo de texto
d <- read.delim("datos.txt",dec = ",")
# desde el portapapeles: se copia la matriz desde excel (o calc) luego uso esto (sin pegar en ningun lado)
d <- read.table("clipboard" , header=TRUE ,dec = "," , sep="\t", blank.lines.skip = TRUE)
# chequear bien el tema de caracteres y decimales

# usar datos de alguno de los ejemplos del paquete. Por ejemplo "spinach"
d <- spinach

# Mirar que grupos de datos son los que estan cargados
ls()

# Borrar toda la memoria
rm(list=ls())

## RUTINA DE TRABAJO
# Corroborar que los datos han sido bien cargados

# Observar los datos en formato de tabla
View(d)
# o sino ver las cabeceras
head(d)

# Comprobar la estructura de los datos
str(d)

# Observar los datos según niveles de factor (cambiar HERBICIDE por el que desea, ver con str(d) )
split(d,d$HERBICIDE)

## Primera aproximacion exploratoria
# Ver un resumen de los estadisticos principales
summary(d)
summary(d$HERBICIDE)
# Si media es = a mediana, datos podrían ser distribucion normal

# grafico para ver que distribucion tienen los datos
xyplot(SLOPE ~ DOSE | factor(HERBICIDE),   # slope en funcion de dose para cada factor
       data=d,
       scales=list(x = list(log=10)),   # eje x en escala logaritmica
       as.table=TRUE)       # graficos como tabla
xyplot(SLOPE ~ DOSE , data=d, groups=HERBICIDE , panel = panel.superpose, type=c("r"))
# Agregar una linea con las medias
xyplot(DW60p ~ Dose | factor(Pobl) , data=d2, groups=Pobl,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.linejoin(x, y, horizontal = FALSE, ...)
       },
       xlab = "x",
       ylab = "y") #
       
# Ver alguna estadistica de un grupo en particular
by(d , list(d$HERBICIDE) , summary)
tapply(d$DW28, d$Pobl, mean, na.rm = TRUE)   # na.rm = TRUE : le dice a R que no tome en cuenta los NA
tapply(d2$fitness, list(d2$trat,d2$Dose), mean, na.rm = TRUE)
mean(d4[,"LER01"], na.rm=TRUE)

## TRABAJAR CON LAS TABLAS

# Seleccionar filas o columnas dentro del conjunto de datos
# data.frame[índice de fila, índice de columna]
d2 <- d[1:25 , ]   # selecciona de la fila 1 a 25
d2 <- d[ which( d$Pobl == "Sus" & d$herb == "Axial" ),  ]  # selecciona poblacion Sus y herbicida Axial

# Si quiero eliminar algunos datos de ese data.frame
d3 <- d2[-c(1,5),]          # [-c(números de filas),(números de columnas)]
# se crea otro data.frame d3 al cual se le borraron (-c) las filas y/o columnas especificadas

# Corregir o reemplazar uno o dos datos
edit(d)   # Luego corrijo el dato en la tabla que aparece
d2 <- edit(data.frame(d))
data.frame[row_number, column_number] = new_value # otra forma, quizás mejor
d[3,4] = 1.39075   
df[df == "oldvalue"] = "newvalue"     # busca el valor y lo cambia
d[d == 1.39075] = 1.3908 

d2$nueva.variable <- 100 * d2$variable # si quiero añadir una columna con una operacion

## CONSTRUCCION DEL MODELO

m <- drm(SLOPE ~ DOSE, HERBICIDE, fct=LL.4(fixed=c(NA,NA,NA,NA), data=d, na.action=na.omit))
# SURV en funcion de DOSE, aqui HERBICIDE es el factor que define niveles de tratamiento
# (ver con str(d) como estan organizados los datos)
# fct=LL.4() : significa funcion log logistica de 4 parametros. 
# con fixed se le indica al modelo que tiene que ajustar con algunos parametros fijos:
    # El orden de los parametros es: b,c,d,e  donde
    # b : es la pendiente en la ED50
    # c : es el limite inferior (lower limit)
    # d : es el limite superior (upper limit)
    # e : es la ED50  
    # NA significa que no hay valores asignados
# na.action: decide que hacer con los datos perdidos. =na.omit significa que los va a omitir. 
#    Por default el modelo falla si hay datos faltantes
?drm  # ver por mas funciones

# Ajuste del modelo
modelFit(m1)
# Notar que el p-value debe ser MAYOR a 0.05 para afirmar que el test ajusta significativamente

# Calculo de DL 50 y DL 90 (puedo elegir una sola DL)
ED(m1, c(50,90))

# Comparar curvas
EDcomp(m1, c(50, 50))
EDcomp(m1, c(50, 50), interval="delta")
?EDcomp 

# ANOVA
fitaov <- aov( SLOPE ~ factor(DOSE)*factor(HERBICIDE),data=d)
anova(fitaov)

## GRAFICO
plot(m1,  type="average",    # type: tipo de grafico ("average"|"all"|"obs"|"bars"|"none")
     main="Pyroxulam", sub="",   # Titulo y subtitulo del grafico
     xlab="Dosis [g.p.a/ha]", ylab="Peso seco 60 días [g/pl]",  # Leyendas de los ejes
     xlim=c(0,50), ylim=c(0,120),   # limites de los ejes x e y (ej: de 0 a 50 para el eje x)
     xt=c(0,5,10,20,50,100),   # fijar las marcas sobre el eje x
     col=c(4,2,3), pch=c(1,2,3), 
     cex=1, lwd=1,  # lwd: grosor de simbolos ; cex: tamaño relativo de los simbolos
     cex.main=1.2 ,  # Se puede usar cex.axis, cex.label, cex.main, cex.sub y cex.legend 
                     # para eje, los labels, el titulo, el subtitulo y la leyenda
     font=1, # tipo de letra: 1 normal, 2 cursiva, 3 negritas, 4 negritas cursivas
     cex.sub=0.8 , cex.axis=0.9 , cex.lab=0.9 , cex.legend=1 ,
     legendText=c("x1","x2") ,  # puede no ser necesario si los niveles de factor tienen el titulo adecuado
     legendPos=c(100,100) , # legendPos: dice la posición de la legenda en funcion de los ejes x,y (aqui y=100 y x=100)
     mar=c(5,4,2,2)+0.1   # vector numerico con la forma: c(bottom, left, top, right)
                           # numero de lineas de margen a ser especificado en los cuatro lados
                           # The default is c(5, 4, 4, 2) + 0.1.
)

?plot.drc  # para mas opciones

## PUBLICAR RESULTADOS

# Para crear archivo txt con la salida de los resultados

fitaov <- aov( DW60p ~ factor(Dose)*factor(Pobl),data=d)
sink("tituloDelArchivo.txt")
summary(m1)
ED(m1, c(50))
SI(m1, c(50,50))
modelFit(m1)
anova(fitaov)
sink()   # cierra 

# Para crear el archivo de grafico:
jpeg("Nombre.jpg",width = 600, height = 600, units = "px", pointsize = 12,
     quality = 100)
# Para publicar se precisa buena resolucion
png("Plot3.png", width = 1200, height = 840, res = 300, pointsize=9) # Me funciono bien. Chequear el pointsize

plot(x, ...) # Ir a la sentencia plot que se desea utilizar
dev.off()  # Crea el archivo

# exportar tabla de datos con la que se hicieron los modelos
write.table(d2, "/path/tabla.txt", sep="\t") 

# Fin!!!!!
q()
