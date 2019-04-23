# install.packages("OpenImageR")# paquete para el tratamiento de imagenes
# install.packages("ClusterR")# paquete para cluster

library(OpenImageR)
library(ClusterR)
library(scales)

img = readImage("../oscar/MEGAsync/R_Archivos/CNC-R/blackhole.png") # para leer la imagen

img_escalada = resizeImage(img, # este comando redimenciona la imagen
                           233, # Alto
                           400, # Ancho
                           method = "bilinear") # metodo de reescalado

imageShow(img_escalada) # para ver la imagen

img_vector = apply(img_escalada, # este comando me permite vectorizar la imagen
                   3, # 3 filas y columnas
                   as.vector) # como un vector

dim(img_vector) # para ver la cantidad de datos Rojo, Verde y Azul RGB
summary(img_vector)

## Tratamiento para crear el CNC ##
img_vector_cnc = img_vector

img_vector_cnc[,1] = rescale(img_vector_cnc[,1], 
                             to = c(0,200)) # escalo los pixeles en X
img_vector_cnc[,2] = rescale(img_vector_cnc[,2], 
                             to = c(0,200)) # escalo los pixeles en Y
img_vector_cnc[,3] = rescale(img_vector_cnc[,3], 
                             to = c(0,100)) # escalo los pixeles en Z


factor(paste("G01 X",img_vector_cnc[,1], sep = "")) # coloco X y G01
img_vector_cnc[,1] = paste("G01 X",img_vector_cnc[,1], sep = "")

factor(paste("Y",img_vector_cnc[,2], sep = "")) # coloco Y
img_vector_cnc[,2] = paste("Y",img_vector_cnc[,2], sep = "")

factor(paste("Z",img_vector_cnc[,3], sep = "")) # coloco Z
img_vector_cnc[,3] = paste("Z",img_vector_cnc[,3], sep = "")

write.table(img_vector_cnc, # aqu? solo guarde el primer intento
            file = "../oscar/MEGAsync/R_Archivos/CNC-R/BH.gcode", 
            sep = " ", 
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE
            )

str(img_escalada) # para saber el tipo de archivo

is.data.frame(img_escalada) # pregunto si es un data frame
img_escalada_bn = rgb_2gray(img_escalada) # la convierto en una imagen BN
imageShow(img_escalada_bn)
str(img_escalada_bn)


img_escalada_vec = apply(img_escalada_bn, # este comando me permite vectorizar la imagen
                        2, # 3 filas y columnas
                        as.matrix) # como un vector
str(img_escalada_vec)
cnc_Z = as.vector(img_escalada_vec)
str(cnc_Z)

i = 0
X1 = 0
n = 0

while (i < 400) { # se crea un patron de 300 veces cada n?mero hasta completar 120000
  n = rep(i, 233)
  i = i + 1
  X1 = c(X1, n)
}
str(X1)

X1 = as.vector(X1) # borro el dato que se pasa
X1 = X1[-c(93200)]   # para acceder a la posici?n del vector
str(X1)

Y = seq(1:233)
Y1 = rep(Y , 400) 
str(Y1)
Y1 = as.vector(Y1)


cnc_XYZ = data.frame(X1, Y1, cnc_Z, check.rows = FALSE) # Uno todos los pixeles

## Tratamiento para crear el CNC ##

cnc_XYZ[,1] = rescale(cnc_XYZ[,1], 
                      to = c(0,200)) # escalo los pixeles en X

cnc_XYZ[,2] = rescale(cnc_XYZ[,2], 
                      to = c(0,200)) # escalo los pixeles en Y

cnc_XYZ[,3] = rescale(cnc_XYZ[,3], 
                             to = c(0,50)) # escalo los pixeles en Z

## Para generar codigos G ##
factor(paste("G01 X",cnc_XYZ[,1], sep = "")) # coloco X y G01
cnc_XYZ[,1] = paste("G01 X",cnc_XYZ[,1], sep = "")

factor(paste("Y",cnc_XYZ[,2], sep = "")) # coloco Y
cnc_XYZ[,2] = paste("Y",cnc_XYZ[,2], sep = "")

factor(paste("Z",cnc_XYZ[,3], sep = "")) # coloco Z
cnc_XYZ[,3] = paste("Z",cnc_XYZ[,3], sep = "")
# hasta aqu? instrodusco las instrucciones G code


colnames(cnc_XYZ) = c("x", "y", "z")

## Nota para generar .xyz no ejecutar lo enterior ##

write.table(cnc_XYZ, # segundo intento guardo la tabla
            file = "../oscar/MEGAsync/R_Archivos/CNC-R/BH.xyz", # las extenci?n xyz para poder ver la nuebe de puntos en MeshLab
            sep = " ", 
            eol = "\n", dec = ".", row.names = FALSE,
            col.names = FALSE
            )



## Esta par juega conb la imagen ##

kmmb = MiniBatchKmeans(img_vector, # Mini Batch K-means t?cnica de cluster
                       clusters = 10, # numero de clusters puede realacionado con el numero de colores en este caso
                       batch_size = 20, # tama?o del lote
                       num_init = 5, # numero inicial de iteraciones
                       max_iters = 100, # numero m?ximo de iteraciones
                       init_fraction = 0.2, # % de datos para a utilizar para inciar los centroides
                       initializer = "kmeans++", # metodo a utilizar
                       early_stop_iter = 10, # para que no se pare antes de la iteraci?n 10
                       verbose = FALSE #  indica si el progreso se imprime durante el cluster (Agrupaci?n)
                       )

prmb = predict_MBatchKMeans(img_vector, # esta funci?n es la predicci?n de los colores
                            kmmb$centroids) # propiedad del cluster anteriors

get_cent_mb = kmmb$centroids # solo para tomar la variable de los centroides
new_img = get_cent_mb[prmb,] # de ellos tomo las filas de prmb y todas las columnas
dim(new_img) = c(nrow(img_escalada), # asigno el numero de filas de la imagen a comparar
                 ncol(img_escalada), # asigno el numero de columnas de la magen a comparar
                 3)

imageShow(new_img)




