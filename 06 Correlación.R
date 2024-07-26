#Correlación

Datos <- read_excel("Documents/R/Scripts Six Sigma/obl Datos Correlación.xlsx", 
                    +     sheet = "Sheet1")

#Objetivo: comprobar la relación estadística entre las variables
#.         por medio del coef. correlación de pearson (R)

#=======================
#Gráficas de dispersión
#=======================
  #Gráfica simple
  library(ggplot2)
  
  ggplot(Datos, aes(x=X1, y=Y)) + geom_point() 
  ggplot(Datos, aes(x=X2, y=Y)) + geom_point() 
  ggplot(Datos, aes(x=X3, y=Y)) + geom_point() 
  ggplot(Datos, aes(x=X4, y=Y)) + geom_point() 

  #Queremos ver la relación visual entre todas las variables
  #Matriz con Paquete BASE de R
  base::plot(Datos, pch=20, cex=0.5) #pch & cex: tamaño y solidez de cada punto.
  
#========================================
#Coeficiente de Correlación de Pearson
#========================================
  
  
  #Matriz con Paquete GGally
  library(GGally)
  
  ggpairs(Datos) #Despliega: dispersión, distribución y coef.corr.de Pearson (-1 a +1)
      #Opcional por nombre
      ggpairs(Datos[,c("Y","X1","X2")])

  #Matriz con Paquete STATS
  library(stats)
      
  cor(Datos)
  
#========================================
#Significancia Estadística (P-Value)
#========================================
  #Matriz con Paquete GGally
  library(GGally)
  
  ggpairs(Datos) #Despliega: dispersión, distribución y coef.corr.de Pearson (-1 a +1)
  
  # "***": Si el Valor P (P-Value) es <0.001
  #  "**": Si el Valor P (P-Value) es <0.01
  #   "*": Si el Valor P (P-Value) es <0.05
  #   ".": Si el Valor P (P-Value) es <0.10
  #   " ": Si el Valor P (P-Value) es >0.10      
  
  #Coef. Correlación Pearson & P-Value
  cor.test(x=Datos$X1, y=Datos$Y, conf.leve=0.95) #R=-0.971 P-Value<0.001
  cor.test(x=Datos$X2, y=Datos$Y, conf.leve=0.95) #R=-0.009 P-Value<0.916

  #Otros ejercicios: Cambiar variables en la siguiente linea
  cor.test(x=Datos$X1, y=Datos$X3, conf.leve=0.95) 
      
        

