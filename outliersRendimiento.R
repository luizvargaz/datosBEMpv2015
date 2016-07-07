#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Construir boxplot con datos del rendimiento
#               Luis Vargas l.vargas@cgiar.org
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Set work directory
setwd('C:/Users/LVARGAS/Documents/CIMMYT/Bases_de_datos/2016 Consultas/2016-05-31 BEM corte mayo')
#setwd('C:/Users/CIMMYT-STUDENTS-AC/Desktop/Luis')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Funcion para leer un libro de excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Funcion para leer un libro de excel y obtener los datos de una hoja determinada
#install.packages('readxl')

abrirExcel <- function(nombreArchivo, numeroHoja){
        
        # Fijar el directorio de trabajo
        numeroHoja = as.numeric(numeroHoja)
        
        extensionArchivo = '.xlsx'
        archivo <-c('./',nombreArchivo,extensionArchivo)
        nombreArchivo <- paste(archivo,collapse="")
        
        library(readxl)
        read_excel(nombreArchivo, sheet = numeroHoja)
        
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get data from Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Utilizar la funcion para obtener los datso del archivo excel y almacenarlos en un objeto
rendimientoRawNA <- abrirExcel('EXPORTAR pv 2015 com', 24)
str(rendimientoRawNA)
### Procedimiento para eliminar las filas que contengan valores NA en todos sus registros
valoresNA <- is.na(rendimientoRawNA[,1])
rendimientoRaw <- rendimientoRawNA[!valoresNA,]

### Detectar registros duplicados y crear un nuveo data fram sin registros duplicados
print('Registros duplicados: '); print(anyDuplicated(rendimientoRaw))
rendimiento <- unique(rendimientoRaw)
print('_ _ _ _ Se han eliminado los registros duplicados')
#head(rendimiento)
#str(rendimiento)

### Filtrar los registros de Tipo de parcela 'Parcela Área de Impacto' del dataframe
rendimientoParcelas <- rendimiento[rendimiento$`Tipo de parcela (testigo o innovación)` != 'Parcela Área de Impacto',]
#str(rendimientoParcelas)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subsets de cultivo, producto cosechado y unidad de medida
## Limpieza de datos
## contrucciond e boxplot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ````````````````````````````````````````````````````````````````````````````````````````
# Funcion para encontrar valores extremo superior y extremo inferior
# ````````````````````````````````````````````````````````````````````````````````````````
extremos <- function(vectorDatos, rendimiento = 'SI'){ # Si es analisis de rendimiento, colocar SI al usar la funcion, para evitar obtener un valor minimo negativo
        q75 <- quantile(vectorDatos, 0.75)
        q25 <- quantile(vectorDatos, 0.25)
        ric <- q75 - q25
        valorMaximo <- q75 + (ric * 1.5)
        valorMaximo <- as.vector(valorMaximo)
        if(rendimiento == 'SI'){
                valorMinimo = 0
        }else{
                valorMinimo <- q25 - (ric * 1.5)
                valorMinimo <- as.vector(valorMinimo)
        }
        
        valores <- c(valorMaximo, valorMinimo)
}

#````````````````````````````````````````````````````````````````````````````````````````
library(dplyr) # para construccion de la grafica
library(tidyr) # para construccion de la grafica
library(ggplot2) # para construccion de la grafica
library(stringr) # Para eliminar los espacios de la cadena de nombre del archivo

vectorCultivos <- unique(rendimientoParcelas$`Nombre del cultivo cosechado`)

conteoParaArchivo <- 0
for(cultivo in vectorCultivos){
        
        rendimientoParcelasCultivo <- rendimientoParcelas[rendimientoParcelas$`Nombre del cultivo cosechado` == cultivo,]
        
        vectorProducto <- unique(rendimientoParcelasCultivo$`Nombre del producto de interés económico obtenido`)
        
        for(producto in vectorProducto){
                
                rendimientoParcelasCultivoProducto <- rendimientoParcelasCultivo[rendimientoParcelasCultivo$`Nombre del producto de interés económico obtenido` == producto,]
                
                vectorUnidad <- unique(rendimientoParcelasCultivoProducto$`Unidad de medida de rendimiento para el producto de interés económico obtenido`)
                
                for(unidad in vectorUnidad){
                        
                        
                        rendimientoParcelasCultivoProductoUnidad <- rendimientoParcelasCultivoProducto[rendimientoParcelasCultivoProducto$`Unidad de medida de rendimiento para el producto de interés económico obtenido` == unidad,]
                        
                        vectorTipo <- unique( rendimientoParcelasCultivoProductoUnidad$`Tipo produccion`)
                        
                        for(tipo in vectorTipo){
                                
                                rendimientoParcelasCultivoProductoUnidadTipo <- rendimientoParcelasCultivoProductoUnidad[rendimientoParcelasCultivoProductoUnidad$`Tipo produccion` == tipo,]
                                
                                # Llamar a la función para encontrar los valores extremos
                                valoresExtremos <- extremos(rendimientoParcelasCultivoProductoUnidadTipo$`Rendimiento (unidad/ha)`)
                                
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                # Encontrar los outliers de un vector de datos, despues almacenalos en una nueva variable
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                ## Validar si un valor es un outlier, guardar T o F en un vector
                                count = 0       
                                for(i in rendimientoParcelasCultivoProductoUnidadTipo$`Rendimiento (unidad/ha)`){
                                        #print(i)
                                        if(count == 0){
                                                if(i > valoresExtremos[1] | i < valoresExtremos[2]){
                                                        esOutlier = TRUE
                                                }else{
                                                        esOutlier = FALSE
                                                }
                                                
                                        }else{
                                                if(i > valoresExtremos[1] | i < valoresExtremos[2]){
                                                        esOutlier = c(esOutlier, TRUE)
                                                }else{
                                                        esOutlier = c(esOutlier, FALSE)
                                                }
                                        }
                                        count = count + 1
                                        
                                }
                                
                                print(esOutlier)
                                
                                ## Crear una nueva columna en el set de datos con los valores V o F de outliers
                                rendimientoParcelasCultivoProductoUnidadTipo$rendimiento_outlier <- esOutlier
                                rendimientoParcelasCultivoProductoUnidadTipoSinoutlier <- rendimientoParcelasCultivoProductoUnidadTipo[rendimientoParcelasCultivoProductoUnidadTipo$rendimiento_outlier == FALSE,]
                                
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                # Fin de: Encontrar los outliers de un vector de datos, despues almacenalos en una nueva variable
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                # Almacenar los subset de datos sin outliers para que al final se escriban en un archivo
                                
                                if(conteoParaArchivo == 0){
                                        
                                        unionDatos <- rendimientoParcelasCultivoProductoUnidadTipoSinoutlier
                                        
                                }else{
                                        
                                        unionDatos <- rbind(unionDatos, rendimientoParcelasCultivoProductoUnidadTipoSinoutlier)
                                        
                                }   
                                
                                # Fin de almacenar los subset de datos sin outliers para que al final se escriban en un archivo
                                # ````````````````````````````````````````````````````````````````````````````````````````
                                conteoParaArchivo <- conteoParaArchivo + 1
                                
                                
                                vectorEstados <- unique(rendimientoParcelasCultivoProductoUnidadTipoSinoutlier$Estado)
                                
                                for(estado in vectorEstados){
                                        
                                        rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado <- rendimientoParcelasCultivoProductoUnidadTipoSinoutlier[rendimientoParcelasCultivoProductoUnidadTipoSinoutlier$Estado == estado,]
                                        
                                        uni <- sub('/ha', ' ha-1 ',unidad,)
                                        ao <- unique(rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$Año)
                                        ciclo <- unique(rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$Ciclo)
                                        
                                        etiqueta <- paste(estado, cultivo, producto, tipo, ao, ciclo)
                                        print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                                        print(etiqueta)
                                        
                                        # write.table(utilidadResumen, file = "utilidadResumenParcela.csv", sep = ",",row.name=FALSE)
                                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        ## Obtener las medias de cada grupo 
                                        medias <- tapply(rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$`Rendimiento (unidad/ha)`, rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$`Tipo de parcela (testigo o innovación)`, mean)
                                        medias <- data.frame(medias)
                                        vectorMedias <- round(medias[,1], digits = 1) # Redondear numero https://stat.ethz.ch/R-manual/R-devel/library/base/html/Round.html
                                        
                                        # Pasos para agregar los valores a la grafica: http://stackoverflow.com/questions/28225777/full-text-label-on-boxplot-with-added-mean-point
                                        # Pasos para agregar número de observaciones: http://stackoverflow.com/questions/23330279/ggplot2-annotate-labelling-geom-boxplot-with-position-dodge
                                        dataFrame <- data.frame(variable = rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$`Tipo de parcela (testigo o innovación)`, value = rendimientoParcelasCultivoProductoUnidadTipoSinoutlierEstado$`Rendimiento real (unidad/ha)`)
                                        
                                        meanFunction <- function(x){
                                                return(data.frame(y = round(mean(x), 2),label = round(mean(x, na.rm = T), 2)))
                                        }
                                        
                                        fun_length <- function(x){
                                                return(data.frame(y = median(x),label = paste0("n = ", length(x))))
                                        }
                                        
                                        
                                        g <- ggplot(data = dataFrame, aes(x = variable, y = value, fill = variable)) + theme_minimal() +
                                                geom_boxplot(width = 0.5) + scale_fill_brewer(palette = 'Paired') + ggtitle(etiqueta) + # para eliminar el eje x:  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
                                                labs(fill = 'Tipo de parcela', x = ' ', y = paste('Rendimiento', str_replace_all(string = paste('(', unidad, ')'), pattern = " ", repl = ""))) +
                                                stat_summary(fun.y = mean, geom = 'point', shape = 18, colour = 'darkred', size=4) +
                                                stat_summary(fun.data = meanFunction, geom = 'text', color = 'white', size = 5, vjust = 1.3) +
                                                stat_summary(fun.data = fun_length, geom = 'text', position=position_dodge(width = 0.9), size = 5, vjust = 4)
                                        
                                        df <- ggplot_build(g)$data[[1]] %>%
                                                select(ymin:ymax, x) %>%
                                                gather(type, value, - x) %>%
                                                arrange(x)
                                        
                                        g <- g + annotate("text", x = df$x + 0.3, y = df$value, label = df$value, size = 3)
                                        print(g)
                                        
                                        if(dir.exists('./grapBoxplotYieldGg')){
                                                print('Existe el directorio para guardar la grafica')
                                                print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                                        }else{
                                                dir.create('./grapBoxplotYieldGg')
                                                print('Se ha creado el directorio para guardar la grafica')
                                                print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                                        }
                                        
                                        #install.packages('stringr')
                                        etiquetaGrafica <- paste(estado,'_', cultivo, '_', producto, '_', uni, '_', tipo, '_',ao, '_',ciclo)
                                        nombreGrafica <- str_replace_all(string = paste('./grapBoxplotYieldGg/', etiquetaGrafica, ".png"), pattern=" ", repl="")
                                        
                                        
                                        dev.copy(png, file = nombreGrafica, width=800, height=800)
                                        dev.off()
                                }
                                
                                
                        }
                        
                        
                }
                
        }
        
}

# exportar el data frame que contiene los datos que se han almacenado en el objeto unionDatos
write.csv(unionDatos , file = "RENDIMIENTO_sinOtliers.csv", row.names = FALSE)
print('The exportation has been successful!')
