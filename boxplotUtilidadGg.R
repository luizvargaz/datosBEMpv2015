#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Construir boxplot con datos de la utilidad
#               Luis Vargas l.vargas@cgiar.org
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Set work directory
setwd('C:/Users/LVARGAS/Documents/CIMMYT/Bases_de_datos/2016 Consultas/2016-05-31 BEM corte mayo')
#setwd('C:/Users/CIMMYT-STUDENTS-AC/Desktop/Luis')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get data UTILIDAD RESUMEN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### leer los datos del archivo en csv
utilidadResumenRawNA <- read.csv('./UTILIDAD/UTILIDAD_RESUMEN pv 2015csv.csv', as.is = T) ## Usar as.is = T para evirtar que los factores mantengas los niveles eleiminados despues de un subset http://stackoverflow.com/questions/1195826/drop-factor-levels-in-a-subsetted-data-frame  
# str(parcelaProductorRawNA); names(parcelaProductorRawNA); head(parcelaProductorRawNA, 3); tail(parcelaProductorRawNA, 3)

### Procedimiento para eliminar las filas que contengan valores NA en todos sus registros
valoresNA <- is.na(utilidadResumenRawNA[,1])
utilidadResumenRaw <- utilidadResumenRawNA[!valoresNA,]

### Detectar registros duplicados y crear un nuveo data frame sin registros duplicados
print('Registros duplicados: '); print(anyDuplicated(utilidadResumenRaw))
utilidadResumenCompleto <- unique(utilidadResumenRaw)
class(utilidadResumenCompleto)
#str(utilidadResumenCompleto)
print('_ _ _ _ Se han eliminado los registros duplicados')

### Filtrar el dataframe
utilidadResumenConFactores <- utilidadResumenCompleto[utilidadResumenCompleto$nb.Tipo.Parcela != 'Parcela Área de Impacto',]
# ----- La variable nb.Tipo.Parcela se almacena como un factor y conserva los niveles eliminados
# ----- es decir el valor de "Parcela Área de Impacto". Para eliminar los niveles almacenados en los factores, 
# ----- se utiliza la funcion droplevels, http://stackoverflow.com/questions/17217951/how-can-i-drop-unused-levels-from-a-data-frame
utilidadResumenConAlgunosCultivosVacios <- droplevels(utilidadResumenConFactores)
nrow(utilidadResumenConAlgunosCultivosVacios)

# ````````````````````````````````````````````````````````````````````````````````````````
# Funcion para encontrar valores extremo superior y extremo inferior
# ````````````````````````````````````````````````````````````````````````````````````````
extremos <- function(vectorDatos, rendimiento = 'NO'){ # Si es analisis de rendimiento, colocar SI al usar la funcion, para evitar obtener un valor minimo negativo
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Función grafica por cultivo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # para construccion de la grafica
library(tidyr) # para construccion de la grafica
library(ggplot2) # para construccion de la grafica
library(stringr) # Para eliminar los espacios de la cadena de nombre del archivo

# Eliminar los registros con cultivo cosechado nullo
dataUtilidadResumen <- utilidadResumenConAlgunosCultivosVacios[utilidadResumenConAlgunosCultivosVacios$nb.Cultivos.Cosechados !="", ]
nrow(dataUtilidadResumen)
vectorTipo <- unique(dataUtilidadResumen$nb.Tipo.Produccion)

conteoParaArchivo <- 0
for (tipo in vectorTipo) {
        dataUtilidadResumenTipoOutlier <- dataUtilidadResumen[dataUtilidadResumen$nb.Tipo.Produccion == tipo,]
       
        valoresExtremos <- extremos(dataUtilidadResumenTipoOutlier$RENTABILIDAD....ha.)
       
        # ````````````````````````````````````````````````````````````````````````````````````````
        # Encontrar los outliers de un vector de datos, despues almacenalos en una nueva variable
        # ````````````````````````````````````````````````````````````````````````````````````````
        ## Validar si un valor es un outlier, guardar T o F en un vector
        count = 0       
        for(i in dataUtilidadResumenTipoOutlier$RENTABILIDAD....ha.){
                print(i)
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
        dataUtilidadResumenTipoOutlier$RENTABILIDAD....ha_outlier <- esOutlier
        dataUtilidadResumenTipo <- dataUtilidadResumenTipoOutlier[dataUtilidadResumenTipoOutlier$RENTABILIDAD....ha_outlier == FALSE, ]
        
        # ````````````````````````````````````````````````````````````````````````````````````````
        # Almacenar los subset de datos sin outliers para que al final se escriban en un archivo
        
        if(conteoParaArchivo == 0){
                
                unionDatos <- dataUtilidadResumenTipo
                
        }else{
                
                unionDatos <- rbind(unionDatos, dataUtilidadResumenTipo)
                
        }   
        
        # Fin de almacenar los subset de datos sin outliers para que al final se escriban en un archivo
        # ````````````````````````````````````````````````````````````````````````````````````````
        conteoParaArchivo <- conteoParaArchivo + 1
        
        
        vectorEstados <- unique(dataUtilidadResumenTipo$nb.Estado)
        
        for(estado in vectorEstados){
                
                dataUtilidadResumenTipoEstado <- dataUtilidadResumenTipo[dataUtilidadResumenTipo$nb.Estado == estado,]
                
                vectorCultivos <- unique(dataUtilidadResumenTipoEstado$nb.Cultivos.Cosechados)
                
                for(cultivo in vectorCultivos){
                        
                        # Subset de datos por cultivo y crear etiqueta para el titulo y nombre de la  grafica 
                        dataUtilidadResumenTipoEstadoCultivo <- dataUtilidadResumenTipoEstado[dataUtilidadResumenTipoEstado$nb.Cultivos.Cosechados == cultivo, ]
                       
                        ao <- unique(dataUtilidadResumenTipoEstadoCultivo$Anio)
                        ciclo <- unique(dataUtilidadResumenTipoEstadoCultivo$nb.Ciclo)
                        etiqueta <- paste(estado, cultivo, tipo, ao, ciclo)
                        print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                        print(etiqueta)
                                
                        # write.table(utilidadResumen, file = "utilidadResumenParcela.csv", sep = ",",row.name=FALSE)
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        ## Obtener las medias de cada grupo 
                        medias <- tapply(dataUtilidadResumenTipoEstadoCultivo$RENTABILIDAD....ha., dataUtilidadResumenTipoEstadoCultivo$nb.Tipo.Parcela, mean)
                        medias <- data.frame(medias)
                        vectorMedias <- round(medias[,1]) # Redondear numero https://stat.ethz.ch/R-manual/R-devel/library/base/html/Round.html
                                
                        # Pasos para agregar los valores a la grafica: http://stackoverflow.com/questions/28225777/full-text-label-on-boxplot-with-added-mean-point
                        # Pasos para agregar número de observaciones: http://stackoverflow.com/questions/23330279/ggplot2-annotate-labelling-geom-boxplot-with-position-dodge
                        dataFrame <- data.frame(variable = dataUtilidadResumenTipoEstadoCultivo$nb.Tipo.Parcela, value = dataUtilidadResumenTipoEstadoCultivo$RENTABILIDAD....ha.)
                        
                        meanFunction <- function(x){
                                return(data.frame(y = round(mean(x), 2),label = round(mean(x, na.rm = T), 2)))
                        }
                        
                        fun_length <- function(x){
                                return(data.frame(y = median(x),label = paste0("n = ", length(x))))
                        }
                        
                        
                        g <- ggplot(data = dataFrame, aes(x = variable, y = value, fill = variable)) + theme_minimal() +
                                geom_boxplot(width = 0.5) + scale_fill_brewer(palette='Paired') + ggtitle(etiqueta) + # para eliminar el eje x:  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
                                labs(fill = 'Tipo de parcela', x = ' ', y = 'Utilidad ($/ha)') +
                                stat_summary(fun.y = mean, geom = 'point', shape = 18, colour = 'darkred', size=4) +
                                stat_summary(fun.data = meanFunction, geom = 'text', color = 'white', size = 5, vjust = 1.3) +
                                stat_summary(fun.data = fun_length, geom = 'text', position=position_dodge(width = 0.9), size = 5, vjust = 4)
                        
                        df <- ggplot_build(g)$data[[1]] %>%
                                select(ymin:ymax, x) %>%
                                gather(type, value, - x) %>%
                                arrange(x)
                        
                        g <- g + annotate("text", x = df$x + 0.3, y = df$value, label = df$value, size = 3)
                        print(g)
                        
                        if(dir.exists('./grapBoxplotProfit')){
                                print('Existe el directorio para guardar la grafica')
                                print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                        }else{
                                dir.create('./grapBoxplotProfit')
                                print('Se ha creado el directorio para guardar la grafica')
                                print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
                        }
                                
                        library(stringr) # Para eliminar los espacios de la cadena de nombre del archivo
                        etiquetaGrafica <- paste(estado,'_', cultivo, '_', tipo, '_',ao, '_',ciclo)
                        nombreGrafica <- str_replace_all(string = paste('./grapBoxplotProfit/', etiquetaGrafica, ".png"), pattern=" ", repl="")
                                
                        dev.copy(png, file = nombreGrafica, width=800, height=800)
                        dev.off()
                        
                }
                
        }
        
}

# exportar el data frame que contiene los datos que se han almacenado en el objeto unionDatos
write.csv(unionDatos , file = "UTILIDAD_RESUMEN_sinOtliers.csv", row.names = FALSE)
print('The exportation has been successful!')
