#problem set 3
#Diana Cano Sánchez-202110997
# R version 4.3.1 (2023-06-16 ucrt)


#Cargar las librerias necesarias

library(dplyr)
library(data.table)
library(ggplot2)

# Establecer la ruta de la carpeta input

ruta_input <- "C:/Users/PERSONAL/OneDrive - Universidad de los andes/Universidad de Los Andes/Sexto Semestre/Taller R/Problem sets/#3/Taller_3/input"

# Punto 1 -----------------------------------------------------------------

#1.1

# Obtener la lista de archivos en la carpeta input y sus subcarpetas

archivos <- list.files(path = ruta_input, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

#Lo que se hace es guardar en el objeto archivo la lista de archivos que están dentro de ruta_input
# y dentro de sus subcarpetas (recursive=TRUE) trayendo sus nombres completos y que tengan extensión .csv
#deberían ser 356: 120 para 2019, 116 para 2020 y 120 para 2021.

# Mostrar la lista de archivos

print(archivos)

#Comentario: Sólo en 2021 trae 90 archivos no está trayendo lo de Octubre, Noviembre y Diciembre

#1.2 Todos los archivos de la carpeta input

#Esta función guarda en el df TODOS los archivos .csv dentro de la carpeta input.

importar_archivo <- function(ruta_archivo) {
  # Importar los archivos CSV, tener en cuenta que cada uno tiene encabezado y esta separado por ;
  # los cuales se encuentran en una ruta especificada (ruta_archivo)
  #Todo se almacena en el df, el cual será de tipo dataframe
  
  df <- read.csv(ruta_archivo, header = T, sep = ";")
  
  # Agregar una columna(variable) con el año al objeto df. 
  #El valor de año lo agrega al buscar en el ruta_archivo un patrón de la forma"/xxxx/" es decir los 4 dígitos
  # que forman el valor del año
  #(\\d{4}) captura esos cuatro dígitos como un grupo. 
  #Luego, \\1 se refiere al primer grupo capturado, que son los cuatro dígitos del año.
  #as.numeric convierte el año extraído a formato numérico y por último se asigna el valor a la nueva columna year
  
  df$year <- as.numeric(sub(".*/(\\d{4})/.*", "\\1", ruta_archivo))
  
  return(df) #se retorna todo el df modificado incluido la columna year
}


# Aplicar la función importar_archivo creada arriba a cada archivo en la lista, es decir a cada uno de los archivos (lista
# guardada en el objeto archivos
#El objeto lista_dataframes es una lista de dataframes donde cada uno corresponde a cada archivo.csv de los que hay en la lista de archivos guardada en el objeto archivos

lista_dataframes <- lapply(archivos, importar_archivo)

# Manera en que se puede acceder a un dataframe para verificar que todo está bien,

primer_dataframe <- lista_dataframes[[1]]


#Unir todo los dataframes para formar uno solo que se asigna al objeto cg.
#Como no todos tiene las mismas dimensiones (columnas) se agrega fill=T para que R haga este ajuste y la línea de código no genere error

cg <- rbindlist(lista_dataframes, fill = TRUE)
str(cg)

#Exportar cg a un archivo de excel
#ruta donde se va a guardar

setwd('C:/Users/PERSONAL/OneDrive - Universidad de los andes/Universidad de Los Andes/Sexto Semestre/Taller R/Problem sets/#3/Taller_3/output/') #AJUSTARLA ANTES DE CORRER WRITE.CSV

write.csv(cg, "Dataframe_FinalP1.csv") #como el df es muy grande va a ser muy pesado (GB) como resultado

# Punto 2 -----------------------------------------------------------------


cg1 <- cg[!is.na(cg$AREA)]

unique(cg1$AREA)

areas_deseadas <- c(5,8,11,13,17,23,50,52,54,66,68,73,76)

cg1_f <- cg1[cg1$AREA %in% areas_deseadas]

unique(cg1_f$AREA)

cg1_f <- cg1_f %>%
  mutate(AREA = case_when(
    AREA %in% c(5) ~ "Medellin",
    AREA %in% c(8) ~ "Barranquilla",
    AREA %in% c(11) ~ "Bogota D.C",
    AREA %in% c(13) ~ "Cartagena",
    AREA %in% c(17) ~ "Manizales",
    AREA %in% c(23) ~ "Monteria",
    AREA %in% c(50) ~ "Villavicencio",
    AREA %in% c(52) ~ "Pasto",
    AREA %in% c(54) ~ "Cucuta",
    AREA %in% c(66) ~ "Pereira",
    AREA %in% c(68) ~ "Bucaramanga",
    AREA %in% c(73) ~ "Ibague",
    AREA %in% c(76) ~ "Cali",
    TRUE ~ as.character(AREA)
  ))

unique(cg1_f$AREA) #ver que los cambios anteriores quedaran bien hechos

#Visualización 1 #Area y Escolaridad de Área caracteristicas generales (Personas) Pág 30


grafico_1 <- ggplot(cg1_f, aes(x = as.numeric(ESC))) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  facet_wrap(~AREA, nrow = 6, ncol = 4) +
  labs(title = "Años de Escolaridad por Área del País",
       x = "Años de Escolaridad (número)",
       y = "Frecuencia") +
  theme_minimal()+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold", colour = "red")) +
  scale_x_continuous(breaks = seq(0, 26, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 4000))

grafico_1

#Ver que el setwd este ajustado en la carpeta output
ggsave(plot = grafico_1, filename = 'Anos_Escolaridad.png',
       width = 40, height = 25, units = 'cm', dpi = 500)


#Visualización 2 Cabecera pág 62 Cabecera - Características generales (Personas)
#Area y #P6170 actualmente asiste a la universidad 1 si, 2 no

unique(cg1_f$P6170) #tiene valores NA que hay que quitarlos.

cg2 <- cg1_f[!is.na(cg1_f$P6170)]

unique(cg2$P6170)


cg2$P6170 <- factor(cg2$P6170, levels = c("1", "2"))
cg2$AREA <- factor(cg2$AREA)

grafico_2 <- ggplot(cg2, aes(x = P6170, fill = P6170)) +
  geom_bar(color = "black", position = "stack") + 
  facet_wrap(~AREA, nrow = 6, ncol = 4) +
  labs(title = "Distribución de la Asistencia a Preescolar, Escuela, Colegio o Universidad por Área del País",
       x = "Asistencia (lógica)",
       y = "Frecuencia") +
  theme_minimal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold", colour = "black")) +
  scale_fill_manual(values = c("1" = "orange", "2" = "orange"), guide = "none") + 
  scale_x_discrete(labels = c("1" = "NO", "2" = "SI")) +
  scale_y_continuous(breaks = seq(0, 60000, by = 4000))

grafico_2

#Ver que el setwd este ajustado en la carpeta output
ggsave(plot = grafico_2, filename = 'Asistencia_Escolar.png',
       width = 40, height = 25, units = 'cm', dpi = 500)
