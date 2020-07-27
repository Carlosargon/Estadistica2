
#         CODIGO DE PREPARACION 

# Cargar librerias 
pacman::p_load(readxl, dplyr,stargazer, car, sjlabelled, summarytools)
options(scipen=999)

#---------BASE DE DATOS N°1 -------
# Cargar base de datos
SIMCE2018 <- read_excel("input/data/original/simce2m2018_rbd_publica_final.xlsx")

#-----Explorar base
#Tipo de base
class(SIMCE2018)
#Dimension de la base
dim(SIMCE2018)
#Nombres en las variables 
names(SIMCE2018)
#Visualizar
View(SIMCE2018)

#----------BASE DE DATOS N°2---------

# Cargar base de datos
IDPS2018 <- read_excel("input/data/original/idps_2m2018.xlsx")

#-----Explorar base
#Tipo de base
class(IDPS2018)
#Dimension de la base
dim(IDPS2018)
#Nombres de las variables
names(IDPS2018)
#Visualizar
View(IDPS2018)

# Renombrar variable RBD
IDPS2018 <- IDPS2018 %>% rename("rbd" = RBD)

#-----Union bases de datos-----

SIM_IDPS<- left_join(x = IDPS2018,y = SIMCE2018, by ="rbd")
View(SIM_IDPS)

# Seleccionar variables
proc_SIM_IDPS <- SIM_IDPS %>% select("nom_reg_rbd","cod_grupo.y",  "ind_cc_rbd", "ind_am_rbd",
                                   "prom_lect2m_rbd", "prom_mate2m_rbd", "prom_nat2m_rbd") 


View(proc_SIM_IDPS)

proc_SIM_IDPS2 <- iris[iris$Species == "setosa",]

# Renombrar variables
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("GSE" = cod_grupo.y)
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("prom_lectura" = prom_lect2m_rbd)
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("prom_matematica" = prom_mate2m_rbd)
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("prom_csociales" = prom_nat2m_rbd)
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("ind_clima" = ind_cc_rbd)
proc_SIM_IDPS <- proc_SIM_IDPS %>% rename("ind_autoestima" = ind_am_rbd)

# Verificar
View(proc_SIM_IDPS)

#------Casos perdidos 

#Dimension base
dim(proc_SIM_IDPS)
#Contar casos perdidos 
sum(is.na(proc_SIM_IDPS))
#identificar casos perdidos 
which(is.na(proc_SIM_IDPS))
#Omitir casos perdidos
proc_SIM_IDPS <- na.omit(proc_SIM_IDPS)
#Rectificar
sum(is.na(proc_SIM_IDPS))

# Verificar etiquetas de la base
sjlabelled::get_label(proc_SIM_IDPS)

# Generar etiquetas
proc_SIM_IDPS$GSE <- set_label(x = proc_SIM_IDPS$GSE,label = "Grupo socioeconómico")
proc_SIM_IDPS$ind_clima <- set_label(x = proc_SIM_IDPS$ind_clima,label = "Puntaje Indicador Clima de Convivencia escolar del colegio")
proc_SIM_IDPS$ind_autoestima <- set_label(x = proc_SIM_IDPS$ind_autoestima,label = "Puntaje Indicador Autoestima académica y motivación escolar del colegio")
proc_SIM_IDPS$prom_lectura <- set_label(x = proc_SIM_IDPS$prom_lectura,label = "Puntaje promedio del establecimiento en Lectura")
proc_SIM_IDPS$prom_matematica <- set_label(x = proc_SIM_IDPS$prom_matematica,label = "Puntaje promedio del establecimiento en Matemática")
proc_SIM_IDPS$prom_csociales <- set_label(x = proc_SIM_IDPS$prom_csociales,label = "Puntaje promedio del establecimiento en Ciencias Sociales")

# Verificar etiquetas 
get_label(proc_SIM_IDPS$GSE)
get_label(proc_SIM_IDPS$ind_clima)
get_label(proc_SIM_IDPS$ind_autoestima)
get_label(proc_SIM_IDPS$prom_lectura)
get_label(proc_SIM_IDPS$prom_matematica)
get_label(proc_SIM_IDPS$prom_csociales)

# Tabla estadisticos descriptivos
stargazer(proc_SIM_IDPS,type = "text")

dfSummary(proc_SIM_IDPS, plain.ascii = FALSE)
view(dfSummary(proc_SIM_IDPS, headings=FALSE))

# Guardar base de datos procesados 
save(proc_SIM_IDPS,file= "input/data/proc/proc_SIM_IDPS.RData" )



