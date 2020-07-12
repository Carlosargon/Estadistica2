#Cargar librerias 
pacman::p_load(readxl, dplyr,stargazer, car, sjlabelled, summarytools)
options(scipen=999)

#------- BASE DE DATOS N°1 -------
#Cargar base de datos
SIMCE2018 <- read_excel("input/simce2m2018_rbd_publica_final.xlsx")

#-----Explorar base-----
#Tipo de base
class(SIMCE2018)
#Dimension de la base
dim(SIMCE2018)
#Nombres en las bases 
names(SIMCE2018)
#Visualizar
View(SIMCE2018)

#Seleccionar variables
proc_SIMCE <- SIMCE2018 %>% select("nom_reg_rbd", "nom_rbd", "cod_grupo", 
                                   "prom_lect2m_rbd", "prom_mate2m_rbd", "prom_nat2m_rbd") 

View(proc_SIMCE)


#Renombrar variables
proc_SIMCE <- proc_SIMCE %>% rename("GSE" = cod_grupo)
proc_SIMCE <- proc_SIMCE %>% rename("region" = nom_reg_rbd)
proc_SIMCE <- proc_SIMCE %>% rename("establecimiento" = nom_rbd)
proc_SIMCE <- proc_SIMCE %>% rename("prom_lectura" = prom_lect2m_rbd)
proc_SIMCE <- proc_SIMCE %>% rename("prom_matematica" = prom_mate2m_rbd)
proc_SIMCE <- proc_SIMCE %>% rename("prom_csociales" = prom_nat2m_rbd)



#----------BASE DE DATOS N°2---------

#Cargar base de datos
IDPS2018 <- read_excel("input/idps_2m2018.xlsx")

#Explorar base
class(IDPS2018)
dim(IDPS2018)
names(IDPS2018)
View(IDPS2018)

#Seleccionar variables
proc_IDPS <- IDPS2018 %>% select("NOM_RBD", "NOM_REGI_N", "ind_cc_rbd")
View(proc_IDPS)

#Renombrar variables
proc_IDPS <- proc_IDPS %>% rename("establecimiento" = NOM_RBD)
proc_IDPS <- proc_IDPS %>% rename("region" = NOM_REGI_N)
proc_IDPS <- proc_IDPS %>% rename("ind_clima_conv" = ind_cc_rbd)


#-----Union bases de datos-----
SIM_IDPS <- cbind(proc_IDPS, proc_SIMCE)

#----Casos perdidos
#Dimension base
dim(SIM_IDPS)
#Contar casos perdidos 
sum(is.na(SIM_IDPS))
#identificar casos perdidos 
which(is.na(SIM_IDPS))
#Omitir casos perdidos
SIM_IDPS <- na.omit(SIM_IDPS)
#Rectificar
sum(is.na(SIM_IDPS))


