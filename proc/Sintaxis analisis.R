#           CODIGO DE ANALISIS

#Cargar librerias 
pacman::p_load(dplyr, sjmisc, summarytools, sjPlot, ggplot2, webshot,
               ggpubr, gridExtra, texreg)
webshot::install_phantomjs( force = T)

# Abrir base de datos 
load(file= "input/data/proc/proc_SIM_IDPS.RData")

# Tabla estadisticos descriptivos 
view(dfSummary(proc_SIM_IDPS, headings = FALSE, method = 'render'), file = "output/tablas/tabla1.html")
webshot("output/tablas/tabla1.html","output/tablas/tabla1.png")

#Graficar los predictores y su relacion con variable independiente

gclima <- ggscatter(proc_SIM_IDPS, x = "ind_clima", y = "GSE",
                  shape = 21, size = 3, # Forma y tamaño de puntos
                  add = "reg.line", #Agregar recta de regresion
                  cor.coef = TRUE)# Agregar coeficiente correlacion


gautoestima <- ggscatter(proc_SIM_IDPS, x = "ind_autoestima", y = "GSE",
                    shape = 21, size = 3, # Forma y tamaño de puntos
                    add = "reg.line", #Agregar recta de regresion
                    cor.coef = TRUE)# Agregar coeficiente correlacion


glectura <- ggscatter(proc_SIM_IDPS, x = "prom_lectura", y = "GSE",
                    shape = 21, size = 3, # Forma y tamaño de puntos
                    add = "reg.line", #Agregar recta de regresion
                    cor.coef = TRUE)# Agregar coeficiente correlacion


gmatematica <- ggscatter(proc_SIM_IDPS, x = "prom_matematica", y = "GSE",
                    shape = 21, size = 3, # Forma y tamaño de puntos
                    add = "reg.line", #Agregar recta de regresion
                    cor.coef = TRUE)# Agregar coeficiente correlacion

gcsociales <- ggscatter(proc_SIM_IDPS, x = "prom_csociales", y = "GSE",
                        shape = 21, size = 3, # Forma y tamaño de puntos
                        add = "reg.line", #Agregar recta de regresion
                        cor.coef = TRUE)# Agregar coeficiente correlacion

#Unir graficos
grid.arrange(gclima, gautoestima, glectura, gmatematica, gcsociales, nrow = 1)


#-------------- MODELO DE REGRESION MULTIPLE --------------

#Regresiones simples
clima_models<-lm(GSE ~ ind_clima, data=proc_SIM_IDPS)
autoestima_models<-lm(GSE ~ ind_autoestima, data=proc_SIM_IDPS)
lectura_models<-lm(GSE ~ prom_lectura, data=proc_SIM_IDPS)
matematica_models<-lm(GSE ~ prom_matematica, data=proc_SIM_IDPS)
csociales_models<-lm(GSE ~ prom_csociales, data=proc_SIM_IDPS)

#Tabla simple de regresion multiple
modelom <- lm(GSE ~ ind_clima + ind_autoestima + prom_lectura + prom_matematica + prom_csociales, data=proc_SIM_IDPS)

#Tabla publicable de regresion multiple 
tab_model(list(modelom), show.ci=FALSE,
                  p.style = "asterisk", dv.labels = c("Modelo1"),
                  string.pred = "Predictores", string.est = "β")



