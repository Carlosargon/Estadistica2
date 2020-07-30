#           CODIGO DE ANALISIS

#Cargar librerias 
pacman::p_load(dplyr, sjmisc, summarytools, sjPlot, ggplot2, webshot,
               ggpubr, gridExtra, texreg, sjlabelled)
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
grafico1 <- grid.arrange(gclima, gautoestima, glectura, gmatematica, gcsociales, nrow = 1)

#Guardar grafico
dev.copy(png,"output/graficos/Grafico1.png", width = 750, height = 545); dev.off()

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
tab_model(list(clima_models, autoestima_models, lectura_models, matematica_models, csociales_models, modelom), show.ci=FALSE,
                  p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6"),
                  string.pred = "Predictores", string.est = "β",
          file = "output/tablas/Modelo_tab.html")

webshot("output/tablas/Modelo_tab.html", "output/tablas/Modelo_tab.png", vwidth = 750, vheight = 500)

#Grafico regresion multiple
plot_model(modelom, show.values = TRUE)+ theme_sjplot()

#---------------------------------
#MODELOS UNO POR UNO 

Mod1puntaje <- lm(GSE ~ prom_lectura + prom_matematica + prom_csociales, data = proc_SIM_IDPS)
Mod2clima <- lm(GSE ~ ind_clima, data = proc_SIM_IDPS)
Mod3autoestima <- lm(GSE ~ ind_autoestima, data = proc_SIM_IDPS)
Mod4indicadores <- lm(GSE ~ ind_autoestima + ind_clima, data = proc_SIM_IDPS)
Modelo5 <- lm(GSE ~ prom_lectura + prom_matematica + prom_csociales + ind_autoestima + ind_clima, data = proc_SIM_IDPS)


#Tabla publicable de regresion multiple 
tab_model(list(Mod1puntaje, Mod2clima, Mod3autoestima, Mod4indicadores, Modelo5), show.ci=FALSE,
          p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
          string.pred = "Predictores", string.est = "β",
          file = "output/tablas/Modelos.html")

webshot("output/tablas/Modelos.html", "output/tablas/Modelos.png", vwidth = 750, vheight = 500)





#-------------------------
promedio1 <- mean(proc_SIM_IDPS$prom_lectura)
promedio2 <- mean(proc_SIM_IDPS$prom_matematica)
promedio3 <- mean(proc_SIM_IDPS$prom_csociales)
promediototal <- mean(promedio1 + promedio2 + promedio3)
promediototal

proc_SIM_IDPS$promedioSIMCE <- apply(proc_SIM_IDPS[, c(4,5,6)], 1, mean)


#-------------------modelos con puntaje simce promedio--------------------
Model1 <- lm(GSE ~ promedioSIMCE, data = proc_SIM_IDPS)
Model2 <- lm(GSE ~ promedioSIMCE + ind_autoestima + ind_clima, data = proc_SIM_IDPS)

tab_model(list(Model1, Model2), show.ci=FALSE,
          p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2"),
          string.pred = "Predictores", string.est = "β")


#-----------------------+
proc_SIM_IDPS$GSE <- set_labels(proc_SIM_IDPS$GSE,
                             labels=c( "Bajo"=1,
                                       "Medio bajo"=2,
                                       "Medio"=3,
                                       "Medio alto"=4,
                                       "Alto"=5))
#Distribucion variable GSE en tabla
sjmisc::frq(x = proc_SIM_IDPS$GSE,show.na = F)
#Distribucion variable GSE en grafico
plot_frq(data = proc_SIM_IDPS$GSE)
class(proc_SIM_IDPS$GSE)
str(proc_SIM_IDPS$GSE)
#convertir GSE a factor
proc_SIM_IDPS$GSE <- as_factor(proc_SIM_IDPS$GSE)

modelogse <- lm(ind_clima ~ GSE + ind_autoestima + prom_lectura + prom_matematica + prom_csociales, data=proc_SIM_IDPS)

tab_model(list(modelogse), show.ci=FALSE,
          p.style = "stars", dv.labels = c("Modelo 1"),
          string.pred = "Predictores", string.est = "β",
          file = "output/tablas/Modelo_tab2.html")

webshot("output/tablas/Modelo_tab2.html", "output/tablas/Modelo_tab2.png", vwidth = 750, vheight = 500)


