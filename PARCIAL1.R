
# Se tiene una variable x (no necesariamente temperatura) que depende de la elevaci´on. Se sabe que entre
 #los 1000 y 3000 metros, esta variable se ve reducido en 2 unidades cada 500 metros. Entre los 3000 y 4000
# metros, var´ıa en 0.5 unidades, y a una altitud mayor, su valor es constante. Cree una funci´on que permita
#obtener el valor de esta variable, ´unicamente con el dato de la elevaci´on. [2pts]
# * El valor de la variable x a 1000 metros es de 81.4 unidades
 
f <- function(x){
  if (x>=1000 & x<=3000){
    t<-85.4 -(2*(x/500))
  }
  if(x>3000 & x<=4000){
    t<-85.4 -(0.5*(x/500))
  }
  if(x>4000){
    t<-85.4
  }
  return(t)
}

f(1000)

     
     
# 2. Resolver el siguiente sistema de ecuaciones.
(matriz<- matrix(c(3,2,-2,2,-1,3,1,4,2), nrow = 3 , byrow = T))
(sol_matriz <- c(0,9,-4))
(solve(matriz,sol_matriz))
(respuestas <- solve(matriz,sol_matriz))
(names(respuestas) <- c("x","y","z"))
respuestas

# 1. A partir del siguiente conjunto de datos, se describen las variables:
# uh name → Nombre de cuenca
# bh esc → Escenario (observado y modelos clim´aticos)
# bh month → Meses
# bh pc → Precipitaci´on
# bh er → Evapotranspiraci´on Real
# bh rh → Rendimiento H´ıdrico
# bh qd → Caudal


# Se solicita lo siguiente:
# (a) Calcular la precipitaci´on acumulada anual (Valores observados) para la cuenca asignada. [1pt]

data01 <- read.csv(file = "mods_clima_uh.csv")
head(data01)
(cuenca <- as_tibble(data01))
(cuenca_tumbes_obs <- cuenca %>% 
  subset( uh_name == "Cuenca Tumbes" & bh_esc == "Observado" ) %>% 
  group_by(uh_name) %>% 
  summarize( pp_acumulada = mean(bh_pc)))

(cuenca_tumbes_ACCESS <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "ACCESS 1.0" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_acumulada = mean(bh_pc)))

(cuenca_tumbes_MPI <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "MPI-ESM-LR" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_acumulada = mean(bh_pc)))

(cuenca_tumbes_HadGEM2 <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "HadGEM2-ES" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_acumulada = mean(bh_pc)))

# (b) Calcular el porcentaje de sesgo (%, PBIAS) de los escenarios clim´aticos (ACCESS, HADGEM2, MPI)
#respecto a los datos observados para cada mes (enero - diciembre) de cada variable, para la cuenca
# asignada. [3pts]

(v_cuenca_tumbes_ACCESS <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "ACCESS 1.0" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_variacion = sd(bh_pc)))

(v_cuenca_tumbes_MPI <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "MPI-ESM-LR" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_variacion = sd(bh_pc)))

(v_cuenca_tumbes_HadGEM2 <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "HadGEM2-ES" ) %>% 
    group_by(uh_name) %>% 
    summarize( pp_variacion = sd(bh_pc)))


#(c) De la pregunta anterior, ¿Cu´al es el escenario clim´atico m´as preciso? Fundamente su respuesta. [1pt]

desv <- data.frame(v_cuenca_tumbes_ACCESS , v_cuenca_tumbes_MPI , v_cuenca_tumbes_HadGEM2 )
desv 

# (d) Graficar, con ggplot2, la precipitaci´on (enero a diciembre) observada y modelos clim´aticos. [1pt]
(ggplot_cuenca_tumbes_obs <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "Observado")) %>% 
    mutate( meses = bh_month <- month.abb )

(ggplot_tumbes_ACCESS <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "ACCESS 1.0" )) %>% 
    mutate( meses = bh_month <- month.abb )

(ggplot_tumbes_MPI <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "MPI-ESM-LR" )) %>% 
    mutate( meses = bh_month <- month.abb )

(ggplot_tumbes_HadGEM2 <- cuenca %>% 
    subset( uh_name == "Cuenca Tumbes" & bh_esc == "HadGEM2-ES" )) %>% 
    mutate( meses = bh_month <- month.abb )
  
