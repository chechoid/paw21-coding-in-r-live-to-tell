#### Análisis Predictivos Atrition ####


library(tidyverse) # Limpiar y manipular datos
library(caret) # Paquete para hacer análisis predictivos
library(rpart)
library(pROC)



# Cargo los datos desde una página web
datos_rh <- read_csv("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv")

# Elminamos la variable 'sales' y cambiemos los valores de 'salary' a numéricos.
datos_rh <- datos_rh %>% 
  select(-sales) %>%
  mutate(salary = as.numeric(case_when(
    salary == 'low' ~ 0,
    salary == 'medium' ~ 1,
    salary == 'high' ~ 2
  )))


# Defino una semilla para poder replicar los resultados
set.seed(234)

# Parto el índice para dividir el dataset en training y test
modelo_hr <- createDataPartition(y = datos_rh$left, p = 0.7,
                                 list = FALSE)


#Armo el dataframe de training [fila, columna]
modelo_hr_train <- datos_rh[modelo_hr,]

# Con el signo - (menos), creamos el dataset de testing, con todas las filas 'que no estén en modelo_hr'
modelo_hr_test <- datos_rh[-modelo_hr,]


# Calculamos un modelo de entrenamiento
modelo_glm2 <- glm(left ~. , family = "binomial",
                   data = modelo_hr_train)


# Entreno el modelo - Calculo las probabilidades en los datos de entrenamiento
pred_train <- predict(modelo_glm2, newdata = modelo_hr_train, type = "response")


# Luego aplica esos cálculos en el dataset de test
pred_test <- predict(modelo_glm2, newdata = modelo_hr_test, type = "response")


# Asigna las probabilidades a una variable nueva llamada "score".
modelo_hr_test$score <- pred_test


# Luego en base al score, asigno una clase predicha en función a si la probabilidad es mayor a 0.5
modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion = ifelse(score > 0.5, 1, 0))

#### Analizando la calidad del modelo ####

# Creo la matriz de confusión
conf_matrix <- table(modelo_hr_test$prediccion, modelo_hr_test$left)

#### Árbol de Decisión ####

arbol_hr_train <- rpart(left ~., data = modelo_hr_train, method = "class")
arbol_hr_test <- predict(arbol_hr_train, newdata = modelo_hr_test)

#Agrego los resultados del modelo a los datos de test
modelo_hr_test$score_arbol <- arbol_hr_test[,2]
modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion_arbol = ifelse(score_arbol > 0.5, 1, 0))

# Creamos la matriz de confusión, con los valores predichos y los valores reales de la variable target.
conf_matrix_arbol <- table(modelo_hr_test$prediccion_arbol, modelo_hr_test$left)



rocobj1 <- plot.roc(modelo_hr_test$left, modelo_hr_test$score,
                    main="Curva ROC",percent=TRUE, col="#1c61b6")

rocobj2 <- lines.roc(modelo_hr_test$left, modelo_hr_test$score_arbol,
                     percent=TRUE, col="#008600")

testobj <- roc.test(rocobj1, rocobj2)


legend("bottomright", legend=c("Logistics Regression", "Decision Tree"), 
       col=c("#1c61b6", "#008600"), lwd=2)


#### Clustering ####

# Gráfico de Niveles de Satisfacción y de Desempeño por empleados actuales y de baja
ggplot(datos_rh, aes(x = last_evaluation, y = satisfaction_level, color = factor(left)))+
  geom_point(alpha = 0.7, size = 2)+
  scale_color_manual(values = c("#BFC9CA","#2874A6"))+
  labs(title = "Performance and Satisfaction Levels",
       subtitle = "0 = Current Employee, 1 = Left",
       x= "Performance",
       y= "Satisfaction",
       color = "Employee \n de Status")


# Seleccionamos las variables para elegir los clusters
variables_cluster <- modelo_hr_test %>%
  select(last_evaluation, satisfaction_level)

# Preparo los datos para hacer el cálculo
vc <- scale(variables_cluster)

# Corro el algoritmo de clustering k-means  
fit_vc <- kmeans(vc, 3)

# Agrego los clusters ajustados (calculados) al dataset
modelo_hr_test$cluster <- fit_vc$cluster

library(ggthemes)

# Gráfico de clusters
ggplot(modelo_hr_test, aes(x = last_evaluation, y = satisfaction_level, color = factor(cluster)))+
  geom_point(alpha = 0.8)+
  scale_color_colorblind()+
  labs(title = "Employee Clusters by Performance and Satisfaction",
       x= "Performance",
       y= "Satisfaction",
       color = "Cluster") +
  theme_light()



#### Text Mining ####

library(reshape2)
library(googlesheets4)
library(gargle)

EncuestaHomeOffice <- sheets_read("1g2q3c_MMrBc4MehO4Yjktpu2fk7s7M8Bn2wIgV6yQHo")


EncuestaHomeOffice <- EncuestaHomeOffice %>% 
  select("¿Creés que va a cambiar la forma de trabajar después de esta crisis?",
         "Justifica la respuesta")

#### Limpieza de Datos ####

# Cambio los nombres de las variables para hacerlo más manejable
hos <- EncuestaHomeOffice %>%
  rename("Cambios_Futuros" = "¿Creés que va a cambiar la forma de trabajar después de esta crisis?",
         "Comentarios" = "Justifica la respuesta")

#### Text Mining ####
# Fuente: http://www.aic.uva.es/cuentapalabras/palabras-vacias.html

library(tidytext)
library(wordcloud2)

zy <- theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(colour = "#F4F6F6"),
            axis.line = element_line(colour = "grey"))

zx <- theme(panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = "#F4F6F6"),
            axis.line = element_line(colour = "grey"))


eho_text <- hos %>%
  select(Cambios_Futuros, Comentarios) %>%
  filter(!is.na(Comentarios)) %>%
  mutate(Comentarios = as.character(Comentarios))

eho_text_pal <- eho_text %>%
  unnest_tokens(palabra, Comentarios)


# Un lexicon más exhaustivo y detallado
vacias <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                   locale = default_locale())


# Hacer un anti_join para eliminar las palabras del corpus que están en el listado del lexicon
eho_text_vacio <- eho_text_pal %>%
  anti_join(vacias)


# Si quiero armar un listado específico de palabras para eliminar del análisis, luego uso un anti_join
vacias_adhoc <- tibble(palabra = c("trabajo", "home", "office", "van", "va"))

# Hay varias palabras que se repiten y que no aportan mucho valor así que las elimino.
eho_text_vacio <- eho_text_vacio %>%
  anti_join(vacias_adhoc)

# Ordeno los comentarios en base a la variable "Cambios_Futuros"
library(forcats)

eho_text_vacio$Cambios_Futuros <- fct_relevel(eho_text_vacio$Cambios_Futuros, "Sí", "Tal vez", "No")
escala <- c("#4445f8", "#c7beca", "#da8a10" )
ze <- scale_fill_manual(values = escala)


eho_text_vacio %>%
  group_by(Cambios_Futuros) %>%
  count(palabra, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder_within(palabra, n, Cambios_Futuros), y = n, fill = Cambios_Futuros)) +
  scale_fill_manual(values = escala)+
  geom_bar(stat = 'identity', show.legend = FALSE) +
  scale_x_reordered()+
  facet_wrap(~Cambios_Futuros, ncol = 2, scales = "free")+
  labs(x = "", y= "Frecuencia Absoluta")+
  ggtitle("Top 10 de palabras por Respuesta",
          subtitle = "Pregunta: ¿Creés que va a cambiar la forma de trabajar?")+
  coord_flip() +
  zx

###### Análisis de Sentimientos

# Lexicon de sentimientos
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())

# Modificación de la función get_sentiments de tidyverse
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

## Análisis General
eho_text_nrc <- eho_text_vacio %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)


feelings <- c("negativo", "positivo", "negativo", "negativo", "negativo", "positivo", "positivo", "positivo")

eho_text_nrc %>%
  filter(sentimiento != "negativo", sentimiento !="positivo") %>%
  cbind(feelings) %>%
  ggplot(aes(reorder(sentimiento, n), n, fill = feelings)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#F5B041","#5DADE2"))+
  zx +
  coord_flip() +
  labs(title="Sentiment Analysis",
       caption = "Source: Home Office Survey 2020",
       x = "Sentiment",
       y = "Times")



library(wordcloud2)
library(webshot)
webshot::install_phantomjs()


eho_text_vacio %>%
  filter(Cambios_Futuros == "Sí") %>%
  count(palabra, sort = TRUE) %>%
  filter(n >=3) %>% 
  ungroup() %>%
  wordcloud2( size = 0.6, color = rep_len(c("#4445f8", "#7563fa", "#9881fc", "#b59ffe"), nrow(.)))


# Plots --------------------

library(googlesheets4)
library(gargle)
library(lubridate)
library(tidyverse)
library(extrafont)

loadfonts(quiet = T)

expectativas_laborales <- gs4_get("1HeFbgf0aubb5HBSFJTRzGCpW536O1cji7I6lgiNqvqg") %>%
  read_sheet()

exp_lab <- expectativas_laborales%>%
  rename(Expectativa = Período) %>%
  pivot_longer(-Expectativa, names_to = "Periodo", values_to = "Valor") %>%
  mutate(Periodo = dmy(Periodo),
         Trimestre = quarter(Periodo, with_year = TRUE, fiscal_start = 1),
         Expectativa = factor(Expectativa, levels = c("La dotación aumentará",
                                                      "La dotación disminuirá",
                                                      "La dotación se mantendrá"),
                              labels = c("Aumentará", "Disminuirá", "Sin Cambios"))) %>%
  filter(Trimestre > 2013.04) %>%
  group_by(Trimestre) %>%
  summarise(Exp_Aumento = mean(Valor[Expectativa== "Aumentará"]),
            Exp_Disminuye = mean(Valor[Expectativa== "Disminuirá"]),
            Exp_Igual = mean(Valor[Expectativa == "Sin Cambios"]))

exp_empresaria <- exp_lab %>%
  pivot_longer(-Trimestre, names_to = "Expectativa", values_to = "Valor") %>%
  mutate(Expectativa = factor(Expectativa, levels = c("Exp_Aumento",
                                                      "Exp_Disminuye",
                                                      "Exp_Igual"),
                              labels = c("Aumentará", "Disminuirá", "Sin Cambios"))) %>% 
  filter(Expectativa != "Sin Cambios")


ggplot(exp_empresaria, aes(x = Trimestre, y = Valor,  color = Expectativa)) +
  geom_line(size = 1)+
  scale_color_manual(values = c("#2980B9", "#E67E22", "#BDC3C7"))+
  geom_point()+
  geom_smooth() +
  labs(title = "Promedio de Expectativas Empresarias y Puestos Vacantes por trimestre",
       subtitle = "Fuente: Encuesta de Índicadores Laborales",
       caption = "#30diasdegraficos #RStats_ES",
       x = "Trimestre", y = "Valor (porcentaje)") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#D7DBDD"),
        panel.grid.minor.y = element_line(color = "#D7DBDD"),
        panel.background = element_blank(),
        text = element_text(family = "Lucida Sans Typewriter")) +
  scale_y_continuous(limits = c(0,15))+
  geom_vline(aes(xintercept = 2015.4), linetype = 2, alpha = 0.3)+
  geom_vline(aes(xintercept = 2019.4), linetype = 2, alpha = 0.3)


# Sergio 2

library(tidyverse)
library(ggeconodist)
library(ggthemes)
library(extrafont)      # Permite utilizar otras fuentes en los gráficos y salidas
library(scales)


# Datos de ejemplo 

rh <- read_delim("data/rh_ar.csv", delim = ";")


# Preparación 



estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_rect(fill = "#FBFCFC"),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Ubuntu"))



# Compensación vs. Desempeño ---------

rh <- rh %>% 
  filter(puesto %in% c("Analista", "HRBP", "Responsable",
                       "Jefe", "Gerente")) %>% 
  mutate(performance = as.integer(runif(490, min = 1, max = 4)),
         performance = factor(performance,
                              levels = c(1,2,3),
                              labels = c("Bajo", "Regular", "Top")),
         puesto = factor(puesto, 
                         levels = c("Analista", "HRBP", "Responsable",
                                    "Jefe", "Gerente")))


rh %>% 
  ggplot(aes(x = puesto, y = sueldo_ft)) +
  geom_econodist(width = 0.5) +
  geom_point(aes(y = sueldo_bruto, color = performance), size = 2, alpha = 0.3,
             position = position_jitter(width = 0.2)) +
  scale_color_colorblind() +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ";")) +
  coord_flip() +
  labs(title = "Distribución de sueldos por puesto y desempeño",
       x = "", y = "", color = "Desempeño",
       caption = "Datos de desempeño generados aleatoriamente") +
  estilov

# Plot 3
library(hrbrthemes)

hr_data <- read_delim("data/HRDataset_v13.csv", delim = ";")

perf_by_source <- hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% 
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>% 
  arrange(-performance_promedio)

ggplot(perf_by_source, aes(x=performance_promedio, 
                           y = reorder(RecruitmentSource, performance_promedio))) +
  geom_point(color = ft_cols$yellow, size = 2) +
  labs(title="Desempeño promedio por fuente de reclutamiento", # Divide el titulo en dos renglones
       y="",
       x="Desempeño Promedio")+
  theme_ft_rc()+
  theme(plot.title = element_text(hjust = 2))


# Ariadna Angulo Brunet ----------------
# https://github.com/AnguloB/datosdemiercoles/tree/master/00_30diasDeGraficos

# dia 4 facetas
#Datos del SIDC Cat (link  directo en el script )
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts(quiet = T)

font<- "Trebuchet MS" #Fuente que voy a utlizar

library(readr)
Citaciones <- read_csv("https://raw.githubusercontent.com/AnguloB/datosdemiercoles/master/00_30diasDeGraficos/05_arco/Citaciones.csv")

#selecciono solo las variables que me interesan
Citaciones%>%
  select(Authors, Title) ->data

data<-data%>%separate_rows(Authors, sep = ",") #Separo por coma los autores en cada linea
data<-data[seq(1,nrow(data),2) ,] #me quedo solo con los pares
data$Authors<-str_trim(data$Authors) #saco espacios en blanco
data<-data%>%
  group_by(Title) %>% 
  mutate(titleid=factor(group_indices())) #cambio el titulo por un ID

data<-data[,c("titleid","Authors")]
library(stringi)
data$Authors<-stri_trans_general(data$Authors, "Latin-ASCII")


totals<-data%>% #Creo el total de articulos de cada 
  group_by(Authors)%>%
  count()%>%
  arrange(desc(n))
names(totals)<-c("from", "totalreal") 

# transformo los datos de forma que haya la correspondencia entre autores
dta <- full_join(data, data, c('titleid' = 'titleid')) %>% 
  select(-titleid) %>% 
  filter(Authors.x != Authors.y) %>% 
  group_by(Authors.x, Authors.y) %>% 
  summarise(total = n())


names(dta)<- c("from", "to", "total")

dta<-dta%>%
  left_join(totals)%>%
  select(from, to, totalreal)




library(ggraph)



palette30  <- c("grey60","#FFEC33","#33DDFF","#EC4176","#FF8A33","#5564eb")



p1<-ggraph(dta, 'linear') +
  geom_edge_arc(aes(color=factor(totalreal), alpha=factor(totalreal)),  fold=FALSE)+theme_bw()+
  geom_node_point(size=2,alpha=0.5) +
  scale_edge_colour_manual(values=palette30)+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(title= "Arch plot by @AnguloBrunet", 
       fill="", 
       subtitle = "A journey around alpha and omega to estimate internal consistency reliability: \n
       autores y autoras que han citado el articulo y relación entre ellos", 
       y = "", 
       x = "")+
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 


p2<-totals%>%
  group_by(totalreal)%>%
  count()%>%
  ggplot(aes(x=factor(totalreal), y=n, fill=factor(totalreal)))+
  geom_col(aes( alpha=factor(totalreal)))+
  geom_text(aes(label=paste0("N = ",n), hjust=-.25))+
  scale_fill_manual(values=palette30)+
  coord_flip()+theme_bw()+
  theme(text=element_text(family = font, color="#9B77CF"),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text( face="italic", size=10, hjust = 1, color="black"))+
  labs(title ="",
       subtitle="\n \n",
       caption = "Viladrich, Angulo-Brunet y Doval (2017) \n Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: Scopus 15 mayo 2020", 
       y="Autores", 
       x="Nº articulos")+
  scale_y_continuous(position = "right", limits=c(0,180))
library(cowplot)

plot_grid(p1, p2, nrow=1, rel_widths = c(0.8, .2))

# Meme ------------
# load libraries
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(patchwork)

# read in data - manually created
data <-
  read_csv("https://raw.githubusercontent.com/ashten28/my_ggplots/master/guy_checking_out_a_girl_meme/data.csv") %>% 
  mutate(
    paint = fct_relevel(paint, c("black", "brown", "beige", "white", "black_2", "grey_2",  "red", "blue_2", "beige_3", "blue", "beige_2", "grey"))
    
  )

# start ggplot
p1 <- 
  ggplot(data) +
  # using geom_bar, so didnt cheat (though using geom_tile was much easier)
  geom_bar(
    mapping = aes(x = x, fill = paint),
    width = 1
  ) +
  # set colours for bars
  scale_fill_manual(
    values = c("#24211a", "#593326","#e4a095", "#ffffff", "#24211a", "#93a9b6", "#fa0107", "#0b59b3", "#e4a095", "#0b59b3", "#e4a095","#93a9b6")
  ) +
  # make grid squares
  coord_equal() +
  # coord_polar() +
  # add themes
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank()
  )

# customized legend - create data to be plotted to look like a legend (not recommended)
legend_data <-
  data.frame(
    x = 0,
    y = c(4, 6, 8, 10, 12),
    label = c("Making ggplot\nwhen Hadley\nasks", "", "Me", "", "Doing useful\ndata analysis")
  )

# start ggplot for custom legend
p2 <- 
  ggplot(
    data = legend_data,
    mapping = aes(x = x, y = y)
  ) +
  # using geom_tile to create filled boxes
  geom_tile(
    mapping = aes( fill = label),
    width = 0.4, height = 0.4,
  ) + 
  # using geom_text to place text beside tiles
  geom_text(
    mapping = aes(label = label),
    size = 6,
    nudge_x = 0.4,
    nudge_y = 0,
    hjust = 0,
    vjust = 0.5
  ) + 
  scale_fill_manual(
    values = c("#ffffff", "#93a9b6", "#fa0107", "#0b59b3")
  ) +
  # setting scales to look better/ coord_fixed to make plot narrower
  scale_x_continuous(limits = c(-0.5,3)) +
  scale_y_continuous(limits = c(1, 15)) +
  coord_fixed(ratio = 0.8) +
  # add themes
  theme_void() +
  theme(
    legend.position = "none"
  )

# using patchwork to combine main plot and custom legend
p <- p1 + p2
p

# save plot
ggsave(filename = "guy_checking_out_a_girl_meme/plot.png", plot = p, width = 16, height = 9)


# Silly plots ----------
library(ggbernie)

d1 <- rh %>% 
  filter(between(sueldo_bruto,
                 20000,
                 200000),
         puesto %in% c("Analista", "HRBP", "Jefe", "Gerente")) %>% 
  mutate(puesto = factor(puesto, levels = c("Analista", "HRBP", "Jefe", "Gerente"))) %>% 
  group_by(role = puesto) %>% 
  summarise(mean_salary = mean(sueldo_bruto))

ggplot(d1) +
  geom_bernie(aes(x = role, y = mean_salary), bernie = "sitting") +
  labs(title = "Introducing: ggbernie by R-CoderDotCom",
       caption = "GitHub: https://github.com/R-CoderDotCom/ggbernie")

library(metallicaRt)              

fuel <- metalli_palette("fuel")

p1 <- ggplot(d1, aes(x = role, y = mean_salary, fill = role)) +
  geom_col() +
  theme_minimal()
p1

p1 +
  scale_fill_manual(values = fuel) +
  labs(title = "Introducing: metallicaRt package by John MacKintosh",
       caption = "GitHub: https://github.com/johnmackintosh/metallicaRt")

library(gameofthrones)

p1 +
  scale_fill_got(discrete = T, option = "Stark2") +
  labs(title = "Introducing: gameofthrones package by Alejandro Jiménez",
       caption = "GitHub: https://github.com/aljrico/gameofthrones")



# Example analysis ----------
library(tidyverse)
library(noah)

wine_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wine_ar <- wine_ratings %>% 
  filter(province == "Mendoza Province") 

glimpse(wine_ar)


wine_ar %>% 
  summarise(n_winery = unique(winery))



wine_ar %>% 
  filter(variety == "Malbec") %>% 
  group_by(region_1) %>% 
  summarise(avg_points = mean(points)) %>% 
  arrange(-avg_points)

wine_ar %>% 
  filter(variety == "Malbec",
         region_1 == "Perdriel") %>% 
  ggplot(aes(x = points, y = price, color = winery)) +
  geom_point(size = 6, alpha = 0.6, position = "jitter") +
  ggthemes::scale_color_colorblind() +
  theme_minimal()


training_ratings <- read_delim("data/training_ratings.csv", delim = ";")

(training_analysis <- training_ratings %>% 
    mutate(pseudo_supplier = pseudonymize(supplier)) %>% 
    group_by(pseudo_supplier) %>% 
  summarise(alignment = mean(area_goals_alignment))) 
  

ggplot(training_analysis, aes(x = alignment, 
                              y = reorder(pseudo_supplier, alignment))) + 
  geom_col(fill = "#d6dbdf") +
  labs(title = "Average Goal Aligment by Supplier",
       x = "% Alignment",
       y = "Supplier",
       caption = "Data randomly generated") +
  theme_minimal() +
  geom_vline(xintercept = 0.6,
             color = "red",
             linetype = 2)

icons::fontawesome("envelope")
