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
  geom_point(alpha = 0.8)+
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
  labs(title = "Clusters de Desempeño y de Satisfaccion",
       x= "Nivel de Desempeño",
       y= "Nivel de Satisfacción",
       color = "Cluster")


# Datos para graficar correlaciones negativas y positivas

p<- c(25,22,20,16,12,8,5,3,18,23,13,9,23,8,2)
a<- c(3,5,8,12,16,20,22,25,7,6,15,18,7,19,23)
dfn <- data.frame(a, p)
r <- c(1,2,3,4,5,6,7,8,9,10)
s <- c(1,2,3,4,5,6,7,8,9,10)
dfp <- data.frame(r, s)

# Correlación negativa
ggplot(dfn, aes(p, a))+ ggtitle("Correlación Negativa")+ 
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)

# Correlación positiva
ggplot(dfp, aes(s, r))+
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)+
  ggtitle("Correlación Positiva")


#### Regresión Lineal Simple ####
library(googlesheets4)
library(gargle)
library(scales)

gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos

# Cargar este archivo lleva un tiempito... relax
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

# Preprocesamiento
analisis <- encuesta_sysarmy %>%
  select('Trabajo de', `Salario mensual BRUTO (en tu moneda local)`, 'Años de experiencia') %>%
  rename(Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`,
         Experiencia = 'Años de experiencia') %>%
  filter(Puesto == "Developer",
         between(Sueldo_Bruto, 20000, 1000000)) %>%
  mutate(Experiencia = as.numeric(unlist(Experiencia)))


mod_dev <- lm(Sueldo_Bruto ~ Experiencia, data= analisis)

library(broom)

tidy(mod_dev)

# Gráfico de regresión lineal simple
analisis %>%
  filter(Puesto == "Developer") %>%
  ggplot(aes(Experiencia, Sueldo_Bruto))+
  geom_point(alpha = 0.3, size = 2, color="#0794DB")+
  geom_smooth(method = "lm")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(title= "Relación entre sueldo bruto y años de experiencia",
       subtitle = "Developers en Argentina",
       x="Experiencia",
       y= "Sueldo Bruto",
       caption = "Fuente: Encuesta de Sueldos Sysarmy Enero 2020") + 
  theme_bw()


#### Text Mining ####

library(reshape2)

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
  labs(title="Ranking de sentimientos",
       x = "Sentimiento",
       y = "Cantidad de Apariciones")



library(wordcloud2)
library(webshot)
webshot::install_phantomjs()


eho_text_vacio %>%
  filter(Cambios_Futuros == "Sí") %>%
  count(palabra, sort = TRUE) %>%
  ungroup() %>%
  wordcloud2( size = 0.6, shape = "triangle-forward",color = rep_len(c("#4445f8", "#7563fa", "#9881fc", "#b59ffe"), nrow(.)))