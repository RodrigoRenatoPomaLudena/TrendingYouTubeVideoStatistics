#install.packages('hrbrthemes')
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("shiny")
#install.packages("DT")
library(mongolite)
library(rjson)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(shiny)
library(DT)

#--------------PARTE 1 -- CONEXION A MONGO DB Y OBTENCION DE DATA ---------------#

#1.1 Coneccion a la base de datos
connection_string = 'mongodb+srv://read-only-user:U5PN6pQ1eirrSu7e@mycluster.zal73.mongodb.net/dbyoutube'

#1.2 Extrayendo la data de cada coleccion tipo CSV de trending
MX_trending = mongo(collection="MX_youtube_trending_data", db="dbyoutube", url=connection_string)
US_trending = mongo(collection="US_youtube_trending_data", db="dbyoutube", url=connection_string)
FR_trending = mongo(collection="FR_youtube_trending_data", db="dbyoutube", url=connection_string)

#1.3 Eplorando las colecciones de la base de datos de YOUTUBE
MX_trending$run('{"listCollections": 1}')$cursor$firstBatch$name

#1.4 Extrayendo la data de cada coleccion tipo JSON de category
MX_category = mongo(collection="MX_category_id", db="dbyoutube", url=connection_string)
US_category = mongo(collection="US_category_id", db="dbyoutube", url=connection_string)
FR_category = mongo(collection="FR_category_id", db="dbyoutube", url=connection_string)

#1.5 Armamos los archivos de trending que son tipo csv
MX <- MX_trending$find('{}')
US <- US_trending$find('{}')
FR <- FR_trending$find('{}')

#1.6 Armamos los archivos de categoria que son tipo json
cat_MX <- MX_category$find('{}')
cat_US <- US_category$find('{}')
cat_FR <- FR_category$find('{}')


#-----------------------#PARTE 2 --- COMPRENSION Y LIMPIEZA DE LOS DATOS-----------------#

#2.1. TRENDING: Preparacion de base de datos estructurada
#2.1.1 Preparamos las tablas de YOUTUBE creando un nuevo campo country: Mexico, USA y Francia
MX$country = MX$country = 'Mexico'
US$country = US$country = 'USA'
FR$country = FR$country = 'Francia'


#2.1.2. Eliminamos las columnas que no vamos a utilizar
MX[, c("thumbnail_link", "description", "title", "channelId")] <- NULL #tags ara eliminar
US[, c("thumbnail_link", "description", "title", "channelId")] <- NULL
FR[, c("thumbnail_link", "description", "title", "channelId")] <- NULL
#se elimina thumbnail_link porque es la direccion url, no agrega valor al analisis
#se elimina description porque es la descripcion del video, contiene datos no estructurados, por este momento no los consideraremos
#se elimina title porque es el nombre del video, no contamos aun con herramientas de analisis de datos no estructurados
#se elimina channel_title porque es el nombre del canal, no contamos aun con herramientas de analisis de datos no estructurados


#2.1.3 procedemos a juntar las 3 bases de datos 
YTB = bind_rows(MX,US,FR)

#2.1.4. procedemos a analizar y seguir limpiando los datos
colSums(is.na(YTB)) #no contamos con datos vacios
str(YTB)            #encontramos datos tipo factor que no estan en su formato, asi como las fechas
summary(YTB)

YTB$trending_date = substr(YTB$trending_date, start = 1, stop = 10)  #se aplica esta funcion para eliminar la hora, ya que observamos que todos los registros tienen hora 00:00:00, la misma que no proporciona valor
YTB$trending_date <- ymd(YTB$trending_date) #convertimos a formato fecha trending_date
YTB$publishedAt  <- ymd_hms(YTB$publishedAt)#convertimos a formato fecha y hora publishedAt

YTB$country <- as.factor(YTB$country);      #convertimos en factor la informacion de pais
levels(YTB$country) <- c("USA","Francia","Mexico") 

anyNA(YTB)          # tenemos una base de datos casi lista para trabajar, la misma que no contiene nulos, no hubo necesidad de eliminar registros

#2.2.CATEGORIAS: Preparamos las tablas de categorias, que estan en formato json 
#2.2.1. Armar la base de datos de categoria, la misma que nos dara la informacion de categoria 
cat_FR$id                                               #encontramos el valor de ID
cat_FR$snippet$title[1]                                 #encontramos el nombre de la categoria
cbind(id=cat_FR$id[1], title=cat_FR$snippet$title[1])   #hacemos una prueba para armar el dataframe

#2.2.2. Funcion para armar la base de datos de categoria 
#Armamos un codigo que nos ayude a hacerlo mas rapido y de forma segura

#Inf de categoria de Mexico
vector1 = c()
vector2 = c()
for (i in 1:length(cat_MX$id)){
  vector1 = c(vector1, cat_MX$id[i])
  vector2 = c(vector2, cat_MX$snippet$title[i])
}
DFX = data.frame("id"=vector1, "title"=vector2)

#Inf de categoria de USA
vector3 = c()
vector4 = c()
for (i in 1:length(cat_US$id)){
  vector3 = c(vector3, cat_US$id[i])
  vector4 = c(vector4, cat_US$snippet$title[i])
}
DFUS = data.frame("id"=vector3, "title"=vector4)

#Inf de categoria de Francia
vector5 = c()
vector6 = c()
for (i in 1:length(cat_FR$id)){
  vector5 = c(vector1, cat_FR$id[i])
  vector6 = c(vector2, cat_FR$snippet$title[i])
}
DFFR = data.frame("id"=vector5, "title"=vector6)

#2.2.3. juntamos las 3 bases de datos 
DFCAT = bind_rows(DFX,DFUS,DFFR)

#2.2.4 Revisamos y limpiamos los datos antes de aplicar la union

DFCATEGORIA = DFCAT[!duplicated(DFCAT), ] #unique(DFCATEGORIA[c("title")]) --- se revisa que no haya datos perdidos

str(DFCATEGORIA) #observamos que ID esta en valor STR
str(YTB)         #observamos que category_id esta como NUM, lo mejor sera realizar su modificacion

YTB$categoryId = as.integer(YTB$categoryId) #realizamos la conversion de num a str 
table(YTB$categoryId)                        #revisamos los datos sobre los que vamos a preparar la union  

colnames(DFCATEGORIA) = c("categoryId","category") #para aplicar el merge, lo mejor es darle renombrar y dar el mismo nombre a las columnas key

#2.3 YOUTUBE: Creacion de la base de datos final para trabajo

#2.3.1. realizamos el merge considerando los valores que estan a la izquierda
YOUTUBE = merge(x = YTB, y = DFCATEGORIA, all.x = TRUE)

#2.3.2. volvemos a revisar y hacer una limpieza final
YTB$category <- as.factor(YTB$category);  #vemos que falto aplicar factor a categoria
levels(YTB$category) <- c("Film & Animation", "Autos & Vehicles",          "Music",
                          "Pets & Animals"  ,  "Sports"         ,          "Travel & Events",
                          "Gaming"          , "People & Blogs"  ,          "Comedy",
                          "Entertainment"   , "News & Politics" ,          "Howto & Style",
                          "Education"       ,     "Science & Technology" , "Nonprofits & Activism",
                          "Movies"          ,      "Shows"  ,              "Trailers"
)


#2.3.3 Vemos la informacion final que tenemos 

summary(YOUTUBE)        #Descripcion de los valores 
anyNA(YOUTUBE)          #sin vacios
colSums(is.na(YOUTUBE)) #sin vacios
head(YOUTUBE)           #Consulta de los registros



#------------------------#PARTE 3 --- ANALISIS DE INFORMACION -------------------------------#


# 3.1 Variables que vamos a necesitar para el analisis posterior

# 3.1.1  Crear un resumen de la campo de categoria por frecuencia
r_cat <- data.frame(table(YOUTUBE$category)) 

# 3.1.1.2 Resumen de los canales que mas se frecuentan
YOUTUBE.ch <- as.data.frame(sort(table(YOUTUBE$channelTitle), decreasing = TRUE))
names(YOUTUBE.ch) <- c("channelTitle", "count")
YOUTUBE.ch.country <- merge(YOUTUBE.ch, unique(YOUTUBE[c('channelTitle', 'country')]),
                            by='channelTitle', all.x=TRUE)

# 3.1.1.3.Correlacion entre likes y Vistas
sp5 <- YOUTUBE %>% 
  filter(category == c("Music")) %>% 
  select(category,view_count,likes)

#-------------------------------------

#--------------------------------NORMALIZACION Puntuacion Z ----------------------------------#
df_YOUTUBE <- YOUTUBE[c("view_count","likes","comment_count")]

#3.1.2. Seleccionamos los campos que vamos a trabajar
library(Fahrmeir)

#puntuacion Z
df_YOUTUBE$viewsz <- scale(x=df_YOUTUBE$view_count)
df_YOUTUBE$likesz <- scale(x=df_YOUTUBE$likes)
df_YOUTUBE$comment_countz <- scale(x=df_YOUTUBE$comment_count)

# Histograma del valor prestado
hist(df_YOUTUBE$viewsz)
hist(df_YOUTUBE$likesz)
hist(df_YOUTUBE$comment_countz)

colSums(is.na(df_YOUTUBE))

#--------------------------------NORMALIZACION Min-Max ----------------------------------#

# Normalizaci?n usando criterio Min-Max
# """""""""""""""""""""""""""""""""""""
minmax <- function(x, vmin, vmax){
  xmin <- min(x)
  xmax <- max(x)
  y <- ((x-xmin)*(vmax-vmin))/(xmax-xmin) + vmin
  return(y)
}

min_val <- 0
max_val <- 1
df_YOUTUBE$viewsmm <- minmax(df_YOUTUBE$view_count, min_val, max_val)
df_YOUTUBE$likesmm <- minmax(df_YOUTUBE$likes, min_val, max_val)
df_YOUTUBE$comment_countmm <- minmax(df_YOUTUBE$comment_count, min_val, max_val)

hist(df_YOUTUBE$viewsmm)
hist(df_YOUTUBE$likesmm)
hist(df_YOUTUBE$comment_countmm)

names(df_YOUTUBE)

#-----------------------------------MAHALANOBIS--------------------------------------#

X = df_YOUTUBE[c(4,5,6)]             #Selecciona las variables normalizadas con puntuacion Z
Y = df_YOUTUBE[c(7,8,9)]          #Selecciona las variables normalizadas con minmax

#Probando con Puntuacion Z
mu <- colMeans(X)                    # Medias de cada columna
S <- cov(X)                          # Matriz de covarianza
dmX <- mahalanobis(X, mu, S)         # Calculo directo dm2

barplot(dmX, main="Mahalanobis")     # Grafico de barras de dist Mahalanobis^2

idx_maxY <- which.max(dmX)           # indice del valor maximo 
dmX[order(dmX, decreasing=TRUE)]      # Valores ordenados


#Probando con Minmax
mu <- colMeans(Y)                    # Medias de cada columna
S <- cov(Y)                          # Matriz de covarianza
dmY <- mahalanobis(Y, mu, S)        # Calculo directo dm2


idx_maxX <- which.max(dmY)           # indice del valor maximo 
boxplot(dmY)

dmY[order(dmY, decreasing=TRUE)]    # Valores ordenados

#-----------------------------------CHI-CUADRADO--------------------------------------#

# Distribucion de Chi-Cuadrado: Punto de Corte con puntuacion MINMAX

p <- 1-0.001 #Realizamos ese calculo porque hemos encontrado que en proporcion son muy pocos los valores atipicos
dof = ncol(Y)
k <- (qchisq(p, dof))

idx_outliers <- which(dmY > k)
idx_outliers
df_YOUTUBE[idx_outliers,]         # Registros con valores at??icos


#----------------------------------- QQ-plot -------------------------------------#


# Distribucion de QQ-plot con MINMAX 

Qx <- qchisq(ppoints(nrow(Y)), dof)
Qy <- dmY


# 3.2. Creacion de UI 
tema <- "pulse"

ui <- navbarPage(
  
  title = "Trending YouTube Video Statistics",
  theme = bslib::bs_theme(bootswatch = tema),
  mainPanel(
    tabsetPanel(
      # 3.2.1. Creacion del campo para el analisis de visualizacion de los Trends 
      tabPanel("Trend Vids", 
               sliderInput("f_trend_vids", "Rango de frecuencia", value=5000, min=0, max=max(r_cat$Freq), step=1000),
               plotOutput("grafico_1", height = 600, width = 900)
      ),
      # 3.2.2. Creacion del campo para el analisis de visualizacion de los canales mas vistos
      tabPanel("Channels", 
               sliderInput("f_trend_ch", "Rango de frecuencia", value=1, min=0, max=max(YOUTUBE.ch.country$count), step=10),
               selectInput(
                 "rb_countries", "Que pais?", 
                 c(unique(YOUTUBE.ch.country$country)), 
                 selected = c(unique(YOUTUBE.ch.country$country)),
                 multiple = TRUE
               ),
               plotOutput("grafico_2", height = 600, width = 900)
      ),
      # 3.2.3. Creacion del campo para el analisis de correlacion entre likes y vistas
      tabPanel("Likes and view_count", 
               selectInput(
                 "rb_categories", "Que categoria?", 
                 YOUTUBE.categories, 
                 selected = 'Music'
               ),
               plotOutput("grafico_3", height = 500, width = 600)
      ),
      navbarMenu("Analisis Univariados",
                 tabPanel("Histogramas de Variables", 
                          plotOutput("histograma1", height = 600, width = 900),
                          plotOutput("histograma2", height = 600, width = 900),
                          plotOutput("histograma3", height = 600, width = 900)
                 ),
                 tabPanel("Mahalanobis", 
                          plotOutput("mahalanobis", height = 600, width = 900)
                 ),
                 tabPanel("QQ-Plot", 
                          plotOutput("qq_plot", height = 600, width = 900)
                 ),
                 tabPanel("Chi Cuadrado", 
                          plotOutput("chi_cuadrado", height = 600, width = 900)
                 )
      )
    )
  )
)


# 3.3. Creacion del servidor 

server <- function(input, output, session) {
  
  # 3.3.1. Grafico de barras horizontales de frecuencia de categoria, interactivo con su frecuencia
  reactive_df_1 <- reactive(as.data.frame(r_cat[r_cat$Freq >= input$f_trend_vids,]))
  
  output$grafico_1<- renderPlot({
    ggplot(
      reactive_df_1(), aes(x=reorder(Var1, -Freq), y=Freq)) +
      geom_segment( aes(x=reorder(Var1, Freq), xend=reorder(Var1, Freq), y=0, yend=Freq), color="skyblue") +
      geom_point( color="green", size=4, alpha=0.6) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      labs(title = "Frequency of Trending Videos", 
           subtitle = "Based on Categories",
           caption = "Source : YOUTUBE Trending Dataset", 
           x = "Category",
           y = "Number of Videos")
  })
  
  # 3.3.2. Grafico de barras verticales de la frecuencia de canales, interactivo con su frecuencia
  reactive_df_2 <- reactive(
    as.data.frame(YOUTUBE.ch.country[(YOUTUBE.ch.country$country %in% input$rb_countries)
                                     & (YOUTUBE.ch.country$count >= input$f_trend_ch),])
  )
  
  output$grafico_2 <- renderPlot({
    ggplot(reactive_df_2()[1:10,], aes(x = channelTitle, y = count, fill = factor(channelTitle))) + geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 45,hjust = 1), legend.position = "none") + scale_x_discrete(name = "channelTitle",label = function(x) str_wrap(x, width = 15)) + 
      scale_y_continuous(name = "Number of videos") + labs(title = "Top Channels with Trending Videos")
  })
  
  # 3.3.3. Grafico de correlacion interactivo en funcion a la categoria
  reactive_category <- reactive(input$rb_categories)
  reactive_df_3 <- reactive({
    sp <- YOUTUBE %>% 
      filter(category == input$rb_categories) %>% 
      select(category,view_count,likes)
    sp
  })
  
  output$grafico_3 <- renderPlot({
    ggplot(data = reactive_df_3(), aes(x = log(view_count), y = log(likes))) +
      geom_point(color="black",
                 fill="#69b3a2",
                 shape=5,
                 alpha=0.5,
                 size=1,
                 stroke = 1) +
      geom_smooth() +
      labs(title = "Correlation Between Likes & view_count", 
           subtitle = paste("Category =", reactive_category()), 
           caption = "Source : YOUTUBE Trending Dataset", 
           x = NULL, 
           y = NULL) +
      theme_ipsum()
  })
  
  # 3.3.4 ANALISIS MULTIVARIADO
  # 3.3.4.1 HISTOGRAMA
  output$histograma1 <- renderPlot({
    hist(df_YOUTUBE$viewsmm)
  })
  output$histograma2 <- renderPlot({
    hist(df_YOUTUBE$likesmm)
  })
  output$histograma3 <- renderPlot({
    hist(df_YOUTUBE$comment_countmm)
  })
  
  # 3.3.4.2 GRAFICO DE BARRAS DE MAHALANOBIS AL 2
  output$mahalanobis <- renderPlot({
    barplot(dmY, main="Mahalanobis")    # Grafico de barras de dist Mahalanobis^2
  })
  
  # 3.3.4.3 GRAFICA DE QQ-PLOT
  output$qq_plot <- renderPlot({
    qqplot(Qx, Qy, 
           col = "steelblue", lwd = 2,
           main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
    abline(0, 1, col="red")
  })
  
  # 3.4.4.4 GRAFICA CHI CUADRADO
  output$chi_cuadrado <- renderPlot({
    plot(sort(dmY), ppoints(nrow(Y)), 
         type="l",
         col="blue",
         xlab="DM al cuadrado ordenada", 
         ylab="Probabilidad Acumulada")
    abline(v = qchisq(p,dof), col = "brown")
  })
  
}

#3.4. Ejecucion de ShinyApp
shinyApp(ui, server)
