# Carregando pacotes
library('ggplot2')
library('caret')
library('caTools')
library('tsensembler')
library('leaflet')
library('installr')
library('GGally')
library('lmtest')
library('nlme')
library('dplyr')
library('car')




#Carregando base de dados
data = read.csv('housing.csv',sep=";")

summary(dados)

#Criando nova base de dados
dados <- select(data, price, area, price_by_sqm,
                condominium_fee, iptu, rooms, bathrooms, 
                garage, latitude, longitude)


#Preço do condomínio em número
dados$condominium_fee <- as.numeric(
  gsub(".", "", 
       gsub("R$", "", 
            as.character(dados$condominium_fee), 
            fixed = T),
       fixed = T ))

#Preço do condomínio em número
dados$iptu <- as.numeric(
  gsub(".", "", 
       gsub("R$", "", 
            as.character(dados$iptu), 
            fixed = T),
       fixed = T ))

#Retirando que não tem latitude ou longitude
dados <- dados[!(dados$latitude==""),]

#Arrumando as latitudes e longitudes
dados$longitude <- gsub(".", "", dados$longitude, fixed = T)
dados$longitude <- substr(dados$longitude,1,5)
dados$longitude <- as.numeric(dados$longitude)

dados$latitude <- gsub(".", "", dados$latitude, fixed = T)
dados$latitude <- substr(dados$latitude,1,5)
dados$latitude <- as.numeric(dados$latitude)


dados <- dados[!(is.na(dados$rooms)),]
dados <- dados[!(is.na(dados$iptu)),]
dados <- dados[!(is.na(dados$condominium_fee)),]
dados <- dados[!(is.na(dados$garage)),]
dados <- dados[!(is.na(dados$bathrooms)),]
dados <- dados[!is.na(dados$area),]





#Plots
ggpairs(data=data, 
        columns=c("price", "rooms", "garage", "bathrooms", "area"), # columns to plot, default to all.
        title="tips data", 
        colour = "sex")

mapa <- leaflet() %>% 
  addTiles() %>%  
  addMarkers(lng=-46.668812, lat=-23.598892, popup="Localização imóvel")
mapa

ggplot(data=dados, mapping= aes(x=rooms), na.rm = TRUE) + geom_bar(width=0.5,fill = "cyan3", color='black')
ggplot(data=dados, mapping= aes(x=garage), na.rm = TRUE) + geom_bar(width=0.8,fill = "skyblue", color='black')
ggplot(data=dados, mapping= aes(x=bathrooms), na.rm = TRUE) + geom_bar(width=1,fill = "skyblue", color='black' ) 
ggplot(data=dados, aes(x=latitude, y=longitude)) + geom_point(colour="green")   + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank())

qplot(dados$bathrooms,dados$price,colour=dados$bathrooms, xlab ="Banheiros", ylab="Preço($)")
qplot(dados$area,dados$price,colour=dados$area, xlab ="Área", ylab="Preço($)")
qplot(dados$garage,dados$price,colour=dados$garage, xlab ="Garagem", ylab="Preço($)")
qplot(dados$rooms,dados$price,colour=dados$rooms, xlab ="Quartos", ylab="Preço($)")
qplot(dados$longitude,dados$price, xlab ="Longitude", ylab="Preço($)", color="red")





#Coloca algumas coisas na escala log
dados$price <- log(dados$price)
#dados$area <- log(dados$area)
#dados$iptu <- log(dados$iptu)
#dados$condominium_fee <- log(dados$condominium_fee)
#dados$price_by_sqm <- log(dados$price_by_sqm)


dados$bathrooms=as.factor(dados$bathrooms)
dados$rooms=as.factor(dados$rooms)
dados$garage=as.factor(dados$garage)
dados$latitude=as.factor(dados$latitude)
dados$longitude=as.factor(dados$longitude)

#Tem uma casa que tem 54 banheiros. Vou tirar essa observação
dados <- dados[!(dados$bathrooms==54),]

#Seed e Split
set.seed(20)

divisao = sample.split(dados$area, SplitRatio = 0.80)
data_train = subset(dados, divisao == TRUE)
data_test = subset(dados, divisao == FALSE)




#  MODELO LOG LIN
modelo <- lm(price ~ (area + garage + rooms + bathrooms ) , 
             data = data_train)


#Fazendo previsões
data_train$pred <- predict(modelo)
data_test$pred <- predict(modelo,newdata=data_test)

# Dando uma olhada na raíz do erro quadrado médio e no desvio padrão dos preços
(rmse_train <- RMSE(data_train$pred, data_train$price))
(rmse_test <- RMSE(data_test$pred, data_test$price))
sd(dados$price)
(rsq_train <- r_squared(data_train$pred, data_train$price))
(rsq_test <- r_squared(data_test$pred, data_test$price))

summary(modelo)
plot(modelo)
shapiro.test(residuals(modelo))
bptest(modelo)
dwtest(modelo)



# Plotando as previsões e os preços da base de teste
ggplot(data_test, aes(x = pred, y = price)) + 
  geom_point() + 
  geom_abline() +
  xlab("Previsão")+ ylab ("Log Preço Real")



#Modelo inicial
modelo2 <- gls(price ~ (area + garage + rooms + bathrooms ) , 
             data = data_train)



summary(modelo2)
plot(modelo2)



#Fazendo previsões 2
data_train$pred2 <- predict(modelo2)
data_test$pred2 <- predict(modelo2,newdata=data_test)


# Dando uma olhada na raíz do erro quadrado médio e no desvio padrão dos preços
(rmse_train <- RMSE(data_train$pred2, data_train$price))
(rmse_test <- RMSE(data_test$pred2, data_test$price))
sd(dados$price)

(rsq_train <- r_squared(data_train$pred2, data_train$price))
(rsq_test <- r_squared(data_test$pred2, data_test$price))


# Plotando as previsões e os preços da base de teste
ggplot(data_test, aes(x = pred2, y = price)) + 
  geom_point() + 
  geom_abline() +
  xlab("Previsão")+ ylab ("Log Preço Real")




#############################################################


#Carregando base de dados
data = read.csv('housing.csv',sep=";")

summary(dados)

#Criando nova base de dados
dados <- select(data, price, area, price_by_sqm,
                condominium_fee, iptu, rooms, bathrooms, 
                garage, latitude, longitude)


#Preço do condomínio em número
dados$condominium_fee <- as.numeric(
  gsub(".", "", 
       gsub("R$", "", 
            as.character(dados$condominium_fee), 
            fixed = T),
       fixed = T ))

#Preço do condomínio em número
dados$iptu <- as.numeric(
  gsub(".", "", 
       gsub("R$", "", 
            as.character(dados$iptu), 
            fixed = T),
       fixed = T ))

#Retirando que não tem latitude ou longitude
dados <- dados[!(dados$latitude==""),]

#Arrumando as latitudes e longitudes
dados$longitude <- gsub(".", "", dados$longitude, fixed = T)
dados$longitude <- substr(dados$longitude,1,5)
dados$longitude <- as.numeric(dados$longitude)

dados$latitude <- gsub(".", "", dados$latitude, fixed = T)
dados$latitude <- substr(dados$latitude,1,5)
dados$latitude <- as.numeric(dados$latitude)


dados <- dados[!(is.na(dados$rooms)),]
dados <- dados[!(is.na(dados$iptu)),]
dados <- dados[!(is.na(dados$condominium_fee)),]
dados <- dados[!(is.na(dados$garage)),]
dados <- dados[!(is.na(dados$bathrooms)),]
dados <- dados[!is.na(dados$area),]


#Coloca algumas coisas na escala log
dados$price <- log(dados$price)
dados$area <- log(dados$area)
dados$iptu <- log(dados$iptu)
dados$condominium_fee <- log(dados$condominium_fee)
dados$price_by_sqm <- log(dados$price_by_sqm)






dados <- dados[!(dados$bathrooms==54),]

#Seed e Split
set.seed(20)

divisao = sample.split(dados$area, SplitRatio = 0.80)
data_train = subset(dados, divisao == TRUE)
data_test = subset(dados, divisao == FALSE)



modelo=lm(data=data_train, formula=price~ -area + rooms + garage 
          +price_by_sqm + bathrooms + iptu + condominium_fee)
summary(modelo)



#Fazendo previsões
data_train$pred <- predict(modelo)
data_test$pred <- predict(modelo,newdata=data_test)

# Dando uma olhada na raíz do erro quadrado médio e no desvio padrão dos preços
(rmse_train <- RMSE(data_train$pred, data_train$price))
(rmse_test <- RMSE(data_test$pred, data_test$price))
sd(dados$price)





##################################################
## Gradiente Descendente



#Seed e Split
set.seed(20)

divisao = sample.split(dados$area, SplitRatio = 0.80)
data_train = subset(dados, divisao == TRUE)
data_test = subset(dados, divisao == FALSE)








GradD <- function(x, y, alpha = 0.006, epsilon = 10^-10){
  iter <- 0
  i <- 0
  x <- cbind(rep(1,nrow(x)), x)
  theta <- matrix(c(1,1),ncol(x),1)
  cost <- (1/(2*nrow(x))) * t(x %*% theta - y) %*% (x %*% theta - y)
  delta <- 1
  while(delta > epsilon){
    i <- i + 1
    theta <- theta - (alpha / nrow(x)) * (t(x) %*% (x %*% theta - y))
    cval <- (1/(2*nrow(x))) * t(x %*% theta - y) %*% (x %*% theta - y)
    cost <- append(cost, cval)
    delta <- abs(cost[i+1] - cost[i])
    if((cost[i+1] - cost[i]) > 0){
      print("The cost is increasing.  Try reducing alpha.")
      return()
    }
    iter <- append(iter, i)
  }
  print(sprintf("Completed in %i iterations.", i))
  return(theta)
}

TPredict <- function(theta, x){
  x <- cbind(rep(1,nrow(x)), x)
  return(x %*% theta)
}



x <- as.matrix(data_train[,c(2:8)])
y <- as.matrix(data_train[,1])
stheta <- GradD(scale(x), y, alpha = 0.006, epsilon = 10^-10)
stheta
# RMSE Gradiente Descendente
ypred <- TPredict(stheta, scale(x))
print("RMSE Gradiente Descendente")
sqrt(mean((y - ypred)^2))









