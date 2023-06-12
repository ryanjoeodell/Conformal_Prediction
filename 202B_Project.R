library(data.table)
library(fastICA)
path = "C:/Users/ryanj/Downloads/heart.dat"
DT = read.table(path)
setDT(DT)
str(DT)

DT.class = DT$V14
DT[, V14 := NULL]
DT[, V14 := DT.class]

set.seed(100)
out = fastICA(DT, n.comp = 6, tol = 1e-10)

str(out)
cols = ifelse(DT.class == 1 , "steelblue" , "firebrick")
# kinda worked for 5 
plot(out$S, col = cols)
plot( out$X %*% out$K , col = cols)

DT[, V14 := ifelse( V14 == 2 , 0 , 1)]
mod = glm(V14 ~ . , data = DT , family = "binomial")
summary(mod)

path = "C:/Users/ryanj/Downloads/ForestTypes/training.csv"
DT  = fread(path)
str(DT)

col = DT$class
col = as.factor(col)
DT[ , -"class"]
out = fastICA(DT[ , -"class"][,1:9], n.comp = 4, tol = 1e-10)

plot(out$S[,c(1,4)], col = col , pch = 16 )
# source matrix 
plot( (out$X %*% out$K), col = col , pch = 16 )

out.2 = prcomp(DT[,-"class"][,1:9])
biplot(out.2)
library(ggfortify)
autoplot(out.2,
         label = FALSE ,
         loadings.label = TRUE)+
  theme_bw()


# 
path = "C:/Users/ryanj/Downloads/parkinsons.data"
DT  = fread(path)
str(DT)
DT[ , name := NULL]
col = as.factor(DT$status)

out = fastICA(DT[ , -"status"] , n.comp =2 ,  tol = 1e-10)
plot(out$S, col = col , pch = 16 )
# source matrix 
plot( (out$X %*% out$K), col = col , pch = 16 )

out.2 = princomp(DT[,-"status"])
biplot(out.2)
library(ggfortify)
autoplot(out.2,
         label = FALSE ,
         loadings.label = TRUE)+
  theme_bw()


cor = cor(DT[,-"status"])
cor = reshape2::melt(cor)
ggplot(cor , aes(x = Var1, y = Var2 , fill = value))+
  geom_tile( color = "white")


path = "C:/Users/ryanj/Downloads/dataR2.csv"
DT  = fread(path)
str(DT)

col = as.factor(DT$Classification)
DT[ , Classification := NULL]

out = fastICA(DT , n.comp =39 ,  tol = 1e-10)
plot(out$S, col = col , pch = 16 )
# source matrix 
plot( (out$X %*% out$K), col = col , pch = 16 )
out.2 = princomp(DT)
biplot(out.2)
library(ggfortify)
autoplot(out.2,
         label = FALSE ,
         loadings.label = TRUE)+
  theme_bw()





