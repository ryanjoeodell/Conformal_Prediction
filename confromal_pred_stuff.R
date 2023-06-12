library(data.table)   # data processing 
library(xgboost)     # xgboost algorithm
library(glmnet)
library(conformalInference)
library(ggplot2)
library(reshape2)
##########################################################
##########################################################
##########################################################
#
#           Build the Data set 
#
##########################################################
##########################################################
##########################################################
path = "C:/Users/ryanj/Documents/UCLA/STATS_202B/Project/simulation_stats.csv"
stats = fread(path)
# read in the data set 
path = "C:/Users/RyanODell/Downloads/superconduct/train.csv"

train = fread(path)

# independent variables
cols = names(train)[!names(train) == "critical_temp"]



# visualization
cor.mat = round(cor(train[,..cols]),2)
# head(cor.mat)

cor.mat.melt = reshape2::melt(cor.mat)
ggplot(cor.mat.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme_minimal()+
  scale_fill_viridis_c(option = "inferno", limit = c(-1,1))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# fish the correlation matrix 

which(cor.mat.melt$value < -0.75 )

cor.mat.melt[915,]

# this is a good one 
ggplot(train, aes(x = gmean_atomic_radius,
                  y = mean_fie))+
  geom_point(alpha = 0.75)+
  theme_minimal()


# try a basic LM
set.seed(100)
train.ind = sample(nrow(train), (2/3)*nrow(train) )
test = train[-train.ind,]
train = train[train.ind,]

basic_lm = lm( critical_temp ~ .,
               data = train )

summary(basic_lm)
plot(basic_lm)
# residual and qq norm is bad 

test_pred = predict(basic_lm , test , interval = "prediction")

head(test_pred)

RMSE = function(x,y){
  sqrt( mean( (x-y)^2 ))
}

RMSE(test_pred[,1] , test$critical_temp)
# 17.61584
check_interval = function(lower, upper , actual){
  mean( (lower <= actual) & (actual <= upper) )
}
# 95% interval gives  0.9461061 coverage 
check_interval(test_pred[,2], test_pred[,3], test$critical_temp)

# look at length 
check_avg_length = function(lower,upper){
 mean( abs( upper - lower) )
}
check_avg_length(test_pred[,2], test_pred[,3])
# 69.17831


# GLMNET 
system.time(
  {mod = cv.glmnet(x = as.matrix( train[,..cols]),
                   y =train$critical_temp,
                   nfolds = 10 )
  }
)

# conformal pred stuff

library(glmnet)

str(train)

system.time(
  {mod = cv.glmnet(x = as.matrix( train[,..cols]),
                  y =train$critical_temp,
                  alpha = 0 ,
                  nfolds = 10 )
  }
  )

coef.glmnet(mod , s = mod$lambda.min)
funs = lasso.funs(lambda = mod$lambda.min)
system.time({lasso.pred.conf = conformal.pred.split(x = as.matrix(train[,..cols]),
                                                    y = train$critical_temp,
                                                    x0 = as.matrix(test[,..cols]),
                                                    alpha = 0.05,
                                                    train.fun = funs$train,
                                                    predict.fun = funs$predict )})


str(lasso.pred.conf)
# 95% interval gives  0.9461061 coverage 
check_interval(lasso.pred.conf$lo,
               lasso.pred.conf$up,
               test$critical_temp)
#  0.9503386

# look at length 
check_avg_length(lasso.pred.conf$lo,
                 lasso.pred.conf$up)



# PCA stuff


# XGBOOST STUFF
train.mat = xgb.DMatrix(data = data.matrix(train[, ..cols ]),
                        label = train[["critical_temp"]])

test.mat = xgb.DMatrix(data = data.matrix(test[, ..cols ]),
                       label = test[["critical_temp"]])

params <- list( objective = 'reg:squarederror',
                booster = 'gbtree',
                nthread = 12,
                min_child_weight = 10,
                max_depth = 12, 
                learning_rate = 0.35,
                colsample_bytree = 0.75  ,
                subsample =0.50,
                max_leaves = 100)

model2 = xgb.train(params = params,
                   data = train.mat , 
                   nrounds = 750,
                   watchlist = list(test = test.mat ))

xgboost_train = function(x , y , out = NULL){
  train.mat = xgb.DMatrix(data = data.matrix(x),
                          label = y )
  return(xgb.train(params = params,
                   data  = train.mat , 
                   nrounds = 750))
}

xgboost_pred = function(out , newx){
  return(predict(out , newx))
}

# try conformal inference 
xgb.pred.conf = conformal.pred.split(x = data.matrix(train[,..cols]),
                                       y = train$critical_temp,
                                       x0 = data.matrix(test[,..cols]),
                                       alpha = 0.05,
                                       train.fun = xgboost_train ,
                                       predict.fun = xgboost_pred )



check_interval(xgb.pred.conf$lo,
               xgb.pred.conf$up,
               test$critical_temp)
# 0.9517494


# look at length 
check_avg_length(xgb.pred.conf$lo,
                 xgb.pred.conf$up)
# 
54.0881


# TO DO 
# simulation study to see if 95% of the prediction intervals 
# are valid in repeated sampling over the data 
# since they have a nice frequentist intrepretation
# try any other methods? meh. keep it simple 
# read over paper published and referenced last quarter 

pca_X = prcomp(train[,..cols] ,  scale. = TRUE )
summary(pca_X)

str(pca_X)

pca_var = pca_X$sdev^(2) / sum(pca_X$sdev^(2) )


pca_dt = data.frame(PC = 1:81,
                    var = pca_var)

# scree plot 
ggplot(pca_dt , aes(x = PC , y = var ))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ylab("Variance Explained")+
  xlab("Principle Component")+
  ggtitle("% Variance Explained per PC")+
  theme(plot.title = element_text(hjust = 0.5))

# visualize factor loading matrix 
pca_loading_melt = reshape2::melt(pca_X$rotation)
str(pca_loading_melt)

ggplot(pca_loading_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme_minimal()+
  scale_fill_viridis_c(option = "inferno")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())



##########################################################
##########################################################
##########################################################
#
#
#
##########################################################
##########################################################
##########################################################
#
#
#
##########################################################
##########################################################
##########################################################
#
#           OLD DEVELOPMENT STUFF  BELOW
#
##########################################################
##########################################################
##########################################################
#
#
#
##########################################################
##########################################################
##########################################################
#
#
#
##########################################################
##########################################################
##########################################################


str(mod)
plot(mod)
mod = cv.glmnet(x = train$critical_temp,
          y = train[,..cols],
          nfolds = 10 )
mod$lambda.min
library(devtools)
devtools::install_github(repo="ryantibs/conformal",
                         subdir="conformalInference")
library(conformalInference)
funs = lasso.funs(lambda = mod$lambda.min)

set.seed(100)
train.ind = sample(nrow(train), (2/3)*nrow(train) )
test = train[-train.ind,]
train = train[train.ind,]

# get conformal predictions for test 

lasso.pred.conf = conformal.pred(x = as.matrix(train[,..cols]),
                                 y = train$critical_temp,
                                 x0 = as.matrix(test[1:2,..cols]),
                                 alpha = 0.05,
                                 train.fun = funs$train,
                                 predict.fun = funs$predict )
as.numeric(lasso.pred.conf$pred)
lasso.pred.conf$lo
lasso.pred.conf$up

str(lasso.pred.conf)
test$critical_temp[1]

system.time({lasso.pred.conf = conformal.pred.split(x = as.matrix(train[,..cols]),
                                              y = train$critical_temp,
                                              x0 = as.matrix(test[1:2,..cols]),
                                              alpha = 0.05,
                                              train.fun = funs$train,
                                              predict.fun = funs$predict )})

str(lasso.pred.conf)


conformal.pred.wrapper = function(x0){
  
  if(is.null(dim(x0))){
    N = 1
  }else{
    N = nrow(x0)
  }
  #print("first")
  #cat(3, file = "C:/Users/RyanODell/Documents/test.txt")
  lasso.pred.conf = conformal.pred.split(
                                   x = train.mat ,
                                   y = train$critical_temp,
                                   x0 = x0,
                                   alpha = 0.05,
                                   train.fun = funs$train,
                                   predict.fun = funs$predict )
  #print("done")
  #cat(lasso.pred.conf$lo,
  #    file = "C:/Users/RyanODell/Documents/test.txt")
  output = matrix(0, N,3)
  output[1:N,1] = as.numeric(lasso.pred.conf$lo)
  output[1:N,2] = as.numeric(lasso.pred.conf$pred)
  output[1:N,3] = as.numeric(lasso.pred.conf$up) 
  
  #cat(1, file = "C:/Users/RyanODell/Documents/test.txt")
  
  return(output)
  
}


N.cores           = parallel::detectCores()
cl                = snow::makeCluster(N.cores)

test.mat = as.matrix(test[,..cols])
train.mat = as.matrix(train[,..cols])
snow::clusterExport(cl , c("funs",
                           "train.mat",
                           "train",
                           "test.mat",
                           "cols",
                           "conformal.pred.split",
                           "glmnet"))
# parallel apply the results across the columns  of the initial value matrix 
system.time({
Results = snow::parApply( cl, 
                          test.mat,
                          MARGIN  = 1,
                          FUN     = conformal.pred.wrapper)
})
str(Results)
Results = t(Results)
closeAllConnections()

test.vec = test$critical_temp

cbind(Results,  (Results[,1] <= test.vec) & (test.vec <= Results[,3]) )
mean( (Results[,1] <= test.vec) & (test.vec <= Results[,3]) )
sqrt( mean( (Results[,2] - test.vec)**2 ) )
# heat map of correlations

library(ggplot2)
library(reshape2)
cor.mat = round(cor(train[,..cols]),2)
head(cor.mat)

cor.mat.melt = reshape2::melt(cor.mat)
ggplot(cor.mat.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis_c(option = "inferno")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


plot(train.mat[,6], train.mat[,7])

X = princomp(train.mat)
Y = predict(X, test.mat)
rot = X$loadings
plot(X$scores[,1],X$scores[,3])
dt = data.frame(x = X$scores[,1],y= X$scores[,3],z= train$critical_temp)
ggplot(dt , aes(x =x , y= y , col = z ))+
  geom_point(alpha = 0.75)+
  scale_color_viridis_c()

plot(cumsum(X$sdev)/sum(X$sdev), type = "l")

X = as.data.frame(X$scores[,1:23])
X$critical_temp = train$critical_temp
Y = as.data.frame(Y[, 1:23])
Y$critical_temp = test$critical_temp
str(X)

lm_mod = lm(critical_temp ~ . , data = X)
summary(lm_mod)
plot(lm_mod)
summary(lm_mod)

sqrt( mean( lm_mod$residuals**2 ) )
preds = predict(lm_mod , Y)

sqrt(mean( (preds - Y$critical_temp)**2 ))
plot(preds , Y$critical_temp)

library(xgboost)
train.mat = xgb.DMatrix(data = data.matrix(train[, ..cols ]),
                        label = train[["critical_temp"]])

test.mat = xgb.DMatrix(data = data.matrix(test[, ..cols ]),
                       label = test[["critical_temp"]])

params <- list( objective = 'reg:squarederror',
                booster = 'gbtree',
                nthread = 12,
                min_child_weight = 10,
                max_depth = 12, 
                learning_rate = 0.35,
                colsample_bytree = 0.75  ,
                subsample =0.50,
                max_leaves = 100)

model2 = xgb.train(params = params,
                   data = train.mat , 
                   nrounds = 750,
                   watchlist = list(test = test.mat ))

# try conformal inference 
lasso.pred.conf = conformal.pred.split(x = data.matrix(train[,..cols]),
                                 y = train$critical_temp,
                                 x0 = data.matrix(test[,..cols]),
                                 alpha = 0.05,
                                 train.fun = xgboost_train ,
                                 predict.fun = xgboost_pred )

str(lasso.pred.conf)

sqrt(mean( (lasso.pred.conf$pred - test$critical_temp)^2 ))
mean(  lasso.pred.conf$lo <=test$critical_temp  & test$critical_temp <= lasso.pred.conf$up  )
plot( lasso.pred.conf$pred , test$critical_temp)

xgboost_train = function(x , y , out = NULL){
  train.mat = xgb.DMatrix(data = data.matrix(x),
                          label = y )
  return(xgb.train(params = params,
                   data  = train.mat , 
                   nrounds = 750))
}

xgboost_pred = function(out , newx){
  return(predict(out , newx))
}

xgboost_pred(model2, test.mat)


