library(readr)
library(dplyr)
library(tidyverse)
library(broom)
library(stringr)
#library(rlist)
library(ggplot2)
library(cregg)
library(data.table)
library(devtools)
library(ashr)
library(zeallot)
library(devtools)
library(ggthemr)
library(Dict)
devtools::install_github('cttobin/ggthemr')
library(glinternet)
library(randomForest)
library(caTools) 

# Read in data

sim_data <- fread(paste0("cjsim_0.5_0.5_1_1_0.75_4000.csv"))

sim_params <- fread(paste0("cjsim_0.5_0.5_1_1_0.75_4000_p.csv"),fill= TRUE)

num_vectors <-c(2,3,3,4,4,4,5,3,5,2,4)
sum(c(2,3,3,4,4,4,5,3,5))

sim_data_t <- sim_data
Xx<-sim_data_t[,c(1,5:15)] %>%  #sim_id + main effects
  group_by(sim_id)

Yy<-sim_data_t[,c(1,3)] %>% #sim_id + response
  group_by(sim_id)

u_id <-unique(sim_data$sim_id)
test_df<- sim_data[sim_id == u_id[1]]

i_num <- sapply(test_df[,c(5:15)], is.numeric)
idx_num <- (1:length(i_num))[i_num]
fit = glinternet.cv(test_df[,c(5:15)], test_df[,response], num_vectors,interactionCandidates=c(10,11)) #cross-validation
coeffs = coef(fit)
coeffs_1 <-coeffs[15]
coeffs_1[[1]]$mainEffects
main_coef<-coeffs_1[[1]]$mainEffectsCoef$cat
main_indices<-coeffs_1[[1]]$mainEffects$cat
coeffs_1[[1]]$interactionsCoef

r<-names(i_num[coeffs_1[[1]]$mainEffects$cat[1]]) #get the names
main_indices[1] #get the first index
main_coef[1]#coef of the first index
num_vectors[main_indices[1]]# levles of that feature

main_indices<-coeffs_1[[1]]$mainEffects$cat
k<-c()
for (i in (0:(num_vectors[main_indices[1]]-1))){
  #print(i)
  k[[length(k)+1]] = paste0(r , as.character(i))
}
#sim_params$as.character(k[1])


##10 fold validation
cv_fit <- glinternet.cv(test_df[,c(5:15)], test_df[,response], num_vectors,interactionCandidates=c(10,11))
plot(cv_fit)

i_1Std <- which(cv_fit$lambdaHat1Std == cv_fit$lambda)
coefs <- coef(cv_fit$glinternetFit)[[i_1Std]]


coefs$mainEffects$cat

#dict_main<- Dict$new(key = coeffs_1[[1]]$mainEffects$cat, value= main_coef)

#Good stuff
#sim_params[,k[[1]],with=F]
#sim_params[,get(k[[1]])]

#oh?




#
#main_indices<-coeffs_1[[1]]$mainEffects$cat
#all da names
#r<-names(i_num[coeffs_1[[1]]$mainEffects$cat[1]]) #get the names
#rr <-names(i_num[coefs$mainEffects$cat])
#main_ind <-coefs$mainEffects$cat

### mapping varaible names
main_ind <-coefs$mainEffects$cat
main_nn<-c() #main effect names
dict_j=c()
for (ind in main_ind){
  for (level_ind in (0:((num_vectors[ind])-1))){
    var_s <-paste0(names(i_num[ind]), as.character(level_ind))
    main_nn[length(main_nn)+1] = var_s
    cat_v <- paste0('cat',as.character(ind),'_',as.character(level_ind))
    dict_j[cat_v] <- var_s
  }
}

#generalize
mega_dt<-data.table()
int_dt<-data.table()
for (int_pair in coefs$interactionsCoef$catcat){ #each main and context pair
  int_df <-as.data.frame(int_pair) #convert it to a dataframe to access the elements
  pair_name<-c() #map glinternet var name to our name
  for (col_n in colnames(int_df)){
    for (row_n in rownames(int_df)){
      pair_name[[length(pair_name)+1]] = paste0(dict_j[row_n] , ":",dict_j[col_n])
    }
  }
  dt<-data.table( 
    hehe<-unlist(int_df)
  )
  dt<-transpose(dt) #transpose to set col names
  dt<-setNames(dt, unlist(pair_name))
  int_dt<-cbind(int_dt,dt)
}


main_dt <- data.table(
  kek <- unlist(coefs$mainEffectsCoef$cat)
)
main_dt<-transpose(main_dt)
main_dt<-setNames(main_dt, unlist(main_nn))
#main_dt
mega_dt<-cbind(main_dt, int_dt)

median(abs(as.numeric(mega_dt))) #median=> 0.009259197
median(abs(as.numeric(main_dt))) #median=> 0.01385716
median(abs(as.numeric(int_dt))) #median=> 0.008600446
quantile(abs(as.numeric(mega_dt)),probs=0.1) #=> 0.000727359, in this dataset spar = 0.1 in both main and int
quantile(abs(as.numeric(main_dt)),probs=0.1) #=> 0.0008068639 
quantile(abs(as.numeric(int_dt)),probs=0.1) #=> 0.0007278617 
#the first levels ain't 0 for glinternet
sim_one<- sim_params[1]
main_tp<-0
main_tn<- 0
main_tz<-0
acc_p<-0
acc_n<-0
acc_z<-0
for (main_name in main_nn){
  if(substring(main_name, 1, 7)!= "context"){
    acc<-acc+1
    glin_val <-main_dt[,get(main_name)]
    sim_val <- sim_one[,get(main_name)]
    
    if (abs(glin_val)< quantile(abs(as.numeric(main_dt)),probs=0.1)){
      glin_val = 0
    }
    if(sim_val>0){
      acc_p<-acc_p+1
    }
    if(sim_val<0){
      acc_n<-acc_n+1
    }
    if(sim_val == 0){
      acc_z<-acc_z+1
    }
    if (sim_val>0 && glin_val >0){
      main_tp <- main_tp+1
    }
    if (sim_val<0 && glin_val <0){
      main_tn <- main_tn+1
    }
    if (sim_val==0 && glin_val ==0){
      main_tz <- main_tz+1
    }
  }  
  
}
tp_p<-main_tp/acc_p
tn_p<-main_tn/acc_n
ts_p <- (main_tp + main_tn)/ (acc_p + acc_n)
tz_p<-main_tz/acc_z
t_p <- (main_tp + main_tn + main_tz)/ (acc_p + acc_n + acc_z)

#int
int_tp<-0
int_tn<- 0
int_tz<-0
acc_pi<-0
acc_ni<-0
acc_zi<-0
for (int_name in names(int_dt)){
  if(substring(int_name, 1, 7)!= "context"){
    acc<-acc+1
    glin_val <-int_dt[,get(int_name)]
    sim_val <- sim_one[,get(int_name)]
    
    if (abs(glin_val)< quantile(abs(as.numeric(int_dt)),probs=0.1)){
      glin_val = 0
    }
    if(sim_val>0){
      acc_pi<-acc_pi+1
    }
    if(sim_val<0){
      acc_ni<-acc_ni+1
    }
    if(sim_val == 0){
      acc_zi<-acc_zi+1
    }
    if (sim_val>0 && glin_val >0){
      int_tp <- int_tp+1
    }
    if (sim_val<0 && glin_val <0){
      int_tn <- int_tn+1
    }
    if (sim_val==0 && glin_val ==0){
      int_tz <- int_tz+1
    }
  }  
  
}
tp_pi<-int_tp/acc_pi
tn_pi<-int_tn/acc_ni
ts_pi <- (int_tp + int_tn)/ (acc_pi + acc_ni)
tz_pi<-int_tz/acc_zi
t_pi <- (int_tp + int_tn + int_tz)/ (acc_pi + acc_ni + acc_zi)

tp_all<- (main_tp + int_tp)/(acc_p + acc_pi)
tn_all <- (main_tn + int_tn)/(acc_n + acc_ni)
ts_all<- (main_tp + int_tp + main_tn + int_tn)/(acc_p + acc_pi +acc_n + acc_ni )
tz_all <- (main_tz + int_tz)/(acc_z + acc_zi)
t_all<- (main_tp + int_tp + main_tn + int_tn+ main_tz + int_tz)/(acc_p + acc_pi +acc_n + acc_ni + acc_z + acc_zi)

t
k<-"context_1"

threebythree<- data.frame(
  main= c(tp_p, tn_p, tz_p),
  int= c(tp_pi, tn_pi, tz_pi),
  all= c(tp_all, tn_all, tz_all)
)
#threebythree <- setNames(threebythree, c('true_p','true_n', 'true_z'))
rownames(threebythree)<-c('true_p','true_n', 'true_z')
threebythree
#data table doesn't support row names ;-;

#problem
#sparsity does not take into account of the first level of zeros
#can replicate the process of our simulation in python, but how practical is it?
library(MASS)
library(msm)
library(sparsereg)
library(BiocManager)
#install.packages("sparsereg", repos="http://cran.rstudio.com/", dependencies=TRUE)  #didn't work either

n<-20
k<-5
treat<-sample(c("a","b","c"),n,replace=TRUE,pr=c(.5,.25,.25))
treat2<-sample(c("a","b","c","d"),n,replace=TRUE,pr=c(.25,.25,.25,.25))
Sigma<-diag(k)
Sigma[Sigma==0]<-.5
X<-mvrnorm(n,m=rep(0,k),S=Sigma)
s1<-sparsereg(y, X, cbind(treat,treat2), scale.type="TX")
s1.EM<-sparsereg(y, X, cbind(treat,treat2), EM=TRUE, scale.type="TX")



set.seed(1)
n<-500
k<-100
Sigma<-diag(k)
Sigma[Sigma==0]<-.5
X<-mvrnorm(n,mu=rep(0,k),Sigma=Sigma)
y.true<-3+X[,2]*2+X[,3]*(-3)
y<-y.true+rnorm(n)
##Fit a linear model with five covariates.
s1<-sparsereg(y,X[,1:5])
difference(s1,var1=1,var2=2)

## Not run:
set.seed(1)
n<-500
k<-5
treat<-sample(c("a","b","c"),n,replace=TRUE,pr=c(.5,.25,.25))
treat2<-sample(c("a","b","c","d"),n,replace=TRUE,pr=c(.25,.25,.25,.25))
Sigma<-diag(k)
Sigma[Sigma==0]<-.5
X<-mvrnorm(n,m=rep(0,k),S=Sigma) #X has to be binary 
y.true<-3+X[,2]*2+(treat=="a")*2 +(treat=="b")*(-2)+X[,2]*(treat=="b")*(-2)+
  X[,2]*(treat2=="c")*2
y<-y.true+rnorm(n,sd=2)
##Fit a linear model.
s1<-sparsereg(y, X, cbind(treat,treat2), scale.type="TX") #context1 + context1 (cbind(treat,treat2))

s1_con <- sparsereg(unlist(test_df[,response]), as.matrix(th_x), third_p, scale.type = "TX")
s1_con_t<- sparsereg(test_df[,response], th_x)
s1.EM<-sparsereg(y, X, cbind(treat,treat2), EM=TRUE, scale.type="TX")
##Summarize results from MCMC fit
summary(s1)
summary(s1_con)
plot(s1)
plot(s1_con)
violinplot(s1)
#violinplot(s1_con_t)
violinplot(s1_con)
##Summarize results from MCMC fit
summary(s1.EM)
plot(s1.EM)
##Extension using a baseline category
s1.base<-sparsereg(y, X, treat, scale.type="TX", baseline.vec="a")
summary(s1.base)
plot(s1.base)
violinplot(s1.base)
## End(Not run)

#extract one set of data (sim_id == _0) => test_df
th_x <- test_df[,grep("gender0", colnames(test_df)):grep("reason4", colnames(test_df))]
th_y<-test_df[,response]
#third_p<- cbind(sim_data[,context_1],sim_data[,context_2])
third_p <- test_df[, context_1:context_2]

###RF
#df = subset(mydata, select = -c(x,z) )
test_df_rf <- subset(test_df, select = -c(sim_id, respondent_id, y))
#df <- mydata[ -c(1,3:4) ]
test_df_rf <- test_df_rf[,-c(2:12)]





split <- sample.split(test_df_rf, SplitRatio = 0.7) 
#split 
#sample = sample.split(data$anycolumn, SplitRatio = .75)
#train = subset(data, sample == TRUE)
#test  = subset(data, sample == FALSE)
train <- subset(test_df_rf, split == "TRUE") 
test <- subset(iris, split == "FALSE") 

# Fitting Random Forest to the train dataset 
set.seed(120)  # Setting seed 
classifier_RF = randomForest(x = train[-5], 
                             y = train$Species, 
                             ntree = 500) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[-5]) 

# Confusion Matrix 
confusion_mtx = table(test[, 5], y_pred) 
confusion_mtx 

# Plotting model 
plot(classifier_RF) 

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF) 
#rf <- randomForest(Species~., data=train, proximity=TRUE) print(rf)
rf <- randomForest(y=th_y, x=th_x, proximity=TRUE) #print(rf)


sample <- sample.split(test_df_rf, SplitRatio = 0.7) 
#split 
#sample = sample.split(data$anycolumn, SplitRatio = .75)
#train = subset(data, sample == TRUE)
#test  = subset(data, sample == FALSE)

train = subset(test_df_rf, sample == "TRUE") 
test <- subset(test_df_rf, split == "FALSE") 


smp_size <- floor(0.75 * nrow(test_df_rf))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(test_df_rf)), size = smp_size)

train <- test_df_rf[train_ind, ]
test <- test_df_rf[-train_ind, ]

# Fitting Random Forest to the train dataset 
#set.seed(120)  # Setting seed 
classifier_RF = randomForest(x = train[,2:ncol(test_df_rf)], 
                             y = as.factor(train$response))#, 
                             #ntree = 500) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[,2:ncol(test_df_rf)]) 

# Confusion Matrix 
confusion_mtx = table(test$response, y_pred) 
confusion_mtx 

# Plotting model 
plot(classifier_RF) 

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF) 
