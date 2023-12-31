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

# Read in data
data_file <- "cjsim_0.1_0.1_1_1_0.01_400.csv"

#paste0 => concatenate all elements without separator
#sim_data <- fread(paste0("~/git/buffalo/conjoint_decision/conj/simdata/",data_file))
sim_data <- fread(paste0("cjsim_0.1_0.1_1_1_0.01_400 (1).csv"))
#sim_params <- fread(paste0("~/git/buffalo/conjoint_decision/conj/sim_params/",data_file))
sim_params <- fread(paste0("cjsim_0.1_0.1_1_1_0.01_400.csv"),fill= TRUE)


#test_p <- fread(paste0("why"),header = TRUE, fill= TRUE)

# Define variables
conjoint_vars <- c("gender","case_note","race","length",
                   "age","setting","goal","n_removal","reason")
context_vars <- c("context_1","context_2")

# Set all variables as factors
for(cj in c(conjoint_vars,context_vars)){
  set(sim_data, j=cj, value=as.factor(sim_data[[cj]]))
}

# define the linear model

#paste=> concatenate two strings by separating with delimiters
maineffects_model <- paste(conjoint_vars, collapse="+")
interaction_model <- paste(paste(conjoint_vars,context_vars,sep=":"),collapse="+")
full_model <- as.formula(paste("response~",maineffects_model,"+",interaction_model))

# run the linear regressions
lm_res <- sim_data[, tidy(lm(full_model,data=.SD)), by=sim_id]

# combine with the truth
#a little confused
lm_res_w_truth <- merge(lm_res,
                        melt(sim_params, 
                             c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                             intersect(lm_res$term,names(sim_params))),
                        by.x=c("sim_id","term"),
                        by.y=c("sim_id","variable")
)
ggplot(lm_res_w_truth, aes(value,estimate))+geom_point() #+ stat_cor()
lm_res_w_truth[, pred := ifelse(p.value >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
lm_res_w_truth[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
no_c<-lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]


bon<-p.adjust(lm_res_w_truth[,p.value],"bonferroni")
#lm_res_w_truth['p.value_bon']<-bon
lm_res_w_truth[,bon:=bon]

lm_res_w_truth[, pred := ifelse(bon >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
lm_res_w_truth[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
bon_acc<-lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]

bh<-p.adjust(lm_res_w_truth[,p.value],"hochberg")
lm_res_w_truth[,bh:=bh]
lm_res_w_truth[, pred := ifelse(bh >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
lm_res_w_truth[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
bh_acc<-lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]

ash_ob<-ash(lm_res_w_truth[,estimate],lm_res_w_truth[,std.error], method="fdr")
ashR.norm<-ash_ob$result
#ashR.norm <- ash(beta_ash, se.beta_ash, mixcompdist = "normal", method = "fdr")$result
#ashR.norm.post.beta <- ashR.norm[,9]
#ashR.norm.post.sd <- ashR.norm[,10]
ashR.norm.post.qVal <- ashR.norm[,8]
lm_res_w_truth[,adaptive:=ashR.norm.post.qVal]
lm_res_w_truth[, pred := ifelse(adaptive >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
lm_res_w_truth[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
ad<-lm_res_w_truth[, list(accuracy=sum(pred==truth)/.N),by=sim_id]

sim_id_x<-c(seq.int(1,25),seq.int(1,25),seq.int(1,25),seq.int(1,25))

bon_s <- "bon"
bh_s <- "bh"
ash_s <-"ash"
no_c_s<-"no_c"
all_s<-c(rep(bon_s,25),rep(bh_s,25),rep(ash_s,25),rep(no_c_s,25))


all_acc<-c(unlist(bon_acc[,2]),unlist(bh_acc[,2]),unlist(ad[,2]),unlist(no_c[,2]))

df_p<- data.frame(all_s, all_acc)
df_p
ggplot(df_p, aes(sim_id_x, all_acc, col=all_s ))+
  geom_line()

p.adjust(lm_res[,p.value],"bonferroni")
p.adjust(lm_res[,p.value],"hochberg")
#ash_ob<-ash(lm_res[,estimate],lm_res[,std.error], method="fdr")
