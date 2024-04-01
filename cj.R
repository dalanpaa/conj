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
devtools::install_github('cttobin/ggthemr')

# Read in data
#data_file <- "cjsim_0.1_0.1_1_1_0.75_400.csv"

#paste0 => concatenate all elements without separator
#sim_data <- fread(paste0("~/git/buffalo/conjoint_decision/conj/simdata/",data_file))
sim_data <- fread(paste0("cjsim_0.1_0.1_1_1_0.75_4000.csv"))
#sim_data_test<-fread()
#sim_params <- fread(paste0("~/git/buffalo/conjoint_decision/conj/sim_params/",data_file))
sim_params <- fread(paste0("cjsim_0.1_0.1_1_1_0.75_4000_p.csv"),fill= TRUE)
maineffects_model <- paste(conjoint_vars, collapse="+")
#interaction_model <- paste(paste(conjoint_vars,context_vars,sep=":"),collapse="+")
#full_model <- as.formula(paste("response~",maineffects_model,"+",interaction_model)) 
interaction_model<-temp_str
full_model <- as.formula(paste("response~",maineffects_model,"+",temp_str)) 





master_function <- function(n_res){
  result<-data.table()
  for (sp_j in c('0.1_', '0.5_','0.9_')){
    for (sp_z in c('0.1_','0.5_','0.9_')){
      for (m_j in c('1_')){
        for (m_z in c('1_')){
          for (me in c('0.75_')){
            sim_d <- fread(paste0(paste0('cjsim_',sp_j, sp_z,m_j, m_z, me, '4000.csv')))
            #View(sim_d)
            
            print(paste0('cjsim_',sp_j, sp_z,m_j, m_z, me, '4000.csv'))
            #print(sim_d)
            sim_p <- fread(paste0(paste0('cjsim_',sp_j, sp_z,m_j, m_z, me, '4000_p.csv')),fill=TRUE)
            #View(sim_p)
            for(cj in c(conjoint_vars,context_vars)){
              set(sim_d, j=cj, value=as.factor(sim_d[[cj]]))
            }
            print(paste0('cjsim_',sp_j, sp_z,m_j, m_z, me, '4000_p.csv'))
            #View(sim_d)
            #View(sim_p)
            #tep_d<-sim_d
            #tep_p<-sim_p
            result<-rbind(result, everything_in_a_sim(sim_d, sim_p, n_res))
            #return (list(sim_d, sim_p))
            #return (result) #experiment
            
          }
        }
      }
    }
  }
  return (result)
}

#asddd_400<-master_function(400)



save_pls<-master_function(4000)
save_pls_400<-master_function(400)

#asdd<-master_function()
# c(d,p)%<-%master_function()
# everything_in_a_sim(d, p)
# View(sim_data)


#test_p <- fread(paste0("why"),header = TRUE, fill= TRUE)

# Define variables
conjoint_vars <- c("gender","case_note","race","length",
                   "age","setting","goal","n_removal","reason") #add age back in
context_vars <- c("context_1","context_2")

# Set all variables as factors
for(cj in c(conjoint_vars,context_vars)){
  set(sim_data, j=cj, value=as.factor(sim_data[[cj]]))
}

# define the linear model

#paste=> concatenate two strings by separating with delimiters
maineffects_model <- paste(conjoint_vars, collapse="+")
#interaction_model <- paste(paste(conjoint_vars,context_vars,sep=":"),collapse="+")
#full_model <- as.formula(paste("response~",maineffects_model,"+",interaction_model)) 
interaction_model<-temp_str
full_model <- as.formula(paste("response~",maineffects_model,"+",temp_str)) 

#main?
maineffects_model_lm <-as.formula(paste("response~",maineffects_model))
interaction_model_lm<-as.formula(paste("response~",temp_str))

# run the linear regressions
lm_res <- sim_data[, tidy(lm(full_model,data=.SD)), by=sim_id]
lm_res_main<-sim_data[, tidy(lm(maineffects_model_lm,data=.SD)), by=sim_id] #to extract bjs in a dumb way
lm_res_int<-sim_data[, tidy(lm(interaction_model_lm,data=.SD)), by=sim_id] #to extract bzs in a dumb way


lm_res_w_truth_test <- merge(lm_res,
                          melt(sim_params, 
                               c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                               intersect(lm_res$term,names(sim_params))),
                          by.x=c("sim_id","term"),
                          by.y=c("sim_id","variable")
)

# combine with the truth
#a little confused

everything_in_a_sim <- function(sim_data, sim_params, n_res){
  
  
  ### to subset data (will have a better way to address it later)
  #tstride<- sim_data[1:8000,]
  #for (i in seq(1,24)){
    #print(i)
   # tstride<-rbind(tstride, sim_data[(i*80000+1):(i*80000+8000),])
  #}
  #sim_data<-tstride
  if (n_res == 400){
    tstride<- sim_data[1:8000,]
    for (i in seq(1,24)){
    #print(i)
     tstride<-rbind(tstride, sim_data[(i*80000+1):(i*80000+8000),])
    }
    sim_data<-tstride
  }
  
  
  ###
  #get lm's for main, interaction, full
  full_model <- as.formula(paste("response~",maineffects_model,"+",temp_str)) 
  #interaction_model<-temp_str
  maineffects_model_lm <-as.formula(paste("response~",maineffects_model))
  
  lm_res <- sim_data[, tidy(lm(full_model,data=.SD)), by=sim_id]
  lm_res_main<-sim_data[, tidy(lm(maineffects_model_lm,data=.SD)), by=sim_id] #to extract bjs in a dumb way
  lm_res_int<- lm_res[ !(lm_res$term %in% lm_res_main$term),] #full-main
  
  df_full <- get_df(lm_res, lm_res, 'full',sim_params)
  df_int <- get_df(lm_res, lm_res_int, 'int',sim_params)
  df_main <- get_df(lm_res, lm_res_main, 'main',sim_params)
  
  df_final <- rbind(df_full, df_int, df_main)
  
  return (df_final)
}

o <-everything_in_a_sim(sim_data, sim_params)
emp<- data.table()

#test


full_model <- as.formula(paste("response~",maineffects_model,"+",temp_str)) 
lm_res <- sim_data[, tidy(lm(full_model,data=.SD)), by=sim_id]


lm_res_int<- lm_res[ !(lm_res$term %in% lm_res_main$term),]
#
#rbind(o,emp)
get_df <- function (linear_model_full, linear_model_part, model_type,sim_params){
  #no correction
  #linear_model_part<-lm_res
  lm_res_w_truth_f <- merge(linear_model_full,
                            melt(sim_params, 
                                 c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                                 intersect(linear_model_part$term,names(sim_params))), #unsure 
                            by.x=c("sim_id","term"),
                            by.y=c("sim_id","variable")
  )
  lm_res_w_truth_f[, pred := ifelse(p.value >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  no_c_full<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  nc_p_g_0.05<-lm_res_w_truth_f[, list(accuracy=sum(p.value>0.05)/.N),by=sim_id]
  nc_tp_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "+")&(truth=="+"))/sum(truth == "+")),by=sim_id]
  nc_tp_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "+")),'/',as.character(sum(truth == "+")))),by=sim_id]
  nc_tn_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "-")&(truth=="-"))/sum(truth =="-")),by=sim_id]
  nc_tn_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "-")),'/',as.character(sum(truth == "-")))),by=sim_id]
  nc_tz_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== 0)&(truth==0))/sum(truth==0)),by=sim_id]
  nc_tz_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== 0)),'/',as.character(sum(truth == 0)))),by=sim_id]
  
  #bon
  bon<-p.adjust(lm_res_w_truth_f[,p.value],"bonferroni")
  #lm_res_w_truth['p.value_bon']<-bon
  lm_res_w_truth_f[,bon:=bon]
  
  lm_res_w_truth_f[, pred := ifelse(bon >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  bon_acc_full<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  
  bon_p_g_0.05<-lm_res_w_truth_f[, list(accuracy=sum(bon>0.05)/.N),by=sim_id]
  
  bon_tp_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "+")&(truth=="+"))/sum(truth == "+")),by=sim_id]
  bon_tn_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "-")&(truth=="-"))/sum(truth =="-")),by=sim_id]
  bon_tz_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== 0)&(truth==0))/sum(truth==0)),by=sim_id]
  bon_tp_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "+")),'/',as.character(sum(truth == "+")))),by=sim_id]
  bon_tn_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "-")),'/',as.character(sum(truth == "-")))),by=sim_id]
  bon_tz_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== 0)),'/',as.character(sum(truth == 0)))),by=sim_id]
  
  
  #bh
  bh<-p.adjust(lm_res_w_truth_f[,p.value],"hochberg")
  lm_res_w_truth_f[,bh:=bh]
  lm_res_w_truth_f[, pred := ifelse(bh >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  bh_acc_full<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  
  bh_p_g_0.05<-lm_res_w_truth_f[, list(accuracy=sum(bh>0.05)/.N),by=sim_id]
  
  bh_tp_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "+")&(truth=="+"))/sum(truth == "+")),by=sim_id]
  bh_tn_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "-")&(truth=="-"))/sum(truth =="-")),by=sim_id]
  bh_tz_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== 0)&(truth==0))/sum(truth==0)),by=sim_id]
  
  bh_tp_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "+")),'/',as.character(sum(truth == "+")))),by=sim_id]
  bh_tn_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "-")),'/',as.character(sum(truth == "-")))),by=sim_id]
  bh_tz_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== 0)),'/',as.character(sum(truth == 0)))),by=sim_id]
  
  #ash
  ash_ob<-ash(lm_res_w_truth_f[,estimate],lm_res_w_truth_f[,std.error], method="fdr")
  ashR.norm<-ash_ob$result
  ashR.norm.post.qVal <- ashR.norm[,8]
  lm_res_w_truth_f[,adaptive:=ashR.norm.post.qVal]
  lm_res_w_truth_f[, pred := ifelse(adaptive >= 0.05,"0", ifelse(estimate < 0, "-","+") )] #change it to smaller value
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  ad_acc_full<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  
  ad_p_g_0.05<-lm_res_w_truth_f[, list(accuracy=sum(adaptive>0.05)/.N),by=sim_id]
  
  ad_tp_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "+")&(truth=="+"))/sum(truth == "+")),by=sim_id]
  ad_tn_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== "-")&(truth=="-"))/sum(truth =="-")),by=sim_id]
  ad_tz_full<-lm_res_w_truth_f[, list(accuracy=sum((pred== 0)&(truth==0))/sum(truth==0)),by=sim_id]
  
  ad_tp_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "+")),'/',as.character(sum(truth == "+")))),by=sim_id]
  ad_tn_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "-")),'/',as.character(sum(truth == "-")))),by=sim_id]
  ad_tz_str<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== 0)),'/',as.character(sum(truth == 0)))),by=sim_id]
  
  sim_id_dt <- sim_params$sim_id #fuck
  dt <- data.table(
    sim_id = sim_id_dt,
    no_c = no_c_full$accuracy,
    no_c_tp = nc_tp_full$accuracy,
    no_c_tn = nc_tn_full$accuracy,
    no_c_tz = nc_tz_full$accuracy,
    
    nc_pg_0.05 = nc_p_g_0.05$accuracy,
    
    nc_tp_frac = nc_tp_str$accuracy_string,
    nc_tn_frac = nc_tn_str$accuracy_string,
    nc_tz_frac = nc_tz_str$accuracy_string,
    
    bon = bon_acc_full$accuracy,
    bon_tp = bon_tp_full$accuracy,
    bon_tn = bon_tn_full$accuracy,
    bon_tz = bon_tz_full$accuracy,
    
    bon_pg_0.05 = bon_p_g_0.05$accuracy,
    
    bon_tp_frac = bon_tp_str$accuracy_string,
    bon_tn_frac = bon_tn_str$accuracy_string,
    bon_tz_frac = bon_tz_str$accuracy_string,
    
    bh = bh_acc_full$accuracy,
    bh_tp = bh_tp_full$accuracy,
    bh_tn = bh_tn_full$accuracy,
    bh_tz = bh_tz_full$accuracy,
    
    bh_pg_0.05 = bon_p_g_0.05$accuracy,
    
    bh_tp_frac = bh_tp_str$accuracy_string,
    bh_tn_frac = bh_tn_str$accuracy_string,
    bh_tz_frac = bh_tz_str$accuracy_string,
    
    ad = ad_acc_full$accuracy,
    ad_tp = ad_tp_full$accuracy,
    ad_tn = ad_tn_full$accuracy,
    ad_tz = ad_tz_full$accuracy,
    
    ad_pg_0.05 = ad_p_g_0.05$accuracy,
    
    ad_tp_frac = ad_tp_str$accuracy_string,
    ad_tn_frac = ad_tn_str$accuracy_string,
    ad_tz_frac = ad_tz_str$accuracy_string,
    
    model= model_type
    #also do false pos
  )
  return (dt)
}
temp_df <- get_df(lm_res, lm_res_main, 'int',sim_params)
lm_res_ff <- merge(lm_res,
                          melt(sim_params, 
                               c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                               intersect(lm_res$term,names(sim_params))), #unsure 
                          by.x=c("sim_id","term"),
                          by.y=c("sim_id","variable")
)
lm_res_fi <- merge(lm_res,
                          melt(sim_params, 
                               c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                               intersect(lm_res_int$term,names(sim_params))), #unsure 
                          by.x=c("sim_id","term"),
                          by.y=c("sim_id","variable")
)
lm_res_fm <- merge(lm_res,
                          melt(sim_params, 
                               c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                               intersect(lm_res_main$term,names(sim_params))), #unsure 
                          by.x=c("sim_id","term"),
                          by.y=c("sim_id","variable")
)

#get plot function
get_plot <- function(linear_model_full,linear_model_part,model_type) {
  lm_res_w_truth_f <- merge(linear_model_full,
                               melt(sim_params, 
                                    c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                                    intersect(linear_model_part$term,names(sim_params))), #unsure 
                               by.x=c("sim_id","term"),
                               by.y=c("sim_id","variable")
  )
  #ggplot(lm_res_w_truth_main, aes(value,estimate))+geom_point() #+ stat_cor()
  lm_res_w_truth_f[, pred := ifelse(p.value >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  no_c<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  #no_c_test<- get_acc(lm_res_w_truth_f,p.value) #test test
  
  bon<-p.adjust(lm_res_w_truth_f[,p.value],"bonferroni")
  #lm_res_w_truth['p.value_bon']<-bon
  lm_res_w_truth_f[,bon:=bon]
  
  lm_res_w_truth_f[, pred := ifelse(bon >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  bon_acc<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  
  bh<-p.adjust(lm_res_w_truth_f[,p.value],"hochberg")
  lm_res_w_truth_f[,bh:=bh]
  lm_res_w_truth_f[, pred := ifelse(bh >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  bh_acc<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  
  ash_ob<-ash(lm_res_w_truth_f[,estimate],lm_res_w_truth_f[,std.error], method="fdr")
  ashR.norm<-ash_ob$result
  #ashR.norm <- ash(beta_ash, se.beta_ash, mixcompdist = "normal", method = "fdr")$result
  #ashR.norm.post.beta <- ashR.norm[,9]
  #ashR.norm.post.sd <- ashR.norm[,10]
  ashR.norm.post.qVal <- ashR.norm[,8]
  lm_res_w_truth_f[,adaptive:=ashR.norm.post.qVal]
  lm_res_w_truth_f[, pred := ifelse(adaptive >= 0.05,"0", ifelse(estimate < 0, "-","+") )] #change it to smaller value
  lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  ad<-lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]
  ad_tp<-lm_res_w_truth_f[, list(accuracy=sum((pred== "+")&(truth=="+"))/sum(truth == "+")),by=sim_id]
  ad_tn<-lm_res_w_truth_f[, list(accuracy=sum((pred== "-")&(truth=="-"))/sum(truth =="-")),by=sim_id]
  #print(ad_tn)
  ad_tz<-lm_res_w_truth_f[, list(accuracy=sum((pred== 0)&(truth==0))/sum(truth==0)),by=sim_id]
  #line graph
  sim_id_x<-c(seq.int(1,25),seq.int(1,25),seq.int(1,25),seq.int(1,25))
  
  bon_s <- "bon"
  bh_s <- "bh"
  ash_s <-"ash"
  no_c_s<-"no_c"
  all_s<-c(rep(bon_s,25),rep(bh_s,25),rep(ash_s,25),rep(no_c_s,25))
  
  
  all_acc<-c(unlist(bon_acc[,2]),unlist(bh_acc[,2]),unlist(ad[,2]),unlist(no_c[,2]))
  #ci graph
  me_method <- c(bon_s, bh_s, ash_s, no_c_s)
  mean_me <-c(mean(bon_acc$accuracy), mean(bh_acc$accuracy),mean(ad$accuracy),mean(no_c$accuracy))
  bon_ci <-t.test(bon_acc$accuracy)$conf.int
  bh_ci <-t.test(bh_acc$accuracy)$conf.int
  ad_ci <-t.test(ad$accuracy)$conf.int
  no_c_ci <-t.test(no_c$accuracy)$conf.int
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_type,4)
  #sign graph
  sign <- c("tp","tn","tz")
  ad_tp_ttest<-t.test(ad_tp$accuracy)
  ad_tn_ttest<-t.test(ad_tn$accuracy)
  ad_tz_ttest<-t.test(ad_tz$accuracy)
  mean_acc<-c(ad_tp_ttest$est, ad_tn_ttest$est,ad_tz_ttest$est)
  lower_s<-c(ad_tp_ttest$conf.int[1], ad_tn_ttest$conf.int[1],ad_tz_ttest$conf.int[1])
  upper_s<-c(ad_tp_ttest$conf.int[2], ad_tn_ttest$conf.int[2],ad_tz_ttest$conf.int[2])
  model_s<-rep(model_type,3)
  #df's
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  
  df_p<- data.frame(all_s, all_acc)
  #df_p
  
  df_sign<-data.frame(sign, mean_acc, lower_s, upper_s, model_s)

  return (list(df_p,df_ci,df_sign))
}




#<-get_plot(lm_res, lm_res,"Full Model")

c(line_df_full, ci_df_full,sign_df_full) %<-% get_plot(lm_res, lm_res, "full")
c(line_df_int, ci_df_int,sign_df_int) %<-% get_plot(lm_res, lm_res_int, "int")
c(line_df_main, ci_df_main,sign_df_main) %<-% get_plot(lm_res, lm_res_main, "main")

#df for facet grid (method)
ci_df_all<-rbind(ci_df_full, ci_df_main, ci_df_int)



p<-ggplot(ci_df_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
    geom_point() +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
    ggtitle("CI's")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg

#facet grid true sign
sign_df_all <- rbind(sign_df_full, sign_df_int,sign_df_main)
p<-ggplot(sign_df_all, aes(sign, mean_acc)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_s, ymax = upper_s)) +
  ggtitle("sign")
ci_sign<-p+facet_grid(rows=vars(model_s))
ci_sign


#re<-t.test(ad$accuracy)
ggplot(first_df, aes(sim_id_x, all_acc, col=all_s ))+
  geom_line()+
  ggtitle("Bzs Only")

lm_res_w_truth_f <- merge(lm_res,
                          melt(sim_params, 
                               c("sim_id","sp_j","sp_z","m_j","m_z"),#,"n_respondents"
                               intersect(lm_res_int$term,names(sim_params))), #unsure 
                          by.x=c("sim_id","term"),
                          by.y=c("sim_id","variable")
)
get_acc <- function(lm, col_name){
  lm[, pred := ifelse(col_name >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
  lm[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
  return (lm[, list(accuracy=sum(pred==truth)/.N),by=sim_id])
}

get_acc(lm_res_w_truth_f, p.value)
#ggplot(lm_res_w_truth_main, aes(value,estimate))+geom_point() #+ stat_cor()
lm_res_w_truth_f[, pred := ifelse(p.value >= 0.05,"0", ifelse(estimate < 0, "-","+") )]
lm_res_w_truth_f[, truth := ifelse(value ==0,"0", ifelse(value < 0, "-","+") )]
lm_res_w_truth_f[, list(accuracy=sum(pred==truth)/.N),by=sim_id]

#to get true positive & true negative & true 0
lm_res_w_truth_f[, list(accuracy_t_s=sum((pred== "+")&(truth=="+"))/.N),by=sim_id]
lm_res_w_truth_f[, list(accuracy_t_s=sum((pred== "+")&(truth=="+"))/sum(pred == "+")),by=sim_id] #exp
lm_res_w_truth_f[, list(accuracy_t_s=sum((pred== "-")&(truth=="-"))/.N),by=sim_id]
ahh<-lm_res_w_truth_f[, list(accuracy_t_s=sum((pred== "-")&(truth=="-"))/sum(pred=="-")),by=sim_id]
lm_res_w_truth_f[, list(accuracy_t_s=sum((pred== 0)&(truth==0))/.N),by=sim_id]
lm_res_w_truth_f[, list(accuracy=sum(p.value>0.05)/.N),by=sim_id]

te<-lm_res_w_truth_f[, list(accuracy_string=paste0(as.character(sum(pred== "+")),'/',as.character(sum(pred == "+")))),by=sim_id]

cr<-crossing(conjoint_vars,context_vars)
temp_str<-""
for (i in 1:18){ #18
  #temp_str<-paste(paste(temp_str ,paste(toString(cr[i,]['conjoint_vars']),toString(cr[i,]['context_vars']),sep=":")), sep = "+")
  ele<-paste(toString(cr[i,]['conjoint_vars']),toString(cr[i,]['context_vars']),sep=":")
  temp_str<-paste(temp_str, ele, sep = "+")
}
temp_str<-substr(temp_str, 2, nchar(temp_str))
temp_str

paste(temp_str,collapse = "+")
#if main effect == 0, can the interaction terms be non-zero?
#look into true positive/negative rate (grid)
#facet_grid
#change in respondent (1000)
#ci's for different correction method as well (no correction)
#combine simulation data for aggregate analysis


#only need acc
tmod<- "full"
tdt<-data.table(#sim_it <- paste("sim_id",tmod, sep = "_"),
                sim = no_c$sim_id,
                no_c_full = no_c$accuracy,
                bhh_full = bh_acc$accuracy,
                he = 'dfsasd'
                )
tdt

tempnc<-no_c$sim_id[1] #22
substring(tempnc,1,22)

for (i in c('1','5')){
  print(i)
}
#graph

me_method <- c(bon_s, bh_s, ash_s, no_c_s)
mean_me <-c(mean(save_pls$bon), mean(save_pls$bh),mean(save_pls$ad),mean(save_pls$no_c))
bon_ci <-t.test(save_pls$bon)$conf.int
bh_ci <-t.test(save_pls$bh)$conf.int
ad_ci <-t.test(save_pls$ad)$conf.int
no_c_ci <-t.test(save_pls$no_c)$conf.int
lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
model<-save_pls$model
df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)

p<-ggplot(ci_df_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("CI's")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg

save_pls[model %like% 'full']

gen_sub_dtable_4000<-function(model_name){
  sub<-save_pls[model %like% model_name]
  #View(sub)
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  print(mean_me)
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  #print(bon_ci[1])
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
ci_df_all<-rbind(gen_sub_dtable_4000('full'), gen_sub_dtable_4000('main'), gen_sub_dtable_4000('int'))

#gen_sub_dtable('full')
gen_sub_dtable_400<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  #View(sub)
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  print(mean_me)
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  #print(bon_ci[1])
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}

gen_sub_dtable_both<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me_p <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  lower_bound_p <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound_p <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  
  sub<-save_pls[model %like% model_name]
  mean_me_f <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  lower_bound_f <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound_f <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  df_ci <- data.frame(me_method, mean_me_p, lower_bound_p, upper_bound_p, mean_me_f, lower_bound_f, upper_bound_f,model)
  return (df_ci)
}


ci_df_b<-rbind(gen_sub_dtable_both('full'), gen_sub_dtable_both('main'), gen_sub_dtable_both('int'))
colors_b<- c('n=4000' = 'Sky Blue', 'n=400' = 'brown')

ggthemr("dust")
p<-ggplot(ci_df_b, aes(me_method,mean_me_p, mean_me_f)) +        # ggplot2 plot with confidence intervals
  geom_point(aes(y=mean_me_p,colour = "blues9")) +
  geom_point(aes(y=mean_me_f,color = "blue8")) +
  geom_errorbar(aes(ymin = lower_bound_p, ymax = upper_bound_p,color = "n=400")) +
  geom_errorbar(aes(ymin = lower_bound_f, ymax = upper_bound_f,color = "n=4000")) +
  labs(x="Method",
       y="Accuracy",
       color="Legend")+
  scale_color_manual(values = colors_b)+
  #ggthemr("dust")+
  
  ggtitle("CI for all sims")


ci_fg<-p+facet_grid(rows=vars(model)) #+ scale_color_discrete(name='size',breaks=c("4000","400")) #+ labs(colour = "size")#theme(legend.position = "none"
ci_fg

gen_sub_dtable_new<-function(model_name,list_of_att,list_of_str){
  sub<-save_pls[model %like% model_name]
  #View(sub)
  me_method <- list_of_str#c('bon', 'bh', 'ash', 'no_c')
  lower_bound<-c()
  upper_bound<-c()
  mean_me<-c()
  for (i in list_of_att){
    mean_me<-c(mean_me, mean(sub$i))
    lower_bound<-c(lower_bound, t.test(sub$i)$conf.int[1])
    upper_bound<-c(lower_bound, t.test(sub$i)$conf.int[2])
  }
  #mean_me <-c(mean(sub$bonv), mean(sub$bhv),mean(sub$adv),mean(sub$ncv))
  #print(mean_me)
  
  #model<-rep(model_name,length(list))
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound)
  return (df_ci)
}
gen_sub_dtable_new('full',c('bon', 'bh', 'ash', 'no_c'),c('bon', 'bh', 'ash', 'no_c'))
sub$'bon'
l<-c()
for (i in c('bon', 'bh', 'ash', 'no_c')){
  l<-c(l,i)
  print(l)
}
#sub<-save_pls[model %like% model_name]
#View(sub)
me_method <- c('bon', 'bh', 'ash', 'no_c')
lower_bound<-c()
upper_bound<-c()
mean_me<-c()

for (i in c('bon', 'bh', 'ash', 'no_c')){
  #print(i)
  temp<-i
  print(temp)
  print(mean(sub[,eval(temp)]))
  
  #print(mean(sub$temp))
  #mean_me<-c(mean_me, mean(sub$i))
  #lower_bound<-c(lower_bound, t.test(sub$i)$conf.int[1])
  #upper_bound<-c(lower_bound, t.test(sub$i)$conf.int[2])
}
#mean_me <-c(mean(sub$bonv), mean(sub$bhv),mean(sub$adv),mean(sub$ncv))
#print(mean_me)

#model<-rep(model_name,length(list))
df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound)

gen_sub_dtable_true_sign<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  #View(sub)
  me_method <- c('ad_tp', 'ad_tn', 'ad_tz')
  mean_me <-c(mean(sub$'ad_tp',na.rm=TRUE), mean(sub$'ad_tn',na.rm=TRUE),mean(sub$'ad_tz',na.rm=TRUE) )
  #print(mean_me)
  tp_ci <-t.test(sub$ad_tp)$conf.int
  tn_ci <-t.test(sub$ad_tn)$conf.int
  tz_ci <-t.test(sub$ad_tz)$conf.int
  
  #print(bon_ci[1])
  lower_bound <- c(tp_ci[1],tn_ci[1],tz_ci[1])
  upper_bound <- c(tp_ci[2],tn_ci[2],tz_ci[2])
  model<-rep(model_name,3)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
ad_sign_all<-rbind(gen_sub_dtable_true_sign('full'), gen_sub_dtable_true_sign('main'), gen_sub_dtable_true_sign('int'))
p<-ggplot(ad_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("ash CI all sim (n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg
# one individual plot
gen_sub_dtable_true_sign_ind<-function(sparj, sparz, model_name){
  sub<-save_pls[model %like% model_name]
  sub<-save_pls[substring(sim_id,7,9) %like% sparj]
  sub<-sub[substring(sim_id,11,13) %like% sparz]
  #View(sub)
  me_method <- c('ad_tp', 'ad_tn', 'ad_tz')
  mean_me <-c(mean(sub$'ad_tp',na.rm=TRUE), mean(sub$'ad_tn',na.rm=TRUE),mean(sub$'ad_tz',na.rm=TRUE) )
  #print(mean_me)
  tp_ci <-t.test(sub$ad_tp)$conf.int
  tn_ci <-t.test(sub$ad_tn)$conf.int
  tz_ci <-t.test(sub$ad_tz)$conf.int
  
  #print(bon_ci[1])
  lower_bound <- c(tp_ci[1],tn_ci[1],tz_ci[1])
  upper_bound <- c(tp_ci[2],tn_ci[2],tz_ci[2])
  model<-rep(model_name,3)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
ad_sign_all<-rbind(gen_sub_dtable_true_sign_ind('0.9','0.9','full'), gen_sub_dtable_true_sign_ind('0.9','0.9','main'), gen_sub_dtable_true_sign_ind('0.9','0.9','int'))
p<-ggplot(ad_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("ash CI 0.9 & 0.9")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg

#sub_int<-save_pls[model %like% 'int']
#mean(sub_int$'ad_tp')
#bon ci 
gen_sub_dtable_true_sign_bon<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  #View(sub)
  me_method <- c('bon_tp', 'bon_tn', 'bon_tz')
  mean_me <-c(mean(sub$'bon_tp',na.rm=TRUE), mean(sub$'bon_tn',na.rm=TRUE),mean(sub$'bon_tz',na.rm=TRUE) )
  #print(mean_me)
  tp_ci <-t.test(sub$bon_tp)$conf.int
  tn_ci <-t.test(sub$bon_tn)$conf.int
  tz_ci <-t.test(sub$bon_tz)$conf.int
  
  #print(bon_ci[1])
  lower_bound <- c(tp_ci[1],tn_ci[1],tz_ci[1])
  upper_bound <- c(tp_ci[2],tn_ci[2],tz_ci[2])
  model<-rep(model_name,3)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
bon_sign_all<-rbind(gen_sub_dtable_true_sign_bon('full'), gen_sub_dtable_true_sign_bon('main'), gen_sub_dtable_true_sign_bon('int'))
p<-ggplot(bon_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("bon CI all sim (n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg
#bh ci
gen_sub_dtable_true_sign_bh<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  #View(sub)
  me_method <- c('bh_tp', 'bh_tn', 'bh_tz')
  mean_me <-c(mean(sub$'bh_tp',na.rm=TRUE), mean(sub$'bh_tn',na.rm=TRUE),mean(sub$'bh_tz',na.rm=TRUE) )
  #print(mean_me)
  tp_ci <-t.test(sub$bh_tp)$conf.int
  tn_ci <-t.test(sub$bh_tn)$conf.int
  tz_ci <-t.test(sub$bh_tz)$conf.int
  
  #print(bon_ci[1])
  lower_bound <- c(tp_ci[1],tn_ci[1],tz_ci[1])
  upper_bound <- c(tp_ci[2],tn_ci[2],tz_ci[2])
  model<-rep(model_name,3)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
bh_sign_all<-rbind(gen_sub_dtable_true_sign_bh('full'), gen_sub_dtable_true_sign_bh('main'), gen_sub_dtable_true_sign_bh('int'))
p<-ggplot(bh_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("bh CI all sim(n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg

gen_sub_dtable_true_sign_nc<-function(model_name){
  sub<-save_pls_400[model %like% model_name]
  #View(sub)
  me_method <- c('nc_tp', 'nc_tn', 'nc_tz')
  mean_me <-c(mean(sub$'no_c_tp',na.rm=TRUE), mean(sub$'no_c_tn',na.rm=TRUE),mean(sub$'no_c_tz',na.rm=TRUE) )
  #print(mean_me)
  tp_ci <-t.test(sub$no_c_tp)$conf.int
  tn_ci <-t.test(sub$no_c_tn)$conf.int
  tz_ci <-t.test(sub$no_c_tz)$conf.int
  
  #print(bon_ci[1])
  lower_bound <- c(tp_ci[1],tn_ci[1],tz_ci[1])
  upper_bound <- c(tp_ci[2],tn_ci[2],tz_ci[2])
  model<-rep(model_name,3)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
nc_sign_all<-rbind(gen_sub_dtable_true_sign_nc('full'), gen_sub_dtable_true_sign_nc('main'), gen_sub_dtable_true_sign_nc('int'))
p<-ggplot(nc_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("nc CI all sim (n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg


sub_0.1<-save_pls[substring(sim_id,7,9) %like% '0.1']

gen_sub_dtable_spbj<-function(spar, model_name){
  sub<-save_pls_400[substring(sim_id,7,9) %like% spar]
  sub<-sub[model %like% model_name]
  #View(sub)
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  print(mean_me)
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  print(bon_ci[1])
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
nc_sign_all<-rbind(gen_sub_dtable_spbj('0.9','full'), gen_sub_dtable_spbj('0.9','main'), gen_sub_dtable_spbj('0.9','int'))
p<-ggplot(nc_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("BJ Sparsity = 0.9 all sim (n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg


gen_sub_dtable_spbz<-function(spar, model_name){
  sub<-save_pls_400[substring(sim_id,11,13) %like% spar]
  sub<-sub[model %like% model_name]
  #View(sub)
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  print(mean_me)
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  print(bon_ci[1])
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
nc_sign_all<-rbind(gen_sub_dtable_spbz('0.9','full'), gen_sub_dtable_spbz('0.9','main'), gen_sub_dtable_spbz('0.9','int'))
p<-ggplot(nc_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("BZ Sparsity = 0.9 all sim (n=400)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg

gen_sub_dtable_spbzbj<-function(sparj,sparz, model_name){
  sub<-save_pls[substring(sim_id,7,9) %like% sparj]
  sub<-sub[substring(sim_id,11,13) %like% sparz]
  sub<-sub[model %like% model_name]
  View(sub)
  me_method <- c('bon', 'bh', 'ash', 'no_c')
  mean_me <-c(mean(sub$bon), mean(sub$bh),mean(sub$ad),mean(sub$no_c))
  print(mean_me)
  bon_ci <-t.test(sub$bon)$conf.int
  bh_ci <-t.test(sub$bh)$conf.int
  ad_ci <-t.test(sub$ad)$conf.int
  no_c_ci <-t.test(sub$no_c)$conf.int
  print(bon_ci[1])
  lower_bound <- c(bon_ci[1],bh_ci[1],ad_ci[1],no_c_ci[1])
  upper_bound <- c(bon_ci[2],bh_ci[2],ad_ci[2],no_c_ci[2])
  model<-rep(model_name,4)
  df_ci <- data.frame(me_method, mean_me, lower_bound, upper_bound,model)
  return (df_ci)
}
nc_sign_all<-rbind(gen_sub_dtable_spbzbj('0.9','0.9','full'), gen_sub_dtable_spbzbj('0.9','0.9','main'), gen_sub_dtable_spbzbj('0.9','0.9','int'))
p<-ggplot(nc_sign_all, aes(me_method, mean_me)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +
  ggtitle("BJ = 0.9, BZ = 0.9 all sim (n=4000)")
ci_fg<-p+facet_grid(rows=vars(model))
ci_fg



gen_dtable_plot_2_4k<-function (){
  mean_list <- c()
  bj_list <- c()
  bz_list <- c()
  lower_bound <- c()
  upper_bound <- c()
  n_res <- c()
  model <- c()
  models <- c("full","int","main")
  #names_df <- save_pls
  spar <- c('0.1', '0.5','0.9')

  for (m in models){
    for (bj in spar){
      for(bz in spar){
        
        #return ()
        #print(bj)
        #print(bz)
        #print(m)
        sub <- save_pls[substring(sim_id,7,9) %like% bj]
        sub <- sub[substring(sim_id,11,13) %like% bz]
        sub<-sub[model %like% m]
        #View(sub)
        #append(mean_list, mean(sub$ad))
        mean_list[[length(mean_list)+1]]= mean(sub$ad)
        ad_ci <-t.test(sub$ad)$conf.int
        #append(lower_bound, ad_ci[1])
        #append(upper_bound, ad_ci[2])
        lower_bound[[length(lower_bound)+1]]= ad_ci[1]
        upper_bound[[length(upper_bound)+1]]= ad_ci[2]
        #append(model, m)
        model[length(model)+1]= m
        #append(n_res, 4000)
        n_res[[length(n_res)+1]]= 4000
        #append(bj_list, bj)
        bj_list[[length(bj_list)+1]]=as.numeric(bj)
        bz_list[[length(bz_list)+1]]=as.numeric(bz)
        #return ()
      }
    }
  }
  #print(bj_list)
  dt= data.table(
    bj = bj_list,
    bz = bz_list,
    mean = mean_list,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_res = n_res, 
    model = model
  )
  return (dt)
}
gen_dtable_plot_2_4h<-function (){
  mean_list <- c()
  bj_list <- c()
  bz_list <- c()
  lower_bound <- c()
  upper_bound <- c()
  n_res <- c()
  model <- c()
  models <- c("full","int","main")
  #names_df <- save_pls
  spar <- c('0.1', '0.5','0.9')
  
  for (m in models){
    for (bj in spar){
      for(bz in spar){
        
        #return ()
        #print(bj)
        #print(bz)
        #print(m)
        sub <- save_pls_400[substring(sim_id,7,9) %like% bj]
        sub <- sub[substring(sim_id,11,13) %like% bz]
        sub<-sub[model %like% m]
        #View(sub)
        #append(mean_list, mean(sub$ad))
        mean_list[[length(mean_list)+1]]= mean(sub$ad)
        ad_ci <-t.test(sub$ad)$conf.int
        #append(lower_bound, ad_ci[1])
        #append(upper_bound, ad_ci[2])
        lower_bound[[length(lower_bound)+1]]= ad_ci[1]
        upper_bound[[length(upper_bound)+1]]= ad_ci[2]
        #append(model, m)
        model[length(model)+1]= m
        #append(n_res, 4000)
        n_res[[length(n_res)+1]]= 400
        #append(bj_list, bj)
        bj_list[[length(bj_list)+1]]=as.numeric(bj)
        bz_list[[length(bz_list)+1]]=as.numeric(bz)
        #return ()
      }
    }
  }
  #print(bj_list)
  dt= data.table(
    bj = bj_list,
    bz = bz_list,
    mean = mean_list,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_res = n_res, 
    model = model
  )
  return (dt)
}

df_plot_2_4h <- gen_dtable_plot_2_4h()
df_plot_2_4k <- gen_dtable_plot_2_4k()
df_plot_2<-rbind(df_plot_2_4h,df_plot_2_4k)


colors_b<- c('0.1' = 'Sky Blue', '0.5' = 'brown', '0.9' = 'purple' )

ggthemr("dust")

p<- ggplot(df_plot_2, aes(factor(bj), as.numeric(mean), color=factor(bz)))+
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound))+
  labs(x="Beta J",
       y="Accuracy",
  ) +
  ggtitle("Ash Only")
  

  #scale_x_discrete()

ci_fg<-p+facet_grid(rows=vars(model),cols=vars(n_res)) 
ci_fg


labs(x="Method",
     y="Accuracy",
)

ci_fg<-p+facet_grid(rows=vars(model), cols=vars(bj)) 
ci_fg

df_plot_2$bj<-as.numeric(df_plot_2$bj)
df_plot_2$bz<-as.numeric(df_plot_2$bz)
df_plot_2$mean<-as.numeric(df_plot_2$mean)
df_plot_2$lower_bound<-as.numeric(df_plot_2$lower_bound)
df_plot_2$upper_bound<-as.numeric(df_plot_2$upper_bound)
df_plot_2$n_res<-as.numeric(df_plot_2$n_res)

gen_dtable_plot_3<-function (){
  mean_list <- c()
  bj_list <- c()
  bz_list <- c()
  lower_bound <- c()
  upper_bound <- c()
  correction_m <- c()
  model <- c()
  models <- c("full","int","main")
  #names_df <- save_pls
  spar <- c('0.1', '0.5','0.9')
  corr<-c('ad', 'no_c')
  for (correction in corr){
    for (m in models){
      for (bj in spar){
        for(bz in spar){
          #print(correction)
          #View(sub[[correction]])
          
          sub <- save_pls[substring(sim_id,7,9) %like% bj]
          sub <- sub[substring(sim_id,11,13) %like% bz]
          sub<-sub[model %like% m]
          mean_list[[length(mean_list)+1]]= mean(sub[[correction]])
          ad_ci <-t.test(sub[[correction]])$conf.int
          lower_bound[[length(lower_bound)+1]]= ad_ci[1]
          upper_bound[[length(upper_bound)+1]]= ad_ci[2]
          model[length(model)+1]= m
          correction_m[[length(correction_m)+1]]= correction
          bj_list[[length(bj_list)+1]]=as.numeric(bj)
          bz_list[[length(bz_list)+1]]=as.numeric(bz)
          #return ()
        }
      }
    }
  }
  #print(bj_list)
  dt= data.table(
    bj = bj_list,
    bz = bz_list,
    mean = mean_list,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    correction = correction_m, 
    model = model
  )
  return (dt)
}
df_plot_3 <- gen_dtable_plot_3()

p<- ggplot(df_plot_3, aes(factor(bj), as.numeric(mean), color=factor(bz)))+
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound))+
  labs(x="Beta J",
       y="Accuracy",
  ) +
  ggtitle("Ash vs No Correction, n=4000")



#scale_x_discrete()

ci_fg<-p+facet_grid(rows=vars(model),cols=vars(as.character(correction))) 
ci_fg


df_plot_3$bj<-as.numeric(df_plot_3$bj)
df_plot_3$bz<-as.numeric(df_plot_3$bz)
df_plot_3$mean<-as.numeric(df_plot_3$mean)
df_plot_3$lower_bound<-as.numeric(df_plot_3$lower_bound)
df_plot_3$upper_bound<-as.numeric(df_plot_3$upper_bound)



gen_dtable_plot_4<-function (){
  mean_list <- c()
  bj_list <- c()
  bz_list <- c()
  lower_bound <- c()
  upper_bound <- c()
  correction_m <- c()
  model <- c()
  models <- c("full","int","main")
  #names_df <- save_pls
  spar <- c('0.1', '0.5','0.9')
  corr<-c('ad_ts_av', 'nc_ts_av')
  #mean(sub$'ad_tn',na.rm=TRUE)
  for (correction in corr){
    for (m in models){
      for (bj in spar){
        for(bz in spar){
          #print(correction)
          #View(sub[[correction]])
          
          sub <- save_pls[substring(sim_id,7,9) %like% bj]
          sub <- sub[substring(sim_id,11,13) %like% bz]
          sub<-sub[model %like% m]
          mean_list[[length(mean_list)+1]]= mean(sub[[correction]],na.rm=TRUE)
          #k<-mean(sub[[correction]],na.rm=TRUE)
          #
          obj<-try2(t.test(sub[[correction]])$conf.int, silent=TRUE)
          if(is(obj,'try-error')){
            lower_bound[[length(lower_bound)+1]]= 1
            lower_bound[[length(lower_bound)+1]]= 1
          }
          else{
            ad_ci <-t.test(sub[[correction]])$conf.int
            lower_bound[[length(lower_bound)+1]]= ad_ci[1]
            upper_bound[[length(upper_bound)+1]]= ad_ci[2]
          }
          
          
          
          
          #
          model[length(model)+1]= m
          correction_m[[length(correction_m)+1]]= correction
          bj_list[[length(bj_list)+1]]=as.numeric(bj)
          bz_list[[length(bz_list)+1]]=as.numeric(bz)
          #return ()
        }
      }
    }
  }
  #print(bj_list)
  dt= data.table(
    bj = bj_list,
    bz = bz_list,
    mean = mean_list,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    correction = correction_m, 
    model = model
  )
  return (dt)
}

df_plot_4 <- gen_dtable_plot_4()

df_plot_4$bj<-as.numeric(df_plot_4$bj)
df_plot_4$bz<-as.numeric(df_plot_4$bz)
df_plot_4$mean<-as.numeric(df_plot_4$mean)
df_plot_4$lower_bound<-as.numeric(df_plot_4$lower_bound)
df_plot_4$upper_bound<-as.numeric(df_plot_4$upper_bound)

p<- ggplot(df_plot_4, aes(factor(bj), as.numeric(mean), color=factor(bz)))+
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound))+
  labs(x="Beta J",
       y="Accuracy",
  ) +
  ggtitle("Ash vs No Correction, n=4000, non_zero")



#scale_x_discrete()

ci_fg<-p+facet_grid(rows=vars(model),cols=vars(as.character(correction))) 
ci_fg


tid<-(sub$sim_id)[1]
tstride<- sim_data[1:8000,]
for (i in seq(1,24)){
  #print(i)
  tstride<-rbind(tstride, sim_data[(i*80000+1):(i*80000+8000),])
}


try2 <- function(code, silent = FALSE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) message(c)
    invisible(structure(msg, class = "try-error"))
  })
}

gen_dtable_plot_5<-function (){
  mean_list <- c()
  bj_list <- c()
  bz_list <- c()
  lower_bound <- c()
  upper_bound <- c()
  correction_m <- c()
  model <- c()
  models <- c("full","int","main")
  #names_df <- save_pls
  spar <- c('0.1', '0.5','0.9')
  corr<-c('ad_tz', 'no_c_tz')
  for (correction in corr){
    for (m in models){
      for (bj in spar){
        for(bz in spar){
          #print(correction)
          #View(sub[[correction]])
          
          sub <- save_pls[substring(sim_id,7,9) %like% bj]
          sub <- sub[substring(sim_id,11,13) %like% bz]
          sub<-sub[model %like% m]
          mean_list[[length(mean_list)+1]]= mean(sub[[correction]])
          ad_ci <-t.test(sub[[correction]])$conf.int
          lower_bound[[length(lower_bound)+1]]= ad_ci[1]
          upper_bound[[length(upper_bound)+1]]= ad_ci[2]
          model[length(model)+1]= m
          correction_m[[length(correction_m)+1]]= correction
          bj_list[[length(bj_list)+1]]=as.numeric(bj)
          bz_list[[length(bz_list)+1]]=as.numeric(bz)
          #return ()
        }
      }
    }
  }
  #print(bj_list)
  dt= data.table(
    bj = bj_list,
    bz = bz_list,
    mean = mean_list,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    correction = correction_m, 
    model = model
  )
  return (dt)
}

df_plot_5 <- gen_dtable_plot_5()

df_plot_5$bj<-as.numeric(df_plot_5$bj)
df_plot_5$bz<-as.numeric(df_plot_5$bz)
df_plot_5$mean<-as.numeric(df_plot_5$mean)
df_plot_5$lower_bound<-as.numeric(df_plot_5$lower_bound)
df_plot_5$upper_bound<-as.numeric(df_plot_5$upper_bound)

p<- ggplot(df_plot_5, aes(factor(bj), as.numeric(mean), color=factor(bz)))+
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound))+
  labs(x="Beta J",
       y="Accuracy",
  ) +
  ggtitle("Ash vs No Correction, n=4000, true-zero")



#scale_x_discrete()

ci_fg<-p+facet_grid(rows=vars(model),cols=vars(as.character(correction))) 
ci_fg




save_pls$ad_ts_av <- ((save_pls$ad_tn)+ save_pls$ad_tp)/2
save_pls$nc_ts_av <- (save_pls$no_c_tn + save_pls$no_c_tp)/2

save_pls[,n_res:=4000]
save_pls_400[,n_res:=400]
