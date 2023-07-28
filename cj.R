library(readr)
library(dplyr)
library(tidyverse)
library(broom)
library(stringr)
library(rlist)
library(cregg)
#load datasets
#test_cj <- read.csv("test_cj.csv")
real_bzs<-read.csv("bzss.csv")
trim_subset <-read.csv("one_trim.csv")
real_bjs <-read.csv("bjss_new.csv")
#convert numerical data to categorical
trim_subset$gender <- as.factor(trim_subset$gender)
trim_subset$case_note <- as.factor(trim_subset$case_note)
trim_subset$race <- as.factor(trim_subset$race)
trim_subset$length <- as.factor(trim_subset$length)
trim_subset$age <- as.factor(trim_subset$age)
trim_subset$setting <- as.factor(trim_subset$setting)
trim_subset$goal <- as.factor(trim_subset$goal)
trim_subset$n_removal <- as.factor(trim_subset$n_removal)
trim_subset$reason <- as.factor(trim_subset$reason)
trim_subset$has_social <- as.factor(trim_subset$has_social)
trim_subset$question_bucket <- as.factor(trim_subset$question_bucket)

#split data by sim_id
split_by_sim_id <-
  trim_subset %>%
  group_nest(sim_id)
split_by_sim_id

#linear model with interaction terms
model_by_sim_id <-
  split_by_sim_id %>%
  mutate( model = map(data, ~ lm(formula= selected ~  gender + gender:has_social + gender:question_bucket + case_note + case_note:has_social + case_note:question_bucket + race + race:has_social + race:question_bucket + length + length:has_social + length:question_bucket + age + age:has_social+ age:question_bucket + setting + setting:has_social + setting : question_bucket + goal + goal: has_social + goal : question_bucket + n_removal + n_removal: has_social + n_removal:question_bucket + reason + reason:has_social + reason: question_bucket +has_social + question_bucket , data = .x )))
model_by_sim_id



#cregg
#f1 <- selected ~  gender + gender:has_social + gender:question_bucket + case_note + case_note:has_social + case_note:question_bucket + race + race:has_social + race:question_bucket + length + length:has_social + length:question_bucket + age + age:has_social+ age:question_bucket + setting + setting:has_social + setting : question_bucket + goal + goal: has_social + goal : question_bucket + n_removal + n_removal: has_social + n_removal:question_bucket + reason + reason:has_social + reason: question_bucket +has_social + question_bucket
#f2<-selected ~gender + case_note 
#plot(mm(trim_subset[1:400,],f2, id=~X),vline=0.5)

#voodoo magic from tidymodeling book 
linear_fit<-  model_by_sim_id %>%
  mutate(coef = map(model, tidy)) %>% 
  select (sim_id, coef) %>% 
  unnest( cols = c(coef))
linear_fit

#confusion matrix
for (sim in 1:1){
  #print(sim)
  bj_tb<-list()
  accuracy=list()
  plusplus <-0
  pluszero <-0
  plusminus <-0
  zeroplus <-0
  zerozero <-0
  zerominus <-0
  minusplus <-0
  minuszero <-0
  minusminus <-0
  
  for (i in 1:24){ #i = main effect
    
    est<-linear_fit[i+1 +125*(sim-1),]['estimate']
    rl <- real_bjs[sim,i+1]
  
    if (linear_fit[i+1 +125*(sim-1),]['p.value']>0.05){
      est<-0
    }
    #print(est)
    #print(rl)
    if(rl>0 & est>0){
      plusplus <- plusplus +1
    }
    if(rl>0 & est==0){
      pluszero <- pluszero +1
    }
    if(rl>0 & est<0){
      plusminus <- plusminus +1
    }
    if(rl==0 & est>0){
      zeroplus <- zeroplus +1
    }
    if(rl==0 & est==0){
      zerozero <-zerozero +1
    }
    if(rl==0 & est<0){
      zerominus <- zerominus +1
    }
    if(rl<0 & est>0){
      minusplus <- minusplus +1
    }
    if(rl<0 & est==0){
      minuszero <- minuszero +1
    }
    if(rl<0 & est<0){
      minusminus <- minusminus +1
    }
  }
  tb=matrix(c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus),ncol=3,byrow=TRUE)
  colnames(tb) = c('+','0','-')
  rownames(tb)<-c('+','0','-')
  ftb=as.table(tb)
  accu <- (Reduce("+",c(plusplus, zerozero, minusminus)) / Reduce("+",c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus)))
  #ftb
  print(accu)
  
  bj_tb<-append(bj_tb,ftb)
  accuracy[[length(accuracy)+1]]=accu
  
}
#bzs
for (sim in 1:1){
  #print(sim)
  bj_tb<-list()
  accuracy=list()
  plusplus <-0
  pluszero <-0
  plusminus <-0
  zeroplus <-0
  zerozero <-0
  zerominus <-0
  minusplus <-0
  minuszero <-0
  minusminus <-0
  
  for (i in 30:125){ #i = main effect
    
    est<-linear_fit[i +125*(sim-1),]['estimate']
    rl <- real_bzs[sim,i+1 -29]
    
    if (linear_fit[i +125*(sim-1),]['p.value']>0.05){
      est<-0
    }
    #print(est)
    #print(rl)
    if(rl>0 & est>0){
      plusplus <- plusplus +1
    }
    if(rl>0 & est==0){
      pluszero <- pluszero +1
    }
    if(rl>0 & est<0){
      plusminus <- plusminus +1
    }
    if(rl==0 & est>0){
      zeroplus <- zeroplus +1
    }
    if(rl==0 & est==0){
      zerozero <-zerozero +1
    }
    if(rl==0 & est<0){
      zerominus <- zerominus +1
    }
    if(rl<0 & est>0){
      minusplus <- minusplus +1
    }
    if(rl<0 & est==0){
      minuszero <- minuszero +1
    }
    if(rl<0 & est<0){
      minusminus <- minusminus +1
    }
  }
  tb=matrix(c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus),ncol=3,byrow=TRUE)
  colnames(tb) = c('+','0','-')
  rownames(tb)<-c('+','0','-')
  ftb=as.table(tb)
  accu <- (Reduce("+",c(plusplus, zerozero, minusminus)) / Reduce("+",c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus)))
  #ftb
  print(accu)
  
  bj_tb<-append(bj_tb,ftb)
  accuracy[[length(accuracy)+1]]=accu
  
}

#master function (working progress)
fo <-selected ~  gender + gender:has_social + gender:question_bucket + case_note + case_note:has_social + case_note:question_bucket + race + race:has_social + race:question_bucket + length + length:has_social + length:question_bucket + age + age:has_social+ age:question_bucket + setting + setting:has_social + setting : question_bucket + goal + goal: has_social + goal : question_bucket + n_removal + n_removal: has_social + n_removal:question_bucket + reason + reason:has_social + reason: question_bucket +has_social + question_bucket 
get_confusion_matrcies <- function(dataset, simulation_id, formula_sim) {
  split_by_sim_id <-
    trim_subset %>%
    group_nest(sim_id)
  model_by_sim_id <-
    split_by_sim_id %>%
    mutate( model = map(data, ~ lm(fo, data = .x )))
  linear_fit<-  model_by_sim_id %>%
    mutate(coef = map(model, tidy)) %>% 
    select (sim_id, coef) %>% 
    unnest( cols = c(coef))
  for (sim in 1:1){
    #print(sim)
    bj_tb<-list()
    accuracy=list()
    plusplus <-0
    pluszero <-0
    plusminus <-0
    zeroplus <-0
    zerozero <-0
    zerominus <-0
    minusplus <-0
    minuszero <-0
    minusminus <-0
    
    for (i in 1:24){ #i = main effect
      
      est<-linear_fit[i+1 +125*(sim-1),]['estimate']
      rl <- real_bjs[sim,i+1]
      
      if (linear_fit[i+1 +125*(sim-1),]['p.value']>0.05){
        est<-0
      }
      #print(est)
      #print(rl)
      if(rl>0 & est>0){
        plusplus <- plusplus +1
      }
      if(rl>0 & est==0){
        pluszero <- pluszero +1
      }
      if(rl>0 & est<0){
        plusminus <- plusminus +1
      }
      if(rl==0 & est>0){
        zeroplus <- zeroplus +1
      }
      if(rl==0 & est==0){
        zerozero <-zerozero +1
      }
      if(rl==0 & est<0){
        zerominus <- zerominus +1
      }
      if(rl<0 & est>0){
        minusplus <- minusplus +1
      }
      if(rl<0 & est==0){
        minuszero <- minuszero +1
      }
      if(rl<0 & est<0){
        minusminus <- minusminus +1
      }
    }
    tb=matrix(c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus),ncol=3,byrow=TRUE)
    colnames(tb) = c('+','0','-')
    rownames(tb)<-c('+','0','-')
    ftb=as.table(tb)
    accu <- (Reduce("+",c(plusplus, zerozero, minusminus)) / Reduce("+",c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus)))
    #ftb
    print(accu)
    
    bj_tb<-append(bj_tb,ftb)
    accuracy[[length(accuracy)+1]]=accu
    
  }
  #bzs
  for (sim in 1:1){
    #print(sim)
    bj_tb<-list()
    accuracy=list()
    plusplus <-0
    pluszero <-0
    plusminus <-0
    zeroplus <-0
    zerozero <-0
    zerominus <-0
    minusplus <-0
    minuszero <-0
    minusminus <-0
    
    for (i in 30:125){ #i = main effect
      
      est<-linear_fit[i +125*(sim-1),]['estimate']
      rl <- real_bzs[sim,i+1 -29]
      
      if (linear_fit[i +125*(sim-1),]['p.value']>0.05){
        est<-0
      }
      #print(est)
      #print(rl)
      if(rl>0 & est>0){
        plusplus <- plusplus +1
      }
      if(rl>0 & est==0){
        pluszero <- pluszero +1
      }
      if(rl>0 & est<0){
        plusminus <- plusminus +1
      }
      if(rl==0 & est>0){
        zeroplus <- zeroplus +1
      }
      if(rl==0 & est==0){
        zerozero <-zerozero +1
      }
      if(rl==0 & est<0){
        zerominus <- zerominus +1
      }
      if(rl<0 & est>0){
        minusplus <- minusplus +1
      }
      if(rl<0 & est==0){
        minuszero <- minuszero +1
      }
      if(rl<0 & est<0){
        minusminus <- minusminus +1
      }
    }
    tb=matrix(c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus),ncol=3,byrow=TRUE)
    colnames(tb) = c('+','0','-')
    rownames(tb)<-c('+','0','-')
    ftb=as.table(tb)
    accu <- (Reduce("+",c(plusplus, zerozero, minusminus)) / Reduce("+",c(plusplus, pluszero,plusminus,zeroplus,zerozero,zerominus,minusplus,minuszero,minusminus)))
    #ftb
    print(accu)
    
    bj_tb<-append(bj_tb,ftb)
    accuracy[[length(accuracy)+1]]=accu
    
  }
  
}
get_confusion_matrcies(trim_subset, sim_id, fo)



