library(fastDummies)
library(dplyr)
library(tidyr)
library(MASS)

cat("\014")  
rm(list=ls())

coil<-read.csv("C:/Users/NichodemusGedion/Downloads/S45.csv")
str(coil)

converted<-coil %>% 
mutate(
      across
      (
      starts_with('P'), ~case_when
         (
         .x == 0 ~ 0,
         .x == 1 ~ 25,
         .x == 2 ~ 75,
         .x == 3 ~ 150,
         .x == 4 ~ 350,
         .x == 5 ~ 750,
         .x == 6 ~ 3000,
         .x == 7 ~ 7500,
         .x == 8 ~ 15000,
         .x == 9 ~ 30000,
         TRUE ~ -99
         )
      )
   )

converted <- converted%>% relocate (MOSHOO, .after = SeqNum)
converted <- converted%>% relocate (MOSTYP, .after = SeqNum)
converted <- converted%>% relocate (MAANTH, .after = SeqNum)
converted <- converted%>% relocate (MGEMOM, .after = SeqNum)
converted <- converted%>% relocate (MGEMLE, .after = SeqNum)
converted <- converted%>% relocate (MGODRK, .after = MOSHOO)
#Do this for all variables starting with M that should not be converted using L3 format
str(converted)


# Are MGODRK:MSKC the first and last variables that need to be converted using L3 format?
# In not, adjust
converted<-converted %>% 
   mutate(across(MGODRK:MSKA, ~case_when(
       .x == 0 ~ 0,
       .x == 1 ~ 5,
       .x == 2 ~ 17,
       .x == 3 ~ 30,
       .x == 4 ~ 43,
       .x == 5 ~ 56,
       .x == 6 ~ 69,
       .x == 7 ~ 82,
       .x == 8 ~ 94,
       .x == 9 ~ 100,
#more changes here
       TRUE ~ -99
     )))

head(converted)
table(converted$PWAPAR, coil$PWAPAR)

set.seed(1)
	
converted <- dummy_cols(converted, select_columns = c('MOSTYP', 'MOSHOO'),remove_selected_columns = TRUE)
#WHAT OTHER VARIABLE NEEDS TO BE CONVERTED TO BINARY.  CAN ADD TO LINE ABOVE ('MOSTYPE','Other Variable'...)
#  AND RE-RUN PROGRAM FROM BEGINNING 


train <- converted %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(converted, train, by = 'SeqNum')

train<-subset(train,select=-c(SeqNum))
test <-subset(test,select=-c(SeqNum))



fullModel = glm(Resp ~ ., family = 'binomial', data = train) # model with all variables
nullModel = glm(Resp ~ 1, family = 'binomial', data = train) # model with the intercept only

interim<-summary(stepAIC(nullModel, # start with a model containing no variables
                direction = 'forward', # run forward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection

coef<-data.frame(interim[['coefficients']])
final<-coef[coef$Pr...z..<0.05,]
print(final)

varnames<-rownames(final)
varnames<-varnames[2:length(varnames)]
print(varnames)
#Type in the variables produced by line above.  DO NOT SIMPLY USE THE VARIABLES IN LINE BELOW
finalmodel<-glm(Resp ~ MOSHOO_3+MOSTYP_10+AWAPAR+MFALLE+MOSTYP_41+MOSHOO_10+MOSTYP_1+MOSHOO_9+MINKGE, family = 'binomial', data = train)

ForMemo<-lm(Resp ~ MOSHOO_3+MOSTYP_10+AWAPAR+MFALLE+MOSTYP_41+MOSHOO_10+MOSTYP_1+MOSHOO_9+MINKGE, data = train)
summary(ForMemo)

test$pred<-predict(finalmodel,newdata=test,type="response")
test<-test[order(-test$pred),]
test$one<-1
test$cumprospects<-cumsum(test$one)
test$cumresp    <-cumsum(test$Resp)

head(test)

Perf<-subset(test,select=c(pred,cumprospects,cumresp))
Perf$PctProspect<-Perf$cumprospects/nrow(Perf)
Perf$PctResp    <-Perf$cumresp/max(Perf$cumresp)

cutpoint<-subset(Perf,PctProspect>0.745 & PctProspect<0.755)

print(cutpoint)

#Change pathname

#Open in Excel, if have time

Perf$Lift<-Perf$PctResp-Perf$PctProspect
MaxLift<-Perf[Perf$Lift==max(Perf$Lift),]
MaxLift

write.csv(Perf,"C:/R/IDS462/Perf.csv")








