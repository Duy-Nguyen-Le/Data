library(dplyr)
library(compareGroups)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tableone)
library(MatchIt)



setwd('C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/13. Atosiban/results')

data_path = 'C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/13. Atosiban/data/'

res_path = 'C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/13. Atosiban/results/'

df = read_excel(paste0(data_path,'Atosiban_data.xlsx'),sheet='Sheet1')

df = df %>% mutate(treatment=factor(treatment),
	beta=factor(beta),
	clinical=factor(clinical),
	ongoing_12w=factor(ongoing_12w),
	ongoing_24w=factor(ongoing_24w),
	extra_uterine=factor(extra_uterine),
	multi_preg=factor(multi_preg),
	luu=factor(luu))


m.out1 <- matchit(treatment~age, data = df, method = "nearest", distance = "logit")
m.data1 <- match.data(m.out1,distance ="pscore")
write.csv(m.data1, paste0(data_path,"matched_cohort_Atosibanage.csv"))

d = read.csv(paste0(data_path,'matched_cohort_Atosibanage.csv'),header=TRUE)

d %>% mutate(cycle_before=factor(cycle_before),
	total_l1l2_trans_before=factor(total_l1l2_trans_before))
d$inf_duration[is.na(d$inf_duration)] <- mean(d$inf_duration, na.rm = TRUE)


d$inf_duration=d$inf_duration*12
vars1 <- c("age", "bmi", "inf_duration","cycle_before","total_l1l2_trans_before","emb_trans_no","endo")
factorvars1 <- c("type_of_inf",  "cause_of_inf","ocupation")
motabn <- CreateTableOne(vars=vars1,factorVars = factorvars1,strata="treatment", data=d)
motabn <- print(motabn, quote = T, showAllLevels = T, noSpaces = T)

write.csv(motabn,file=paste0(res_path,'table1.csv'))

motabn1 <- CreateTableOne(vars=factorvars1,strata="treatment", data=d)
motabn1 <- print(motabn1, quote = T, showAllLevels = T, noSpaces = T)
write.csv(motabn1,file=paste0(res_path,'table1_factor.csv'))




vars = c("bmi","emb_trans_no","endo","type_of_inf","cause_of_inf","inf_duration","factor(cycle_before)","total_l1l2_trans_before")


univars <- data.frame()  # create an empty data frame so the rest of your code works
multilevel <- data.frame()


for (i in seq_along(vars)) {
  mod <- as.formula(sprintf("clinical ~ %s", vars[i]))  #create a formular to paste into glm()
  glmmodel <- glm(formula = mod, family = "binomial", data = d) #create glm model
  model <- summary(glmmodel)    #summary model
  if (nrow(model$coefficients)<= "2") {
    univars[i,1] <- names(coef(glmmodel))[2]    #names of variables
    univars[i,2] <- exp(glmmodel$coefficients[2])   #OR
    univars[i,3] <- exp(confint.default(glmmodel)[2,1]) #lower CI 95% value
    univars[i,4] <- exp(confint.default(glmmodel)[2,2]) #upper CI 95% value
    univars[i,5] <- model$coefficients[2,4] #p-value
  }
  else {multilevel[i,1] <- names(coef(glmmodel))[2]} #write multilevel variables into another table
  }

#define data.frame colnames
colnames(univars)[1] <- "variables"
colnames(univars)[2] <- "coefficient"
colnames(univars)[3] <- "lower"
colnames(univars)[4] <- "upper"
colnames(univars)[5] <- "p-value"

write.csv(univars,file = paste0(res_path,"univariate.csv"))


#multivariate
glmmodel <- glm(clinical~relevel(factor(d$cycle_before),ref='3'),family="binomial",data=df)
glmmodel <- summary(glmmodel)
multivariate <- data.frame()  # create an empty data frame so the rest of your code works

for (j in 1:nrow(glmmodel$coefficients)) {
    #looping again through glm model coefficients table
        multivariate[j,1] <- rownames(glmmodel$coefficients)[j]
        multivariate[j,2] <- exp(glmmodel$coefficients[j,1])
        multivariate[j,3] <- exp(glmmodel$coefficients[j,1]-qt(0.975,df=glmmodel$df[2])*glmmodel$coefficients[j,2])
        multivariate[j,4] <- exp(glmmodel$coefficients[j,1]+qt(0.975,df=glmmodel$df[2])*glmmodel$coefficients[j,2])
        multivariate[j,5] <- glmmodel$coefficients[j,4]
    }
write.table(multivariate,file=paste0(res.path,'multivar_1.tsv'),sep='\t')


p1=ggplot(df2, aes(x = attemps, y = clinical_rate)) +
    geom_bar(stat='identity',fill="black") + theme(axis.title.x = element_text(size=15),
      axis.title.y=element_text(size=20), 
      axis.text.x = element_text(size=12), axis.text.y = element_text(size=15) ,
    legend.title = element_blank(), legend.position = "bottom",
    legend.text=element_text(size=15))+
    ylab("Clinical pregnancy rate")+xlab("Number of previous failed embryo transfers")+
    theme(panel.grid = element_blank(),axis.line = element_line(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank()) + 
    scale_x_continuous(labels = df2$attemps,breaks=df2$attemps)+
    scale_y_continuous(expand = c(0,0),limit=c(0,110))+
    geom_text(aes(label=df2$clinical_rate),nudge_y = 4,size=5)

ggsave(plot=p1,"Atosiban_plot.png", width = 8, height = 4,dpi=300,device='png')

