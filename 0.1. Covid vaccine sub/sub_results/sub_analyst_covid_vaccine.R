library(tidyverse)
library(readxl)
library(compareGroups)
library(Hmisc)
library(ggplot2)
library(epitools)

setwd('C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/0.1. Covid vaccine sub/sub_results')

res_path = 'C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/0.1. Covid vaccine sub/sub_results/'

data_path = 'C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/0.1. Covid vaccine sub/data/'

df = read_excel(paste0(data_path,'covid_vaccine_20220127.xlsx'))

df = df %>% select("age","previous_preg","type_of_preg","multi_preg","first_dose","second_dose","gest_at_birth",
	"ges_L28","ges_L34","ges_L37","gest_at_vaccine","birthweight1","temp","99th","97th","95th","90th","75th","mean","25th",
	"10th","5th","3rd","1st","bw_percentile","L_10_per","alive","still_birth","gest_hbp_b","gest_hbp_a","HATT","HATTr",
	"gest_at_hbp","gest_diabetes_b","gest_diabetes_a","gest_at_diabetes","gest_hemorr","gest_at_hemorr","preterm_birth",
	"gest_at_preterm","oligohydramnios","polyhydramnios","vaccination","first_vaccine","ICU","second_vaccine",
	"nicu","days_at_nicu","obstruct","defects","iugr_gest","gest_at_iugr","mother_covid","mother_covid_2", "labor_type","low_birthweight","Heavy_bw","birth_weight") %>%
	mutate(multi_preg=factor(multi_preg),
		ges_L28=factor(ges_L28),
		ges_L34=factor(ges_L34),
		ges_L37=factor(ges_L37),
		L_10_per=factor(L_10_per),
		alive=factor(alive),
		still_birth=factor(still_birth),
		oligohydramnios=factor(oligohydramnios),
		polyhydramnios=factor(polyhydramnios),
		ICU=factor(ICU),
		nicu=factor(nicu),
		obstruct=factor(obstruct),
		defects=factor(defects),
		iugr_gest=factor(iugr_gest),
		mother_covid_2=factor(mother_covid_2),
		low_birthweight=factor(low_birthweight),
		Heavy_bw=factor(Heavy_bw))



# #reduce the sample size based on age
# library(MatchIt)
# m.out1 <- matchit(mother_covid_2~age, data = df, method = "nearest", distance = "logit")
# m.data1 <- match.data(m.out1,distance ="pscore")
# write.csv(m.data1, "matched_cohort1.csv")

# d = read.csv(paste0(data_path,'matched_cohort1.csv',header=TRUE))

#reduce the sample size based on age and previous pregnancies
library(MatchIt)
m.out1 <- matchit(mother_covid_2~age+factor(previous_preg), data = df, method = "nearest", distance = "logit")
m.data1 <- match.data(m.out1,distance ="pscore")
write.csv(m.data1, paste0(data_path,"matched_cohort_prev-preg.csv"))

d = read.csv(paste0(data_path,'matched_cohort_prev-preg.csv'),header=TRUE)



#table 1
tab2 <- compareGroups(mother_covid_2 ~ age  + previous_preg + type_of_preg + vaccination + first_vaccine+second_vaccine+ gest_at_vaccine + gest_hbp_b + gest_diabetes_b + 
	gest_hbp_a+gest_diabetes_a+ gest_at_birth+ges_L28+ges_L34+ges_L37+oligohydramnios+polyhydramnios+ICU +multi_preg+ still_birth + birthweight1+low_birthweight+
	Heavy_bw+bw_percentile+L_10_per+nicu+defects, 
	method = c(age = 1, previous_preg =3, type_of_preg =3, vaccination =3, gest_at_birth =1, gest_hbp_b =3, gest_diabetes_b =3, gest_hbp_a =3 ,gest_diabetes_a =3 , 
		gest_at_birth=1,ges_L28=3,ges_L34=3,ges_L37=3,oligohydramnios=3,polyhydramnios=3,ICU =3, still_birth =3, birthweight1=1,low_birthweight=3,Heavy_bw=3,
		bw_percentile=2,L_10_per=3,nicu=3,defects=3), data = df)

t1 <- createTable(tab2, digits = 1, 
	hide = c(gest_hbp_b ="0", gest_diabetes_b ="0",
		gest_hbp_a ="0" ,
		gest_diabetes_a ="0" ,
		gest_at_birth=1,ges_L28="0",
		ges_L34="0",ges_L37="0",
		oligohydramnios="0",
		polyhydramnios="0",
		ICU ="0", still_birth ="0",
		low_birthweight="0",
		Heavy_bw="0",
		L_10_per="0",
		nicu="0",
		defects="0"), 
	sd.type = 2)

print(t1, which.table = "both")
export2csv(t1, file='table1.csv')


#table 2
library(epitools)

#RR 95% CI
m <- matrix(c(73-2,2,73,0), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-0,0,73-2,2), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-1,1,73-0,0), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-4,4,73-0,0), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-8,8,73-5,5), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-1,1,73-6,6), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-2,2,73-4,4), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-0,0,73-2,2), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-3,3,73-2,2), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-0,0,73-1,1), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-1,1,73-4,4), nrow=2, byrow=T); riskratio(m); 
m <- matrix(c(73-0,0,73-1,1), nrow=2, byrow=T); riskratio(m); 


#between group difference 95% CI for ratios
prop.test(c(2,0),c(73,73))
prop.test(c(0,2),c(73,73))
prop.test(c(1,0),c(73,73))
prop.test(c(4,0),c(73,73))
prop.test(c(8,5),c(73,73))
prop.test(c(1,6),c(73,73))
prop.test(c(2,4),c(73,73))
prop.test(c(0,2),c(73,73))
prop.test(c(3,2),c(73,73))
prop.test(c(0,1),c(73,73))
prop.test(c(1,4),c(73,73))
prop.test(c(0,1),c(73,73))

#between group difference 95% CI for means
t.test(m.data1$gest_at_birth[m.data1$mother_covid_2==1],m.data1$gest_at_birth[m.data1$mother_covid_2==0])
t.test(m.data1$birthweight1[m.data1$mother_covid_2==1],m.data1$birthweight1[m.data1$mother_covid_2==0])



#vaccination categorized
df$vacc_group = df$gest_at_vaccine

df$vacc_group[df$gest_at_vaccine<29.43] = "First_quartile"
df$vacc_group[df$gest_at_vaccine>=29.43&df$gest_at_vaccine<32.71] = "Second_quartile"
df$vacc_group[df$gest_at_vaccine>=32.71&df$gest_at_vaccine<35.43] = "Third_quartile"
df$vacc_group[df$gest_at_vaccine>=35.43] = "Fourth_quartile"

table(df$vacc_group)

#
df.az = df %>% filter(first_vaccine=='AstraZeneca')
df.pfi = df %>% filter(first_vaccine=='Pfizer BioNTech')

#
tab3 <- compareGroups(vacc_group ~ age  + previous_preg + type_of_preg + vaccination + first_vaccine+second_vaccine+ gest_at_vaccine + gest_hbp_b + gest_diabetes_b + 
	gest_hbp_a+gest_diabetes_a+ gest_at_birth+ges_L28+ges_L34+ges_L37+oligohydramnios+polyhydramnios+ICU + multi_preg+ still_birth + birthweight1+low_birthweight+
	Heavy_bw+bw_percentile+L_10_per+nicu+defects+mother_covid_2, 
	method = c(age = 1, previous_preg =3, type_of_preg =3, vaccination =3, gest_at_birth =1, gest_hbp_b =3, gest_diabetes_b =3, gest_hbp_a =3 ,gest_diabetes_a =3 , 
		gest_at_birth=1,ges_L28=3,ges_L34=3,ges_L37=3,oligohydramnios=3,polyhydramnios=3,ICU =3, still_birth =3, birthweight1=1,low_birthweight=3,Heavy_bw=3,
		bw_percentile=2,L_10_per=3,nicu=3,defects=3,mother_covid_2=3), data = df)

t2 <- createTable(tab3, digits = 1, 
	hide = c(gest_hbp_b ="0", gest_diabetes_b ="0",
		gest_hbp_a ="0" ,
		gest_diabetes_a ="0" ,
		gest_at_birth=1,ges_L28="0",
		ges_L34="0",ges_L37="0",
		oligohydramnios="0",
		polyhydramnios="0",
		ICU ="0", still_birth ="0",
		low_birthweight="0",
		Heavy_bw="0",
		L_10_per="0",
		nicu="0",
		defects="0"), 
	sd.type = 2)

print(t2, which.table = "both")
export2csv(t2, file='table2_noncovid.csv')

df.firstquart = df %>% filter(vacc_group=='First_quartile')

tab4 <- compareGroups(ges_L28~first_vaccine+gest_diabetes_b,data=df.firstquart,method=3)

t3 = createTable(tab4, digits = 1,sd.type=2)
print(t3,which.table='both')


#####
vars = c("age","relevel(factor(type_of_preg),ref='Natural')","vaccination","gest_diabetes_a","gest_hbp_a","gest_hbp_b","gest_diabetes_b","mother_covid_2")


univars <- data.frame()  # create an empty data frame so the rest of your code works
multilevel <- data.frame()


for (i in seq_along(vars)) {
  mod <- as.formula(sprintf("ges_L37 ~ %s", vars[i]))  #create a formular to paste into glm()
  glmmodel <- glm(formula = mod, family = "binomial", data = df.az) #create glm model
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

univars

write.csv(univars,file = paste0(res_path,"univariate.csv"))


#multivariate
glmmodel <- glm(ges_L28~age+relevel(factor(type_of_preg),ref='Natural')+gest_at_vaccine,family="binomial",data=df)
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
