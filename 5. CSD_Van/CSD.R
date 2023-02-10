library(tidyverse)
library(readxl)
library(compareGroups)
library(ggplot2)
library(plyr)

#read data

setwd('C:/Users/DUY-NCKH/OneDrive/Desktop/2022/HOPE/Working/5. CSD_Van')

df <- read_xlsx("CSD_raw_030122.xlsx")

df = df %>% mutate(
	TutheTC=factor(TutheTC),
	Dichlong=factor(Dichlong),
	UXTC=factor(UXTC),
	Sonhanhphu=factor(Sonhanhphu),
	live_birth=factor(live_birth),
	u_dich=factor(u_dich),
	IVF=factor(IVF),
	THAthai=factor(THAthai),
	ChidinhMLT=factor(ChidinhMLT),
	Dathai=factor(Dathai),
	Duonghuyet=factor(Duonghuyet),
	THA=factor(THA),
	DTD=factor(DTD))

tab1 <- compareGroups(Group~age+bmi+AMH+Chan_doan+inf_duration+ChidinhMLT+Dathai+NhiemtrungVM+THAthai+DTDthai+XHTC+Thongkinh+Dauman+Daugiaohop+chan_doan+OI_IUI+IVF+live_birth+ongoing+beta+clinical+ectopic+mis+thai_bam_seo+mul+nhau_tien_dao+nhau_cai_rl+vo_TC+BHSS+u_dich+cancel+time_to_lb+ges, method = c(age=1,bmi=1,AMH=2,Chan_doan=3,inf_duration=3,ChidinhMLT=3,Dathai=3,NhiemtrungVM=3,THAthai=3,DTDthai=3,XHTC=3,Thongkinh=3,Dauman=3,Daugiaohop=3,chan_doan=3,OI_IUI=3,IVF=3,live_birth=3,ongoing=3,beta=3,clinical=3,ectopic=3,mis=3,thai_bam_seo=3,mul=3,nhau_tien_dao=3,nhau_cai_rl=3,vo_TC=3,BHSS=3,u_dich=3, cancel=3, time_to_lb=2, ges=1), data=df)

t1 <- createTable(tab1,digits = 1)

print(t1, which.table = "both")
export2csv(t1, file='table1.csv', which.table = "both")

wb1 <- df %>% filter(bw1>0) %>% select(Group, twin, bw=bw1)
wb2 <- df %>% filter(bw2>0) %>% select(Group, twin, bw=bw2)

wb <- rbind(wb1, wb2)

x <- wb %>% group_by(Group, twin) %>% summarise(mean(bw), sd(bw))

t.test(wb$bw[wb$twin==1]~wb$Group[wb$twin==1])

#regression table
vars = c("age","bmi","AMH","inf_duration","factor(CS)", "factor(Group)",
	"factor(Dathai)","factor(THAthai)","factor(DTDthai)",
	"MLT_HM_duration","minCycle","maxCycle","factor(TuoithaiMLT)",
	"factor(TutheTC)","factor(ChidinhMLT)","factor(cancel)","factor(u_dich)")


univars <- data.frame()  # create an empty data frame so the rest of your code works
multilevel <- data.frame()


for (i in seq_along(vars)) {
  mod <- as.formula(sprintf("live_birth ~ %s", vars[i]))  #create a formular to paste into glm()
  glmmodel <- glm(formula = mod, family = "binomial", data = df) #create glm model
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

#write data.frame into csv files (or tsv)
write.csv(univars,file = "univariate.csv")


univars #call to see univariate table

multilevel = multilevel %>% na.omit() #omitting NA values from multilevel list

#remove suffixes
multilevel[1,1] = "factor(TutheTC)"
multilevel[2,1] = "relevel(factor(df$ChidinhMLT),ref='1')"
# multilevel[3,1] = "cause_of_inf"

#looping through multilevel list for univariate regression
for (i in 1:nrow(multilevel)) {
    a = as.character(sprintf('multivars.%s',paste(multilevel[i,1])))    #define "a" as a character of sprintf() function
    a <- data.frame() #create empty data frames so the rest of your code works
    mod <- as.formula(sprintf("live_birth ~ %s", multilevel[i,1]))
    glmmodel <- glm(formula = mod, family = "binomial", data = df)  #create glm model
    glmmodel <- summary(glmmodel)   #summary glm model
    #looping again through glm model coefficients table
    for (j in 1:nrow(glmmodel$coefficients)) {
        a[j,1] <- rownames(glmmodel$coefficients)[j]    #variable name
        a[j,2] <- exp(glmmodel$coefficients[j,1])   #OR
        a[j,3] <- exp(glmmodel$coefficients[j,1]-qt(0.975,df=glmmodel$df[2])*glmmodel$coefficients[j,2])    #lower CI 95% value
        a[j,4] <- exp(glmmodel$coefficients[j,1]+qt(0.975,df=glmmodel$df[2])*glmmodel$coefficients[j,2])    #upper CI 95% value
        a[j,5] <- glmmodel$coefficients[j,4]    #p-value
        write.table(a,file=paste0(res.path,'univar.',paste(multilevel[i,1]),'.tsv'),sep='\t')   #write data.frame "a" into files
    }
}



#multivariate
glmmodel <- glm(live_birth~age+MLT_HM_duration+u_dich,family="binomial",data=df)
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

#live birth rate regression
tab1 <- compareGroups(live_birth~Group+age+
	bmi+AMH+Chan_doan+inf_duration+ChidinhMLT+
	Dathai+NhiemtrungVM+THAthai+DTDthai+XHTC+
	Thongkinh+Dauman+Daugiaohop+chan_doan+
	OI_IUI+IVF+ongoing+beta+clinical+ectopic+
	mis+thai_bam_seo+mul+nhau_tien_dao+nhau_cai_rl+
	vo_TC+BHSS+u_dich+cancel+time_to_lb+ges, method = c(age=1,
		bmi=1,AMH=2,Chan_doan=3,inf_duration=3,ChidinhMLT=3,Dathai=3,
		NhiemtrungVM=3,THAthai=3,DTDthai=3,XHTC=3,Thongkinh=3,Dauman=3,
		Daugiaohop=3,chan_doan=3,OI_IUI=3,IVF=3,ongoing=3,beta=3,
		clinical=3,ectopic=3,mis=3,thai_bam_seo=3,mul=3,nhau_tien_dao=3,
		nhau_cai_rl=3,vo_TC=3,BHSS=3,u_dich=3, cancel=3, time_to_lb=2, ges=1), data=df)

t1 <- createTable(tab1,digits = 1)

print(t1, which.table = "both")
export2csv(t1, file='table4.csv', which.table = "both")


#graph
df <- read_xlsx("CSD_graph.xlsx")
df$type[df$type=='loCTC']="Distance between niche \n and external os"
df$type[df$type=='BQAD']="Distance between niche \n and vesicovaginal fold"
df$type[df$type=='maxr']="Largest width"
df$type[df$type=='dayr']="Width"
df$type[df$type=='maxl']="Largest length"
df$type[df$type=='dayl']="Length"
df$type[df$type=='d']="Depth"


df$type = factor(df$type, levels = c("Distance between niche \n and external os",
 "Distance between niche \n and vesicovaginal fold", "AMT", "RMT", "Largest width", "Width", "Largest length",
 "Length","Depth"))



p_meds2 <- ddply(df, .(type),function(x) quantile(x$value))
colnames(p_meds2)=c("type","min","first","med","third","max")
p_meds2$first=round(p_meds2$first,digits=1)
p_meds2$med=round(p_meds2$med,digits=1)
p_meds2$third=round(p_meds2$third,digits=1)

p1=ggplot(df, aes(x = value, y = type)) +
    geom_boxplot(notch=TRUE,aes(fill=type),width=0.55,notchwidth=0.5) + 
    theme(axis.title.x = element_text(size=20,family='Arial'),
      axis.title.y=element_text(size=27), 
      axis.text.x = element_text(size=18), axis.text.y = element_text(size=20) ,
    legend.title = element_blank(), legend.position = "bottom",
    legend.text=element_text(size=20),
    legend.key=element_rect(fill='white'))+
    xlab("Value (mm)")+xlim(0,60)+ylab("")+
    theme(panel.grid = element_blank(),axis.line = element_line(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank()) + 
    # scale_x_discrete()+
    # scale_y_discrete()+
    geom_text(family="Arial",
    	data = p_meds2, aes(x = med, y = type,
    	label = med),
    	size = 5.5, vjust = -1.6,fontface="bold")+
    geom_text(family="Arial",
    	data = p_meds2, aes(x = first, y = type,
    	label = first),
    	size = 5.5, vjust = -0.9,hjust=1.5)+
    geom_text(family="Arial",
    	data = p_meds2, aes(x = third, y = type,
    	label = third),
    	size = 5.5, vjust = -0.9,hjust=-0.5)

p1

ggsave(plot=p1,"CSD.png", width = 20, height = 10,dpi=300,device='png')