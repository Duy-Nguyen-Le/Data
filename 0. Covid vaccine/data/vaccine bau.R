library(tidyverse)
library(readxl)
library(compareGroups)
library(Hmisc)

setwd("~/Desktop/Submitted/vacine bầu/data")


x <- cut2(df$birthweight1, g = 4)

#load data
df <- read_excel("covid_vaccine_20220127.xlsx")


#table 1
resu2 <- compareGroups(first_vaccine ~ age  + previous_preg + type_of_preg + vaccination + gest_at_vaccine + gest_hbp_b + gest_diabetes_b + inject_site_pain_swell + inject_site_redskin + inject_site_itchy + fatigue + throat_nose + coughing + headache + sore_muscle + chilled + fever_38deg + nausea + arthralgia + stomach +diarrhea+rashes+haemorrh+haemoptysis+thrombo+myocard+ anaphylactic + inject_site_pain_swell_2 + inject_site_redskin_2 + inject_site_itchy_2 + fatigue_2 + throat_nose_2 + coughing_2 + headache_2 + sore_muscle_2 + chilled_2 + fever_38deg_2 + nausea_2 + arthralgia_2 + stomach_2 +diarrhea_2+rashes_2+haemorrh_2+haemoptysis_2+thrombo_2+myocard_2+ anaphylactic_2 + gest_hbp_a+gest_diabetes_a+ gest_at_birth+ges_L28+ges_L34+ges_L37+oligohydramnios+polyhydramnios+ICU + still_birth + birthweight1+low_birthweight+Heavy_bw+bw_percentile+L_10_per+nicu+defects, method = c(age = 1, previous_preg =3, type_of_preg =3, vaccination =3, gest_at_birth =1, gest_hbp_b =3, gest_diabetes_b =3, inject_site_pain_swell =3, inject_site_redskin =3, inject_site_itchy =3, fatigue =3, throat_nose =3, coughing =3, headache =3, sore_muscle =3, chilled =3, fever_38deg =3, nausea =3, arthralgia =3, stomach =3,diarrhea=3,rashes=3,haemorrh=3,haemoptysis=3,thrombo=3,myocard=3, anaphylactic =3, inject_site_pain_swell_2 =3, inject_site_redskin_2 =3, inject_site_itchy_2 =3, fatigue_2 =3, throat_nose_2 =3, coughing_2 =3, headache_2 =3, sore_muscle_2 =3, chilled_2 =3, fever_38deg_2 =3, nausea_2 =3, arthralgia_2 =3, stomach_2 =3,diarrhea_2=3,rashes_2=3,haemorrh_2=3,haemoptysis_2=3,thrombo_2=3,myocard_2=3, anaphylactic_2 =3,gest_hbp_a =3 ,gest_diabetes_a =3 , gest_at_birth=1,ges_L28=3,ges_L34=3,ges_L37=3,oligohydramnios=3,polyhydramnios=3,ICU =3, still_birth =3, birthweight1=1,low_birthweight=3,Heavy_bw=3,bw_percentile=2,L_10_per=3,nicu=3,defects=3), data = df)

t1 <- createTable(resu2, digits = 1, hide = c(gest_hbp_b ="0", gest_diabetes_b ="0", inject_site_pain_swell ="0", inject_site_redskin ="0", inject_site_itchy ="0", fatigue ="0", throat_nose ="0", coughing ="0", headache ="0", sore_muscle ="0", chilled ="0", fever_38deg ="0", nausea ="0", arthralgia ="0", stomach ="0",diarrhea="0",rashes="0",haemorrh="0",haemoptysis="0",thrombo="0",myocard="0", anaphylactic ="0", inject_site_pain_swell_2 ="0", inject_site_redskin_2 ="0", inject_site_itchy_2 ="0", fatigue_2 ="0", throat_nose_2 ="0", coughing_2 ="0", headache_2 ="0", sore_muscle_2 ="0", chilled_2 ="0", fever_38deg_2 ="0", nausea_2 ="0", arthralgia_2 ="0", stomach_2 ="0",diarrhea_2="0",rashes_2="0",haemorrh_2="0",haemoptysis_2="0",thrombo_2="0",myocard_2="0", anaphylactic_2 ="0",gest_hbp_a ="0" ,gest_diabetes_a ="0" , gest_at_birth=1,ges_L28="0",ges_L34="0",ges_L37="0",oligohydramnios="0",polyhydramnios="0",ICU ="0", still_birth ="0", low_birthweight="0",Heavy_bw="0",L_10_per="0",nicu="0",defects="0"), sd.type = 2)

print(t1, which.table = "both")
export2csv(t1, file='table1.csv')

p <- df %>% ggplot(aes(x=first_vaccine, y=birthweight1, fill=first_vaccine)) + 
  geom_boxplot(fatten = NULL) + 
  xlab("")+ylab("Birth weight") + 
  annotate("text", x = 0.95, y = 2700, label = "P10") + 
  annotate("text", x = 0.95, y = 2945, label = "P25") + 
  annotate("text", x = 0.95, y = 3160, label = "P50") + 
  annotate("text", x = 0.95, y = 3445, label = "P75") + 
  annotate("text", x = 0.95, y = 3600, label = "P90") + 
  annotate("text", x = 1.95, y = 2700, label = "P10") + 
  annotate("text", x = 1.95, y = 2945, label = "P25") + 
  annotate("text", x = 1.95, y = 3160, label = "P50") + 
  annotate("text", x = 1.95, y = 3445, label = "P75") + 
  annotate("text", x = 1.95, y = 3600, label = "P90") + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text= element_text(size = 13),
        legend.text = element_text(size = 13),
        text = element_text(size = 14))
p

p <- df %>% ggplot(aes(x=first_vaccine, y=bw_percentile, fill=first_vaccine)) + 
  geom_boxplot(fatten = NULL) + 
  xlab("")+ylab("Birthweight percentile") + 
 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text= element_text(size = 13),
        legend.text = element_text(size = 13),
        text = element_text(size = 14))
p

bp.vals <- function(x, probs=c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  r <- quantile(x, probs=probs , na.rm=TRUE)
 # r = c(r[1:2], exp(mean(log(x))), r[3:4])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
  }
bp.vals(df$birthweight1)


p <- df %>% ggplot(aes(x=first_vaccine, y=birthweight1, fill=first_vaccine)) + 
  stat_summary(fun.data=bp.vals, geom="boxplot") + xlab("")+ylab("Birth weight") + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) + 
  scale_y_continuous(breaks = c(0, 2700, 2900, 3160, 3400, 3600, 4500))
p
ggsave("birthweight_percentile.png", dpi=300,  width = 9, height = 7)

df$ges_group = df$gest_at_birth

df$ges_group[df$gest_at_birth<34] = "<34"
df$ges_group[df$gest_at_birth>=34&df$gest_at_birth<37] = "34-<37"
df$ges_group[df$gest_at_birth>=37&df$gest_at_birth<38] = "37-<38"
df$ges_group[df$gest_at_birth>=38&df$gest_at_birth<39] = "38-<39"
df$ges_group[df$gest_at_birth>=39&df$gest_at_birth<40] = "39-<40"
df$ges_group[df$gest_at_birth>=40&df$gest_at_birth<41] = "40-<41"
df$ges_group[df$gest_at_birth>=41] = "41"

table(df$ges_group)

df$bw_percentile_g = df$bw_percentile

df$bw_percentile_g[df$bw_percentile<10] = "<10"
df$bw_percentile_g[df$bw_percentile>=10&df$bw_percentile<25] = "10-<25"
df$bw_percentile_g[df$bw_percentile>=25&df$bw_percentile<50] = "25-<50"
df$bw_percentile_g[df$bw_percentile>=50&df$bw_percentile<75] = "50-<75"
df$bw_percentile_g[df$bw_percentile>=75&df$bw_percentile<90] = "75-<90"
df$bw_percentile_g[df$bw_percentile>=90] = ">=90"

df$bw_percentile_g = factor(df$bw_percentile_g, levels = c("<10", "10-<25", "25-<50", "50-<75", "75-<90", ">=90"))
table(df$bw_percentile_g)

x1 <- df %>% filter(birthweight1>0) %>% group_by(first_vaccine, ges_group) %>% summarise(N = n())

write.csv(x1, "x1.csv")

d <- read.csv("x.csv", header = T)

d$ges_group=factor(d$ges_group, levels = c("<34","34-<37","37-<38", "38-<39", "39-<40", "40-<41", "41"))
d$bw_percentile_g = factor(d$bw_percentile_g, levels = c("Stillbirth","<10", "10-<25", "25-<50", "50-<75", "75-<90", ">=90"))

p <- ggplot(d, aes(fill=bw_percentile_g, y=rate, x=ges_group)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  xlab("Gestational age at delivery (weeks)") +
  ylab("Birth weight percentile rate (%)") +
  facet_wrap(~first_vaccine) +
  theme(#legend.position = "top", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text= element_text(size = 13),
        legend.text = element_text(size = 13),
        text = element_text(size = 14))+
  scale_fill_manual(values = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#9400D3"),
                    name="Birth weight percentile",
                    breaks=c("Stillbirth","<10", "10-<25", "25-<50", "50-<75", "75-<90", ">=90"),
                    labels=c("Stillbirth","<10", "10-<25", "25-<50", "50-<75", "75-<90", "\u226590"))
 # scale_fill_manual(values=c("#08d1f3", "#1cc4df", "#2fb6cc", "#42a8b9", "#569aa5", "#698c92", "#7c7e7f"))
  

p
ggsave("bwp_GA.png", p, dpi=300, width = 15, height = 7)

library(epitools)

m <- matrix(c(441-6,6,513-8,8), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-22,22,513-26,26), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-53,53,513-56,56), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-175,175,513-164,164), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-145,145,513-203,203), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-39,39,513-55,55), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-1,1,513-1,1), nrow=2, byrow=T); riskratio(m)

m <- matrix(c(441-2,2,513-1,1), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-16,16,513-26,26), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-47,47,513-59,59), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-114,114,513-128,128), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-115,115,513-138,138), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-93,93,513-99,99), nrow=2, byrow=T); riskratio(m)
m <- matrix(c(441-54,54,513-62,62), nrow=2, byrow=T); riskratio(m)


#Univariate analysis

df$G[df$first_vaccine=="AstraZeneca"]=0
df$G[df$first_vaccine=="Pfizer BioNTech"]=1
df$G = factor(df$G)
df$low_birthweight = factor(df$low_birthweight)
df$previous_preg2=df$previous_preg
df$previous_preg2[df$previous_preg>=3]=3
df$mother_covid_2 = factor(df$mother_covid_2)

resu3 <- compareGroups(first_vaccine ~ G + age + previous_preg2 + type_of_preg + vaccination+mother_covid_2, method = c(age = 1, previous_preg2 =3, type_of_preg =3, vaccination =3, G=3, mother_covid_2=3), data=df)

t3 <- createTable(resu3, digits = 1, show.ratio = T)


print(t3, which.table = "both")
export2csv(t3, file='table3.csv')

library(tableone)

m1 <- glm(low_birthweight~G+vaccination, family = "binomial", data=df)

m <- ShowRegTable(m1)
write.csv(m,"multivariate.csv")




