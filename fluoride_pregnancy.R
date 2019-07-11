rm(list=ls())
# fluoride levels in pregnant women analyses 
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

# read in data 

df <- read_excel('/Users/danagoin/Documents/Fluoride and pregnant women/Data.xlsx')

df <- df %>% rename(smoker=`Smoker?`, water_fluoride=`Water Fluoride`, mat_urine = `maternal urine F MEAN (ppm)`, 
                    amniotic_fluid = `amniotic fluid MEAN (ppm)`, serum_fluoride = `Serum Fluoride (ppm)`)

df$amniotic_fluid <- as.numeric(df$amniotic_fluid) 

df$ppt_id <- df$SAMPLE


# read in WOC data to merge on covariates 

df_meiosis_chart <- read.csv('/Users/danagoin/Documents/Fluoride and pregnant women/Meiosis_Chart/meiosis_chart.csv')

df_m <- left_join(df, df_meiosis_chart)
# there are two people whose ages differ in these two sources 

# insurance 
# 0 - Medi-Cal 
# 1 - Private / HMO
# 2 - Self-Pay 
# 3 - Medi-Care 
# 9 - Missing 



df_m$race_eth <- factor(ifelse(df_m$ethnicity___1==1,"Latina", 
                        ifelse(df_m$ethnicity___2==1,"Black", 
                               ifelse(df_m$ethnicity___3==1,"White", 
                                      ifelse(df_m$ethnicity___4==1,"Asian/PI", 
                                             ifelse(df_m$ethnicity___5==1,"Other",NA))))), 
                        levels=c("Latina","Black","White","Asian/PI","Other"), ordered=T)


# assuming grav means gravity and para means parity, but not sure 
# smoker and smoking ipv are the same
# drugs 
# 0 - Never 
# 1 - Former 
# 2 - Current 
# 9 - Missing

# drug substance 
# 1 - crack, cocaine
# 2 - meth 
# 3 - heroin 
# 4 - other 
# 9 - missing

# ethnicity 
# 1 - Latina 
# 2 - Black 
# 3 - White 
# 4 - Asian/PI 
# 5 - Other 
# 9 - Missing

# language 
# 0 - English 
# 1 - Spanish 
# 2 - Other 
# 9 - Missing 

# country of origin 
# 0 - USA 
# 1 - Other 
# 9 - Missing 


# add on Meiosis BPA data for education variable 

df_meiosis_bpa <- read.csv('/Users/danagoin/Documents/Fluoride and pregnant women/Meiosis_BPA/meiosis_bpa.csv')

df_meiosis_bpa <-  df_meiosis_bpa %>%  select(ppt_id, edu)

df_m <- left_join(df_m, df_meiosis_bpa, by="ppt_id")

df_m$drug_type <- ifelse(df_m$drug_substance___1==1,1, 
                         ifelse(df_m$drug_substance___2==1,2, 
                                ifelse(df_m$drug_substance___3==1,3, 
                                       ifelse(df_m$drug_substance___4==1,4, NA))))

df_m$mat_educ <- factor(ifelse(df_m$edu<5, "Less than high school", 
                        ifelse(df_m$edu==5 | df_m$edu==6, "High school/GED", 
                               ifelse(df_m$edu==7, "Some college", 
                                      ifelse(df_m$edu>7, "College grad or postgrad", NA)))), 
                        levels=c("Less than high school", "High school/GED", 
                                 "Some college", "College grad or postgrad"), ordered = T)


df_m$parity <- ifelse(df_m$para<5,df_m$para, ifelse(df_m$para==5,4,NA))
# education 
# 1 - 8th grade or less 
# 2 - 9th grade 
# 3 - 10th grade 
# 4 - 11th grade 
# 5 - 12th grade
# 6 - GED 
# 7 - Some college 
# 8 - College grad 
# 9 - Graduate degree 
# -7 - I don't know 
# -8 - Refused

# recoded educ 
# 1 - less than high school 
# 2 - high school or GED 
# 3 - some college 
# 4 - college grad or postgraduate 


# select variables to keep 
df_m <- df_m %>% select(ppt_id, zipcode, Age, age, race_eth, mat_educ, edu, insurance, gest_multiple, 
                        gest_us_w, gest_us_d, grav, para, bmi, smoker, smokingipy, 
                        drugs, drug_type, marital_status, language, country, water_fluoride, mat_urine, 
                        serum_fluoride, amniotic_fluid)


# unadjusted results 
fit1 <- glm(mat_urine ~ water_fluoride, data=df_m)
round(coef(fit1),2)
round(confint(fit1),2)

fit2 <- glm(serum_fluoride ~ water_fluoride, data=df_m)
round(coef(fit2),2)
round(confint(fit2),3)

fit3 <- glm(amniotic_fluid ~ water_fluoride, data=df_m)
round(coef(fit3),2)
round(confint(fit3),2)


# adjusted results 
fit1 <- glm(mat_urine ~ water_fluoride + smoker + Age  + bmi, data=df_m)
qqnorm(residuals(fit1))
qqline(residuals(fit1))

round(coef(fit1),2)
round(confint(fit1),2)



# see whether BMI has nonlinear relationship with fluoride levels 

ggplot(df_m, aes(bmi, mat_urine)) + geom_point() + geom_smooth(span=1, method="loess")
ggplot(df_m, aes(bmi, serum_fluoride)) + geom_point() + geom_smooth(span=1, method="loess")
ggplot(df_m, aes(bmi, amniotic_fluid)) + geom_point() + geom_smooth(span=1, method="loess")

# see whether age has nonlinear relationship with fluoride levels 

ggplot(df_m, aes(Age, mat_urine)) + geom_point() + geom_smooth(span=1, method="loess")
ggplot(df_m, aes(Age, serum_fluoride)) + geom_point() + geom_smooth(span=1, method="loess")
ggplot(df_m, aes(Age, amniotic_fluid)) + geom_point() + geom_smooth(span=1, method="loess")

fit2 <- glm(serum_fluoride ~ water_fluoride + smoker + Age + bmi, data=df_m)
qqnorm(residuals(fit2))
qqline(residuals(fit2))

round(coef(fit2),2)
round(confint(fit2),2)


fit3 <- glm(amniotic_fluid ~ water_fluoride + smoker + Age + bmi, data=df_m)
qqnorm(residuals(fit3))
qqline(residuals(fit3))

round(coef(fit3),2)
round(confint(fit3),2)


cor_mat <- data.frame(round(cor(df %>% select(water_fluoride, mat_urine, serum_fluoride, amniotic_fluid), use="pairwise.complete.obs"),2))
rownames(cor_mat) <- colnames(cor_mat) <- c("Water","Maternal urine","Maternal serum", "Amniotic fluid")
write.csv(cor_mat, file="/Users/danagoin/Documents/Fluoride and pregnant women/PRHE-fluoride-pregnancy/correlation_matrix.csv")


# reorganize to make box plots
df_l <- df_m %>% gather(key="measure",value="concentration", water_fluoride, 
                      mat_urine, amniotic_fluid, serum_fluoride)


ggplot(df_l, aes(x=measure, y=concentration)) + geom_boxplot()

p1 <- ggplot(df_l %>% filter(measure==c("mat_urine","water_fluoride")), aes(x=measure, y=concentration)) + 
    theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
    scale_x_discrete(labels=c("Maternal urine","Water"))

p2 <- ggplot(df_l %>% filter(measure==c("serum_fluoride","amniotic_fluid")), aes(x=measure, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Maternal serum","Amniotic fluid"))

ggsave(p1, file="/Users/danagoin/Documents/Fluoride and pregnant women/PRHE-fluoride-pregnancy/water_urine_fluoride_ppm.pdf", width=8)

ggsave(p2, file="/Users/danagoin/Documents/Fluoride and pregnant women/PRHE-fluoride-pregnancy/serum_amniotic_fluoride_ppm.pdf", width=8)


# see if fluoride levels differ meaningfully by race/ethnicity 
ggplot(df_l %>% filter(measure=="mat_urine"), aes(x=race_eth, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Latina","Black/African American","White","Asian/Pacific Islander"))

ggplot(df_l %>% filter(measure=="serum_fluoride"), aes(x=race_eth, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Latina","Black/African American","White","Asian/Pacific Islander"))

ggplot(df_l %>% filter(measure=="amniotic_fluid"), aes(x=race_eth, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Latina","Black/African American","White","Asian/Pacific Islander"))


# see if fluoride levels differ meaningfully by educational attainment 
ggplot(df_l %>% filter(measure=="mat_urine"), aes(x=mat_educ, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Less than high school","High school grad/GED","Some college","College grad/postgraduate"))

ggplot(df_l %>% filter(measure=="serum_fluoride"), aes(x=mat_educ, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Less than high school","High school grad/GED","Some college","College grad/postgraduate"))

ggplot(df_l %>% filter(measure=="amniotic_fluid"), aes(x=mat_educ, y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("Less than high school","High school grad/GED","Some college","College grad/postgraduate"))


# see if fluoride levels differ meaningfully by parity
ggplot(df_l %>% filter(measure=="mat_urine"), aes(x=factor(parity), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Parity",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("0","1","2","3","4+"))

ggplot(df_l %>% filter(measure=="serum_fluoride"), aes(x=factor(parity), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Parity",y="Mean concentration (ppm)") +
  scale_x_discrete(labels=c("0","1","2","3","4+"))

ggplot(df_l %>% filter(measure=="amniotic_fluid"), aes(x=factor(parity), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Parity",y="Mean concentration (ppm)") + 
  scale_x_discrete(labels=c("0","1","2","3","4+"))


# see if fluoride levels differ meaningfully by gravidity
ggplot(df_l %>% filter(measure=="mat_urine"), aes(x=factor(grav), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Gravidity",y="Mean concentration (ppm)")

ggplot(df_l %>% filter(measure=="serum_fluoride"), aes(x=factor(grav), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Gravidity",y="Mean concentration (ppm)")

ggplot(df_l %>% filter(measure=="amniotic_fluid"), aes(x=factor(grav), y=concentration)) + 
  theme_bw()  + geom_boxplot() + labs(x="Gravidity",y="Mean concentration (ppm)")

# make table of descriptive statistics 

df_m %>% group_by(race_eth) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 

df_m %>% group_by(mat_educ) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N))

df_m %>% group_by(parity) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N))

df_m  %>% summarise(mean=mean(Age, na.rm = T), sd=sqrt(var(Age, na.rm=T)), min=min(Age, na.rm=T), max=max(Age, na.rm=T))

df_m  %>% summarise(mean=mean(bmi, na.rm = T), sd=sqrt(var(bmi, na.rm=T)), min=min(bmi, na.rm=T), max=max(bmi, na.rm=T))

# differences between fluoridated (0.3 or above) and non-fluoridated community water

df_m$fluoridated_cm <- ifelse(df_m$water_fluoride>0.3,1,0)

# mean - sd

# water fluoride 
df_m %>% group_by(fluoridated_cm) %>% summarise(mean=mean(water_fluoride, na.rm=T), sd=sqrt(var(water_fluoride, na.rm=T)))
# urine fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(mean=mean(mat_urine, na.rm=T), sd=sqrt(var(mat_urine, na.rm=T)))
# serum fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(mean=mean(serum_fluoride, na.rm=T), sd=sqrt(var(serum_fluoride, na.rm=T)))
# amniotic fluid fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(mean=mean(amniotic_fluid, na.rm=T), sd=sqrt(var(amniotic_fluid, na.rm=T)))

# min - max

# water fluoride 
df_m %>% group_by(fluoridated_cm) %>% summarise(min=min(water_fluoride, na.rm=T), max=max(water_fluoride, na.rm=T))
# urine fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(min=min(mat_urine, na.rm=T), max=max(mat_urine, na.rm=T))
# serum fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(min=min(serum_fluoride, na.rm=T), max=max(serum_fluoride, na.rm=T))
# amniotic fluid fluoride
df_m %>% group_by(fluoridated_cm) %>% summarise(min=min(amniotic_fluid, na.rm=T), max=max(amniotic_fluid, na.rm=T))


# t-tests for difference in means between fluorinated and non-fluorinated communities 

# urine 
x <- df_m[df_m$fluoridated_cm==0, "mat_urine"]
y <- df_m[df_m$fluoridated_cm==1, "mat_urine"]

t.test(x$mat_urine,y$mat_urine)

# serum 
x <- df_m[df_m$fluoridated_cm==0, "serum_fluoride"]
y <- df_m[df_m$fluoridated_cm==1, "serum_fluoride"]

t.test(x$serum_fluoride,y$serum_fluoride)


# amniotic fluid 
x <- df_m[df_m$fluoridated_cm==0, "amniotic_fluid"]
y <- df_m[df_m$fluoridated_cm==1, "amniotic_fluid"]

t.test(x$amniotic_fluid,y$amniotic_fluid)

