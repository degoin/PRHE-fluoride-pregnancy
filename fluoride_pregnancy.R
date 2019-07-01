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

df_l <- df %>% gather(key="measure",value="concentration", water_fluoride, 
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

fit1 <- glm(mat_urine ~ water_fluoride + smoker + Age, data=df)

fit2 <- glm(amniotic_fluid ~ water_fluoride + smoker + Age, data=df)

fit3 <- glm(serum_fluoride ~ water_fluoride + smoker + Age, data=df)

cor_mat <- data.frame(round(cor(df %>% select(water_fluoride, mat_urine, amniotic_fluid, serum_fluoride), use="pairwise.complete.obs"),2))
rownames(cor_mat) <- colnames(cor_mat) <- c("Water","Maternal urine","Amniotic fluid","Maternal serum")
write.csv(cor_mat, file="/Users/danagoin/Documents/Fluoride and pregnant women/PRHE-fluoride-pregnancy/correlation_matrix.csv")


all_df <- read.csv('/Users/danagoin/Documents/Fluoride and pregnant women/questionnaire_fmt.csv')
                   

test <- left_join(all_df, df)
                   
                   ")