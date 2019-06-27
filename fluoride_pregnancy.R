# fluoride levels in pregnant women analyses 
library(readxl)
library(dplyr)

# read in data 

df <- read_excel('/Users/danagoin/Documents/Fluoride and pregnant women/Data.xlsx')

df <- df %>% rename(smoker=`Smoker?`, water_fluoride=`Water Fluoride`, mat_urine = `maternal urine F MEAN (ppm)`, 
                    amniotic_fluid = `amniotic fluid MEAN (ppm)`, serum_fluoride = `Serum Fluoride (ppm)`)

df$amniotic_fluid <- as.numeric(df$amniotic_fluid)
