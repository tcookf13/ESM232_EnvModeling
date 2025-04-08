library(tidyverse)
library(here)

source(here("~/Bren MESM/ESM 232 ENV Modeling/ESM232_EnvModeling/R/solarpv.R"))
solarpv

# read in R formatted data
load(here("data/sierraczosolar.rda"))

# already in the format required for the model
head(sierraczosolar)

# remove the first year from the dataset
sierraczosolar <- sierraczosolar %>%
  filter(year > 1944)


# plot
# lets make months names rather than labels

sierraczosolar$month <- factor(sierraczosolar$month, levels = 1:12, labels = month.abb)

ggplot(sierraczosolar, aes(x = month, y = (Kdown_direct+Kdown_diffuse), fill=month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Solar Radiation (W/m2)") +
  theme_bw()



# run the model
solarpv(area = 0.1, solar = sierraczosolar, 
        clr = "green", eunit = "W", g=TRUE)

# run and save results - but don't plot
site1 <- solarpv(area = 0.1, solar = sierraczosolar, 
                 clr = "green", eunit = "W", g = FALSE)
site1$mean
site1$annual


#rerun the model with different parameters
#etype is direct, and a non standard efficiency - changed to 0.15
site2 <- solarpv(area = 0.1, solar = sierraczosolar, 
                 clr = "green", eunit = "W", g = TRUE,
                 eff = 0.15, etype = "direct")
site2$mean
site2$annual

#######################

# the rest of this code is in lecture3_buildingmodels.qmd 



