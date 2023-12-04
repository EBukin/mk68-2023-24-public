
# Variable name | Description|
# ---------|---------|
# year	  |year in which data is collected
# sex	    |male = 1|
# indig	  |indigenous = 1|
# dist_sec |nearest distance to a secondary school|
# sc	      |enrolled in school in year of survey|
# grc      |grade enrolled|
# fam_n    |family size|
# min_dist |	min distance to an urban center|
# dist_cap |	min distance to the capital|
# poor     |	poor = 1|
# progresa |treatment =1|
# hohedu	  |years of schooling of head of household|
# hohwag	  |monthly wages of head of household|
# welfare_index|	welfare index used to classify poor|
# hohsex	  |gender of head of household (male=1)|
# hohage   |age of head of household|
# age      |years old|
# folnum	 |individual id|
# village  |	village id|
# sc97	  |schooling in 1997|
# 

pgs <- read_csv("data-raw/progressa/progresa_sample.csv")

names(pgs)

glimpse(pgs)

pgs %>% count(year)


fit1 <- lm(welfare_index  ~ progresa + hohsex + fam_n + hohedu + hohwag, 
           data = pgs%>% filter(year == 98))
summary(fit1)


fit2 <- lm(sc ~ progresa, data = pgs %>% filter(year == 98))
summary(fit2)
