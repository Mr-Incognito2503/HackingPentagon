library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1, 1:77]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
#names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric)]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
tbl_numeric <- drop_na(tbl_numeric)
names(tbl_numeric)
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7))
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3))
# set to logical type column daytime
tbl$daytime = as.logical(tbl$daytime)
# daytime/nighttime = T/F
tbl = subset(tbl, daytime == F)
mod = lm(h2o_flux ~ (rand_err_h2o_flux + h2o_strg + h2o_v_minus_adv + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag)^2 , data = tbl_numeric)
summary(mod)
resid(mod)
coef(mod)
names(tbl_numeric)
qplot(h2o_flux, DOY, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
qplot(h2o_flux, predict(mod), data = tbl_numeric, geom = "line")
qplot(h2o_mixing_ratio, h2o_flux, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
#lm(earn ~ . - age, data = wages)
anova(mod)
