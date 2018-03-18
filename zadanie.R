# turn on required libraries
library("tidyverse")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
# read file, add modifications if required
data = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
# delete 1st line of the frame
data = data[-1,]
data
# see information by columns
glimpse(data)
# remove unnecessary variable "roll"
data = select(data, -(roll))
# convert into factors variables such as char which contain duplicate values
data = data %>% mutate_if(is.character, factor)
# fix the problem with signs in variables
names(data) =  str_replace_all(names(data), "[!]","_emph_")
names(data) = names(data) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(data)
# leave only numerical data 
data_numeric = data[,sapply(data,is.numeric) ]
# only night time
as.integer(data$time)
time = as.integer(data$time)/3600
night = time > 0 & time < 7 | time > 20 & time < 23.6
# Create training and test samples
row_numbers = 1:length(data$time)
set.seed(655)
training = sample(row_numbers, floor(length(data$time)*.7))
test = row_numbers[-training]
teaching_data_unq = data[training,]
testing_data_unq = data[test,]

# CORRELATION ANALYSIS
cor_td = cor(data_numeric)
cor_td
# rid of all the lines where there is at least one value NA
cor_td = cor(drop_na(data_numeric))

cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude
vars
# collect all the variables from the vector with the names of the variables into one formula
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
# linear model
my.model = lm(h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                rand_err_LE + co2_flux + qc_h2o_flux + rand_err_h2o_flux + 
                h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + 
                un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + 
                v_var + w_var + h2o_var + w_div_ts_cov + w_div_co2_cov + 
                w_div_h2o_cov + flowrate, data = teaching_data_unq)
summary(my.model)
# checking
pred.model1 = predict(my.model, newdata1 = testing_data_unq)
summary(pred.model1)
# install library ithir
library(ithir)
goofcat(observed = data$h2o_flux[training], predicted = my.model)
# Checking the model on an independent sample
V.pred.my.model1 <- predict(my.model, newdata = data[-training, ])
goofcat(observed = data$h2o_flux[-training], predicted = my.model)
