# Волков Иван создайте модель множественной линейной регрессии потоков дневных потоков паров воды за осенний период 2013 
# года по данным измерений методом турбулентной пульсации

library("tidyverse")  #целая вселенная
library("readr")      #функция read_csv()
library("stringr")    #функция str_replace_all
library("dplyr")      #функции: filter(); arrange(); select(); mutate(); summarize(); group_by(); sample_n()
library("ggplot2")    #Графики функцией qplot()

#подключаем библиотеки
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")    

#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))
#преобразуем строковые значения в факторные
eddypro = eddypro %>% mutate_if(is.character, factor)
#заменяем конфликтующие знаки колонок
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

glimpse(eddypro)
# уберем na
eddypro = drop_na(eddypro)
#осенний период, дневное время
eddypro = filter(eddypro, DOY >= 245 & DOY < 335)
eddypro = filter(eddypro, daytime==TRUE)
#переменные типов numeric и  не numeric отдельно
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
#создадим выборки
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]
# МОДЕЛЬ 1 по обучающей выборке
mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)

#коэффициенты
coef(mod1)
#остатки
resid(mod1)
#доверительный интервал
confint(mod1)
#P-значения по модели
summary(mod1)
#дисперсионный анализ
anova(mod1)
#графическое представление модели:
plot(mod1)

# МОДЕЛЬ 2
mod2 = lm ( h2o_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + qc_h2o_flux + rand_err_co2_flux
            + co2_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction + h2o_mixing_ratio + sonic_temperature + air_temperature
            + air_pressure + air_density + air_heat_capacity + air_molar_volume + e + es 
            + specific_humidity + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
            + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + L + L_z_minus_dL__div_L
            + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_70_perc_ + un_Tau 
            + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + w_spikes + co2_spikes 
            + h2o_spikes + u_var + v_var + co2_var + h2o_var + w_div_h2o_cov + co2 + h2o 
            + co2_signal_strength_7200 + flowrate, data = teaching_tbl)

coef(mod2)
resid(mod2)
confint(mod2)
summary(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2) 


# МОДЕЛЬ 3
mod3 = lm ( h2o_flux~ DOY + file_records + Tau + qc_Tau + rand_err_Tau + H
            + qc_H + rand_err_H + LE + qc_LE + rand_err_LE + qc_h2o_flux
            + rand_err_co2_flux + co2_flux + rand_err_h2o_flux+ H_strg
            + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density
            + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density, data = teaching_tbl)


coef(mod3)
resid(mod3)
confint(mod3)
summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

cor_teaching_tbl = select(teaching_tbl, DOY, file_records, Tau, qc_Tau, rand_err_Tau, H,
                          qc_H, rand_err_H, LE, qc_LE, rand_err_LE, qc_co2_flux,
                          rand_err_co2_flux, co2_flux, rand_err_h2o_flux, H_strg,
                          co2_v_minus_adv, h2o_v_minus_adv, h2o_molar_density,
                          h2o_mole_fraction, h2o_mixing_ratio, co2_molar_density)

cor_td = cor(cor_teaching_tbl) %>% as.data.frame

qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

