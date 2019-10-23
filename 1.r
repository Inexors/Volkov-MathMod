# Волков Иван – для региона 3 рассчитайте урожайность пшеницы в период с 1999 по 2003 
# год взяв для рассчета средние суммы активных температур за эти годы, 
# с 12 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца, 
# когда среднедневные температуры были выше 8 градусов, но учитывая, что посев не может начаться раньше середины апреля, 
# а вегетация составляет 3 месяца
# 	Республика Бурятия - 51.823515, 107.590539 Улан-Удэ

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
# Константы
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры
lat = 51.823515; lon = 107.590539

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# Получим список метеостанций
ulan_ude = data.frame(id = "ULAN-UDE", latitude = lat,  longitude = lon)
#найдем станции, соответствующие критериям
ulan_ude_around = meteo_nearby_stations(lat_lon_df = ulan_ude, station_data = station_data,
                                         limit = 12, var = "TAVG", 
                                         year_min = 1999, year_max = 2003)
# Создадим таблицу
all_data = tibble()
for (i in 1:12)
{
  # Определим станцию из 7 ближайших:
  ulan_ude_id = ulan_ude_around[["ULAN-UDE"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = ulan_ude_id,
                          var="TAVG",
                          date_min="1999-01-01",
                          date_max="2003-12-31")
  # Занесем данные в таблицу, объединив их:
  all_data = bind_rows(all_data, data)
}
# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  # Добавим колонку month для группировки данных:
  mutate(month=month(date)) %>%
  group_by(month, id) %>%
  # Найдем месячный d и cумму активных температур для каждой станции:
  mutate(d = length(tavg[tavg>80])/length(tavg)) %>%
  summarise(tavg = sum(tavg[tavg>50])/10, d = mean(d)) %>%
  # Сгруппировав данные по месяцам,
  group_by(month) %>%
  # найдем средние активные температуры и d для каждого месяца:
  summarise(s = mean(tavg, na.rm = TRUE), d = mean(d)) %>%
  filter(month>=4 & month <=7) %>%
  # Добавим колонки для расчета:
  mutate (a = af[4:7], b = bf[4:7]) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Согласно расчету, урожайность пшеницы в Бурятии в 1999-2003 составила:
Yield = sum(clean_data$fert); Yield