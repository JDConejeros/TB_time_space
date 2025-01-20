# Code 1: TBC data preparation ----

rm(list=ls())

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Input/"
data_out <- "Output/"

# Data TBC ----

name_data <- "BDD tuberculosis PROCET 2011 al 2021.xlsx"
name_data_up <- "COHORTES TB 2015-2023.xlsx"

for(i in 2011:2021) { 
  name <- paste0("tb_", stringr::str_extract(i, pattern = "20[0-2][0-9]"))
  data <- import(paste0(data_inp, name_data), sheet=as.character(i)) |> janitor::clean_names()
  data <- data |> mutate(anio=i) |> relocate(anio)
  assign(name, data)
}

# Join all data in a list 
tablas <- do.call("list", mget(ls(pattern = "tb_*")))

# 2.1 Explorer and clean ----

glimpse(tablas)

# Observaciones
sum(sapply(tablas, nrow)) # 28406

# Variables
list(lapply(tablas, function(x){names(x)}))
list(lapply(tablas, function(x) sapply(x, class)))

# 2.2 Adjust variables -----

# Preguntar por el:
# - Método de confirmación, 
# - lugar_de_diagnostico_nivel_de_atencion 

# sexo: 2011 - 2021
map(tablas, ~ unique(.x$sexo)) # Sexo generar una dummie para las categorías FEMENINO, MASCULINO

# edad_ 2011 - 2021
map(tablas, ~ sort(unique(.x$edad))) 
# Ajustar la edad a numérica en 2012, 2013, 2014, 2015, 2016, 2018, 2019 
# Edades 0 son equivalentes a desconocidos?
# Recodificar la categoría desconocido como NA

# fecha_notificacion_eno 2011 - 2018
map(tablas, ~ sort(unique(.x$fecha_notificacion_eno))) 
map(tablas, ~ sort(unique(.x$fecha_notificacion))) # Homologar variables
# 2011 a-m-d
# 2012 d-m-a + "digitos"
# 2013 d-m-a + "digitos"
# 2014 d-m-a + "digitos"
# 2015 a-m-d
# 2016 a-m-d
# 2017 a-m-d
# 2018 a-m-d
# 2019 a-m-d
# 2020 a-m-d
# 2021 a-m-d
# Transformar todo a fecha 

# cie_10
map(tablas, ~ sort(unique(.x$cie_10))) 
# Recodificar cie_10 con categorías del excel https://www.tuotromedico.com/CIE10/Tuberculosis-A15-A19/

# comuna 
map(tablas, ~ sort(unique(.x$comuna))) 
# Tenemos comunas con vacíos que se pueden imputar
# Pegar código de comuna a partir de la librería chilemapas que ya están codificadas
# Ver si hay casos fuera del territorio nacional para ajustar los gráficos

# caso_nuevo_o_recaida 2011-2018
# caso 2019 - 2021
map(tablas, ~ sort(unique(.x$caso_nuevo_o_recaida))) 
map(tablas, ~ sort(unique(.x$caso))) 

# test_de_elisa_vih 2011-2018
# test_de_elisa 2019-2021
map(tablas, ~ sort(unique(.x$test_de_elisa_vih))) 
map(tablas, ~ sort(unique(.x$test_de_elisa))) 

# factor_de_riesgo_1 2011-2018
# riesgo_1 2019-2021
# riesgo_2 2011-2021
# riesgo_3 2011-2021
# riesgo_4 2012-2021

map(tablas, ~ sort(unique(.x$factor_de_riesgo_1))) 
map(tablas, ~ sort(unique(.x$riesgo_1))) 
map(tablas, ~ sort(unique(.x$riesgo_2))) 
map(tablas, ~ sort(unique(.x$riesgo_3))) 
map(tablas, ~ sort(unique(.x$riesgo_4))) 

# nacionalidad 2012 - 2021
map(tablas, ~ sort(unique(.x$nacionalidad))) 

# susceptibilidad 2019 - 2021
map(tablas, ~ sort(unique(.x$test_de_susceptibilidad)))  

# 2.3 Individual data -----

variables_data <- c("correlativo", "anio", "sexo", "edad", "nacionalidad", "comuna", 
                    "fecha_notificacion", "fecha_incio_tratamiento", "fecha_inicio", 
                    "cie_10", "tb1", "tb2", 
                    "caso", "test_de_elisa", "test_de_susceptibilidad",
                    "riesgo_1", "riesgo_2", "riesgo_3", "riesgo_4")

renames <- c(
  fecha_notificacion_eno = "fecha_notificacion",
  fecha_inicio_tratamiento = "fecha_inicio", 
  caso_nuevo_o_recaida = "caso",
  test_de_elisa_vih = "test_de_elisa",
  factor_de_riesgo_1 = "riesgo_1"
)

tablas_rec <- map(tablas, ~ adjust_data(.x, variables_data, renames))

list(lapply(tablas_rec, function(x){names(x)}))

# Join full data
tb_data <- bind_rows(tablas_rec)

# Recode final data 
glimpse(tb_data)

# age NA (54) y 0 (40)
tb_data <- tb_data |> 
    select(any_of(c(variables_data))) |> 
    mutate(
         sex = str_to_sentence(sexo),
         female = ifelse(sexo == "FEMENINO", 1,
                      ifelse(sexo == "MASCULINO", 0, NA_real_)),
         nationality=str_to_sentence(nacionalidad),
         nationality_cl = ifelse((nacionalidad=="CHILENA" | is.na(nacionalidad)), 1, 0),
         municipality=str_to_sentence(comuna),
         case_type=str_to_sentence(caso),
         elisa_test=str_to_sentence(test_de_elisa),
         suscept_test=str_to_sentence(test_de_susceptibilidad),
         risk_1=str_to_sentence(riesgo_1),
         risk_2=str_to_sentence(riesgo_2),
         risk_3=str_to_sentence(riesgo_3),
         risk_4=str_to_sentence(riesgo_4)
         )
  
# Edit information comuna 
data_comunas <- chilemapas::codigos_territoriales |> 
  left_join(chilemapas::mapa_comunas, by=c("codigo_region", "codigo_provincia", "codigo_comuna"))

table(tb_data$municipality, useNA = "ifany")

length(unique(tb_data$municipality))
length(unique(data_comunas$nombre_comuna))

name_com <- data.frame(cbind(data = sort(unique(tb_data$municipality)), 
                             chile= sort(unique(data_comunas$nombre_comuna))))

writexl::write_xlsx(name_com, "Input/comunas_muestra.xlsx")

# Open rec data 
name_com_rec <- rio::import("Input/rec_muni_sample.xlsx", sheet="Hoja1")

# Join data 
tb_data <- tb_data |> 
  left_join(name_com_rec, by=c("municipality"="comuna")) |> 
  left_join(data_comunas, by="nombre_comuna")

# Adjust time 2012, 2013, 2014
tb_data_full <- tb_data |> 
  mutate(
    date = if_else(str_detect(fecha_notificacion, "^\\d+$"), as.character(as.Date(as.numeric(fecha_notificacion), origin = "1899-12-30")), fecha_notificacion), 
    date = str_replace(date, "\\.", "-"),
    date = parse_date_time(date, orders = c("ymd", "dmy")),
    agno = lubridate::year(date), 
    month = lubridate::month(date), 
    week = lubridate::week(date),
    day = lubridate::day(date),
    epiweek = epiweek(date)
    #date2 = format(date, "%Y-%m-%d")
  )

## Add poblation data 

pob <- rio::import("Input/estimaciones-y-proyecciones-2002-2035-comunas.xlsx") 

poblacion <- pob |> select(5, 18:28) |> pivot_longer(cols = !Comuna, 
                            names_to = "periodo",
                            values_to = "pob") |> 
  mutate(agno=as.numeric(str_extract(periodo, pattern="\\d+"))) |> 
  rename(codigo_comuna=Comuna) |> 
  select(codigo_comuna, agno, pob)

poblacion <- poblacion |> 
  dplyr::group_by(codigo_comuna, agno) |> 
  summarise(pob=base::sum(pob, na.rm = TRUE)) |> 
  ungroup() 

tb_data_full <- tb_data_full |>
  mutate_at(c("codigo_comuna", "codigo_provincia", "codigo_region"), as.numeric)  |> 
  left_join(poblacion, by=c("agno", "codigo_comuna"))

tb_data_full <- tb_data_full |>
  mutate(age_group = 
           case_when(
             edad>=0 & edad<=18 ~ 1,
             edad>=19 & edad<=35 ~ 2,
             edad>=36 & edad<=60 ~ 3,
             edad>=61 ~ 4,
             TRUE ~ NA_real_
           ),
         age_group=factor(age_group, levels=c(1,2,3,4),
                          labels=c("1 - 18 años", 
                                   "19 - 35 años", 
                                   "35 - 60 años", 
                                   "> 60 años"))
           ) |> 
  rename(age=edad)

tb_data_full <- tb_data_full |> 
  select(correlativo, anio, sex, female, age, age_group, nationality, nationality_cl, 
         municipality, nombre_comuna, codigo_comuna, nombre_provincia, codigo_provincia, nombre_region, codigo_region, geometry,
         date, agno, month, week, day, epiweek, 
         cie_10, tb1, tb2, case_type, elisa_test, suscept_test,
         risk_1, risk_2, risk_3, risk_4
         ) |> 
  rename(name_muni=nombre_comuna, 
         cod_muni=codigo_comuna, 
         name_prov=nombre_provincia, 
         cod_prov=codigo_provincia, 
         name_reg=nombre_region, 
         cod_reg=codigo_region)



translation_dict <- c(
  "Desnutricion" = "Malnutrition",
  "Privado de libertad" = "Incarcerated",
  "Coinfeccion retroviral" = "Retroviral coinfection",
  "Diabetico" = "Diabetic",
  "Contacto" = "Contact",
  "Drogadiccion" = "Drug addiction",
  "Alcoholismo" = "Alcoholism",
  "Pueblo indigena" = "Indigenous population",
  "Extranjero" = "Foreigner",
  "Situacion de calle" = "Homeless",
  "Otra inmunosupresion" = "Other immunosuppression",
  "Personal de salud" = "Healthcare worker",
  "Residente de hogar" = "Resident of care home"
)

tb_data_full <- tb_data_full |>
  mutate(
    risk_1 = recode(risk_1, !!!translation_dict),
    risk_2 = recode(risk_2, !!!translation_dict),
    risk_3 = recode(risk_3, !!!translation_dict),
    risk_4 = recode(risk_4, !!!translation_dict)
  )

tb_data_full <- tb_data_full |> 
  mutate(first_risk = coalesce(risk_1, risk_2, risk_3, risk_4))


tb_data_full <- tb_data_full |> 
  mutate(first_risk=if_else(is.na(first_risk), "No risk factor", first_risk))

tb_data_full |>
  summarise(
    risk_1_NA = sum(is.na(risk_1)),
    risk_2_NA = sum(is.na(risk_2)),
    risk_3_NA = sum(is.na(risk_3)),
    risk_4_NA = sum(is.na(risk_4)),
    risk_f_NA = sum(is.na(first_risk))
  )

tb_data_full <- tb_data_full %>%
  mutate(across(first_risk, as.factor)) %>% # Asegurarse de que risk_1 sea categórica
  pivot_wider(
    names_from = first_risk,
    values_from = first_risk,
    values_fn = list(first_risk = ~1),
    values_fill = 0
  )

# Check missing values in date
tb_no_date <- tb_data_full |> 
  filter(is.na(date) | is.na(cod_muni)) # 1270 drop; 4.456%

tb_data_wna <- tb_data_full %>%
    drop_na(date, cod_muni) # DROP NA without information 

saveRDS(tb_data_wna, "Input/Serie_TBC_2011_2021.rds")
writexl::write_xlsx(tb_data_wna, "Input/Serie_TBC_2011_2021.xlsx")

