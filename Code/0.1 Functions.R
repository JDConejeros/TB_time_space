
# Adjust data files 

adjust_data <- function(df, columnas, columnas_rename) {
  df |>
    # rename(fecha_notificacion=fecha_notificacion_eno,
    #        caso=caso_nuevo_o_recaida,
    #        test_de_elisa=test_de_elisa_vih,
    #        riesgo_1=factor_de_riesgo_1
    # ) |>
    rename_at(vars(any_of(names(renames))),
              ~ renames[.]) %>%
    mutate(edad = as.numeric(edad),
           fecha_notificacion=as.character(fecha_notificacion), 
           fecha_inicio=as.character(fecha_inicio), 
           fecha_notificacion=if_else(is.na(fecha_notificacion), fecha_inicio, fecha_notificacion),
           tb1 = case_when(
            str_detect(cie_10, "A15+") ~ "Respiratory TBC",  
            str_detect(cie_10, "A16+") ~ "Respiratory TBC",  
            str_detect(cie_10, "A17+") ~ "TBC of the nervous system",  
            str_detect(cie_10, "A18+") ~ "TBC of other organs",  
            str_detect(cie_10, "A19+") ~ "Miliary TBC", 
            TRUE ~ NA_character_
           ), 
           tb2  = case_when(
            cie_10 == "A15.0" ~ "TBC of the lung",
            cie_10 == "A15.1" ~ "TBC of the lung",
            cie_10 == "A15.2" ~ "TBC of the lung",
            cie_10 == "A15.3" ~ "TBC of the lung",
            cie_10 == "A15.4" ~ "TBC of intrathoracic lymph nodes",
            cie_10 == "A15.5" ~ "TBC of larynx, trachea, and bronchi",
            cie_10 == "A15.6" ~ "Tuberculous pleurisy",
            cie_10 == "A15.7" ~ "Primary respiratory TBC",
            cie_10 == "A15.8" ~ "Other respiratory TBC",
            cie_10 == "A15.9" ~ "Unspecified respiratory TBC",
            cie_10 == "A16.0" ~ "TBC of the lung",
            cie_10 == "A16.1" ~ "TBC of the lung",
            cie_10 == "A16.2" ~ "TBC of the lung",
            cie_10 == "A16.3" ~ "TBC of intrathoracic lymph nodes",
            cie_10 == "A16.4" ~ "TBC of larynx, trachea, and bronchi",
            cie_10 == "A16.5" ~ "Tuberculous pleurisy",
            cie_10 == "A16.7" ~ "Primary respiratory TBC",
            cie_10 == "A16.8" ~ "Other respiratory TBC",
            cie_10 == "A16.9" ~ "Unspecified respiratory TBC",
            cie_10 == "A17.0" ~ "Tuberculous meningitis",
            cie_10 == "A17.1" ~ "Meningeal tuberculoma",
            cie_10 == "A17.8" ~ "Other TBC of the nervous system",
            cie_10 == "A17.9" ~ "TBC of the nervous system",
            cie_10 == "A18.0" ~ "TBC of bones and joints",
            cie_10 == "A18.1" ~ "TBC of the genitourinary system",
            cie_10 == "A18.2" ~ "Tuberculous peripheral lymphadenopathy",
            cie_10 == "A18.3" ~ "TBC of intestines, peritoneum, and mesenteric lymph nodes",
            cie_10 == "A18.4" ~ "TBC of skin and subcutaneous tissue",
            cie_10 == "A18.5" ~ "TBC of the eye",
            cie_10 == "A18.6" ~ "TBC of the ear",
            cie_10 == "A18.7" ~ "TBC of adrenal glands",
            cie_10 == "A18.8" ~ "TBC of other specified organs",
            cie_10 == "A19.0" ~ "Acute miliary TBC",
            cie_10 == "A19.1" ~ "Acute miliary TBC",
            cie_10 == "A19.2" ~ "Acute miliary TBC",
            cie_10 == "A19.8" ~ "Acute miliary TBC",
            cie_10 == "A19.9" ~ "Acute miliary TBC",            
            TRUE ~ NA_character_
           ) 
           ) |>
    select(any_of(columnas))
}

data1921_process <- function(df) {
  df |>
    mutate(fecha_diagnostico=as.character(fecha_diagnostico), 
           fecha_diagnostico=if_else(is.na(fecha_notificacion), fecha_diagnostico, fecha_notificacion))
}
