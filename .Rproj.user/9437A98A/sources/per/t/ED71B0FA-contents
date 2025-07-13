datos = readxl::read_excel("INEGI Trabajo 28-06-2025/madres_jovenes.xlsx", sheet = 1)
datos_avance = datos


municipios = sf::read_sf("../../Lalo/Escuela/Gob/Importantes_documentos_usar/Municipios/municipiosjair.shp")


datos_limpios = datos |>  
  dplyr::filter(!is.na(Latitud))

mapa_primero = sf::st_as_sf(x = datos_limpios, coords = c("Longuitud", "Latitud"), crs = sf::st_crs(municipios))


library(leaflet)
library(leaflet.extras)
mapa_web = leaflet() |> 
  addTiles() |> 
  addHeatmap(data = datos_limpios, lat = datos_limpios$Latitud, lng = datos_limpios$Longuitud)

mapa_web





datos$LOC_RESID[datos$LOC_RESID == 9999] = 1
datos$LOC_RESID |>  class()


datos = datos |> 
  dplyr::mutate(CVEGEO_Nueva = paste0(sprintf("%02d", ENT_RESID), sprintf("%03d", MUN_RESID), sprintf("%04d", LOC_RESID)))

datos = datos |> 
  dplyr::select(IND_MUN:CVEGEO, CVEGEO_Nueva, Latitud:Frecuencia) |> 
  dplyr::select(-CVEGEO)


coordenadas = read.csv("INEGI Trabajo 28-06-2025/Coordenadas/AGEEML_2025514316256.csv", fileEncoding = "latin1")
coordenadas_corte = coordenadas |> 
  dplyr::mutate(CVEGEO = sprintf("%09d", CVEGEO)) |> 
  dplyr::select(CVEGEO, LON_DECIMAL, LAT_DECIMAL)



datos = merge(x = datos, y = coordenadas_corte, by.x = "CVEGEO_Nueva", by.y = "CVEGEO", all.x = T)
datos = datos |>  dplyr::select(CVEGEO_Nueva:Longuitud, LAT_DECIMAL,LON_DECIMAL, EDAD_MADN:Frecuencia) 

datos = datos |>  dplyr::select(-Latitud,-Longuitud)
datos = datos |>  dplyr::select(-EDAD_PADREN)
datos$EDAD_PADN[datos$EDAD_PADN == "99"] = NA
datos = datos |>  dplyr::select(-DIF_EDAD)




poblacion_total = read.csv("INEGI Trabajo 28-06-2025/Población Nacional/Pob_nacional20CSV.csv")
poblacion = poblacion_total |> 
  dplyr::filter(ENTIDAD !=0, MUN != 0, LOC != 0)

poblacion = poblacion |> 
  dplyr::filter(LOC != 9999)

poblacion = poblacion |> 
  dplyr::select(ENTIDAD, MUN, LOC, POBTOT, POBFEM, P_8A14_F, P_15A17_F, PROM_HNV, PCLIM_PMEN, PCATOLICA:PSIN_RELIG, PROM_OCUP)

poblacion[poblacion == "*"] = NA
poblacion = poblacion |>  dplyr::mutate(CVEGEO = paste0(sprintf("%02d", ENTIDAD), sprintf("%03d", MUN), sprintf("%04d", LOC))) |> 
  dplyr::select(CVEGEO, ENTIDAD:PROM_OCUP)



#### Remplazar apartir del directorio
datos = datos |>  dplyr::select(-Frecuencia)



Tipo_nac_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Simple",
                "2" = "Doble",
                "3" = "Triple o más",
                "4" = "No especificado",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$TIPO_NAC = sapply(datos$TIPO_NAC, Tipo_nac_remplazar, simplify = T, USE.NAMES = F)

Orden_part_remplazar = function(str) {
  return(switch(as.character(str),
                "99" = "No especificado",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$ORDEN_PART = sapply(datos$ORDEN_PART, Orden_part_remplazar, simplify = T, USE.NAMES = F)


Edociv_mad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Soltera",
                "2" = "Casada",
                "3" = "Unión libre",
                "4" = "Separada",
                "5" = "Divorciada",
                "6" = "Viuda",
                "9" = "No especificado",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$EDOCIV_MAD = sapply(datos$EDOCIV_MAD, Edociv_mad_remplazar, simplify = T, USE.NAMES = F)



Escol_mad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Sin escolaridad",
                "2" = "De 1 a 3 años de primaria",
                "3" = "De 4 a 5 años de primaria",
                "4" = "Primaria completa",
                "5" = "Secundaria o equivalente",
                "6" = "Preparatoria o equivalente",
                "7" = "Profesional",
                "8" = "Otra",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$ESCOL_MAD = sapply(datos$ESCOL_MAD, Escol_mad_remplazar, simplify = T, USE.NAMES = F)




Escol_pad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Sin escolaridad",
                "2" = "De 1 a 3 años de primaria",
                "3" = "De 4 a 5 años de primaria",
                "4" = "Primaria completa",
                "5" = "Secundaria o equivalente",
                "6" = "Preparatoria o equivalente",
                "7" = "Profesional",
                "8" = "Otra",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$ESCOL_PAD = sapply(datos$ESCOL_PAD, Escol_pad_remplazar, simplify = T, USE.NAMES = F)


Act_mad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Trabaja",
                "2" = "No trabaja",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$ACT_MAD = sapply(datos$ACT_MAD, Act_mad_remplazar, simplify = T, USE.NAMES = F)




Act_pad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Trabaja",
                "2" = "No trabaja",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$ACT_PAD = sapply(datos$ACT_PAD, Act_pad_remplazar, simplify = T, USE.NAMES = F)




Sitlab_mad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Tiene trabajo o está buscando",
                "2" = "Estudiante",
                "3" = "Dedicado a quehaceres del hogar",
                "4" = "Jubilado o pensionado",
                "5" = "Incapacitado permanentemente para trabajar",
                "6" = "Otra",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}
datos$SITLAB_MAD = sapply(datos$SITLAB_MAD, Sitlab_mad_remplazar, simplify = T, USE.NAMES = F)



Sitlab_pad_remplazar = function(str) {
  return(switch(as.character(str),
                "1" = "Tiene trabajo o está buscando",
                "2" = "Estudiante",
                "3" = "Dedicado a quehaceres del hogar",
                "4" = "Jubilado o pensionado",
                "5" = "Incapacitado permanentemente para trabajar",
                "6" = "Otra",
                "9" = "No especificada",
                str  # valor por defecto si no hay coincidencia
  ))
}

datos$SITLAB_PAD = sapply(datos$SITLAB_PAD, Sitlab_pad_remplazar, simplify = T, USE.NAMES = F)


library(writexl)
write_xlsx(datos, "datos_madres_jovenes.xlsx")















































































write_xlsx(poblacion, "poblacion_totales_para_madres_jovenes.xlsx")


######################
### Segunda Parte ####
######################


municipios_mexico = sf::read_sf("../../Lalo/Escuela/Gob/Importantes_documentos_usar/Cartografia_Nacional/Nacional Total/conjunto_de_datos/00mun.shp")
plot(municipios_mexico$geometry)

poblacion[, c(5:ncol(poblacion))] = lapply(poblacion[, c(5:ncol(poblacion))], as.numeric)


poblaciones_municipales = poblacion |> 
  dplyr::group_by(ENTIDAD,MUN) |>  
  dplyr::summarise(Poblacion_total = sum(POBTOT, na.rm = T), Poblacion_total_Femenina = sum(POBFEM, na.rm = T),
                   Poblacion_total_Femenina_11A17 =sum(P_8A14_F, na.rm = T), Poblacion_total_Femenina_18A20 = sum(P_15A17_F, na.rm = T))

poblaciones_municipales = poblaciones_municipales |> 
  dplyr::mutate(CVEGEO = paste0(sprintf("%02d", ENTIDAD), sprintf("%03d", MUN))) |> 
  dplyr::select(-ENTIDAD,-MUN)

poblaciones_municipales = poblaciones_municipales |> dplyr::ungroup() |> 
  dplyr::select(CVEGEO, Poblacion_total:Poblacion_total_Femenina_18A20)


municipios_mexico = merge(x = municipios_mexico |>  dplyr::select(CVEGEO,NOMGEO,geometry), y = poblaciones_municipales, by.x= "CVEGEO", by.y = "CVEGEO", all.x = T)

municipios_mexico = municipios_mexico |> 
  dplyr::mutate(Poblacion_total_Femenina_11A20 = Poblacion_total_Femenina_11A17 + Poblacion_total_Femenina_18A20)

municipios_mexico = municipios_mexico |> 
  dplyr::select(CVEGEO:Poblacion_total_Femenina_18A20, Poblacion_total_Femenina_11A20, geometry)


datos = datos |>  
  dplyr::mutate(CVEGEO_MUN = substr(x = CVEGEO_Nueva, start = 1, stop = 5))



contar_datos = datos |> dplyr::filter(EDAD_MADN != 10) |> 
  dplyr::group_by(CVEGEO_MUN) |> 
  dplyr::summarise(conteo = dplyr::n())



municipios_mexico = merge(x = municipios_mexico, y = contar_datos, by.x ="CVEGEO", by.y = "CVEGEO_MUN", all.x = T)
names(municipios_mexico)[8] = "Conteo_nacimientos"

municipios_mexico = municipios_mexico |> 
  dplyr::mutate(Porcentaje_nacimientos = (Conteo_nacimientos/Poblacion_total_Femenina_11A20)*100,
                Porcentaje_libre = 100-Porcentaje_nacimientos)

municipios_mexico = municipios_mexico |> 
  dplyr::select(CVEGEO:Conteo_nacimientos, Porcentaje_nacimientos, Porcentaje_libre) |> 
  dplyr::arrange(dplyr::desc(Porcentaje_nacimientos))


municipios_mexico_mapa = sf::st_transform(x = municipios_mexico, crs = sf::st_crs(municipios))
municipios_mexico_mapa = municipios_mexico_mapa |> 
  dplyr::mutate(area = sf::st_area(x = municipios_mexico_mapa) |>  as.numeric()) |> 
  dplyr::arrange(dplyr::desc(area), dplyr::desc(Porcentaje_nacimientos))



municipios_mexico_mapa$Porcentaje_nacimientos |>  hist()


nombres_entidades = sf::read_sf("../../Lalo/Escuela/Gob/Importantes_documentos_usar/Cartografia_Nacional/Nacional Total/conjunto_de_datos/00ent.shp")
nombres_entidades = nombres_entidades |>  sf::st_drop_geometry() |>  dplyr::select(CVEGEO,NOMGEO)

municipios_mexico_mapa = municipios_mexico_mapa |>  dplyr::mutate(CVEGEO_ENT = substr(x = CVEGEO,start = 1,stop = 2))

municipios_mexico_mapa = merge(x = municipios_mexico_mapa, y = nombres_entidades, by.x = "CVEGEO_ENT", by.y = "CVEGEO", all.x = T)
municipios_mexico_mapa = municipios_mexico_mapa |> 
  dplyr::select(CVEGEO,NOMGEO.y,NOMGEO.x:area, geometry)
colnames(municipios_mexico_mapa)[2:3] = c("NOM_ENT", "NOM_MUN")

municipios_mexico_mapa = municipios_mexico_mapa |> 
  dplyr::arrange(CVEGEO)


sf::write_sf(municipios_mexico_mapa, "INEGI Trabajo 28-06-2025/Nacimientos/Nacimientos_porcentaje_geometria.shp")
write.csv(municipios_mexico_mapa |>  sf::st_drop_geometry(), "INEGI Trabajo 28-06-2025/Nacimientos/Nacimientos_porcentaje.csv", fileEncoding = "latin1")


stats::kmeans(x = municipios_mexico_mapa$Poblacion_total_Femenina, iter.max = 5)

mapa_web = leaflet() |> 
  addTiles() |>
  addPolygons(data = municipios_mexico_mapa, popup = municipios_mexico_mapa$Porcentaje_nacimientos, label = municipios_mexico_mapa$NOMGEO)

mapa_web





datos = read.csv("INEGI Trabajo 28-06-2025/Nacimientos/Nacimientos_porcentaje.csv", fileEncoding = "latin1")
datos = datos |> dplyr::select(-X)
write.csv(datos, "INEGI Trabajo 28-06-2025/Nacimientos/Nacimientos_porcentaje_utf8.csv", fileEncoding = "UTF-8", row.names = F)

library(writexl)

write_xlsx(datos, "INEGI Trabajo 28-06-2025/Nacimientos/Nacimientos_porcentaje_excel.xlsx")
