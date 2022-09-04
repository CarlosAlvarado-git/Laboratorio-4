library(readr)
library(dplyr)
library(stringr)
library(highcharter) #graficas
library(plotly)
df <- read.csv('tabla_completa.csv', sep = ',')
df1 <- read.csv('tabla_completa_v1.csv', sep = ',')
df1 <- select(df1, -X.1, -X.2, -X.3)

#Marcar faltantes
df <- df %>%
  mutate(faltante = case_when(
    grepl("faltante", tolower(df$CLIENTE)) ~ 1,
    
  ))
#Marcar despacho al cliente
df <- df %>%
  mutate(despacho_cliente = case_when(
    grepl("despacho a cliente",tolower(df$CLIENTE)) ~ 1
  ))
#Marcar devolucion
df <- df %>%
  mutate(devolucion = case_when(
    grepl("devolucion",tolower(df$CLIENTE)) ~ 1
  ))
df1 <- df1 %>% mutate(faltante = df$faltante)
df1 <- df1 %>% mutate(despacho_cliente = df$despacho_cliente)
df1 <- df1 %>% mutate(devolucion = df$devolucion)
df <- df1
df$CLIENTE <- iconv(df$CLIENTE, to = 'UTF-8')
df$CLIENTE <- str_remove_all(df$CLIENTE, " ")


#---------
#Ver cuentos faltantes hay 
df %>%
  select(faltante, despacho_cliente, devolucion, CANTIDAD) %>%
  filter(faltante == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  summarise(faltantes = n(), promedio = mean(CANTIDAD))


#ver despacho a cliente
df %>%
  select(faltante, despacho_cliente, devolucion, CANTIDAD) %>%
  filter(despacho_cliente == 1) %>%
  filter(is.na(faltante)) %>%
  filter(is.na(devolucion)) %>%
  summarise(despacho_clientes = n(), promedio = mean(CANTIDAD))
#ver devolucion
df %>%
  select(faltante, despacho_cliente, devolucion, CANTIDAD) %>%
  filter(devolucion == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(faltante)) %>%
  summarise(devoluciones = n(), promedio = mean(CANTIDAD))



# Ver cuales son faltantes que fueron al despacho al cliente
df %>%
  select(faltante, despacho_cliente) %>%
  filter(faltante == 1) %>%
  filter(despacho_cliente == 1) %>%
  summarise(faltante_despacho_a_cliente = n())


# sin clasificacion/ extra
df %>%
  select(faltante, despacho_cliente, devolucion, CANTIDAD) %>%
  filter(is.na(faltante)) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  summarise(sincat = n(), promedio = mean(CANTIDAD))



# ver la cantidad de cada credito, teniendo un envio de faltante
df %>%
  select(CREDITO, faltante, despacho_cliente, devolucion) %>%
  filter(faltante == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  group_by(CREDITO) %>%
  summarise(cant = n())
# Ver cuales son faltantes que fueron al despacho al cliente, con crédito
df %>%
  select(faltante, despacho_cliente, CREDITO) %>%
  filter(faltante == 1) %>%
  filter(despacho_cliente == 1) %>%
  group_by(CREDITO) %>%
  summarise(cant = n())



#Viajes por mes
df %>%
  select(MES) %>%
  group_by(MES) %>%
  summarise(cant = n())

#Cuentos pilotos tienes
df %>%
  select(PILOTO) %>%
  summarise(cant = n_distinct(PILOTO))


#Transporte con faltante
df %>%
  select(UNIDAD, faltante, CANTIDAD, despacho_cliente, devolucion) %>%
  filter(is.na(devolucion)) %>%
  filter(is.na(faltante)) %>%
  filter(is.na(despacho_cliente)) %>%
  group_by(UNIDAD) %>%
  summarise(cant_viajes = n(),can_unidades = sum(CANTIDAD), promedio = mean(CANTIDAD)) 

#CLIENTES VIAJES
df %>%
  select(CLIENTE) %>%
  group_by(CLIENTE) %>%
  summarise(pedidos = n()) %>%
  arrange(desc(pedidos)) %>%
  hchart("column", hcaes(x = CLIENTE, y = pedidos))

#clientes con faltante
df %>%
  select(CLIENTE, faltante, despacho_cliente, devolucion) %>%
  filter(faltante == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  group_by(CLIENTE) %>%
  summarise(faltantes = sum(faltante)) %>%
  arrange(desc(faltantes))
df %>%
  select(MES, faltante, despacho_cliente, devolucion) %>%
  filter(faltante == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  group_by(MES) %>%
  summarise(faltantes = sum(faltante)) %>%
  filter(faltantes > 53) %>%
  hchart("column", hcaes(x = MES, y = faltantes))

#clientes con despacho
df %>%
  select(CLIENTE, faltante, despacho_cliente, devolucion) %>%
  filter(despacho_cliente == 1) %>%
  filter(is.na(faltante)) %>%
  filter(is.na(devolucion)) %>%
  group_by(CLIENTE) %>%
  summarise(despacho_cliente = sum(despacho_cliente)) %>%
  arrange(desc(despacho_cliente))

#clientes con devolucion
df %>%
  select(faltante, despacho_cliente, devolucion, CANTIDAD, CLIENTE) %>%
  filter(is.na(devolucion)) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(faltante)) %>%
  group_by(CLIENTE) %>%
  summarise(viajes = n()) %>% arrange(desc(viajes))

#CLIENTES SIN CAT
df %>%
  select(faltante, despacho_cliente, devolucion, CLIENTE, CANTIDAD) %>%
  filter(is.na(faltante)) %>%
  filter(is.na(despacho_cliente)) %>%
  filter(is.na(devolucion)) %>%
  group_by(CLIENTE) %>%
  summarise(sincat = n(), cant = mean(CANTIDAD))
#--------------------

#viajes por piloto:
df %>%
  select(PILOTO, UNIDAD) %>%
  group_by(PILOTO, UNIDAD) %>%
  summarise(viajes = n()) %>%
  arrange((PILOTO)) %>%
  print(n = 28)
  
  hchart("column", hcaes(x = PILOTO, y = viajes))
#viajes por piloto faltantes
df %>%
  select(PILOTO) %>%
  group_by(PILOTO) %>%
  summarise(viajes = n()) %>%
  hchart("column", hcaes(x = PILOTO, y = viajes)) %>%
  hc_title(text = "<b> Viajes por piloto </b>")
#viajes por piloto faltantes y despacho_cliente
df %>%
  select(PILOTO, faltante, despacho_cliente, devolucion) %>%
  filter(faltante == 1)%>%
  filter(despacho_cliente == 1) %>%
  filter(is.na(devolucion)) %>%
  group_by(PILOTO) %>%
  summarise(viajes = n()) %>%
  hchart("column", hcaes(x = PILOTO, y = viajes))

#viajes por ubicacion
df %>%
  select(UBICACION) %>%
  group_by(UBICACION) %>%
  summarise(n = n())

#ventas por clientes

df %>%
  select(CLIENTE, Q, devolucion) %>%
  filter(is.na(devolucion)) %>%
  group_by(CLIENTE) %>%
  summarise(ventas = sum(Q)) %>%
  arrange(desc(ventas)) %>%
  hchart("column", hcaes(x = CLIENTE, y = ventas)) %>%
  hc_title(text = "<b> Ventas por cliente </b>") %>%
  hc_subtitle(text = "<i>  </i>")

df %>%
  select(MES, faltante, despacho_cliente, devolucion) %>%
  filter(is.na(devolucion))%>%
  filter(faltante == 1) %>%
  filter(is.na(despacho_cliente)) %>%
  group_by(MES) %>%
  summarise(n = n()) %>%
  filter(n > 54)

df %>%
  select(MES) %>%
  group_by(MES) %>%
  summarise(cant = n()) %>%
  filter(cant > 198) %>%
  hchart("column", hcaes(x = MES, y = cant)) %>%
  hc_title(text = "<b> Meses con viajes por encima del promedio </b>") %>%
  hc_subtitle(text = "<i> Promedio de 198 </i>")

df %>%
  select(CREDITO, CLIENTE, devolucion) %>%
  filter(is.na(devolucion)) %>%
  filter(CREDITO == 30) %>%
  group_by(CREDITO, CLIENTE) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

df %>%
  select(Q) %>%
  summarise(n = sum(Q))
