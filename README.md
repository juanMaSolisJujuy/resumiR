
# {resumiR}

<!-- badges: start -->
<!-- badges: end -->

Permite obtener rápidamente una serie de medidas de resumen y gráficos
estadísticos para datos numéricos discretos o continuos en series
simples.

También permite obtener tablas de frecuencia clásicas y gráficos cuando
se desea realizar un análisis de series agrupadas.

Su objetivo es la aplicación didáctica de funciones de Estadística
Descriptiva en un curso introductorio de Bioestadística utilizando el
software R, para las carreras de grado y otras ofertas educativas de la
Faculad de Ciencias Agrarias de la UNJu.

## Ejemplo de aplicación

``` r
library(resumiR)

x = iris$Sepal.Length
set.seed(12345)
x[sample(10)] = NA

# Medidas de resumen y boxplot para series simples

s_simple(
  x,
  decimales = 2,
  recorte = 0.05,
  boxplot = T
)

# Tabla de frecuencias y gráficos para series agrupadas

s_agrupada(
  x,
  li = 4,
  ls = 8,
  a = 1,
  grafico = "fs"
)
```
