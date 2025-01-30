# Import necessary functions
#' @importFrom stats quantile var sd setNames
#' @importFrom grDevices boxplot.stats
#' @importFrom utils write.table
#' @import tibble
#' @import ggplot2

# GlobalVariables function
utils::globalVariables(c("fi"))
utils::globalVariables("count")

#' @name s_simple
#' @aliases s_simple
#' @title Medidas de resumen univariadas en series numéricas simples
#' @description
#' Devuelve medidas de resumen univariadas de los datos numéricos ingresados, discretos o continuos, como series simples. Permite graficar la serie por medio de un gráfico de cajas y bigotes.
#' @param x vector numérico de tipo *integer* o *numeric* (puede incluir NA's o valores infinitos).
#' @param decimales el número de posiciones decimales de los resultados; por defecto es 1.
#' @param recorte proporción de recorte unilateral de la serie ordenada para la estimación de la media recortada; el máximo valor admitido es 0.25;  por defecto 0.1.
#' @param percentiles vector numérico de probabilidades acumuladas para el cálculo de percentiles; por defecto \{0.25, 0.5, 0.75\}
#' @param boxplot lógico; si es verdadero (TRUE), devuelve el gráfico de cajas de la serie; hereda propiedades de ggplot; por defecto es FALSE.
#' @param ... argumentos que heredan de la función boxplot()
#' @returns devuelve un objeto de tipo tibble con una serie de medidas de resumen; si el argumento boxplot=TRUE, devuelve un gráfico de cajas que hereda propiedades de ggplot().
#' @examples
#' library(resumiR)
#' # Ejemplo
#' x = iris[[1]]
#' s_simple(x, decimales = 2, recorte = 0.05, boxplot = TRUE)
#' @export

s_simple = function(
    x,
    decimales = 1,
    recorte = 0.1,
    percentiles = c(0.25, 0.5, 0.75),
    boxplot = FALSE,
    ...
    ){
  if (!is.numeric(x)){
    stop("Debes ingresar un vector num\u00e9rico.")
  } else {

    omitidos = (length(x[is.na(x)]))|>
      setNames("Omitidos")
    infinitos = length(x[is.infinite(x)])|>
      setNames("Infinitos")
    n0 = length(x)|>
      setNames("n0")
    x = x[(!is.na(x))&(x!=Inf)&(x!=-Inf)]
    if (length(x)>1){
      tipo = ifelse(
        class(x) == "integer",
        "V.a. cuantitativa discreta",
        "V.a. cuantitativa continua"
      ) |>
        setNames("Tipo de variable")
      mm = mean(x)|>
        setNames("Media")
      recorte = ifelse(
        recorte <=0.25,
        recorte,
        0.1
      )
      mmtrim = mean(x, trim = recorte)|>
        setNames(paste0("Media recortada al ",
                        (recorte * 100)|>round(2) ,
                        "%"))
      min = min(x)|>
        setNames("Min.")
      max = max(x)|>
        setNames("Max.")
      cuantiles = quantile(x,
                           probs = percentiles,
                           type = 2)
      varianza = var(x)|>
        setNames("Varianza muestral")
      s = sd(x)|>
        setNames("Desviaci\u00f3n t\u00edpica muestral")
      r = diff(range(x))|>
        setNames("Rango")
      ri = diff(quantile(x,
                         probs = c(0.25,0.75),
                         type = 2)[c("25%","75%")])|>
        setNames("RI")
      cv = sd(x) / mean(x) * 100|>
        setNames("CV")
      n = length(x)|>
        setNames("n")
      ee = sd(x) / sqrt(n)|>
        setNames("ee")
      Atipicos = length(boxplot.stats(x, coef = 1.5)$out) |>
        setNames("Atipicos")
      Extremos = length(boxplot.stats(x, coef = 3)$out) |>
        setNames("Extremos")
      sn = sqrt(varianza * (n-1) / n) |>
        setNames("Desviaci\u00f3n t\u00edpica no corregida")
      momento3 = mean((x - mm)^3)
      momento4 = mean((x - mm)^4)
      sesgo = momento3 / (sn^3)|>
        setNames("Asimetr\u00eda (momentos)")
      curtosis = momento4 / (sn^4)|>
        setNames("Curtosis (momentos)")
      Medidas = data.frame(
        Valor =    c(n0,
                     n,
                     omitidos,
                     infinitos,
                     c(mm,
                       mmtrim,
                       min,
                       max,
                       cuantiles,
                       varianza,
                       s,
                       cv,
                       ee,
                       r,
                       ri,
                       sesgo,
                       curtosis)|>round(decimales),
                     Atipicos,
                     Extremos
        )
      ) |>
        tibble::rownames_to_column(
          "Medida"
        ) |>
        tibble::as_tibble()

      if (boxplot == TRUE){

      bxplt = x|>
          as.data.frame()|>
          ggplot(aes(y=x),...) +
          geom_boxplot(
            width=0.5,
            fill = "gray"
            ) +
          xlim(-1,1) +
          labs(
             title = "Grafico de cajas y bigotes"
          ) +
          theme_minimal()
      return(
        list(Medidas = Medidas, Boxplot = bxplt)
      )
      } else {
        return(Medidas = Medidas)
      }
    } else {
      stop("Ingresa un vector num\u00e9rico con m\u00e1s de un elemento")
    }
  }
}


#' @name s_agrupada
#' @aliases s_agrupada
#' @title Tablas de Frecuencias y Gráficos para Datos Numéricos
#' @description
#' Devuelve la tabla de frecuencias de una serie univariada de datos numéricos enteros o continuos. En el caso de datos de tipo continuo, es posible especificar los límites y la amplitud de los intervalos de clase. Por defecto, construye los intervalos de clase cerrados por derecha, con la regla de Sturges.
#' @param x vector numérico de tipo *integer* o *numeric*; en este último caso, el vector debe tener una longitud mayor a 1, y permitir el cálculo de medidas de variabilidad.
#' @param li límite inferior del primer intervalo de clase, para datos de tipo *numeric*.
#' @param ls límite superior del intervalo de clase superior, para datos de tipo *numeric*.
#' @param a amplitud del intervalo de clase, para datos de tipo *numeric*.
#' @param derecha si los intervalos de clase son cerrados por derecha, para datos de tipo *numeric*.
#' @param decimales el número de posiciones decimales de los resultados.
#' @param grafico texto indicando el tipo de gráfico a realizar: frecuencias simples, acumuladas o ambas; los valores posibles son "ninguno", "fs" y "fa"; por defecto es "ninguno"; hereda propiedades de ggplot()
#' @param frec texto indicando tipo de frecuencia a utilizar en caso de realizar un gráfico; los valores posibles son "absoluta" y "relativa"; por defecto es "absoluta".
#' @param pf lógico; si es verdadero (TRUE), devuelve el polígono de frecuencias en lugar del histograma de frecuencias, cuando x es continua; por defecto es FALSE.
#' @param ... argumentos que heredan de la función ggplot()
#' @returns Devuelve una tabla de frecuencias para datos continuos o discretos; cuando el argumento gráfico es "fs" o "fa" devuelve el gráfico de frecuencias simples y acumulados respeectivamente, adecuados para cada tipo de variable
#' @examples
#' library(resumiR)
#' x = iris[[1]]
#' s_agrupada(x, li=4, ls=8, a=1, grafico="fs")
#' s_agrupada(x, grafico="fs", pf=TRUE)
#' s_agrupada(x, grafico="fa")
#' @export


s_agrupada = function(x,
                      decimales = 1,
                      li = NULL,
                      ls = NULL,
                      a = NULL,
                      derecha = TRUE,
                      grafico = "ninguno",
                      frec = "relativa",
                      pf = FALSE,
                      ...
                      ){

  # Función de tabla de frecuencias #

  tff = function(x, limites = NULL, tipo = 1){
    if (tipo == 1){
      tf = x|>
        factor(levels = (min(x):max(x))|>as.character())|>
        table(dnn = list("mc"))|>
        as.data.frame(responseName = "fi")|>
        transform(
          fri = (fi / sum(fi))|>round(decimales),
          Fi = cumsum(fi),
          Fri = (cumsum(fi/sum(fi)))|>round(decimales)
        )
      tf
    } else {
      mc = ((limites[-length(limites)]+limites[-1]) / 2)|>round(decimales)
      tf = x |>
        cut(breaks = limites,
            include.lowest = TRUE,
            dig.lab = decimales,
            right = derecha) |>
        table(dnn = list("Clase")) |>
        as.data.frame(responseName = "fi")|>
        transform(
          fri = (fi / sum(fi))|>round(decimales),
          Fi = cumsum(fi),
          Fri = (cumsum(fi/sum(fi)))|>round(decimales)
        )|>cbind(mc)
      tf[,c(1,6,2:5)]
    }
  }

  # Función de histograma #

  ghist = function(x, limites, frec, pf,...){
    x = as.data.frame(x)
    if (frec == "absoluta"){
      ymap = aes(y = ggplot2::after_stat(count))
    } else {
      ymap = aes(y = ggplot2::after_stat(count / sum(count)))
    }
    if (pf == TRUE){
      if (frec == "absoluta"){
        titulo = "Pol\U00EDgono de frecuencias absolutas"
      } else {
        titulo = "Pol\U00EDgono de frecuencias relativas"
      }
      x |>
        as.data.frame() |>
        ggplot(aes( x = x),...) +
          geom_freqpoly(ymap,
                        breaks = limites) +
        stat_bin(
            ymap,
            breaks = limites,
            geom = "point",
            size = 2
          ) +
        labs(
          title = titulo,
          x = "X",
          y = "f(X)"
        ) +
        theme_minimal()

    } else {
      if (frec == "absoluta"){
        titulo = "Histograma de frecuencias absolutas"
      } else {
        titulo = "Histograma de frecuencias relativas"
      }
      x |>
        ggplot(aes( x = x),...) +
        geom_histogram(ymap,
                       breaks = limites,
                       fill = "gray",
                       color = "gray5") +
        scale_x_continuous(
          breaks = limites
        ) +
        labs(
          title = titulo,
          x = "X",
          y = "f(X)"
        ) +
        theme_minimal()

    }
  }

  # Función ojiva #

  gojiva = function(limites, frec, tf, ...){
    if (frec == "relativa"){
      y = c(0, tf$Fri)
    } else {
      y = c(0, tf$Fi)
    }
    data.frame(
      x = limites,
      y = y
    ) |>
      ggplot(aes(x = x, y = y),...) +
      geom_line() +
      geom_point() +
      scale_x_continuous(
        breaks = limites
      ) +
      labs(
        title = "Ojiva",
        x = "X",
        y = "f(X)"
      ) +
      theme_minimal()

  }

  # Función bastón #

  gbaston = function(tf, frec,...){
   if (frec == "relativa"){
     y = tf$fri
   } else {
     y = tf$fi
   }
   data.frame(
     x = tf$mc |> as.character()|>as.numeric(),
     y = y
   ) |>
     ggplot(aes(x = x, y = y),...) +
     geom_col(fill = "black",
              color = "black",
              width = 0.001) +
     scale_x_continuous(
       breaks = x
     ) +
     labs(
       title = "Grafico de bastones",
       x = "X",
       y = "f(X)"
     ) +
     theme_minimal()

  }

  # Función escalonado #

  gstep = function(tf, frec,...){
    if (frec == "relativa"){
      y = tf$Fri
    } else {
      y = tf$Fi
    }
    data.frame(
      x = tf$mc |> as.character()|>as.numeric(),
      y
    ) |>
      ggplot(aes(x = x, y = y),...) +
      geom_step(
        direction = "hv"
      ) +
      geom_segment(
        data = data.frame(
          x = tf$mc |> as.character()|>as.numeric()|>max(),
          y = max(y)
        ),
        aes(
          x = max(x),
          xend = max(x) + 1,
          y = max(y),
          yend = max(y)
        )
      ) +
      geom_segment(
        data = data.frame(
          x = tf$mc |> as.character()|>as.numeric()|>min(),
          y = 0
        ),
        aes(
          x = min(x) - 1,
          xend = min(x),
          y = 0,
          yend = 0
        )
      ) +
      geom_segment(
        data = data.frame(
          x = tf$mc |> as.character()|>as.numeric()|>min(),
          y = min(y)
        ),
        aes(
          x = min(x),
          xend = min(x),
          y = 0,
          yend = min(y)
        )
      ) +
      scale_x_continuous(
        breaks = c(min(x) - 1, x, max(x) + 1)
      ) +
      labs(
        title = "Grafico escalonado",
        x = "X",
        y = "f(X)"
      ) +
      theme_minimal()

  }

  # Tabla de frecuencias #


  if (!is.numeric(x)){
    stop("Debes ingresar un vector num\u00e9rico")
  } else {
    n0 = length(x) |>
      setNames("n0")
    omitidos = length(x[is.na(x)])|>
      setNames("Omitidos")
    infinitos = length(x[is.infinite(x)])|>
      setNames("Infinitos")
    x = x[(!is.na(x))&(x!=Inf)&(x!=-Inf)]
    n = length(x) |>
      setNames("n")
    tipo = ifelse(
      class(x) == "integer",
      "V.a. cuantitativa discreta",
      "V.a. cuantitativa continua"
    )
    if (is.integer(x)){
        tf = tff(x, tipo = 1)
    } else {
      if ((length(x)<2)|(length(unique(x))<=1)){
        stop("Ingrese un vector num\u00e9rico que permita la agrupaci\u00f3n en intervalos de clase.")
      }else{
        if ((!is.null(li)) && (!is.null(ls)) && (!is.null(a))){
          limites = seq(li, ls, a)
          tf = tff(x, limites, tipo = 2)
        } else {
          k = ceiling(1 + 3.3 * log10(n))
          li = min(x)
          ls = max(x)
          a = (max(x) - min(x)) / k
          limites = seq(li, ls, a)
          tf = tff(x, limites, tipo = 2)
        }
      }
    }

    if ((is.numeric(x)) && !(is.integer(x))){
      tf$Clase = gsub(",", " - ", tf$Clase)
      tf$Clase = gsub("\\.", ",", tf$Clase)
    }

    # Salidas #

    options(scipen = 0)
    tf = tibble::as_tibble(tf)

    if (!(grafico %in% c("fs", "fa"))) {
      return(list(
        `Tabla de frecuencias` = tf,
        `Tipo de variable` = tipo,
        n0 = n0,
        n = n,
        Omitidos = omitidos,
        Infinitos = infinitos
      ))
    } else {
      # Gráficos #

      if (grafico %in% c("fs", "fa") &&
          (!(frec %in% c("absoluta", "relativa")))) {
        frec = "relativa"
      }

      if (is.numeric(x) && (!is.integer(x)) && (grafico == "fs")){
        graf = ghist(x, limites, frec, pf)
      } else {
        if (is.numeric(x) && (!is.integer(x)) && grafico == "fa") {
          graf = gojiva(limites, frec, tf)
        } else {
          if (is.integer(x) && grafico == "fs"){
            graf = gbaston(tf, frec)
          } else {
            if (is.integer(x) && grafico == "fa") {
              graf = gstep(tf, frec)
            }
          }
        }
      }

      return(list(
        `Tabla de frecuencias` = tf,
        Grafico = graf,
        `Tipo de variable` = tipo,
        n0 = n0,
        n = n,
        Omitidos = omitidos,
        Infinitos = infinitos
      ))
    }
  }
}

