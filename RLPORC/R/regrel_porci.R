#' Regresión lineal para calcular cantidad de lechones en próximo parto
#'
#' @param datos  (vector) Datos reales para entrenamiento de regresión lineal.
#' @param p_cerda (vector) Pesos de cerda landrace.
#' @param no_partos (vector) Número de partos anteriores.
#' @param edad (vector) Edad de la cerda en meses
#' @param con_ali (vector) Condición alimenticia. 1 = Óptima, 0 = Restringida
#' @param cant_lech (vector) Promedio de lechones de cerda landrace.
#' @return Cantidad de lechones en próximo parto.
#' @export
rgl <- function(pc, npa, ec, ca, cl, datos){
  pc <- datos[ ,pc]
  np <- datos[ ,npa]
  edad <- datos[ ,ec]
  cal <- datos[ ,ca]
  can_lec <- datos[ ,cl]
  modelo <- lm(can_lec ~ pc + np + edad + cal )
  return(modelo)
}







