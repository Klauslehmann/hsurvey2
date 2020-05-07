#' Selecciona información útil dentro de tabla validada
#'
#' Recibe una base validada y cosnerva solo la información útil
#'
#' @param report Data frame
#' @param label String. Etiqueta para nombrar a cada una de las columnas
#' @param id Identificador de cada registro. Es un string
#' @export


clean_report <- function(report, label, id) {
  id_sym <- sym(id)
  unpacked_data <- report %>%
    select(all_of(id_sym), !!label := all_rules_true)
  return(unpacked_data)

}
