#' Valida la columna de un data frame, en base a reglas establecidas
#'
#' Valida una columna de un \code{data.frame}, siguiendo las reglas establecidas con anterioridad.
#'
#' @param data Data frame
#' @param validator Objeto creado con validate. Conjunto de reglas que debe ser creado previamente para cada una de las columnas.
#' @param id Identificador de cada registro. Es un string que debe ser creado dentro de la tabla que se está validando.
#' @return Retorna un \code{data.frame} con información de verdadero o falso para cada uno de los registros
#' \itemize{
#'  \item id2
#'  \item name
#'  \item value
#'  \item expression
#'  \item all_rules_true
#' }
#' @import validate
#' @export

validate_var <- function(data, validator, id) {
  #Función para generar reporte reducido. Esta función recibe el resultado de todas las reglas y devuelve un registro resumido que dice sí o no.
  small_report <- function(report, id) {
    id_sym <- sym(id)
    report_df <- as.data.frame(report)

    small_report <- report_df %>%
      group_by(!!id_sym) %>%
      mutate(all_rules_true = mean(value, na.rm = T),
             all_rules_true = if_else(all_rules_true == 1, 1, 0)) %>%
      filter(row_number() == 1)
    return(small_report)
  }

  #Confrontar los datos con las reglas de validación establecidas
  cf <- confront(data, validator , key = id)

  return(small_report(cf, id))
}


#' Valida un conjunto de columnas de un data frames
#'
#' Valida un conjunto de columnas de un data frames, siguiendo las reglas establecidas con anterioridad mediante el paquete validate.
#'
#' @param data Data frame
#' @param vals Vector de strings que contiene cada una de las reglas. Se requiere un objeto validate para cada columna que se quiere evaluar.
#' @param id Identificador de cada registro. Es un string que debe ser creado dentro de la tabla que se está validando.
#' @return Lista que contiene un \code{data.frame} para cada una de las columnas evaluadas
#' @import validate
#' @export

validate_data <- function(data, vals, id) {
  validators <- vals
  reports <- list()
  for (i in 1:length(validators)) {
    reports[[i]] <- validate_var(data, validators[[i]], id)
  }
  return(reports)
}


