#' \code{plot_results} grafica la cantidad de registros verdaderos y falsos para una columna
#'
#' Muestra un gráfico de \code{ggplot} con la cantidad de registros válidos e inválidos para una columna en un \code{data.frame}.
#'
#' @param data \code{data.frame}
#' @param var Columna del data frame que se quiere graficar
#' @return Gráfico de \code{ggplot} con registros válidos e inválidos
#' @importFrom ggplot2 ggplot
#' @export

plot_results <- function(data, var) {

  x_var <- sym(var)
  plot <- data %>%
    group_by(!!x_var) %>%
    summarise(suma = n()) %>%
    ggplot(aes(x = as.factor(!!x_var), y = suma)) +
    ggtitle(paste("Resultados", var )) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = suma),
              size = 2.5,
              position = position_dodge(width = 0.6),
              vjust = -0.3,
              hjust = 0.5) +
    theme(text = element_text(size = 10))
  return(plot)
}
