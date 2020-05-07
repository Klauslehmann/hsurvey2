#' Grafica una columna del data frame
#'
#' Valida una columna del data frame, mostrando la cantidad de casos válidos e inválidos.
#'
#' @param data Data frame
#' @param var Columna del data frame que se quiere graficar
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
