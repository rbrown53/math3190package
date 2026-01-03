#' Create a 1D or 2D UMAP Plot
#'
#' This function plots the UMAP layouts in either 1D or 2D and can optionally 
#' color them using a variable from a given dataset.
#'
#' @param umap_object An object fit by the `umap()` function in the `umap` 
#' library.
#' @param data The dataset that has the variable to be used for `color_umap`. 
#' @param res The variable that should be used to change the color of the points
#' on the graph.
#' @param ... Extra arguments for the geom_point() function when plotting in 2D
#' or for the geom_histogram() function when plotting in 1D.
#'
#' @return This function returns a ggplot object plot. It can be added onto like
#' any ggplot object. 
#'
#' @examples
#' data(iris)
#' iris_umap <- iris[,1:4] |> umap(random_state = 1)
#' umap_plot(iris_umap, data = iris, color_umap = "Species")
#'
#' @import
#'   dplyr
#'   ggplot2
#'
#' @importFrom umap umap
#'
#' @export

umap_plot <- function(umap_object, data, color_umap = NULL, ...) {
  if(ncol(umap_object$layout) == 2) {
    umap_df <- umap_object$data |>
      as.data.frame() |>
      mutate(UMAP1 = umap_object$layout[,1],
             UMAP2 = umap_object$layout[,2])
    if(!is.null(color_umap)) {
      umap_df |>
        mutate(color_umap_plot = data[[color_umap]]) |>
        ggplot(aes(x = UMAP1, y = UMAP2, color = color_umap_plot)) +
        geom_point(...) +
        labs(color = color_umap) +
        theme_bw()
    } else {
      ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
        geom_point(...) +
        theme_bw()
    }
  } else if (ncol(umap_object$layout) == 1) {
    umap_df <- umap_object$data |>
      as.data.frame() |>
      mutate(UMAP1 = umap_object$layout[,1])
    if(!is.null(color_umap)) {
      umap_df |>
        mutate(color_umap_plot = data[[color_umap]]) |>
        ggplot(aes(x = UMAP1, fill = color_umap_plot)) +
        geom_histogram(color = "black", ...) +
        facet_grid(rows = vars(color_umap_plot)) +
        labs(fill = color_umap) +
        theme_bw()
    } else {
      ggplot(umap_df, aes(x = UMAP1)) +
        geom_histogram(color = "black", ...) +
        theme_bw()
    }
  } else {
    print("This function only works with 1 or 2 umap components")
  }
}