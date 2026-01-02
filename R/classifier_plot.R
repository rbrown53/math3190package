#' Plots the Points and Predicted Region for a Classifier Model
#'
#' This function plots the points and predicted region for a classifier model of
#' class "naive_bayes", "lda", or "svm". This function can support up to three
#' predictor variables: two of which can be quantitative and one can be
#' categorical. 
#'
#' @param classifier_model The classifier model of class "naive_bayes", "lda", 
#' or "svm".
#' @param data The dataset that should be plotted. This need not be the dataset
#' on which the classifier was trained. 
#' @param res The resolution of the plot. Larger values will make the lines look
#' straighter and make the grid more fine, but take longer.
#' @param sv_diamonds A boolean indicating whether the support vectors should be
#' plotted with diamonds around them when plotting a support vector machine 
#' classifier. 
#'
#' @return This function returns a ggplot object plot. It can be added onto like
#' any ggplot object. 
#'
#' @examples
#' data(mtcars)
#' mtcars2 <- mtcars |> mutate(vs = factor(vs), am = factor(am))
#' classifier <- MASS::lda(vs ~ mpg + disp + am, data = mtcars2)
#' classifier_plot(classifier, data = mtcars2)
#' plot_tukey(t)
#'
#' @import
#'   dplyr
#'   ggplot2
#'
#' @importFrom MASS lda qda
#' @importFrom e1071 svm
#' @importFrom naivebayes naive_bayes
#'
#' @export

classifier_plot <- function(classifier_model, data, 
                            res = 200, sv_diamonds = TRUE) {
  # 1. Identify predictor and response names
  all_vars <- all.vars(
    classifier_model$call
  )[-length(all.vars(classifier_model$call))]
  response_var <- all_vars[1]
  predictor_vars <- all_vars[-1]
  # categorical <- names(classifier_model$xlevels)
  
  predictor_data <- data |> 
    dplyr::select(all_of(predictor_vars))
  categorical <- predictor_data[sapply(predictor_data, \(x) !is.numeric(x))] |>
    names()
  
  if(length(categorical) != 0) {
    predictor_vars <- predictor_vars[-grep(categorical, predictor_vars)]
  }
  
  # --- CASE 1: TWO PREDICTORS ---
  if (length(predictor_vars) == 2) {
    x_var <- predictor_vars[1]
    y_var <- predictor_vars[2]
    
    # Grid generation (including categorical levels)
    grid_range_x <- seq(
      ifelse(sign(min(data[[x_var]])) > 0, 
             min(data[[x_var]]) * 0.9,
             min(data[[x_var]]) * 1.1), 
      ifelse(sign(max(data[[x_var]])) > 0, 
             max(data[[x_var]]) * 1.1,
             max(data[[x_var]]) * 0.9), 
      length.out = res
    )
    grid_range_y <- seq(
      ifelse(sign(min(data[[y_var]])) > 0, 
             min(data[[y_var]]) * 0.9,
             min(data[[y_var]]) * 1.1), 
      ifelse(sign(max(data[[y_var]])) > 0, 
             max(data[[y_var]]) * 1.1,
             max(data[[y_var]]) * 0.9),  
      length.out = res
    )
    
    if(length(categorical) != 0) {
      grid_range_cat <- unique(data[[categorical]])
      grid <- expand.grid(
        setNames(
          list(grid_range_x, grid_range_y, grid_range_cat), 
          c(x_var, y_var, categorical)
        )
      )
      
      if(inherits(classifier_model, "lda")) {
        # Calculate local means for each group within each facet
        facet_means <- data |>
          group_by(!!sym(response_var), !!sym(categorical)) |>
          summarize(m1 = mean(!!sym(x_var), na.rm=T), 
                    m2 = mean(!!sym(y_var), na.rm=T), .groups = "drop")
      }
      
    } else {
      if(inherits(classifier_model, "lda")) {
        # Use model means if no categorical variable is present
        facet_means <- as.data.frame(classifier_model$means) |>
          tibble::rownames_to_column(response_var) |>
          rename(m1 = 2, m2 = 3) # Renaming for consistent aes mapping
      }
      grid <- expand.grid(setNames(list(grid_range_x, grid_range_y), c(x_var, y_var)))
    }
    
    if(inherits(classifier_model, "lda")) {
      grid$predicted <- predict(classifier_model, grid)$class
    } else if(inherits(classifier_model, "naive_bayes") |
              inherits(classifier_model, "svm")) {
      grid$predicted <- predict(classifier_model, grid, type = "class")
    }
    grid$pred_num <- as.numeric(grid$predicted)
    
    p <- ggplot() +
      geom_tile(data = grid, 
                aes(x = .data[[x_var]], y = .data[[y_var]], fill = predicted),
                alpha = 0.2) +
      geom_contour(data = grid, 
                   aes(x = .data[[x_var]], y = .data[[y_var]], z = pred_num),
                   color = "black", 
                   linetype = "dashed", 
                   linewidth = 0.5, 
                   breaks = c(1.5, 2.5), show.legend = FALSE) +
      geom_point(data = data, 
                 aes(x = .data[[x_var]], y = .data[[y_var]], 
                     shape = .data[[response_var]]), 
                 color = "black", size = 3.1, stroke = 1, show.legend = FALSE) +
      geom_point(data = data, 
                 aes(x = .data[[x_var]], 
                     y = .data[[y_var]], 
                     color = .data[[response_var]],
                     shape = .data[[response_var]]), 
                 size = 3)
    # scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
    # scale_fill_viridis_d(option = "viridis") +
    # theme_minimal()
    
    if(inherits(classifier_model, "lda")) {
      # Plotting the group-specific X marks
      p <- p + geom_point(data = facet_means, 
                          aes(x = m1, y = m2, color = .data[[response_var]]), 
                          shape = 4, size = 5, stroke = 2, show.legend = FALSE) +
        labs(title = paste("LDA Decision Boundaries (2D):", response_var),
             subtitle = "X marks indicate group centroids",
             fill = "Predicted", color = "Actual", shape = "Actual")
    } else if (inherits(classifier_model, "naive_bayes")) {
      p <- p + 
        labs(title = paste("Naïve Bayes Decision Boundaries (2D):", response_var),
             fill = "Predicted", color = "Actual", shape = "Actual")
    } else if (inherits(classifier_model, "svm")) {
      kenel_type <- case_when(
        classifier_model$kernel == 0 ~ "Linear",
        classifier_model$kernel == 1 ~ "Polynomial",
        classifier_model$kernel == 2 ~ "Radial Basis",
        classifier_model$kernel == 3 ~ "Sigmoid"
      )
      if(sv_diamonds) {
        p <- p + geom_point(data = data[classifier_model$index,], 
                            aes(x = .data[[x_var]], y = .data[[y_var]]),
                            shape = 5, size = 5, color = "black")
      }
      p <- p + 
        labs(title = paste("SVM Decision Boundaries (2D):", response_var),
             subtitle = ifelse(
               kenel_type == "Polynomial",
               paste0("Using a ", kenel_type, 
                      " Kernel with Degree = ", classifier_model$degree,
                      " and Cost = ", classifier_model$cost),
               ifelse(kenel_type == "Radial Basis",
                      paste0("Using a ", kenel_type, 
                             " Kernel with gamma = ", classifier_model$gamma,
                             " and Cost = ", classifier_model$cost),
                      paste0("Using a ", kenel_type, 
                             " Kernel with Cost = ", classifier_model$cost))
             ),
             fill = "Predicted", color = "Actual", shape = "Actual")
    }
    
    if(length(categorical) != 0) {
      p <- p + facet_grid(rows = vars(!!sym(categorical)),
                          labeller = label_both)
    }
    p <- p + 
      scale_shape_manual(values = c(16, 17, 15, 18, 19)) + # Solid shapes
      scale_fill_viridis_d(option = "viridis", name = "Predicted") +
      scale_color_viridis_d(option = "viridis", name = "Actual") +
      guides(
        fill = guide_legend(override.aes = list(alpha = 0.5)),
        color = guide_legend(override.aes = list(size = 4))
      ) +
      theme_minimal()
    p
    
    # --- CASE 2: ONE PREDICTOR ---
  } else if (length(predictor_vars) == 1) {
    x_var <- predictor_vars[1]
    
    # Create a 1D grid for the background line
    if(length(categorical) != 0) {
      grid_range_x <- seq(
        ifelse(sign(min(data[[x_var]])) > 0, 
               min(data[[x_var]]) * 0.9,
               min(data[[x_var]]) * 1.1), 
        ifelse(sign(max(data[[x_var]])) > 0, 
               max(data[[x_var]]) * 1.1,
               max(data[[x_var]]) * 0.9), 
        length.out = res)
      grid_range_cat <- unique(data[[categorical]])
      grid <- expand.grid(grid_range_x, grid_range_cat)
      colnames(grid) <- c(x_var, categorical)
    } else {
      grid <- data.frame(
        temp = seq(min(data[[x_var]]) * 0.9, 
                   max(data[[x_var]]) * 1.1, 
                   length.out = res
        )
      )
      colnames(grid) <- x_var
    }
    if(inherits(classifier_model, "lda")) {
      grid$predicted <- predict(classifier_model, grid)$class
    } else if(inherits(classifier_model, "naive_bayes") |
              inherits(classifier_model, "svm")) {
      grid$predicted <- predict(classifier_model, grid, type = "class")
    }
    
    # Identify the cutoff point(s) where predictions change
    
    if(length(categorical) != 0) {
      cutoffs <- grid |>
        group_by(!!sym(categorical)) |>
        mutate(change = c(0, diff(as.numeric(predicted)))) |>
        filter(change != 0) |>
        dplyr::select(!!sym(categorical), vl = !!sym(x_var))
    } else {
      cutoffs <- data.frame(
        vl = grid[[x_var]][which(diff(as.numeric(grid$predicted)) != 0)]
      )
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], fill = .data[[response_var]])) +
      # Density curves to show the "spread" of each group
      geom_density(alpha = 0.3) +
      scale_fill_viridis_d(option = "viridis") +
      # scale_fill_brewer(palette = "Dark2") +
      # scale_fill_grey() +
      # Rug plot at the bottom to show actual data points
      geom_rug(aes(color = .data[[response_var]])) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal()
    
    if(inherits(classifier_model, "lda")) {
      p <- p + 
        labs(title = paste("LDA Decision Boundary (1D):", response_var),
             subtitle = "Dashed line indicates decision boundary",
             fill = "Actual Class")
    } else if (inherits(classifier_model, "naive_bayes")) {
      p <- p + 
        labs(title = paste("Naïve Bayes Decision Boundary (1D):", response_var),
             subtitle = "Dashed line indicates decision boundary",
             fill = "Actual Class")
    } else if (inherits(classifier_model, "svm")) {
      p <- p + 
        labs(title = paste("SVM Decision Boundary (1D):", response_var),
             subtitle = "Dashed line indicates decision boundary",
             fill = "Actual Class")
    }
    
    if(length(categorical) != 0) {
      p <- p + facet_grid(rows = vars(!!sym(categorical)), 
                          labeller = label_both) +
        geom_vline(data = cutoffs, aes(xintercept = vl), 
                   linetype = "dashed", linewidth = 0.8, color = "grey30")
    } else {
      p <- p + geom_vline(data = cutoffs, aes(xintercept = vl), 
                          linetype = "dashed", linewidth = 1, color = "grey30")
    }
    p
    
  } else {
    stop("This function only supports 1 or 2 predictor variables unless there is a categorical predictor.")
  }
}