pca_ggplots <- function(pca_res,
                            class = NULL,
                            varnames = NULL,
                            list_plots = FALSE) {
  scores <- pca_res$scores

  n <- length(pca_res$eigen.values)



  plt_data <- data.frame(x = scores[, 1], y = scores[, 2])

  if (!is.null(class)) {
    plt_data <- cbind(plt_data, class = class)
  }


  p_scores <- ggplot2::ggplot(plt_data, ggplot2::aes(x, y))

  if (is.null(class)) {
    p_scores <-  p_scores + ggplot2::geom_point(pch = 21,
                                       size = 3,
                                       color = "black")
  } else{
    p_scores <-  p_scores + ggplot2::geom_point(
      ggplot2::aes(fill = class),
      pch = 21,
      size = 3,
      color = "black"
    )
  }
  p_scores <- p_scores + ggplot2::scale_color_brewer(palette = "Set2") + ggplot2::theme_bw()

  # composing axis labels

  p_scores <- p_scores + ggplot2::xlab(paste0("PC1 [", round(pca_res$rel.var.perc[1], 1) , " %]"))
  p_scores <- p_scores + ggplot2::ylab(paste0("PC2 [", round(pca_res$rel.var.perc[2], 1) , " %]"))

  cors_pc1 <- apply(pca_res$D, 2, \(x) cor(x, scores[, 1]))
  cors_pc2 <- apply(pca_res$D, 2, \(x) cor(x, scores[, 2]))

  plt_data_barpl <- data.frame(
    x = rep(1:n, 2),
    y = c(cors_pc1, cors_pc2),
    PC = as.factor(rep(1:2, each = n))
  )

  p_cor <- ggplot2::ggplot(plt_data_barpl, ggplot2::aes(x = x, y = y, fill = PC)) +
    ggplot2::geom_col(position = "dodge", width = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::xlab("feature") +
    ggplot2::ylab("correlation")

  if (!is.null(varnames)) {
    p_cor <- p_cor + ggplot2::scale_x_continuous(breaks = 1:n, labels = varnames)
  }


  plt_data_var <- data.frame(
    x = rep(1:n, 2),
    y = c(pca_res$rel.var.perc, pca_res$cumul.rel.var.perc),
    Percentage = c(rep("var", n), rep("var, cumul.", n))

  )


  p_var <- ggplot2::ggplot(plt_data_var, ggplot2::aes(x = x, y = y, colour = Percentage)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c(
      "var" = "dodgerblue4",
      "var, cumul." = "firebrick3"
    )) +
    ggplot2::ylim(0, 100) +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::theme_bw()


  if(list_plots){
    return(
      list(
        scores_plot = p_scores,
        correlation_plot = p_cor,
        expl_var_plot = p_var
      )
    )
  }

  print(p_scores)
  print(p_cor)
  print(p_var)


}
