pretty_scree_plot <- function(fa_parallel) {
  # plotting code inspired by https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/
  
  obs <- tibble(
    eigenvalue = fa_parallel$fa.values,
    type = "Observed data",
    num = seq_along(eigenvalue)
  )
  
  sim <- fa_parallel$values |> 
    as_tibble() |> 
    select(starts_with("Fsim")) |> 
    map_dbl(\(x) quantile(x, .95)) |> 
    as_tibble() |> 
    mutate(type = "Simulated data",
           num = seq_along(value)) |> 
    rename(eigenvalue = value)
  
  eigen_data <- bind_rows(
    obs,
    sim
  )
  
  ggplot(eigen_data,
         aes(x = num, y = eigenvalue, shape = type, col = type)) +
    geom_line() +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(min(eigen_data$num),
                                    max(eigen_data$num))) +
    scale_shape_manual(values = c(16, 1)) +
    geom_vline(xintercept = fa_parallel$nfact + 0.5, 
               lty = "dashed") +
    labs(x = "Factor number",
         y = "Eigenvalue",
         col = "Data type",
         shape = "Data type") +
    theme(legend.position = "inside",
          legend.position.inside = c(0.85, 0.85))
  
}