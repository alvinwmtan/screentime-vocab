ses_draws <- function(params, mod_pars, mod_mcmc) {
  ses_vals <- read_csv(here("data", "ses_vals.csv"),
                       show_col_types = FALSE)
  
  map(3:8, \(ses_val) {
    pars <- mod_pars |> 
      filter(lhs %in% params,
             str_starts(rhs, "ses.") | op == "~1") |> 
      select(lhs, rhs, label)
    cur_ses_vals <- ses_vals |> 
      filter(ses == ses_val) |> 
      pivot_longer(-ses, names_to = "rhs", values_to = "value")
    cur_mcmc <- mod_mcmc |> 
      select(iter, all_of(pars$label)) |> 
      pivot_longer(-iter, names_to = "label", values_to = "coef")
    cur_all <- cur_mcmc |> 
      left_join(pars, by = join_by(label)) |> 
      left_join(cur_ses_vals, by = join_by(rhs)) |> 
      mutate(value = replace_na(value, 1),
             est = coef * value) |>
      group_by(iter, lhs) |>
      summarise(est = sum(est, na.rm = TRUE),
                .groups = "drop") |> 
      pivot_wider(names_from = "lhs", values_from = "est") |> 
      ungroup() |> 
      select(-iter)
    
    ses_hpd <- HPDinterval(as.mcmc(cur_all), prob = 0.95) |> 
      as_tibble(rownames = "var")
    ses_means <- cur_all |> 
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |> 
      pivot_longer(everything(), names_to = "var", values_to = "mean")
    ses_est <- ses_means |> 
      left_join(ses_hpd, by = join_by(var)) |> 
      rename(ci.lower = lower, ci.upper = upper) |> 
      mutate(ses = ses_val)
  }) |> list_rbind()
}
