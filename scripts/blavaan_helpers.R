# Code by Simon Brauer, obtained from
# https://discourse.mc-stan.org/t/draws-of-named-and-derived-parameters-in-blavaan/31454/2

get_model_draws <- function(m) {
  mcmc <- blavInspect(m, 'mcmc', add.labels = FALSE)
  map2(mcmc, 1:length(m), function(d, chn) {
    d |>
      as.data.frame() |>
      mutate(chain = chn, iter = 1:n()) |>
      relocate(chain, iter)
  }) |> list_rbind()
}

select_params <- function(draws, old_names, new_names) {
  for (i in 1:length(old_names)) {
    colnames(draws)[colnames(draws) == old_names[i]] <- new_names[i]
  }
  
  draws |>
    select(all_of(new_names))
}

get_model_parameters <- function(m) {
  # parfix <- read_csv(here("data", "intercepts.csv"),
  #                    show_col_types = FALSE)
  
  parTable(m) |>
    as.data.frame() |>
    select(lhs, op, rhs, label, pxnames) # |> 
    # left_join(parfix, by = join_by(lhs, op, pxnames)) |> 
    # mutate(label = coalesce(label.y, label.x)) |> 
    # select(-label.x, -label.y)
}

get_derived_draws <- function(m) {
  model_params <- get_model_parameters(m)
  model_draws <- get_model_draws(m)
  
  chain_iter_df <- model_draws |>
    select(chain, iter)
  
  renamed_params <- model_params |>
    filter(label != '' & !is.na(pxnames)) |>
    (function(rp) {
      model_draws |>
        select_params(rp$pxnames, rp$label)
    })()
  
  derived_params <- model_params |>
    filter(op == ':=') |>
    mutate(mutate_code = str_c(label, ' = ', rhs)) |>
    (function(dp) {
      mc <- str_c('mutate(', str_c(dp$mutate_code, collapse = ', '), ')')
      sc <- str_c('select(', str_c(dp$label, collapse = ','), ')')
      
      fc <- str_c('renamed_params |>', mc, '|>', sc)
      
      eval(parse(text = fc))
    })()
  
  as_tibble(bind_cols(chain_iter_df, renamed_params, derived_params))
}