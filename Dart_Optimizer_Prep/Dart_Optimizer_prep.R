source("../Dart_Optimizer/Dart_Optimizer_support_1.R")

n_sim <- 2000

targets <- -35:35 / 35
poss_targets_df <- crossing(x_target = targets, y_target = targets) %>%
  filter(x_target^2 + y_target^2 < 1)

poss_sds <- seq(0.01, 0.5, 0.01)
poss_sds_df <- crossing(sd_vert = poss_sds, sd_horiz = poss_sds)

norm_sim <- tibble(dx = rnorm(n_sim), dy = rnorm(n_sim))
basic_sim <- crossing(poss_targets_df, norm_sim) 

sim_means <- tibble()
for (sd_vert in poss_sds) {
  for (sd_horiz in poss_sds) {
    print(sd_vert)
    print(sd_horiz)
    sim <- basic_sim %>% 
      mutate(x_sim = x_target + sd_horiz * dx,
             y_sim = y_target + sd_vert * dy) %>%
      mutate(res = dart_result(tibble(x_sim, y_sim)))
    
    new_means <- sim %>%
      group_by(x_target, y_target) %>%
      summarize(mean_res = mean(res)) %>%
      ungroup() %>%
      mutate(sd_vert = sd_vert, sd_horiz = sd_horiz)
    
    sim_means <- bind_rows(sim_means, new_means)
  }
}

write_csv(sim_means, "../Dart_Optimizer/sim_means.csv")
