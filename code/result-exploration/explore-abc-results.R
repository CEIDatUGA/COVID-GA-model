out_abc <- readRDS("output/abc-results.RDS")


abc_num_mcmc <- 200000
abc_num_burn <- 150000
abc_num_thin <- 50
all_abc <- tibble()
for(i in 1:length(out_abc)) {
  tmp <- out_abc[[i]]
  param_mat <- tail(tmp@traces, abc_num_mcmc - abc_num_burn) %>%
    as.data.frame() %>%
    mutate(chain = i,
           accept = tmp@accepts / abc_num_mcmc)
  param_out <- param_mat[seq(1, nrow(param_mat), abc_num_thin), ]
  param_out$iter <- 1:nrow(param_out)
  all_abc <- bind_rows(all_abc, param_out)
}

keep_abc <- all_abc %>%
  filter(accept > 0.2)

end_abc <- keep_abc %>%
  filter(iter == 100)


 

filename = here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

allparvals <- colMeans(keep_abc)


M2 <- pomp_model
horizon <- 7*6
time(M2) <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
covars <- pomp_model@covar@table
covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
covars <- as.data.frame(covars) %>%
  mutate(time = 1:n()) %>%
  rename("rel_beta_change" = covars)
# covars$rel_beta_change <- 1
M2 <- pomp(M2, covar = covariate_table(covars, times = "time", order = "constant"))

#run simulation a number of times
sims <- pomp::simulate(M2, 
                       params=allparvals, 
                       nsim=10, format="data.frame", 
                       include.data=TRUE)

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

pl <- sims %>%
  left_join(dates_df) %>%
  dplyr::select(Date, .id, C_new, H_new, D_new, cases, hosps, deaths) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id) %>%
  mutate(.id = ifelse(.id == "data", "ZZZ", .id)) %>%
  ggplot(aes(x = Date, y = value, group = .id, color=.id=="ZZZ",
             size = .id=="ZZZ", alpha = .id == "ZZZ")) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_size_manual(values = c(0.5, 1)) +
  scale_alpha_manual(values = c(0.1, 1)) +
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

plot(pl)
