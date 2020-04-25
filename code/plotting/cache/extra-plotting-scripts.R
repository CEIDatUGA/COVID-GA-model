



# Increase social distancing trajectory
# ggplot(sim_summs %>%
#          filter(SimType == "linear_increase_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   theme_minimal() +
#   ggtitle("1. Increased social distancing")
# ggsave("./output/figures/increased-sd-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # sim_summs %>%
# #   filter(SimType == "linear_increase_sd") %>%
# #   filter(Period == "Future") %>%
# #   filter(Date == min(Date))
# # sim_summs %>%
# #   filter(SimType == "linear_increase_sd") %>%
# #   filter(Period == "Past") %>%
# #   filter(Date == max(Date))
# 
# ggplot(sim_summs %>%
#          filter(SimType == "linear_increase_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   scale_y_continuous(labels = scales::comma)+
#   theme_minimal() +
#   ggtitle("1. Increased social distancing")
# ggsave("./output/figures/increased-sd-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # Status quo trajectory
# ggplot(sim_summs %>%
#          filter(SimType == "status_quo"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   ggtitle("2. Status quo")
# ggsave("./output/figures/status-quo-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# ggplot(sim_summs %>%
#          filter(SimType == "status_quo"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("2. Status quo")
# ggsave("./output/figures/status-quo-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # Relax social distancing trajectory
# ggplot(sim_summs %>%
#          filter(SimType == "linear_decrease_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   ggtitle("3. Relax social distancing")
# ggsave("./output/figures/relax-sd-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# ggplot(sim_summs %>%
#          filter(SimType == "linear_decrease_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("3. Relax social distancing")
# ggsave("./output/figures/relax-sd-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # Return to normal
# ggplot(sim_summs %>%
#          filter(SimType == "return_normal"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   ggtitle("4. Return to normal")
# ggsave("./output/figures/return-normal-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# ggplot(sim_summs %>%
#          filter(SimType == "return_normal"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("4. Return to normal")
# ggsave("./output/figures/return-normal-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # No intervention
# ggplot(sim_summs %>%
#          filter(SimType == "no_intervention"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   ggtitle("6. No intervention")
# ggsave("./output/figures/no-intervention-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# ggplot(sim_summs %>%
#          filter(SimType == "no_intervention"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("6. No intervention")
# ggsave("./output/figures/no-intervention-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # Best SD to date
# ggplot(sim_summs %>%
#          filter(SimType == "lowest_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
#   ggtitle("5. Continuously improving social distancing")
# ggsave("./output/figures/lowest-sd-traj-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# ggplot(sim_summs %>%
#          filter(SimType == "lowest_sd"), 
#        aes(x = Date, color = Period, fill = Period)) +
#   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(aes(y = ptvalue)) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
#   scale_color_brewer(type = "qual") +
#   scale_fill_brewer(type = "qual") +
#   theme_minimal() +
#   ylab("Number of persons") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("5. Continuously improving social distancing")
# ggsave("./output/figures/lowest-sd-traj.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)


# nada <- sim_summs %>%
#   filter(SimType == "no_intervention") %>%
#   filter(Variable == "cases")
# 
# par(mfrow = c(1,2))
# plot(cumsum(ptvalue)~Date, data = nada, type = "h", ylab = "Cumulative cases")
# plot(ptvalue~Date, data = nada, type = "l", ylab = "New cases")


# rel_beta_change = seq(0.1, 2, by = 0.01)
# log_beta_s <- -16.9
# Isd_tot = 14*4
# Isu_tot = 90*4
# E_tot = 40*4
# Ia_tot = 22*4
# C_tot = 2*4
# H_tot = 2*4
# trans_e = 2
# trans_a = 0
# trans_c = 1
# trans_h = 10
# foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
# plot(foi)


# all_cumms <- all_summs %>%
#   dplyr::select(SimType, Variable, Date, ptvalue) %>%
#   group_by(SimType, Variable) %>%
#   mutate(total = cumsum(ptvalue)) %>%
#   ungroup()
# 
# ggplot(all_cumms, aes(x = Date, y = total, color = SimType)) +
#   geom_line(size = 1) +
#   facet_wrap(~Variable, scales = "free_y")
# 
# test <- all_summs %>%
#   filter(Variable == "Acases") %>%
#   filter(SimType == "5No intervention")
# par(mfrow=c(1,2))
# plot(test$ptvalue)
# plot(cumsum(test$ptvalue))


# All scenarios, ribbon nat
# ggplot(all_summs, aes(x = Date,  fill = SimType)) +
#   geom_line(aes(y = ptvalue, alpha = Period)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", 
#              labeller = labeller(Variable = variable_names)) +
#   scale_fill_viridis_d(end = 0.8, name = "", labels= labs) +
#   scale_alpha_manual(values = c(1,0)) +
#   theme_minimal() +
#   ylab("Number of persons") +
#   scale_y_continuous(labels = scales::comma)+
#   theme_minimal()
# ggsave("./output/figures/all-projs-ribbon-nat.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
# 
# # All scenarios, ribbon log
# ggplot(all_summs, aes(x = Date,  fill = SimType)) +
#   geom_line(aes(y = ptvalue, alpha = Period)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
#   facet_wrap(~Variable, ncol = 3, scales = "free_y", 
#              labeller = labeller(Variable = variable_names)) +
#   scale_fill_viridis_d(end = 0.8, name = "", labels= labs) +
#   scale_alpha_manual(values = c(1,0)) +
#   theme_minimal() +
#   ylab("Number of persons") +
#   scale_y_continuous(labels = scales::comma, trans = "log", 
#                      limits = c(1,100000), breaks = c(10,100,1000,10000,100000))+
#   theme_minimal()
# ggsave("./output/figures/all-projs-ribbon-log.png", width = 8.5, height = 3, 
#        units = "in", dpi = 300)
