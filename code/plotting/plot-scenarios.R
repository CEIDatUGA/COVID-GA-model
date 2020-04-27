# plot-scenarios.R
# Script to summarise and plot the results from scenario simulations
# for the website.

library(tidyverse)
library(here)

# Load the most recent simulation files -----------------------------------
datasource <- "COV"
if(datasource == "COV") {
  fig_outpath <- here("output/figures/covidtracker-figures/")
}
if(datasource == "GAD") {
  fig_outpath <- here("output/figures/gadph-figures/")
}


# Can set filename_label manually, if needed
# filename_label <- "Georgia_GAD_2020-04-26-16-29"
filename_label <- "Georgia_COV_2020-04-26-20-20"  # fixed detection function
#filename_label <- "Georgia_COV_2020-04-26-17-53"  # estimated detection function
# filename_mif <- here('output', paste0(filename_label, '_mif.rds'))
filename_mif <- "./back-compatible-results-2020-04-26/mif-results.RDS"
pomp_model <- readRDS("./back-compatible-results-2020-04-26/pomp-model.RDS")

simfile <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))

covarfile <- here('output', paste0(filename_label, '_simulation-covariates.rds'))

out_sims <- readRDS(simfile)
covar_scens <- readRDS(covarfile)

# Also load mif summaries to get data
# all_mif <- readRDS(filename_mif)
# pomp_data <- all_mif$pomp_data %>%
#   dplyr::select(-time) %>%
#   rename("Acases" = cases,
#          "Bhosps" = hosps,
#          "Cdeaths" = deaths) %>%
#   gather(key = "Variable", value = "Value", -Date) %>%
#   mutate(SimType = "obs", Period = "Past")

end_date <- as.Date("2020-04-25")
dates <- seq.Date(as.Date("2020-03-01"), end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
pomp_data <- pomp_model@data %>%
  t() %>%
  as.data.frame() %>%
  dplyr::mutate(time = 1:n()) %>%
  right_join(dates_df, by = "time") %>%
  dplyr::select(Date, cases, hosps, deaths) %>%
  rename("Acases" = cases,
                  "Bhosps" = hosps,
                  "Cdeaths" = deaths) %>%
           gather(key = "Variable", value = "Value", -Date) %>%
           mutate(SimType = "obs", Period = "Past")


# Summarize the simulations -----------------------------------------------

sim_summs <- out_sims %>%
  dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
  rename("Acases" = cases,
         "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(lower = ceiling(quantile(Value, 0.1)),
            ptvalue = ceiling(quantile(Value, 0.5)),
            upper = ceiling(quantile(Value, 0.9))) %>%
  ungroup() 

# out_sims %>%
#   filter(rep_id == "4-1") %>%
#   filter(SimType == "no_intervention") %>%
#   mutate(allcases = R + D) %>%
#   pull(allcases) %>% plot()


cumulative_summs <- out_sims %>%
  dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
  rename("Acases" = cases,
         "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
  arrange(SimType, Variable, rep_id, Date) %>%
  group_by(SimType, Variable, rep_id) %>%
  mutate(Value = cumsum(Value)) %>%
  group_by(SimType, Variable, Date) %>%
  summarise(min = quantile(Value, 0.1),
            ptvalue = ceiling(quantile(Value, 0.5)),
            max = quantile(Value, 0.9)) %>%
  ungroup() %>%
  filter(Date == max(Date)) %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2)


# Make the plots ----------------------------------------------------------

mycols <- c("#a11c3e", "#5798d1", "#252525", "#319045",
            "#5e2b7b", "#e2908c", "#226e83")
# mycols <- c()

variable_names <- c(
  "Acases" = 'New cases',
  "Bhosps" ='New hospitalizations',
  "Cdeaths" = 'New deaths'
)

variable_names_cum <- c(
  "Acases" = 'Total cases',
  "Bhosps" ='Total hospitalizations',
  "Cdeaths" = 'Total deaths'
)

# Fits to data
end_date <- as.Date(max(pomp_data$Date))
dates <- seq.Date(as.Date("2020-03-01"), end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

fits <- sim_summs %>%
  filter(SimType == "status_quo") %>%
  filter(Period == "Past")

ggplot(fits, aes(x = Date)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  geom_point(data = pomp_data, aes(x = Date, y = Value)) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() -> pfits
ggsave(filename = paste0(fig_outpath, "/fits-to-data.png"), 
       plot = pfits,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)

all_summs <- sim_summs %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  mutate(Period = ifelse(Period == "Past", "APast", "BFuture"))

scen_labs <- c("1. Increased social distancing",
               "2. Status quo",
               "3. Relax social distancing",
               "4. Return to normal",
               "5. Continuously improving social distancing",
               "6. No intervention")

# OVERVIEW FIGURE
foredate <- all_summs %>%
  filter(Period == "APast") %>%
  filter(Date == max(Date)) %>%
  pull(Date) %>%
  unique() 
cum_summs_traj <- out_sims %>%
  dplyr::select(SimType, Date, cases, rep_id) %>%
  rename("Acases" = cases) %>%
  gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
  group_by(SimType, Variable, rep_id) %>%
  mutate(Value = cumsum(Value)) %>%
  group_by(SimType, Variable, Date) %>%
  summarise(ptvalue = ceiling(quantile(Value, 0.5))) %>%
  ungroup() %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2)
lp <- ggplot(cum_summs_traj, aes(x = Date, color = SimType)) +
  geom_line(aes(y = ptvalue), size = 1) +
  geom_vline(aes(xintercept = foredate), color = "grey35", linetype = 2) +
  scale_color_manual(values = mycols, name = "", labels= scen_labs) +
  theme_minimal() +
  ylab("Total number of\nconfirmed cases") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000000))+
  theme_minimal() +
  guides(color = FALSE)

rp <- ggplot(cumulative_summs %>%
               filter(Variable == "Acases"), 
             aes(x = SimType, color = SimType)) +
  geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
  geom_point(aes(y=ptvalue), color = "white", size = 1) +
  scale_color_manual(values = mycols) +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000000))+
  scale_x_discrete(labels = rep("", 6)) +
  theme_void() +
  guides(color = FALSE)

dates_df <- cum_summs_traj %>%
  dplyr::select(Date) %>%
  distinct() %>%
  mutate(time = 1:n())
covar_scensp <- covar_scens %>%
  left_join(dates_df, by = "time") %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2)
cp <- ggplot(covar_scensp, aes(x = Date, y = rel_beta_change, color = SimType)) +
  geom_line(size = 1) +
  geom_vline(aes(xintercept = foredate), color = "grey35", linetype = 2) +
  scale_color_manual(values = mycols, name = "", labels = scen_labs) +
  ylab("Social distancing\n(human movement\nas % of baseline)")+
  xlab("")+
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow=3))

plots <- cowplot::align_plots(cp, lp, align = "v", axis = "l")
bottom_row <- cowplot::plot_grid(plots[[2]], rp, align = "h", rel_widths = c(1, 0.1))
top_row <- cowplot::plot_grid(plots[[1]], NULL, rel_widths = c(1,0.1))
landfig <- cowplot::plot_grid(top_row, bottom_row, ncol = 1)
ggsave(filename = paste0(fig_outpath, "/landing-page-fig.png"), 
       plot = landfig,
       width = 8, height = 6, 
       units = "in", dpi = 300)

# Cumulative min/maxes 6 weeks out
scen_labs <- c("1. Increased social distancing",
               "2. Status quo",
               "3. Relax social distancing",
               "4. Return to normal",
               "5. Continuously improving social distancing",
               "6. No intervention")

title <- paste("Range of projections by", max(cumulative_summs$Date))
ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
  geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
  geom_point(aes(y=ptvalue), color = "white", size = 1) +
  facet_wrap(~Variable) +
  # scale_colour_viridis_d(end = 0.8) +
  scale_color_manual(values = mycols) +
  facet_wrap(~Variable, ncol = 3, scales = "free_x", labeller = labeller(Variable = variable_names_cum)) +
  ylab("Number of persons") +
  xlab("") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = scen_labs) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  coord_flip() +
  ggtitle(title, subtitle = "scenarios are described above") -> cumrangenat
ggsave(filename = paste0(fig_outpath, "/cumulative-forecasts.png"), 
       plot = cumrangenat,
       width = 8.5, height = 4, 
       units = "in", dpi = 300)

ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
  geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
  geom_point(aes(y=ptvalue), color = "white", size = 1) +
  facet_wrap(~Variable) +
  # scale_colour_viridis_d(end = 0.8) +
  scale_color_manual(values = mycols) +
  facet_wrap(~Variable, ncol = 3, scales = "free_x", labeller = labeller(Variable = variable_names_cum)) +
  ylab("Number of persons") +
  xlab("") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1000,10000000), breaks = c(1000,10000,100000,1000000))+
  scale_x_discrete(labels = scen_labs) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  coord_flip() +
  ggtitle(title, subtitle = "scenarios are described above") -> cumrangelog
ggsave(filename = paste0(fig_outpath, "/cumulative-forecasts-log.png"),
       plot = cumrangelog,
       width = 8.5, height = 4, 
       units = "in", dpi = 300)


labs <- c("1. Increased social distancing",
          "2. Status quo",
          "3. Relax social distancing",
          "4. Return to normal",
          "5. Continuously improving social distancing",
          "6. No intervention")
# All scenarios - line, natural
ggplot(all_summs, aes(x = Date, color = SimType)) +
  geom_line(aes(y = ptvalue)) +
  geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", 
             labeller = labeller(Variable = variable_names)) +
  scale_color_manual(values = mycols, name = "", labels= labs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "top") -> allscensnat
ggsave(paste0(fig_outpath, "/all-projs-line-nat.png"), 
       plot = allscensnat,
       width = 8.5, height = 4, 
       units = "in", dpi = 300)


# All scenarios - line, log
ggplot(all_summs, aes(x = Date,  color = SimType)) +
  geom_line(aes(y = ptvalue)) +
  geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y", 
             labeller = labeller(Variable = variable_names)) +
  scale_color_manual(values = mycols, name = "", labels= labs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,100000), breaks = c(10,100,1000,10000,100000))+
  theme_minimal() +
  theme(legend.position = "top") -> allscenslog
ggsave(paste0(fig_outpath, "/all-projs-line-log.png"), 
       plot = allscenslog,
       width = 8.5, height = 4, 
       units = "in", dpi = 300)



scen_labs <- c("1Increased social distancing" = "1. Increased\nsocial distancing",
               "2Status quo" = "2. Status quo",
               "3Relax social distancing" = "3. Relax\nsocial distancing",
               "4Return to normal" = "4. Return to normal",
               "5Continuously improving social distancing" = "5. Continuously\nimproving\nsocial distancing",
               "6No intervention" = "6. No intervention")

## NATURAL SCALE
# Cases
pomp_data <- pomp_data %>%
  mutate(Period = "APast")
collabs <- c("Past", "Future")
ggplot(all_summs %>%
         filter(Variable == "Acases"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Acases") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily cases") +
  coord_cartesian(ylim = c(0, 30000)) -> pcasesnat
ggsave(paste0(fig_outpath, "/cases-trajs-nat.png"), 
       plot = pcasesnat,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Hosps
ggplot(all_summs %>%
         filter(Variable == "Bhosps"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Bhosps") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily hospitalizations") +
  coord_cartesian(ylim = c(0, 10000)) -> phospsnat
ggsave(paste0(fig_outpath, "/hosps-trajs-nat.png"), 
       plot = phospsnat,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Deaths
ggplot(all_summs %>%
         filter(Variable == "Cdeaths"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Cdeaths") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily deaths") +
  coord_cartesian(ylim = c(0, 6000)) -> pdeathsnat
ggsave(paste0(fig_outpath, "/deaths-trajs-nat.png"), 
       plot = pdeathsnat,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)


## LOG SCALE
# Cases
ggplot(all_summs %>%
         filter(Variable == "Acases"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Acases") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,200000), breaks = c(10,100,1000,10000,100000)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily cases") -> pcaseslog
ggsave(paste0(fig_outpath, "/cases-trajs-log.png"), 
       plot = pcaseslog,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Hosps
ggplot(all_summs %>%
         filter(Variable == "Bhosps"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Bhosps") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,100000), breaks = c(10,100,1000,10000,100000)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily hospitalizations") -> phospslog
ggsave(paste0(fig_outpath, "/hosps-trajs-log.png"), 
       plot = phospslog,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)

# Deaths
ggplot(all_summs %>%
         filter(Variable == "Cdeaths"),
       aes(x = Date, color = Period, fill = Period)) +
  geom_point(data = pomp_data %>%
               filter(Variable == "Cdeaths") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log", 
                     limits = c(1,20000), breaks = c(10,100,1000,10000,100000)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily deaths") -> pdeathslog
ggsave(paste0(fig_outpath, "/deaths-trajs-log.png"), 
       plot = pdeathslog,
       width = 8.5, height = 3, 
       units = "in", dpi = 300)



# Stats for press release -------------------------------------------------


pomp_data %>% 
  arrange(Date) %>%
  group_by(Variable) %>%
  mutate(Value = ifelse(is.na(Value), 0, Value)) %>%
  mutate(cum_sum = cumsum(Value)) %>%
  ungroup() %>%
  filter(Period == "APast") %>% 
  filter(Date == max(Date)) %>%
  dplyr::select(-Value) %>%
  spread(Variable, cum_sum) -> sdist

sim_summs %>%
  filter(SimType == "no_intervention") %>%
  dplyr::select(-lower, -upper) %>%
  arrange(Date) %>%
  group_by(Variable) %>%
  mutate(cum_sum = cumsum(ptvalue)) %>%
  ungroup() %>%
  filter(Period == "Past") %>% 
  filter(Date == max(Date)) %>%
  dplyr::select(-ptvalue) %>%
  spread(Variable, cum_sum) -> noint

 1 - (sdist$Acases / noint$Acases) 
noint$Cdeaths - sdist$Cdeaths


cumulative_summs %>%
  filter(SimType == "3Relax social distancing") %>%
  dplyr::select(-min, -max) %>%
  spread(Variable, ptvalue) -> relax

cumulative_summs %>%
  filter(SimType == "2Status quo") %>%
  dplyr::select(-min, -max) %>%
  spread(Variable, ptvalue) -> sq

relax$Acases-sq$Acases
relax$Cdeaths-sq$Cdeaths

cumulative_summs %>%
  filter(SimType == "6No intervention") %>%
  dplyr::select(-min, -max) %>%
  spread(Variable, ptvalue) -> noi

cumulative_summs %>%
  filter(SimType == "1Increased social distancing") %>%
  dplyr::select(-min, -max) %>%
  spread(Variable, ptvalue) -> good

cumulative_summs %>%
  filter(SimType == "4Return to normal") %>%
  dplyr::select(-min, -max) %>%
  spread(Variable, ptvalue) -> normal

noint$Acases - sdist$Acases
noint$Cdeaths - sdist$Cdeaths
# noi$Acases-sq$Acases
# noi$Cdeaths-sq$Cdeaths
relax$Acases/sq$Acases
relax$Acases/good$Acases
relax$Cdeaths/sq$Cdeaths
relax$Cdeaths/good$Cdeaths


cumulative_summs2 <- out_sims %>%
  dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
  rename("Acases" = cases,
         "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
  arrange(SimType, Variable, rep_id, Date) %>%
  group_by(SimType, Variable, rep_id) %>%
  filter(Date >= "2020-05-26") %>%
  group_by(SimType, Variable, rep_id) %>%
  mutate(Value = cumsum(Value)) %>%
  group_by(SimType, Variable, Date) %>%
  summarise(min = quantile(Value, 0.1),
            ptvalue = ceiling(quantile(Value, 0.5)),
            max = quantile(Value, 0.9)) %>%
  ungroup() %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  filter(Date == min(Date) | Date == max(Date))

cumulative_summs2 %>% filter(SimType == "2Status quo")
cumulative_summs2 %>% filter(SimType == "6No intervention")
