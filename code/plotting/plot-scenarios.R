# plot-scenarios.R
# Script to summarise and plot the results from scenario simulations
# for the website.

library(tidyverse)
library(here)

# Load the most recent simulation files -----------------------------------
datasource <- "COV"
if(datasource == "COV") {
  fig_outpath <- here("output/figures/covidtracker-figures/")
  most_recent_files <- tail(list.files(path = here("output"), "Georgia_COV"), 3)
  
  # For web content
  # most_recent_files <- tail(list.files(path = here("output"), "Georgia_COV"), 3*10)[4:6]
}
if(datasource == "GAD") {
  fig_outpath <- here("output/figures/gadph-figures/")
}


# Can set filename_label manually, if needed

filename_mif <- most_recent_files[grep(pattern = "mif", most_recent_files)]
filename_sims <- most_recent_files[grep(pattern = "simulation-scenarios", most_recent_files)]
filename_covs <- most_recent_files[grep(pattern = "simulation-covariates", most_recent_files)]

simfile <- here('output', filename_sims)
covarfile <- here('output', filename_covs)
miffile <- here("output", filename_mif)
out_sims <- readRDS(simfile)
covar_scens <- readRDS(covarfile)
all_mif <- readRDS(miffile)
pomp_model <- all_mif$pomp_model
pomp_data <- all_mif$pomp_data %>%
  dplyr::select(-time) %>%
  rename("Acases" = cases,
         "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(SimType = "obs", Period = "Past")



# Summarize the simulations -----------------------------------------------

sim_summs <- out_sims %>%
  dplyr::select(SimType, Period, Date, cases, H_new, deaths) %>%
  rename("Acases" = cases,
         "Bhosps" = H_new,
         "Cdeaths" = deaths) %>%
  # rename("Acases" = C_new,
  #        "Bhosps" = H_new,
  #        "Cdeaths" = D_new) %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(lower = ceiling(quantile(Value, 0.025)),
            ptvalue = ceiling(mean(Value)),
            # ptvalue = median(Value),
            upper = ceiling(quantile(Value, 0.975))) %>%
  ungroup()

# filter out scenarios
sim_summs <- sim_summs %>%  filter(SimType != "linear_decrease_sd")

cumulative_summs <- out_sims %>%
  dplyr::select(SimType, Date, cases, deaths, rep_id) %>%
  rename("Acases" = cases,
         # "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
  arrange(SimType, Variable, rep_id, Date) %>%
  group_by(SimType, Variable, rep_id) %>%
  mutate(Value = cumsum(Value)) %>%
  group_by(SimType, Variable, Date) %>%
  summarise(min = quantile(Value, 0.025),
            ptvalue = ceiling(mean(Value)),
            max = quantile(Value, 0.975)) %>%
  ungroup() %>%
  filter(Date == max(Date)) %>%
  mutate(#SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         #SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         # SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2)

# filter out scenarios
cumulative_summs <- cumulative_summs %>%  filter(SimType != "3Relax social distancing")


# Make the plots ----------------------------------------------------------

# mycols <- c("#a11c3e", "#5798d1", "#252525", "#319045", "#5e2b7b", "#e2908c", "#226e83")
# mycols <- c()
mycols <- c("#5798d1", "#319045", "#e2908c", "#a11c3e", "#226e83", "#5e2b7b", "#252525")
names(mycols) <- c('lightblue', 'green', 'pink', 'red', 'blue', 'purple', 'black')
mycols.vec <- mycols
names(mycols.vec) <- NULL
mycols.vec.filt <- mycols[c('lightblue','green','red')]
names(mycols.vec.filt) <- NULL

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


# Fits to data --------------------------------------------------------------------------------

end_date <- as.Date(max(pomp_data$Date))
dates <- seq.Date(as.Date("2020-03-01"), end_date, "days")
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)

fits <- sim_summs %>%
  filter(SimType == "status_quo") %>%
  filter(Period == "Past")

# Diagnostic plot
fitreps <- out_sims %>%
  filter(SimType == "status_quo") %>%
  filter(Period == "Past") %>%
  dplyr::select(mle_id, SimType, Date, cases, deaths) %>%
  rename("Acases" = cases,
         # "Bhosps" = hosps,
         "Cdeaths" = deaths) %>%
  # dplyr::select(mle_id, SimType, Date, C_new, D_new) %>%
  # rename("Acases" = C_new,
  #        "Cdeaths" = D_new) %>%
  gather(key = "Variable", value = "Value",-mle_id, -SimType, -Date) %>%
  group_by(Date, Variable, mle_id) %>%
  summarise(Value = ceiling(mean(Value))) %>%
  ungroup()

meanline <- fitreps %>%
  group_by(Date, Variable) %>%
  summarise(Value = ceiling(mean(Value))) %>%
  ungroup()

medline <- fitreps %>%
  group_by(Date, Variable) %>%
  summarise(Value = ceiling(median(Value))) %>%
  ungroup()

ggplot() +
  geom_line(data = fitreps, aes(x = Date, y = Value, group = mle_id), 
            alpha = 0.2) +
  geom_line(data = meanline, aes(x = Date, y = Value), color = "blue") +
  geom_line(data = medline, aes(x = Date, y = Value), color = "red") +
  geom_line(data = pomp_data %>% filter(Variable != "Bhosps"), aes(x = Date, y = Value), color = "black") +
  facet_wrap(~Variable, scales = "free_y", ncol = 1)

## Function makes and save png and html plots, and returns html plot

pomp_data_filt <- pomp_data %>% filter(Variable != "Bhosps")
fits_filt <- fits %>% filter(Variable != "Bhosps")
plot_fits <- function() {
  ggplot(fits_filt) +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(alpha=1, aes(x = Date, y = ptvalue, group = Variable,
                           text=sprintf("Date: %s<br>Fit: %s", Date, ptvalue)
    )
    ) +
    # geom_point(data = pomp_data, alpha=.5, aes(x = Date, y = Value)) +
    ## changed points to bars
    geom_col(data = pomp_data_filt,
             alpha=.5, aes(x = Date, y = Value,
                           text=sprintf("Date: %s<br>Data: %s", Date, Value))) +
    facet_wrap(~Variable, ncol = 3, scales = "free_y",
               labeller = labeller(Variable = variable_names)) +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = -45)) +
    xlab("") -> pfits

  ## make interactive
  plotly_pfits <- pfits %>% plotly::ggplotly(tooltip = 'text') 
  # %>% 
  #   layout(xaxis=list(tickangle = 45))

  ## save as Widget
  plotly_pfits %>% htmlwidgets::saveWidget(file = paste0(fig_outpath, "fits-to-data.html"))

  ## save as image
  ggsave(filename = paste0(fig_outpath, "/fits-to-data.png"),
         plot = pfits,
         width = 8.5, height = 3,
         units = "in", dpi = 300)

  plotly_pfits
}

# plot_fits()

# OVERVIEW FIGURE --------------------------------------------------------------------------------

all_summs <- sim_summs %>%
  mutate(#SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         #SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         # SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  mutate(Period = ifelse(Period == "Past", "APast", "BFuture"))

# filter out scenarios
all_summs <- all_summs %>% filter(SimType != "3Relax social distancing")

scen_labs <- c("1. Increase social distancing",
               "2. Maintain social distancing (status quo)",
               #"3. Relax social distancing",
               "3. Return to normal")
               # "5. What if social distancing had continued to increase?",
               #"6. What if social distancing had never begun?")


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
  summarise(ptvalue = ceiling(mean(Value))) %>%
  ungroup() %>%
  mutate(#SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         #SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         # SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  rename(Cases = ptvalue)

# filter out scenarios
cum_summs_traj <- cum_summs_traj %>% filter(SimType != "3Relax social distancing")

## line subplot
# lp <- ggplot(cum_summs_traj, aes(x = Date, color = SimType)) +
#   geom_line(aes(x = x, y = y), size = 1) +
#   geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
#   scale_color_manual(values = mycols, name = "", labels= scen_labs) +
#   theme_minimal() +
#   ylab("Total number of\nconfirmed cases") +
#   scale_y_continuous(labels = scales::comma, limits = c(0, 250000))+
#   theme_minimal() +
#   guides(color = FALSE)

## alt line subplot

cumsum.ylim <- c(0, plyr::round_any(max(cumulative_summs$max), 20000, f = ceiling))

# geom_line(data = df[3:4,], aes(x = x, y = y), color = 'blue', size = 3)
cases.ylim <- c(0,max(cumulative_summs$max))
lp <- ggplot(cum_summs_traj, aes(x = Date, y = Cases)) +
  # geom_line(data = filter(cum_summs_traj, SimType == '5Continuously improving social distancing'),
  #           color = mycols['blue'], size = 1, linetype = 2) +
  # geom_line(data = filter(cum_summs_traj, SimType == '6No intervention'),
  #           color = mycols['purple'], size = 1, linetype = 2) +
  geom_line(data = filter(cum_summs_traj, SimType == '1Increased social distancing'),
            color = mycols['lightblue'], size = 1, linetype = 1) +
  geom_line(data = filter(cum_summs_traj, SimType == '2Status quo'),
            color = mycols['green'], size = 1, linetype = 1) +
  # geom_line(data = filter(cum_summs_traj, SimType == '3Relax social distancing'),
  #           color = mycols['pink'], size = 1, linetype = 1) +
  geom_line(data = filter(cum_summs_traj, SimType == '4Return to normal'),
            color = mycols['red'], size = 1, linetype = 1) +
  geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
  ylab("Total number of\nconfirmed cases") +
  scale_y_continuous(labels = scales::comma, limits = cumsum.ylim) +
  theme_minimal()

### plotly
plotly_lp <- lp %>% plotly::ggplotly() %>%
  layout(showlegend=FALSE,
         yaxis = list(range = cumsum.ylim),
         xaxis = list(showline = TRUE),
         annotations= list(yref = 'paper', xref = "x", y = .25, x = as.numeric(foredate),
                           text = format(foredate, format="%b %d"),
                           showarrow = FALSE, textangle=-90, xanchor = 'right')
         )

## range subplot

rp <- ggplot(cumulative_summs %>%
               filter(Variable == "Acases"),
             aes(x = SimType, color = SimType,
                 text = sprintf("%s<br>Max: %s<br>Min: %s<br>Mean: %s", SimType, max, min, ptvalue))) +
  geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
  geom_point(aes(y=ptvalue), color = "white", size = 1) +
  scale_color_manual(values = mycols.vec.filt) +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::comma, limits = cumsum.ylim)+
  scale_x_discrete(labels = rep("", 6)) +
  theme_void() +
  guides(color = FALSE)

### plotly
plotly_rp <- rp %>% plotly::ggplotly(tooltip='text') %>% 
  layout(showlegend=FALSE,
         yaxis = list(range = cumsum.ylim,
                      showline = TRUE,
                      side = "right",
                      title = "Final range across\nmultiple simulations",
                      titlefont = list(size = 12)),
         xaxis = list(showgrid = FALSE,
                      showline = TRUE))

dates_df <- cum_summs_traj %>%
  dplyr::select(Date) %>%
  distinct() %>%
  mutate(time = 1:n())
covar_scensp <- covar_scens %>%
  left_join(dates_df, by = "time") %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         #SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         # SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2)

# filter out scenarios
covar_scensp <- covar_scensp %>% filter(SimType != "3Relax social distancing")

## conditions subplot
# cp <- ggplot(covar_scensp, aes(x = Date, y = rel_beta_change, color = SimType)) +
#   geom_line(size = 1) +
#   geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
#   scale_color_manual(values = mycols.vec, name = "", labels = scen_labs) +
#   ylab("Human movement\n(% of normal)") +
#   xlab("")+
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#   theme_minimal()
# +
#   theme(legend.position = "top") +
#   guides(color = guide_legend(nrow=3))

## alt conditions subplot

covar_scensp <- covar_scensp %>% rename(Movement = rel_beta_change)

cp <- ggplot(covar_scensp,
             aes(x = Date, y = Movement, text = )) +
  # geom_line(data = filter(covar_scensp, SimType == '5Continuously improving social distancing'),
  #           color = mycols['blue'], size = 1, linetype = 2) +
  # geom_line(data = filter(covar_scensp, SimType == '6No intervention'),
  #           color = mycols['purple'], size = 1, linetype = 2) +
  geom_line(data = filter(covar_scensp, SimType == '1Increased social distancing'),
            color = mycols['lightblue'], size = 1, linetype = 1) +
  geom_line(data = filter(covar_scensp, SimType == '2Status quo'),
            color = mycols['green'], size = 1, linetype = 1) +
  # geom_line(data = filter(covar_scensp, SimType == '3Relax social distancing'),
  #           color = mycols['pink'], size = 1, linetype = 1) +
  geom_line(data = filter(covar_scensp, SimType == '4Return to normal'),
            color = mycols['red'], size = 1, linetype = 1) +
  geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
  # geom_text(aes(x=as.numeric(foredate), label=foredate, y=750000), angle=90, text=element_text(size=11))
  xlab("") +
  ylab("Human movement\n(% of normal)") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow=3))



### plotly
plotly_cp <- cp %>% plotly::ggplotly() %>% 
  layout(showlegend=FALSE,
         annotations= list(yref = 'y', xref = "x", y = .15, x = as.numeric(foredate),
                           text = format(foredate, format="%b %d"),
                           showarrow = FALSE, textangle=-90, xanchor = 'right'))

## plotly scenario labels
scen_incr_end <- covar_scensp %>% filter(SimType=='1Increased social distancing') %>% 
  select(Movement) %>% tail(1) %>% pull()
scen_sq_end <- covar_scensp %>% filter(SimType=='2Status quo') %>% 
  select(Movement) %>% tail(1) %>% pull()
scen_norm_end <- covar_scensp %>% filter(SimType=='4Return to normal') %>% 
  select(Movement) %>% tail(1) %>% pull()

plotly_scenlabs <- plotly_empty() %>% 
  layout(showlegend=FALSE,
         yaxis = list(range = c(0,1)),
         xaxis = list(range = c(0,1), showline = FALSE),
         annotations = list(list(yref = 'y', xref = "x", x = 0, showarrow = FALSE, xanchor = 'left',
                                 y = scen_incr_end, text = c('Increase distancing'),
                                 font=list(color = mycols['lightblue'], size = 8)),
                            list(yref = 'y', xref = "x", x = 0, showarrow = FALSE, xanchor = 'left',
                                 y = scen_sq_end, text = c('Maintain distancing'),
                                 font=list(color = mycols['green'], size = 8)),
                            list(yref = 'y', xref = "x", x = 0, showarrow = FALSE, xanchor = 'left',
                                 y = scen_norm_end, text = c('Return to Normal'),
                                 font=list(color = mycols['red'], size = 8))
                            )
         )

## make plotly dashboard and save as Widget

## Function makes and saves html plot, returns html plot
plot_summaryfig <- function() {
  p_landfig <- plotly::subplot(plotly_cp, plotly_scenlabs, plotly_lp, plotly_rp,
                               nrows = 2, heights = c(.4,.6), widths = c(.85,.15),
                               shareX = FALSE, titleY = TRUE
                               ) %>%
    layout(showlegend=FALSE)

  p_landfig %>% htmlwidgets::saveWidget(file = paste0(fig_outpath, "landing-page-fig.html"))
  p_landfig
}

### static layout
plots <- cowplot::align_plots(cp, lp, align = "v", axis = "l")
bottom_row <- cowplot::plot_grid(plots[[2]], rp, align = "h", rel_widths = c(1, 0.1))
top_row <- cowplot::plot_grid(plots[[1]], NULL, rel_widths = c(1,0.1))
landfig <- cowplot::plot_grid(top_row, bottom_row, ncol = 1)
ggsave(filename = paste0(fig_outpath, "/landing-page-fig.png"),
       plot = landfig,
       width = 8, height = 6,
       units = "in", dpi = 300)



# Cumulative min/maxes 6 weeks out
title <- paste("Range of projections by", max(cumulative_summs$Date))
ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
  geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
  geom_point(aes(y=ptvalue), color = "white", size = 1) +
  facet_wrap(~Variable) +
  # scale_colour_viridis_d(end = 0.8) +
  scale_color_manual(values = mycols.vec.filt) +
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
  scale_color_manual(values = mycols.vec.filt) +
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

# labs <- scen_labs

# All scenarios - line, natural
ggplot(all_summs, aes(x = Date, color = SimType)) +
  geom_line(aes(y = ptvalue)) +
  geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y",
             labeller = labeller(Variable = variable_names)) +
  scale_color_manual(values = mycols.vec.filt, name = "", labels = scen_labs) +
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
  scale_color_manual(values = mycols.vec.filt, name = "", labels = scen_labs) +
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

# 2 scenarios - line, natural, with ribbons

two_summs <- all_summs %>% 
  filter(SimType == "4Return to normal" | SimType == "2Status quo")
ggplot(two_summs, aes(x = Date, color = SimType)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  facet_wrap(~Variable, ncol = 3, scales = "free_y",
             labeller = labeller(Variable = variable_names)) +
  scale_color_manual(values = mycols.vec, name = "", labels = scen_labs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "top") -> twoscensnat
ggsave(paste0(fig_outpath, "/two-projs-line-nat.png"),
       plot = twoscensnat,
       width = 8.5, height = 4,
       units = "in", dpi = 300)

scen_labs <- c("1Increased social distancing" = "1. Increase social distancing",
               "2Status quo" = "2. Maintain social distancing (status quo)",
               #"3Relax social distancing" = "3. Relax\nsocial distancing",
               "4Return to normal" = "3. Return to normal")
               # "5Continuously improving social distancing" = "5. What if\nsocial distancing had\ncontinued to increase?",
               #"6No intervention" = "6. What if\nsocial distancing had\nnever begun?")

## NATURAL SCALE
# Infections
infection_summaries <- out_sims %>%
  dplyr::select(SimType, Period, Date, S) %>%
  mutate(N = 10600170) %>%
  mutate(Infections = N - S) %>%
  group_by(SimType, Period, Date) %>%
  summarise(lower = ceiling(quantile(Infections, 0.1)),
            ptvalue = ceiling(mean(Infections)),
            upper = ceiling(quantile(Infections, 0.9))) %>%
  ungroup() %>%
  mutate(#SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
         #SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
         # SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType),
         SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  mutate(Period = ifelse(Period == "Past", "APast", "BFuture"))

# filter out scenarios
infection_summaries <- infection_summaries %>% filter(SimType != "3Relax social distancing")

collabs <- c("Past", "Future")
ggplot(infection_summaries, aes(x = Date, color = Period, fill = Period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(~SimType, labeller = labeller(SimType = scen_labs), ) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total number of infections") -> pinfectionsnat
ggsave(paste0(fig_outpath, "/infections-trajs-nat.png"),
       plot = pinfectionsnat,
       width = 8.5, height = 4,
       units = "in", dpi = 300)

# Cases

pomp_data <- pomp_data %>%
  mutate(Period = "APast")
collabs <- c("Past", "Future")
ggplot(all_summs %>%
         filter(Variable == "Acases") %>% 
         mutate(lower = if_else(Period=='APast',ptvalue,lower)) %>% 
         mutate(upper = if_else(Period=='APast',ptvalue,upper)),
       aes(x = Date, color = Period, fill = Period)) +
  geom_line(data = pomp_data %>%
              filter(Variable == "Acases") %>%
              dplyr::select(-SimType),
            aes(x = Date, y = Value), size = 0.25, color = "grey35", linetype = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue), size = 0.25) +
  geom_vline(aes(xintercept = foredate+0.5), color = "grey") +
  facet_wrap(~SimType, ncol = 1, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal(base_line_size = 0.25) +
  theme(legend.position = "top") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily cases") +
  coord_cartesian(ylim = c(0, 3000)) -> pcasesnat
ggsave(paste0(fig_outpath, "/cases-trajs-nat.png"),
       plot = pcasesnat,
       width = 5, height = 6,
       units = "in", dpi = 300)

# Hosps
ggplot(all_summs %>%
         filter(Variable == "Bhosps") %>% 
         mutate(lower = if_else(Period=='APast',ptvalue,lower)) %>% 
         mutate(upper = if_else(Period=='APast',ptvalue,upper)),
       aes(x = Date, color = Period, fill = Period)) +
  # geom_point(data = pomp_data %>%
  #              filter(Variable == "Bhosps") %>%
  #              dplyr::select(-SimType),
  #            aes(x = Date, y = Value), size = 1, color = "grey35") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_wrap(~SimType, ncol = 1, labeller = labeller(SimType = scen_labs)) +
  # facet_grid(SimType~., labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily hospitalizations") +
  coord_cartesian(ylim = c(0, 1000)) -> phospsnat
ggsave(paste0(fig_outpath, "/hosps-trajs-nat.png"),
       plot = phospsnat,
       width = 8.5, height = 3,
       units = "in", dpi = 300)

# Deaths
ggplot(all_summs %>%
         filter(Variable == "Cdeaths") %>% 
         mutate(lower = if_else(Period=='APast',ptvalue,lower)) %>% 
         mutate(upper = if_else(Period=='APast',ptvalue,upper)),
       aes(x = Date, color = Period, fill = Period)) +
  geom_line(data = pomp_data %>%
               filter(Variable == "Cdeaths") %>%
               dplyr::select(-SimType),
             aes(x = Date, y = Value), size = 0.25, color = "grey35", linetype = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue), size = 0.25) +
  geom_vline(aes(xintercept = foredate+0.5), color = "grey") +
  facet_wrap(~SimType, ncol = 1, labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal(base_line_size = 0.25) +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("New daily deaths") +
  coord_cartesian(ylim = c(0, 150)) -> pdeathsnat
ggsave(paste0(fig_outpath, "/deaths-trajs-nat.png"),
       plot = pdeathsnat,
       width = 5, height = 6,
       units = "in", dpi = 300)


## TODO remove reibbons from past for all log plots
## LOG SCALE
ggplot(infection_summaries, aes(x = Date, color = Period, fill = Period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = ptvalue)) +
  facet_grid(SimType~., labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log",
                     limits = c(100,10000000), breaks = c(100,1000,10000,100000,1000000,10000000)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total number of infections") -> pinfectionslog
ggsave(paste0(fig_outpath, "/infections-trajs-log.png"),
       plot = pinfectionslog,
       width = 8.5, height = 3,
       units = "in", dpi = 300)

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
  facet_grid(SimType~., labeller = labeller(SimType = scen_labs)) +
  scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
  scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
  theme_minimal() +
  ylab("Number of persons") +
  scale_y_continuous(labels = scales::comma, trans = "log",
                     limits = c(1,3300000), breaks = c(10,100,1000,10000,100000)) +
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
  facet_grid(SimType~., labeller = labeller(SimType = scen_labs)) +
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
  facet_grid(SimType~., labeller = labeller(SimType = scen_labs)) +
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

# 
# pomp_data %>%
#   arrange(Date) %>%
#   group_by(Variable) %>%
#   mutate(Value = ifelse(is.na(Value), 0, Value)) %>%
#   mutate(cum_sum = cumsum(Value)) %>%
#   ungroup() %>%
#   filter(Period == "APast") %>%
#   filter(Date == max(Date)) %>%
#   dplyr::select(-Value) %>%
#   spread(Variable, cum_sum) -> sdist
# 
# sim_summs %>%
#   filter(SimType == "no_intervention") %>%
#   dplyr::select(-lower, -upper) %>%
#   arrange(Date) %>%
#   group_by(Variable) %>%
#   mutate(cum_sum = cumsum(ptvalue)) %>%
#   ungroup() %>%
#   filter(Period == "Past") %>%
#   filter(Date == max(Date)) %>%
#   dplyr::select(-ptvalue) %>%
#   spread(Variable, cum_sum) -> noint
# 
#  1 - (sdist$Acases / noint$Acases)
# noint$Cdeaths - sdist$Cdeaths
# 
# 
# cumulative_summs %>%
#   filter(SimType == "3Relax social distancing") %>%
#   dplyr::select(-min, -max) %>%
#   spread(Variable, ptvalue) -> relax
# 
# cumulative_summs %>%
#   filter(SimType == "2Status quo") %>%
#   dplyr::select(-min, -max) %>%
#   spread(Variable, ptvalue) -> sq
# 
# relax$Acases-sq$Acases-sdist$Acases
# relax$Cdeaths-sq$Cdeaths-sdist$Cdeaths
# 
# cumulative_summs %>%
#   filter(SimType == "6No intervention") %>%
#   dplyr::select(-min, -max) %>%
#   spread(Variable, ptvalue) -> noi
# 
# cumulative_summs %>%
#   filter(SimType == "1Increased social distancing") %>%
#   dplyr::select(-min, -max) %>%
#   spread(Variable, ptvalue) -> good
# 
# cumulative_summs %>%
#   filter(SimType == "4Return to normal") %>%
#   dplyr::select(-min, -max) %>%
#   spread(Variable, ptvalue) -> normal
# 
# noint$Acases - sdist$Acases
# noint$Cdeaths - sdist$Cdeaths
# # noi$Acases-sq$Acases
# # noi$Cdeaths-sq$Cdeaths
# relax$Acases/sq$Acases
# relax$Acases/good$Acases
# relax$Cdeaths/sq$Cdeaths
# relax$Cdeaths/good$Cdeaths
# 
# 
# cumulative_summs2 <- out_sims %>%
#   dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
#   rename("Acases" = cases,
#          "Bhosps" = hosps,
#          "Cdeaths" = deaths) %>%
#   gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
#   arrange(SimType, Variable, rep_id, Date) %>%
#   group_by(SimType, Variable, rep_id) %>%
#   filter(Date >= "2020-05-26") %>%
#   group_by(SimType, Variable, rep_id) %>%
#   mutate(Value = cumsum(Value)) %>%
#   group_by(SimType, Variable, Date) %>%
#   summarise(min = quantile(Value, 0.1),
#             ptvalue = ceiling(quantile(Value, 0.5)),
#             max = quantile(Value, 0.9)) %>%
#   ungroup() %>%
#   mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
#          SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
#          SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
#          SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
#          SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
#          SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
#   mutate(SimType = SimType2) %>%
#   dplyr::select(-SimType2) %>%
#   filter(Date == min(Date) | Date == max(Date))
# 
# cumulative_summs2 %>% filter(SimType == "2Status quo")
# cumulative_summs2 %>% filter(SimType == "6No intervention")
