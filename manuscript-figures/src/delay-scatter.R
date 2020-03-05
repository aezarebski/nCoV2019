#' -----------------------------------------------------------------------------
#' This script looks at the cleaned data sets and produces a plot comparing the
#' time at which an case was confirmed and the time of either symptom onset or
#' hospitalisation.
#'
#' -----------------------------------------------------------------------------
#' Usage:
#'
#' $ Rscript src/delay-scatter.R
#' $ evince out/delay-scatter-outside-hubei-confirmation.pdf
#'
#' -----------------------------------------------------------------------------
#' ChangeLog:
#'
#' - 05-03-20
#'   + Initial draft.
#'
#' -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
source("../covid19/src/tools.cleaner.R")


delay_figures <- function(clean_data, location_string, literal_na_values = FALSE) {
    chosen_theme <- theme_classic() + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0))

    if (class(clean_data$date_onset_symptoms) != "character") {
        browser()
        stop()
    }
  onset_dates <- strpdate(clean_data$date_onset_symptoms, literal_na_values)
  hosp_dates <- strpdate(clean_data$date_admission_hospital, literal_na_values)
  plot_df <- data.frame(onset_date = onset_dates, delay = hosp_dates - onset_dates) %>% filter(!is.na(delay))
  delay_hosp_plot <- ggplot(plot_df, aes(x = as.Date(onset_date, origin = "1970-01-01"), y = delay)) +
      geom_jitter(width = 0.5, height = 0.5) +
      labs(x = "Date of symptom onset", y = "Days after symptom onset until hospitalisation") +
      ggtitle(sprintf("Delay from symptom onset to\nhospital admission %s", location_string)) +
      chosen_theme

    onset_dates <- strpdate(clean_data$date_onset_symptoms, literal_na_values)
    conf_dates <- strpdate(clean_data$date_confirmation, literal_na_values)
  plot_df <- data.frame(onset_date = onset_dates, delay = conf_dates - onset_dates) %>% filter(!is.na(delay))
  delay_conf_plot <- ggplot(plot_df, aes(x = as.Date(onset_date, origin = "1970-01-01"), y = delay)) +
      geom_jitter(width = 0.5, height = 0.5) +
      labs(x = "Date of symptom onset", y = "Days after symptom onset for confirmation") +
      ggtitle(sprintf("Delay from symptom onset to\nconfirmation %s", location_string)) +
      chosen_theme

  return(list(hospitalisation = delay_hosp_plot, confirmation = delay_conf_plot))
}




golden_ratio <- 1.618
unit_length <- 14

clean_data_hubei <- read.csv("../covid19/data/clean-hubei.csv", stringsAsFactors = FALSE)
plots_hubei <- delay_figures(clean_data_hubei, "in Hubei", literal_na_values = TRUE)
ggsave("out/delay-scatter-hubei-hospitalisation.pdf",
       plot = plots_hubei$hospitalisation,
       width = golden_ratio * unit_length,
       height = unit_length, units = "cm")
ggsave("out/delay-scatter-hubei-confirmation.pdf",
       plot = plots_hubei$confirmation,
       width = golden_ratio * unit_length,
       height = unit_length, units = "cm")

clean_data_outside_hubei <- read.csv("../covid19/data/clean-outside-hubei.csv", stringsAsFactors = FALSE)
plots_outside_hubei <- delay_figures(clean_data_outside_hubei, "outside Hubei", literal_na_values = TRUE)
ggsave("out/delay-scatter-outside-hubei-hospitalisation.pdf",
       plot = plots_outside_hubei$hospitalisation,
       width = golden_ratio * unit_length,
       height = unit_length, units = "cm")
ggsave("out/delay-scatter-outside-hubei-confirmation.pdf",
       plot = plots_outside_hubei$confirmation,
       width = golden_ratio * unit_length,
       height = unit_length, units = "cm")
