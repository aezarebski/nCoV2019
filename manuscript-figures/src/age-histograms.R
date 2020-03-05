#' -----------------------------------------------------------------------------
#' This script looks at the cleaned data sets and produces a plot comparing the
#' age distribution of confirmed cases stratified by sex.
#'
#' -----------------------------------------------------------------------------
#' Usage:
#'
#' $ Rscript src/age-histograms.R
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



.completeness_data_frame <- function(x) {
    result <- as.data.frame(table(x))
    names(result) <- c("value", "frequency")
    result$value <- as.character(result$value)
    return(result)
}

.mesh <- function(max_age, age_string) {
    if (age_string == na_string) {
        rep(0, max_age + 1)
    } else if (grepl(pattern = anchor_wrap(.rgx_single_age), x = age_string)) {
        age_val <- as.integer(age_string)
        sapply(0:max_age, function(x) as.integer(age_val == x))
    } else if (grepl(pattern = anchor_wrap(.rgx_age_range), x = age_string)) {
        range_vals <- as.integer(unlist(strsplit(x = age_string, split = "-")))
        range_length <- diff(range_vals) + 1
        sapply(0:max_age, function(x) as.integer((range_vals[1] <= x) & (x <= range_vals[2]))) / range_length
    } else {
        stop("Could not parse age string.")
    }
}

.bin_strings <- function(n, m) {
    unlist(lapply(0:(n-1) * m, function(ix) rep(sprintf("%d-%d", ix, ix + m - 1), each = m)))
}

histogram_date_strings <- function(age_df) {
    df <- .completeness_data_frame(age_df)
    max_age <- 99
    acc <- rep(0,max_age + 1)
    for (ix in 1:nrow(df)) {
        age_string <- df[ix, "value"]
        string_freq <- df[ix, "frequency"]
        acc <- acc + string_freq * .mesh(max_age, age_string)
    }
    tmp <- data.frame(acc = acc, bin = .bin_strings(10,10))
    tmp %>% group_by(bin) %>% summarise(sum_acc = sum(acc))
}



sex_hist <- function(clean_data, annotation_x) {
  female_plot_df <- clean_data %>% filter(sex == "female") %>% select(age) %>% histogram_date_strings() %>% mutate(sex = "female")
  male_plot_df <- clean_data %>% filter(sex == "male") %>% select(age) %>% histogram_date_strings()  %>% mutate(sex = "male")
  plot_df <- bind_rows(female_plot_df, male_plot_df)
  clean_data %>% filter(sex != "NA") %>% filter(age != "NA") %>% select(age, sex)
  tmp1 <- clean_data %>% filter(sex != "NA") %>% filter(age != "NA") %>% nrow
  tmp2 <- sum(plot_df$sum_acc)
  stopifnot(tmp1 == tmp2)
  rm(tmp1)
  rm(tmp2)
  annotation_text <- sprintf("Proportion missing:\n%.2f", 1 - sum(plot_df$sum_acc) / nrow(clean_data))
  sex_age_plot <- ggplot(plot_df, aes(x = bin, y = sum_acc, colour = sex)) +
      geom_bar(stat = "identity", position = "dodge", fill = "white", size = 1) +
      annotate("text", x = annotation_x, y = 0.8 * max(plot_df$sum_acc), label = annotation_text, size = 6) +
      labs(x = "Age", y = "Frequency", colour = "Sex") +
      theme_classic() +
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = -30))
  return(sex_age_plot)
}


golden_ratio <- 1.618
unit_length <- 14

clean_data_hubei <- read.csv("../covid19/data/clean-hubei.csv")
plot_hubei <- sex_hist(clean_data_hubei, 3) + ggtitle("Hubei")
ggsave("out/age-histogram-hubei.pdf", width = golden_ratio * unit_length, height = unit_length, units = "cm")



clean_data_outside_hubei <- read.csv("../covid19/data/clean-outside-hubei.csv")
plot_outside_hubei <- sex_hist(clean_data_outside_hubei, 9) + ggtitle("Outside Hubei")
ggsave("out/age-histogram-outside-hubei.pdf", width = golden_ratio * unit_length, height = unit_length, units = "cm")
