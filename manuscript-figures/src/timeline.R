library(xtable)
source("../covid19/src/tools.cleaner.R")

x <- read.csv("../covid19/data/clean-outside-hubei.csv", stringsAsFactors = FALSE)

remove_na <- function(x) {
    Filter(function(y) !is.na(y), x)
}

first_event_date_func <- function(x) {
    min_date <- function(y) {
        as.Date(min(remove_na(strpdate(y$date_confirmation, literal_na_value = TRUE))), origin = "1970-01-01")
    }
    function(country_name) {
        conf_dates <- subset(x, subset = country == country_name, select = date_confirmation)
        min_date(conf_dates)
    }
}

first_conf <- first_event_date_func(x)

countries <- remove_na(unique(x$country))

first_conf_df <- data.frame(country = countries, first_confirmation = sapply(countries, function(x) as.character(first_conf(x))))
sort_ixs <- sort.int(as.Date(first_conf_df$first_confirmation), index.return = TRUE)$ix
first_conf_df <- first_conf_df[sort_ixs,]

print(xtable(first_conf_df))

cat("| Country | Date of first confirmed case outside Hubei |\n")
cat("|----------|---------------------|---------|\n")
for (ix in 1:nrow(first_conf_df)) {
    cat(sprintf("| %s | `%s` |\n", first_conf_df[ix,"country"], first_conf_df[ix,"first_confirmation"]))
}
