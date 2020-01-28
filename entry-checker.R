#!/usr/bin/env Rscript

sensible_ages <- list(is_good = function(df) {all(grepl(pattern = "(^$|^[0-9]+$|^[0-9]+-[0-9]+)", x = df$age))},
                      success_message = "all ages match regex\n",
                      error_message = function(df) {"at least one age entry does not look right.\n"})

distinct_ids <- list(is_good = function(df) {nrow(df) == length(unique(df$ID))},
                     success_message = "all identifiers are distinct\n",
                     error_message = function(df) {"the number of rows does not equal the number of unique ID values.\n"})

main <- function() {
    cat("Entry checker\n")
    data_file <- "ncov_hubei.csv"
    cat("\t", sprintf("Checking file: %s\n", data_file))
    df <- read.csv(data_file, stringsAsFactors = FALSE)
    tests <- list(sensible_ages,distinct_ids)
    for (test in tests) {
        if (test$is_good(df)) {
            cat("\t\t", test$success_message)
        } else {
            cat("\n\t\t", "there was a problem...\n\t\t\t", test$error_message(df))
        }
    }
}

if (!interactive()) {
    main()
}
