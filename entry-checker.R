#!/usr/bin/env Rscript

distinct_ids <- list(is_good = function(df) {nrow(df) == length(unique(df$ID))},
                     success_message = "all identifiers are distinct\n",
                     error_message = function(df) {"there was a problem... the number of rows does not equal the number of unique ID values.\n"})

main <- function() {
    cat("Entry checker\n")
    data_file <- "ncov_hubei.csv"
    cat("\t", sprintf("Checking file: %s\n", data_file))
    df <- read.csv(data_file, stringsAsFactors = FALSE)
    tests <- list(distinct_ids)
    for (test in tests) {
        if (test$is_good(df)) {
            cat("\t\t", test$success_message)
        } else {
            cat("\t\t", test$error_message(df))
        }
    }
}

if (!interactive()) {
    main()
}
