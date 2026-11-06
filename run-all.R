
library (here)
library (magrittr)


dir.create(
  here("output", "logs"), showWarnings = FALSE, recursive = TRUE
)


scripts <- list.files(
  here("scripts"), pattern = "\\.R$", full.names = TRUE
)

for ( fn in scripts ) {
  message( "Running", basename(fn) )
  tryCatch({
    source( fn, echo = FALSE )
  },
  error = function(e) {
    cat(
      "Error encountered in file", basename(fn),
      "\n  \u2192 ", conditionMessage(e), "\n"
    )
    invisible( NULL )
  })
}


sessionInfo() %>%
  capture.output() %>%
  writeLines(
    here("output", "logs", "sessionInfo.txt")
  )

