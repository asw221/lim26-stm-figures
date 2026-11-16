
library (here)
library (magrittr)

## ===================================================================
## Run to reproduce:
##  * Fig.   8, subparts C--H
##  * Fig.  S9, subparts B--E
##  * Fig. S10, subparts A--D
##
## From Lim et al., "HPV16 E6 splice variant drives partial
##  epithelial-mesenchymal transition and aggressive invasion in
##  oropharyngeal cancer."
##
## Andrew Whiteman (awhitem@umich.edu)
## Yvonne Lim (yvlim@umich.edu)
##
## Last updated: November, 2026
## ===================================================================


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

