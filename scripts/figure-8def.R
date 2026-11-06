
library (dplyr)
library (ggplot2)
library (here)
library (survival)
library (survminer)


## Create Fig. 8 parts D, E, and F
##

source( here("R", "themes.R") )
source( here("R", "kmplots.R") )

sl <- read.csv(here("data", "subject-level.csv"))



## --- Fig. 8 D-F ----------------------------------------------------

f8def <- arrange_ggsurvplots(
  list(
    km.wrap( filter(sl, use_in_rfs == "Yes", Cohort == 1L) ),
    km.wrap( filter(sl, use_in_rfs == "Yes", Cohort == 2L) ),
    km.wrap( filter(sl, use_in_rfs == "Yes") )
  ),
  ncol = 3, title = "Fig. 8 D, E, & F", print = FALSE
)

ggsave(
  here("output", "figure-8def.pdf"), plot = f8def,
  units = "in", height = 5.35, width = 11.55
)



