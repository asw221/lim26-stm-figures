
library (dplyr)
library (irr)
library (ggplot2)
library (here)
library (tidyr)


## Create Fig. 8C
##

source( here("R", "themes.R") )
source( here("R", "auxiliary.R") )


## --- Read and reformat data ---

es <- read.csv(here("data", "ecad-ls-scores.csv"))
esw <- pivot_wider(
  es,
  id_cols = c(ID, Cohort), names_from = Rater, values_from = ECAD_LS
)



## --- Compute ICC statistics ---

stats <- c(
  with(
    icc(select(esw, A, B)),
    sprintf(
      "Pooled: ICC=%.3f, 95%% CI (%.3f, %.3f), %s",
      value, lbound, ubound, format.pval(p.value)
    )
  ),
  with(
    icc(select(filter(esw, Cohort == 1L), A, B)),
    sprintf(
      "Cohort 1: ICC=%.3f, 95%% CI (%.3f, %.3f), %s",
      value, lbound, ubound, format.pval(p.value)
    )
  ),
  with(
    icc(select(filter(esw, Cohort == 2L), A, B)),
    sprintf(
      "Cohort 2: ICC=%.3f, 95%% CI (%.3f, %.3f), %s",
      value, lbound, ubound, format.pval(p.value)
    )
  )
)



## --- Create figure ---
## dev.new( units = "in", height = 2.85, width = 2.75 )

ggplot( esw, aes(A, B) ) +
  geom_abline(
    slope = 1, intercept = 0, linetype = "32", linewidth = rel(0.5)
  ) +
  geom_point(aes(color = factor(Cohort)), size = rel(0.8)) +
  annotate(
    geom = "text", color = "black", fontface = "italic",
    x = 50, y = -20, hjust = 0, size = rel(2.7),
    label = stats[1]
  ) +
  annotate(
    geom = "text", color = "dodgerblue3", fontface = "italic",
    x = 50, y = 20, hjust = 0, size = rel(2.7),
    label = stats[2]
  ) +
  annotate(
    geom = "text", color = "darkorange3", fontface = "italic",
    x = 50, y = 0, hjust = 0, size = rel(2.7),
    label = stats[3]
  ) +
  coord_cartesian( xlim = c(100, 300), ylim = c(100, 300), clip = "off" ) +
  scale_color_manual( values = .cohort.cols ) +
  guides( color = "none" ) +
  labs(
    x = "Rater 1: ECAD-LS",
    y = "Rater 2: ECAD-LS"
  ) +
  theme_classic() +
  theme(
    plot.margin = margin(0.2, 0.2, 3.2, 0.2, unit = "lines")
  )

ggsave(
  here("output", "figure-8c.pdf"),
  height = 2.85, width = 2.75, units = "in"
)



