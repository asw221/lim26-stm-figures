
library (dplyr)
library (ggplot2)
library (here)
library (survival)
library (survminer)


## Create components of Fig. S9
##

source( here("R", "themes.R") )
source( here("R", "kmplots.R") )

sl <- read.csv(here("data", "subject-level.csv"))



## --- Fig. S9 B -----------------------------------------------------

fmt.med.iqr <- function( x ) {
  sprintf(
    "Median=%.1f\nIQR=(%.1f, %.1f)",
    median(x, na.rm = TRUE),
    quantile(x, 0.25, na.rm = TRUE),
    quantile(x, 0.75, na.rm = TRUE)
  )
}

med.dens <- function( x, ... ) {
  d <- density( x, ... )
  m <- median( x, na.rm = TRUE )
  f <- approxfun( d$x, d$y )
  list( med = m, f = f, fm = f(m) )
}


stats <- c(
  fmt.med.iqr( sl$ECAD_LS ),
  fmt.med.iqr( filter(sl, Cohort == 1L)$ECAD_LS ),
  fmt.med.iqr( filter(sl, Cohort == 2L)$ECAD_LS )
)

stats2 <- rbind(
  with( med.dens(sl$ECAD_LS), c( med, fm ) ),
  with( med.dens(filter(sl, Cohort == 1L)$ECAD_LS), c( med, fm ) ),
  with( med.dens(filter(sl, Cohort == 2L)$ECAD_LS), c( med, fm ) )
)

dimnames (stats2) <- list(
  c("Pooled", "Cohort 1", "Cohort 2"),
  c("Median", "Density@median")
)


## dev.new( units = "in", height = 2.35, width = 3.25 )


## --- Pooled cohort panel ---

ggplot( sl, aes(ECAD_LS) ) +
  geom_density(
    color = "black", fill = "black", alpha = 0.4
  ) +
  geom_rug( alpha = 0.6, linewidth = rel(0.4) ) +
  annotate(
    geom = "segment", color = "black", linetype = "32",
    x = stats2[1, 1], y = stats2[1, 2], yend = 0,
    linewidth = rel(0.5)
  ) +
  annotate(
    geom = "text", color = "black", fontface = "italic",
    x = 105, y = 0.97 * stats2[1, 2], hjust = 0, size = rel(2.8),
    label = stats[1]
  ) +
  labs( x = "ECAD-LS", y = "Density", subtitle = "Pooled" ) +
  scale_x_continuous( limits = c(100, 300) ) +
  theme_classic() +
  theme()

ggsave(
  here("output", "figure-s9b-pooled.pdf"),
  height = 2.35, width = 3.25, units = "in"
)


## --- Cohort 1 panel ---

ggplot( filter(sl, Cohort == 1L), aes(ECAD_LS) ) +
  geom_density(
    color = .cohort.cols[1], fill = .cohort.cols[1], alpha = 0.4
  ) +
  geom_rug( alpha = 0.6, linewidth = rel(0.4), color = .cohort.cols[1] ) +
  annotate(
    geom = "segment", color = .cohort.cols[1], linetype = "32",
    x = stats2[2, 1], y = stats2[2, 2], yend = 0,
    linewidth = rel(0.5)
  ) +
  annotate(
    geom = "label", color = "dodgerblue3", fontface = "italic",
    x = 105, y = 0.97 * stats2[2, 2], hjust = 0, size = rel(2.8),
    label = stats[2], alpha = 0.5, linewidth = 0
  ) +
  labs( x = "ECAD-LS", y = "Density", subtitle = "Cohort 1" ) +
  scale_x_continuous( limits = c(100, 300) ) +
  theme_classic() +
  theme()

ggsave(
  here("output", "figure-s9b-cohort1.pdf"),
  height = 2.35, width = 3.25, units = "in"
)


## --- Cohort 2 panel ---

ggplot( filter(sl, Cohort == 2L), aes(ECAD_LS) ) +
  geom_density(
    color = .cohort.cols[2], fill = .cohort.cols[2], alpha = 0.4
  ) +
  geom_rug( alpha = 0.6, linewidth = rel(0.4), color = .cohort.cols[2] ) +
  annotate(
    geom = "segment", color = .cohort.cols[2], linetype = "32",
    x = stats2[3, 1], y = stats2[3, 2], yend = 0,
    linewidth = rel(0.5)
  ) +
  annotate(
    geom = "label", color = "darkorange3", fontface = "italic",
    x = 105, y = 0.97 * stats2[3, 2], hjust = 0, size = rel(2.8),
    label = stats[3], alpha = 0.5, linewidth = 0
  ) +
  labs( x = "ECAD-LS", y = "Density", subtitle = "Cohort 2" ) +
  scale_x_continuous( limits = c(100, 300) ) +
  theme_classic() +
  theme()

ggsave(
  here("output", "figure-s9b-cohort2.pdf"),
  height = 2.35, width = 3.25, units = "in"
)



## --- Fig. S9 C and D - KM curves -----------------------------------

fs9c <- arrange_ggsurvplots(
  list(
    km.wrap(
      filter(sl, use_in_rfs == "Yes", Cohort == 2L),
      stats2["Cohort 2", "Median"]
    ),
    km.wrap(
      filter(sl, use_in_rfs == "Yes"),
      stats2["Pooled", "Median"]
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S9 C", print = FALSE
)

ggsave(
  here("output", "figure-s9c.pdf"), plot = fs9c,
  height = 5.25, width = 8.55
)



fs9d <- arrange_ggsurvplots(
  list(
    km.wrap(
      filter(sl, use_in_os == "Yes"),
      stats2["Cohort 1", "Median"], "OS"
    ),
    km.wrap(
      filter(sl, use_in_os == "Yes"),
      stats2["Pooled", "Median"], "OS"
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S9 D", print = FALSE
)

ggsave(
  here("output", "figure-s9d.pdf"), plot = fs9d,
  height = 5.25, width = 8.55
)





