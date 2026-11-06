
library (dplyr)
library (ggplot2)
library (here)
library (magrittr)
library (survial)
library (survminer)

## Create components of Fig. S10
##


source( here("R", "themes.R") )
source( here("R", "auxiliary.R") )


sl <- read.csv( here("data", "subject-level.csv") )



## --- Fig. S10 A and B ----------------------------------------------

fmt.cor <- function( rho ) {  # output of cor.test input
  nm <- gsub("([A-z]*)'s.*", "\\1", rho$method)
  sprintf(
    "%s r=%.3f, %s", nm, rho$estimate, format.pval(rho$p.value)
  )
}

do.cor <- function( x, y, meth = c("pearson", "spearman") ) {
  rr <- lapply( meth, \(m) fmt.cor(cor.test(x, y, method = m)) )
  paste( rr, collapse = "\n" )
}

stats <- group_by( sl, Cohort ) %>%
  summarize(
    ECAD = do.cor( E6FL_score, ECAD_total ),
    ecad.max = max( ECAD_total, na.rm = TRUE ),
    VIM  = do.cor( E6FL_score, VIM_total ),
    vim.max = max( VIM_total, na.rm = TRUE )
  )


## dev.new( units = "in", height = 2.5, width = 5.7 )

ggplot( sl, aes(E6FL_score, ECAD_total) ) +
  geom_point(aes(color = factor(Cohort)), size = rel(0.6)) +
  geom_smooth(
    aes(fill = factor(Cohort)), color = "gray98",
    method = "lm", se = TRUE, linewidth = rel(1.2)
  ) +
  geom_smooth(
    aes(color = factor(Cohort)),
    method = "lm", se = FALSE, linewidth = rel(0.5)
  ) +
  geom_text(
    aes( x = 280, y = ecad.max * 0.92, label = ECAD ),
    data = stats, fontface = "italic", hjust = 0, size = rel(2.8)
  ) +
  facet_wrap(~ paste("Cohort", Cohort), scales = "free_y") +
  labs(
    x = expression("E6FL:E6"[ALL]~score),
    y = "Total ECAD"
  ) +
  scale_color_manual( values = .cohort.cols ) +
  scale_fill_manual( values = .cohort.cols ) +
  guides( color = "none", fill = "none" ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  here("output", "figure-s10a.pdf"),
  units = "in", height = 2.5, width = 5.7
)




ggplot( sl, aes(E6FL_score, VIM_total) ) +
  geom_point(aes(color = factor(Cohort)), size = rel(0.6)) +
  geom_smooth(
    aes(fill = factor(Cohort)), color = "gray98",
    method = "lm", se = TRUE, linewidth = rel(1.2)
  ) +
  geom_smooth(
    aes(color = factor(Cohort)),
    method = "lm", se = FALSE, linewidth = rel(0.5)
  ) +
  geom_text(
    aes( x = 280, y = vim.max * 0.9, label = VIM ),
    data = stats, fontface = "italic", hjust = 0, size = rel(2.8)
  ) +
  facet_wrap(~ paste("Cohort", Cohort), scales = "free_y") +
  labs(
    x = expression("E6FL:E6"[ALL]~score),
    y = "Total VIM"
  ) +
  scale_color_manual( values = .cohort.cols ) +
  scale_fill_manual( values = .cohort.cols ) +
  guides( color = "none", fill = "none" ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  here("output", "figure-s10b.pdf"),
  units = "in", height = 2.5, width = 5.7
)





## --- Fig. S10 C ----------------------------------------------------

fs10cl <- arrange_ggsurvplots(
  list(
    km.wrap2(
      filter(sl, use_in_os == "Yes", Cohort == 1L),
      ECAD_total, outcome = "OS", na.rm = TRUE
    ),
    km.wrap2(
      filter(sl, use_in_os == "Yes", Cohort == 2L),
      ECAD_total, outcome = "OS", na.rm = TRUE
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S10 C (left) - Total ECAD",
  print = FALSE
)

ggsave(
  here("output", "figure-s10c-left.pdf"), plot = fs10cl,
  height = 5.25, width = 8.55
)



fs10cr <- arrange_ggsurvplots(
  list(
    km.wrap2(
      filter(sl, use_in_rfs == "Yes", Cohort == 1L),
      ECAD_total, outcome = "RFS", na.rm = TRUE
    ),
    km.wrap2(
      filter(sl, use_in_rfs == "Yes", Cohort == 2L),
      ECAD_total, outcome = "RFS", na.rm = TRUE
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S10 C (right) - Total ECAD",
  print = FALSE
)

ggsave(
  here("output", "figure-s10c-right.pdf"), plot = fs10cr,
  height = 5.25, width = 8.55
)




## --- Fig. S10 D ----------------------------------------------------

fs10dl <- arrange_ggsurvplots(
  list(
    km.wrap2(
      filter(sl, use_in_os == "Yes", Cohort == 1L),
      VIM_total, outcome = "OS", na.rm = TRUE
    ),
    km.wrap2(
      filter(sl, use_in_os == "Yes", Cohort == 2L),
      VIM_total, outcome = "OS", na.rm = TRUE
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S10 D (left) - Total VIM",
  print = FALSE
)

ggsave(
  here("output", "figure-s10d-left.pdf"), plot = fs10dl,
  height = 5.25, width = 8.55
)



fs10dr <- arrange_ggsurvplots(
  list(
    km.wrap2(
      filter(sl, use_in_rfs == "Yes", Cohort == 1L),
      VIM_total, outcome = "RFS", na.rm = TRUE
    ),
    km.wrap2(
      filter(sl, use_in_rfs == "Yes", Cohort == 2L),
      VIM_total, outcome = "RFS", na.rm = TRUE
    )
  ),
  ncol = 2, nrow = 1, title = "Fig. S10 D (right) - Total VIM",
  print = FALSE
)

ggsave(
  here("output", "figure-s10d-right.pdf"), plot = fs10cr,
  height = 5.25, width = 8.55
)





