
library (dplyr)
library (ggplot2)
library (here)


## Create Fig. 8 H
##

source( here("R", "themes.R") )

sl <- read.csv(here("data", "subject-level.csv"))


fit0 <- lm(
  scale(ECAD_LS) ~ scale(E6FL_score) + factor(Cohort),
  data = sl
)

fit.c <- list(
  update(
    fit0, . ~ scale(E6FL_score), data = filter(sl, Cohort == 1L)
  ),
  update(
    fit0, . ~ scale(E6FL_score), data = filter(sl, Cohort == 2L)
  )
)


## dev.new( units = "in", height = 2.45, width = 3.25 )

ggplot( sl, aes(E6FL_score, ECAD_LS) ) +
  geom_point(aes(color = factor(Cohort)), size = rel(0.9)) +
  geom_smooth(
    aes(color = factor(Cohort), group = factor(Cohort)),
    method = "lm", se = FALSE, linewidth = rel(0.6),
    linetype = "12"
  ) +
  geom_smooth(
    method = "lm", se = FALSE, color = "black", linewidth = rel(0.6)
  ) +
  annotate(
    geom = "text", color = "darkorange3", fontface = "italic",
    x = 340, y = 142, hjust = 0, size = rel(2.8),
    label = sprintf(
      "Cohort 1: r=%.3f, p=%.3f",
      coef(fit.c[[1]])["scale(E6FL_score)"],
      coef(summary(fit.c[[1]]))["scale(E6FL_score)", "Pr(>|t|)"]
    )
  ) +
  annotate(
    geom = "text", color = "dodgerblue3", fontface = "italic",
    x = 340, y = 127, hjust = 0, size = rel(2.8),
    label = sprintf(
      "Cohort 2: r=%.3f, p=%.3f",
      coef(fit.c[[2]])["scale(E6FL_score)"],
      coef(summary(fit.c[[2]]))["scale(E6FL_score)", "Pr(>|t|)"]
    )
  ) +
  annotate(
    geom = "text", color = "black", fontface = "italic",
    x = 340, y = 112, hjust = 0, size = rel(2.8),
    label = sprintf(
      "Pooled: r=%.3f, p=%.3f",
      coef(fit0)["scale(E6FL_score)"],
      coef(summary(fit0))["scale(E6FL_score)", "Pr(>|t|)"]
    )
  ) +
  scale_color_manual(
    values = c("1" = "dodgerblue2", "2" = "darkorange2")
  ) +
  guides( color = "none" ) +
  labs(
    x = expression("E6FL:E6"[ALL]~score),
    y = "ECAD-LS"
  ) +
  theme_classic()

ggsave(
  here("output", "figure-8h.pdf"),
  units = "in", height = 2.45, width = 3.25
)

