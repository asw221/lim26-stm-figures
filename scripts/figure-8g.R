
library (dplyr)
library (ggplot2)
library (here)
library (survival)
library (viridis)


## Create Fig. 8 G:
## Recurrence-free survival contours via Weibull regression
## (Continuous ECAD-LS, linear effect, stratified by cohort)
##

source( here("R", "auxiliary.R") )
source( here("R", "themes.R") )

sl <- read.csv(here("data", "subject-level.csv"))



## --- Fig. 8 G ------------------------------------------------------

## Estimate marginal effect of ECAD-LS on RFS times
## (adjusting only for cohort)
## -> Weibull hazard works well for these data
##
fit.rfsm <- survreg(
  Surv(rfs_time, rfs_event) ~ I((ECAD_LS - 200)/10) + factor(Cohort),
  data = filter(
    sl, !is.na(AJCC_stage), use_in_rfs == "Yes"
  ),
  model = TRUE, x = TRUE, y = TRUE
)


## Adjust for age and stage for inference on ECAD-LS
fit.rfsa <- update(
  fit.rfsm,
  . ~ . + rcs(scale(Age), 3, lin = TRUE) + AJCC_stage
)

print( summary(fit.rfsa) )
##                                   Value Std. Error     z     p
## (Intercept)                      2.8395     1.2278  2.31 0.021
## I((ECAD_LS - 200)/10)            0.1328     0.0875  1.52 0.129
## factor(Cohort)2                  1.5560     0.8388  1.86 0.064
## rcs(scale(Age), 3, lin = TRUE)1 -1.2076     0.9576 -1.26 0.207
## rcs(scale(Age), 3, lin = TRUE)2  1.8421     1.3043  1.41 0.158
## AJCC_stageStage II               1.7779     1.0008  1.78 0.076
## AJCC_stageStage III+             0.0612     0.9601  0.06 0.949
## Log(scale)                       0.2699     0.2082  1.30 0.195
##
## Scale= 1.31
##
## Weibull distribution
## Loglik(model)= -106.7



T <- with( sl, max(rfs_time) )
r <- with( sl, range(ECAD_LS) )
p1 <- with( sl, mean(Cohort == 1L) )

grid <- expand.grid(
  Time = seq( 0, T * 1.02, length.out = 100 ),
  Marker = seq( r[1] * 0.98, r[2] * 1.02, length.out = 100 )
) %>%
  mutate(
    Surv = p1 * weibull.S(
      fit.rfsm, Time,
      cbind( 1, (Marker - 200)/10, 0 )
    ) +
      (1 - p1) * weibull.S(
        fit.rfsm, Time,
        cbind( 1, (Marker - 200)/10, 1 )
      ),
    Surv.Group = cut( Surv, c(0, (5:10)/10), include.lowest = TRUE )
  )



## dev.new( units = "in", width = 4.5, height = 2.95 )

f8g <- ggplot( grid, aes(Time, Marker) ) +
  geom_raster(aes(fill = Surv.Group), alpha = 0.75) +
  geom_point(
    aes(
      rfs_time, ECAD_LS, size = factor(rfs_event),
      shape = factor(rfs_event)
    ),
    data = filter( sl, use_in_rfs == "Yes" )
  ) +
  scale_fill_viridis( option = "mako", discrete = TRUE, begin = 0.3 ) +
  ## annotate(
  ##   geom = "text", color = "black", fontface = "italic",
  ##   x = 2.5, y = r[2] * 0.98, hjust = 0, size = rel(3),
  ##   label = format.pval(
  ##     summary(fit.rfsa)$table["I((ECAD_LS - 200)/10)", "p"]
  ##     ## ^^ p-value from adjusted model
  ##   )
  ## ) +
  coord_cartesian( expand = 0 ) +
  scale_size_manual(
    values = c("0" = rel(0.7), "1" = rel(1.4))
  ) +
  scale_shape_manual(
    values = c("0" = 3, "1" = 19),
    labels = c("0" = "Censored", "1" = "Recur./death")
  ) +
  guides(
    size = "none", # shape = "none",
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(alpha = 0.8)
    )
  ) +
  labs(
    x = "Time (months)",
    y = "ECAD-LS",
    fill = "RFS\nprobability",
    shape = "Follow-up status",
    subtitle = "Marginal Weibull model\nContinuous ECAD-LS; stratified by cohort"
  ) +
  theme_base(
    legend.title = element_text( size = unit(8, "pt") ),
    legend.text = element_text( size = unit(8, "pt") ),
    plot.subtitle = element_text(
      face = "bold.italic", size = unit(9.5, "pt")
    )
    ## legend.position = "bottom"
  )


ggsave(
  here("output", "figure-8g.pdf"), plot = f8g,
  units = "in", width = 4.5, height = 2.95
)



