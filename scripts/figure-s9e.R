
library (dplyr)
library (ggplot2)
library (here)
library (survival)
library (viridis)


## Create Fig. S9 E:
## Overall survival contours via Weibull regression
## (Continuous ECAD-LS, linear effect, stratified by cohort)
##

source( here("R", "auxiliary.R") )
source( here("R", "themes.R") )

sl <- read.csv(here("data", "subject-level.csv"))



## --- Fig. S9 E -----------------------------------------------------

## Estimate marginal effect of ECAD-LS on OS times
## (adjusting only for cohort)
## -> Weibull hazard works well for these data
##
fit.osm <- survreg(
  Surv(os_time, os_event) ~ I((ECAD_LS - 200)/10) + factor(Cohort),
  data = filter(
    sl, !is.na(AJCC_stage), use_in_os == "Yes"
  ),
  model = TRUE, x = TRUE, y = TRUE
)


## Adjust for age and stage for inference on ECAD-LS
fit.osa <- update(
  fit.osm,
  . ~ . + rcs(scale(Age), 3, lin = TRUE) + AJCC_stage
)

print( summary(fit.osa) )
##                                   Value Std. Error     z       p
## (Intercept)                      4.4002     0.9328  4.72 2.4e-06
## I((ECAD_LS - 200)/10)            0.0206     0.0539  0.38   0.702
## factor(Cohort)2                  1.1071     0.5732  1.93   0.053
## rcs(scale(Age), 3, lin = TRUE)1 -0.4332     0.6656 -0.65   0.515
## rcs(scale(Age), 3, lin = TRUE)2  0.4190     0.7975  0.53   0.599
## AJCC_stageStage II               0.5475     0.7868  0.70   0.487
## AJCC_stageStage III+            -0.7205     0.7625 -0.94   0.345
## Log(scale)                      -0.4206     0.2589 -1.62   0.104
##
## Scale= 0.657
##
## Weibull distribution
## Loglik(model)= -70.8



T <- with( sl, max(os_time) )
r <- with( sl, range(ECAD_LS) )
p1 <- with( sl, mean(Cohort == 1L) )

grid <- expand.grid(
  Time = seq( 0, T * 1.02, length.out = 100 ),
  Marker = seq( r[1] * 0.98, r[2] * 1.02, length.out = 100 )
) %>%
  mutate(
    Surv = p1 * weibull.S(
      fit.osm, Time,
      cbind( 1, (Marker - 200)/10, 0 )
    ) +
      (1 - p1) * weibull.S(
        fit.osm, Time,
        cbind( 1, (Marker - 200)/10, 1 )
      ),
    Surv.Group = cut( Surv, c(0, (5:10)/10), include.lowest = TRUE )
  )



## dev.new( units = "in", width = 4.5, height = 2.95 )

fs9e <- ggplot( grid, aes(Time, Marker) ) +
  geom_raster(aes(fill = Surv.Group), alpha = 0.75) +
  geom_point(
    aes(
      os_time, ECAD_LS, size = factor(os_event),
      shape = factor(os_event)
    ),
    data = filter( sl, use_in_os == "Yes" )
  ) +
  scale_fill_viridis( option = "mako", discrete = TRUE, begin = 0.3 ) +
  ## annotate(
  ##   geom = "text", color = "black", fontface = "italic",
  ##   x = 2.5, y = r[2] * 0.98, hjust = 0, size = rel(3),
  ##   label = format.pval(
  ##     summary(fit.osa)$table["I((ECAD_LS - 200)/10)", "p"]
  ##     ## ^^ p-value from adjusted model
  ##   )
  ## ) +
  coord_cartesian( expand = 0 ) +
  scale_size_manual(
    values = c("0" = rel(0.7), "1" = rel(1.4))
  ) +
  scale_shape_manual(
    values = c("0" = 3, "1" = 19),
    labels = c("0" = "Censored", "1" = "Died")
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
    fill = "OS\nprobability",
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
  here("output", "figure-s9e.pdf"), plot = fs9e,
  units = "in", width = 4.5, height = 2.95
)



