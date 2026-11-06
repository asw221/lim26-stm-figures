
## Special instance functions to create annotated Kaplan-Meier-type
## graphics
##


source( here("R", "auxiliary.R") )


## Create annotated K-M graphic with survminer::survplot
##
kmplot <- function(
  km,  # survival::survfit object
  data,
  .cutp = 195
) {
  f <- formula(km)
  lrt <- survival::survdiff(f, data = data)
  cx <- survival::coxph(f, data = data) %>%
    update(
      . ~ . + survival::strata(Cohort) + scale(Age) + AJCC_stage
    )
  cx.summ <- summary(cx)
  ylab <- as.character(f) %>%
    paste( collapse = " " ) %>%
    gsub( ".*Surv\\(([A-z_0-9]*),.*", "\\1", . ) %>%
    tolower() %>%
    (\(x) {
      dplyr::case_when(
        grepl("rfs", x, fixed = TRUE) ~ "RFS probability",
        grepl("os", x, fixed = TRUE) ~ "OS probability",
        .default = "Survival probability"
      )
    })
  hr <- sprintf("HR=%.2f", exp(coef(cx)[1]))
  mar <- ggplot2::margin( 0.5, 1, 0.5, 0.5, unit = "lines" )
  g <- survminer::ggsurvplot(
    km,
    data = data,
    break.time.by = 12,
    risk.table = TRUE, conf.int = TRUE,
    xlab = "Months", ylab = ylab,
    pval = paste(
      sprintf("Cutoff=%.1f", .cutp),
      hr,
      paste(format.pval(lrt$pvalue), "(log rank)"),
      paste(format.pval(coef(cx.summ)[1, "Pr(>|z|)"]), "(Cox)"),
      sep = "\n"
    ),
    legend.labs = c("Low", "High")
  )
  g$plot <- g$plot + ggplot2::theme(plot.margin = mar)
  g$table <- g$table +
    ggplot2::coord_cartesian(
      xlim = attr(g$plot, "parameters")$xlim,
      clip = "off"
    ) +
    ggplot2::theme(plot.margin = mar)
  g
}


## Wrapper function for kmplot for ECAD-LS group analyses
##
km.wrap <- function( data, cutp = 195, outcome = c("RFS", "OS") ) {
  outcome <- match.arg( outcome )
  data <- dplyr::mutate(
    data,
    ECAD_Group = dplyr::case_when(
      ECAD_LS >  cutp ~ "High",
      ECAD_LS <= cutp ~ "Low",
      .default = NA_character_
    ) %>%
      factor( levels = c("Low", "High") )
  )
  if ( outcome == "RFS" )
    km <- survival::survfit(
      survival::Surv(rfs_time, rfs_event) ~ ECAD_Group, data = data
    )
  else
    km <- survival::survfit(
      survival::Surv(os_time, os_event) ~ ECAD_Group, data = data
    )
  kmplot( km, data, .cutp = cutp )
}


## Alternative wrapper function for kmplot
## (any x variable, user-specified cutoff function)
##
km.wrap2 <- function(
  data, xvar, cutf = median, outcome = c("RFS", "OS"), ...
) {
  outcome <- match.arg( outcome )
  thr <- cutf( dplyr::pull( data, {{xvar}} ), ... )
  data <- dplyr::mutate(
    data,
    Group = dplyr::case_when(
      {{xvar}} >  thr ~ "High",
      {{xvar}} <= thr ~ "Low",
      .default = NA_character_
    ) %>%
      factor( levels = c("Low", "High") )
  )
  if ( outcome == "RFS" )
    km <- survival::survfit(
      survival::Surv(rfs_time, rfs_event) ~ Group, data = data
    )
  else
    km <- survival::survfit(
      survival::Surv(os_time, os_event) ~ Group, data = data
    )
  kmplot( km, data, .cutp = thr )
}


