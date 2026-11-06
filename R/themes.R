
.cohort.cols <- c(
  "1" = "dodgerblue2",
  "2" = "darkorange2"
)


theme_base <- function(
  ## text = element_text(family = "serif", color = "#22211d"),
  strip.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  ...
) {
  theme_bw() +
    theme(
      ## text = text,
      strip.background = strip.background,
      panel.grid.minor = panel.grid.minor,
      panel.grid.major = panel.grid.major,
      ...
    )
}


