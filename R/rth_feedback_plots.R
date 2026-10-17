# Charts for the feedback report. Soft pastel palette, thin marks, small type,
# one axis per panel. Pastel fills sit below 3:1 contrast on white, so every
# coloured mark is also direct-labelled or titled (dataviz validator: pass with
# relief).

#' @keywords internal
rth_pal <- list(
  ink = "#2B3442", muted = "#7A8291", grid = "#EEEAE6", surface = "#FFFFFF",
  sleep = "#7D93B5",       # soft periwinkle-slate for sleep series
  sleep_light = "#DCE3EE", # time in bed (behind sleep)
  stress = "#EE8A72",      # soft coral
  positive = "#2FB5A6",    # soft teal
  help = "#9A8AE6",        # soft lavender
  band = "#F6CFB3",        # reference range (peach)
  same = "#F1EFEC"         # "about the same" zone
)

#' @keywords internal
theme_rth <- function(base = 8) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = rth_pal$ink),
      axis.text = ggplot2::element_text(colour = rth_pal$muted, size = base - 1.5),
      axis.title = ggplot2::element_text(colour = rth_pal$muted, size = base - 1.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = rth_pal$grid, linewidth = .3),
      strip.text = ggplot2::element_text(hjust = 0, face = "plain", size = base - .5,
                                         colour = rth_pal$muted, margin = ggplot2::margin(2, 0, 2, 0)),
      plot.margin = ggplot2::margin(2, 6, 2, 2),
      legend.position = "top", legend.justification = "left",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.title = ggplot2::element_blank(),
      legend.key.size = grid::unit(8, "pt"),
      legend.text = ggplot2::element_text(size = base - 1.5, colour = rth_pal$muted)
    )
}

.date_axis <- function() {
  ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "3 days",
                        expand = ggplot2::expansion(add = .6))
}
.pt  <- 1.1   # point size
.lw  <- .45   # line width

#' Night-to-night sleep, small multiples with the person's own average
#' @keywords internal
plot_nights <- function(days, guideline = NULL, ref_se_min = 85, ref_sol_max = 30) {
  d <- dplyr::filter(days, !is.na(tst))
  long <- dplyr::bind_rows(
    tibble::tibble(date = d$date, panel = "Sleep time (hours)", value = d$tst / 60),
    tibble::tibble(date = d$date, panel = "Sleep efficiency (%)", value = d$se),
    tibble::tibble(date = d$date, panel = "Sleep latency (minutes)", value = d$sol),
    tibble::tibble(date = d$date, panel = "Times you woke up", value = d$awakenings))
  long$panel <- factor(long$panel, levels = unique(long$panel))
  avg <- dplyr::summarise(dplyr::group_by(long, panel), m = mean(value, na.rm = TRUE))

  p <- ggplot2::ggplot(long, ggplot2::aes(date, value))
  x0 <- min(long$date) - .6; x1 <- max(long$date) + .6
  lv <- levels(long$panel)
  bands <- data.frame(panel = factor(c("Sleep efficiency (%)", "Sleep latency (minutes)"), levels = lv),
                      lo = c(ref_se_min, 0),
                      hi = c(max(100, max(d$se, na.rm = TRUE)), ref_sol_max))
  if (!is.null(guideline)) {
    bands <- rbind(data.frame(panel = factor("Sleep time (hours)", levels = lv),
                              lo = guideline$lo, hi = guideline$hi), bands)
  }
  bands$x0 <- x0; bands$x1 <- x1
  p <- p + ggplot2::geom_rect(data = bands, inherit.aes = FALSE,
                              ggplot2::aes(xmin = x0, xmax = x1, ymin = lo, ymax = hi),
                              fill = rth_pal$band, alpha = .55)
  p +
    ggplot2::geom_hline(data = avg, ggplot2::aes(yintercept = m),
                        linetype = "22", colour = rth_pal$muted, linewidth = .3) +
    ggplot2::geom_line(colour = rth_pal$sleep, linewidth = .lw) +
    ggplot2::geom_point(colour = rth_pal$sleep, size = .pt) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(breaks = function(l) pretty(l, n = 3)) +
    .date_axis() + ggplot2::labs(x = NULL, y = NULL) + theme_rth() +
    ggplot2::theme(panel.spacing.y = grid::unit(2, "pt"),
                   strip.text = ggplot2::element_text(margin = ggplot2::margin(1, 0, 1, 0)))
}

#' Sleep window per night: in bed (light) and asleep (darker), on a clock axis
#' @keywords internal
plot_sleep_window <- function(days, anchor_h = 18) {
  d <- dplyr::filter(days, !is.na(bed), !is.na(wake))
  cm <- function(x) .clock_min(x, anchor_h) / 60
  d <- dplyr::mutate(d, b = cm(bed), w = cm(wake), on = cm(onset), off = cm(offset),
                     lab = format(date, "%a %b %d"))
  d$lab <- factor(d$lab, levels = rev(d$lab))
  brks <- seq(0, 18, by = 3)
  lbls <- sub(":00", "", sapply(brks * 60, fmt_clock, anchor_h = anchor_h))
  ggplot2::ggplot(d, ggplot2::aes(y = lab)) +
    ggplot2::geom_segment(ggplot2::aes(x = b, xend = w, yend = lab),
                          colour = rth_pal$sleep_light, linewidth = 2.6, lineend = "round") +
    ggplot2::geom_segment(ggplot2::aes(x = on, xend = off, yend = lab),
                          colour = rth_pal$sleep, linewidth = 2.6, lineend = "round") +
    ggplot2::scale_x_continuous(breaks = brks, labels = lbls,
                                limits = c(min(c(d$b, 3), na.rm = TRUE) - .3,
                                           max(c(d$w, 15), na.rm = TRUE) + .3)) +
    ggplot2::labs(x = NULL, y = NULL) + theme_rth() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = rth_pal$grid, linewidth = .3),
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Watch vs. self-report: time in bed per night, two dots joined
#' @keywords internal
plot_watch_vs_you <- function(days) {
  d <- dplyr::filter(days, !is.na(tib), !is.na(sr_tib))
  if (!nrow(d)) return(NULL)
  long <- dplyr::bind_rows(
    tibble::tibble(date = d$date, who = "Watch", h = d$tib / 60),
    tibble::tibble(date = d$date, who = "Your diary", h = d$sr_tib / 60))
  ggplot2::ggplot(long, ggplot2::aes(date, h)) +
    ggplot2::geom_segment(data = d, inherit.aes = FALSE,
                          ggplot2::aes(x = date, xend = date, y = tib / 60, yend = sr_tib / 60),
                          colour = "#E2DDD8", linewidth = .6) +
    ggplot2::geom_point(ggplot2::aes(colour = who, shape = who), size = 1.5) +
    ggplot2::scale_colour_manual(values = c(Watch = rth_pal$sleep, `Your diary` = rth_pal$positive)) +
    ggplot2::scale_shape_manual(values = c(Watch = 16, `Your diary` = 17)) +
    .date_axis() + ggplot2::labs(x = NULL, y = "Hours in bed") + theme_rth()
}

#' Daily events: three small bar charts with the person's own average
#' @keywords internal
plot_events <- function(days) {
  d <- dplyr::filter(days, !is.na(stress_n) | !is.na(pos_n) | !is.na(help_n))
  long <- dplyr::bind_rows(
    tibble::tibble(date = d$date, panel = "Unpleasant events", n = d$stress_n),
    tibble::tibble(date = d$date, panel = "Positive events", n = d$pos_n),
    tibble::tibble(date = d$date, panel = "Times you helped others", n = d$help_n))
  long$panel <- factor(long$panel, levels = unique(long$panel))
  avg <- dplyr::summarise(dplyr::group_by(long, panel), m = mean(n, na.rm = TRUE))
  ggplot2::ggplot(long, ggplot2::aes(date, n, fill = panel)) +
    ggplot2::geom_col(width = .62) +
    ggplot2::geom_hline(data = avg, ggplot2::aes(yintercept = m),
                        linetype = "22", colour = rth_pal$ink, linewidth = .3) +
    ggplot2::scale_fill_manual(values = c(`Unpleasant events` = rth_pal$stress,
                                          `Positive events` = rth_pal$positive,
                                          `Times you helped others` = rth_pal$help),
                               guide = "none") +
    ggplot2::scale_y_continuous(breaks = function(l) unique(floor(pretty(l))),
                                expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    .date_axis() + ggplot2::labs(x = NULL, y = NULL) + theme_rth()
}

#' Daily mood on the 0-100 scale
#' @keywords internal
plot_mood <- function(days) {
  d <- dplyr::filter(days, !is.na(neg_pomp) | !is.na(pos_pomp))
  long <- dplyr::bind_rows(
    tibble::tibble(date = d$date, who = "Negative mood", v = d$neg_pomp),
    tibble::tibble(date = d$date, who = "Positive mood", v = d$pos_pomp))
  ggplot2::ggplot(long, ggplot2::aes(date, v, colour = who, shape = who)) +
    ggplot2::geom_line(linewidth = .lw) + ggplot2::geom_point(size = .pt + .2) +
    ggplot2::scale_colour_manual(values = c(`Negative mood` = rth_pal$stress,
                                            `Positive mood` = rth_pal$sleep)) +
    ggplot2::scale_shape_manual(values = c(`Negative mood` = 16, `Positive mood` = 17)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    .date_axis() + ggplot2::labs(x = NULL, y = "Out of 100") + theme_rth()
}

#' Change in mood on days with one more event: one shared axis (mood points)
#' @keywords internal
plot_mood_change <- function(links, band = 5) {
  ttl <- c(stress_n = "On days with 1 more unpleasant event",
           pos_n = "On days with 1 more positive event",
           help_n = "On days you helped others 1 more time")
  d <- links
  d$predictor <- factor(ttl[d$predictor], levels = ttl)
  d$outcome <- factor(c(neg_pomp = "Negative mood", pos_pomp = "Positive mood")[d$outcome],
                      levels = c("Positive mood", "Negative mood"))
  ok <- d$status %in% c("shown", "about_same")
  d$val <- ifelse(ok, d$est, NA_real_)
  d$val[ok & abs(d$val) < .1] <- .1          # a sliver so a zero still shows
  d$txt <- mapply(function(e, s) {
    if (s %in% c("too_few_days", "no_variation")) return("not enough days")
    r <- round(e)
    if (r == 0) return("0 pts")
    sprintf("%s%d %s", if (r > 0) "+" else intToUtf8(8722), abs(r), if (abs(r) == 1) "pt" else "pts")
  }, links$est, links$status)
  d$val_lab <- ifelse(ok, d$val, 0)
  lim <- max(15, ceiling(max(abs(d$val), na.rm = TRUE) / 5) * 5 + 8)
  d$hj <- ifelse(!is.na(d$val) & d$val < 0, 1.12, -0.12)
  ggplot2::ggplot(d, ggplot2::aes(y = outcome)) +
    ggplot2::annotate("rect", xmin = -band, xmax = band, ymin = -Inf, ymax = Inf, fill = rth_pal$same) +
    ggplot2::geom_vline(xintercept = 0, colour = rth_pal$muted, linewidth = .3) +
    ggplot2::geom_col(ggplot2::aes(x = val, fill = outcome), width = .55,
                      data = d[!is.na(d$val), ]) +
    ggplot2::geom_text(ggplot2::aes(x = val_lab, label = txt, hjust = hj), size = 2.5,
                       colour = rth_pal$ink) +
    ggplot2::scale_fill_manual(values = c(`Negative mood` = rth_pal$stress,
                                          `Positive mood` = rth_pal$positive), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-lim, lim),
                                labels = function(x) ifelse(x > 0, paste0("+", x), sub("-", intToUtf8(8722), as.character(x)))) +
    ggplot2::facet_wrap(~predictor, ncol = 1) +
    ggplot2::labs(x = "Change in mood that day (points out of 100)", y = NULL) +
    theme_rth() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = rth_pal$grid, linewidth = .3),
                   strip.text = ggplot2::element_text(face = "bold", colour = rth_pal$ink))
}

#' Sleep window per night as vertical bars: dates across, clock time down
#' @keywords internal
plot_sleep_window_v <- function(days, anchor_h = 18) {
  d <- dplyr::filter(days, !is.na(bed), !is.na(wake))
  cm <- function(x) .clock_min(x, anchor_h) / 60
  d <- dplyr::mutate(d, b = cm(bed), w = cm(wake), on = cm(onset), off = cm(offset))
  lo <- floor(min(d$b, na.rm = TRUE)) - .3; hi <- ceiling(max(d$w, na.rm = TRUE)) + .3
  brks <- seq(0, 18, by = 4); brks <- brks[brks >= lo & brks <= hi]
  lbls <- sub(":00", "", sapply(brks * 60, fmt_clock, anchor_h = anchor_h))
  ggplot2::ggplot(d, ggplot2::aes(x = date)) +
    ggplot2::geom_segment(ggplot2::aes(xend = date, y = b, yend = w),
                          colour = rth_pal$sleep_light, linewidth = 2.6, lineend = "round") +
    ggplot2::geom_segment(ggplot2::aes(xend = date, y = on, yend = off),
                          colour = rth_pal$sleep, linewidth = 2.6, lineend = "round") +
    ggplot2::scale_y_reverse(breaks = brks, labels = lbls, limits = c(hi, lo)) +
    .date_axis() + ggplot2::labs(x = NULL, y = NULL) + theme_rth() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = rth_pal$grid, linewidth = .3))
}
