# Mock-ups of three designs for "How your events and that night's sleep went
# together", rendered from simulated participant 1234. Not part of the package.
source("dev/load_dev.R")
nd <- read_night_diary(file.path(.rth_home(), "example_data", "sim_night_diary.sav"), "1234")
md <- read_morning_diary(file.path(.rth_home(), "example_data", "sim_morning_diary.sav"), "1234")
ac <- read_actigraphy(file.path(.rth_home(), "example_data", "sim_sleep_1234.csv"))
r  <- compute_report("1234", 16, nd, md, ac)
sk <- r$sleep_links; s <- r$settings

preds <- c(stress_n = "1 more stressful event", pos_n = "1 more positive event",
           help_n = "Helped others 1 more time", neg_pomp = "Negative mood 25 points higher",
           pos_pomp = "Positive mood 25 points higher")
pcol <- c(stress_n = rth_pal$stress, pos_n = rth_pal$positive, help_n = rth_pal$help,
          neg_pomp = rth_pal$stress, pos_pomp = rth_pal$positive)
outs <- c(tst = "Sleep time", se = "Sleep efficiency", sol = "Sleep latency", awakenings = "Times you woke up")
val_txt <- function(o, e, st) {
  if (st == "about_same") return("about the same")
  if (st %in% c("too_few_days", "no_variation")) return("not enough days")
  a <- abs(e); sg <- if (e > 0) "+" else "&minus;"
  switch(o, tst = paste0(sg, round(a), " min"), sol = paste0(sg, round(a), " min"),
         se = paste0(sg, round(a), " pts"), awakenings = paste0(sg, format(round(a, 1), nsmall = 1), " times"))
}

# Option A: change cards (one card per event type, Oura-contributor style) ----
optA <- paste(vapply(names(preds), function(p) {
  rows <- vapply(names(outs), function(o) {
    x <- sk[sk$predictor == p & sk$outcome == o, ]
    band <- s$same_band[[o]]
    lim <- max(3 * band, abs(x$est) * 1.25, na.rm = TRUE)
    pos <- function(v) 50 + 50 * v / lim
    bar <- if (x$status == "shown") {
      l <- min(pos(0), pos(x$est)); w <- abs(pos(x$est) - pos(0))
      sprintf('<div class="c-bar" style="left:%.1f%%;width:%.1f%%;background:%s"></div>', l, w, pcol[[p]])
    } else ""
    sprintf('<div class="c-row"><div class="g-top"><span>%s</span><span class="%s">%s</span></div><div class="c-track"><div class="c-same" style="left:%.1f%%;width:%.1f%%"></div><div class="c-zero"></div>%s</div></div>',
            outs[[o]], if (x$status == "shown") "g-val" else "muted", val_txt(o, x$est, x$status),
            pos(-band), pos(band) - pos(-band), bar)
  }, "")
  ttlA <- c(stress_n = "On days with 1 more stressful event", pos_n = "On days with 1 more positive event",
            help_n = "On days you helped others 1 more time", neg_pomp = "On days your negative mood was 25 points higher",
            pos_pomp = "On days your positive mood was 25 points higher")
  sprintf('<div class="c-card" style="border-top-color:%s"><div class="c-title">%s&hellip;</div>%s</div>',
          pcol[[p]], ttlA[[p]], paste(rows, collapse = ""))
}, ""), collapse = "")

# Option B: highlights (only what differed, biggest first) ----------------------
shown <- sk[sk$status == "shown", ]
shown$rel <- abs(shown$est) / unlist(s$same_band[shown$outcome])
shown <- shown[order(-shown$rel), ]
night_txt <- c(tst = "sleep that night", se = "sleep efficiency that night",
               sol = "time to fall asleep that night", awakenings = "times you woke up that night")
optB <- paste0(
  '<div class="h-grid">',
  paste(vapply(seq_len(nrow(shown)), function(i) {
    x <- shown[i, ]
    sprintf('<div class="h-card" style="border-left-color:%s"><div class="h-num">%s</div><div class="h-txt">%s on days with <strong>%s</strong></div></div>',
            pcol[[x$predictor]], val_txt(x$outcome, x$est, x$status), night_txt[[x$outcome]], tolower(preds[[x$predictor]]))
  }, ""), collapse = ""),
  '</div><p class="hint" style="margin-top:10px">Everything else stayed about the same:</p><p class="chips">',
  paste(vapply(which(sk$status == "about_same"), function(i)
    sprintf('<span class="chip">%s &rarr; %s</span>', sub(" 25 points higher", " &uarr;", sub("^1 more ", "", sub("Helped others 1 more time", "helping", preds[[sk$predictor[i]]]))), tolower(outs[[sk$outcome[i]]])), ""),
    collapse = ""), "</p>")

# Option C: dot matrix (ggplot) --------------------------------------------------
dm <- sk
dm$rel <- ifelse(dm$status == "shown", abs(dm$est) / unlist(s$same_band[dm$outcome]), 0)
dm$dir <- ifelse(dm$status != "shown", "about the same", ifelse(dm$est > 0, "more / higher", "less / lower"))
dm$lab <- mapply(function(o, e, st) gsub("&minus;", intToUtf8(8722), sub(" times| pts| min", "", val_txt(o, e, st))), dm$outcome, dm$est, dm$status)
dm$lab[dm$status != "shown"] <- ""
dm$p <- factor(preds[dm$predictor], levels = rev(preds))
dm$o <- factor(outs[dm$outcome], levels = outs)
pC <- ggplot2::ggplot(dm, ggplot2::aes(o, p)) +
  ggplot2::geom_point(ggplot2::aes(size = pmax(rel, .35), fill = dir), shape = 21, colour = "white", stroke = .6) +
  ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.6, nudge_y = -.38, colour = rth_pal$ink) +
  ggplot2::scale_size_area(max_size = 9, guide = "none") +
  ggplot2::scale_fill_manual(values = c(`more / higher` = "#E9B872", `less / lower` = "#7D93B5", `about the same` = "#E4E0DC")) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, override.aes = list(size = 3))) +
  ggplot2::scale_x_discrete(position = "top", labels = function(x) gsub(" ", "\n", x)) +
  ggplot2::labs(x = NULL, y = NULL) + theme_rth() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(size = 8.5, colour = rth_pal$ink),
                 axis.text.y = ggplot2::element_text(size = 8.5, colour = rth_pal$ink), legend.position = "bottom", legend.justification = "center")
optC <- img_tag(pC, height = 3.6, alt = "Dot matrix of day-to-night links")

css <- paste(readLines("inst/rth_feedback/report.css"), collapse = "\n")
extra <- "
.opt-label { display:inline-block; background:#2B3442; color:#fff; border-radius:999px; padding:2px 10px; font-size:.8rem; font-weight:700; }
.c-card { border:1px solid var(--line); border-top:4px solid; border-radius:14px; padding:12px 14px 6px; margin:10px 0; }
.c-title { font-weight:700; margin-bottom:4px; }
.c-row { padding:6px 0 8px; font-size:.92rem; }
.c-track { position:relative; height:10px; margin-top:6px; background:#F6F4F1; border-radius:5px; }
.c-same { position:absolute; top:0; bottom:0; background:#E9E5E1; border-radius:5px; }
.c-zero { position:absolute; left:50%; top:-3px; bottom:-3px; width:1px; background:#9AA1AD; }
.c-bar { position:absolute; top:2px; height:6px; border-radius:3px; }
.muted { color:var(--muted); font-size:.85rem; }
.h-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; }
.h-card { border:1px solid var(--line); border-left:5px solid; border-radius:12px; padding:10px 12px; }
.h-num { font-size:1.35rem; font-weight:800; line-height:1.2; }
.h-txt { font-size:.82rem; color:var(--muted); line-height:1.3; margin-top:2px; }
.chips { line-height:2.1; }
.chip { display:inline-block; background:#F3F0ED; border-radius:999px; padding:0 10px; font-size:.78rem; margin:0 4px 0 0; white-space:nowrap; }
"
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Events and sleep: three options</title><style>', css, extra, '</style></head><body>',
  '<header class="hero"><h1 style="font-size:1.5rem">Events &amp; sleep: three options</h1><p class="dates">Simulated participant 1234 &middot; pick one (or mix)</p></header>',
  '<section><span class="opt-label">Option A</span><h2 style="margin-top:8px">Change cards</h2><p class="hint">One card per kind of day. Each row has its own scale; the grey zone is &ldquo;about the same&rdquo;. Same look as the sleep-at-a-glance rows.</p>', optA, '</section>',
  '<section><span class="opt-label">Option B</span><h2 style="margin-top:8px">Highlights</h2><p class="hint">Only the links that differed, biggest first, as headline numbers. Everything else is listed as &ldquo;about the same&rdquo;. Shortest to read.</p>', optB, '</section>',
  '<section><span class="opt-label">Option C</span><h2 style="margin-top:8px">Dot matrix</h2><p class="hint">All 20 links at once. Bigger dots = bigger difference (relative to &ldquo;about the same&rdquo;); colour = direction, not good or bad.</p>', optC, '</section>',
  '</body></html>')
writeLines(html, "dev/out/events_sleep_options.html")
cat("written\n")
