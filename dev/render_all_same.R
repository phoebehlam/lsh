source("dev/load_dev.R")
ex <- file.path(.rth_home(), "example_data")
args <- function(o, st) list(id = "1234", age = 16, night_sav = file.path(ex, "sim_night_diary.sav"),
  morning_sav = file.path(ex, "sim_morning_diary.sav"), sleep_csv = "dev/sim_sleep_1234_allsame.csv",
  out_dir = o, settings = st)
st <- rth_settings(); st$same_show_top <- 0
do.call(render_rth_feedback, args("dev/out/allsame_text_only", st))
do.call(render_rth_feedback, args("dev/out/allsame_top3", rth_settings()))
