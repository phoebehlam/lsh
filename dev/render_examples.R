# Render example reports from the simulated data (run from the package root).
source("dev/load_dev.R")
render_rth_feedback("1234", 16, file.path(.rth_home(), "example_data", "sim_night_diary.sav"), file.path(.rth_home(), "example_data", "sim_morning_diary.sav"), file.path(.rth_home(), "example_data", "sim_sleep_1234.csv"), out_dir = "dev/out")
render_rth_feedback("5678", 18, file.path(.rth_home(), "example_data", "sim_night_diary.sav"), file.path(.rth_home(), "example_data", "sim_morning_diary.sav"), file.path(.rth_home(), "example_data", "sim_sleep_5678.csv"), out_dir = "dev/out")
