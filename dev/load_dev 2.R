# Work on the report from source without installing lshlab:
#   source("dev/load_dev.R")   (run from the package root)
options(rth_feedback.home = normalizePath("inst/rth_feedback"))
for (f in list.files("R", pattern = "^rth_feedback_.*\\.R$", full.names = TRUE)) source(f)
