# Work on the report from source without installing lshlab:
#   source("dev/load_dev.R")   (run from the package root)
options(rth_feedback.home = normalizePath("inst/rth_feedback"),
        rth_recruit.home = normalizePath("inst/rth_recruit"))
for (f in list.files("R", pattern = "^rth_(feedback|recruit)_.*\\.R$", full.names = TRUE)) source(f)
