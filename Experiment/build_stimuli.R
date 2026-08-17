# Regenerates js/pse_stimuli.js from pse_stimuli.csv.

# Usage: Rscript build_stimuli.R

library(readr)
library(jsonlite)

script_dir <- dirname(sub("--file=", "", commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))]))
csv_path <- file.path(script_dir, "pse_stimuli.csv")
out_path <- file.path(script_dir, "js", "pse_stimuli.js")

stimuli <- read_csv(csv_path, col_types = cols(.default = col_character()))

json <- toJSON(stimuli, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE)

out <- paste0(
  "// Generated from pse_stimuli.csv by build_stimuli.R. Do not edit by hand —\n",
  "// edit pse_stimuli.csv and re-run `Rscript build_stimuli.R` instead.\n",
  "var PSE_STIMULI = ", json, ";\n"
)

writeLines(out, out_path)
cat("Wrote", nrow(stimuli), "rows to", file.path("js", "pse_stimuli.js"), "\n")
