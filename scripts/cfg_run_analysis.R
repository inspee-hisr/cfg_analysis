#!/usr/bin/env Rscript
# Run locally from the repo root after restoring renv. Stop on the first failure.
if (!file.exists("scripts/cfg_load_data.R") || !dir.exists("data")) {
    stop("Run this command from your CFG repository root.")
}

stages <- c(
    "tests/cfg_regression_checks.R",
    "scripts/cfg_spatial_analysis.R",
    "scripts/cfg_geology.R",
    "scripts/cfg_questions.R",
    "scripts/cfg_questions_caves.R",
    "scripts/cfg_questions_islands.R",
    "scripts/cfg_questions_species.R",
    "scripts/cfg_questions_temporal.R",
    "scripts/cfg_website_plots.R",
    "scripts/cfg_composite_figures.R"
)
for (directory in c("results", "plots", "website_plots", "figures")) {
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
        stop("Could not create output directory: ", directory)
    }
}
for (stage in stages) {
    cat("\nRunning ", stage, "\n", sep = "")
    status <- system2(file.path(R.home("bin"), "Rscript"), args = shQuote(stage))
    if (status != 0L) stop("Failed: ", stage, " (exit ", status, ")")
}
cat("\nFinished. TSVs: results/; figures: plots/, website_plots/, figures/.\n")
