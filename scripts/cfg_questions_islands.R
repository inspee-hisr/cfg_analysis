#!/usr/bin/env Rscript
# Island biogeography questions: SAR, richness rankings, isolation, beta diversity.
# Depends on cfg_spatial_analysis.R outputs: cfg_island_areas.tsv,
#   cfg_island_species.tsv, cfg_caves_island_summary.tsv.

.libPaths(c("/workspace/.Rlib", .libPaths()))
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(scales)
library(vegan)
library(ggrepel)

source("scripts/cfg_load_data.R")
source("scripts/cfg_plot_style.R")

# ── helpers ──────────────────────────────────────────────────────────────────
save_tsv <- function(df, name) {
    write_delim(df, file.path("results", paste0(name, ".tsv")), delim = "\t")
    invisible(df)
}
save_plot <- function(p, name, w = 18, h = 12) {
    ggsave(file.path("plots", paste0(name, ".png")),
           plot = p, width = w, height = h, units = "cm", dpi = 300)
    invisible(p)
}

################################################################
cat("\n================================================================\n")
cat("LOADING PRECOMPUTED ISLAND DATA\n")
cat("================================================================\n")

island_areas   <- read_delim("results/cfg_island_areas.tsv",
                              delim = "\t", show_col_types = FALSE)
island_species <- read_delim("results/cfg_island_species.tsv",
                              delim = "\t", show_col_types = FALSE)
caves_island   <- read_delim("results/cfg_caves_island_summary.tsv",
                              delim = "\t", show_col_types = FALSE)

cat("Island areas:", nrow(island_areas), "rows\n")
cat("Island species:", nrow(island_species), "rows\n")

# ── build island-level summary ────────────────────────────────────────────────
island_sp_counts <- island_species |>
    filter(is_island) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    group_by(NAME_2, NAME_3) |>
    summarise(
        n_species     = n_distinct(Species),
        n_endemic     = n_distinct(Species[Distribution == "Endemic to Greece"]),
        n_obligate    = n_distinct(Species[Classification %in% c("Troglobiont","Stygobiont")]),
        n_troglobiont = n_distinct(Species[Classification == "Troglobiont"]),
        .groups       = "drop"
    )

island_cave_counts <- caves_island |>
    filter(is_island) |>
    group_by(NAME_2, NAME_3) |>
    summarise(n_caves = n_distinct(Cave_ID), .groups = "drop")

island_summary <- island_areas |>
    left_join(island_sp_counts,   by = c("NAME_2", "NAME_3")) |>
    left_join(island_cave_counts, by = c("NAME_2", "NAME_3")) |>
    mutate(across(c(n_species, n_caves, n_endemic, n_obligate, n_troglobiont),
                  ~ replace_na(.x, 0L)),
           area_island_km2 = as.numeric(area_island_km2),
           species_per_km2  = round(n_species / pmax(area_island_km2, 0.01), 5),
           endemic_per_km2  = round(n_endemic / pmax(area_island_km2, 0.01), 5)) |>
    filter(n_caves > 0) |>
    arrange(desc(n_species))

cat("Islands with ≥ 1 cave:", nrow(island_summary), "\n")

################################################################
cat("\n================================================================\n")
cat("ISLANDS — SUMMARY TABLE\n")
cat("================================================================\n")

print(island_summary |> select(NAME_2, NAME_3, area_island_km2,
                                n_caves, n_species, n_endemic, n_obligate, n_troglobiont))
save_tsv(island_summary, "q_islands_summary")

p_bubble <- ggplot(island_summary |> filter(area_island_km2 > 0, n_species > 0),
                   aes(x = area_island_km2, y = n_species,
                       size = n_caves, colour = NAME_2,
                       label = NAME_3)) +
    geom_point(alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = NAME_3), size = 2.5, max.overlaps = 20,
                             segment.colour = "#cccccc") +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_size_continuous(name = "Caves", range = c(2, 12)) +
    scale_colour_viridis_d(option = "D", name = "Island group") +
    labs(title    = "Island summary: area vs cave species richness",
         subtitle = "Bubble size = number of caves; log–log axes",
         x = "Island area (km², log scale)", y = "Total species (log scale)") +
    theme_cfg() + theme(legend.position = "right")
save_plot(p_bubble, "q_islands_summary", w = 24, h = 16)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — SPECIES–AREA RELATIONSHIPS\n")
cat("================================================================\n")

sar_data <- island_summary |>
    filter(area_island_km2 > 0)

# SAR for total, endemic, and obligate species
run_sar <- function(df, y_col, label) {
    y_vals      <- df[[y_col]]
    df_fit      <- df[y_vals > 0, ]
    df_fit$ysar <- df_fit[[y_col]]
    if (nrow(df_fit) < 3) return(NULL)
    fit <- lm(log10(ysar) ~ log10(area_island_km2), data = df_fit)
    s   <- summary(fit)
    tibble(
        type        = label,
        n_islands   = nrow(df_fit),
        slope_z     = round(coef(fit)["log10(area_island_km2)"], 3),
        intercept_c = round(coef(fit)["(Intercept)"], 3),
        r_squared   = round(s$r.squared, 3),
        p_value     = round(coef(s)["log10(area_island_km2)", "Pr(>|t|)"], 5)
    )
}

sar_results <- bind_rows(
    run_sar(sar_data, "n_species",     "All species"),
    run_sar(sar_data, "n_endemic",     "Endemic to Greece"),
    run_sar(sar_data, "n_obligate",    "Obligate (Troglo+Stygo)"),
    run_sar(sar_data, "n_troglobiont", "Troglobiont only")
)
print(sar_results)
save_tsv(sar_results, "q_islands_sar_results")

# Plot: log-log SAR
sar_long <- sar_data |>
    pivot_longer(cols = c(n_species, n_endemic, n_obligate),
                 names_to = "type", values_to = "richness") |>
    mutate(type = recode(type,
                         n_species  = "All species",
                         n_endemic  = "Endemic to Greece",
                         n_obligate = "Obligate")) |>
    filter(richness > 0)

p_sar <- ggplot(sar_long,
                aes(x = area_island_km2, y = richness, colour = type)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.9,
                aes(fill = type), alpha = 0.15) +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_colour_manual(values = c("All species" = "#0072B2", "Obligate" = "#009E73",
                                   "Endemic to Greece" = "#D55E00"), name = NULL) +
    scale_fill_manual(values   = c("All species" = "#0072B2", "Obligate" = "#009E73",
                                   "Endemic to Greece" = "#D55E00"), name = NULL) +
    labs(title    = "Species–area relationship (SAR) for Greek cave fauna",
         subtitle = "log–log regression; one point per island with ≥ 1 cave",
         x = "Island area (km², log scale)", y = "Species richness (log scale)") +
    theme_cfg() +
    theme(legend.position = c(0.18, 0.85),
          legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))
save_plot(p_sar, "q_islands_sar", w = 20, h = 14)

# Caves–area relationship
p_car <- ggplot(sar_data |> filter(n_caves > 0),
                aes(x = area_island_km2, y = n_caves)) +
    geom_point(aes(colour = NAME_2), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "black", linewidth = 0.8) +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10() +
    scale_colour_viridis_d(option = "D", name = "Island group") +
    labs(title    = "Caves–area relationship across Greek islands",
         subtitle = "log–log axes",
         x = "Island area (km², log scale)", y = "Number of caves (log scale)") +
    theme_cfg()
save_plot(p_car, "q_islands_caves_area", w = 20, h = 13)

save_tsv(sar_data |>
             select(NAME_2, NAME_3, area_island_km2, n_caves,
                    n_species, n_endemic, n_obligate, n_troglobiont,
                    species_per_km2, endemic_per_km2),
         "q_islands_sar_data")

################################################################
cat("\n================================================================\n")
cat("ISLANDS — EFFORT-CORRECTED SAR\n")
cat("================================================================\n")

# ── Add effort and non-obligate column ───────────────────────────────────────
# n_caves is the effort proxy: number of distinct caves sampled per island.
# n_refs is a secondary effort proxy (references covering island caves).
cave_refs_island <- census_long_man |>
    distinct(Cave_ID, Reference_ID) |>
    inner_join(caves_island |> filter(is_island) |>
                   select(Cave_ID, NAME_3),
               by = "Cave_ID") |>
    group_by(NAME_3) |>
    summarise(n_refs_island = n_distinct(Reference_ID), .groups = "drop")

sar_eff <- sar_data |>
    mutate(n_non_obligate = pmax(n_species - n_obligate, 0L),
           log_A          = log10(area_island_km2),
           log_E          = log10(pmax(n_caves, 1L))) |>
    left_join(cave_refs_island, by = "NAME_3") |>
    mutate(log_E_refs = log10(pmax(replace_na(n_refs_island, 1L), 1L)))

cat("Islands used for effort-corrected SAR:", nrow(sar_eff), "\n")
cat("Effort range (n_caves):", min(sar_eff$n_caves), "–", max(sar_eff$n_caves), "\n")

# ── Helper: fit effort-corrected model ───────────────────────────────────────
# Model: log10(S) ~ log10(A) + log10(n_caves)
# Partial coefficient on log10(A) is the effort-corrected z.
run_sar_eff <- function(df, y_col, label, effort_col = "log_E") {
    df_fit      <- df[df[[y_col]] > 0 & df$n_caves >= 1, ]
    df_fit$ysar <- df_fit[[y_col]]
    df_fit$logE <- df_fit[[effort_col]]
    if (nrow(df_fit) < 5) return(NULL)
    fit_naive <- lm(log10(ysar) ~ log_A,       data = df_fit)
    fit_eff   <- lm(log10(ysar) ~ log_A + logE, data = df_fit)
    ci_naive  <- confint(fit_naive, "log_A", level = 0.95)
    ci_eff    <- confint(fit_eff,   "log_A", level = 0.95)
    tibble(
        type          = label,
        n_islands     = nrow(df_fit),
        z_naive       = round(coef(fit_naive)["log_A"], 3),
        z_naive_lo    = round(ci_naive[1], 3),
        z_naive_hi    = round(ci_naive[2], 3),
        z_eff         = round(coef(fit_eff)["log_A"], 3),
        z_eff_lo      = round(ci_eff[1], 3),
        z_eff_hi      = round(ci_eff[2], 3),
        beta_effort   = round(coef(fit_eff)["logE"], 3),
        r2_naive      = round(summary(fit_naive)$r.squared, 3),
        r2_eff        = round(summary(fit_eff)$r.squared, 3),
        p_area_eff    = round(coef(summary(fit_eff))["log_A", "Pr(>|t|)"], 5),
        p_effort      = round(coef(summary(fit_eff))["logE",  "Pr(>|t|)"], 5)
    )
}

sar_eff_results <- bind_rows(
    run_sar_eff(sar_eff, "n_species",       "All species"),
    run_sar_eff(sar_eff, "n_obligate",      "Obligate (Troglo+Stygo)"),
    run_sar_eff(sar_eff, "n_non_obligate",  "Non-obligate"),
    run_sar_eff(sar_eff, "n_endemic",       "Endemic to Greece")
)
cat("\nEffort-corrected SAR slopes:\n")
print(sar_eff_results |> select(type, n_islands, z_naive, z_eff, z_eff_lo, z_eff_hi,
                                 beta_effort, r2_eff, p_area_eff))
save_tsv(sar_eff_results, "q_islands_sar_effort_corrected")

# ── Test: obligate z vs non-obligate z ───────────────────────────────────────
# Method 1 — Wald test on separate models (independent coefficients)
# The two groups are mutually exclusive so models are independent;
# SE of the difference = sqrt(SE_obl^2 + SE_nobl^2).
cat("\n--- Slope comparison: obligate vs non-obligate ---\n")
df_obl_fit  <- sar_eff |> filter(n_obligate     > 0, n_caves >= 1)
df_nobl_fit <- sar_eff |> filter(n_non_obligate > 0, n_caves >= 1)

fit_obl  <- lm(log10(n_obligate)     ~ log_A + log_E, data = df_obl_fit)
fit_nobl <- lm(log10(n_non_obligate) ~ log_A + log_E, data = df_nobl_fit)

z_obl  <- coef(fit_obl) ["log_A"];  se_obl  <- coef(summary(fit_obl)) ["log_A","Std. Error"]
z_nobl <- coef(fit_nobl)["log_A"];  se_nobl <- coef(summary(fit_nobl))["log_A","Std. Error"]

delta_z    <- round(z_obl - z_nobl, 3)
se_delta   <- sqrt(se_obl^2 + se_nobl^2)
wald_stat  <- delta_z / se_delta
delta_z_ci <- round(delta_z + c(-1, 1) * qnorm(0.975) * se_delta, 3)
wald_p     <- round(2 * (1 - pnorm(abs(wald_stat))), 5)   # two-sided
wald_p_one <- round(pnorm(-wald_stat), 5)                  # one-sided: H_A: z_obl > z_nobl

cat("z_obligate (effort-corrected) =", round(z_obl, 3),
    "   SE =", round(se_obl, 3), "\n")
cat("z_non-obligate (effort-corrected) =", round(z_nobl, 3),
    "   SE =", round(se_nobl, 3), "\n")
cat("z_obligate − z_non-obligate = ", delta_z,
    "  95% CI [", delta_z_ci[1], ",", delta_z_ci[2], "]\n")
cat("  Wald two-sided p =", wald_p,
    "  one-sided (z_obl > z_nobl) p =", wald_p_one, "\n")
if (delta_z_ci[1] > 0) {
    cat("  → CI entirely > 0: obligate z significantly STEEPER after effort correction.\n")
    cat("  → Consistent with stronger dispersal limitation in obligate cave fauna.\n")
} else {
    cat("  → CI overlaps 0: slope difference not significant at α = 0.05.\n")
}

# Method 2 — Fully parameterized stacked model (each group gets its own effort slope)
# fauna*(log_A + log_E) allows separate z and beta_effort per fauna type.
# The fauna:log_A interaction is the slope difference; no shared-slope constraint.
df_stacked <- bind_rows(
    df_obl_fit  |> transmute(NAME_3, richness = n_obligate,     log_A, log_E, fauna = "Obligate"),
    df_nobl_fit |> transmute(NAME_3, richness = n_non_obligate, log_A, log_E, fauna = "Non-obligate")
) |> mutate(fauna = factor(fauna, levels = c("Non-obligate", "Obligate")))

fit_stack <- lm(log10(richness) ~ fauna * (log_A + log_E), data = df_stacked)
stack_sum <- summary(fit_stack)
stack_ci  <- confint(fit_stack, level = 0.95)
int_term  <- "faunaObligate:log_A"
stacked_delta    <- round(coef(fit_stack)[int_term], 3)
stacked_delta_ci <- round(stack_ci[int_term, ], 3)
stacked_delta_p  <- round(coef(stack_sum)[int_term, "Pr(>|t|)"], 5)
cat("Stacked model (fauna*(log_A+log_E)):\n")
cat("  Interaction (z_obl − z_nobl) =", stacked_delta,
    "  95% CI [", stacked_delta_ci[1], ",", stacked_delta_ci[2], "]\n")
cat("  p (interaction) =", stacked_delta_p, "\n")

# ── Bootstrap sensitivity check ──────────────────────────────────────────────
# Resample islands (with replacement) 4 000 times; refit both effort-corrected
# models; record z_obligate − z_non-obligate.
cat("\nBootstrap test (B = 4000): z_obligate − z_non-obligate\n")
set.seed(2024)
B <- 4000

boot_diff <- vapply(seq_len(B), function(i) {
    tryCatch({
        b_o  <- df_obl_fit [sample(nrow(df_obl_fit),  replace = TRUE), ]
        b_n  <- df_nobl_fit[sample(nrow(df_nobl_fit), replace = TRUE), ]
        zo   <- coef(lm(log10(n_obligate)     ~ log_A + log_E, data = b_o))["log_A"]
        zn   <- coef(lm(log10(n_non_obligate) ~ log_A + log_E, data = b_n))["log_A"]
        zo - zn
    }, error = function(e) NA_real_)
}, FUN.VALUE = numeric(1))

boot_diff <- boot_diff[!is.na(boot_diff)]
boot_ci95 <- quantile(boot_diff, c(0.025, 0.975))
boot_p    <- mean(boot_diff <= 0)   # one-sided: P(z_obl ≤ z_nobl)

cat("Bootstrap 95% CI of (z_obligate − z_non-obligate): [",
    round(boot_ci95[1], 3), ",", round(boot_ci95[2], 3), "]\n")
cat("Bootstrap P(z_obligate ≤ z_non-obligate) =", round(boot_p, 4), "\n")

slope_comparison <- tibble(
    method      = c("Wald test (independent models)",
                    "Stacked model (fauna * (log_A + log_E))",
                    "Bootstrap (B = 4 000)"),
    delta_z     = c(delta_z,         stacked_delta,         round(mean(boot_diff), 3)),
    ci_lo       = c(delta_z_ci[1],   stacked_delta_ci[1],   round(boot_ci95[1], 3)),
    ci_hi       = c(delta_z_ci[2],   stacked_delta_ci[2],   round(boot_ci95[2], 3)),
    p_value     = c(wald_p_one,      stacked_delta_p,       round(boot_p, 4)),
    p_type      = c("one-sided (z_obl>z_nobl)", "two-sided", "one-sided (z_obl>z_nobl)"),
    significant = c(wald_p_one < 0.05, stacked_delta_p < 0.05, boot_p < 0.05)
)
print(slope_comparison)
save_tsv(slope_comparison, "q_islands_sar_slope_comparison")

# ── Partial regression plot (effort partialled out) ───────────────────────────
# Partial residuals: regress out log_E from both log_A and log10(richness),
# then plot residuals of richness ~ residuals of area. This is the visual
# equivalent of the effort-corrected slope.
partial_df <- function(df, y_col, label) {
    df_fit <- df[df[[y_col]] > 0 & df$n_caves >= 1, ]
    df_fit$ysar <- df_fit[[y_col]]
    resid_A <- residuals(lm(log_A        ~ log_E, data = df_fit))
    resid_S <- residuals(lm(log10(ysar) ~ log_E, data = df_fit))
    tibble(NAME_3 = df_fit$NAME_3, res_A = resid_A, res_S = resid_S, type = label)
}

partial_data <- bind_rows(
    partial_df(sar_eff, "n_species",      "All species"),
    partial_df(sar_eff, "n_obligate",     "Obligate"),
    partial_df(sar_eff, "n_non_obligate", "Non-obligate")
)

p_partial <- ggplot(partial_data, aes(x = res_A, y = res_S, colour = type)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                aes(fill = type), alpha = 0.15, linewidth = 0.9) +
    scale_colour_manual(
        values = c("All species"  = "#0072B2",
                   "Obligate"     = "#009E73",
                   "Non-obligate" = "#D55E00"),
        name = NULL) +
    scale_fill_manual(
        values = c("All species"  = "#0072B2",
                   "Obligate"     = "#009E73",
                   "Non-obligate" = "#D55E00"),
        name = NULL) +
    labs(title    = "Effort-corrected species–area relationship",
         subtitle = paste0(
             "Partial regression: log10(richness) ~ log10(area) | log10(n_caves)\n",
             "Obligate z = ", filter(sar_eff_results, type == "Obligate (Troglo+Stygo)")$z_eff,
             " [", filter(sar_eff_results, type == "Obligate (Troglo+Stygo)")$z_eff_lo,
             "–", filter(sar_eff_results, type == "Obligate (Troglo+Stygo)")$z_eff_hi, "]",
             "   Non-obligate z = ", filter(sar_eff_results, type == "Non-obligate")$z_eff,
             " [", filter(sar_eff_results, type == "Non-obligate")$z_eff_lo,
             "–", filter(sar_eff_results, type == "Non-obligate")$z_eff_hi, "]"),
         x = "log10(area)  |  effort",
         y = "log10(richness)  |  effort") +
    theme_cfg() +
    theme(legend.position = c(0.18, 0.88),
          legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))
save_plot(p_partial, "q_islands_sar_partial", w = 20, h = 14)

# ── CI comparison plot: naive vs effort-corrected z ───────────────────────────
z_compare <- sar_eff_results |>
    select(type, z_naive, z_naive_lo, z_naive_hi, z_eff, z_eff_lo, z_eff_hi) |>
    pivot_longer(c(z_naive, z_eff),
                 names_to = "model", values_to = "z") |>
    mutate(
        ci_lo = if_else(model == "z_naive", z_naive_lo, z_eff_lo),
        ci_hi = if_else(model == "z_naive", z_naive_hi, z_eff_hi),
        model = recode(model,
                       z_naive = "Naive SAR",
                       z_eff   = "Effort-corrected SAR")
    ) |>
    select(type, model, z, ci_lo, ci_hi) |>
    filter(type != "Endemic to Greece")   # keep the key comparison clean

p_z <- ggplot(z_compare,
              aes(x = type, y = z, colour = model,
                  ymin = ci_lo, ymax = ci_hi)) +
    geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4) +
    geom_pointrange(position = position_dodge(width = 0.45), size = 0.7) +
    scale_colour_manual(values = c("Naive SAR"           = "#aaaaaa",
                                   "Effort-corrected SAR" = "#0072B2"),
                        name = NULL) +
    labs(title    = "SAR slopes before and after effort correction",
         subtitle = "Points = z (slope); bars = 95% CI.  Effort proxy = log10(n_caves)",
         x = NULL, y = "SAR slope z  [95% CI]") +
    theme_cfg() +
    theme(legend.position = "bottom")
save_plot(p_z, "q_islands_sar_slope_comparison", w = 18, h = 12)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — SAR BY TAXONOMIC ORDER\n")
cat("================================================================\n")

# Per-island species count by Order, joined to area + effort predictors
island_order_eff <- island_species |>
    filter(is_island) |>
    left_join(species |> select(Species_Full_Name, Order),
              by = c("Species" = "Species_Full_Name")) |>
    filter(!is.na(Order)) |>
    group_by(NAME_2, NAME_3, Order) |>
    summarise(n_species = n_distinct(Species), .groups = "drop") |>
    inner_join(sar_eff |> select(NAME_2, NAME_3, area_island_km2, n_caves, log_A, log_E),
               by = c("NAME_2", "NAME_3")) |>
    filter(n_caves >= 1)

# Effort-corrected SAR for each Order with ≥ 5 islands recording that order
orders_split <- island_order_eff |>
    group_by(Order) |>
    group_split()

sar_order_results <- lapply(orders_split, function(df) {
    ord    <- df$Order[1]
    df_fit <- df[df$n_species > 0, ]
    if (nrow(df_fit) < 5) return(NULL)
    tryCatch({
        fit_naive <- lm(log10(n_species) ~ log_A,          data = df_fit)
        fit_eff   <- lm(log10(n_species) ~ log_A + log_E,  data = df_fit)
        ci_naive  <- confint(fit_naive, "log_A", level = 0.95)
        ci_eff    <- confint(fit_eff,   "log_A", level = 0.95)
        tibble(
            Order           = ord,
            n_islands       = nrow(df_fit),
            n_species_total = sum(df_fit$n_species),
            z_naive         = round(coef(fit_naive)["log_A"], 3),
            z_naive_lo      = round(ci_naive[1], 3),
            z_naive_hi      = round(ci_naive[2], 3),
            z_eff           = round(coef(fit_eff)["log_A"], 3),
            z_eff_lo        = round(ci_eff[1], 3),
            z_eff_hi        = round(ci_eff[2], 3),
            beta_effort     = round(coef(fit_eff)["log_E"], 3),
            r2_naive        = round(summary(fit_naive)$r.squared, 3),
            r2_eff          = round(summary(fit_eff)$r.squared, 3),
            p_area_eff      = round(coef(summary(fit_eff))["log_A", "Pr(>|t|)"], 5),
            p_effort        = round(coef(summary(fit_eff))["log_E",  "Pr(>|t|)"], 5)
        )
    }, error = function(e) NULL)
}) |> bind_rows() |> arrange(desc(n_islands))

cat("Orders with ≥ 5 islands:", nrow(sar_order_results), "\n")
print(sar_order_results |>
          select(Order, n_islands, n_species_total,
                 z_naive, z_eff, z_eff_lo, z_eff_hi, p_area_eff, beta_effort))
save_tsv(sar_order_results, "q_islands_sar_by_order")

# Forest plot: effort-corrected z per Order, sorted by z_eff
p_h <- max(8, nrow(sar_order_results) * 0.7 + 3)
p_order_z <- ggplot(
        sar_order_results |>
            mutate(Order    = fct_reorder(Order, z_eff),
                   sig_area = p_area_eff < 0.05),
        aes(x = Order, y = z_eff, ymin = z_eff_lo, ymax = z_eff_hi,
            colour = sig_area)) +
    geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4, linetype = "dashed") +
    geom_pointrange(size = 0.5, linewidth = 0.7) +
    scale_colour_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#999999"),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name = "Area effect") +
    coord_flip() +
    labs(title    = "Effort-corrected SAR slope by taxonomic Order",
         subtitle = "z from log₁₀(S) ~ log₁₀(A) + log₁₀(n caves); orders with ≥ 5 island presences",
         x = NULL, y = "SAR slope z  [95% CI]") +
    theme_cfg_bar()
save_plot(p_order_z, "q_islands_sar_by_order", w = 20, h = p_h)

# Facet plot: log-log SAR scatter + regression line per Order
# Join slope labels from results table
order_labels <- sar_order_results |>
    mutate(facet_label = paste0(Order,
                                "\nz = ", z_eff,
                                ifelse(p_area_eff < 0.05, "*", ""),
                                "  n = ", n_islands))

sar_order_plot_data <- island_order_eff |>
    filter(Order %in% sar_order_results$Order, n_species > 0) |>
    left_join(order_labels |> select(Order, facet_label, p_area_eff), by = "Order") |>
    mutate(sig_area = p_area_eff < 0.05)

n_orders  <- n_distinct(sar_order_plot_data$Order)
n_cols    <- 5L
n_rows    <- ceiling(n_orders / n_cols)

p_order_facet <- ggplot(sar_order_plot_data,
                        aes(x = area_island_km2, y = n_species)) +
    geom_point(aes(colour = sig_area), size = 1.8, alpha = 0.75) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#333333", fill = "#cccccc",
                linewidth = 0.7, alpha = 0.25) +
    scale_x_log10(labels = label_comma(accuracy = 1)) +
    scale_y_log10() +
    scale_colour_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#999999"),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name = "Area effect") +
    facet_wrap(~ facet_label, ncol = n_cols, scales = "free") +
    labs(title    = "Species–area relationships by taxonomic Order",
         subtitle = "log–log axes; regression line with 95% CI; * = p < 0.05 for area term after effort correction",
         x = "Island area (km²)", y = "Species richness") +
    theme_cfg() +
    theme(legend.position   = "bottom",
          strip.text        = element_text(size = rel(0.78)),
          axis.text         = element_text(size = rel(0.75)))
save_plot(p_order_facet, "q_islands_sar_by_order_facet",
          w = n_cols * 8, h = n_rows * 7)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — ENDEMIC SAR: OVERALL AND BY ORDER\n")
cat("================================================================\n")

# ── Overall endemic SAR ───────────────────────────────────────────────────────
cat("\n--- Overall endemic SAR ---\n")
sar_endemic_eff <- sar_eff |> filter(n_endemic > 0)

fit_end_naive <- lm(log10(n_endemic) ~ log_A,         data = sar_endemic_eff)
fit_end_eff   <- lm(log10(n_endemic) ~ log_A + log_E, data = sar_endemic_eff)
ci_end_eff    <- confint(fit_end_eff, "log_A", level = 0.95)

z_end_naive <- round(coef(fit_end_naive)["log_A"], 3)
z_end_eff   <- round(coef(fit_end_eff)["log_A"], 3)
z_end_lo    <- round(ci_end_eff[1], 3)
z_end_hi    <- round(ci_end_eff[2], 3)
p_end_area  <- round(coef(summary(fit_end_eff))["log_A", "Pr(>|t|)"], 5)
b_end_eff   <- round(coef(fit_end_eff)["log_E"], 3)

cat("Naive z =", z_end_naive,
    "  Effort-corrected z =", z_end_eff,
    "  95% CI [", z_end_lo, ",", z_end_hi, "]",
    "  p =", p_end_area, "\n")
cat("Beta effort:", b_end_eff, "\n")

# Raw log-log scatter (naive line shown; both z values in subtitle)
p_end_overall <- ggplot(sar_endemic_eff,
                        aes(x = area_island_km2, y = n_endemic)) +
    geom_point(colour = "#D55E00", size = 2.5, alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = NAME_3), size = 2.2,
                             colour = "#555555", max.overlaps = 15,
                             segment.colour = "#cccccc") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#333333", fill = "#cccccc",
                linewidth = 0.8, alpha = 0.25) +
    scale_x_log10(labels = label_comma(accuracy = 1)) +
    scale_y_log10() +
    labs(title    = "Endemic species–area relationship across Greek islands",
         subtitle = paste0("Naive z = ", z_end_naive,
                           "   Effort-corrected z = ", z_end_eff,
                           " [", z_end_lo, "–", z_end_hi, "]",
                           "   p = ", p_end_area,
                           "   β_effort = ", b_end_eff,
                           "   n = ", nrow(sar_endemic_eff), " islands"),
         x = "Island area (km²)", y = "Endemic species richness (log scale)") +
    theme_cfg()
save_plot(p_end_overall, "q_islands_sar_endemic_overall", w = 22, h = 15)

# ── Per-order endemic SAR ─────────────────────────────────────────────────────
cat("\n--- Per-order endemic SAR ---\n")

island_endemic_order <- island_species |>
    filter(is_island) |>
    left_join(species |> select(Species_Full_Name, Order, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    filter(!is.na(Order), Distribution == "Endemic to Greece") |>
    group_by(NAME_2, NAME_3, Order) |>
    summarise(n_endemic = n_distinct(Species), .groups = "drop") |>
    inner_join(sar_eff |> select(NAME_2, NAME_3, area_island_km2, n_caves, log_A, log_E),
               by = c("NAME_2", "NAME_3")) |>
    filter(n_caves >= 1)

sar_endemic_order_results <- island_endemic_order |>
    group_by(Order) |>
    group_split() |>
    lapply(function(df) {
        ord    <- df$Order[1]
        df_fit <- df[df$n_endemic > 0, ]
        if (nrow(df_fit) < 5) return(NULL)
        tryCatch({
            fit_naive <- lm(log10(n_endemic) ~ log_A,          data = df_fit)
            fit_eff   <- lm(log10(n_endemic) ~ log_A + log_E,  data = df_fit)
            ci_naive  <- confint(fit_naive, "log_A", level = 0.95)
            ci_eff    <- confint(fit_eff,   "log_A", level = 0.95)
            tibble(
                Order           = ord,
                n_islands       = nrow(df_fit),
                n_endemic_total = sum(df_fit$n_endemic),
                z_naive         = round(coef(fit_naive)["log_A"], 3),
                z_naive_lo      = round(ci_naive[1], 3),
                z_naive_hi      = round(ci_naive[2], 3),
                z_eff           = round(coef(fit_eff)["log_A"], 3),
                z_eff_lo        = round(ci_eff[1], 3),
                z_eff_hi        = round(ci_eff[2], 3),
                beta_effort     = round(coef(fit_eff)["log_E"], 3),
                r2_naive        = round(summary(fit_naive)$r.squared, 3),
                r2_eff          = round(summary(fit_eff)$r.squared, 3),
                p_area_eff      = round(coef(summary(fit_eff))["log_A", "Pr(>|t|)"], 5),
                p_effort        = round(coef(summary(fit_eff))["log_E",  "Pr(>|t|)"], 5)
            )
        }, error = function(e) NULL)
    }) |>
    bind_rows() |>
    arrange(desc(n_islands))

cat("Orders with ≥ 5 islands for endemic SAR:", nrow(sar_endemic_order_results), "\n")
print(sar_endemic_order_results |>
          select(Order, n_islands, n_endemic_total,
                 z_naive, z_eff, z_eff_lo, z_eff_hi, p_area_eff, beta_effort))
save_tsv(sar_endemic_order_results, "q_islands_sar_endemic_by_order")

# Forest plot (vermillion for endemic to distinguish from total-species blue)
p_h_end <- max(8, nrow(sar_endemic_order_results) * 0.7 + 3)
p_end_order_z <- ggplot(
        sar_endemic_order_results |>
            mutate(Order    = fct_reorder(Order, z_eff),
                   sig_area = p_area_eff < 0.05),
        aes(x = Order, y = z_eff, ymin = z_eff_lo, ymax = z_eff_hi,
            colour = sig_area)) +
    geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4, linetype = "dashed") +
    geom_pointrange(size = 0.5, linewidth = 0.7) +
    scale_colour_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#999999"),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name = "Area effect") +
    coord_flip() +
    labs(title    = "Effort-corrected endemic SAR slope by taxonomic Order",
         subtitle = "z from log₁₀(endemics) ~ log₁₀(A) + log₁₀(n caves); orders with ≥ 5 island presences",
         x = NULL, y = "SAR slope z  [95% CI]") +
    theme_cfg_bar()
save_plot(p_end_order_z, "q_islands_sar_endemic_by_order", w = 20, h = p_h_end)

# Facet plot: one panel per qualifying Order
end_order_labels <- sar_endemic_order_results |>
    mutate(facet_label = paste0(Order,
                                "\nz = ", z_eff,
                                ifelse(p_area_eff < 0.05, "*", ""),
                                "  n = ", n_islands))

sar_end_plot_data <- island_endemic_order |>
    filter(Order %in% sar_endemic_order_results$Order, n_endemic > 0) |>
    left_join(end_order_labels |> select(Order, facet_label, p_area_eff), by = "Order") |>
    mutate(sig_area = p_area_eff < 0.05)

n_end_orders <- n_distinct(sar_end_plot_data$Order)
n_end_cols   <- min(5L, n_end_orders)
n_end_rows   <- ceiling(n_end_orders / n_end_cols)

p_end_facet <- ggplot(sar_end_plot_data,
                      aes(x = area_island_km2, y = n_endemic)) +
    geom_point(aes(colour = sig_area), size = 1.8, alpha = 0.75) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#333333", fill = "#cccccc",
                linewidth = 0.7, alpha = 0.25) +
    scale_x_log10(labels = label_comma(accuracy = 1)) +
    scale_y_log10() +
    scale_colour_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#999999"),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                        name = "Area effect") +
    facet_wrap(~ facet_label, ncol = n_end_cols, scales = "free") +
    labs(title    = "Endemic species–area relationships by taxonomic Order",
         subtitle = "log–log axes; * = p < 0.05 for area term (effort-corrected)",
         x = "Island area (km²)", y = "Endemic species richness") +
    theme_cfg() +
    theme(legend.position = "bottom",
          strip.text      = element_text(size = rel(0.78)),
          axis.text       = element_text(size = rel(0.75)))
save_plot(p_end_facet, "q_islands_sar_endemic_by_order_facet",
          w = n_end_cols * 8, h = n_end_rows * 7)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — RICHNESS RANKINGS\n")
cat("================================================================\n")

cat("\n--- Q: Island richness rankings ---\n")

# Top by total species
cat("Top 10 by total species:\n")
print(island_summary |> head(10) |> select(NAME_2, NAME_3, area_island_km2, n_caves, n_species))

# Top by species per km²
cat("\nTop 10 by species density (per km²):\n")
print(island_summary |> arrange(desc(species_per_km2)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_species, species_per_km2))

# Top by endemic count
cat("\nTop 10 by endemic species:\n")
print(island_summary |> arrange(desc(n_endemic)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_endemic, endemic_per_km2))

# Top by troglobiont count
cat("\nTop 10 by troglobiont species:\n")
print(island_summary |> arrange(desc(n_troglobiont)) |>
          head(10) |> select(NAME_2, NAME_3, area_island_km2, n_troglobiont))

save_tsv(island_summary |>
             mutate(rank_total    = rank(-n_species,     ties.method = "min"),
                    rank_density  = rank(-species_per_km2, ties.method = "min"),
                    rank_endemic  = rank(-n_endemic,     ties.method = "min"),
                    rank_troglo   = rank(-n_troglobiont, ties.method = "min")) |>
             arrange(rank_total),
         "q_islands_richness_rank")

# Richness ranking plot (top 15 by total species)
top15 <- island_summary |>
    head(15) |>
    mutate(NAME_3 = fct_reorder(NAME_3, n_species))

rank_long <- top15 |>
    pivot_longer(cols = c(n_species, n_endemic, n_troglobiont),
                 names_to = "type", values_to = "n") |>
    mutate(type = recode(type,
                         n_species     = "Total species",
                         n_endemic     = "Endemic",
                         n_troglobiont = "Troglobiont"))

p_rank <- ggplot(rank_long,
                 aes(x = NAME_3, y = n, fill = type)) +
    geom_col(position = "dodge", width = 0.75) +
    geom_text(aes(label = n), position = position_dodge(0.75),
              hjust = -0.2, size = 2.8, colour = "#333333") +
    scale_fill_manual(values = c("Total species" = "#0072B2",
                                 "Endemic"        = "#D55E00",
                                 "Troglobiont"    = "#009E73"),
                      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title    = "Top 15 islands by species richness",
         subtitle = "Total species, endemics, and troglobionts",
         x = NULL, y = "Number of species") +
    theme_cfg_bar() +
    theme(legend.position = "bottom")
save_plot(p_rank, "q_islands_richness_rank", w = 22, h = 16)

# Species density plot (top 15 by density)
top15_density <- island_summary |>
    filter(area_island_km2 > 1) |>  # exclude tiny islets
    arrange(desc(species_per_km2)) |>
    head(15) |>
    mutate(NAME_3 = fct_reorder(NAME_3, species_per_km2))

p_density <- ggplot(top15_density,
                    aes(x = NAME_3, y = species_per_km2, fill = NAME_2)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = round(species_per_km2, 3)), hjust = -0.2, size = 3,
              colour = "#333333") +
    scale_fill_viridis_d(option = "D", name = "Island group") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    coord_flip() +
    labs(title    = "Top 15 islands by cave species density",
         subtitle = "Species per km² (islands > 1 km²)",
         x = NULL, y = "Species per km²") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p_density, "q_islands_density", w = 20, h = 14)

# Q: Island group comparison
cat("\n--- Q: Island group comparison ---\n")
island_group_comp <- island_summary |>
    group_by(NAME_2) |>
    summarise(
        n_islands      = n(),
        mean_caves     = round(mean(n_caves), 1),
        mean_species   = round(mean(n_species), 1),
        mean_endemic   = round(mean(n_endemic), 1),
        mean_obligate  = round(mean(n_obligate), 1),
        total_species  = sum(n_species),
        total_endemic  = sum(n_endemic),
        .groups        = "drop"
    )
print(island_group_comp)
save_tsv(island_group_comp, "q_islands_group_comparison")

p_group <- ggplot(island_summary,
                  aes(x = NAME_2, y = n_species, fill = NAME_2)) +
    geom_boxplot(outlier.alpha = 0.5, width = 0.5, show.legend = FALSE) +
    scale_fill_viridis_d(option = "D") +
    labs(title    = "Cave species richness by island group",
         subtitle = "Each box = one island in that group",
         x = NULL, y = "Total cave species per island") +
    theme_cfg()
save_plot(p_group, "q_islands_group_comparison", w = 16, h = 10)

################################################################
cat("\n================================================================\n")
cat("ISLANDS — BIOGEOGRAPHIC PATTERNS\n")
cat("================================================================\n")

# Q: Jaccard overlap between each island and the mainland fauna
cat("\n--- Q: Species overlap (Jaccard) between islands and mainland ---\n")

mainland_spp <- island_species |>
    filter(region_type == "Mainland") |>
    distinct(Species) |>
    pull(Species)

island_jaccard <- island_species |>
    filter(is_island) |>
    group_by(NAME_3) |>
    summarise(island_spp = list(unique(Species)), .groups = "drop") |>
    mutate(
        n_island   = lengths(island_spp),
        n_shared   = sapply(island_spp, function(s) sum(s %in% mainland_spp)),
        n_mainland = length(mainland_spp)
    ) |>
    mutate(
        jaccard_sim  = round(n_shared / (n_island + n_mainland - n_shared), 4),
        jaccard_dist = round(1 - jaccard_sim, 4)
    ) |>
    select(-island_spp) |>
    arrange(desc(jaccard_sim))

cat("Islands with highest Jaccard similarity to mainland:\n")
print(head(island_jaccard, 10))
save_tsv(island_jaccard, "q_islands_jaccard")

p_jacc <- ggplot(island_jaccard |>
                     filter(n_island > 0) |>
                     mutate(NAME_3 = fct_reorder(NAME_3, jaccard_sim)),
                 aes(x = NAME_3, y = jaccard_sim)) +
    geom_col(aes(fill = jaccard_sim), width = 0.75, show.legend = FALSE) +
    scale_fill_gradient(low = seq_lo, high = seq_hi) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Jaccard similarity of island cave fauna to mainland",
         subtitle = "Higher = more species shared with the mainland",
         x = NULL, y = "Jaccard similarity (0–1)") +
    theme_cfg_bar()
save_plot(p_jacc, "q_islands_jaccard", w = 18, h = 16)

# Q: Strict island endemics — species on only 1 island
cat("\n--- Q: Strict island endemics (1 island only) ---\n")
strict_island_endemic <- island_species |>
    filter(is_island) |>
    group_by(Species) |>
    summarise(
        n_islands  = n_distinct(NAME_3),
        islands    = paste(sort(unique(NAME_3)), collapse = "|"),
        NAME_3     = first(NAME_3),
        .groups    = "drop"
    ) |>
    filter(n_islands == 1) |>
    filter(!Species %in% (island_species |>
                              filter(region_type == "Mainland") |>
                              pull(Species))) |>
    left_join(species |> select(Species_Full_Name, Classification, Distribution),
              by = c("Species" = "Species_Full_Name")) |>
    arrange(NAME_3, Species)

cat("Strict single-island endemic species:", nrow(strict_island_endemic), "\n")
cat("Distribution by island:\n")
print(strict_island_endemic |> count(NAME_3, sort = TRUE))
save_tsv(strict_island_endemic, "q_islands_strict_endemic")

p_strict <- strict_island_endemic |>
    count(NAME_3, Classification) |>
    mutate(NAME_3 = fct_reorder(NAME_3, n, sum)) |>
    ggplot(aes(x = NAME_3, y = n, fill = Classification)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5),
              size = 3, colour = "white", fontface = "bold") +
    scale_fill_manual(values = clf_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip() +
    labs(title    = "Strict single-island endemic species by island",
         subtitle = "Species with records on only one island and no mainland record",
         x = NULL, y = "Number of species") +
    theme_cfg_bar() + theme(legend.position = "bottom")
save_plot(p_strict, "q_islands_strict_endemic", w = 20, h = 14)

# Q: Beta diversity — pairwise Jaccard among island groups
cat("\n--- Q: Beta diversity (Jaccard) among island groups ---\n")

group_species <- island_species |>
    filter(is_island) |>
    distinct(NAME_2, Species)

# Presence/absence matrix: rows = island groups, columns = species
island_groups <- sort(unique(group_species$NAME_2))
all_spp       <- sort(unique(group_species$Species))

pa_matrix <- matrix(0L, nrow = length(island_groups), ncol = length(all_spp),
                    dimnames = list(island_groups, all_spp))
for (grp in island_groups) {
    spp <- group_species |> filter(NAME_2 == grp) |> pull(Species)
    pa_matrix[grp, spp] <- 1L
}

# Include mainland as a group
mainland_spp_vec <- intersect(mainland_spp, all_spp)
mainland_row <- matrix(0L, nrow = 1, ncol = length(all_spp),
                       dimnames = list("Mainland", all_spp))
mainland_row[1, mainland_spp_vec] <- 1L
pa_full <- rbind(pa_matrix, mainland_row)

jacc_dist <- vegan::vegdist(pa_full, method = "jaccard")
jacc_mat  <- as.matrix(jacc_dist)

jacc_df <- as.data.frame(jacc_mat) |>
    tibble::rownames_to_column("group_from") |>
    pivot_longer(-group_from, names_to = "group_to", values_to = "jaccard_dist")

save_tsv(jacc_df, "q_islands_beta")

# Heatmap
p_heat <- ggplot(jacc_df,
                 aes(x = group_to, y = group_from, fill = jaccard_dist)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = round(jaccard_dist, 2)), size = 3.5) +
    scale_fill_gradient(low = seq_lo, high = seq_hi,
                        name = "Jaccard\ndistance") +
    labs(title    = "Beta diversity among island groups and mainland",
         subtitle = "Pairwise Jaccard distances (0 = identical, 1 = no shared species)",
         x = NULL, y = NULL) +
    theme_cfg() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(p_heat, "q_islands_beta", w = 16, h = 12)

cat("\n================================================================\n")
cat("DONE — results/ and plots/ updated\n")
cat("================================================================\n")
