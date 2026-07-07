#!/usr/bin/env Rscript
# Shared plot style: NYT-inspired theme + colorblind-safe palettes.
# Sourced by all cfg_questions_*.R scripts after cfg_load_data.R.

# ── NYT-inspired theme ────────────────────────────────────────────────────────
theme_cfg <- function(base = 12) {
    theme_minimal(base_size = base) +
    theme(
        # Background
        panel.background   = element_blank(),
        plot.background    = element_rect(fill = "white", colour = NA),
        # Gridlines: horizontal only, very subtle
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#e5e5e5", linewidth = 0.35),
        panel.grid.minor   = element_blank(),
        # Axis
        axis.line.x        = element_line(colour = "#555555", linewidth = 0.35),
        axis.ticks.x       = element_line(colour = "#555555", linewidth = 0.25),
        axis.ticks.y       = element_blank(),
        axis.text          = element_text(colour = "#444444", size = rel(0.9)),
        axis.title         = element_text(colour = "#444444"),
        # Titles
        plot.title         = element_text(face = "bold", colour = "#111111",
                                          size = rel(1.1), hjust = 0,
                                          margin = margin(b = 4)),
        plot.subtitle      = element_text(colour = "#666666", size = rel(0.88),
                                          hjust = 0, margin = margin(b = 8)),
        plot.caption       = element_text(colour = "#aaaaaa", size = rel(0.72),
                                          hjust = 0),
        # Legend
        legend.background  = element_blank(),
        legend.key         = element_blank(),
        legend.title       = element_text(colour = "#444444", size = rel(0.85)),
        legend.text        = element_text(colour = "#444444"),
        # Facet strips
        strip.background   = element_rect(fill = "#f5f5f5", colour = NA),
        strip.text         = element_text(face = "bold", colour = "#333333",
                                          size = rel(0.9)),
        # Margins
        plot.margin        = margin(t = 10, r = 24, b = 10, l = 10)
    )
}

# Variant for coord_flip() bar charts: swap grid orientation, no ticks on value axis
theme_cfg_bar <- function(base = 12) {
    theme_cfg(base) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#e5e5e5", linewidth = 0.35),
        axis.line.x        = element_blank(),
        axis.line.y        = element_line(colour = "#555555", linewidth = 0.35),
        axis.ticks.x       = element_blank(),
        axis.ticks.y       = element_blank()
    )
}

# ── Colorblind-safe palettes ──────────────────────────────────────────────────

# Okabe-Ito (8-color, universally colorblind-safe)
okabe <- c(
    orange      = "#E69F00",
    sky_blue    = "#56B4E9",
    green       = "#009E73",
    yellow      = "#F0E442",
    blue        = "#0072B2",
    vermillion  = "#D55E00",
    pink        = "#CC79A7",
    black       = "#000000"
)

# Ecological classification palette (Okabe-Ito derived, consistent across all scripts)
clf_colours <- c(
    "Troglobiont" = "#0072B2",   # blue
    "Stygobiont"  = "#009E73",   # green
    "Troglophile" = "#56B4E9",   # sky blue
    "Stygophile"  = "#44AA88",   # muted teal
    "Trogloxene"  = "#E69F00",   # amber
    "Stygoxene"   = "#D55E00",   # vermillion
    "Accidental"  = "#999999"    # neutral gray
)

# Region palette: 13 Greek administrative regions — uses Paul Tol "muted"
region_colours <- c(
    "Attica"                      = "#332288",
    "Central Greece"              = "#117733",
    "Central Macedonia"           = "#44AA99",
    "Crete"                       = "#88CCEE",
    "Eastern Macedonia and Thrace"= "#DDCC77",
    "Epirus"                      = "#CC6677",
    "Ionian Islands"              = "#AA4499",
    "North Aegean"                = "#882255",
    "Peloponnese"                 = "#999933",
    "South Aegean"                = "#661100",
    "Thessaly"                    = "#6699CC",
    "Western Greece"              = "#888888",
    "Western Macedonia"           = "#C4A8E0"
)

# Two-colour diverging (N2000 / protection status)
prot_colours  <- c("Inside N2000"  = "#0072B2",
                   "Outside N2000" = "#D55E00")
yn_colours    <- c("Yes" = "#009E73", "No" = "#D55E00")

# Sequential: single-hue blue (use for gradients)
# call: scale_fill_gradient(low = seq_lo, high = seq_hi)
seq_lo  <- "#deebf7"
seq_hi  <- "#08306b"
