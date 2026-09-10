#!/usr/bin/env Rscript
# Composite publication figures for journal submission.
# Output: figures/ — PNG + TIFF, 300 DPI, 170 × 230 mm (double-column portrait).
#
# Requires: cowplot, png, jpeg, grid (no magick needed)
# Run from repo root: Rscript scripts/cfg_composite_figures.R

library(ggplot2)
library(cowplot)
library(grid)
library(png)
library(jpeg)

dir.create("figures", showWarnings = FALSE)

# ── Constants ─────────────────────────────────────────────────────────────────
W_MM  <- 170          # double-column journal width
H_MM  <- 230          # full-page portrait height
DPI   <- 300L
W_IN  <- W_MM / 25.4
H_IN  <- H_MM / 25.4

# ── Helpers ───────────────────────────────────────────────────────────────────

# Read a PNG or JPEG and return a theme_void ggplot panel
load_panel <- function(path) {
    readable <- file.exists(path) && file.access(path, 4L) == 0L
    if (!readable) {
        return(
            ggplot() +
                annotate("text", x = .5, y = .5, label = "(unavailable)",
                         colour = "#bbbbbb", size = 3, fontface = "italic") +
                theme_void() +
                theme(plot.background = element_rect(fill = "#f5f5f5",
                                                     colour = "#e0e0e0"))
        )
    }
    ext <- tolower(tools::file_ext(path))
    raster <- switch(ext,
        png  = png::readPNG(path),
        jpeg = ,
        jpg  = jpeg::readJPEG(path),
        stop("Unsupported format: ", ext)
    )
    rg <- grid::rasterGrob(raster, interpolate = TRUE,
                           width  = unit(1, "npc"),
                           height = unit(1, "npc"))
    ggplot() +
        annotation_custom(rg, xmin = -Inf, xmax = Inf,
                              ymin = -Inf, ymax = Inf) +
        theme_void() +
        theme(plot.margin = unit(rep(1, 4), "pt"))
}

# Assemble panels into a labelled grid and save PNG + TIFF
make_figure <- function(paths, name, ncol = 2,
                        w = W_IN, h = H_IN, dpi = DPI) {
    cat(sprintf("\nFigure %s\n", name))
    panels <- lapply(paths, load_panel)
    fig <- plot_grid(
        plotlist        = panels,
        labels          = LETTERS[seq_along(panels)],
        label_size      = 9,
        label_fontface  = "bold",
        label_colour    = "black",
        label_x         = 0.01,
        label_y         = 0.99,
        hjust           = 0,
        vjust           = 1,
        ncol            = ncol
    )

    png_path  <- file.path("figures", paste0(name, ".png"))
    tiff_path <- file.path("figures", paste0(name, ".tiff"))

    ggsave(png_path, plot = fig,
           width = w, height = h, units = "in", dpi = dpi, bg = "white")

    tiff(tiff_path,
         width  = round(w * dpi),
         height = round(h * dpi),
         units  = "px", res = dpi, compression = "lzw")
    print(fig)
    dev.off()

    cat(sprintf("  → %s  (%.0f × %.0f mm, %d DPI, PNG + TIFF)\n",
                name, w * 25.4, h * 25.4, dpi))
    invisible(fig)
}

# ── Figure 1 — The CFG database ───────────────────────────────────────────────
# A: database portal screenshot   B: species richness map
# C: taxonomic diversity          D: ecological classification
# E: reference inventory          F: open-data linkage
make_figure(
    name  = "Figure1_database",
    ncol  = 2,
    paths = c(
        "plots/Screenshot_database.png",
        "plots/map_greece_plot_lines_grid_species.png",
        "plots/q_species_taxonomy.png",
        "plots/q_species_classification.png",
        "plots/q_refs_inventory.png",
        "plots/q_species_database_links.png"
    )
)

# ── Figure 2 — Timeline of subterranean biology ───────────────────────────────
# A: species per decade            B: caves per decade
# C: cumulative discovery curves   D: records per decade
# E: top authors                   F: most productive references
make_figure(
    name  = "Figure2_timeline",
    ncol  = 2,
    paths = c(
        "plots/q_species_per_decade.png",
        "plots/q_caves_per_decade.png",
        "plots/q_species_accumulation_by_classification.png",
        "plots/q_records_per_decade.png",
        "plots/q_author_contributions.png",
        "plots/q_refs_most_caves.png"
    )
)

# ── Figure 3 — Sampling effort and knowledge gaps ─────────────────────────────
# A: effort vs richness scatter    B: Chao2 completeness by region
# C: under-sampled caves           D: 'lost?' species timeline
# E: Linnaean shortfall by order   F: references per cave distribution
make_figure(
    name  = "Figure3_sampling",
    ncol  = 2,
    paths = c(
        "plots/q_caves_refs_species_scatter.png",
        "plots/q_sampling_completeness.png",
        "plots/q_caves_undersampled.png",
        "plots/q_species_lost.png",
        "plots/q_species_linnaean_shortfall.png",
        "plots/q_caves_refs_distribution.png"
    )
)

# ── Figure 4 — Endemicity and hotspots ───────────────────────────────────────
# A: endemic richness by region    B: troglobiont richness by region
# C: top endemic-proportion caves  D: top obligate-proportion caves
# E: altitudinal gradient          F: single-cave species by order
make_figure(
    name  = "Figure4_endemicity",
    ncol  = 2,
    paths = c(
        "plots/q_species_region_endemic_richness.png",
        "plots/q_species_region_troglobiont_richness.png",
        "plots/q_caves_endemic_proportion.png",
        "plots/q_caves_obligate_proportion.png",
        "plots/q_species_altitude_gradient.png",
        "plots/q_species_singlecave.png"
    )
)

# ── Figure 5 — Cave fauna and insularity ─────────────────────────────────────
# A: area vs richness bubble       B: naive SAR (all / endemic / obligate)
# C: endemic SAR with island labels D: naive vs effort-corrected slopes
# E: island richness rankings      F: inter-island beta diversity
make_figure(
    name  = "Figure5_insularity",
    ncol  = 2,
    paths = c(
        "plots/q_islands_summary.png",
        "plots/q_islands_sar.png",
        "plots/q_islands_sar_endemic_overall.png",
        "plots/q_islands_sar_slope_comparison.png",
        "plots/q_islands_richness_rank.png",
        "plots/q_islands_beta.png"
    )
)

# ── Figure 6 — Conservation gaps ─────────────────────────────────────────────
# A: Natura 2000 cave map          B: N2000 coverage by region
# C: IUCN Red List breakdown       D: troglobionts outside N2000
# E: threatened × N2000 overlap    F: threatened endemic species
make_figure(
    name  = "Figure6_conservation",
    ncol  = 2,
    paths = c(
        "plots/q_caves_natura2000_map.png",
        "plots/q_caves_natura2000_by_region.png",
        "plots/q_species_iucn.png",
        "plots/q_caves_natura2000_troglo.png",
        "plots/q_species_threatened_n2000.png",
        "plots/q_species_threatened_endemic.png"
    )
)

cat("\n================================================================\n")
cat("DONE — figures/ contains 6 × (PNG + TIFF) at 300 DPI\n")
cat("================================================================\n")
