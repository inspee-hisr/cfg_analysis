# Paired island bootstrap for the difference in effort-corrected SAR slopes.
# Each sampled row carries both fauna groups, preserving their dependence.
# Zero-richness groups are excluded only when fitting their log-richness model.
sar_slope_difference <- function(df) {
    slopes <- vapply(c("n_obligate", "n_non_obligate"), function(y) {
        fit_data <- df[df[[y]] > 0, , drop = FALSE]
        if (nrow(fit_data) < 5L) return(NA_real_)
        fit_data$log_S <- log10(fit_data[[y]])
        fit <- stats::lm(log_S ~ log_A + log_E, data = fit_data)
        if (fit$rank < 3L) return(NA_real_)
        unname(stats::coef(fit)["log_A"])
    }, numeric(1))
    unname(slopes[1] - slopes[2])
}

bootstrap_sar_difference <- function(df, B = 4000L, seed = 2024L) {
    required <- c("NAME_2", "NAME_3", "n_obligate", "n_non_obligate",
                  "log_A", "log_E")
    stopifnot(all(required %in% names(df)), B >= 100L,
              !anyDuplicated(df[c("NAME_2", "NAME_3")]))
    numeric_cols <- c("n_obligate", "n_non_obligate", "log_A", "log_E")
    if (!all(vapply(df[numeric_cols], function(x) all(is.finite(x)), logical(1)))) {
        stop("SAR bootstrap requires finite richness, area and effort values.")
    }
    if (any(df$n_obligate < 0 | df$n_non_obligate < 0)) {
        stop("SAR richness must be non-negative.")
    }
    observed <- sar_slope_difference(df)
    if (!is.finite(observed)) stop("Insufficient data for the SAR slope comparison.")
    set.seed(seed)
    draws <- vapply(seq_len(B), function(i) {
        sampled <- df[sample.int(nrow(df), nrow(df), replace = TRUE), , drop = FALSE]
        sar_slope_difference(sampled)
    }, numeric(1))
    valid <- draws[is.finite(draws)]
    if (length(valid) < 0.95 * B) {
        stop("Fewer than 95% of paired SAR bootstrap fits are estimable.")
    }
    ci <- unname(stats::quantile(valid, c(0.025, 0.975)))
    list(delta_z = observed, ci = ci, draws = draws,
         n_valid = length(valid),
         # Descriptive bootstrap tail fraction, not a null-calibrated p-value.
         fraction_nonpositive = mean(valid <= 0))
}
