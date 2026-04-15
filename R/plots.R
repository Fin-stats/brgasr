plot.brgas_fit <- function(x,
                           dates = NULL,
                           returns = NULL,
                           break_date = NULL,
                           return_col = "gray35",
                           scale_col = "navy",
                           break_col = "firebrick",
                           ...) {
  if (is.null(returns)) {
    returns <- x$y
  }
  if (is.null(dates)) {
    dates <- seq_along(returns)
  }
  if (length(dates) != length(returns)) {
    stop("`dates` and `returns` must have the same length.", call. = FALSE)
  }
  scale_path <- x$fitted$scale_path
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

  graphics::plot(
    x = dates,
    y = returns,
    type = "h",
    col = return_col,
    main = "Returns",
    xlab = "",
    ylab = "Return",
    xaxt = "n",
    ...
  )
  axis_dates <- pretty(dates)
  graphics::axis(1, at = axis_dates, labels = format(axis_dates, "%Y-%m"))
  if (!is.null(break_date)) {
    graphics::abline(v = break_date, col = break_col, lty = 2, lwd = 2)
  }

  graphics::plot(
    x = dates,
    y = scale_path,
    type = "l",
    col = scale_col,
    lwd = 2,
    main = "Fitted Conditional Scale",
    xlab = "",
    ylab = "Scale",
    xaxt = "n",
    ...
  )
  graphics::axis(1, at = axis_dates, labels = format(axis_dates, "%Y-%m"))
  if (!is.null(break_date)) {
    graphics::abline(v = break_date, col = break_col, lty = 2, lwd = 2)
  }

  invisible(x)
}

plot_var_es_forecasts <- function(forecasts,
                                  tau,
                                  model_order = NULL,
                                  palette = NULL,
                                  realized_col = "black",
                                  ...) {
  required <- c("date", "tau", "realized", "var")
  missing_cols <- setdiff(required, names(forecasts))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!"model" %in% names(forecasts)) {
    forecasts$model <- "br-gas-t"
  }
  df <- forecasts[forecasts$tau == tau, , drop = FALSE]
  if (nrow(df) == 0) {
    stop("No forecast rows found for the requested `tau`.", call. = FALSE)
  }
  df$date <- as.Date(df$date)
  models <- unique(df$model)
  if (is.null(model_order)) {
    model_order <- models
  }
  model_order <- model_order[model_order %in% models]
  if (length(model_order) == 0) {
    stop("`model_order` does not match any models in `forecasts`.", call. = FALSE)
  }
  if (is.null(palette)) {
    palette <- grDevices::hcl.colors(length(model_order), palette = "Dark 3")
  }

  y_rng <- range(c(df$realized, df$var), finite = TRUE)
  graphics::plot(
    x = df$date[df$model == model_order[1]],
    y = df$realized[df$model == model_order[1]],
    type = "h",
    col = realized_col,
    ylim = y_rng,
    main = sprintf("One-Step-Ahead VaR Forecasts (tau = %.3f)", tau),
    xlab = "",
    ylab = "Return / VaR",
    xaxt = "n",
    ...
  )
  axis_dates <- pretty(df$date)
  graphics::axis(1, at = axis_dates, labels = format(axis_dates, "%Y-%m"))

  for (i in seq_along(model_order)) {
    sub_df <- df[df$model == model_order[i], , drop = FALSE]
    sub_df <- sub_df[order(sub_df$date), , drop = FALSE]
    graphics::lines(sub_df$date, sub_df$var, col = palette[i], lwd = 2)
  }

  graphics::legend(
    "topleft",
    legend = c("realized", model_order),
    col = c(realized_col, palette),
    lty = c(1, rep(1, length(model_order))),
    lwd = c(1, rep(2, length(model_order))),
    bty = "n"
  )

  invisible(df)
}
