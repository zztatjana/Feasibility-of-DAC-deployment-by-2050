plotProbabilisticFeasibilitySpacePolicy <- function(data.row,
                                                     colour,
                                                     title,
                                                     ci.80 = F,  # 80% confidence int.
                                                     slices = 100,
                                                     random.paths = 10,
                                                     zoom.panel = F) { 
  
  # Unnest results
  data.plot <- data.row %>%
    unnest(sensitivities) %>%
    select(!demand) %>%
    unnest(results)
  
  # Calculate median and percentiles
  if (ci.80 == T) {
    quantiles <- c(0.95, 0.9, 0.5, 0.1, 0.05)
    col.quant <- c("0.95" = darken(colour, 0.1),
                   "0.9" = darken(colour, 0.3),
                   "0.5" = darken(colour, 0.5),
                   "0.1" = darken(colour, 0.3),
                   "0.05" = darken(colour, 0.1))
  } else {
    quantiles <- c(0.95, 0.5, 0.05)
    col.quant <- c("0.95" = darken(colour, 0.1),
                   "0.5" = darken(colour, 0.5),
                   "0.05" = darken(colour, 0.1))
  }
  
  quibble <- function(x, q = quantiles) {
    tibble(x = quantile(x, q), q = q)
  }
  
  data.stat <- data.plot %>%
    group_by(year) %>%
    reframe(quibble(forecast)) %>%  # Use reframe instead of summarise
    mutate(q = as.factor(q))
  
  # Get demand curves for all DAC targets
  data.plot.demand <- data.row %>%
    unnest(demand) %>%
    select(year, demand, dac_target_policy) %>%  
    group_by(year) %>%
    summarise(demand_mean = mean(demand), .groups = 'drop')
  
  # Select random paths
  data.plot.random <- data.plot %>%
    filter(sample %in% sample(1:max(data.plot$sample), random.paths))
  
  # Calculate density of scenarios for each year and save in raster
  data.raster <- NULL
  for (y in unique(data.plot$year)) {
    # Select
    subset <- data.plot %>%
      filter(year == y) %>%
      pull(forecast)
    # Density
    dens <- density(
      subset,
      n = slices,
      na.rm = T,
      from = 0,
      to = max(data.plot.demand$demand_mean, na.rm = T)
    )
    # Normalise
    dens$y <- dens$y / max(dens$y)
    temp <- tibble(year = y,
                   forecast = dens$x,
                   density = dens$y)
    # Save
    data.raster <- bind_rows(data.raster, temp)
  }
  
  # Plot using ggplot2::geom_tile instead of geom_raster
  p.facet <- ggplot() +
    # Density
    geom_tile(
      data = data.raster,
      mapping = aes(x = year, y = forecast, fill = density)
    ) +
    scale_fill_gradient(name = "Probability Density\n(normalised)", low = "white", high = colour) +
    # Random paths
    geom_line(
      data = data.plot.random,
      mapping = aes(
        x = year,
        y = forecast,
        group = sample,
        color = "Example Path"
      ),
      lwd = 0.5,
      alpha = 0.5
    ) +
    scale_color_manual(
      values = c("Example Path" = "grey"),
      name = NULL,
      guide = guide_legend(order = 3)
    ) +
    new_scale_color() +
    geom_line(
      data = data.stat,
      mapping = aes(x = year, y = x, color = q),
      size = 1
    ) +
    # Quantiles
    scale_color_manual(values = col.quant,
                       name = "Percentiles",
                       labels = c("0.95" = "95 %",
                                  "0.9" = "90 %",
                                  "0.5" = "50 % (Median)",
                                  "0.1" = "10 %",
                                  "0.05" = "5 %"),
                       guide = guide_legend(order = 4)) +
    new_scale_color() +
    xlab("year") +
    ylab(expression("DAC Capacity [GtCO"[2]*"]/a")) +
    scale_y_continuous(
      labels = scales::label_number(scale = 1e-3), 
      expand = expansion(mult = c(0.01, 0.5))
    ) +
    ggtitle(title) +
    theme(
      legend.position = c(0.1, 0.9),  
      legend.justification = c(0, 1),  
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA), 
      plot.title = element_text(face = "bold", size = font.size)
    )
  
  
  # Add optional zoom panel
  if (zoom.panel == T) {
    p.facet <- p.facet +
      facet_zoom(
        xlim = c(t.split, 2030),
        ylim = c(0, max(data.plot.demand$demand_mean)),
        horizontal = FALSE,
        zoom.size = 0.5,
        show.area = FALSE
      )
  }
  p.plot <- p.facet + theme(legend.position = c(0.1, 0.9),  
                            legend.justification = c(0, 1),  
                            legend.background = element_rect(fill = alpha("white", 0.7), color = NA))
  
  
  return(p.plot)
}
