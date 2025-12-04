plotParameterDistributionsCAP <- function(data.input, data.input.param) {
  bins  <- 60
  plots <- list()
  
  for (r in 1:3) {
    region.name <- regions.dac$name[[r]]
    
    # Parameter
    start.min  <- data.input.param$start.min[[r]]
    start.mean <- data.input.param$start.mean[[r]]
    start.sd   <- data.input.param$start.sd[[r]]
    x.lim      <- x.limits[[region.name]]
    
    #Density only in the visible range (from truncation limit)
    eps    <- 1e-6
    x.from <- max(x.lim[1], start.min + eps)
    n_pts  <- max(300, ceiling(40 * (x.lim[2] - x.from)))
    
    data.plot.density <- tibble(
      start      = seq(x.from, x.lim[2], length.out = n_pts),
      start.dens = dtruncnorm(start, a = start.min, mean = start.mean, sd = start.sd)
    )
    
    # Histogram-Samples
    data.plot.sample <- data.input %>% filter(region == region.name)
    
    # (A) Start distribution: Histogram + Density
    p.start.dist <- ggplot() +
      geom_histogram(
        data = data.plot.sample,
        aes(x = start, y = ..density..),
        breaks = seq(x.lim[1], x.lim[2], length.out = bins + 1),
        fill = "grey70",
        color = "white",
        size = 0.1,
        alpha = 0.8
      ) +
      geom_line(
        data = data.plot.density,
        aes(x = start, y = start.dens),
        color = "black",
        linewidth = 0.7
      ) +
      coord_cartesian(xlim = x.lim) +
      xlab(NULL) +
      ylab(expression("Probability Density [1/MtCO"[2]*"]")) +
      ggtitle(region.name) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin  = unit(c(0,0,0,0), "cm"),
        plot.title   = element_text(hjust = 0.5, vjust = -1.5, face = "bold", size = font.size)
      )
    
    plots <- append(plots, list(p.start.dist))
    
    # (B) Status bar: Robust data processing
    data.plot.cap <- data.dac %>%
      filter(region %in% regions.dac$list[[r]], year == t.split) %>%
      group_by(status) %>%
      summarise(cumcap.sum = sum(cumcap.sum), .groups = "drop") %>%
      mutate(status = factor(status, levels = status_levels)) %>%
      tidyr::complete(status, fill = list(cumcap.sum = 0)) %>%
      filter(status != "Decommissioned") %>%      # optional ausblenden
      arrange(status)
    
    # (C) Draw status bars (no hard ylim sections): 
    p.start.cap <- ggplot() +
      geom_bar(
        data = data.plot.cap,
        aes(x = 1, y = cumcap.sum, fill = status),
        stat = "identity", width = 0.9
      ) +
      scale_fill_manual(name = "Status", values = status_cols, drop = FALSE) +
      coord_flip(ylim = c(0, x.lim[2])) +   # nur Ansicht beschneiden
      xlab(NULL) +
      ylab(paste0("Capacity in ", t.split, " [MtCO2]")) +
      theme(
        plot.margin = unit(c(-0.25,0,0,0), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    plots <- append(plots, list(p.start.cap))
  }
  
  return(plots) 
}