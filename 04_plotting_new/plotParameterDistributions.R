# Function to plot growth rate and confidence interval for different technologies
plotParameterDistributions <- function(data.input, data.input.param, tech_to_plot = "Wind") {
  bins <- 20
  plots <- list()
  
  # Define available technologies
  technologies <- c("Wind", "LNG", "Ammonia Synthesis")
  
  # Define max growth rate per technology for x-axis scaling
  growth_max_values <- list(
    "Wind" = 0.40,               # Adjust as needed
    "LNG" = 0.08,                # Custom max growth rate for LNG
    "Ammonia Synthesis" = 0.20   # Custom max growth rate for Ammonia Synthesis
  )
  
  # Define x-axis break intervals per technology
  growth_breaks_values <- list(
    "Wind" = c(100 * bmin, seq(5, 150, 5)),  # Breaks for Wind 
    "LNG" = c(100 * bmin, seq(1, 150, 1)),   # Breaks for LNG 
    "Ammonia Synthesis" = c(100 * bmin, seq(5, 150, 5))  # Breaks for Ammonia Synthesis 
  )
  
  for (r in 1:3) { # Loop over each region
    for (tech in technologies) { # Loop over each technology
      # Retrieve values
      start.min <- data.input.param$start.min[[r]]
      start.max <- 1.2 * data.input.param$start.max[[r]]
      growth.min <- data.input.param$growth.min[[r]]
      growth.max <- growth_max_values[[tech]]  # Use custom max growth rate
      growth.breaks <- growth_breaks_values[[tech]]  # Use custom breaks
      
      # Generate density distribution
      data.plot.density <- tibble(
        start = seq(start.min, start.max, length.out = 100),
        start.dens = dtruncnorm(start, a = start.min, mean = data.input.param$start.mean[[r]], sd = data.input.param$start.sd[[r]]),
        growth = seq(growth.min, growth.max, length.out = 100),
        growth.dens = dtruncnorm(growth, a = growth.min, mean = data.input.param$growth.mean[[r]], sd = data.input.param$growth.sd[[r]])
      )
      
      # Filter data for the selected region
      data.plot.sample <- data.input %>%
        filter(region == regions.dac$name[[r]])
      
      # Generate the growth rate plot
      p.growth <- ggplot() +
        geom_line(data = data.plot.density, mapping = aes(x = 100 * growth, y = growth.dens, color = "Density")) +
        scale_color_manual(name = NULL, values = c("Density" = "black")) +
        scale_x_continuous(name = "Emergence Growth Rate [%/yr]", 
                           limits = c(0, 100 * growth.max), 
                           breaks = growth.breaks) +  # Use custom breaks
        ylab("Probability Density [yr]") +
        ggtitle(paste0("Global ", tech, " Growth")) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0, face = "bold", size = font.size)
        )
      
      # Add vertical lines for growth rate values
      growth_values <- switch(
        tech,
        "Ammonia Synthesis" = data.input.param[r, ] %>% unnest(growth.technology) %>% pull(growth.technology),
        "LNG" = data.input.param[r, ] %>% unnest(growth.technology) %>% pull(growth.technology),
        "Wind" = data.input.param[r, ] %>% unnest(growth.technology) %>% pull(growth.technology)
      )
      
      for (j in seq_along(growth_values)) {
        p.growth <- p.growth +
          geom_vline(
            xintercept = 100 * growth_values[j],
            linetype = "dotted",
            color = switch(tech,
                           "Ammonia Synthesis" = "#E64B35FF",
                           "LNG" = "#51A492",
                           "Wind" = "blue")
          )
      }
      
      # Store plots only for the selected technology
      if (tech == tech_to_plot) {
        plots <- append(plots, list(p.growth))
      }
    }
  }
  return(plots)
}
