plotDACProjects <- function(data.dac, regions, fill.by, title, leg.label) {
  data.plot <- data.dac %>%
    filter(region %in% names(regions)) %>%
    revalue.levels(region = regions) %>%
    order.levels(region = regions) %>%
    group_by(across(all_of(c("year", fill.by)))) %>%
    summarise(cumcap.sum = sum(cumcap.sum), .groups = "drop")
  
  # Split the data into two parts
  data.plot.1 <- data.plot %>%
    filter(year %in% seq(t.min, t.split))
  
  data.plot.2 <- data.plot %>%
    filter(year %in% seq(t.split, t.max))
  
  # Define colors based on 'fill.by'
  colors <- if (fill.by %in% c("region", "country")) {
    c("#4D7DBF", "#3395AB", "#818F42", "orange", "pink", "magenta", "#8C8C8C")
  } else {
    c("#4D7DBF", "#3395AB", "#818F42", "orange", "pink", "magenta", "#8C8C8C")
  }
  
  # Create plot for data.plot.1
  p1 <- ggplot(data = data.plot.1) +
    geom_bar(
      aes(x = year, y = cumcap.sum, fill = !!sym(fill.by)),
      stat = "identity",
      position = "stack",
      width = 0.9
    ) +
    scale_fill_manual(name = leg.label, values = colors) +
    scale_x_continuous(name = "Year", breaks = seq(t.min, t.split, 5)) +
    scale_y_continuous(
      trans = custom.trans(),
      labels = scales::number_format(accuracy = 1),
      limits = c(0, NA)
    ) +
    ylab(expression("DAC Capacity [MtCO"[2]*"]")) + 
    theme(legend.position = c(0.06, 0.73), plot.margin = unit(c(5, 0, 12, 5), "pt"))
  
  # Create plot for data.plot.2
  p2 <- ggplot(data = data.plot.2) +
    geom_bar(
      aes(x = year, y = cumcap.sum, fill = !!sym(fill.by)),
      stat = "identity",
      position = "stack"
    ) +
    scale_fill_manual(name = NULL, values = colors) +
    scale_x_continuous(name = "Year", breaks = seq(t.split, t.max, 5)) +
    scale_y_continuous(
      trans = custom.trans(),
      labels = scales::number_format(accuracy = 1),
      limits = c(0, NA)
    ) +
    ylab(expression("DAC Capacity [MtCO"[2]*"]")) +
    theme(legend.position = "none", plot.margin = unit(c(5, 5, 12, 0), "pt"))
  
  # Title for the plot
  p.title <- ggdraw() +
    draw_label(title, size = font.size, fontface = "bold", hjust = 1)
  
  # Combine p1 and p2 into a row
  p.row <- plot_grid(p1, p2, ncol = 2, rel_widths = c(1, 1))
  
  # Final combined plot
  p <- plot_grid(p.title, p.row, nrow = 2, rel_heights = c(0.05, 1))
  
  return(p)
}
