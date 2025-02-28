## Call simulation
calcTechnologyDiffusionAnticipation <- function(data.demand, data.input, delta.t){
  
  print("Starting simulation.")
  print("0 %")
  
  # Merge `data.demand` and `data.input` by region and sample
  data.full <- full_join(data.demand, data.input, by = c("region", "sample")) %>% 
    mutate(results = NA) %>% 
    arrange(match(region, c("Global", "North America", "Europe")))   # Global first
  
  
  # Loop over samples
  for (i in 1:dim(data.full)[1]){
    
    # Print every 10 %
    if (i %% (dim(data.full)[1]/10) == 0){
      print(paste0(100*i/dim(data.full)[1], " %"))
    }
    
    # Simulation
    data.temp <- data.full[i,] %>% 
      select(!results) %>% 
      unnest(demand) %>% 
      # Increase temporal resolution
      group_by(region, anticipation, sample, growth, start, dac_target_base) %>%  # Add dac_target_base here
      complete(year = seq(first(start.year), t.max, delta.t)) %>%
      mutate(demand = na.approx(demand, rule = 2),
             start.year = first(start.year)) %>% 
      # Adjust growth rate accordingly --> b_q = (1 + b)^(1/4) - 1 
      mutate(growth = (1 + growth)**(delta.t) - 1) %>% 
      # purrr::accumulate, demand is passed as second argument (.y) to function,
      # cumulative value as .x
      mutate(forecast = purrr::accumulate(demand,
                                          ~ .x + first(growth)*(1 - .x/.y)*.x, #Logistic model equation
                                          .init = first(start))[1:n()]) %>% 
      # Nest results
      nest(results = c(year, demand, forecast))
    
    # Copy nested data
    data.full$results[i] <- data.temp$results[1]
  }
  
  # Nest sensitivities
  data.results.anticipation <- data.full %>% 
    group_by(region, anticipation, demand, dac_target_base) %>%  # Include dac_target_base here
    nest() %>% 
    ungroup() %>% 
    rename(sensitivities = data)
  
  return(data.results.anticipation)
}
