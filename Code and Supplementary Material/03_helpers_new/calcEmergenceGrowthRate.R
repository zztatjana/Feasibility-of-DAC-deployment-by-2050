calcEmergenceGrowthRate <- function(data.technologies, regions = c("Global"), time.slice.length = 5:10) {
  
  exp_fit_technologies <- function(df) {
    nlsLM(value ~ a * (1 + b)^(year - min(year)),
          data = df,
          start = list(a = 500, b = 0.05),  # Initial values (adjustable)
          control = nls.lm.control(maxiter = 1000))  # Increased number of iterations
  }
  
  get_growthrate_technology <- function(mod) {
    coef(mod)[["b"]]
  }
  
  get_a_technology <- function(mod) {
    coef(mod)[["a"]]
  }
  
  get_predict_technology <- function(mod) {
    predict(mod)
  }
  
  get_year <- function(mod) {
    mod$m$getEnv()$year
  }
  
  # Empty DataFrame for results
  data.technologies.fit <- NULL
  
  # Loop over regions
  for (region in regions) {
    # Extract data for the region
    region_data <- data.technologies %>% filter(region == !!region)
    
    # Available years for this region
    time.span <- sort(unique(region_data$year))
    
    for (t in time.slice.length) {
      # Check if enough data is available for the time interval
      if (length(time.span) >= t) {
        for (s in 1:(length(time.span) - t + 1)) {
          slice <- time.span[s:(s + t - 1)]
          
          temp <- data.technologies %>%
            filter(year %in% slice) %>%
            group_by(technology, year) %>%
            summarise(value = sum(value), .groups = 'drop') %>%
            group_by(technology) %>%
            nest() %>%
            mutate(model.exp = map(data, exp_fit_technologies)) %>%
            transmute(technology,
                      b = map_dbl(model.exp, get_growthrate_technology),
                      a = map_dbl(model.exp, get_a_technology),
                      predict = map(model.exp, get_predict_technology),
                      year = map(model.exp, get_year),
                      slice.length = t,
                      slice.start = slice[1],
                      region = region)
          
          # Collect results for this region and technology
          data.technologies.fit <- bind_rows(data.technologies.fit, temp)
        }
      }
    }
  }
  
  # Remove negative growth rates
  data.technologies.fit <- data.technologies.fit %>%
    filter(b > 0)
  
  # Return the calculated growth rates
  return(data.technologies.fit)
}
