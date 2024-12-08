source("R/data-handler.R")

n <- 500 
loaded_data <- read_single_simulation_data(
  n = n, 
  R = 200, 
  is_time_varying = T, 
  i = 1, 
  eta_type = "linear-interaction",  
  baseline_type = "cosine"
)

single_data_origin <- loaded_data$data
hist(single_data_origin$T)

# Try a few C and see the w 
single_data_modified <- 
  single_data_origin %>%
  mutate(C = rexp(n=n, rate = 0.2),
         eventtime = single_data_origin$T) %>%
  mutate(U = pmin(eventtime, C), 
         Delta = as.numeric(eventtime <= C))
single_data_modified %>% 
  count(Delta)

{
  par(mfrow=c(1,2))
  hist(single_data_modified$T)
  hist(single_data_modified$U)
  par(mfrow=c(1,1))
}
