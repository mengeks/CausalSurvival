library(jsonlite)
library(here)
library(dplyr)
source("scripts/TV-CSL/result-process.R")
source("scripts/TV-CSL/time-varying-estimate.R")

eta_type <- "non-linear"; HTE_type <- "linear"


## Gather table for TV-CSL
source("scripts/TV-CSL/result-process.R")
json_file_TV_CSL <- "scripts/TV-CSL/params-TV-CSL.json"
results <- evaluate_HTE_metrics(
  json_file = json_file_TV_CSL,
  results_dir = "scripts/TV-CSL/results/",
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
  HTE_spec = "linear"
)

json_file_lasso <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  json_file = json_file_lasso,
  eta_type = eta_type,
  HTE_type = HTE_type,
)
source("scripts/TV-CSL/result-process.R")



# make_prop_score_by_eta_plots <- function(json_file_lasso,
#                                          json_file_TV_CSL, 
#                                          eta_type,
#                                          HTE_type,
#                                          MSE_csv_dir = "scripts/TV-CSL/tables-and-plots",
# ) {
  data_lasso <- load_and_process_table_data_single_spec(json_file_lasso,eta_type,HTE_type, MSE_csv_dir)
  data_lasso <- data_lasso %>%
    rename(
      eta_spec = eta_model,
      HTE_spec = HTE_model
    )
  
  data_lasso <- data_lasso %>%
    mutate(
      eta_spec = ifelse(grepl("complex", eta_spec), "complex", 
                        ifelse(grepl("linear", eta_spec), "linear", eta_spec)),
      HTE_spec = ifelse(grepl("complex", HTE_spec), "complex", 
                        ifelse(grepl("linear", HTE_spec), "linear", HTE_spec))
    )
  
  data_lasso_selected <- data_lasso %>%
    filter(HTE_spec == "linear") %>%
    select(Method, eta_spec, HTE_spec, n, prop_score_spec,MSE) 
  
  data_TV_CSL <- load_and_process_table_data_multi_spec(json_file=json_file_TV_CSL,
                                             eta_type=eta_type,
                                             HTE_type=HTE_type, 
                                             MSE_csv_dir = MSE_csv_dir)
  data_TV_CSL$method <- "TV-CSL"
  
  data_TV_CSL$Method <- paste(data_TV_CSL$method, data_TV_CSL$lasso_type, data_TV_CSL$stage, sep = "_")
  data_TV_CSL_selected  <- data_TV_CSL %>% 
    filter(k == 0) %>%
    select(Method, eta_spec, HTE_spec, n, prop_score_spec,MSE) 
  
  
  
  
  combined_data <- bind_rows(
    data_lasso_selected %>% mutate(prop_score_spec = "cox-linear-censored-only"), 
    data_lasso_selected %>% mutate(prop_score_spec = "cox-linear-mis-specification"),
    data_TV_CSL_selected, .id = "source")
  
  
  hte_labels <- c(
    "complex" = "Treatment effect: overly complex",
    "linear" = "Treatment effect: correctly specified"
  )
  
  eta_labels <- c(
    "complex" = "Baseline: Mildly mis-specified",
    "linear" = "Baseline: Quite mis-specified"
  )
  
  prop_score_labels <- c(
    "cox-linear-censored-only" = "Prop score: correctly specified",
    "cox-linear-mis-specification" = "Prop score: mis-specified"
  )
  
  p <- ggplot(combined_data, aes(x = as.factor(n), y = MSE, color = Method)) +
    geom_point() +
    geom_line(aes(group = interaction(Method, eta_spec, prop_score_spec))) +
    facet_grid(
      prop_score_spec ~ eta_spec,
      labeller = labeller(
        eta_spec = eta_labels,
        prop_score_spec = prop_score_labels
      )
    ) + 
    labs(x = "n", y = "MSE", title = "MSE by Method and Model Specifications") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          strip.text = element_text(face = "bold"))
  p
  output_plot_path <- 
    file.path(output_csv_dir, 
              paste0("both-methods_", 
                     paste0(eta_type, "_", HTE_type), 
                     "_MSE_plots.png") )
  ggsave(output_plot_path, plot = p, width = 8, height = 6)
  
  return(output_plot_path)
}








source("scripts/TV-CSL/result-process.R")
make_HTE_by_eta_plots(json_file_lasso = json_file_lasso,
                     json_file_TV_CSL = json_file_TV_CSL, 
                     eta_type = eta_type,
                     HTE_type = HTE_type,
                     output_csv_dir = "scripts/TV-CSL/tables-and-plots")





process_results_to_csv(
  json_file =  json_file_TV_CSL,
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
)



source("scripts/TV-CSL/result-process.R")
make_plots_from_json(json_file_lasso = json_file_lasso,
                     json_file_TV_CSL = json_file_TV_CSL, 
                     eta_type = eta_type,
                     HTE_type = HTE_type,
                     output_csv_dir = "scripts/TV-CSL/tables-and-plots")

# source("scripts/TV-CSL/result-process.R")
# make_beta_HTEs_table(
#   json_file = json_file
# ) 

# json_file_TV_CSL <- "scripts/TV-CSL/params-lasso.json"
# process_results_to_csv(
#   "scripts/TV-CSL/params-TV-CSL.json"
# )
