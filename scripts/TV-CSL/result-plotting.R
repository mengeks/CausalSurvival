library(jsonlite)
library(here)
library(dplyr)
source("scripts/TV-CSL/result-process.R")
source("scripts/TV-CSL/time-varying-estimate.R")

## Figure 1
eta_type <- "non-linear"; HTE_type <- "linear"
json_file_TV_CSL <- "scripts/TV-CSL/params-TV-CSL.json"
json_file_lasso <- "scripts/TV-CSL/params-lasso.json"


MSE_csv_dir = "scripts/TV-CSL/tables-and-plots"
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
                      ifelse(grepl("linear", HTE_spec), "linear", HTE_spec)),
    prop_score_spec = 'NA'
  )

data_lasso_selected <- data_lasso %>%
  filter(HTE_spec == "linear") %>%
  select(Method, eta_spec, HTE_spec, n, prop_score_spec,MSE, MCSE_MSE, n_iterations) 

data_TV_CSL <- load_and_process_table_data_multi_spec(json_file=json_file_TV_CSL,
                                                      eta_type=eta_type,
                                                      HTE_type=HTE_type, 
                                                      MSE_csv_dir = MSE_csv_dir)
data_TV_CSL$method <- "TV-CSL"

data_TV_CSL_selected  <- data_TV_CSL %>% 
  filter(k == 0) %>%
  filter(stage == "final") %>%
  select(method, lasso_type, stage, eta_spec, HTE_spec, n, prop_score_spec,MSE, MCSE_MSE, n_iterations) 
data_TV_CSL_selected$Method <- paste(data_TV_CSL_selected$method, data_TV_CSL_selected$lasso_type, sep = "_")



combined_data <- bind_rows(
  data_lasso_selected %>% mutate(prop_score_spec = "cox-linear-censored-only"), 
  data_lasso_selected %>% mutate(prop_score_spec = "cox-linear-mis-specification"),
  data_TV_CSL_selected, .id = "source")


hte_labels <- c(
  "complex" = "Treatment effect: overly complex",
  "linear" = "Treatment effect: correctly specified"
)

eta_labels <- c(
  "complex" = "Baseline: Complex",
  "linear" = "Baseline: Linear"
)

prop_score_labels <- c(
  "cox-linear-censored-only" = "Prop score: correctly specified",
  "cox-linear-mis-specification" = "Prop score: mis-specified"
)

p <- ggplot(combined_data <- combined_data %>%
              filter(Method == "TV-CSL_S-lasso" | Method == "Lasso") %>%
              mutate(Method = ifelse(Method == "TV-CSL_S-lasso", "TV-CSL", 
                                     ifelse(Method == "Lasso", "S-Lasso", Method)) ), aes(x = as.factor(n), y = MSE, color = Method)) +
  geom_point() +
  geom_line(aes(group = interaction(Method, eta_spec, prop_score_spec))) +
  geom_errorbar(aes(ymin = MSE - 1.96 * MCSE_MSE, 
                    ymax = MSE + 1.96 * MCSE_MSE), 
                width = 0.2) +  # Width of the error bar caps
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

output_csv_dir <- "scripts/TV-CSL/tables-and-plots"
ggsave(file.path(output_csv_dir, "lasso-vs-TV-CSL.png" ), plot = p, width = 8, height = 6)
# ggsave(output_plot_path, plot = p, width = 8, height = 6)


p <- ggplot(combined_data %>% filter(Method == "TV-CSL_S-lasso_final" | Method == "TV-CSL_m-regression_final"), aes(x = as.factor(n), y = MSE, color = Method)) +
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
output_csv_dir <- "scripts/TV-CSL/tables-and-plots"
ggsave(file.path(output_csv_dir, "m-regression-vs-S-lasso.png" ), plot = p, width = 8, height = 6)




## Figure 2
source("scripts/TV-CSL/result-process.R")
make_HTE_by_eta_plots(json_file_lasso = "scripts/TV-CSL/params-lasso.json",
                      json_file_TV_CSL = "scripts/TV-CSL/results/result-TV-CSL-5-Nov-2024/params-TV-CSL.json", 
                      eta_type = "non-linear",
                      HTE_type = "linear",
                      output_csv_dir = "scripts/TV-CSL/tables-and-plots/5-Nov-2024")


