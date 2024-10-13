library(here)
# source(here("old/cox-utils.R"))
source(here("old/R/report-results.R"))
### Time-varying
# Parameters
n <- 500
R <- 200
tau <- 1
data_folder <- 
  "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/time-varying-cox/"
plot_folder <- 
  "~/Dropbox (Harvard University)/Xiang_Iav/code/figures/time-varying-cox/"

# Call the reporting function

report_results(is_linear=T, 
               causal=F,
               n, R, data_folder, plot_folder)

report_results(is_linear=F, 
               causal=F,
               n, R, data_folder, plot_folder)
# 0.84 coverage for n=100, R = 100
# 0.865 coverage for n = 200, R = 200
# 0.505 coverage for n = 1000, R = 800
report_results(is_linear=F, 
               causal=T,
               n, R, data_folder, plot_folder)
# 0.89 coverage for n = 200, R = 200
# 0.925 coverage for n = 500, R = 200
# 0.86 coverage for n = 1000, R = 800

#### Non-time-varying
n <- 100
R <- 100
data_folder <- 
  "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/non-time-varying-cox/"
plot_folder <-
  "~/Dropbox (Harvard University)/Xiang_Iav/code/figures/non-time-varying-cox/"

report_results(is_linear=F, 
               causal=F,
               n, R, data_folder, plot_folder,
               light_censoring=T,
               outcome_type = "non-linear")

report_results(is_linear=F, 
               causal=T,
               n, R, data_folder, plot_folder,
               light_censoring=T,
               outcome_type = "non-linear")
# n = 100, R = 100: 1 min
# n = 250, R = 200: 4.5 mins


report_results(is_linear=F, 
               causal=F,
               n, R, data_folder, plot_folder)
report_results(is_linear=F, 
               causal=T,
               n, R, data_folder, plot_folder,
               light_censoring = light_censoring)


report_results(is_linear=T, 
               causal=F,
               n, R, data_folder, plot_folder)
report_results(is_linear=T, 
               causal=T,
               n, R, data_folder, plot_folder,
               light_censoring = light_censoring)




