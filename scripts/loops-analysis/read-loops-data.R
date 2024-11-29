library(arrow)
library(here)
df_path <- here("scripts/heart-transplant-analysis/data/Loops_2022_Q1.parquet")
df <- read_parquet(df_path)

