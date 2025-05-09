library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
# library(text)

df_raw <- read_xlsx("data/bwh-heart-transplant/bwh-listing.xlsx")
# Rule 1: ransplant Date is non-NA only when reason is 'Deceased Donor tx, removed by transplanting center'
# Verification: Check if there are any violations of the rule

violations <- df_raw[!is.na(df_raw$TransplantDate) & df_raw$ReasonForRemoval != "Deceased Donor tx, removed by transplanting center", ]

if (nrow(violations) == 0) {
  message("✅ Rule holds: Transplant Date is non-NA only when reason is 'Deceased Donor tx, removed by transplanting center'.")
} else {
  print("❌ Violations found:")
  print(violations)
}
