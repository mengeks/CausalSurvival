# BWH Heart Transplant Dataset Documentation

## Overview

This document describes the BWH (Brigham and Women's Hospital) heart transplant dataset, which contains information on patients listed for heart transplantation. The dataset includes information on listing dates, transplant outcomes, survival data, and patient characteristics.

**Note**: This dataset is for internal use between collaborators and is not publicly available.

## Dataset Structure

The dataset (`df_bwh`) contains 1,221 patient records with the following variables:

## Core Variables

### Identification and Time Period Variables

| Variable | Type | Description |
|----------|------|-------------|
| `subject` | Integer | Unique identifier for each patient (row number in the dataset) |
| `year` | Numeric | Time in years since the start of the study (calculated as `difftime(listing_date, min(listing_date), units = "days") / 365.25`) |

### Outcome Variables

| Variable | Type | Description |
|----------|------|-------------|
| `listing_date` | Date | Date when patient was added to the transplant registry |
| `time_to_event` | Numeric (months) | Time from listing to death or censoring (calculated as `difftime(coalesce(death_date, censor_date), listing_date, units = "days") / 30.44`) |
| `event` | Binary (0/1) | Indicates whether death occurred (1) or patient was censored (0) |

### Treatment Variables

| Variable | Type | Description |
|----------|------|-------------|
| `transplant` | Logical (TRUE/FALSE) | Indicates whether patient received a heart transplant |
| `time_to_tx` | Numeric (months) | Time from listing to transplant (calculated as `difftime(transplant_date, listing_date, units = "days") / 30.44`) |
| `transplant_date` | Date | Date when transplant was performed (NA if no transplant) |

### Patient Characteristics

| Variable | Type | Description |
|----------|------|-------------|
| `age` | Numeric | Age in years at time of listing |
| `gender` | Character | Patient's birth sex (lowercase) |
| `race` | Character | Patient's ethnicity (lowercase) |
| `blood_type` | Character | Blood type (uppercase) |
| `status_listing` | Character | Wait list medical urgency status at time of transplant offer/removal |
| `diagnosis` | Character | Primary diagnosis at time of listing (lowercase) |
| `height_cm` | Numeric | Height in centimeters at listing |
| `weight_kg` | Numeric | Weight in kilograms at listing |
| `bmi` | Numeric | Body Mass Index at listing |

### Life Support and Medical Interventions

| Variable | Type | Description |
|----------|------|-------------|
| `ventilation` | Logical | Whether patient was on ventilator support at listing |
| `ecmo` | Logical | Whether patient was on ECMO (Extracorporeal Membrane Oxygenation) at listing |
| `iabp` | Logical | Whether patient was on IABP (Intra-Aortic Balloon Pump) at listing |
| `inotropes` | Logical | Whether patient was on IV inotropes at listing |
| `support_device` | Logical | Composite variable indicating if patient was on any support device (ECMO, IABP, or inotropes) |

### Additional Dates

| Variable | Type | Description |
|----------|------|-------------|
| `death_date` | Date | Date of death (NA if patient did not die during follow-up) |
| `censor_date` | Date | Date when patient was removed from the registry for reasons other than transplant or death |

## Modeling Considerations

### Variables for Survival Outcome Modeling:
- `age`, `gender`, `race`, `blood_type`, `status_listing`, `diagnosis`, `bmi`, `ventilation`, `support_device`

### Variables for Donor Heart Waiting Time Modeling:
- `blood_type`, `status_listing`, `diagnosis`, `age`, `insurance`, `support_device`, `ventilation`

## Data Processing Notes

### Outcome Variables
- `listing_date`: Date when the patient was added to the registry. Non-NA values are required for inclusion in the dataset.
- `death_date`: Date of death. If NA, the patient is considered censored (lost to follow-up or still alive).
- `censor_date`: Date when a patient was taken off the registry. This serves as a proxy for the last known contact with the patient. For censored data, this allows for inclusion in model fitting. If a death date exists, the censor date should be earlier than the death date.
- `time_to_event`: Calculated in months (days/30.44) from listing to either death or censoring.

### Treatment Variables
- `transplant_date`: Date of heart transplant. Can be NA for various reasons:
  - Patient died before transplant
  - Patient's condition deteriorated (too sick for transplant)
  - Patient's condition improved (transplant no longer needed)
  - Patient still on waiting list
  - Administrative reasons (transferred to another center, etc.)
  - Patient refused transplant
- `transplant`: Binary variable (TRUE/FALSE) derived from `transplant_date` (!is.na(transplant_date))
- `time_to_tx`: Time in months from listing to transplant

## Data Transformation Details

The dataset was derived from the raw data (`bwh-listing.xlsx`) with the following transformations:

```r
df <- df_raw %>%
  mutate(listing_date = as.Date(`Listing Date`)) %>%
  mutate(age = as.numeric(`Age in Years at Time of Listing`)) %>%
  mutate(transplant_date = as.Date(`Transplant Date`)) %>%
  mutate(death_date = as.Date(`Date of Death`)) %>%
  mutate(censor_date = as.Date(`Removal Date of Registration`)) %>%
  mutate(transplant = !is.na(transplant_date)) %>%
  mutate(
    time_to_event = as.numeric(difftime(
      coalesce(death_date, censor_date),
      listing_date,
      units = "days"
    )) / 30.44
  ) %>%
  mutate(
    event = ifelse(!is.na(death_date), 1, 0),
    time_to_tx = as.numeric(difftime(
      transplant_date,
      listing_date,
      units = "days"
    )) / 30.44
  ) %>%
  # Additional transformations...
  # Add unique subject ID and year since study start
  mutate(
    subject = row_number(),
    year = as.numeric(difftime(listing_date, min(listing_date, na.rm = TRUE), units = "days")) / 365.25
  ) %>%
  # Select all variables including new ones
  select(
    subject,
    listing_date,
    year,
    # Other variables...
  )
```

### Notes:
- Time variables are converted from days to months by dividing by 30.44 (average days per month)
- Text variables are standardized (lowercase or uppercase as appropriate)
- Support device variables are derived from text fields using regex pattern matching

## Requirements for Time-Varying Causal Survival Analysis

For use with the time-varying causal survival R package, data should:

1. Have complete information for required fields:
   - `listing_date` (non-NA)
   - `time_to_event` (calculated from death_date or censor_date)
   - `event` (binary indicator of death)
   - `time_to_tx` (for those who received transplant)
   - `transplant` (binary indicator of transplant)

2. Follow the time structure:
   - Time variables should be in consistent units (months in this dataset)
   - `time_to_tx` must be less than or equal to `time_to_event` for patients who received transplants

3. Have sufficient covariate information for the chosen model

## Pre-processing Guide

If your dataset doesn't match this structure, you'll need to:

1. Convert all date fields to Date format
2. Calculate time variables in a consistent unit (preferably months)
3. Create binary indicators for treatment and event
4. Handle missing data appropriately:
   - Patients with missing listing dates should be excluded
   - Missing death dates should be interpreted as censored
   - Missing treatment dates should be interpreted as no treatment

## Example Data Validation

```r
# Check for required non-NA values
validate_required <- function(df) {
  issues <- list()
  if(any(is.na(df$listing_date))) 
    issues <- c(issues, "Missing listing_date values")
  if(any(is.na(df$time_to_event))) 
    issues <- c(issues, "Missing time_to_event values")
  if(any(is.na(df$event))) 
    issues <- c(issues, "Missing event indicator values")
  if(any(is.na(df$transplant))) 
    issues <- c(issues, "Missing transplant indicator values")
  if(any(!is.na(df$transplant_date) & is.na(df$time_to_tx))) 
    issues <- c(issues, "Missing time_to_tx for transplanted patients")
  
  if(length(issues) == 0) {
    return(TRUE)
  } else {
    return(issues)
  }
}

# Check time relationships
validate_times <- function(df) {
  issues <- list()
  
  # For transplanted patients, time_to_tx should be <= time_to_event
  transplanted <- df$transplant == TRUE
  if(any(transplanted)) {
    if(any(df$time_to_tx[transplanted] > df$time_to_event[transplanted], na.rm = TRUE))
      issues <- c(issues, "Some patients have time_to_tx > time_to_event")
  }
  
  if(length(issues) == 0) {
    return(TRUE)
  } else {
    return(issues)
  }
}
```