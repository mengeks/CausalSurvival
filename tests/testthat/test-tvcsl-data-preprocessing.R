library(testthat)
library(dplyr)
source(here::here("R/tvcsl-data-preprocessing.R"))


test_that("Function handles basic case with no treatment", {
  # Test 1: Subject with no treatment (censored)
  test_data <- data.frame(
    id = 1,
    time_to_event = 5,
    event = 0,
    time_to_tx = NA,
    age = 65,
    surgery = 1
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "surgery")
  )
  
  # Should return one row with trt=0
  expect_equal(nrow(result), 1)
  expect_equal(result$tstart, 0)
  expect_equal(result$tstop, 5)
  expect_equal(result$status, 0)
  expect_equal(result$trt, 0)
  expect_equal(result$age, 65)
  expect_equal(result$surgery, 1)
})

test_that("Function handles basic case with treatment after event", {
  # Test 2: Subject with treatment time after event time (treated as no treatment)
  test_data <- data.frame(
    id = 1,
    time_to_event = 5,
    event = 1,
    time_to_tx = 10,
    age = 65,
    surgery = 1
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "surgery")
  )
  
  # Should return one row with trt=0
  expect_equal(nrow(result), 1)
  expect_equal(result$tstart, 0)
  expect_equal(result$tstop, 5)
  expect_equal(result$status, 1)
  expect_equal(result$trt, 0)
})

test_that("Function handles time-varying treatment correctly", {
  # Test 3: Subject with treatment before event
  test_data <- data.frame(
    id = 1,
    time_to_event = 10,
    event = 1,
    time_to_tx = 5,
    age = 65,
    surgery = 1
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "surgery")
  )
  
  # Should return two rows: one before treatment and one after
  expect_equal(nrow(result), 2)
  
  # First interval: from 0 to treatment
  expect_equal(result$tstart[1], 0)
  expect_equal(result$tstop[1], 5)
  expect_equal(result$status[1], 0)  # No event in this interval
  expect_equal(result$trt[1], 0)     # Not treated yet
  
  # Second interval: from treatment to event
  expect_equal(result$tstart[2], 5)
  expect_equal(result$tstop[2], 10)
  expect_equal(result$status[2], 1)  # Event in this interval
  expect_equal(result$trt[2], 1)     # Treated in this interval
  
  # Covariates should be the same in both rows
  expect_equal(result$age[1], result$age[2])
  expect_equal(result$surgery[1], result$surgery[2])
})

test_that("Function handles multiple subjects correctly", {
  # Test 4: Multiple subjects with different patterns
  test_data <- data.frame(
    id = 1:3,
    time_to_event = c(10, 5, 7),
    event = c(1, 0, 1),
    time_to_tx = c(5, NA, 10),
    age = c(65, 70, 55),
    surgery = c(1, 0, 1)
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "surgery")
  )
  
  # Subject 1 should have 2 rows (treated before event)
  # Subject 2 should have 1 row (no treatment)
  # Subject 3 should have 1 row (treatment after event)
  expect_equal(nrow(result), 4)
  
  # Check each subject's pattern
  subject1 <- result[result$id == 1, ]
  expect_equal(nrow(subject1), 2)
  expect_equal(subject1$trt, c(0, 1))
  
  subject2 <- result[result$id == 2, ]
  expect_equal(nrow(subject2), 1)
  expect_equal(subject2$trt, 0)
  
  subject3 <- result[result$id == 3, ]
  expect_equal(nrow(subject3), 1)
  expect_equal(subject3$trt, 0)
})

test_that("Function handles treatment at exact event time", {
  # Test 5: Treatment occurs at the same time as event
  test_data <- data.frame(
    id = 1,
    time_to_event = 5,
    event = 1,
    time_to_tx = 5,
    age = 65,
    surgery = 1
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "surgery")
  )
  
  # Should return one row with trt=0 (borderline case)
  expect_equal(nrow(result), 1)
  expect_equal(result$trt, 0)
})

test_that("Function uses all available covariates when covariates=NULL", {
  # Test 6: No covariates specified, should use all available
  test_data <- data.frame(
    id = 1,
    time_to_event = 10,
    event = 1,
    time_to_tx = 5,
    age = 65,
    surgery = 1,
    year = 2020,
    extra_var = "test"
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx"
  )
  
  # Should include all covariates
  expect_true("age" %in% names(result))
  expect_true("surgery" %in% names(result))
  expect_true("year" %in% names(result))
  expect_true("extra_var" %in% names(result))
})

test_that("Function handles custom id column", {
  # Test 7: Custom ID column
  test_data <- data.frame(
    patient_id = 1,
    time_to_event = 10,
    event = 1,
    time_to_tx = 5,
    age = 65
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = "age",
    id_col = "patient_id"
  )
  
  # Should use patient_id as identifier
  expect_true("patient_id" %in% names(result))
  expect_false("id" %in% names(result))
})

test_that("Function handles invalid inputs", {
  # source(here::here("R/tvcsl-data-preprocessing.R"))
  # Test 8: Event time variable exists but is NA
  test_data <- data.frame(
    id = 1,
    time_to_event = NA,
    event = 1,
    time_to_tx = 5,
    age = 65
  )

  # This should issue a warning rather than error
  expect_warning(
    result <- create_time_varying_dataset(
      data = test_data,
      event_time = "time_to_event",
      event = "event",
      tx_time = "time_to_tx",
      covariates = "age"
    ),
    "Row 1 skipped: missing event time"
  )

  # Test 9: Non-existent column
  test_data <- data.frame(
    id = 1,
    time_to_event = 10,
    event = 1,
    time_to_tx = 5,
    age = 65
  )

  expect_error(create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "nonexistent_column",
    covariates = "age"
  ))
})

test_that("Function eliminates zero-length intervals", {
  # Test 10: Would result in zero-length interval
  test_data <- data.frame(
    id = 1,
    time_to_event = 5,
    event = 1,
    time_to_tx = 5,  # Same as event time
    age = 65
  )
  
  result <- create_time_varying_dataset(
    data = test_data,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = "age"
  )
  
  # Should not have any rows where tstart == tstop
  expect_true(all(result$tstop > result$tstart))
})
