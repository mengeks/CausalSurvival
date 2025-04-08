# Load necessary libraries
library(survival)

# Step 1: Assign subject IDs
jasa$subject <- 1:nrow(jasa)
# Ensure that the 'accept.dt' is in Date format
jasa$accept.dt <- as.Date(jasa$accept.dt)
# Calculate the year variable as the difference from the study start date (October 1, 1967)
jasa$year <- as.numeric(jasa$accept.dt - as.Date("1967-10-01")) / 365.25
jasa$trt <- jasa$transplant

jasa <- jasa %>% mutate(
  futime = pmax(0.5, fu.date - accept.dt),  # Adjust follow-up time
  txtime = ifelse(tx.date == fu.date, 
                  (tx.date - accept.dt) - 0.5, 
                  (tx.date - accept.dt))
)

# Step 2: Adjust follow-up time and transplant time (as mentioned, time starts at 0.5 days if event happens at 0)
tdata <- with(jasa, data.frame(
  subject = subject,
  futime = pmax(0.5, fu.date - accept.dt),  # Adjust follow-up time
  txtime = ifelse(tx.date == fu.date, 
                  (tx.date - accept.dt) - 0.5, 
                  (tx.date - accept.dt)),  # Adjust transplant time
  fustat = fustat  # Death status
))

# Step 3: Create time-dependent covariate (transplant status)
sdata <- tmerge(jasa, tdata, id = subject, 
                death = event(futime, fustat),  # Time to death
                trt = tdc(txtime),  # Time-dependent transplant status
                options = list(idname = "subject"))

# Step 4: Adjust age and year to match K&P Table 6.1 definitions
sdata$age <- (sdata$age - 48) / 365.25  # Age adjustment in years from 48
sdata$year <- as.numeric(sdata$accept.dt - as.Date("1967-10-01")) / 365.25  # Years since study start

df_time_var <- sdata

df_original <- jasa
df_original <- left_join(df_original, df_time_var%>% distinct(subject, .keep_all = T) %>% select(subject,death),
                 by = "subject")

save(df_original, df_time_var, 
        file=here::here("scripts/data-application/data/stanford-RHC-processed.rds")
)
