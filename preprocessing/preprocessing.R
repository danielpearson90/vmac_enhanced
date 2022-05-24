library(here)
library(tidyverse)
source(here("other", "summarySEwithin2.r"))


unprocessed_df <- read_csv(here("data", "raw_data", "vmac_sensorywincues_summary_anonymised.csv")) %>%
  select(-starts_with("..."))

did_not_complete_ps <- unprocessed_df %>%
  filter(totalTrials < 640) %>%
  pull(subNum)

completed_df <- unprocessed_df %>%
  filter(!subNum %in% did_not_complete_ps)

processed_df <- completed_df

many_exclusion_ps <- processed_df %>%
  filter(total_exclusions > 640*.15) %>%
  pull(subNum)

processed_df <- processed_df %>%
  filter(!subNum %in% many_exclusion_ps)

low_acc_ps <- processed_df %>%
  filter(accAfterExclusions < .6) %>%
  pull(subNum)

processed_df <- processed_df %>%
  rowwise() %>%
  mutate(mean_rt = mean(c(rt_enr, rt_high, rt_low, rt_abs))) %>%
  ungroup()

rt_cutoff <- processed_df %>%
  summarise(sd_rt = sd(mean_rt),
    overall_mean_rt = mean(mean_rt),
            cutoff = overall_mean_rt + 3.5*sd_rt)

slow_ps <- processed_df %>%
  filter(mean_rt > rt_cutoff$cutoff) %>%
  pull(subNum)

processed_df <- processed_df %>%
  filter(!subNum %in% low_acc_ps)

processed_df <- processed_df %>%
  filter(!subNum %in% slow_ps)



processed_df <- processed_df %>%
  mutate(err_enr = 1 - acc_enr,
         err_high = 1 - acc_high,
         err_low = 1 - acc_low,
         err_abs = 1 - acc_abs)

processed_df <- processed_df %>%
  mutate(vmac_standard = rt_high - rt_low,
         vmac_enriched = rt_enr - rt_low)

quality_summary <- processed_df %>%
  summarise(mean_acc = mean(accAfterExclusions),
            mean_exclusions = mean(total_exclusions)/640*100,
            mean_timeouts = mean(timeouts),
            prop_timeouts = mean_timeouts/640*100,
            mean_anticipations = mean(anticipations),
            prop_anticipation = mean_anticipations/640*100)

long_rt_df <- processed_df %>%
  select(subNum, rt_enr, rt_high, rt_low, rt_abs) %>%
  pivot_longer(cols = c(rt_enr, rt_high, rt_low, rt_abs),
               names_to = "trial_type",
               names_prefix = "rt_",
               values_to = "rt") %>%
  mutate(trial_type = factor(trial_type, levels = c("enr", "high", "low", "abs"),
                             labels = c("High-enriched", "High", "Low", "Absent")))

long_err_df <- processed_df %>%
  select(subNum, err_enr, err_high, err_low, err_abs) %>%
  pivot_longer(cols = c(err_enr, err_high, err_low, err_abs),
               names_to = "trial_type",
               names_prefix = "err_",
               values_to = "err") %>%
  mutate(trial_type = factor(trial_type, levels = c("enr", "high", "low", "abs"),
                             labels = c("High-enriched", "High", "Low", "Absent")))

