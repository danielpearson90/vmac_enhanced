library(here)
library(afex)
library(emmeans)

source(here("preprocessing", "preprocessing.R"))
source(here("other", "dz_calculator.R"))


# DEMOGRAPHICS ------------------------------------------------------------

# gender_count <- completed_df %>%
#   count(gender)

# age_summary <- completed_df %>%
#   summarize(mean_age = mean(age),
#             sd_age = sd(age))

# RT ANALYSIS -------------------------------------------------------------
# rts pass the normality tests
shapiro.test(long_rt_df %>% filter(trial_type == "High-enriched") %>% pull(rt))
shapiro.test(long_rt_df %>% filter(trial_type == "High") %>% pull(rt))
shapiro.test(long_rt_df %>% filter(trial_type == "Low") %>% pull(rt))
shapiro.test(long_rt_df %>% filter(trial_type == "Absent") %>% pull(rt))
# Omnibus ANOVA of RTs
aov_omni_rts <- aov_ez(id = "subNum",
                       dv = "rt",
                       within = "trial_type",
                       data = long_rt_df,
                       anova_table = list(es = "pes"))

enriched_absent_es <- dz_calculator(processed_df$rt_enr, processed_df$rt_abs)
high_absent_es <- dz_calculator(processed_df$rt_high, processed_df$rt_abs)
low_absent_es <- dz_calculator(processed_df$rt_low, processed_df$rt_abs)
high_low_es <- dz_calculator(processed_df$rt_low, processed_df$rt_high)
enriched_low_es <- dz_calculator(processed_df$rt_low, processed_df$rt_enr)

# vmac scores for enriched and high
vmac_standard <- t.test(processed_df$vmac_standard)
vmac_enriched <- t.test(processed_df$vmac_enriched)
vmac_contrast <- t.test(processed_df$vmac_standard, processed_df$vmac_enriched, paired = T)

vmac_contrast_es <- dz_calculator(processed_df$vmac_standard, processed_df$vmac_enriched)

# ERROR ANALYSIS -------------------------------------------------------

aov_omni_err <- aov_ez(id = "subNum",
                       dv = "err",
                       within = "trial_type",
                       data = long_err_df,
                       anova_table = list(es = "pes"))
long_err_df <- long_err_df %>%
  mutate(subNum_fct = factor(subNum))

bf <- BayesFactor::anovaBF(err ~ trial_type + subNum_fct, data = long_err_df,
                           whichRandom = "subNum_fct")

# CHOICE TASK -------------------------------------------------------------

processed_df2 <- processed_df %>%
  mutate(chce_enr_high2 = case_when(chce_enr_high == 0 ~ -1,
                                    chce_enr_high == .5 ~ 0,
                                    TRUE ~ chce_enr_high),
         chce_enr_low2 = case_when(chce_enr_low == 0 ~ -1,
                                    chce_enr_low == .5 ~ 0,
                                    TRUE ~ chce_enr_low),
         chce_high_low2 = case_when(chce_high_low == 0 ~ -1,
                                    chce_high_low == .5 ~ 0,
                                    TRUE ~ chce_high_low))


qplot(processed_df2$chce_enr_high2)
shapiro.test(processed_df2$chce_enr_high2)
qplot(processed_df2$chce_enr_low2)
shapiro.test(processed_df2$chce_enr_low2)
qplot(processed_df2$chce_high_low2)
shapiro.test(processed_df2$chce_high_low2)



wilcox_chce_enr_high <- wilcox.test(x = processed_df2$chce_enr_high2, mu = 0)
z_chce_enr_high <- qnorm(wilcox_chce_enr_high$p.value/2)

diff <- processed_df2$chce_enr_high2 - 0
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])

wilcox_chce_enr_low <- wilcox.test(processed_df2$chce_enr_low2, mu = 0)
z_chce_enr_low <- qnorm(wilcox_chce_enr_low$p.value/2)

diff <- processed_df2$chce_enr_low2 - 0
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])

wilcox_chce_high_low <- wilcox.test(processed_df2$chce_high_low2, mu = 0)
z_chce_high_low <- qnorm(wilcox_chce_high_low$p.value/2)

diff <- processed_df2$chce_high_low2 - 0
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])

# ESTIMATION TASK ---------------------------------------------------------
qplot(processed_df$valEst_enr)
shapiro.test(processed_df$valEst_enr)
qplot(processed_df$valEst_high)
shapiro.test(processed_df$valEst_high)
qplot(processed_df$valEst_low)
shapiro.test(processed_df$valEst_low)

wilcox_valest_enr_low <- wilcox.test(x = processed_df$valEst_enr, y = processed_df$valEst_low, paired = T)
diff <- processed_df$valEst_enr - processed_df$valEst_low
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])

z_valest_enr_low <- qnorm(wilcox_valest_enr_low$p.value/2)


wilcox_valest_enr_high <- wilcox.test(processed_df$valEst_enr, processed_df$valEst_high, paired = T)

diff <- processed_df$valEst_enr - processed_df$valEst_high
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])

z_valest_enr_high <- qnorm(wilcox_valest_enr_high$p.value/2)
wilcox_valest_high_low <- wilcox.test(processed_df$valEst_high, processed_df$valEst_low, paired = T)
z_valest_high_low <- qnorm(wilcox_valest_high_low$p.value/2)

diff <- processed_df$valEst_high - processed_df$valEst_low
diff <- diff[diff!=0]
diff.rank <- rank(abs(diff))
diff.rank
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])
