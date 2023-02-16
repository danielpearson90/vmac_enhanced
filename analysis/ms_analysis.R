library(here)
library(afex)
library(emmeans)
library(lme4)
library(sjPlot)
library(effects)
library(lmerTest)

source(here("preprocessing", "preprocessing.R"))
source(here("other", "dz_calculator.R"))


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

# salience capture effects
enr_abs_ttest <- t.test(processed_df$rt_enr, processed_df$rt_abs, paired = T)
high_abs_ttest <- t.test(processed_df$rt_high, processed_df$rt_abs, paired = T)
low_abs_ttest <- t.test(processed_df$rt_low, processed_df$rt_abs, paired = T)

enriched_absent_es <- dz_calculator(processed_df$rt_enr, processed_df$rt_abs)
high_absent_es <- dz_calculator(processed_df$rt_high, processed_df$rt_abs)
low_absent_es <- dz_calculator(processed_df$rt_low, processed_df$rt_abs)
high_low_es <- dz_calculator(processed_df$rt_low, processed_df$rt_high)
enriched_low_es <- dz_calculator(processed_df$rt_low, processed_df$rt_enr)
enriched_high_es <- dz_calculator(processed_df$rt_enr, processed_df$rt_high)

# vmac scores for enriched and high
vmac_standard <- t.test(processed_df$rt_high, processed_df$rt_low, paired = T)
vmac_enriched <- t.test(processed_df$rt_enr, processed_df$rt_low, paired = T)
enriched_effect <- t.test(processed_df$rt_enr, processed_df$rt_high, paired = T)

all_ps <- c(enr_abs_ttest$p.value, high_abs_ttest$p.value, low_abs_ttest$p.value,
            vmac_standard$p.value, vmac_enriched$p.value, enriched_effect$p.value)

p.adjust(all_ps, method = "holm")

# ERROR ANALYSIS -------------------------------------------------------

aov_omni_err <- aov_ez(id = "subNum",
                       dv = "err",
                       within = "trial_type",
                       data = long_err_df,
                       anova_table = list(es = "pes"))
long_err_df <- long_err_df %>%
  mutate(subNum_fct = factor(subNum))

# CHOICE TASK -------------------------------------------------------------

processed_choice_df2 <- mutate(processed_choice_df,
                               trialChoiceTypeFct = factor(trialChoiceType, levels = c(1,2,3), labels = c("enr_high",
                                                                                                          "enr_low",
                                                                                                          "high_low")))
processed_choice_df2 <- mutate(processed_choice_df2,
                               chosenFirst = case_when(chosenOption == 1 ~ 1,
                                                       TRUE ~ 0))

processed_choice_df2 <- mutate(processed_choice_df2,
                               val = case_when(chosenOption == 1 & trialChoiceType == 3 ~ 1,
                                               chosenOption == 2 & trialChoiceType == 1 ~ 1,
                                               TRUE ~ 0),
                               enr = case_when(chosenOption == 1 & trialChoiceType %in% c(1,2) ~ 1,
                                               TRUE ~ 0))


log_enr_high_r <- glmer(chosenFirst ~ 1 + (1|subNum), data = processed_choice_df2 %>% filter(trialChoiceType == 1),
                        family = binomial())

log_enr_low_r <- glmer(chosenFirst ~ 1 + (1|subNum), data = processed_choice_df2 %>% filter(trialChoiceType == 2),
                       family = binomial())

log_high_low_r <- glmer(chosenFirst ~ 1 + (1|subNum), data = processed_choice_df2 %>% filter(trialChoiceType == 3),
                        family = binomial())

summary(log_enr_high_r)
summary(log_enr_low_r)
summary(log_high_low_r)


# ESTIMATION TASK ---------------------------------------------------------
shapiro.test(processed_df$valEst_enr)
shapiro.test(processed_df$valEst_high)
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



# POINTS ANALYSIS ---------------------------------------------------------

points_summary <- processed_extra_df %>%
  summarise(points_enr = mean(points_enr),
            points_high = mean(points_high),
            points_low = mean(points_low),
            points_abs = mean(points_abs))

t.test(processed_extra_df$points_enr, processed_extra_df$points_high, paired = T)


points_long_df <- processed_extra_df %>%
  select(subNum, sonaID, starts_with("points")) %>%
  pivot_longer(cols = starts_with("points"),
               values_to = "points",
               names_to = "trial_type",
               names_prefix = "points_")

aov_omni_points <- aov_ez(id = "subNum",
                       dv = "points",
                       within = "trial_type",
                       data = points_long_df,
                       anova_table = list(es = "pes"))

points_emmeans <- emmeans(aov_omni_points, ~trial_type)

dz_calculator(processed_extra_df$points_enr, processed_extra_df$points_low)
dz_calculator(processed_extra_df$points_enr, processed_extra_df$points_abs)
dz_calculator(processed_extra_df$points_high, processed_extra_df$points_low)
dz_calculator(processed_extra_df$points_high, processed_extra_df$points_abs)
dz_calculator(processed_extra_df$points_low, processed_extra_df$points_abs)
dz_calculator(processed_extra_df$points_enr, processed_extra_df$points_high)



pairs(points_emmeans, adjust = "none")
