
# Cherkasova --------------------------------------------------------------

# Can approximate Cohens d from beta coefficient and estimated SD of the DV.
# The study only gives the SE of the beta coefficient, but that can be back transformed
# into an estimate of SD

# Effect of sensory cues on choice in VGT

cherkasova_se <- .22
cherkasova_b <- .58
cherkasova_n <- 132

cherkasova_sd <- sqrt(cherkasova_n)*cherkasova_se
cherkasova_d <- cherkasova_b/cherkasova_sd
# 0.23 - small effect


# Spetch ------------------------------------------------------------------

# E1 - effect on risky choice

# d = .28 - small effect

# E2
spetch_r2 <- .11
spetch_r <- sqrt(spetch_r2)
spetch_d <- (2*spetch_r)/(sqrt(1-spetch_r^2))
# 0.703 - medium to large effect
