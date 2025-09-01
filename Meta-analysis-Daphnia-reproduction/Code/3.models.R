# Load packages
library(metafor)
library(orchaRd)
library(drda)

# Import data
dat = readRDS("Data/scaled_data.rds")

# Check data structure
str(dat)
summary(as.data.frame(dat))
dat = na.omit(dat)
dat = droplevels(dat)


# calculate full effect size
res1 <- rma.mv(yi, vi,  random = ~ 1 | pub_ID/sample_ID, data = dat)
res1

# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  334.8129  18.2979     64     no            pub_ID 
# sigma^2.2  497.6634  22.3084    369     no  pub_ID/sample_ID 
# 
# Test for Heterogeneity:
#   Q(df = 368) = 29979.0537, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval     ci.lb    ci.ub      
# -13.6000  2.7700  -4.9098  <.0001  -19.0290  -8.1710  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Full model
full <- rma.mv(yi, vi,
               mods = ~ sp_name + age_d +
                 exp_time_d +
                 temperature  +
                 conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                 polymer + size_um_mean +
                 MP.NP + shape +
                fluorescence + mod_type +
                surfactant + biotic_env, 
               random = ~ 1 | pub_ID/sample_ID,  data = dat)
full



# Species reduced model
m.species <- rma.mv(yi, vi,
                    mods = ~ age_d +
                      exp_time_d +
                      temperature  +
                      conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                      polymer + size_um_mean +
                      MP.NP + shape +
                      fluorescence + mod_type +
                      surfactant + biotic_env, 
                    random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Age reduced model
m.age <-rma.mv(yi, vi,
               mods = ~ sp_name +
                 exp_time_d +
                 temperature  +
                 conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                 polymer + size_um_mean +
                 MP.NP + shape +
                 fluorescence + mod_type +
                 surfactant + biotic_env, 
               random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Exposure time reduced model
m.exp.time <- rma.mv(yi, vi,
                     mods = ~ sp_name + age_d +
                       temperature  +
                       conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                       polymer + size_um_mean +
                       MP.NP + shape +
                       fluorescence + mod_type +
                       surfactant + biotic_env, 
                     random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Temperature reduced model
m.temp <- rma.mv(yi, vi,
                 mods = ~ sp_name + age_d +
                   exp_time_d +
                   conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                   polymer + size_um_mean +
                   MP.NP + shape +
                   fluorescence + mod_type +
                   surfactant + biotic_env, 
                 random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Concentration mg_l reduced model
m.conc <- rma.mv(yi, vi,
                 mods = ~ sp_name + age_d +
                   exp_time_d +
                   temperature  +
                   polymer + size_um_mean +
                   MP.NP + shape +
                   fluorescence + mod_type +
                   surfactant + biotic_env, 
                 random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Polymer type reduced model
m.polymer <- rma.mv(yi, vi,
                    mods = ~ sp_name + age_d +
                      exp_time_d +
                      temperature  +
                      conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                      size_um_mean +
                      MP.NP + shape +
                      fluorescence + mod_type +
                      surfactant + biotic_env, 
                    random = ~ 1 | pub_ID/sample_ID,  data = dat)


# MP or NP reduced model
m.MP.NP <- rma.mv(yi, vi,
                  mods = ~ sp_name + age_d +
                    exp_time_d +
                    temperature  +
                    conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                    polymer + size_um_mean +
                    shape +
                    fluorescence + mod_type +
                    surfactant + biotic_env, 
                  random = ~ 1 | pub_ID/sample_ID,  data = dat)


# Size reduced model
m.size <- rma.mv(yi, vi,
                 mods = ~ sp_name + age_d +
                   exp_time_d +
                   temperature  +
                   conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                   polymer +
                   MP.NP + shape +
                   fluorescence + mod_type +
                   surfactant + biotic_env, 
                 random = ~ 1 | pub_ID/sample_ID,  data = dat)



# Shape reduced model
m.shape <- rma.mv(yi, vi,
                  mods = ~ sp_name + age_d +
                    exp_time_d +
                    temperature  +
                    conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                    polymer + size_um_mean +
                    MP.NP + 
                    fluorescence + mod_type +
                    surfactant + biotic_env, 
                  random = ~ 1 | pub_ID/sample_ID,  data = dat)


# Fluorescence reduced model
m.fluorescence <- rma.mv(yi, vi,
                         mods = ~ sp_name + age_d +
                           exp_time_d +
                           temperature  +
                           conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                           polymer + size_um_mean +
                           MP.NP + shape +
                           mod_type +
                           surfactant + biotic_env, 
                         random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Modification type reduced model
m.mod.type <- rma.mv(yi, vi,
                     mods = ~ sp_name + age_d +
                       exp_time_d +
                       temperature  +
                       conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                       polymer + size_um_mean +
                       MP.NP + shape +
                       fluorescence +
                       surfactant + biotic_env, 
                     random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Surfactant reduced model
m.surf <- rma.mv(yi, vi,
                 mods = ~ sp_name + age_d +
                   exp_time_d +
                   temperature  +
                   conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                   polymer + size_um_mean +
                   MP.NP + shape +
                   fluorescence + mod_type +
                   biotic_env, 
                 random = ~ 1 | pub_ID/sample_ID,  data = dat)

# Biotic environment reduced model
m.biotic <- rma.mv(yi, vi,
                   mods = ~ sp_name + age_d +
                     exp_time_d +
                     temperature  +
                     conc_mg_l.1 + I(conc_mg_l.1^2) + I(conc_mg_l.1^3) +
                     polymer + size_um_mean +
                     MP.NP + shape +
                     fluorescence + mod_type +
                     surfactant, 
                   random = ~ 1 | pub_ID/sample_ID,  data = dat)


# Comparison with full model
species = anova(full, m.species, refit=TRUE)
species # p-val 0.4571

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 35 3471.1101 3607.9880 3478.6777 -1700.5551 2.6023 0.4571 20643.7712 


conc = anova(full, m.conc, refit=TRUE)
conc # p-val = <.0001 --- significant

#         df       AIC       BIC      AICc     logLik     LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539                20228.7399 
# Reduced 35 3496.9967 3633.8746 3504.5643 -1713.4984 28.4890 <.0001 20905.7978 


age = anova(full, m.age, refit=TRUE)
age # p-val 0.7659

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3472.5964 3617.2959 3481.0919 -1699.2982 0.0886 0.7659 20255.2836 


exp.time = anova(full, m.exp.time, refit=TRUE)
exp.time # p-val 0.0007 --- significant

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539                20228.7399 
# Reduced 37 3483.8716 3628.5711 3492.3671 -1704.9358 11.3639 0.0007 21286.5532 


temp = anova(full, m.temp, refit=TRUE)
temp # p-val <.0001 --- significant

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539                20228.7399 
# Reduced 37 3514.8642 3659.5637 3523.3597 -1720.4321 42.3564 <.0001 20249.8824


polymer = anova(full, m.polymer, refit=TRUE)
polymer # p-val 0.5668

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539                20228.7399 
# Reduced 26 3461.0695 3562.7502 3465.1747 -1704.5347 10.5617 0.5668 21055.2541 


size = anova(full, m.size, refit=TRUE)
size # p-val 0.7666

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3472.5959 3617.2954 3481.0913 -1699.2979 0.0881 0.7666 20228.7566 


MP.NP = anova(full, m.MP.NP, refit=TRUE)
MP.NP # p-val 0.0419 --- significant

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3476.6470 3621.3464 3485.1424 -1701.3235 4.1392 0.0419 20546.0595 


shape = anova(full, m.shape, refit=TRUE)
shape # 0.2129

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 36 3473.6018 3614.3905 3481.6259 -1700.8009 3.0940 0.2129 20309.8598 


fluorescence = anova(full, m.fluorescence, refit=TRUE)
fluorescence # 0.5608

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3472.8461 3617.5455 3481.3415 -1699.4230 0.3383 0.5608 20413.9730 


mod.type = anova(full, m.mod.type, refit=TRUE)
mod.type #p-val 0.9539

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE 
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 31 3462.6116 3583.8463 3468.4988 -1700.3058 2.1038 0.9539 20298.0820 


surf = anova(full, m.surf, refit=TRUE)
surf # p-val 0.0853

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3475.4696 3620.1691 3483.9651 -1700.7348 2.9618 0.0853 22060.8755 


biotic = anova(full, m.biotic, refit=TRUE)
biotic # p-val 0.9870

#         df       AIC       BIC      AICc     logLik    LRT   pval         QE
# Full    38 3474.5078 3623.1181 3483.4896 -1699.2539               20228.7399 
# Reduced 37 3472.5080 3617.2075 3481.0035 -1699.2540 0.0003 0.9870 20229.5545


# Check for residual heterogeneity
summary(full)
# Check log-likelihood profile of sigma^2
profile(full)

# Check residuals
hist(residuals(full))
funnel(full, atransf = exp)


# Fit reduced models
#species -----
m_species <- rma.mv(yi,vi, mods = ~sp_name-1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_species)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1726.9307   3453.8615   3465.8615   3489.2609   3466.0961   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  318.6831  17.8517     64     no            pub_ID 
# sigma^2.2  497.2486  22.2991    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 365) = 29717.1463, p-val < .0001
# 
# Test of Moderators (coefficients 1:4):
#   QM(df = 4) = 30.9727, p-val < .0001
# 
# Model Results:
#   
#   estimate       se     zval    pval     ci.lb     ci.ub      
# sp_nameDaphnia carinata              -53.1968  17.5879  -3.0246  0.0025  -87.6684  -18.7252   ** 
# sp_nameDaphnia galeata x longispina    2.6304  23.8355   0.1104  0.9121  -44.0863   49.3470      
# sp_nameDaphnia magna                 -12.3943   2.8485  -4.3512  <.0001  -17.9772   -6.8114  *** 
# sp_nameDaphnia pulex                 -17.4312   7.5907  -2.2964  0.0217  -32.3088   -2.5537    * 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#polymer -------
m_polymer = rma.mv(yi, vi, mods = ~polymer-1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_polymer)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1682.1213   3364.2426   3394.2426   3452.3665   3395.6543   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  310.2803  17.6148     64     no            pub_ID 
# sigma^2.2  503.8126  22.4458    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 356) = 25209.2399, p-val < .0001
# 
# Test of Moderators (coefficients 1:13):
#   QM(df = 13) = 38.3752, p-val = 0.0003
# 
# Model Results:
#   
#   estimate       se     zval    pval     ci.lb     ci.ub     
# polymerothers       3.6446  10.6056   0.3437  0.7311  -17.1420   24.4312     
# polymerPA           0.2293  19.4007   0.0118  0.9906  -37.7953   38.2539     
# polymerPCL         -5.4069  20.7829  -0.2602  0.7947  -46.1406   35.3269     
# polymerPE         -17.9113   6.1687  -2.9036  0.0037  -30.0017   -5.8208  ** 
# polymerPES        -19.3779  13.4653  -1.4391  0.1501  -45.7693    7.0135     
# polymerPET        -51.3809  16.5121  -3.1117  0.0019  -83.7440  -19.0177  ** 
# polymerPLA        -21.1751  13.8621  -1.5276  0.1266  -48.3443    5.9941     
# polymerPS         -11.3161   3.6128  -3.1322  0.0017  -18.3970   -4.2351  ** 
# polymerPUR         -8.0851  16.7194  -0.4836  0.6287  -40.8545   24.6842     
# polymerPVC        -19.3718  11.4366  -1.6938  0.0903  -41.7872    3.0436   . 
# polymerLDPE        -0.3498  19.8802  -0.0176  0.9860  -39.3144   38.6148     
# polymerThermoset  -25.2390   8.0784  -3.1243  0.0018  -41.0723   -9.4057  ** 
# polymerTW         -25.3049  12.8577  -1.9681  0.0491  -50.5055   -0.1042   * 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#shape --------
m_shape = rma.mv(yi, vi, mods = ~shape-1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_shape)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1733.7335   3467.4669   3477.4669   3496.9801   3477.6336   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  323.2525  17.9792     64     no            pub_ID 
# sigma^2.2  502.5870  22.4185    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 366) = 27593.5573, p-val < .0001
# 
# Test of Moderators (coefficients 1:3):
#   QM(df = 3) = 25.9768, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval     ci.lb    ci.ub      
# shapefiber      -23.8272  9.3070  -2.5601  0.0105  -42.0686  -5.5859    * 
# shapefragment   -13.2335  4.2110  -3.1426  0.0017  -21.4868  -4.9801   ** 
# shapespherical  -13.1226  3.1945  -4.1078  <.0001  -19.3838  -6.8615  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#MP.NP -------
m_MP.NP = rma.mv(yi, vi, mods = ~MP.NP -1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_MP.NP)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1735.1791   3470.3582   3478.3582   3493.9796   3478.4687   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  338.5667  18.4002     64     no            pub_ID 
# sigma^2.2  487.0362  22.0689    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 367) = 29941.2533, p-val < .0001
# 
# Test of Moderators (coefficients 1:2):
#   QM(df = 2) = 31.4212, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval     ci.lb     ci.ub      
# MP.NPMicroplastic  -16.5543  2.9793  -5.5564  <.0001  -22.3937  -10.7149  *** 
#   MP.NPNanoplastic    -1.3079  5.3072  -0.2464  0.8054  -11.7098    9.0941      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#biotic (DOM present) -------
m_biotic = rma.mv(yi, vi, mods = ~biotic_env -1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_biotic)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1739.0611   3478.1222   3486.1222   3501.7437   3486.2327   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  334.9313  18.3011     64     no            pub_ID 
# sigma^2.2  499.6615  22.3531    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 367) = 27116.8774, p-val < .0001
# 
# Test of Moderators (coefficients 1:2):
#   QM(df = 2) = 24.1221, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval     ci.lb    ci.ub      
# biotic_envno   -13.6650  2.7887  -4.9001  <.0001  -19.1307  -8.1992  *** 
# biotic_envyes  -12.2214  7.1487  -1.7096  0.0873  -26.2326   1.7898    . 
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### Supplementary Material ####

# Fluorescence -------
m_fluorescence = rma.mv(yi, vi, mods = ~fluorescence -1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_fluorescence)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1738.6866   3477.3732   3485.3732   3500.9947   3485.4837   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  343.0271  18.5210     64     no            pub_ID 
# sigma^2.2  497.2657  22.2995    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 367) = 28556.5513, p-val < .0001
# 
# Test of Moderators (coefficients 1:2):
#   QM(df = 2) = 23.9624, p-val < .0001
# 
# Model Results:
#   
# estimate      se     zval    pval     ci.lb    ci.ub      
# fluorescenceno   -12.7732  3.2554  -3.9236  <.0001  -19.1538  -6.3926  *** 
# fluorescenceyes  -15.1905  4.2274  -3.5933  0.0003  -23.4761  -6.9049  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Modification type -------
m_mod.type = rma.mv(yi, vi, mods = ~mod_type -1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_mod.type)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1711.2897   3422.5793   3442.5793   3481.4681   3443.2079   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  334.7847  18.2971     64     no            pub_ID 
# sigma^2.2  506.8549  22.5134    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 361) = 25890.9144, p-val < .0001
# 
# Test of Moderators (coefficients 1:8):
#   QM(df = 8) = 26.8787, p-val = 0.0007
# 
# Model Results:
#   
#   estimate       se     zval    pval     ci.lb    ci.ub      
# mod_typeAluminium oxide   -7.9377  21.4948  -0.3693  0.7119  -50.0668  34.1913      
# mod_typeAminated          -5.7260  16.6050  -0.3448  0.7302  -38.2713  26.8193      
# mod_typeBP-3             -10.2887  15.0843  -0.6821  0.4952  -39.8534  19.2759      
# mod_typeCarboxylated      -5.5312   7.0290  -0.7869  0.4313  -19.3078   8.2454      
# mod_typeDiNP              10.5247  26.9276   0.3909  0.6959  -42.2525  63.3019      
# mod_typenone             -15.1543   2.9686  -5.1048  <.0001  -20.9727  -9.3359  *** 
# mod_typeRecycled         -10.0853  16.9355  -0.5955  0.5515  -43.2783  23.1077      
# mod_typeUV-weathered     -12.7499  11.9278  -1.0689  0.2851  -36.1279  10.6280      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Presence of surfactant -------
m_surf = rma.mv(yi, vi, mods = ~surfactant -1, random = ~ 1 | pub_ID/sample_ID, data = dat)
summary(m_surf)
# Multivariate Meta-Analysis Model (k = 369; method: REML)
# 
# logLik    Deviance         AIC         BIC        AICc   
# -1738.3097   3476.6195   3484.6195   3500.2409   3484.7299   
# 
# Variance Components:
#   
#   estim     sqrt  nlvls  fixed            factor 
# sigma^2.1  334.3235  18.2845     64     no            pub_ID 
# sigma^2.2  497.8465  22.3125    369     no  pub_ID/sample_ID 
# 
# Test for Residual Heterogeneity:
#   QE(df = 367) = 29440.3767, p-val < .0001
# 
# Test of Moderators (coefficients 1:2):
#   QM(df = 2) = 24.7726, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval     ci.lb    ci.ub      
# surfactantno   -14.4038  2.9443  -4.8920  <.0001  -20.1746  -8.6330  *** 
# surfactantyes   -7.4589  8.1360  -0.9168  0.3593  -23.4052   8.4874      
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



## save results:---
saveRDS(list(full = full, m_species = m_species, m_polymer = m_polymer, m_shape = m_shape, m_MP.NP = m_MP.NP, m_biotic = m_biotic, m_fluorescence = m_fluorescence,
             m_mod.type = m_mod.type, m_surf = m_surf),
        "Data/models.rds")

### END-------------------