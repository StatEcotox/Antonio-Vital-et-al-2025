#load packages:
library(dplyr)
library(metafor)
library(forcats)

####### Calculate missing values when possible #######

# import data
dat <- read.csv("Data/Meta-analysis-reproduction.csv", stringsAsFactors = TRUE)

# check data structure
str(dat)

# number of studies for which data extraction was successful:
length(unique(dat$pub_ID)) #64

# str(dat)
dim(dat) #369 and #53


## Check variable classes:-------------------------
# change all integer classes to numeric:
dat[sapply(dat, is.integer)] <- lapply(dat[sapply(dat, is.integer)], as.numeric)
# str(dat)




#######################################################################
# take median of age_d and size for missing values

##### age_d
# Calculate the median for age_d ignoring NAs
median_age = median(dat$age_d, na.rm = TRUE)

# Fill NA values of age_d with median
dat = dat %>%
  mutate(age_d = ifelse(is.na(age_d), median_age, age_d))


##### size as um_mean
# Calculate the median size ignoring NAs
median_size = median(dat$size_um_mean, na.rm = TRUE)

# Fill NA values of age_d with median
dat = dat %>%
  mutate(size_um_mean = ifelse(is.na(size_um_mean), median_size, size_um_mean))


# make new categorical column with MP (larger than 0.1 um = 100 nm) vs. NP (smaller or equal to 0.1 um = 100 nm):
dat$MP.NP = NA
dat$MP.NP[dat$size_um_mean <= 0.1] = "Nanoplastic" 
dat$MP.NP[dat$size_um_mean > 0.1] = "Microplastic"
dat$MP.NP = as.factor(dat$MP.NP)


# fix modification types
levels(dat$mod_type)
levels(dat$mod_type)[levels(dat$mod_type) == "aminated"] <- "Aminated"
levels(dat$mod_type)[levels(dat$mod_type) == "carboxylated"] <- "Carboxylated"
levels(dat$mod_type)[levels(dat$mod_type) == "UV-weathered"] <- "UV-weathered"
levels(dat$mod_type)[levels(dat$mod_type) == "UV-aged"] <- "UV-weathered"
levels(dat$mod_type)[levels(dat$mod_type) == "DiNP, stab. 0,25%"] <- "DiNP"
levels(dat$mod_type)[levels(dat$mod_type) == "Al₂O₃"] <- "Aluminium oxide"
levels(dat$mod_type)[levels(dat$mod_type) == "incubated"] <- NA
levels(dat$mod_type)[levels(dat$mod_type) == "blended"] <- "PBAT"
levels(dat$mod_type)[levels(dat$mod_type) == "recycled"] <- "Recycled"
levels(dat$mod_type)

### Polymer type-------
#number of studies per polymer type:
dat$polymer = as.factor(dat$polymer)
poly <- levels(dat$polymer)
poly
n.studies= data.frame(
  polymer = c(poly,NA),
  n.stud = rep(NA,length(poly) + 1)
)

# loop to count the number of unique studies for each group
i=1
for(p in poly){
  sub = dat$pub_ID[dat$polymer == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 

# Count NAs
sub_na <- dat$pub_ID[is.na(dat$polymer)]
n.studies$n.stud[i] <- length(unique(sub_na))

n.studies
# polymer n.stud
# 1                                   EAA      1
# 2                                  HDPE      1
# 3                                    PA      1
# 4                                   PCL      1
# 5                                    PE     14
# 6                                   PES      2
# 7                                   PET      2
# 8                          PET, PS, ABS      1
# 9                                   PLA      2
# 10                        Plastic mix A      1
# 11                        Plastic mix B      1
# 12                                   PP      1
# 13                                   PS     34
# 14                                  PUR      1
# 15                                  PVC      4
# 16                        recycled LDPE      1
# 17 Thermoset amino formaldehyde polymer      5
# 18                                   TW      2
# 19                          virgin LDPE      1
# 20                                 <NA>      0

as.data.frame(table(dat$polymer))
# Var1 Freq
# 1                                   EAA    2
# 2                                  HDPE    2
# 3                                    PA    8
# 4                                   PCL    5
# 5                                    PE   36
# 6                                   PES   10
# 7                                   PET    4
# 8                          PET, PS, ABS    2
# 9                                   PLA    6
# 10                        Plastic mix A    3
# 11                        Plastic mix B    3
# 12                                   PP    1
# 13                                   PS  231
# 14                                  PUR    4
# 15                                  PVC    9
# 16                        recycled LDPE    3
# 17 Thermoset amino formaldehyde polymer   24
# 18                                   TW   13
# 19                          virgin LDPE    3

levels(dat$polymer)[levels(dat$polymer)=="Thermoset amino formaldehyde polymer"] <- "Thermoset"
levels(dat$polymer)[levels(dat$polymer)=="virgin LDPE" ] <- "LDPE"
levels(dat$polymer)[levels(dat$polymer)=="recycled LDPE" ] <- "LDPE"


as.data.frame(table(dat$polymer))
# Var1 Freq
# 1            EAA    2
# 2           HDPE    2
# 3             PA    8
# 4            PCL    5
# 5             PE   36
# 6            PES   10
# 7            PET    4
# 8   PET, PS, ABS    2
# 9            PLA    6
# 10 Plastic mix A    3
# 11 Plastic mix B    3
# 12            PP    1
# 13            PS  231
# 14           PUR    4
# 15           PVC    9
# 16          LDPE    6
# 17     Thermoset   24
# 18            TW   13

# check sample size per polymer type:
poly.samples = dat %>% group_by(polymer) %>% summarize(n = n())
# pool all polymer types with <=3 data points as "others"
others = as.vector(poly.samples$polymer[poly.samples$n <= 3])
levels(dat$polymer)[levels(dat$polymer) %in% others] <- "others"
dat$polymer <- factor(dat$polymer)
dat <- droplevels(dat)
levels(dat$polymer)



# create col_yes_no for color:
levels(dat$color)
dat$col_yes_no = NA
dat$col_yes_no[dat$color == "no" | dat$color == "clear"] = "no"
dat$col_yes_no[dat$color != "no"] = "yes"
dat$col_yes_no <- as.factor(dat$col_yes_no)
dat$col_yes_no = fct_na_value_to_level(dat$col_yes_no, "no")
levels(dat$col_yes_no)

# check for shapes
levels(dat$shape)
levels(dat$shape)[levels(dat$shape) == "fibers"] <- "fiber"
levels(dat$shape)[levels(dat$shape) == "fragments"] <- "fragment"
levels(dat$shape)


# Subset dataset to include only columns used in meta-analysis:
dat <- subset(dat, select = c("pub_ID", "sp_name", "polymer", "size_um_mean", "shape", "col_yes_no", 
                              "fluorescence", "modified", "mod_type", "surface_charge", "surfactant", "biotic_env", 
                              "density_gcm.3", "age_d", "food_present", "biodegradable.y.n",
                              "milled", "weathered","exp_time_d", 
                              "conc_p_ml.1", "conc_mg_l.1", "MP.NP", "temperature",
                              "c_n_replicates", "c_mean", "c_SD", "c_SE", "c_ind_per_replicate", "c_n_individuals",
                              "t_n_replicates", "t_mean", "t_SD", "t_SE", "t_ind_per_replicate", "t_n_individuals"))
str(dat)
dim(dat) #now there are 369 data points and 35 variables left


# Change SD = 0 to a small number
## for control
any(dat$c_SD == 0)
dat$c_SD[dat$c_SD == 0] <- 1e-10

## for treatment
any(dat$t_SD == 0)
dat$t_SD[dat$t_SD == 0] <- 1e-10
any(dat$t_mean == 0)
dat$t_mean[dat$t_mean == 0] <- 1e-10

# Generate Sample_ID based on Pub_ID
dat <- dat %>%
  group_by(pub_ID) %>%
  mutate(sample_ID = paste("", row_number())) %>%
  ungroup()

# sampleID as a factor:
dat$sample_ID <- as.factor(dat$sample_ID)
dat$sample_ID


# Check for NA for concentration mg_l
sum(is.na(dat$conc_mg_l.1)) #94
dat[is.na(dat$conc_mg_l.1) & is.na(dat$density_gcm.3),]

######################################################

# Calculate the median density for each polymer with concentration as NA
median_density_by_polymer = dat %>%
  group_by(polymer) %>%
  summarize(median_density = median(density_gcm.3, na.rm = TRUE))

# Join the mean density back to the original data
dat= left_join(dat, median_density_by_polymer, by = "polymer")

# Fill NA values of density_gcm.3 with the calculated median per polymer type
dat = dat %>%
  mutate(density_gcm.3 = ifelse(is.na(density_gcm.3), median_density, density_gcm.3))

# Remove the temporary column median_density
dat = dat %>% select(-median_density)
dat$density_gcm.3

# Add density for PVC from literature (1.33 g/cm3 - ToMEx database)
dat$density_gcm.3[dat$polymer == "PVC"] <- 1.33

######################################################

# Calculate concentrations in mg_l: estimate mg_l from particles_ml, size and density; ----
## Samples that do not report concentrations mg_l nor p_ml
sel = is.na(dat$conc_mg_l.1) & is.na(dat$conc_p_ml.1)
sum(sel) #0 samples report neither mg_l nor particles_ml

# Calculate particle volume for different shapes according to Thornton-Hampton et al. 2022:
dat$p.vol[!is.na(dat$shape) & dat$shape == "spherical"] = 4/3 * pi * (dat$size_um_mean[!is.na(dat$shape) & dat$shape == "spherical"]/2000)^3  #unit:mm^3
dat$p.vol[!is.na(dat$shape) & dat$shape == "fragment"] = pi/6 * (dat$size_um_mean[!is.na(dat$shape) & dat$shape == "fragment"]/1000)^3 * 0.4^2 # unit:mm^3

# see: Kooi M, Koelmans AA. Simplifying microplastic via continuous probability distributions for size, shape, and density. Environ Sci Technol Lett. 2019;6(9):551–7. 
# and: Thornton Hampton, Leah M., Heili Lowman, Scott Coffin, Emily Darin, Hannah De Frond, Ludovic Hermabessiere, Ezra Miller, Vera N. de Ruijter, Andrea Faltynkova, 
# Syd Kotar, Laura Monclús, Samreen Siddiqui, Johannes Völker, Susanne Brander, Albert A. Koelmans, Chelsea M. Rochman, Martin Wagner, and Alvine C. Mehinto. 2022. ‘A Living Tool for the Continued Exploration of Microplastic Toxicity’. Microplastics and Nanoplastics 2(1):13. doi: 10.1186/s43591-022-00032-4.

# calculate fiber volumes:
dat$p.vol[dat$shape=="fiber" & dat$pub_ID == "Schell2022"] = pi * (20/2000)^2 * 0.6 #unit:mm^3 length: Schell2022: 600µm (mean) and diameter 20um
dat$p.vol[dat$shape=="fiber" & dat$pub_ID == "Schwarzer2022"] = pi * (3/2000)^2 * 0.075 #unit:mm^3 length: Schwarzer2022: 75µm (mean) and diameter 3um


# Calculate particle weight in mg
# particle weight(mg) = density(mg/mm^3) * Vol(mm^3) --- (density in mg/mm^3 is numerically the same as g/cm^3)
dat$p.weight = dat$density_gcm.3 * dat$p.vol #unit: mg

# get all samples where conc in mg_l is missing, but particles_ml and density are available:
sel = is.na(dat$conc_mg_l.1) & !is.na(dat$conc_p_ml.1) & !is.na(dat$density_gcm.3)
sum(sel) #94

# Calculate concentration in mg_l for these samples:
# mg_l = number of particles_ml * particle weight (mg) * 1000:
dat$conc_mg_l.1[sel] = (dat$p.weight[sel] * dat$conc_p_ml.1[sel]) * 1000
rm(sel)

#get all samples where concentration in particles per ml is missing, but mg per l and particle weight are available
sel = is.na(dat$conc_p_ml.1) & !is.na(dat$conc_mg_l.1) & !is.na(dat$p.weight)
# Calculate particles_ml from mg_l and particle weight:
dat$conc_p_ml.1[sel] = (dat$conc_mg_l.1[sel]/dat$p.weight[sel])/1000
rm(sel)
# 
sum(is.na(dat$conc_mg_l.1)) # 0 NA's for mg/l
sum(is.na(dat$conc_p_ml.1)) # 13 NA's for p/ml 


## Calculate effect size (yi) for mean difference and sampling variance (vi): -----------
analysis_dat <- escalc(measure = "MD", m1i = t_mean, sd1i = t_SD, n1i = t_n_replicates, m2i = c_mean, sd2i = c_SD,
                       n2i = c_n_replicates, slab = pub_ID, data = dat)

# calculate standard error from sampling variance and inverse standard error for plotting:
analysis_dat$se <- sqrt(analysis_dat$vi)/sqrt(analysis_dat$c_n_replicates + analysis_dat$t_n_replicates)
analysis_dat$invse <- 1/analysis_dat$se

# Check for zero standard error before division
analysis_dat$invse.by.10 <- ifelse(analysis_dat$se != 0, 1 / analysis_dat$se / 10, NA)
analysis_dat$log.invse <- log(analysis_dat$invse)

# Calculate mean for control
control = escalc(mi = dat$c_mean, sdi = dat$c_SD, ni = dat$c_n_individuals, measure = "MN", data = dat)
fit = rma(yi = control$yi, vi = control$vi)
fit
# Random-Effects Model (k = 369; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 2976.8127 (SE = 221.3624)
# tau (square root of estimated tau^2 value):      54.5602
# I^2 (total heterogeneity / total variability):   99.99%
# H^2 (total variability / sampling variability):  13733.08
# 
# Test for Heterogeneity:
#   Q(df = 368) = 783405.0221, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb    ci.ub      
# 65.3706  2.8538  22.9065  <.0001  59.7772  70.9639  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





##### Mean for each species
# D. magna
d.magna <- subset(dat, sp_name == "Daphnia magna")
c.magna = escalc(mi = d.magna$c_mean, sdi = d.magna$c_SD, ni = d.magna$c_n_individuals, measure = "MN", data = d.magna)
fit.magna = rma(yi = c.magna$yi, vi = c.magna$vi)
fit.magna
# Random-Effects Model (k = 331; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 2604.6560 (SE = 204.6339)
# tau (square root of estimated tau^2 value):      51.0358
# I^2 (total heterogeneity / total variability):   99.99%
# H^2 (total variability / sampling variability):  13283.19
# 
# Test for Heterogeneity:
#   Q(df = 330) = 724237.3246, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb    ci.ub      
# 62.7676  2.8195  22.2617  <.0001  57.2414  68.2937  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# D. pulex
d.pulex <- subset(dat, sp_name == "Daphnia pulex")
c.pulex = escalc(mi = d.pulex$c_mean, sdi = d.pulex$c_SD, ni = d.pulex$c_n_individuals, measure = "MN", data = d.pulex)
fit.pulex = rma(yi = c.pulex$yi, vi = c.pulex$vi)
fit.pulex
# Random-Effects Model (k = 32; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 1612.4725 (SE = 420.0425)
# tau (square root of estimated tau^2 value):      40.1556
# I^2 (total heterogeneity / total variability):   99.80%
# H^2 (total variability / sampling variability):  495.71
# 
# Test for Heterogeneity:
#   Q(df = 31) = 2211.4355, p-val < .0001
# 
# Model Results:
#   
#   estimate      se     zval    pval    ci.lb    ci.ub      
# 79.0146  7.1901  10.9894  <.0001  64.9223  93.1069  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# D. carinata
d.carinata <- subset(dat, sp_name == "Daphnia carinata")
c.carinata = escalc(mi = d.carinata$c_mean, sdi = d.carinata$c_SD, ni = d.carinata$c_n_individuals, measure = "MN", data = d.carinata)
fit.carinata = rma(yi = c.carinata$yi, vi = c.carinata$vi)
fit.carinata
# Random-Effects Model (k = 4; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 33449.9238 (SE = 27318.5904)
# tau (square root of estimated tau^2 value):      182.8932
# I^2 (total heterogeneity / total variability):   99.98%
# H^2 (total variability / sampling variability):  4603.09
# 
# Test for Heterogeneity:
#   Q(df = 3) = 11977.8536, p-val < .0001
# 
# Model Results:
#   
#   estimate       se    zval    pval    ci.lb     ci.ub    
# 189.4271  91.4581  2.0712  0.0383  10.1726  368.6816  * 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# D. galeata
d.galeata <- subset(dat, sp_name == "Daphnia galeata x longispina")
c.galeata = escalc(mi = d.galeata$c_mean, sdi = d.galeata$c_SD, ni = d.galeata$c_n_individuals, measure = "MN", data = d.galeata)
fit.galeata = rma(yi = c.galeata$yi, vi = c.galeata$vi)
fit.galeata
# Random-Effects Model (k = 2; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 0 (SE = 1.5120)
# tau (square root of estimated tau^2 value):      0
# I^2 (total heterogeneity / total variability):   0.00%
# H^2 (total variability / sampling variability):  1.00
# 
# Test for Heterogeneity:
#   Q(df = 1) = 0.0000, p-val = 1.0000
# 
# Model Results:
#   
#   estimate      se     zval    pval   ci.lb   ci.ub      
# 8.4700  0.7311  11.5845  <.0001  7.0370  9.9030  *** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#save data frame for use in plotting and modelling:
saveRDS(analysis_dat, "Data/analysis.rds")


### END-------------------