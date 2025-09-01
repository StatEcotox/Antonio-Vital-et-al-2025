# import data:
analysis_dat = readRDS("Data/analysis.rds")
df = as.data.frame(analysis_dat)

str(df)

# 1.Handling NA values-------
# A. NA as additional factor level "not reported": surface_charge,

NA.to.notreported = function(dframe, colname) {
  dframe[,colname] <- as.character(dframe[,colname])
  dframe[is.na(dframe[,colname]),colname] <- "not reported"
  dframe[,colname] <- as.factor(dframe[,colname])
}

columns = c("surface_charge")
for(i in 1:length(columns)){
  df[, columns[i]] = NA.to.notreported(dframe = df, colname = columns[i])
}
summary(df)

# B. NA as additional factor "none": mod_type, DOC_type
df[,"mod_type"] <- as.character(df[,"mod_type"])
df[is.na(df[,"mod_type"]),"mod_type"] <- "none"
df[,"mod_type"] <- as.factor(df[,"mod_type"])

summary(df)

# 2. Check concentrations ----
# do all samples have values in mg per ml and particles per ml?

## All the samples report concentrations in mg_l (see "0.data_preparation.R")
sel = is.na(df$conc_mg_ml.1)
sum(sel) #0 


# 3. Scale numeric predictors -----
df$age_d = as.numeric(df$age_d)
df$exp_time_d = as.numeric(df$exp_time_d)
df$temperature = as.numeric(df$temperature)

str(df)
df[,c("age_d", "exp_time_d", "temperature", "conc_mg_l.1", "size_um_mean", "density_gcm.3")] <- scale(df[,c("age_d", "exp_time_d", "temperature", "conc_mg_l.1", "size_um_mean", "density_gcm.3")])
summary(df)



# 4. Select variables for final dataset------
subset = subset(df, select = c("sample_ID", "pub_ID",
                               "sp_name", "age_d", "weathered", "exp_time_d", "temperature",
                               "conc_mg_l.1", "biodegradable.y.n",
                               "polymer", "size_um_mean", "MP.NP", "shape", "col_yes_no", "fluorescence",
                               "modified", "mod_type",
                               "surface_charge", "surfactant", "biotic_env",
                               "yi", "vi"))
summary(subset)


sum(complete.cases(subset)) 

# Save datase
saveRDS(subset, "Data/scaled_data.rds")


### END-------------------