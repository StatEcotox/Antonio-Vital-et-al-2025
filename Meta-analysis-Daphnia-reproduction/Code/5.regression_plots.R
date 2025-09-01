library(grid)
library(gridExtra)
library(metafor)
library(ggplot2)
library(dplyr)

# Load the data
dat <- readRDS("Data/analysis.rds")


# Prepare data
mg_l <- dat[!is.na(dat$conc_mg_l.1), ]
mg_l <- droplevels(mg_l)
mg_l$se <- mg_l$vi / (mg_l$c_mean + mg_l$t_mean + mg_l$t_SD + mg_l$c_SD)
mg_l$invse <- 1 / mg_l$se
mg_l$invse.by.10 <- mg_l$invse / 10
mg_l$log.invse <- log(mg_l$invse)

# scale datapoints
size =  scales::rescale(mg_l$log.invse, to = c(1, 7))


#################################################
#                   Age plot                    #
#################################################

# model for age
m.age <- rma.mv(yi, vi, mods = ~ as.numeric(age_d), random = ~ 1|pub_ID/sample_ID, data = mg_l)
m.age

### Plot and save it
png("Plots/Regression.age.png", width = 7, height = 7, units = "cm", res = 1000, pointsize = 5)
par(mar=c(5, 6, 4, 2) + 0.1)
age = regplot(m.age, bty = "l" ,ylab="Mean difference", xlab="Age of individuals (days)",psize = size,
              col=adjustcolor("grey20", alpha.f=0.2),
              bg=adjustcolor("grey20", alpha.f=0.1), lcol="grey60",
              at=c(100, 0, -100, -230), cex.lab=1.9, cex.axis=1.9, las = 1, mgp = c(4,1,0))
dev.off()  #


####################################################
#              Exposure duration plot              #
####################################################

# Model
m_exp <- rma.mv(yi, vi, mods = exp_time_d, random = ~ 1|pub_ID/sample_ID, data = mg_l)
m_exp

### Plot and save it
png("Plots/Regression.exposure.png", width = 7, height = 7, units = "cm", res = 1000, pointsize = 5)
par(mar=c(5, 6, 4, 2) + 0.1)
exp = regplot(m_exp, bty = "l", ylab = "Mean difference", xlab = "Exposure duration (days)", psize=size,
              col = adjustcolor("grey20", alpha.f = 0.2),
              bg = adjustcolor("grey20", alpha.f=0.1), lcol = "grey60",
              at=c(-200, -100, 0, 100), cex.lab=1.9, cex.axis=1.9, las = 1, mgp = c(4,1,0))
dev.off()



##################################################
#              Temperature plot                  #
##################################################

# Model
m_temp <- rma.mv(yi, vi, mods = ~temperature, random = ~ 1|pub_ID/sample_ID, data = mg_l)
m_temp

### Plot and save it
png("Plots/Regression.temperature.png", width = 7, height = 7, units = "cm", res = 1000, pointsize = 5)
par(mar=c(5, 6, 4, 2) + 0.1)
temp = regplot(m_temp, bty = "l", ylab = "Mean difference", xlab = "Temperature (°C)", psize=size,
               col = adjustcolor("grey20", alpha.f = 0.2),
               bg = adjustcolor("grey20", alpha.f = 0.1), lcol = "grey60",at=c(-200, -100, 0, 100),
               cex.lab=1.9, cex.axis=1.9, las = 1, mgp = c(4,1,0))
dev.off()



#############################
#           Size            #
#############################
#Log scale
mg_l$size_um_mean <- log(mg_l$size_um_mean)

# scale datapoints
size =  scales::rescale(mg_l$size_um_mean, to = c(1, 7))

# model for size
m.size <- rma.mv(yi, vi, mods = ~size_um_mean , random = ~ 1|pub_ID/sample_ID, data = mg_l)
m.size


### Plot and save it
png("Plots/Regression.size.png", width = 7, height = 7, units = "cm", res = 1000, pointsize = 4)
par(mar=c(5, 6, 4, 2) + 0.2)
conc = regplot(m.size, mod = "size_um_mean",ylab="Mean difference", xlab="Particle size (µm)",psize = size,
               col=adjustcolor("grey20", alpha.f=0.2),
               bg=adjustcolor("grey20", alpha.f=0.1), lcol="grey60",
               at=c(100, 0, -100, -230), cex.lab=1.9, cex.axis=1.9, las = 1, mgp = c(4,1,0))
dev.off()  #




#################################################
#         Concentration plot                    #
#################################################
# Log scale
mg_l$conc_mg_l.1 <- log(mg_l$conc_mg_l.1)

# model for concentration
m.conc <- rma.mv(yi, vi, mods = ~ conc_mg_l.1 + I(conc_mg_l.1^2) +I(conc_mg_l.1^3), random = ~ 1|pub_ID/sample_ID, data = mg_l)
m.conc

# Prediction range in log space
xs <- seq(-17, 7, length = 601)
tmp <- predict(m.conc, newmods = cbind(xs, xs^2, xs^3))

# Tick markers
tick_labels <- c("1e-07", "1e-05", "0.001", "0.1", "1", "10", "1000")
tick_positions_original <- as.numeric(tick_labels)
tick_positions_log <- log(tick_positions_original)

### Plot and save it
png("Plots/Regression.concentration.png", width = 7, height = 7, units = "cm", res = 1000, pointsize = 4)
par(mar = c(5, 6, 6, 2))

conc <- regplot(m.conc, mod = "conc_mg_l.1", pred = tmp, 
                bty = "l", 
                ylab = "Mean difference", 
                xlab = "Concentration (mg/L)", 
                psize = size, 
                xlim = c(min(xs), max(xs)), 
                xvals = xs,
                col = adjustcolor("grey20", alpha.f = 0.2),
                bg = adjustcolor("grey20", alpha.f = 0.1), 
                lcol = "grey60",
                at = c(100, 0, -100, -230), 
                cex.lab = 1.9, 
                cex.axis = 1.9, 
                las = 1, 
                mgp = c(4, 1, 0),
                xaxt = "n")

# Add axis
axis(side = 1, at = tick_positions_log, labels = tick_labels, cex.axis = 1.9)


dev.off()