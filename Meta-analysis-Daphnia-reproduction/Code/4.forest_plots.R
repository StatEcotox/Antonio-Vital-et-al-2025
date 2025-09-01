# Load packages
library(metafor)
library(orchaRd)
library(cowplot)
library(ggplot2)


# Import data
dat = readRDS("Data/models.rds")


# Forest plots

#species -----
species = dat$m_species
p.species = orchard_plot(species, mod = "sp_name", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                     trunk.size = 1, branch.size = 1.2,
                     twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "italic", angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#20B2AA", "#AC4657", "#C3C770", "#9095CD")) +
  scale_color_manual(values = c("#20B2AA", "#AC4657", "#C3C770", "#9095CD"))
p.species


#Save plot
png("Plots/Forest.plot.species.png", width = 16, height = 12, units = "cm", res = 1000, pointsize = 5)
p.species
dev.off()


#polymer -----
polymer <- dat$m_polymer
p.polymer <- orchard_plot(polymer, mod = "polymer", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                      trunk.size = 1, branch.size = 1.2,
                      twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#88CCEF", "#CD6678", "#DDCD77", "#342288",
                               "#8FB31D", "#AB4499", "#44AA99", "#16E2F5",
                               "#6A0DAD", "#F2A2E8", "#661100", "#F75D59",
                               "#CA762B", "#14A600")) +
  scale_color_manual(values= c("#88CCEF", "#CD6678", "#DDCD77", "#342288",
                               "#8FB31D", "#AB4499", "#44AA99", "#16E2F5",
                               "#6A0DAD", "#F2A2E8", "#661100", "#F75D59",
                               "#CA762B", "#14A600"))
p.polymer

#Save plot
png("Plots/Forest.plot.polymer.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.polymer
dev.off()




#shape -----
shape = dat$m_shape
p.shape = orchard_plot(shape, mod = "shape", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                     trunk.size = 1, branch.size = 1.2,
                     twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#6495ED", "#FF4500", "#8FBC8F")) +
  scale_color_manual(values = c("#6495ED", "#FF4500", "#8FBC8F"))
p.shape

#Save plot
png("Plots/Forest.plot.shape.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.shape
dev.off()




#MP.NP -------
MP.NP = dat$m_MP.NP
p.MP.NP = orchard_plot(MP.NP, mod = "MP.NP", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                     trunk.size = 1, branch.size = 1.2,
                     twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#FF0080", "#347C17")) + 
  scale_color_manual(values = c("#FF0080", "#347C17"))
p.MP.NP

#Save plot
png("Plots/Forest.plot.size.category.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.MP.NP
dev.off()



#biotic (DOM present)-------
biotic = dat$m_biotic
p.biotic = orchard_plot(biotic, mod = "biotic_env", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                       trunk.size = 1, branch.size = 1.2,
                       twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#347C17", "#342288")) +
  scale_color_manual(values = c("#347C17", "#342288"))
p.biotic

#Save plot
png("Plots/Forest.plot.DOM.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.biotic
dev.off()


#### Supplementary Material ####


#Fluorescence-------
fluorescence = dat$m_fluorescence
p.fluorescence = orchard_plot(fluorescence, mod = "fluorescence", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                        trunk.size = 1, branch.size = 1.2,
                        twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#FF4500", "#342288")) +
  scale_color_manual(values = c("#FF4500", "#342288"))
p.fluorescence

#Save plot
png("Plots/Forest.plot.fluorescence.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.fluorescence
dev.off()



#Modification type-------
mod.type = dat$m_mod.type
p.mod.type = orchard_plot(mod.type, mod = "mod_type", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                              trunk.size = 1, branch.size = 1.2,
                              twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#FF0080", "#342288", "#6495ED", "#FF4500", "#8FBC8F", "#88CCEF", "#CD6678", "#DDCD77")) +
  scale_color_manual(values = c("#FF0080", "#342288", "#6495ED", "#FF4500", "#8FBC8F", "#88CCEF", "#CD6678", "#DDCD77"))
p.mod.type

#Save plot
png("Plots/Forest.plot.mod.type.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.mod.type
dev.off()



#Presence of surfactant-------
surfactant = dat$m_surf
p.surfactant = orchard_plot(surfactant, mod = "surfactant", group = "pub_ID", xlab = "Mean difference", alpha = 0.5,
                          trunk.size = 1, branch.size = 1.2,
                          twig.size = 0.5, legend.pos = "none", k.pos="left") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size= 14)) +
  scale_fill_manual(values = c("#88CCEF", "#CD6678")) +
  scale_color_manual(values = c("#88CCEF", "#CD6678"))
p.mod.type

#Save plot
png("Plots/Forest.plot.surfactant.png", width = 16, height = 16, units = "cm", res = 1000, pointsize = 5)
p.surfactant
dev.off()


### END-------------------