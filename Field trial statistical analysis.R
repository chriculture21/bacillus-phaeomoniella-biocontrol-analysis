library(agricolae)
library(lme4)
library(readxl)
library(ggplot2)

trt <- c("ba4", "Control")

# Row 1: 25 plants per treatment, randomised
row1 <- design.crd(trt = trt, r = 25, seed = 101, serie = 0)$book
row1$Row <- 1
row1$Position <- 1:50

# Row 2: 25 plants per treatment, randomised separately
row2 <- design.crd(trt = trt, r = 25, seed = 202, serie = 0)$book
row2$Row <- 2
row2$Position <- 1:50

# Combine and keep the useful columns
layout <- rbind(row1, row2)
layout <- layout[, c("Row", "Position", "trt")]
names(layout)[3] <- "Treatment"

# Check: 25 of each treatment per row
table(layout$Row, layout$Treatment)

# Save the planting plan
write.csv(layout, "directory/layout.csv", row.names = FALSE)

##------------------------------------------------------------------------------
#Field trial statistical analysis, both treatment and rows as fixed effects in 
#the model.

df_field <- read_excel("directory/data.xlsx", 
                       col_types = c("text", "text", "numeric"))

df_field$Treatment <- relevel(factor(df_field$Treatment), ref = "Control")
df_field$Row <- factor(df_field$Row)

m <- lm(log ~ Treatment + Row, data = df_field)

# Assumptions
shapiro.test(resid(m))
leveneTest(resid(m) ~ df_field$Treatment, center = median)
qqnorm(resid(m)); qqline(resid(m))

# Results
anova(m)
summary(m)        # Treatment ba4 row = difference vs. control (cm)
confint(m)        # 95% CI for the difference
car::Anova(m, type = 2) # get the F-test and the p-value

##------------------------------------------------------------------------------
##Figure generation from the field experiment data

library(ggpubr)

df_biomass <- read_excel("directory/data.xlsx", 
                       col_types = c("text", "text", "numeric"))

df_discol <- read_excel("directory/data.xlsx", 
                        col_types = c("text", "text", "numeric"))

df_biomass$Treatment <- factor(df_biomass$Treatment, levels = c("Control", "ba4"))
df_discol$Treatment <- factor(df_discol$Treatment, levels = c("Control", "ba4"))

##------------------------------------------------------------------------------
#Vascular discoloration
p_dl <- ggbarplot(df_discol, x = "Treatment", y = "Discoloration",
                  add = "mean_se", fill = "Treatment",
                  ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(x = "Treatment", y = "Vascular discoloration (cm)", tag = "(a)") +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black", linewidth = 0.6),
        axis.title = element_text(face = "bold"),
        axis.ticks = element_line(colour = "black"),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = "bottomleft")

p_dl

tops <- aggregate(Discoloration ~ Treatment, data = df_discol,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))
pvals <- c("ba4" = 0.00446)
tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

p_dl <- p_dl + geom_text(data = tops, aes(x = Treatment, y = Discoloration, label = label),
                         vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p_dl

##------------------------------------------------------------------------------
#Fungal biomass
p_bio <- ggbarplot(df_biomass, x = "Treatment", y = "Biomass",
                   add = "mean_se", fill = "Treatment",
                   ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(x = "Treatment", y = "Fungal biomass (pg/ng plant DNA)", tag = "(b)") +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black", linewidth = 0.6),
        axis.title = element_text(face = "bold"),
        axis.ticks = element_line(colour = "black"),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = "bottomleft")

tops <- aggregate(Biomass ~ Treatment, data = df_biomass,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))
pvals <- c("ba4" = 0.0715)
tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

p_bio <- p_bio + geom_text(data = tops, aes(x = Treatment, y = Biomass, label = label),
                           vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p_bio

##combine the two plots
fig_field <- ggarrange(p_dl, p_bio, ncol = 2, nrow = 1)
fig_field
fig_field <- annotate_figure(fig_field,
                             top = text_grob("Field trial", size = 16, face = "bold"))
fig_field
ggsave("directory/plot.pdf", fig_field, width = 8, height = 5)
