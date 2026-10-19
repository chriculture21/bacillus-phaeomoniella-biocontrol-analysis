library(ggpubr)

#load data
df <- read_excel("directory/Data.xlsx", 
                 col_types = c("text", "text", "numeric"))

#Copy the data and order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(AUDPC ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$AUDPC,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "AUDPC",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 1", x = "Treatment", y = "AUDPC", tag = "(a)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.04, 0.03),
        axis.line = element_line(colour = "black", linewidth = 0.5)
        )

p

tops <- aggregate(AUDPC ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Pch), typed by hand
pvals <- c("ba11 + Pch"       = 0.96,
           "ba24 + Pch"       = 0.99,
           "ba4 + Pch"        = 0.45,
           "ba5 + Pch"        = 0.02,
           "Consortium + Pch" = 0.99)

tops$p <- pvals[as.character(tops$Treatment)]

tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""     # control: no label

p <- p + geom_text(data = tops, aes(x = Treatment, y = AUDPC, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
p

ggsave("directory/AUDPC_inoculated.pdf", p, width = 5, height = 6, dpi = 300)

##------------------------------------------------------------------------------
#load data
df <- read_excel("directory/Data.xlsx", 
                 col_types = c("text", "text", "numeric"))

library(ggpubr)

#Order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(AUDPC ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$AUDPC,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "AUDPC",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 2", x = "Treatment", y = "AUDPC", tag = "(b)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.04, 0.03),
        axis.line = element_line(colour = "black", linewidth = 0.5)
  )

p

# 2. Height of each bar + SE
tops <- aggregate(AUDPC ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Control), converted to stars
pvals <- c("ba11"       = 0.6810,
           "ba24"       = 0.7832,
           "ba4"        = 0.0172,
           "ba5"        = 0.1014,
           "Consortium" = 0.7109)

tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

# 4. Add the stars and save
p <- p + geom_text(data = tops, aes(x = Treatment, y = AUDPC, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p
ggsave("directory/AUDPC_natural.pdf", p, width = 5, height = 6)

##-----------------------------------------------------------------------------
##Discoloration - Experimental group 1
#Order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(Discoloration ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$Discoloration,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "Discoloration",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 1", x = "Treatment",
       y = "Vascular discoloration (cm)", tag = "(a)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.line = element_line(colour = "black", linewidth = 0.6),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.02, 0.02))

p

# 2. Height of each bar + SE
tops <- aggregate(Discoloration ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Pch), converted to stars
pvals <- c("ba11 + Pch"       = 0.8577,
           "ba24 + Pch"       = 0.0037,
           "ba4 + Pch"        = 0.0339,
           "ba5 + Pch"        = 0.0002,
           "Consortium + Pch" = 0.0641)

tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

# 4. Add the stars and save
p <- p + geom_text(data = tops, aes(x = Treatment, y = Discoloration, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p
ggsave("directory/Discoloration_group1.pdf", p, width = 5, height = 6)

##------------------------------------------------------------------------------
##Discoloration experimental group 2
#Order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(Discoloration ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$Discoloration,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "Discoloration",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 2", x = "Treatment",
       y = "Vascular discoloration (cm)", tag = "(b)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.6),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.02, 0.02))

p
# 2. Height of each bar + SE
tops <- aggregate(Discoloration ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Control), converted to stars
pvals <- c("ba11"       = 0.3266,
           "ba24"       = 1.0000,
           "ba4"        = 0.9999,
           "ba5"        = 0.9980,
           "Consortium" = 0.2108)

tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

# 4. Add the stars and save
p <- p + geom_text(data = tops, aes(x = Treatment, y = Discoloration, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p
ggsave("directory/Discoloration_group2.pdf", p, width = 5, height = 6)

##------------------------------------------------------------------------------
##Fungal biomass - experimental group 1
#Order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(Biomass ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$Biomass,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "Biomass",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 1", x = "Treatment",
       y = "Pathogen biomass (fg/100 ng of Plant DNA)", tag = "(c)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.6),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.02, 0.02))
p
# 2. Height of each bar + SE
tops <- aggregate(Biomass ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Pch), converted to stars
pvals <- c("ba11 + Pch"       = 0.0851,
           "ba24 + Pch"       = 0.0632,
           "ba4 + Pch"        = 0.0168,
           "ba5 + Pch"        = 0.0632,
           "Pch + Consortium" = 0.1469)

tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

# 4. Add the stars and save
p <- p + geom_text(data = tops, aes(x = Treatment, y = Biomass, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p
ggsave("directory/Biomass_group1.pdf", p, width = 5, height = 6)

##------------------------------------------------------------------------------
#Biomass experimental group 2
#Order treatments from highest to lowest mean
df_plot <- df
means <- aggregate(Biomass ~ Treatment, data = df_plot, FUN = mean)
df_plot$Treatment <- factor(df_plot$Treatment,
                            levels = means$Treatment[order(means$Biomass,
                                                           decreasing = TRUE)])

# 1. Bar plot with mean ± SE
p <- ggbarplot(df_plot, x = "Treatment", y = "Biomass",
               add = "mean_se", fill = "Treatment",
               ggtheme = theme_grey(base_size = 14)) +
  scale_fill_viridis_d() +
  labs(title = "Experimental group 2", x = "Treatment",
       y = "Pathogen biomass (fg/100 ng of Plant DNA)", tag = "(d)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.line = element_line(colour = "black", linewidth = 0.6),
        plot.tag = element_text(size = 16, face = "bold"),
        plot.tag.position = c(0.02, 0.02))
p
# 2. Height of each bar + SE
tops <- aggregate(Biomass ~ Treatment, data = df_plot,
                  FUN = function(x) mean(x) + sd(x) / sqrt(length(x)))

# 3. Dunnett p-values (vs. Control), converted to stars
pvals <- c("ba11"       = 0.9998,
           "ba24"       = 0.2735,
           "ba4"        = 0.9991,
           "ba5"        = 0.1781,
           "Consortium" = 0.7833)

tops$p <- pvals[as.character(tops$Treatment)]
tops$label <- as.character(cut(tops$p,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                               labels = c("***", "**", "*", "")))
tops$label[is.na(tops$label)] <- ""

# 4. Add the stars and save
p <- p + geom_text(data = tops, aes(x = Treatment, y = Biomass, label = label),
                   vjust = -0.3, size = 8, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
p
ggsave("directory/Biomass_group2.pdf", p, width = 5, height = 6)
