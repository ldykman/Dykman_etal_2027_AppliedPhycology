### Analyzing the success of kelp outplanting methods at Denman Island ###

# Lauren Dykman
# Created: April 16, 2024
# Modified: November 17, 2026

rm(list=ls())

# Installing packages

# install.packages("dplyr")
# install.packages("ggfortify")
# install.packages("RColorBrewer")

# Loading packages

library(dplyr)
library(ggfortify)
library(RColorBrewer)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2027_AppliedPhycology"

getwd()
setwd(path)
getwd()

# Importing data

input_file = "denman_methods_kelp_monitoring.csv"
data <- read.csv(paste(path, "data", input_file, sep = "/"), header=TRUE)
data$date <- as.Date(data$date)
data$days_after_outplanting <- as.character(as.Date(data$date) - as.Date("2024-02-25"))

# Converting data types

data$length_mm <- as.numeric(data$length_mm)
data$length_cm <- as.numeric(data$length_mm/10)
data$days_after_outplanting <- as.numeric(data$days_after_outplanting)

data$method[data$method == "C"] <- "cobble"
data$method[data$method == "M"] <- "medium gravel"
data$method[data$method == "P"] <- "pea gravel"
data$method[data$method == "T"] <- "tile"
data$method[data$method == "L"] <- "line"
data$method[data$method == "D"] <- "direct seeding"

# Removing April because kelp were quite small and data taken in a different way

data <- data[!data$date == "2024-04-10",]

write.csv(data, paste(path, "output", "denman_kelp_monitoring_clean.csv", sep = "/"))

### Plotting number of kelp per m2 ###

# Calculate the mean and standard deviation kelp counts for each method

counts <- data %>%
  group_by(days_after_outplanting, method, plot_id, number_kelp_in_quadrat) %>%
  tally()

data.error <- counts %>%
  group_by(days_after_outplanting, method) %>%
  summarise_at(vars(number_kelp_in_quadrat),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE)))
data.error$se <-data.error$sd/sqrt(5)

data.error$method <- factor(data.error$method, levels = c("tile", "cobble", "medium gravel", "pea gravel", "line", "direct seeding"))

# Barplot of kelp count

ggsave(paste(path, "Figures", paste0("Figure_Denman_Kelp_Counts_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.error, aes(x = days_after_outplanting, y = mean, group = method, color = method)) +
         geom_errorbar(aes(ymin = mean - se, ymax = mean + se), linewidth = 1.5, width = 2) +
         geom_line(linetype = "dashed", linewidth = 1.5) +
         geom_point(size = 2.5) +
         scale_colour_manual(values = brewer.pal(8, "Dark2")[c(2,8,3,7,4,1)]) +
         xlab("days after outplanting") +
         ylab(expression("count per m"^2)) +
         ggtitle("Kelp Density") +
         labs(color = "method") +
         theme_bw() +
         theme(text = element_text(size = 26),
               aspect.ratio = 0.6,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 10) # Setting the theme

### Plotting average stipe length of kelp ###

counts <- data %>%
  group_by(days_after_outplanting, method, plot_id, number_kelp_in_quadrat, length_cm) %>%
  tally()

counts2 <- counts %>%
  group_by(days_after_outplanting, method) %>%
  summarise(non_na_count = sum(!is.na(length_cm)))

counts2$date.type <- paste(counts2$days_after_outplanting, counts2$method)

# Calculate the mean and standard deviation of kelp length for each outplanting method

summary <- data %>%
  group_by(days_after_outplanting, method) %>%
  summarise_at(vars(length_cm),
               list(mean = ~mean(as.numeric(.), na.rm = TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE),
                    n = ~sum(!is.na(.))))
summary$se <- summary$sd/sqrt(summary$n)
summary[is.na(summary)] <- 0

summary$method <- factor(summary$method, levels = c("tile", "cobble", "medium gravel", "pea gravel", "line", "direct seeding"))

# Barplot of kelp length

ggsave(paste(path, "Figures", paste0("Figure_Denman_Kelp_Lengths_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(summary, aes(x = days_after_outplanting, y = mean, group = method, color = method)) +
         geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 2, linewidth = 1.5) +
         geom_line(linetype = "dashed", linewidth = 1.5) +
         geom_point(size = 2.5) +
         scale_colour_manual(values = brewer.pal(8, "Dark2")[c(2,8,3,7,4,1)]) +
         xlab("days after outplanting") +
         ylab("length (cm)") +
         ggtitle("Kelp Length") +
         labs(color = "method") +
         theme_bw() +
         theme(text = element_text(size = 26),
               aspect.ratio = 0.6,
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 10) # Setting the theme
