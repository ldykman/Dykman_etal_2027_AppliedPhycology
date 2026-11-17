### Model for Restoration Cost Analysis ###

# Lauren Dykman
# Created: Feb 28 2024
# Last Modified: November 06, 2026

rm(list=ls())

# Installing packages

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lme4)
#install.packages("tidyr")
#install.packages("paletteer")
#install.packages("RColorBrewer")
#install.packages("scales")

# Loading packages

library(ggplot2)
library(dplyr)
library(lme4)
library(tidyr)
library(paletteer)
library(RColorBrewer)
library(scales)

# Setting working directory

path <- "/Users/laurendykman/Desktop/github/Dykman_etal_2027_AppliedPhycology"

getwd()
setwd(path)
getwd()

# Importing data

input_file1 = "materials_costs.csv"
data.materials <- read.csv(paste(path, "data", input_file1, sep = "/"), header = TRUE, na = c("", "NA")) %>% filter(rowSums(is.na(.)) < ncol(.))

input_file2 = "boat_costs.csv"
data.boat <- read.csv(paste(path, "data", input_file2, sep = "/"), header = TRUE, na = c("", "NA")) %>% filter(rowSums(is.na(.)) < ncol(.))

input_file3 = "nursery_costs.csv"
data.nursery <- read.csv(paste(path, "data", input_file3, sep = "/"), header = TRUE, na = c("", "NA")) %>% filter(rowSums(is.na(.)) < ncol(.))

input_file4 = "other_studies.csv"
data.other <- read.csv(paste(path, "data", input_file4, sep = "/"), header = TRUE, na = c("", "NA")) %>% filter(rowSums(is.na(.)) < ncol(.))

input_file6 = "denman_kelp_monitoring_clean.csv"
data.kelp <- read.csv(paste(path, "output", input_file6, sep = "/"), header = TRUE, na = c("", "NA")) %>% filter(rowSums(is.na(.)) < ncol(.))

# Converting from CAD to USD 2025 correcting for inflation

data.materials$cost_per_m2_field[data.materials$currency_year == "CAD 2024"] <- data.materials$cost_per_m2_field[data.materials$currency_year == "CAD 2024"] / 1.3700 * (321.943 / 313.689) # USD/CAD exchange rate in 2024 multiplied by inflation rate
data.materials$cost_per_m2_field[data.materials$currency_year == "CAD 2025"] <- data.materials$cost_per_m2_field[data.materials$currency_year == "CAD 2025"] / 1.3977 # USD/CAD exchange rate in 2025

data.nursery$personnel_cost <- data.nursery$personnel_cost / 1.3977 # USD/CAD exchange rate in 2025
data.nursery$materials_cost <- data.nursery$materials_cost / 1.3977 # USD/CAD exchange rate in 2025
data.nursery$total_nursery_cost <- data.nursery$total_nursery_cost / 1.3977 # USD/CAD exchange rate in 2025

data.boat$boat_fees <- data.boat$boat_fees / 1.3977 # USD/CAD exchange rate in 2025
data.boat$personnel_cost <- data.boat$personnel_cost / 1.3977 # USD/CAD exchange rate in 2025
data.boat$tank_fills <- data.boat$tank_fills / 1.3977 # USD/CAD exchange rate in 2025
data.boat$total_boat_cost <- data.boat$total_boat_cost / 1.3977 # USD/CAD exchange rate in 2025

# Editing costs tables into the correct vector format

M <- data.materials %>%
  group_by(method, expense_type, outplant_density_trial) %>%
  summarise_at(vars(cost_per_m2_field),
               list(sum = ~sum(as.numeric(.), na.rm = FALSE)))
M <- M %>% pivot_wider(names_from = expense_type, values_from = sum)
M[is.na(M)] <- 0
M$materials_cost_total <- M$materials_cost + M$salary

N <- data.nursery %>%
  group_by(method) %>%
  summarise_at(vars(personnel_cost, materials_cost, total_nursery_cost, area_per_nursery_unit_m2, area_outplanted_m2, outplant_density),
               list(sum = ~sum(as.numeric(.), na.rm = FALSE)))

B <- data.boat %>%
  group_by(method) %>%
  summarise_at(vars(boat_fees, personnel_cost, tank_fills, total_boat_cost, area_boat_used, area_outplanted_m2, outplant_density),
               list(sum = ~sum(as.numeric(.), na.rm=FALSE)))

# Defining vector variables

method <- M$method
C_mat <- M$materials_cost_total # Vector of the cost of material per square meter of outplanted area in a restoration trial (units = $ / m2 field [trial]) for each method, i.
D <- N$outplant_density_sum # Vector of the restoration density planned for your upscaled project, specifically the nursery area used per area outplanted in the field (units = m2 nursery / m2 field) for each method, i.
D_trial <- M$outplant_density_trial # Vector of the restoration density used in your restoration trial for each method, i, which may be different than your planned density for upscaling. Specifically, the nursery area used per area outplanted in the field (units = m2 nursery / m2 field [trial]).
C_nur <- N$total_nursery_cost_sum # Vector of the total cost of operating a single nursery unit for the growing period (units = $ / nursery unit) for each method, i.
A_nur <- N$area_per_nursery_unit_m2_sum # Vector of the total nursery area (units = m2) available to hold material.
C_bt <- B$total_boat_cost_sum # Vector of the total cost of running a single fully-loaded boat trip (units = $ / boat trip) for each method, i.
A_bt <- B$area_boat_used_sum # A vector of the total boat area (units = m2) available to hold material, which may be limited by area, dive time, or weight capacity.
O_nur <- N$area_outplanted_m2_sum # Vector of the area that can be outplanted by one nursery unit fully loaded for each method, i.
O_bt <- B$area_outplanted_m2_sum # Vector of the area that can be outplanted by one boat trip fully loaded for each method, i.

# The equation

x <- matrix(1, nrow = 10000, ncol = length(C_mat)) * c(1:10000) # Create a data frame of the dimensions you want (# methods x # m2 outplant area)
y <- as.data.frame((C_mat / D_trial * D * t(x) + C_nur * ceiling(t(x) * D / A_nur) + C_bt * ceiling(t(x) * D / A_bt)))# / t(x)) # Calculate cost of restoration at that scale

colnames(y) <- c(1:10000)
y$method <- method

# Formatting data for plotting

upscale <- y %>%
  pivot_longer(!method, names_to = "area", values_to = "cost")

upscale$area <- as.numeric(upscale$area)

upscale$cost_per_m2 <- as.numeric(upscale$cost / upscale$area)

upscale$method <- factor(upscale$method, levels = c("tile - yr1", "tile - yr2", "cobble", "medium gravel", "pea gravel", "line", "direct seeding - diver", "direct seeding - boat"))

# Plotting total cost

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Total_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(upscale, aes(x = area, y = cost, group = method, color = method)) +
         scale_colour_manual(values = brewer.pal(8, "Dark2")[c(2,6,8,3,7,4,1,5)]) +
         geom_line(linewidth = 1.5) +
         xlab(expression("m"^2)) +
         ylab(expression("$ USD 2025")) +
         scale_y_continuous(labels = scales::comma, breaks = seq(0, 150000, by = 25000)) +
         ggtitle("Cost Total") +
         theme_bw() +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=5, width=10) # Setting the theme

# Plotting cost per square meter

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Per_Meter_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(upscale, aes(x = area, y = cost_per_m2, group = method, color = method)) +
         scale_colour_manual(values = brewer.pal(8, "Dark2")[c(2,6,8,3,7,4,1,5)]) +
         geom_line(linewidth = 1.5) +
         geom_point(data = data.other, aes(x = area_m2, y = converted_to_USD_2025), shape = 21,  # Use shape 21 for filled circles
                    fill = "grey",   # Set the fill color to grey
                    color = "black", # Set the border color to black
                    size = 4) +
         xlab(expression("m"^2)) +
         ylab(expression(log ~ "$" / m^2 ~ "USD 2025")) +
         ggtitle("Cost Per Square Meter") +
         scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
         theme_bw() +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 10) # Setting the theme

# Making a data frame of costs at the three scales

cost.subset <- upscale[upscale$area %in% c(100, 1000, 10000),]
cost.subset$area <- as.character(cost.subset$area)

cost.subset$method <- factor(cost.subset$method, levels = c("tile - yr1", "tile - yr2", "cobble", "medium gravel", "pea gravel", "line", "direct seeding - diver", "direct seeding - boat"))

# Plotting costs at the three scales

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Per_Meter_Subset_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(cost.subset, aes(fill = method, x = area, y = cost_per_m2)) +
         geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.6, linewidth = 0.2, color = "black") +
         scale_fill_manual(values = brewer.pal(8, "Dark2")[c(2,6,8,3,7,4,1,5)]) +
         xlab(expression("m"^2)) +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Per Area") +
         theme_bw() +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 10) # Setting the theme

# Saving data tables

# Table of coefficients in the costs equation

C <- cbind(method, C_mat, D, D_trial, C_nur, A_nur, O_nur, C_bt, A_bt, O_bt)

write.csv(C, paste(path, "output", "cost_coefficient_matrix.csv", sep = "/"))

# Costs at different scales

write.csv(cost.subset, paste(path, "output", "cost_at_scales.csv", sep = "/"))

# Calculating costs of materials versus labor

# Materials

materials <- as.numeric(M$materials_cost_total / D_trial * D + N$materials_cost_sum * (D / A_nur))

# Salary

salary <- M$salary / D_trial * D + N$personnel_cost_sum * (D / A_nur) + B$personnel_cost_sum * (D / A_bt)

# Fees

fees <- as.numeric((B$boat_fees_sum + B$tank_fills_sum) * (D / A_bt))

# Adding all together

total <- materials + salary + fees

# Creating data frame of percentage cost

cost_percent <- as.data.frame(cbind(method, materials, salary, fees, total))

cost_percent <- cost_percent %>%
  mutate(across(c("materials", "salary", "fees", "total"), as.numeric))

cost_percent <- cost_percent %>%  pivot_longer(cols = materials:fees, names_to = "category", values_to = "cost")

cost_percent$percent <- cost_percent$cost / cost_percent$total

cost_percent$method <- factor(cost_percent$method, levels = c("tile - yr1", "tile - yr2", "cobble", "medium gravel", "pea gravel", "line", "direct seeding - diver", "direct seeding - boat"))

# Saving file of cost percentage data table

write.csv(cost_percent, paste(path, "output", "cost_category_percent.csv", sep = "/"))

# Plotting total cost categories

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Category_Total_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(cost_percent, aes(fill = category, y = cost, x = method)) + 
         geom_bar(position = "stack", stat = "identity") +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Categories") +
         scale_fill_manual(values = brewer.pal(4, "PuBuGn")[c(2,3,4)]) +
         theme_bw() +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 6, width = 8) # Setting the theme

# Plotting percent cost categories

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Category_Percent_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(cost_percent, aes(fill = category, y = cost, x = method)) + 
         geom_bar(position = "fill", stat = "identity") +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Categories") +
         scale_fill_manual(values = brewer.pal(4, "PuBuGn")[c(2,3,4)]) +
         theme_bw() +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 6, width = 8) # Setting the theme

# Calculating cost per kelp

# First we create a data table of the average number of kelp per m2 for the three successful methods
# Considered kelp a successful adult if it survived 150 days in field, to mid-July
# At this point, the fish community around the kelp was well developed and most had reached max size

# Making column for outplant density of the trial (D_trial)

data.kelp <- data.kelp %>% mutate(D_trial = case_when(
  method == "tile" ~ 0.09,
  method %in% c("cobble", "medium gravel", "pea gravel") ~ 0.132,
  method == "line" ~ 0.0116,
  method == "direct seeding" ~ 0.025))

# Making column of desired outplant density (consistent with D in rest of study)

data.kelp <- data.kelp %>% mutate(D = case_when(
  method == "tile" ~ 0.007,
  method %in% c("cobble", "medium gravel", "pea gravel", "direct seeding") ~ 0.025,
  method == "line" ~ 0.00025))

data.kelp[data.kelp$method == "tile",]$method <- "tile - yr1"
data.kelp[data.kelp$method == "direct seeding",]$method <- "direct seeding - diver"

# Merging with costs data

data.all <- merge(data.kelp, cost.subset, by = "method")
data.all <- data.all[data.all$days_after_outplanting == "149",]

# Summarizing at the plot level

summary <- data.all %>%
  group_by(method, plot_id, number_kelp_in_quadrat, D_trial, D, area, cost, cost_per_m2) %>%
  tally()

summary.2 <- summary %>%
  group_by(method, D_trial, D, area, cost, cost_per_m2) %>%
  summarise_at(vars(number_kelp_in_quadrat),
               list(sum = ~sum(as.numeric(.), na.rm = TRUE)))

summary.2$kelp_per_m2_converted <- summary.2$sum / 5 * summary.2$D / summary.2$D_trial # kelp per m2 at ideal field density   
summary.2$cost_per_kelp <- summary.2$cost_per_m2 / summary.2$kelp_per_m2_converted

summary.2 <- summary.2[!summary.2$cost_per_kelp == "Inf",]
summary.2$method <- factor(summary.2$method, levels = c("medium gravel", "tile - yr1", "cobble"))

write.csv(summary.2, paste(path, "output", "cost_per_kelp.csv", sep = "/"))

# Cost per kelp summing over all five plots

ggsave(paste(path, "figures", paste0("Figure_Denman_Cost_Per_Kelp_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(summary.2, aes(fill = method, x = area, y = cost_per_kelp)) +
         geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8, linewidth = 0.2, color = "black") +
         scale_fill_manual(values = brewer.pal(8, "Dark2")[c(3,2,8)]) +
         xlab(expression("m"^2)) +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Per Kelp") +
         theme_bw() +
         ylim(0, 450) +
         theme(text = element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 10) # Setting the theme 

# Sensitivity analysis varying each coefficient individually

variation <- c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3)
data.sim <- as.data.frame(matrix(nrow = 0, ncol = 10))
A <- c(1, 1, 1, 1, 1, 1)

for (i in 1:length(A)) { # Iterate through each plot

  print(i)
  
  A <- c(1, 1, 1, 1, 1, 1)
  
  for (var in variation) {
    
    print(var)
    print(i)
    
    A <- c(1, 1, 1, 1, 1, 1)
    
    A[i] <- var
    
    print(A[i])
    
    y <- (A[1] * C_mat) * (A[2] * D) / D_trial + (A[3] * C_nur) / (A[4] * A_nur) * (A[2] * D) + (A[5] * C_bt) / (A[6] * A_bt) * (A[2] * D) # Uncomment for calculating total cost
    print(y)
    
    data.sim <- rbind(c(i, var, y), data.sim)
    
  }}

colnames(data.sim) <- c(c("coeff", "var"), method)
data.sim.2 <- pivot_longer(data = data.sim, cols = `cobble`:`tile - yr2`, names_to = "method", values_to = "cost")
data.sim.2$coeff <- as.character(data.sim.2$coeff)
data.sim.2$method <- factor(data.sim.2$method, levels = c("tile - yr1", "tile - yr2", "cobble", "medium gravel", "pea gravel", "line", "direct seeding - diver", "direct seeding - boat"))

# Plotting sensitivity for each coefficient

ggsave(paste(path, "figures", paste0("Figure_Denman_Sensitivity_Analysis_Per_Value_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.sim.2, aes(x = coeff, y = cost, fill = coeff)) +
         geom_boxplot() +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Sensitivity Analysis") +
         facet_wrap(~method, nrow = 2, ncol = 4) +
         scale_fill_manual(values = brewer.pal(8, "BrBG")) +
         theme_bw() +
         theme(text = element_text(size = 18),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 8, width = 11) # Setting the theme

# Sensitivity analysis varying all variables random iterations

variation <- c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3)
data.sim <- as.data.frame(matrix(nrow = 0, ncol = 14))

for (i in 1:10000) { # Iterate through each plot
  
  print(i)
    
  A <- sample(variation, 6, replace = TRUE, prob = NULL)
  
  y <- (A[1] * C_mat) * (A[2] * D) / D_trial + (A[3] * C_nur) / (A[4] * A_nur) * (A[2] * D) + (A[5] * C_bt) / (A[6] * A_bt) * (A[2] * D) # Uncomment for calculating total cost
  
  data.sim <- rbind(c(A, y), data.sim)
}

colnames(data.sim) <- c(c("V1 nursery cost", "V2 nursery area", "V3 seeding density", "V4 materials costs", "V5 boat cost", "V6 boat area"), method)
data.sim.2 <- pivot_longer(data = data.sim, cols = `cobble`:`tile - yr2`, names_to = "method", values_to = "cost")
data.sim.2$method <- factor(data.sim.2$method, levels = c("tile - yr1", "tile - yr2", "cobble", "medium gravel", "pea gravel", "line", "direct seeding - diver", "direct seeding - boat"))

summary <- data.sim.2 %>%
  group_by(method) %>%
  summarise_at(vars(cost),
               list(mean = ~mean(as.numeric(.), na.rm=TRUE),
                    sd = ~sd(as.numeric(.), na.rm = TRUE),
                    n = ~sum(!is.na(.))))
summary$se <- summary$sd/sqrt(summary$n)

ggsave(paste(path, "figures", paste0("Figure_Denman_Sensitivity_Analysis_Randomized_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(data.sim.2, aes(x = method, y = cost, fill = method)) +
         geom_boxplot() +
         scale_fill_manual(values = brewer.pal(8, "Dark2")[c(2,6,8,3,7,4,1,5)]) +
         ylab(expression("$ USD 2025")) +
         ggtitle("Cost Sensitivity Analysis") +
         theme_bw() +
         theme(text=element_text(size = 20),
               aspect.ratio = 0.6,
               axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black", linewidth = 0.5),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 6, width = 8) # Setting the theme
