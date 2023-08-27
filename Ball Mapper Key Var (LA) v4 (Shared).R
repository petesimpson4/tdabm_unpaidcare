#Ballmapper Code
# This script provides the code used to produce the TDABM model used to evaluate regional unpaid care imbalances

#Load Libraries
library(dplyr)
library(BallMapper)
library(readr)
library(readxl)
library(openxlsx)
library(data.table)
library(jsonlite)
library(purrr)
library(Rcpp)

#Set up BallmapperCPP - a copy of file provided should be saved in your working directory
setwd("*****ENTER LOCATION OF WORKING DIRECTORY WHERE BALLMAPPER.CPP IS SAVED******")
sourceCpp('BallMapper.cpp')

#Function to use BallMapper CPP
BallMapperCpp <- function( points , values , epsilon ) {
  output <- BallMapperCppInterface( points , values , epsilon )
  colnames(output$vertices) = c('id','size')
  return_list <- output
}#BallMapperCpp

#Function for high resolution BM Graphs
ColorIgraphPlot5a <- function(outputFromBallMapper, showVertexLabels = TRUE, ltext = "Colouration", showLegend = FALSE, minimal_ball_radius = 7, maximal_ball_scale = 20, maximal_color_scale = 10, seed_for_plotting = -1, store_in_file = "", default_x_image_resolution = 512, default_y_image_resolution = 512, number_of_colors = 100, minc = -99999, maxc = 99999) {
  vertices = outputFromBallMapper$vertices
  vertices[, 2] <- maximal_ball_scale * vertices[, 2] / max(vertices[, 2]) + minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges, vertices = vertices, directed = FALSE)
  
  jet.colors <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green", "cyan","dodgerblue2", "violet"))
  color_spectrum <- jet.colors(number_of_colors)
  
  min_ <- ifelse(minc == -99999, min(outputFromBallMapper$coloring), minc)
  max_ <- ifelse(maxc == 99999, max(outputFromBallMapper$coloring), maxc)
  min_ <- min_ - 0.01 * (max_ - min_)
  max_ <- max_ + 0.01 * (max_ - min_)
  
  color <- vector(length = length(outputFromBallMapper$coloring), mode = "double")
  for (i in 1:length(outputFromBallMapper$coloring)) {
    position <- base::max(base::ceiling(number_of_colors * (outputFromBallMapper$coloring[i] - min_) / (max_ - min_)), 1)
    color[i] <- color_spectrum[position]
  }
  igraph::V(net)$color <- color
  
  if (showVertexLabels == FALSE) igraph::V(net)$label = NA
  
  if (seed_for_plotting != -1) base::set.seed(seed_for_plotting)
  
  if (store_in_file != "") grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)
  
  igraph::V(net)$label.color = "black"
  igraph::V(net)$label.cex = 1.2 #Font Size in Balls
  
  par(oma = c(0, 0, 0, 4))
  par(mar = c(0, 0, 0, 4))
  
  graphics::plot(net, edge.width = 1, edge.color = "black")  # Change edge.color to black
  
  fields::image.plot(legend.only = TRUE, zlim = c(min_, max_), col = color_spectrum, legend.width = 2.5, legend.shrink = 0.8, axis.args = list(cex.axis = 1.3), legend.args = list(text = ltext, side = 4, font = 0.3, line = 5, cex = 1.5))
  
  if (store_in_file != "") grDevices::dev.off()
}

#Read in Full Census Data, Convert to a Dataframe and View as Table
exp_vars <- read_csv("*****ENTER LOCATION OF DIRECTORY WHERE exp_vars_v2 FILE IS SAVED******")
exp_vars <- as.data.frame(exp_vars)
rownames(exp_vars) <- exp_vars$`geography code` #Use Geography Code as index
exp_vars$`geography code` <- NULL

#Read in Key Variable Dataset (Finalised 50 dimensional subset defined in paper)
key_vars <- read_csv("*****ENTER LOCATION OF DIRECTORY WHERE key_vars_v4 FILE IS SAVED******")
key_vars <- as.data.frame(key_vars)
rownames(key_vars) <- exp_vars$`geography code` #Use Geography Code as index
key_vars$`geography code` <- NULL
key_vars_excLDN <- filter(key_vars, region != "London")
key_vars <- subset(key_vars, select=-c(geography,region))
key_vars_excLDN <- subset(key_vars_excLDN, select=-c(geography,region))

#Read in Supplementary Data Sources (e.g. Urban Categories)
inc_vars <- read_csv("*****ENTER LOCATION OF DIRECTORY WHERE inc_vars FILE IS SAVED******")
inc_vars <- as.data.frame(inc_vars)
rownames(inc_vars) <- inc_vars$`geography code` #Use Geography Code as index
inc_vars$`geography code` <- NULL

#############################Ball Mapper Tool################################

#Create a vector with the Unpaid Care columns and remove these from the explanatory variable dataset
unpd_car_cols <- grep("^unpd_car", colnames(exp_vars), value = TRUE)
exp_vars_v2 <- exp_vars[, !(colnames(exp_vars) %in% unpd_car_cols)]

#Normalise continuous Variables (Population, Households and Population Density)
exp_vars_v2[, 1:5] <- normalize_to_min_0_max_1(exp_vars_v2[, 1:5])

# Create a new data frame with only the removed "unpd_car" columns
unpd_car <- exp_vars[, colnames(exp_vars) %in% unpd_car_cols]
unpd_car$unpd_car_gt20_prop <- unpd_car$unpd_car_2049_prop + unpd_car$unpd_car_gt50_prop
unpd_car$unpd_car_gt20_asp_prop <- unpd_car$unpd_car_2049_asp_prop + unpd_car$unpd_car_gt50_asp_prop
unpd_car$unpd_car_high_prop <- unpd_car$unpd_car_gt50_prop / (unpd_car$unpd_car_lt19_prop + unpd_car$unpd_car_2049_prop + unpd_car$unpd_car_gt50_prop)
#unpd_car$gt20_5pc<-as.numeric(unpd_car$unpd_car_gt20_prop>0.05)

#Set up Explanatory variable sets
x1<-as.data.frame(exp_vars_v2) #Explanatory variables (full)
x2<-as.data.frame(key_vars) #Explanatory variables (key vars)
x3<-as.data.frame(key_vars_excLDN) #Explanatory variables (key vars)

#Unpaid Care Outomes 
u1<-as.data.frame(unpd_car$unpd_car_none_prop) #Proportion of population performing no unpaid care
u2<-as.data.frame(unpd_car$unpd_car_gt20_prop) #Proportion of population performing 20 or more hours unpaid care
u3<-as.data.frame(unpd_car$unpd_car_gt50_prop) #Proportion of population performing 50 or more hours unpaid care
u4<-as.data.frame(unpd_car$unpd_car_lt19_prop) #Proportion of population performing 20 or more hours unpaid care
u5<-as.data.frame(unpd_car$unpd_car_2049_prop)
u6<-as.data.frame(unpd_car$unpd_car_high_prop)
u7<-as.data.frame(unpd_car$unpd_car_none_asp_prop) #Proportion of population performing no unpaid care
u8<-as.data.frame(unpd_car$unpd_car_gt20_asp_prop) #Proportion of population performing 20 or more hours unpaid care
u9<-as.data.frame(unpd_car$unpd_car_gt50_asp_prop) #Proportion of population performing 50 or more hours unpaid care
u10<-as.data.frame(unpd_car$unpd_car_lt19_asp_prop) #Proportion of population performing 20 or more hours unpaid care
u11<-as.data.frame(unpd_car$unpd_car_2049_asp_prop)


#Demographic Outcomes
d1<-as.data.frame(inc_vars$median_age)
d2<-as.data.frame(exp_vars_v2$age_under18_prop)
d3<-as.data.frame(exp_vars_v2$age_18_29_prop)
d4<-as.data.frame(exp_vars_v2$age_30_64_prop)
d5<-as.data.frame(exp_vars_v2$age_65_80_prop)
d6<-as.data.frame(exp_vars_v2$age_over80_prop)
d7<-as.data.frame(key_vars$houshold_2ddep_pls_prop)
d8<-as.data.frame(key_vars$wrk_NSSEC3_prop)
d9<-as.data.frame(key_vars$wrk_NSSEC4_prop)
d10<-as.data.frame(key_vars$wrk_NSSEC2_prop)
d11<-as.data.frame(key_vars$wrk_trav_otr_prop)
d12<-as.data.frame(key_vars$wrk_NSSEC1_prop)
d13<-as.data.frame(key_vars$wrk_trav_wfh_prop)

#Occupation
o1<-as.data.frame()

#Geography
g1<-as.data.frame(inc_vars$urban_class) 
g2<-as.data.frame(inc_vars$coastal) 

#Plot the Ball Mapper - Key Variables
bm<-BallMapperCpp(x2,u2,0.35)
#ColorIgraphPlot(bm,seed_for_plotting=5, maximal_color_scale = 10)
ColorIgraphPlot5a(bm,seed_for_plotting=5, ltext = "Residents Performing 20+ Hours Unpaid Care Weekly (%)")

#Create Summary of Balls
points_covered_bm <- bm$points_covered_by_landmarks
pc_grid_bm <- t(plyr::ldply(points_covered_bm, rbind))
pc_grid_bm <- as.data.table(pc_grid_bm)
file_path <- "***SPECIFY OUTPUT DIRECTORY***"
write.xlsx(pc_grid_bm, file_path)


# Create an empty dataframe to store the results
results <- data.frame(variable = character(),
                      estimate = numeric(),
                      std_error = numeric(),
                      t_value = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each variable in key_vars to perform univariate tests
for (var in names(key_vars)) {
  # Create the formula for the linear regression model (NOTE VARIABLE FOR UNIVARIATE TEST SHOULD BE DEFINED HERE)
  formula <- as.formula(paste("unpd_car$unpd_car_gt50_prop ~ key_vars$", var))
  
  # Fit the linear regression model
  model <- lm(formula)
  
  # Extract the coefficients from the model summary and store them in the results dataframe
  coef_summary <- summary(model)$coefficients[2, ]
  estimate <- coef_summary["Estimate"]
  std_error <- coef_summary["Std. Error"]
  t_value <- coef_summary["t value"]
  p_value <- coef_summary["Pr(>|t|)"]
  results <- rbind(results, data.frame(variable = var, estimate = estimate, std_error = std_error, t_value = t_value, p_value = p_value))
}

# Print the summary table
print(results)

# Set the output file path
output_path <- "***SPECIFY OUTPUT PATH FOR UNIVATIATE TEST RESULTS"

# Export the summary table to a CSV file
write.csv(results, file = output_path, row.names = FALSE)

# Print the confirmation message
cat("Summary table exported to:", output_path, "\n")


