# Load required libraries
library(vegan)

# Set the working directory
setwd('/Users/celine.eymery/Documents/University of Manchester/Dissertation/Data/')

# Load the data
dataset <- read.csv("NEWnartiang1000.csv")

# Handle missing values in the whole dataset before separation
dataset[is.na(dataset)] <- 0

# Separate the mosquito community data, landscape variables data, and seasonal data
# Separate the mosquito community data
mosquito_data <- dataset[, c('An._Maculatus', 'An._Pseudowillmori', 'An._Jeyporiensis', 'An._Nivipes', 'An._Nitidus')]
landscape_data <- dataset[, c(     'Building_Material_Brick',
                                   'Building_Material_Concrete',
                                   'Building_Material_Mud',
                                   'Building_Material_Stone',
                                   'Building_Material_Thatch',
                                   'Building_Material_Other',
                                   'Roof_Type_Concrete',
                                   'Roof_Type_Thatched',
                                   'Roof_Type_Thatched_and_Tiles',
                                   'Roof_Type_Tin',
                                   'Animals_Nearby',
                                   'Chicken_Count',
                                   'Cattle_Count',
                                   'Dog_Count',
                                   'Goat_Count',
                                   'Pig_Count',
                                   'Cat_Count',
                                   'Terrain_Hill',
                                   'Terrain_Plain',
                                   'Terrain_Plateau',
                                   'Terrain_Valley',
                                   'Sky_Clear',
                                   'Sky_Heavy_Rain',
                                   'Sky_Light_Rain',
                                   'Sky_Mist',
                                   'Sky_Overcast',
                                   'Sky_Partly_Cloudy',
                                   'Environment_Secondary_Evergreen_Forest',
                                   'Environment_Village',
                                   'Environment_Cultivated_Field',
                                   'Environment_Scrub.bush',
                                   'Wind_Gusts',
                                   'Wind_Light',
                                   'Wind_None',
                                   'Wind_Strong',
                                   'Slept_Under_ITN_LN',
                                   'Used_Repellent_LN',
                                   'Repellent_Type_Coil_LN',
                                   'Repellent_Type_Cream_LN',
                                   'Repellent_Type_Mat_LN',
                                   'Repellent_Type_Spray_LN',
                                   'Repellent_Type_Vaporizer_LN',
                                   'Used_Repellent_CN',
                                   'Repellent_Type_Coil_CN',
                                   'Repellent_Type_Cream_CN',
                                   'Repellent_Type_Mat_CN',
                                   'Repellent_Type_Spray_CN',
                                   'Repellent_Type_Vaporizer_CN',
                                   'Rice',
                                   'Open_Forest',
                                   'Closed_Forest',
                                   'Plantation',
                                   'Village',
                                   'Bare_Earth')]
seasonal_data <- dataset[, "Month"] 

# Remove rows from all datasets where all mosquito_data values are 0
non_zero_rows <- rowSums(mosquito_data) > 0
mosquito_data <- mosquito_data[non_zero_rows, ]
landscape_data <- landscape_data[non_zero_rows, ]
seasonal_data <- seasonal_data[non_zero_rows]

# Compute Jaccard distance matrix for mosquito data
dist_matrix <- vegdist(as.matrix(mosquito_data), method = 'jaccard')

# Remove variables from landscape_data that have only one unique value
landscape_data <- landscape_data[, sapply(landscape_data, function(x) length(unique(x)) > 1)]

# PERMANOVA Analysis with Landscape Variables Only
permanova_result_landscape <- adonis2(dist_matrix ~ ., data = landscape_data, permutations = 999)
print("PERMANOVA with Landscape Variables Only:")
print(permanova_result_landscape)

# PERMANOVA Analysis with Landscape Variables and Seasonality
# Merge 'Month' with landscape_data
combined_data <- cbind(landscape_data, Month = seasonal_data)
# If a factor column in combined_data has only one level, convert it to numeric
combined_data <- combined_data[, sapply(combined_data, function(x) length(unique(x)) > 1)]

# Perform PERMANOVA analysis with combined_data
permanova_result_landscape_seasonality <- adonis2(dist_matrix ~ ., data = combined_data, permutations = 999)
print("PERMANOVA with Landscape Variables and Seasonality:")
print(permanova_result_landscape_seasonality)