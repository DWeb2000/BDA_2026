###############################################################################
# RANDOM FOREST MODEL AND DISCRIMINATING VARIABLES ANALYSIS
###############################################################################

################################################################################
# 1) Load required packages
################################################################################

library(randomForest)  # Fit the Random Forest classification model.
library(caret)    # Split data and compute confusion matrices.
library(dplyr)  # Select and manipulate columns.
library(ggplot2)
library(pROC)  # Compute ROC curves and AUC.

############################################################
# 2) Select the variables to use.
############################################################

# Define numeric predictors : 
preds <- c(
  "elevation",
  "NDVI",
  "prec_annual",
  "tmax_mean_c",
  "tas_current_october_c",
  "tas_future_october_2050_c",
  "delta_tas_october_c"
)

# Define categorical predictors :
cats <- c(
  "Temperatur",
  "Moisture",
  "Landcover",
  "Landforms",
  "Climate_Re",
  "WEcosystm"
)

# Keep only the columns that are actually present:
vars_present <- intersect(
  c("species", "longitude", "latitude", preds, cats),
  names(owl)
)

# Build the modelling table :
ml_df <- owl %>%
  dplyr::select(all_of(vars_present))

# Convert categorical predictors to factors :
for(col in cats) {
  if(col %in% names(ml_df)) {
    ml_df[[col]] <- as.factor(ml_df[[col]])
  }
}

# Convert species to factor:
ml_df$species <- as.factor(ml_df$species)

# Remove rows with missing values:
ml_df <- na.omit(ml_df)

# Inspect final table:
str(ml_df)
table(ml_df$species)

############################################################
# 3) Split into train and test sets.
############################################################

# Make the split reproducible:
set.seed(123)

# Stratified split: 70% training, 30% testing:
train_index <- createDataPartition(
  y = ml_df$species,
  p = 0.7,
  list = FALSE
)

# Then, build train and test tables:
train_df <- ml_df[train_index, ]
test_df  <- ml_df[-train_index, ]

# Check species counts in both sets:
table(train_df$species)
table(test_df$species)

############################################################
# 4) Train the Random Forest model
############################################################

# Train the model:
# I remove longitude and latitude here because they aren't environmental predictors.
rf_species <- randomForest(
  species ~ .,
  data = train_df %>% dplyr::select(-longitude, -latitude),
  ntree = 500,
  importance = TRUE
)

# Display model summary:
print(rf_species)

#The Random Forest model demonstrates a moderate ability to distinguish between 
#the two species of owl. The error rate is 20.66%, which means that approximately 
#one in five observations is misclassified during internal validation.
#The model performs better for the barn owl (Tyto alba) than for the little owl 
#(Athene noctua), as the classification error is lower for the barn owl (0.116) than 
#for the little owl (0.364).


############################################################
# 5) Predict the test set.
############################################################

# Predict the class for each test observation:
pred_class <- predict(
  rf_species,
  newdata = test_df %>% dplyr::select(-longitude, -latitude)
)

# Predict class probabilities:
pred_prob <- predict(
  rf_species,
  newdata = test_df %>% dplyr::select(-longitude, -latitude),
  type = "prob"
)

############################################################
# 6) Evaluate model performance.
############################################################

# Confusion matrix:
conf <- confusionMatrix(
  data = pred_class,
  reference = test_df$species
)

print(conf)
#The confusion matrix shows that the Random Forest model correctly 
#distinguishes between the two species of owl. Then, the accuracy is 83.5%, 
#so the model performs significantly better than a prediction based solely on 
#the most common species, a fact also confirmed by the fact that the accuracy is 
#significantly higher than the no-information rate.
#The sensitivity for Athene noctua is 0.7857, which means that approximately 79% of 
#observations of this species are correctly identified. Finally, the balanced accuracy 
#of 0.8244 and the Kappa coefficient of 0.6455 are relatively high and indicate that 
#the environmental variables included in the model contain a signal relevant to 
#distinguishing between the two species.


# ROC and AUC for each species:
species_levels <- levels(train_df$species)

for (sp in species_levels) {
  roc_i <- pROC::roc(
    response = as.numeric(test_df$species == sp),
    predictor = pred_prob[, sp]
  )
  cat("\n", sp, "AUC =", as.numeric(pROC::auc(roc_i)), "\n")
}
#The ROC analysis shows that the Random Forest model has well discriminatory 
#power, with an AUC of 0.882 for Athene noctua and Tyto alba, meaning that, whatever 
#classification threshold is chosen, the model distinguishes between the two species 
#significantly better than a random classification.


############################################################
# 7) Extract variable importance.
############################################################

# Get importance measures from the RF model:
imp_mat <- importance(rf_species) %>% as.data.frame()

# Add a feature column:
imp_mat$feature <- rownames(imp_mat)

# Order by MeanDecreaseGini:
imp_df <- imp_mat %>%
  arrange(desc(MeanDecreaseGini))

# Print ranked variables:
print(imp_df)


############################################################
# 8) Plot variable importance.
############################################################

x11()
# Basic importance plot:
p_imp <- ggplot(
  imp_df,
  aes(
    x = reorder(feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "Variable importance",
    x = "Feature",
    y = "Mean decrease in Gini"
  )

print(p_imp)

# Save the plot:
ggsave(
  "Figures/Owl_variable_importance.png",
  p_imp,
  width = 8,
  height = 6,
  dpi = 300
)
