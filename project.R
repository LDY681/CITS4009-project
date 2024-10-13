# Packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(lime)
library(pROC)
library(cluster)

# Import the datasets
wd <- getwd()
setwd(wd)
df <- read.csv("dataset.csv")
population_df <- read.csv("pop.csv")
gdp_df <- read.csv("gdp.csv")

# 1 Data Exploration
## 1.1 Structure of the dataset
str(df)
summary(df)
head(df)

## 1.2 Data visualization
### Bar chart
df_cause_gather <- df %>% 
  select(-Entity, -Code, -Year) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather(key = "cause", value = "death")
ggplot(df_cause_gather, aes(x = reorder(cause, death), y = death, fill = cause)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Total Deaths by causes",
       x = "Death Cause",
       y = "Total Deaths") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_flip()

### Scatter plot
df_cause <- df %>%
  select(-Entity, -Code, -Year)
png("scatterplot_matrix.png", width = 1920, height = 1080)
pairs(df_cause, main = "Scatterplot Matrix")
dev.off()

### Line plot
df_cause_year_gathered <- df %>%
  select(-Entity, -Code) %>%
  group_by(Year) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  gather(key = "cause", value = "death", -Year)
df_labels <- df_cause_year_gathered %>%
  group_by(cause) %>%
  filter(Year == max(Year))

### line_plot_per_year
ggplot(df_cause_year_gathered, aes(x = Year, y = death, color = cause)) + 
  geom_line(size = 1) + 
  geom_text(data = df_labels, aes(label = cause), hjust = -0.2, size = 3) + 
  labs(title = "Death per Cause Vs Year", 
       x = "Year", 
       y = "Total Deaths") + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  xlim(min(df_cause_year_gathered$Year), max(df_cause_year_gathered$Year) + 1)

# line_plot_per_year_facet
ggplot(df_cause_year_gathered, aes(x = Year, y = death)) + 
  geom_line(size = 1, aes(color = cause)) + 
  facet_wrap(~ cause) +
  labs(title = "Death per Cause Vs Year", 
       x = "Year", 
       y = "Total Deaths") + 
  theme_minimal() + 
  theme(legend.position = "none")

# 2 Data Modelling
## 2.1 Data processing
# Death summarized from WHO dataset
death_summary <- df %>%
  select(Entity, Code, Year, everything()) %>%
  group_by(Entity, Code, Year) %>%
  summarise(across(starts_with("Outdoor.air.pollution"):starts_with("Iron.deficiency"), ~ sum(.x, na.rm = TRUE)),
            Total_deaths = sum(across(starts_with("Outdoor.air.pollution"):starts_with("Iron.deficiency"), ~ sum(.x, na.rm = TRUE)))) %>%
  ungroup()
# Population treated from World Bank Population dataset
population_truncated <- population_df %>%
  select(Country.Name, Country.Code, `X1990`:`X2019`)  # Keep year from 1990–2019
population_long <- population_truncated %>%
  pivot_longer(cols = `X1990`:`X2019`, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(sub("X", "", Year)))  # Remove the "X" and convert Year to integer
# GDP per capita treated from World Bank GDP dataset
gdp_truncated <- gdp_df %>%
  select(Country.Name, Country.Code, `X1990`:`X2019`)  # Keep year columns 1990–2019
gdp_long <- gdp_truncated %>%
  pivot_longer(cols = `X1990`:`X2019`, names_to = "Year", values_to = "GDP_per_capita") %>%
  mutate(Year = as.integer(sub("X", "", Year)))  # Remove the "X" and convert Year to integer
# Join dataset and match by country code and year
merged_data <- death_summary %>%
  inner_join(population_long %>% select(-Country.Name), by = c("Code" = "Country.Code", "Year" = "Year")) %>%
  inner_join(gdp_long %>% select(-Country.Name), by = c("Code" = "Country.Code", "Year" = "Year"))
# Data missing treatment by filling, if cant fill get rid of them
merged_data <- merged_data %>%
  group_by(Code) %>%
  fill(GDP_per_capita, .direction = "downup") %>%
  fill(Population, .direction = "downup") %>%
  mutate(Death_rate = Total_deaths / Population) %>%
  ungroup() %>%
  filter(!is.na(GDP_per_capita) & !is.na(Population))
# Add death_rate_class classifier
threshold <- median(merged_data$Death_rate, na.rm = TRUE)
merged_data <- merged_data %>%
  mutate(Death_rate_class = ifelse(Death_rate > threshold, 1, 0))
# Add continent class to dataset
# Create a dictionary where continents are keys, and arrays of country codes are values
continent_dict <- list(
  Asia = c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "CYP", "GEO", "IND", "IDN", "IRN", 
           "IRQ", "ISR", "JPN", "JOR", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MYS", "MDV", "MNG", "MMR", "NPL", 
           "OMN", "PAK", "PSE", "PHL", "PRK", "QAT", "KOR", "SAU", "SGP", "LKA", "SYR", "TJK", "TLS", "THA", "TUR", 
           "TKM", "ARE", "UZB", "VNM", "YEM"),
  
  Europe = c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
             "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "MDA", "MCO", 
             "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", 
             "CHE", "UKR", "GBR"),
  
  Africa = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "COG", "CIV", "COD", 
             "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", 
             "LBY", "MAR", "MDG", "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", 
             "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "TUN", "TZA", "UGA", "ZMB", "ZWE"),
  
  North_America = c("ATG", "BHS", "BRB", "BLZ", "BMU", "CAN", "CRI", "CUB", "DMA", "DOM", "SLV", "GRD", "GRL", "GTM", "HTI", 
                    "HND", "JAM", "MEX", "NIC", "PAN", "PRI", "KNA", "LCA", "VCT", "TTO", "USA", "VIR"),
  
  South_America = c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", "PER", "SUR", "URY", "VEN"),
  
  Oceania = c("ASM", "AUS", "FJI", "FSM", "GUM", "KIR", "MHL", "MNP", "NRU", "NZL", "PLW", "PNG", "WSM", "SLB", "TON", "TUV", 
              "VUT")
)
continent_df <- do.call(rbind, lapply(names(continent_dict), function(continent) {
  data.frame(Code = continent_dict[[continent]], Continent = continent)
}))
merged_data <- merged_data %>%
  left_join(continent_df, by = c("Code" = "Code"), , relationship = "many-to-many")
# Add continent class
merged_data <- merged_data %>%
  mutate(Continent_class = case_when(
    Continent == "Asia" ~ 1,
    Continent == "Europe" ~ 2,
    Continent == "Africa" ~ 3,
    Continent == "North_America" ~ 4,
    Continent == "South_America" ~ 5,
    Continent == "Oceania" ~ 6
  ))
rows_with_na <- which(rowSums(is.na(merged_data)) > 0)
print(rows_with_na)

## 2.2 Variable Combination Discovery
### 2.2.1 Correlation between GDP, Death_rate, Population
merged_data <- merged_data %>% ungroup() # Ungroup to ditch the Country Code grouper
numeric_vars <- merged_data %>%
  select(-Entity, -Code, -Death_rate, -Continent)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Only death_rate correlation
cor_with_death_rate <- cor_matrix["Death_rate_class", ]
cor_with_death_rate <- cor_with_death_rate[abs(cor_with_death_rate) >= 0.14]
cor_with_death_rate <- sort(abs(cor_with_death_rate), decreasing = TRUE)
print(cor_with_death_rate)

# Add Child.related.death
merged_data <- merged_data %>%
  mutate(Child.related.death = rowSums(select(., Child.wasting, Child.stunting, Non.exclusive.breastfeeding, Discontinued.breastfeeding, Vitamin.A.deficiency), 
                                       na.rm = TRUE))

# 2.2.2 Information Gain
tree_model <- rpart(Death_rate_class ~ Continent_class + GDP_per_capita + Child.related.death + Year + Unsafe.sex, data = merged_data, method = "class")
var_importance <- as.data.frame(varImp(tree_model))
print(var_importance)

### 2.2.3 PCA analysis
numeric_vars_pca <- merged_data %>%
  select(GDP_per_capita, Year, Unsafe.sex, Child.related.death, Continent_class)
# Perform PCA
pca_model <- prcomp(numeric_vars_pca, scale. = TRUE)
# Summary of PCA
summary(pca_model)
# Get the loading scores to interpret the importance of each variable
pca_loading_scores <- pca_model$rotation
print(pca_loading_scores)

-----------------------------------------------------
## 2.3 Data modelling
# Split the data into training (80%) and test (20%) sets
set.seed(123)
n <- nrow(merged_data)
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- merged_data[train_indices, ]
test_data <- merged_data[-train_indices, ]

### Variates from Variable Combination Discovery
# Train the decision tree model
decision_tree_model <- rpart(Death_rate_class ~ Continent_class + GDP_per_capita + Unsafe.sex + Child.related.death + Year, data = train_data, method = "class")
# Train the logistic regression model
logistic_regression_model <- glm(Death_rate_class ~ Child.related.death + Unsafe.sex + Year + GDP_per_capita + Continent_class, data = train_data, family = "binomial")

# View model summary
rpart.plot(decision_tree_model)
summary(logistic_regression_model)

## 2.4 Modelling predictions
# Make predictions with the decision tree model
test_data$predicted_tree_class <- predict(decision_tree_model, test_data, type = "class")
# Make predictions with logistic regression model
test_data$predicted_prob <- predict(logistic_regression_model, test_data, type = "response")
test_data$predicted_lr_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

## 2.5 Using LIME
# Model classifier prediction functions
predict_model.rpart <- function(x, newdata, ...) {
  pred <- predict(x, newdata, type = "prob")
  return(as.data.frame(pred))
}
predict_model.glm <- function(x, newdata, ...) {
  pred <- predict(x, newdata, type = "response")
  data.frame(low = 1 - pred, high = pred)
}
# Create LIME explainers
train_features = train_data %>%
  select(Continent_class, GDP_per_capita, Unsafe.sex, Child.related.death, Year)
explainer_tree <- lime::lime(
  train_features,
  model = decision_tree_model,
  bin_continuous = TRUE,
  n_bins = 10
)
explainer_lr <- lime::lime(
  train_features,
  model = logistic_regression_model,
  bin_continuous = TRUE,
  n_bins = 10
)

# Select test features for explanation
test_features <- test_data %>%
  select(Continent_class, GDP_per_capita, Unsafe.sex, Child.related.death, Year)
test_case <- test_features[c(1,100,200,300,400,500), ]

# Prediction to check if LIME works
# processed_case <- explainer_tree$preprocess(test_case)
# print(processed_case)
# predicted_probs <- predict_model.rpart(explainer_tree$model, processed_case)
# print(predicted_probs)

# Create LIME explanations
explanations_tree <- lime::explain(
  test_case,
  explainer_tree, 
  n_labels = 2,
  n_features = 5
)
explanations_lr <- lime::explain(
  test_features[1:5, ], 
  explainer_lr, 
  n_labels = 2, 
  n_features = 5
)

# View explanations
print(explanations_tree)
print(explanations_lr)

----------------------------------------------
# 2.6 Modeling evaluation
# Get prediction from training set for comparison
train_data$predicted_tree_class <- predict(decision_tree_model, train_data, type = "class")
train_data$predicted_prob <- predict(logistic_regression_model, train_data, type = "response")
train_data$predicted_lr_class <- ifelse(train_data$predicted_prob > 0.5, 1, 0)

# Convert to class
test_data$Death_rate_class <- as.factor(test_data$Death_rate_class)
test_data$predicted_lr_class <- as.factor(test_data$predicted_lr_class)
test_data$predicted_tree_class <- as.factor(test_data$predicted_tree_class)
train_data$Death_rate_class <- as.factor(train_data$Death_rate_class)
train_data$predicted_lr_class <- as.factor(train_data$predicted_lr_class)
train_data$predicted_tree_class <- as.factor(train_data$predicted_tree_class)

# Confusion matrix
# Confusion matrix for decision tree
confusionMatrix(train_data$predicted_tree_class, train_data$Death_rate_class)
confusionMatrix(test_data$predicted_tree_class, test_data$Death_rate_class)

# Confusion matrix for logistic regression
confusionMatrix(train_data$predicted_lr_class, train_data$Death_rate_class)
confusionMatrix(test_data$predicted_lr_class, test_data$Death_rate_class)

# ROC Curve and AUC for Decision Tree
tree_probs <- predict(decision_tree_model, test_data, type = "prob")[,2]
roc_tree <- roc(test_data$Death_rate_class, tree_probs)
plot(roc_tree, main = "ROC for Decision Tree model")
auc_tree <- auc(roc_tree)
print(paste("AUC for Decision Tree:", auc_tree))

# ROC Curve and AUC for Logistic Regression
lr_probs <- predict(logistic_regression_model, test_data, type = "response")
roc_lr <- roc(test_data$Death_rate_class, lr_probs)
plot(roc_lr, main = "ROC for Logistic Regression Model")
auc_lr <- auc(roc_lr)
print(paste("AUC for Logistic Regression:", auc_lr))

---------------------------------------------
# 2.7 Clustering
# Prepare data for clustering
clustering_data <- merged_data %>%
  select(GDP_per_capita, Child.related.death, Unsafe.sex, Year)
clustering_data_scaled <- scale(clustering_data)

# Calculate WSS to find optimal k
wss <- (nrow(clustering_data_scaled)-1)*sum(apply(clustering_data_scaled,2,var))  # Initial WSS
for (i in 2:10) {
  wss[i] <- sum(kmeans(clustering_data_scaled, centers=i)$tot.withinss)
}
# Plot the elbow curve
plot(1:10, wss, type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K", 
     ylab="Total WSS",
     main="Optimal K finding Elbow")

# Fit K-means with the selected number of clusters
set.seed(123)
k_optimal <- 3 # Change this number to see selection of k
cluster_plot <- as.data.frame(prcomp(clustering_data_scaled)$x[, 1:2])  # PC1 and PC2 only
kmeans_model <- kmeans(clustering_data_scaled, centers = k_optimal, nstart = 100)
cluster_plot$cluster <- as.factor(kmeans_model$cluster)

# Shape the convex hull with points
find_convex_hull <- function(df, groups) {
  do.call(rbind, lapply(unique(groups), function(c) {
    f <- subset(df, cluster == c)
    f[chull(f$PC1, f$PC2), ]
  }))
}
hull_data <- find_convex_hull(cluster_plot, kmeans_model$cluster)

# Plot cluster betwwen PC1 and PC2
ggplot(cluster_plot, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  geom_polygon(data = hull_data, aes(group = cluster, fill = cluster), alpha = 0.4) +
  labs(title = sprintf("Clustering Visualization for k = %d", k_optimal))