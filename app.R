library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(cluster)

# ----------------------------------------------------------------
# Load datasets
df <- read.csv("dataset.csv")
population_df <- read.csv("pop.csv")
gdp_df <- read.csv("gdp.csv")

# ----------------------------------------------------------------
# Data preparation
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
continent_dict <- list(
  Asia = c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "CYP", "GEO", "IND", "IDN", "IRN", "IRQ", "ISR", "JPN", "JOR", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MYS", "MDV", "MNG", "MMR", "NPL", "OMN", "PAK", "PSE", "PHL", "PRK", "QAT", "KOR", "SAU", "SGP", "LKA", "SYR", "TJK", "TLS", "THA", "TUR", "TKM", "ARE", "UZB", "VNM", "YEM"),
  Europe = c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "MDA", "MCO", "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR"),
  Africa = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MAR", "MDG", "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "TUN", "TZA", "UGA", "ZMB", "ZWE"),
  North_America = c("ATG", "BHS", "BRB", "BLZ", "BMU", "CAN", "CRI", "CUB", "DMA", "DOM", "SLV", "GRD", "GRL", "GTM", "HTI", "HND", "JAM", "MEX", "NIC", "PAN", "PRI", "KNA", "LCA", "VCT", "TTO", "USA", "VIR"),
  South_America = c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", "PER", "SUR", "URY", "VEN"),
  Oceania = c("ASM", "AUS", "FJI", "FSM", "GUM", "KIR", "MHL", "MNP", "NRU", "NZL", "PLW", "PNG", "WSM", "SLB", "TON", "TUV", "VUT")
)
continent_df <- do.call(rbind, lapply(names(continent_dict), function(continent) {
  data.frame(Code = continent_dict[[continent]], Continent = continent)
}))
merged_data <- merged_data %>%
  left_join(continent_df, by = c("Code" = "Code")) %>%
  mutate(Continent_class = case_when(
    Continent == "Asia" ~ 1,
    Continent == "Europe" ~ 2,
    Continent == "Africa" ~ 3,
    Continent == "North_America" ~ 4,
    Continent == "South_America" ~ 5,
    Continent == "Oceania" ~ 6
  ))
# Add Child.related.death
merged_data <- merged_data %>%
  mutate(Child.related.death = rowSums(select(., Child.wasting, Child.stunting, Non.exclusive.breastfeeding, Discontinued.breastfeeding, Vitamin.A.deficiency), na.rm = TRUE))

# ----------------------------------------------------------------
# Shiny UI
ui <- fluidPage(
  titlePanel("CITS4009 Dayu Liu - EDA, Modeling and Clustering Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tab", "Choose Analysis Type:", 
                  choices = c("EDA", "Data Modeling", "Clustering")),
      
      # EDA options (based on df dataset)
      conditionalPanel(
        condition = "input.tab == 'EDA'",
        selectizeInput("eda_var", 
                       "Variables for Pair Plot:", 
                       choices = colnames(df)[4:31], 
                       multiple = TRUE, 
                       options = list(maxItems = 2)),
        selectInput("eda_cause", "Cause for Line Plot:",
                    choices = colnames(df)[4:31])
      ),
      
      # Data Modeling options
      conditionalPanel(
        condition = "input.tab == 'Data Modeling'",
        sliderInput("split", "Train/Test Split:", min = 0.5, max = 0.9, value = 0.8),
        checkboxGroupInput("modelling_vars", "Variables for Modeling:",
                           choices = colnames(merged_data)[c(3, 34, 14, 38, 39)], 
                           selected = c("Continent_class", "GDP_per_capita", "Child.related.death", "Unsafe.sex", "Year"))
      ),
      
      # Clustering options
      conditionalPanel(
        condition = "input.tab == 'Clustering'",
        numericInput("k_input", "Number of Clusters:", value = 4, min = 2, max = 5),
        checkboxGroupInput("cluster_vars", "Variables for Clustering:",
                           choices = colnames(merged_data)[c(34, 39, 14, 3)],
                           selected = c("GDP_per_capita", "Child.related.death", "Unsafe.sex", "Year"))
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.tab == 'EDA'",
        tabsetPanel(
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Structure", verbatimTextOutput("structure")),
          tabPanel("Head", verbatimTextOutput("head")),
          tabPanel("Total Deaths by Cause", plotOutput("bar_chart")),
          tabPanel("Line Plot", plotOutput("line_plot")),
          tabPanel("Pair Plot", plotOutput("pair_plot"))
        )
      ),
      
      conditionalPanel(
        condition = "input.tab == 'Data Modeling'",
        tabsetPanel(
          tabPanel("Confusion Matrices", 
                   verbatimTextOutput("confusion_matrix_tree"), 
                   verbatimTextOutput("confusion_matrix_logistic")
          ),
          tabPanel("ROC Curves", 
                   plotOutput("roc_curve_tree"), 
                   plotOutput("roc_curve_logistic")
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.tab == 'Clustering'",
        tabsetPanel(
          tabPanel("WSS Plot", plotOutput("wss_plot")),
          tabPanel("Cluster Visualization", plotOutput("cluster_plot"))
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # EDA Outputs (based on df dataset)
  output$summary <- renderPrint({ summary(df) })
  output$structure <- renderPrint({ str(df) })
  output$head <- renderPrint({ head(df) })
  
  output$bar_chart <- renderPlot({
    df_cause_gather <- df %>%
      select(-Entity, -Code, -Year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      gather(key = "cause", value = "death")
    ggplot(df_cause_gather, aes(x = reorder(cause, death), y = death)) + 
      geom_bar(stat = "identity") + coord_flip() +
      labs(title = "Total Deaths by Cause", x = "Death Cause", y = "Total Deaths")
  })
  
  output$line_plot <- renderPlot({
    cause <- input$eda_cause
    df_cause_year_gathered <- df %>%
      select(Year, all_of(cause)) %>%
      group_by(Year) %>%
      summarise(death = sum(.data[[cause]], na.rm = TRUE))
    
    ggplot(df_cause_year_gathered, aes(x = Year, y = death)) + 
      geom_line() +
      labs(title = paste("Death per Year from", cause), x = "Year", y = "Total Deaths")
  })
  
  output$pair_plot <- renderPlot({
    selected_vars <- input$eda_var
    if(length(selected_vars) > 1) {
      ggplot(df, aes_string(x = selected_vars[1], y = selected_vars[2])) +
        geom_point()
    }
  })
  
  # Data Modeling Outputs
  observeEvent(list(input$modelling_vars, input$split), {
    set.seed(123)
    n <- nrow(merged_data)
    split <- as.numeric(input$split)
    train_indices <- sample(1:n, size = split * n)
    train_data <- merged_data[train_indices, ]
    test_data <- merged_data[-train_indices, ]
    
    # Convert Death_rate_class to factor
    train_data$Death_rate_class <- as.factor(train_data$Death_rate_class)
    test_data$Death_rate_class <- as.factor(test_data$Death_rate_class)
    
    # Ensure Death_rate_class is included in the training data!!!!!
    modelling_data_train <- train_data[, c("Death_rate_class", input$modelling_vars)]
    modelling_data_test <- test_data[, c("Death_rate_class", input$modelling_vars)]
    
    # Train the decision tree model/logistic regression model
    decision_tree_model <- rpart(Death_rate_class ~ ., data = modelling_data_train)
    logistic_regression_model <- glm(Death_rate_class ~ ., data = modelling_data_train, family = binomial)
    
    # Confusion matrix for decision tree/logistic regression
    output$confusion_matrix_tree <- renderPrint({
      test_data$predicted_tree_class <- predict(decision_tree_model, modelling_data_test, type = "class")
      confusionMatrix(test_data$predicted_tree_class, test_data$Death_rate_class)
    })
    output$confusion_matrix_logistic <- renderPrint({
      test_data$predicted_lr_class <- ifelse(predict(logistic_regression_model, modelling_data_test, type = "response") > 0.5, 1, 0)
      confusionMatrix(as.factor(test_data$predicted_lr_class), test_data$Death_rate_class)
    })
    
    # ROC Curve for decision tree/logistic regression
    output$roc_curve_tree <- renderPlot({
      tree_probs <- predict(decision_tree_model, modelling_data_test, type = "prob")[, 2]  # Class probabilities
      roc_tree <- roc(test_data$Death_rate_class, tree_probs)
      plot(roc_tree, main = "ROC for Decision Tree model")
    })
    output$roc_curve_logistic <- renderPlot({
      lr_probs <- predict(logistic_regression_model, modelling_data_test, type = "response")
      roc_logistic <- roc(test_data$Death_rate_class, lr_probs)
      plot(roc_logistic, main = "ROC for Logistic Regression model")
    })
  })
  
  # Clustering Outputs
  output$wss_plot <- renderPlot({
    clustering_data <- merged_data %>% select(all_of(input$cluster_vars))
    clustering_data_scaled <- scale(clustering_data)
    wss <- (nrow(clustering_data_scaled)-1)*sum(apply(clustering_data_scaled, 2, var))
    for (i in 2:10) {
      wss[i] <- sum(kmeans(clustering_data_scaled, centers = i)$tot.withinss)
    }
    plot(1:10, wss, type="b", pch = 19, xlab="Number of Clusters K", ylab="Total WSS", main="Optimal K finding Elbow")
  })
  
  output$cluster_plot <- renderPlot({
    k <- input$k_input
    clustering_data <- merged_data %>% select(all_of(input$cluster_vars))
    clustering_data_scaled <- scale(clustering_data)
    kmeans_model <- kmeans(clustering_data_scaled, centers = k, nstart = 100)
    
    cluster_plot <- as.data.frame(prcomp(clustering_data_scaled)$x[, 1:2])
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
      labs(title = sprintf("Clustering Visualization for k = %d", k))
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)