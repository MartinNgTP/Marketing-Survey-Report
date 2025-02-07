# Functions

percentage.table <- function(x, digits = 1){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}

# Function to count respondents, calculate percentage, and generate bar plot (and data table)
plot_respondents_distribution <- function(data, group_var) {
  library(data.table)   
  library(ggplot2)     
  library(DT)           
  library(scales)
  
  # Count the number of respondents by the selected group
  counts <- unique(data[, .N, by = group_var])
  
  # Calculate percentage distribution and rounded column
  counts[, percentage := (N / sum(N)) * 100]
  counts[, Percentage := round(percentage, 2)]
  
  if (group_var %in% c(region.name, persona.name, gender.name)) {
    counts <- counts[order(-Percentage)]  # Order by Percentage in descending order
  } else {
    counts <- counts[order(get(group_var))]  # Order by group_var for other cases
  }
  
  clean_group_var <- gsub("_", " ", group_var)
  counts[[group_var]] <- factor(counts[[group_var]], levels = counts[[group_var]])
  
  # Create the bar plot
  plot <- ggplot(counts, aes_string(x = group_var, y = "percentage", fill = group_var)) +
    geom_bar(stat = "identity") +  
    geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4) +  
    labs(title = paste0("Percentage of Respondents by ", clean_group_var),
         y = "Percentage",
         x = clean_group_var) +
    theme_minimal() +  
    theme(panel.grid = element_blank(),   
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.position = "right",
          plot.margin = margin(10, 20, 30, 40),  # Increase left margin to 40
          axis.text.x = if (group_var == persona.name) {
            element_text(angle = 30, hjust = 1)  # Rotate 45 degrees for Persona
          } else {
            element_text(angle = 0)  # No rotation for other variables
          }) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    guides(fill = guide_legend(title = clean_group_var))
  
  setnames(counts, old = names(counts)[1], new = group_var)  
  
  # Return both the plot and data table
  return(list(
    plot = plot,
    table = datatable(counts[, .SD, .SDcols = c(group_var, "Percentage")],
                      rownames = FALSE,
                      options = list(dom = 't', ordering = FALSE))
  ))
}



# Combined function to plot top products by state of engagement with dynamic filtering
plot_top_products <- function(data, metric, region = NULL, gender = NULL, income = NULL, age = NULL, persona = NULL, top_n) {
  
  # Filter data based on the input criteria
  filtered_data <- data
  
  if (!is.null(region)) {
    filtered_data <- filtered_data[get(region.name) == region]
  }
  
  if (!is.null(gender)) {
    filtered_data <- filtered_data[get(gender.name) == gender]
  }
  
  if (!is.null(income)) {
    filtered_data <- filtered_data[get(income.name) >= income]
  }
  
  if (!is.null(age)) {
    filtered_data <- filtered_data[get(age.name) >= age]
  }
  
  if (!is.null(persona)) {
    filtered_data <- filtered_data[get(persona.name) == persona]
  }
  
  # Calculate the selected metric (awareness or advocacy) rates by product
  if (metric == "awareness") {
    rates <- filtered_data[, .(rate = mean(get(awareness.name), na.rm = TRUE)), by = .(Product = get(product.name))]
    rate_label <- "Awareness Rate"
  } else if (metric == "advocacy") {
    rates <- filtered_data[, .(rate = mean(get(advocacy.name), na.rm = TRUE)), by = .(Product = get(product.name))]
    rate_label <- "Advocacy Rate"
  } else if (metric == "consideration") {
    rates <- filtered_data[, .(rate = mean(get(consideration.name), na.rm = TRUE)), by = .(Product = get(product.name))]
    rate_label <- "Consideration Rate"
  } else if (metric == "consumption") {
    rates <- filtered_data[, .(rate = mean(get(consumption.name), na.rm = TRUE)), by = .(Product = get(product.name))]
    rate_label <- "Consumption Rate"
  } else if (metric == "satisfaction") {
    rates <- filtered_data[, .(rate = mean(get(satisfaction.name), na.rm = TRUE)), by = .(Product = get(product.name))]
    rate_label <- "Satisfaction Rate"
  }
  
  # Convert rate to percentage
  rates[, percentage := rate * 100]
  
  # Sort by the selected metric and get the top N products
  top_products <- rates[order(-percentage)][1:top_n]
  
  # Create the bar plot
  plot <- ggplot(top_products, aes(x = reorder(Product, percentage), y = percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.3, size = 5) +
    labs(title = paste("Top", top_n, "Products by", rate_label),
         x = "Product",
         y = paste(rate_label, "(%)")) +
    theme_minimal() + # Set minimal theme
    theme(panel.grid = element_blank()) + # Remove gridlines
    coord_flip()
  
  # Print the result and return the table and plot
  return(list(plot = plot, table = datatable(top_products)))
}

# Define the function for calculating the overall brand perception
calculate_brand_perception <- function(num_products, 
                                       age_group = "all", 
                                       gender = "all", 
                                       income_group = "all", 
                                       region = "all", 
                                       persona = "all") {
  
  # Filter the dataset based on input parameters, with defaults set to "all"
  filtered_data <- dat[]
  
  if (age_group != "all") {
    filtered_data <- filtered_data[Age_Group %in% age_group]
  }
  
  if (gender != "all") {
    filtered_data <- filtered_data[Gender %in% gender]
  }
  
  if (income_group != "all") {
    filtered_data <- filtered_data[Income %in% income_group]
  }
  
  if (region != "all") {
    filtered_data <- filtered_data[Region %in% region]
  }
  
  if (persona != "all") {
    filtered_data <- filtered_data[Persona %in% persona]
  }
  
  # Calculate the average score for each positive and negative trait by product
  positive_scores <- filtered_data[, lapply(.SD, mean, na.rm = TRUE), by = Product, .SDcols = positive_traits]
  negative_scores <- filtered_data[, lapply(.SD, function(x) 10 - mean(x, na.rm = TRUE)), by = Product, .SDcols = negative_traits]
  
  # Combine the positive and negative scores into a single table
  combined_scores <- merge(positive_scores, negative_scores, by = "Product")
  
  # Calculate the overall average perception for each product
  combined_scores[, Overall_Average_Perception := rowMeans(.SD, na.rm = TRUE), .SDcols = c(positive_traits, negative_traits)]
  
  # Rank the products by overall perception and select the top N products
  top_brands <- combined_scores[order(-Overall_Average_Perception)][1:num_products, .(Product, Overall_Average_Perception)]
  
  return(top_brands)
}


top_n_engagement_rates <- function(data, state1, state2, num_products) {
  # Step 1: Calculate the average engagement rates for the two states by product
  engagement_rates <- data[, .(
    first_outcome_rate = mean(get(state1), na.rm = TRUE),
    second_outcome_rate = mean(get(state2), na.rm = TRUE)
  ), by = Product]
  
  # Step 2: Calculate the difference between the two states
  engagement_rates[, difference := 100 * (first_outcome_rate - second_outcome_rate)]
  
  # Step 3: Sort by the largest positive differences and select the top N products
  top_n_products <- engagement_rates[order(-difference)][1:num_products]
  
  # Return the top N products with engagement rate differences
  return(top_n_products)
}

round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}


fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = T)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}

logistic.regression.summary <- function(glm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(glm.coefs[])
}

linear.regression.summary <- function(lm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}
























