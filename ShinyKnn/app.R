# Load necessary libraries
library(fullPage)
library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(primes)
library(mclust)
library(shinyvalidate)
library(fastDummies)
library(progress)


#' KNN model function
#'
#' This function implements the K-nearest neighbors algorithm for classification.
#'
#' @param train_data The training data.
#' @param test_data The test data.
#' @param target_train The target variable in the training data.
#' @param k The number of neighbors to consider (default is 5).
#' @param distance The distance metric to use (default is "euclidean").
#' @return A vector of predictions for the test data.
#' @export
knn <- function(train_data, test_data, target_train, k = 5, distance = "euclidean",minkowski_p=2) {
  tryCatch({
    # Calculate the number of training and test samples
    num_train_samples <- nrow(train_data)
    num_test_samples <- nrow(test_data)
    # Initialize an empty vector to store predictions
    predictions <- character(num_test_samples)
    
    if(distance=="minkowski"){
      dist_matrix <- as.matrix(dist(rbind(as.matrix(train_data), as.matrix(test_data)), method = distance,p=minkowski_p))
    }else{
      # Calculate the distances for all test points at once
      dist_matrix <- as.matrix(dist(rbind(as.matrix(train_data), as.matrix(test_data)), method = distance))
    }
    for (i in 1:num_test_samples) {
      # Get the distances from the test point to all training points
      test_distances <- dist_matrix[num_train_samples + i, 1:num_train_samples]
      # Find the k-nearest neighbors
      nearest_indices <- order(test_distances)[1:k]
      nearest_labels <- target_train[nearest_indices]
      
      # Make a prediction based on the majority class of the k-nearest neighbors
      table_nearest_labels <- table(nearest_labels)
      max_count <- max(table_nearest_labels)
      
      # Check for ties
      if (sum(table_nearest_labels == max_count) == 1) {
        # Only one class has the maximum count, no tie
        predictions[i] <- names(table_nearest_labels[table_nearest_labels == max_count])
      } else {
        # Handle tie by choosing the class with the smallest index
        tie_classes <- names(table_nearest_labels[table_nearest_labels == max_count])
        predictions[i] <- tie_classes[which.min(match(tie_classes, levels(factor(target_train))))]
      }
    }
    
    return(predictions)
  }, error = function(e) {
    error_msg <- paste("Error in knn function:", conditionMessage(e))
    traceback_info <- traceback()
    stop(list(message = error_msg, traceback = traceback_info), call. = FALSE)
  })
}

#' KNN model wrapper
#'
#' This function wraps the KNN model for convenient use.
#'
#' @param train_data The training data.
#' @param target_train The target variable in the training data.
#' @param k The number of neighbors to consider (default is 5).
#' @param distance The distance metric to use (default is "euclidean").
#' @return An object representing the KNN model.
#' @export
knn_model <- function(train_data, target_train, k = 5, distance = "euclidean") {
  model <- list(train_data = train_data, target_train = target_train, k = k, distance = distance)
  class(model) <- "knn_model"
  return(model)
}

#' Predict method for KNN model
#'
#' This function predicts the target variable using the KNN model.
#'
#' @param model The KNN model object.
#' @param new_data The new data for prediction.
#' @return A vector of predictions for the new data.
#' @export
predict.knn_model <- function(model, new_data) {
  return(knn(model$train_data, new_data, model$target_train, k = model$k, distance = model$distance))
}

#' Repeated Cross-Validation Function
#'
#' This function performs repeated cross-validation for KNN classification.
#'
#' @param data The input data.
#' @param target_col The target column in the dataset.
#' @param k_values A vector of positive integers representing different values of k.
#' @param test_size The proportion of the data to use for testing (default is 0.2).
#' @param distance_metric The distance metric to use (default is "euclidean").
#' @param num_folds The number of folds for cross-validation (default is 5).
#' @param Scale Should numeric variables be scaled? (default is TRUE).
#' @return A list containing mean accuracies and predictions for each k value.
#' @export
r_cv <- function(data, target_col, k_values = c(3, 5), test_size = 0.2, distance_metric = "euclidean", num_folds = 5, Scale = TRUE, minkowski_p = 2,progress=FALSE) {
  # Check if data is empty
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }
  
  # Check if target_col is valid
  if (!(target_col %in% names(data))) {
    stop("Target column not found in the dataset.")
  }
  
  # Check if there is enough data for cross-validation
  if (nrow(data) < num_folds) {
    stop("Not enough data for the specified number of folds.")
  }
  
  # Check if k_values are valid
  if (!all(k_values %% 1 == 0 & k_values > 0)) {
    stop("Invalid k_values. Please provide positive integers.")
  }
  
  # Check if distance_metric is valid
  valid_distance_metrics <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")  # Add more if needed
  if (!(distance_metric %in% valid_distance_metrics)) {
    stop('Invalid distance_metric. Please choose a valid metric, choose one of these-
         "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"')
  }
  
  # Handling missing values in k_values
  if (length(k_values) == 0) {
    stop("No values provided for k_values.")
  }
  
  plot_test_data <- list()
  # Initialize variables to store results
  mean_accuracies <- numeric(length(k_values))
  predictions_lists <- list()
  data_target <- data[, target_col]
  data <- data[, !names(data) %in% target_col]
  
  # Check for factors and characters
  if (any(sapply(data, function(x) is.factor(x) || is.character(x)))) {
    # Dummy encode factors
    data <- fastDummies::dummy_cols(data, remove_selected_columns = TRUE)
  }
  
  # Scale numeric variables if required
  if (Scale == TRUE) {
    data <- as.data.frame(scale(data))
  }
  
  data[, target_col] <- data_target
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent :eta",
    total = length(k_values),
    clear = FALSE
  )
  
  for (k in k_values) {
    pb$tick()  # Increment progress bar
    # Initialize variables to store results for the current k
    accuracies <- numeric(num_folds)
    fold_predictions_list <- list()
    fold_plot_test_data <- list()
    for (fold in 1:num_folds) {
      tryCatch({
        # Split data outside the folds for efficiency
        set.seed(123 + fold)
        shuffled_data <- data[sample(nrow(data)), ]
        num_test_samples <- ceiling(nrow(data) * test_size)
        train_data <- shuffled_data[-(1:num_test_samples), ]
        test_data <- shuffled_data[1:num_test_samples, ]
        plot_test <- test_data
        target_train <- train_data[, target_col]
        target_test <- test_data[, target_col]
        train_data <- train_data[, !names(train_data) %in% target_col]
        test_data <- test_data[, !names(test_data) %in% target_col]
        
        # Predict and evaluate on the current fold
        predictions <- knn(train_data, test_data, target_train, distance = distance_metric, k, minkowski_p)
        accuracies[fold] <- mean(predictions == target_test)
        fold_predictions_list[[fold]] <- list(predictions, target_test)
        fold_test_data <- plot_test
        fold_test_data$predictions <- predictions
        fold_plot_test_data[[fold]] <- fold_test_data
      }, error = function(e) {
        cat("Error in fold", fold, ":", conditionMessage(e), "\n")
        accuracies[fold] <- NA
      })
    }
    plot_test_data[[k]] <- fold_plot_test_data
    # Compute and print mean accuracy over all folds for the current k
    mean_accuracy <- mean(accuracies, na.rm = TRUE)
    cat("Mean Accuracy (k =", k, ") over", num_folds, "folds:", mean_accuracy, "\n")
    
    # Store mean accuracy and predictions for k
    mean_accuracies[k] <- mean_accuracy
    predictions_lists[[k]] <- fold_predictions_list
  }
  
  # Close progress bar
  pb$terminate()
  
  return(list(mean_accuracies, predictions_lists, plot_test_data))
}


#' Mode function
#'
#' This function calculates the mode of a vector.
#'
#' @param x A vector.
#' @param na.rm Should missing values be removed? (default is FALSE).
#' @return The mode of the vector.
#' @export
mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  if (length(ux) == 0) {
    return(NA)
  }
  ux[which.max(tabulate(match(x, ux)))]
}

#' KNN imputation function
#'
#' This function performs KNN imputation for missing values in a dataset.
#'
#' @param data The input data.
#' @param k The number of neighbors to consider for imputation.
#' @return The imputed dataset.
#' @export
# KNN imputation function with additional check for infinite distances
knn_imputation <- function(data, k) {
  tryCatch({
    # Get the number of samples
    num_samples <- nrow(data)
    
    # Identify numeric and categorical columns for imputation
    numeric_cols <- sapply(data, is.numeric)
    categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
    for (i in 1:num_samples) {
      # Check for missing values in the test data
      test_point_numeric <- as.numeric(data[i, numeric_cols])  # Only consider numeric columns
      test_point_categorical <- data[i, categorical_cols]  # Keep categorical columns as they are
      missing_values_numeric <- is.na(test_point_numeric)
      missing_values_categorical <- is.na(test_point_categorical)
      
      if (any(missing_values_numeric) || any(missing_values_categorical)) {
        # Convert the selected numeric and categorical columns to matrices
        data_numeric <- as.matrix(data[, numeric_cols, drop = FALSE])
        data_categorical <- as.matrix(data[, categorical_cols, drop = FALSE])
        
        # Calculate Euclidean distances between the test point and all other data points for numeric columns
        distances_numeric <- sqrt(rowSums((data_numeric - test_point_numeric)^2))
        
        # Check if all distances are infinite
        if (all(is.infinite(distances_numeric))) {
          cat("All distances are infinite for data point", i, ". Skipping imputation.\n")
          next  # Skip imputation for this data point
        }
        
        # Calculate Hamming distances between the test point and all other data points for categorical columns
        distances_categorical <- apply(data_categorical, 1, function(row) sum(row != test_point_categorical))
        
        # Combine distances for numeric and categorical variables (you can adjust weights if needed)
        distances <- distances_numeric + distances_categorical
        
        if (all(is.infinite(distances))) {
          stop("All distances are infinite. Check for constant or identical values in the data.")
        }
        
        # Find k-nearest neighbors
        nearest_indices <- order(distances)[1:k]
        
        # Impute missing values in the test data with the mode (most common category) of the nearest neighbors
        for (j in which(missing_values_categorical)) {
          data[i, categorical_cols][j] <- mode(data_categorical[nearest_indices, j], na.rm = TRUE)
        }
        
        # Impute missing values in the test data with the average of the nearest neighbors for numeric columns
        data[i, numeric_cols][missing_values_numeric] <- colMeans(data_numeric[nearest_indices, ], na.rm = TRUE)[missing_values_numeric]
      }
    }
    
    return(data)
  }, error = function(e) {
    cat("Error in knn_imputation function:", conditionMessage(e), "\n")
    cat("Traceback:", conditionCall(e), "\n")
    # Handle the error or exit gracefully based on your requirements
    if (inherits(e, "stop")) {
      cat("Error occurred in knn_imputation function. Details:", conditionMessage(e), "\n")
    }
  })
}




# Define the UI
ui <- fullPage(
  center = TRUE,
  menu = c(
    "Introduction" = "intro",
    "Slides" = "slides",
    "WorkFlow"="work",
    "Libraries" = "lib",
    "Knn" = "knn",
    "R_CV"="r_cv",
    "Imputation"= "imp",
    "Problems"="prob",
    "KNN_play" = "knn_page",
    "End" = "end"
  ),
  # Change the overall theme
  
  fullSection(
    menu = "intro",
    pageContainer(
      tags$img(src = "https://images.unsplash.com/photo-1495592822108-9e6261896da8?q=80&w=2340&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D",
               style = "width:100%;height:100%;position:absolute;top:0;left:0;z-index:-1;filter: brightness(100%);"),
      div(
        h1("K-Nearest Neighbors (KNN)", style = "margin-bottom:8vh;color:#FFFFFF;font-family: 'Arial', sans-serif;font-weight: bold;font-size: 4.5em;margin-top:1vh;"),
        div(
          #h2("Understanding KNN", style = "color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 2em;margin-top:30px;"),
          p("K-Nearest Neighbors (KNN) is a versatile and simple algorithm used for both classification and regression in machine learning. It predicts the classification of a data point based on the majority class of its 'k' nearest neighbors in the feature space. By measuring the distance between data points, KNN finds similarity and makes predictions.", style = "color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 1.8em;margin-bottom:40px;"),
          #h2("Benefits of Creating KNN from Scratch", style = "color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 2em;margin-top:30px;"),
          # p("Creating a KNN algorithm from scratch provides a deep understanding of its inner mechanisms. It allows you to customize the algorithm according to specific business needs, optimize its performance, and gain insights into feature importance. This understanding empowers companies to tackle unique business problems like customer segmentation, recommendation systems, fraud detection, and more, leading to better-informed decisions and innovative solutions.", style = "color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 1.8em;")
        )
      )
    )
  ),
  
  fullSection(
    menu = "slides", # Assigning slides to its own section
    fullSlide(
      pageContainer(
        tags$img(src = "https://images.unsplash.com/photo-1458419948946-19fb2cc296af?q=80&w=2340&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D",
                 style = "width:30%;height:100%;position:absolute;top:0;left:0;z-index:-1;filter: brightness(70%);"),
        h1("Why build KNN from scratch?",style="font-size:5.8em;color:#FFFFFF"),
      )),
    fullSlide(style = "font-weight: bold;",
              h1("Deprecation"),
              p("
When a machine learning library or any package is deprecated, it may no longer receive updates or support. By having your own implementation of KNN, you can reduce your dependency on external packages for this specific algorithm.

Building from scratch allows you to have full control over your codebase. You won't be affected by changes or deprecation in external packages if you're not heavily reliant on them for your core algorithms."),
              style = "background-image:url('https://images.unsplash.com/photo-1700005473024-a570c93bae52?q=80&w=2228&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D');color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 1.8em;padding: 40px;"
    ),
    
    fullSlide(style = "font-weight: bold;",
              h1("Optimization and Customization"),
              p(" Algorithm Optimization: As you build KNN from scratch, you'll likely encounter opportunities for optimization. You can experiment with different data structures for storing training examples, efficient methods for calculating distances, and strategies for handling ties in voting. This can enhance the performance of your implementation."),
              p("Customization: Building from scratch allows you to customize the algorithm to suit specific needs. You can experiment with different distance metrics, weighting schemes, or even implement modifications like a radius-based variant of KNN."),
              style = "background-image:url('https://www.stockvault.net/data/2007/03/01/98017/preview16.jpg');color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 1.8em;padding: 40px;"
    ),
    fullSlide(style = "font-weight: bold;",
              h1("Knowledge"),
              p("Implementing algorithms from scratch is a great way to enhance your programming skills. You'll get hands-on experience in translating theoretical concepts into functional code.
Debugging and Problem-Solving: Building KNN from scratch provides opportunities for debugging and problem-solving. You'll likely encounter challenges along the way, and solving them will deepen your understanding of both the algorithm and programming in general.
The skills you gain from building KNN from scratch can be applied to understanding and implementing other machine learning algorithms. It provides a foundation for tackling more complex models and algorithms."),
              style = "background-image:url('https://images.unsplash.com/photo-1587876931567-564ce588bfbd?w=600&auto=format&fit=crop&q=60&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxzZWFyY2h8MTl8fGtub3dsZWRnZXxlbnwwfHwwfHx8Mg%3D%3D');color:#FFFFFF;font-family: 'Arial', sans-serif;font-size: 1.8em;padding: 40px;width = '100%';height='200%'"
    )
  ),
  
  fullSection(style="background-color:#B1B2FF",
              menu = "work",
              h2("KNN_400 Package",style = "font-weight: bold;"),
              pageContainer(style="margin-right:13%",
                tags$img(src = "https://drive.google.com/uc?export=view&id=1qgTFUkTk5REb4PYMqNVRa7U8-rxtUbbN",height=700,width=1200
                )
              )
  ),
  
  fullSection(
    menu = "lib",
    style="background-color: #7ED7C1",
    fluidRow(style="margin:2%;color:black",
             column(
               width = 6,
               img(src = "https://drive.google.com/uc?export=view&id=1L0VUh9OpQ1jYw9kZm3C7HaheEREE7GHy", width = "100%",height='200%',  # Replace with your image URL
                   align = "center")),
             
             column(style = "text-align: left;",
                    width = 6,
                    h2("Load Required Libraries", style = "font-weight: bold;"),
                    p("This section loads the necessary libraries for parallel processing and dummy encoding."),
                    
                    h4("Imported Libraries", style = "font-weight: bold;"),
                    p("- **doParallel:** A library for parallel processing."),
                    p("- **fastDummies:** A library for efficient dummy encoding of categorical variables."),
                    
                    h2("Detect the Number of Available CPU Cores", style = "font-weight: bold;"),
                    p("The 'detectCores' function is used to identify the number of available CPU cores on the system."),
                    
                    h4("Return Value", style = "font-weight: bold;"),
                    p("An integer representing the number of CPU cores."),
                    
                    h2("Initialize and Register a Parallel Cluster", style = "font-weight: bold;"),
                    p("This section initializes and registers a parallel cluster for parallel processing."),
                    
                    h4("Function Workflow", style = "font-weight: bold;"),
                    p("1. Try to create a parallel cluster using the 'makeCluster' function."),
                    p("2. Register the created cluster for parallel processing using 'registerDoParallel'."),
                    p("3. Handle errors gracefully if cluster creation fails."),
                    p("4. Ensure that the parallel cluster is stopped, even if an error occurs, using 'on.exit'."),
                    
                    h4("Exported Function", style = "font-weight: bold;"),
                    p("The entire process is encapsulated in a 'tryCatch' block, providing error handling."),
                    
             )
             
    )),
  
  fullSection(
    menu = "knn",
    
    fullSlide(style="background-color: #FF8080",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1ThVyJjXDpXu35aWsV_0bBSINULv6CCvu", width = "100%",height='200%',  # Replace with your image URL
                             align = "center")),
                       
                       column(style = "text-align: left;font-size:20px",
                              width = 6,
                              h2("KNN Model Function", style = "font-weight: bold;"),
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **train_data**: Training data used to build the KNN model."),
                              p("- **test_data**: Test data for which predictions are made."),
                              p("- **target_train**: Target variable in the training data."),
                              p("- **k**: Number of neighbors to consider (default is 5)."),
                              p("- **distance**: Distance metric to use (default is 'euclidean')."),
                              
                              h4("Implementation Details", style = "font-weight: bold;"),
                              p("- Distance Calculation: Utilizes the dist function with the specified distance metric."),
                              p("- Majority Voting: Counts occurrences of each class in the k-nearest neighbors. Handles ties by selecting the class with the smallest index."),
                              p("- Error Handling: Uses tryCatch to capture and report errors with relevant information."),
                              
                              h4("Example Usage", style = "font-weight: bold;"),
                              tags$pre("# Example Usage",
                                "train_data <- ...",
                                "test_data <- ...",
                                "target_train <- ...",
                                "predictions <- knn(train_data, test_data, target_train, k = 5, distance = 'euclidean')"
                              ),
                       )
              )),
    
    fullSlide(style="background-color: #7F669D",              
              h2("KNN Workflow",style = "font-weight: bold;"),
              pageContainer(style="margin-right:15%",
                tags$img(src="https://drive.google.com/uc?export=view&id=1m46xcjiEO4HNc3I6hk0iKcN0-OfJaxd0"))
             ),
    fullSlide(style="background-color: #C3F3EC",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1AiT7Yi7jLSEsbjITqNY2wcurnrv8TFeG", width = "100%",height='300%',  # Replace with your image URL
                             align = "center"),
                         div(style = "height:73.5px"),
                         img(src = "https://drive.google.com/uc?export=view&id=1DT5vi8JgsofOPNbH0bSkBzIuve3mK-CI",width = "100%",height='300%',  # Replace with your image URL
                         )),
                       
                       column(style = "text-align: left",
                              width = 6,
                              h2("KNN Model Wrapper", style = "font-weight: bold;"),
                              p("This function wraps the KNN model for convenient use."),
                              
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **train_data:** The training data."),
                              p("- **target_train:** The target variable in the training data."),
                              p("- **k:** The number of neighbors to consider (default is 5)."),
                              p("- **distance:** The distance metric to use (default is 'euclidean')."),
                              
                              h4("Return Value", style = "font-weight: bold;"),
                              p("An object representing the KNN model."),
                              
                              h4("Exported Function", style = "font-weight: bold;"),
                              code("# Exported Function",
                                   "knn_model <- function(train_data, target_train, k = 5, distance = 'euclidean') {",
                                   "  model <- list(train_data = train_data, target_train = target_train, k = k, distance = distance)",
                                   "  class(model) <- 'knn_model'",
                                   "  return(model)",
                                   "}"),
                              
                              h2("Predict Method for KNN Model", style = "font-weight: bold;"),
                              p("This function predicts the target variable using the KNN model."),
                              
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **model:** The KNN model object."),
                              p("- **new_data:** The new data for prediction."),
                              
                              h4("Return Value", style = "font-weight: bold;"),
                              p("A vector of predictions for the new data."),
                              
                              h4("Exported Function", style = "font-weight: bold;"),
                              code("# Exported Function",
                                   "predict.knn_model <- function(model, new_data) {",
                                   "  return(knn(model$train_data, new_data, model$target_train, k = model$k, distance = model$distance))",
                                   "}"),
                              
                       )
              ))
  ),
  fullSection(
    menu = "r_cv",
    
    fullSlide(style="background-color: #DFCCFB",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1c9TSmLq9jjsMmqbR4A-HzUvLtifwezrn", width = "100%",height='200%',  # Replace with your image URL
                             align = "center")),
                       
                       column(style = "text-align: left;font-size:20px",
                              width = 6,
                              h2("Repeated Cross-Validation Function", style = "font-weight: bold;"),
                              h4("This function performs repeated cross-validation for KNN classification.",style = "font-weight: bold;"),
                              
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **data:** The input data."),
                              p("- **target_col:** The target column in the dataset."),
                              p("- **k_values:** A vector of positive integers representing different values of k."),
                              p("- **test_size:** The proportion of the data to use for testing (default is 0.2)."),
                              p("- **distance_metric:** The distance metric to use (default is 'euclidean')."),
                              p("- **num_folds:** The number of folds for cross-validation (default is 5)."),
                              p("- **Scale:** Should numeric variables be scaled? (default is TRUE)."),
                              h4("Additional Considerations", style = "font-weight: bold;"),
                              p("- The function ensures proper error handling, reporting errors during execution."),
                              p("- Numeric variables are scaled to prevent dominance of variables with larger scales."),
                              p("- Dummy encoding is applied for categorical variables, enhancing model performance."),
                           
                       )
              )),
    fullSlide(style="background-color: #146C94", 
              h2("Repeted CV(r_cv()) Workflow",style = "font-weight: bold;color:white"),
              pageContainer(style="margin-right:15%",
                            tags$img(src="https://drive.google.com/uc?export=view&id=1vuHwSKoL8ms17ACd2j-gcnnZTEdyfVEv"))
    ),
    
    fullSlide(style="background-color: #B0D9B1",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1yIWtswjK0trdgB-ltzzk-i_9qWCX9apL", width = "100%",height='200%',  # Replace with your image URL
                             align = "center")
                       ),
                       
                       column(style = "text-align: left;font-size:20px",
                              width = 6,
                              p("- Mean accuracies are computed to assess the overall performance of the KNN model."),
                              p("- The function supports different distance metrics for flexibility in model tuning."),
                              p("- Results include predictions for each k value, allowing for detailed analysis."),
                              
                              h4("Return Value", style = "font-weight: bold;"),
                              p("A list containing mean accuracies and predictions for each k value."),
                              tags$pre(
                                "# Example Usage ",
                                "data <- ... ",
                                "target_col <- ...",
                                "cross_validation <- r_cv(data, target_col, k_values = c(3,5), test_size = 0.2, distance_metric = 'euclidean', num_folds = 5, Scale=TRUE)"),
                              
                       )
              ))
  ),
  fullSection(
    menu = "imp",
    
    fullSlide(style="background-color: #FFDD83",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1kWCFS3XQ5y4HGIC5AYfg7qa3ZG3dvoFC", width = "100%",height='200%',  # Replace with your image URL
                             align = "center")),
                       
                       column(style = "text-align: left;font-size:20px",
                              width = 6,
                              h2("Mode Function", style = "font-weight: bold;"),
                              p("This function calculates the mode of a vector."),
                              
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **x:** A vector."),
                              p("- **na.rm:** Should missing values be removed? (default is FALSE)."),
                              
                              h4("Return Value", style = "font-weight: bold;"),
                              p("The mode of the vector."),
                              
                              h4("Exported Function", style = "font-weight: bold;"),
                              
                              h2("KNN Imputation Function", style = "font-weight: bold;"),
                              p("This function performs KNN imputation for missing values in a dataset."),
                              
                              h4("Function Parameters", style = "font-weight: bold;"),
                              p("- **data:** The input data."),
                              p("- **k:** The number of neighbors to consider for imputation."),
                              
                              h4("Return Value", style = "font-weight: bold;"),
                              p("The imputed dataset."),
                              
                              
                       )
              )),
    
    
    fullSlide(style="background-color: #65647C",              
              h2("KNN Imputation Workflow",style = "font-weight: bold;color:white"),
              pageContainer(style="margin-right:13%",
                            tags$img(src="https://drive.google.com/uc?export=view&id=1ZXF6rTsHk0FFEjpKj0Zln0abZfjPJVwJ",width=1200))
    ),
    fullSlide(style="background-color: #B4E4FF",
              fluidRow(style="margin:2%;color:black",
                       column(
                         width = 6,
                         img(src = "https://drive.google.com/uc?export=view&id=1YgOTW0nboEOSkQYAR2WRoou82pgovaOW", width = "100%",height='200%',  # Replace with your image URL
                             align = "center")
                       ),
                       
                       column(style = "text-align: left;font-size:20px",
                              width = 6,
                              h4("Error Handling", style = "font-weight: bold;"),
                              p("The function utilizes tryCatch to handle potential errors gracefully."),
                              p("- In case of an error, it prints an error message and traceback information."),
                              p("- If the error is of type 'stop', it provides additional details about the error."),
                              p("This ensures robust error reporting and helps diagnose issues during execution."),
                              h2("KNN Imputation Function - Example Usage", style = "font-weight: bold;"),
                              tags$pre("# Example: KNN Imputation",
                                   "data <- data.frame(",
                                   "  numeric_col = c(1, 2, NA, 4, 5),",
                                   "  categorical_col = c('A', 'B', 'A', 'B', NA)",
                                   ")",
                                   "k <- 3",
                                   "imputed_data <- knn_imputation(data, k)",
                                   "print('Original Data:')",
                                   "print(data)",
                                   "print('Imputed Data:')",
                                   "print(imputed_data)"),
                              
                       )))
  ),
  
  
  
  fullSection(style="background-color:#E8ECD6;",
              menu = "prob",
              h1("Challenges in Model Development", style = "font-weight: bold; text-align: center;"),
              
              h2("1. Handling Imbalanced Data", style = "text-align: left; font-weight: bold;margin:1.5%"),
              p("Dealing with imbalanced datasets where the distribution of classes is uneven.", style = "text-align: left;margin:1.5%"),
              p("Ensuring that the model can effectively learn from minority classes and doesn't bias predictions towards the majority class.", style = "text-align: left;margin:1.5%"),
              
              h2("2. Parallel Processing Complexity", style = "text-align: left; font-weight: bold;margin:1.5%"),
              p("Implementing parallel processing using doParallel for enhanced performance while managing potential complexities.", style = "text-align: left;margin:1.5%"),
              p("Ensuring it works seamlessly in different computing environments.", style = "text-align: left;margin:1.5%"),
              
              h2("3. Error Handling Strategy", style = "text-align: left; font-weight: bold;margin:1.5%"),
              p("Building a robust error-handling strategy involves anticipating potential issues during model execution.", style = "text-align: left;margin:1.5%"),
              p("Providing clear, actionable error messages, addressing common pitfalls, checking for edge cases, and logging information to assist users in understanding and resolving errors.", style = "text-align: left;margin:1.5%"),
              
              h2("4. Data Preprocessing", style = "text-align: left; font-weight: bold;margin:1.5%"),
              p("Data preprocessing is crucial for preparing the dataset for the chosen algorithm.", style = "text-align: left;margin:1.5%"),
              p("Handling missing values, encoding categorical variables, and scaling numeric features require careful consideration.", style = "text-align: left;margin:1.5%"),
              p("The challenge is to implement preprocessing steps that enhance the model's performance without introducing biases or distortions.", style = "text-align: left;margin:1.5%"),
              
              h2("5. Tuning Hyperparameters", style = "text-align: left; font-weight: bold;margin:1.5%"),
              p("Tuning hyperparameters, especially the number of neighbors (k) in KNN, involves experimenting with different values to find the optimal configuration.", style = "text-align: left;margin:1.5%"),
              p("Balancing model accuracy on the training set with the ability to generalize to new data is a nuanced challenge.", style = "text-align: left;margin:1.5%"),
              p("It requires iterative testing and validation, addressing potential overfitting or underfitting issues.", style = "text-align: left;margin:1.5%"),
              
  ),
  
  fullSection(style="background-color: #FFE7CE",
              menu = "knn_page",
              fluidRow(style="margin:2%",
                       column(
                         width = 2,
                         selectInput("default_dataset", "Select Default Dataset:",
                                     choices = c("None", "iris", "diabetes")),
                         conditionalPanel(
                           condition = "input.default_dataset == 'None'",
                           fileInput("file", "Upload Dataset (CSV format)")
                         )
                       ),
                       column(
                         width = 2,
                         sliderInput("num_folds", "Select Number of Folds:", min = 2, max = 10, value = 5),
                       ),
                       column(
                         width = 2,
                         sliderInput("k_range", "Select K-value Range:", min = 3, max = 30, value = c(3, 5))
                         
                       ),
                       column(
                         width = 2,
                         selectInput("selected_k_value", "Select K-value:",choices = NULL)
                       ),
                       column(
                         width = 2,
                         sliderInput("test_size", "Select Test Size:", min = 0.1, max = 0.5, value = 0.2)
                       ),
                       column(
                         width = 2,
                         sliderInput("mink_p", "Select Minkowski p:", min = 1, max = 10, value = 2)
                       )
                       
              ),
              
              # Filters row
              fluidRow(style="margin:2%",
                       column(
                         width = 2,
                         selectInput("target_col", "Select Target Column:", choices = NULL)
                       ),
                       
                       column(
                         width = 2,
                         selectInput("distance_metric", "Select Distance Metric:",
                                     choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), selected = "euclidean")
                       ),
                       # Add more columns for other filters
                       column(
                         width = 2,
                         selectInput("x_column", "Select X-axis column:", choices = NULL)
                       ),
                       column(
                         width = 2,
                         selectInput("y_column", "Select Y-axis column:", choices = NULL)
                       ),
                       column(
                         width = 2,
                         selectInput("z_column", "Select Z-axis column:", choices = NULL)
                       ),
                       column(
                         width = 2,
                         actionButton("submit_button", "Submit")
                       )
                       # Add more columns for additional filters
              ),
              fluidRow(style="margin:2%",
                       column(
                         width = 8,
                         plotlyOutput(outputId = "knn_plot",height="180%")
                       ),
                       column(
                         width = 4,
                         plotlyOutput(outputId = "accuracy_plot",height="180%"),
                       )
              ),
  ),
  fullSection(style="background-color:#FFB085",
              menu = "end",
              h2("Thank You",style = "font-weight: bold;font-size:60px"),
              p(style="font-size:20px",icon("github"),
                "Visit the GitHub repository for Knn_400 package",
                tags$a(href = "https://github.com/rahilfaizan/Knn_400", "Knn_400 Repository")),
              p(style="font-size:20px",icon("github"),
              "Visit the GitHub repository for Shiny App:",
              tags$a(href = "https://github.com/rahilfaizan/Knn_shiny", "Knn_shiny Repository"))
  )
  
)

# Shiny Server Function
server <- function(input, output, session) {
  # Reactive value to store the selected dataset
  data <- reactiveVal(NULL)
  
  # Event handler for selecting a default dataset
  observeEvent(input$default_dataset, {
    if (input$default_dataset != "None") {
      dataset <- switch(input$default_dataset,
                        "iris" = iris,
                        "diabetes" = diabetes  # Add other default datasets here
      )
      data(dataset)
    } else {
      data(NULL)
    }
  })
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      df <- read.csv(input$file$datapath)
      data(df)
    }
  })
  # Observer for updating select inputs based on the loaded dataset
  observe({
    if (!is.null(data())) {
      prime_k_range <- generate_primes(input$k_range[1], input$k_range[2])
      columns <- names(data())
      updateSelectInput(session, "selected_k_value", choices = prime_k_range)
      updateSelectInput(session, "target_col", choices = columns)
      updateSelectInput(session, "x_column", choices = columns)
      updateSelectInput(session, "y_column", choices = columns)
      updateSelectInput(session, "z_column", choices = c("None", columns))
    }
  })
  
  # Event handler for submitting the form
  observeEvent(input$submit_button, {
    # Input validation using shinyvalidate package
    iv <- InputValidator$new()
    iv$add_rule("target_col", sv_required())
    iv$add_rule("x_column", sv_required())
    iv$add_rule("y_column", sv_required())
    iv$add_rule("x_column", ~if (length(unique(c(input$x_column, input$y_column, input$z_column, input$target_col))) != 4) "Please select distinct columns for X, Y, and Z.")
    
    iv$enable()
    
    # Check if a dataset is selected
    if (is.null(data())) {
      # No dataset selected; handle accordingly
      return()
    }
    
    # Determine if z-axis column is selected
    z_selected <- input$z_column != "None"
    prime_k_range <- generate_primes(input$k_range[1], input$k_range[2])
    
    # Call your custom cross-validation function with the selected parameters and data
    if (iv$is_valid()) {
      withProgress(
        message = 'Running Cross-Validation...',
        value = 0,
        {
          result <- r_cv(
            data(),                   # Data from uploaded file or default dataset
            input$target_col,         # Selected target column
            prime_k_range,
            test_size = input$test_size,
            distance_metric = input$distance_metric,
            num_folds = input$num_folds,
            minkowski_p = input$mink_p,
            # Other parameters as needed based on your function
            # Add progress argument to r_cv
            progress = TRUE
          )
        })
      
      # Extract accuracy data
      acc <- result[[1]][prime_k_range]
      mean_accuracies <- acc
      acc_data <- data.frame(k_values = prime_k_range, accuracy = mean_accuracies)
      
      # Render accuracy plot
      output$accuracy_plot <- renderPlotly({
        min_accuracy <- min(acc_data$accuracy) - 0.01  # Adjust the offset as needed
        
        plot_ly(acc_data, x = ~k_values, y = ~accuracy, type = 'scatter', mode = 'lines+markers') %>%
          add_trace(line = list(color = 'blue', width = 2)) %>%
          layout(
            yaxis = list(title = "Accuracy", tickformat = ".2%", range = c(min_accuracy, 1)),
            title = "Accuracy for each K-value",
            hovermode = 'closest',
            hoverlabel = list(bgcolor = 'white', font = list(family = "Arial", size = 12)),
            annotations = list(
              list(
                x = which.max(acc_data$accuracy),
                y = max(acc_data$accuracy),
                xref = "x",
                yref = "y",
                text = paste("Max Accuracy: ", scales::percent(max(acc_data$accuracy))),
                showarrow = TRUE,
                arrowhead = 7,
                ax = 0,
                ay = -40
              )
            ),
            shapes = list(
              list(
                type = 'line',
                x0 = which.max(acc_data$accuracy),
                x1 = which.max(acc_data$accuracy),
                y0 = min_accuracy,
                y1 = max(acc_data$accuracy),
                line = list(color = 'red', width = 2, dash = 'dash')
              )
            )
          )
      })
      
      # Render KNN plot
      output$knn_plot <- renderPlotly({
        # Get the plot test data
        plot_test_data <- result[[3]]  # Assuming plot test data is in the third element of 'result'
        # Combine all fold test data into a single dataframe
        combined_test_data <- do.call(rbind, plot_test_data[[as.numeric(input$selected_k_value)]])
        
        # Check if combined_test_data is a data frame; if not, convert it to a data frame
        if (!is.data.frame(combined_test_data)) {
          combined_test_data <- as.data.frame(combined_test_data)
        }
        
        # Ensure the columns 'predictions' and 'actual_target' exist in the combined test data
        if (!("predictions" %in% colnames(combined_test_data))) {
          stop("Column 'predictions' does not exist in the test data.")
        }
        if (!(input$target_col %in% colnames(combined_test_data))) {
          stop("Column 'actual_target' does not exist in the test data.")
        }
        
        # Convert predictions and actual_target to a common type for comparison (e.g., character or factor)
        combined_test_data$predictions <- as.character(combined_test_data$predictions)
        combined_test_data$actual_target <- as.character(combined_test_data[, input$target_col])
        
        # Define colors based on correct or incorrect predictions
        combined_test_data$color <- ifelse(combined_test_data$predictions == combined_test_data[, input$target_col], combined_test_data[, input$target_col], 'black')
        
        # Create the scatter plot with color-coded points based on prediction accuracy and tooltips
        if (input$z_column != "None") {
          plot <- plot_ly(combined_test_data, x = ~get(input$x_column), y = ~get(input$y_column), z = ~get(input$z_column),
                          type = 'scatter3d', mode = 'markers',
                          marker = list(size = 6, color = ~color,line = list(color = 'black', width = 1)),
                          text = ~paste("Predicted: ", combined_test_data$predictions, "<br>Actual: ", combined_test_data[, input$target_col])) %>%
            layout(scene = list(xaxis = list(title = input$x_column, automargin = TRUE),
                                yaxis = list(title = input$y_column),
                                zaxis = list(title = input$z_column)
            ), title = paste("Predicted vs Actual Test Points for K=", input$selected_k_value, ", accuracy:", round(result[[1]][as.numeric(input$selected_k_value)],3))) # Adjust width and height as needed
        } else {
          
          plot <- plot_ly(combined_test_data, x = ~get(input$x_column), y = ~get(input$y_column),
                          marker = list(size = 8, color = ~color,line = list(color = 'black', width = 1)),
                          text = ~paste("Predicted: ", combined_test_data$predictions, "<br>Actual: ", combined_test_data[, input$target_col])) %>%
            layout(xaxis = list(title = input$x_column),
                   yaxis = list(title = input$y_column),title = paste("Predicted vs Actual Test Points for K =",input$selected_k_value, ", accuracy =", round(result[[1]][as.numeric(input$selected_k_value)],3)))
          # Adjust width and height as needed
          
        }
        
      })
    }
    
  })
}
# Run the Shiny app
shinyApp(ui, server)
