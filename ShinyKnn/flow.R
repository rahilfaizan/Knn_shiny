library(DiagrammeR)

#knn_flow
# Create a visually appealing flowchart without background color
flowchart_code <- "
digraph Workflow {
  graph [bgcolor=transparent];
  node [shape=box, style=rounded, width=1.5, height=0.6, fontname=helvetica, fontsize=10, color=\"#FFFFFF\", margin=0.2];
  
  CalculateSampleSizes [fillcolor=\"#92CDDC\", style=filled];
  InitializePredictionsVector [fillcolor=\"#A2D8E3\", style=filled];
  CalculateDistances [fillcolor=\"#B3E0E8\", style=filled];
  IterateOverTestSamples [fillcolor=\"#C3EBF2\", style=filled];
  MakePredictions [fillcolor=\"#D4F5FD\", style=filled];
  HandleErrors [fillcolor=\"#E5FAFF\", style=filled];

  {rank=same; CalculateSampleSizes InitializePredictionsVector}
  {rank=same; CalculateDistances IterateOverTestSamples MakePredictions}
  {rank=same; HandleErrors}

  CalculateSampleSizes -> InitializePredictionsVector;
  InitializePredictionsVector -> CalculateDistances;
  CalculateDistances -> IterateOverTestSamples;
  IterateOverTestSamples -> MakePredictions;
  MakePredictions -> HandleErrors;
}
"
flowchart <- grViz(flowchart_code, engine = "dot")

# Display the flowchart
print(flowchart)

#r_cv flow
# Create a flowchart for the second workflow with updated colors
flowchart_code <- "
digraph Workflow {
  graph [bgcolor=transparent];
  node [shape=box, style=rounded, width=1.5, height=1.6, fontname=helvetica, fontsize=30, color=\"#FFFFFF\", margin=0.2];
  
  CheckEmptyInput [fillcolor=\"#FFD966\", style=filled, label=\"1. Check for empty input data\"];
  ValidateTargetColumn [fillcolor=\"#FFD966\", style=filled, label=\"2. Validate target column\"];
  VerifyEnoughData [fillcolor=\"#D7C0AE\", style=filled, label=\"3. Verify enough data for cross-validation\"];
  ValidateKValues [fillcolor=\"#D7C0AE\", style=filled, label=\"4. Validate k_values\"] ;
  ValidateDistanceMetric [fillcolor=\"#99DBF5\", style=filled, label=\"5. Validate distance metric\"] ;
  HandleMissingKValues [fillcolor=\"#99DBF5\", style=filled, label=\"6. Handle missing k_values\"] ;
  InitializeResults [fillcolor=\"#B4A7D6\", style=filled, label=\"7. Initialize results\"] ;
  DummyEncodeVariables [fillcolor=\"#B4A7D6\", style=filled, label=\"8. Dummy encode variables\"] ;
  ScaleNumericVariables [fillcolor=\"#D0F5BE\", style=filled, label=\"9. Scale numeric variables\"] ;
  PerformCrossValidation [fillcolor=\"#D0F5BE\", style=filled, label=\"10. Perform cross-validation\"] ;
  ComputeMeanAccuracy [fillcolor=\"#70AD47\", style=filled, label=\"11. Compute mean accuracy\"] ;
  StoreResults [fillcolor=\"#70AD47\", style=filled, label=\"12. Store results\"] ;

{rank=same; CheckEmptyInput ValidateTargetColumn}
{rank=same; VerifyEnoughData ValidateKValues}
{rank=same;  ValidateDistanceMetric HandleMissingKValues}
{rank=same; InitializeResults DummyEncodeVariables }
{rank=same; ScaleNumericVariables PerformCrossValidation}
  {rank=same; ComputeMeanAccuracy StoreResults}

  CheckEmptyInput -> ValidateTargetColumn;
  ValidateTargetColumn->VerifyEnoughData->ValidateKValues;
  ValidateKValues-> ValidateDistanceMetric -> HandleMissingKValues;
  HandleMissingKValues -> InitializeResults -> DummyEncodeVariables;
  DummyEncodeVariables-> ScaleNumericVariables-> PerformCrossValidation;
  PerformCrossValidation-> ComputeMeanAccuracy -> StoreResults;
}
"
flowchart <- grViz(flowchart_code, engine = "dot")

# Display the flowchart
print(flowchart)




#knn_imp flow
# Create a flowchart with groups in different columns and colors for each group
flowchart_code <- "
digraph Workflow {
  graph [bgcolor=transparent, rankdir=TB];
  node [shape=box, style=rounded, width=1.5, height=1.6, fontname=helvetica, fontsize=30, color=\"#FFFFFF\", margin=0.2];

  subgraph cluster_data_preparation {
    label = \"Data Preparation\";
    color=\"#FFD4B2\";
    rankdir=TB;
    fontsize=50;
    GetNumSamples [fillcolor=\"#FFD4B2\", style=filled, label=\"1. Get the number of samples\", group=cluster_data_preparation] ;
    IdentifyColumns [fillcolor=\"#FFD4B2\", style=filled, label=\"2. Identify columns for imputation\", group=cluster_data_preparation] ;
    IterateSamples [fillcolor=\"#FFD4B2\", style=filled, label=\"3. Iterate over each sample\", group=cluster_data_preparation] ;
  }

  subgraph cluster_missing_check {
    label = \"Missing Value Checks\";
    color=\"#FFF6BD\";
    rankdir=TB;
    fontsize=50;
    CheckMissingNumeric [fillcolor=\"#FFF6BD\", style=filled, label=\"4. Check missing values in numeric columns\", group=cluster_missing_check] ;
    CheckMissingValues [fillcolor=\"#FFF6BD\", style=filled, label=\"7. Check missing values in columns\", group=cluster_missing_check] ;
  }

  subgraph cluster_distance_calculation {
    label = \"Distance Calculation\";
    color=\"#CEEDC7\";
    rankdir=TB;
    fontsize=50;
    ConvertToMatrices [fillcolor=\"#CEEDC7\", style=filled, label=\"8. Convert to matrices for distance\", group=cluster_distance_calculation] ;
    CalculateNumericDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"9. Calculate numeric distances\", group=cluster_distance_calculation] ;
    CheckInfiniteDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"10. Check infinite distances\", group=cluster_distance_calculation] ;
    CalculateCategoricalDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"11. Calculate categorical distances\", group=cluster_distance_calculation] ;
    CombineDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"12. Combine distances\", group=cluster_distance_calculation] ;
  }

  subgraph cluster_knn_imputation {
    label = \"KNN Imputation\";
    color=\"#86C8BC\";
    rankdir=TB;
    fontsize=50;
    FindKNearestNeighbors [fillcolor=\"#86C8BC\", style=filled, label=\"13. Find k-nearest neighbors\", group=cluster_knn_imputation] ;
    ImputeCategorical [fillcolor=\"#86C8BC\", style=filled, label=\"14. Impute categorical with mode\", group=cluster_knn_imputation] ;
    ImputeNumeric [fillcolor=\"#86C8BC\", style=filled, label=\"15. Impute numeric with average\", group=cluster_knn_imputation] ;
  }

  GetNumSamples -> IdentifyColumns -> IterateSamples;
  CheckMissingNumeric -> CheckMissingValues;
  ConvertToMatrices -> CalculateNumericDistances -> CheckInfiniteDistances -> CalculateCategoricalDistances -> CombineDistances;
  FindKNearestNeighbors -> ImputeCategorical -> ImputeNumeric;
}
"
flowchart <- grViz(flowchart_code, engine = "dot")

# Display the updated flowchart
print(flowchart)



#knn_400 package
# Create a flowchart with groups in different columns and colors for each group
flowchart_code <- "
digraph Workflow {
  graph [bgcolor=transparent, rankdir=TB];
  node [shape=box, style=rounded, width=1.5, height=1.6, fontname=helvetica, fontsize=30, color=\"#FFFFFF\", margin=0.2];

  subgraph cluster_data_preparation {
    label = \"Data Preparation\";
    color=\"#FFD4B2\";
    rankdir=TB;
    fontsize=50;
    GetNumSamples [fillcolor=\"#FFD4B2\", style=filled, label=\"1. Get the number of samples\", group=cluster_data_preparation] ;
    IdentifyColumns [fillcolor=\"#FFD4B2\", style=filled, label=\"2. Identify columns for imputation\", group=cluster_data_preparation] ;
    IterateSamples [fillcolor=\"#FFD4B2\", style=filled, label=\"3. Iterate over each sample\", group=cluster_data_preparation] ;
  }

  subgraph cluster_missing_check {
    label = \"Missing Value Checks\";
    color=\"#FFF6BD\";
    rankdir=TB;
    fontsize=50;
    CheckMissingNumeric [fillcolor=\"#FFF6BD\", style=filled, label=\"4. Check missing values in numeric columns\", group=cluster_missing_check] ;
    CheckMissingValues [fillcolor=\"#FFF6BD\", style=filled, label=\"7. Check missing values in columns\", group=cluster_missing_check] ;
  }

  subgraph cluster_distance_calculation {
    label = \"Distance Calculation\";
    color=\"#CEEDC7\";
    rankdir=TB;
    fontsize=50;
    ConvertToMatrices [fillcolor=\"#CEEDC7\", style=filled, label=\"8. Convert to matrices for distance\", group=cluster_distance_calculation] ;
    CalculateNumericDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"9. Calculate numeric distances\", group=cluster_distance_calculation] ;
    CheckInfiniteDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"10. Check infinite distances\", group=cluster_distance_calculation] ;
    CalculateCategoricalDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"11. Calculate categorical distances\", group=cluster_distance_calculation] ;
    CombineDistances [fillcolor=\"#CEEDC7\", style=filled, label=\"12. Combine distances\", group=cluster_distance_calculation] ;
  }

  subgraph cluster_knn_imputation {
    label = \"KNN Imputation\";
    color=\"#86C8BC\";
    rankdir=TB;
    fontsize=50;
    FindKNearestNeighbors [fillcolor=\"#86C8BC\", style=filled, label=\"13. Find k-nearest neighbors\", group=cluster_knn_imputation] ;
    ImputeCategorical [fillcolor=\"#86C8BC\", style=filled, label=\"14. Impute categorical with mode\", group=cluster_knn_imputation] ;
    ImputeNumeric [fillcolor=\"#86C8BC\", style=filled, label=\"15. Impute numeric with average\", group=cluster_knn_imputation] ;
  }

  GetNumSamples -> IdentifyColumns -> IterateSamples;
  CheckMissingNumeric -> CheckMissingValues;
  ConvertToMatrices -> CalculateNumericDistances -> CheckInfiniteDistances -> CalculateCategoricalDistances -> CombineDistances;
  FindKNearestNeighbors -> ImputeCategorical -> ImputeNumeric;
}
"
flowchart <- grViz(flowchart_code, engine = "dot")

# Display the updated flowchart
print(flowchart)



# Create a new flowchart with the provided groups and nodes
new_flowchart_code <- "
digraph Workflow {
  graph [bgcolor=transparent, rankdir=TB];
  node [shape=box, style=rounded, width=2.5, height=1.6, fontname=helvetica, fontsize=30, color=\"#FFFFFF\", margin=0.2];

  subgraph cluster_knn {
    label = \"KNN Group\";
    color=\"#CDFCF6\";
    fontsize=50;
    Knn [fillcolor=\"#CDFCF6\", style=filled, label=\"Knn()\", group=cluster_knn];
    Knn_model_wrapper [fillcolor=\"#CDFCF6\", style=filled, label=\"Knn_model_wrapper\", group=cluster_knn];
    knn_model [fillcolor=\"#CDFCF6\", style=filled, label=\"knn_model\", group=cluster_knn];
  }

  subgraph cluster_repeated_cv {
    label = \"Repeated CV Group\";
    color=\"#ECC5FB\";
    fontsize=50;
    r_cv [fillcolor=\"#ECC5FB\", style=filled, label=\"r_cv()\", group=cluster_repeated_cv];
    accuracy [fillcolor=\"#ECC5FB\", style=filled, label=\"accuracy()\", group=cluster_repeated_cv];
    fi_score [fillcolor=\"#ECC5FB\", style=filled, label=\"fi_score()\", group=cluster_repeated_cv];
    precision [fillcolor=\"#ECC5FB\", style=filled, label=\"precision()\", group=cluster_repeated_cv];
    recall [fillcolor=\"#ECC5FB\", style=filled, label=\"recall()\", group=cluster_repeated_cv];
  }

  subgraph cluster_knn_imputation {
    label = \"KNN Imputation Group\";
    color=\"#FF9494\";
    fontsize=50;
    Knn_imputation [fillcolor=\"#FF9494\", style=filled, label=\"Knn_imputation()\", group=cluster_knn_imputation];
    mode [fillcolor=\"#FF9494\", style=filled, label=\"mode()\", group=cluster_knn_imputation];
  }
Knn->Knn_model_wrapper->knn_model
r_cv->accuracy->fi_score->precision->recall
Knn_imputation->mode
}
"
new_flowchart <- grViz(new_flowchart_code, engine = "dot")

# Display the updated flowchart
print(new_flowchart)
