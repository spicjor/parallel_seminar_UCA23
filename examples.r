# Load packages
library(parallel)
library(foreach)
library(doParallel)
library(palmerpenguins)
library(ranger)
library(ggplot2)
library(dplyr)

# Prepare data-----------------------------------

# Create a vector with 1 billion elements
data <- 1:1e9

# Make a list of 4 of these vectors
data_list <- list("1" = data,
                  "2" = data,
                  "3" = data,
                  "4" = data)



# Make an apply funtion parallel-------------------

# Calculate the "serial" mean
time_benchmark <- system.time(
    lapply(data_list, mean)
)
time_benchmark

# Check how many cores are available
parallel::detectCores()

# Create a local cluster
cl <- parallel::makeCluster(8)

# Calculate the "parallel" mean
time_parallel <- system.time(
    parallel::parLapply(cl,
                        data_list,
                        mean)
)
time_parallel

# Stop the cluster
parallel::stopCluster(cl)



# Running a loop in parallel-----------------------

# Activate the cluster again
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

# Calculate the "parallel" mean using foreach------
time_foreach <- system.time(
    r <- foreach::foreach(i = 1:length(data_list),
                 .combine = rbind) %dopar% {
        mean(data_list[[i]])
    }
)

time_foreach

# Stop the cluster
parallel::stopCluster(cl)



# A real example: Random forest with ranger--------

# Prepare data
penguins <- as.data.frame(
    na.omit(
        penguins[, c("species",
                     "bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g"
        )]
    )
)

# Fit a random forest model
m <- ranger::ranger(
    data = penguins,
    dependent.variable.name = "species",
    importance = "permutation"
)

# Check results
m
m$variable.importance

# Hyperparameter optimization

# Create a data frame with all the possible combinations of hyperparameters
sensitivity.df <- expand.grid(
    num.trees = c(500, 1000, 1500),
    mtry = 2:4,
    min.node.size = c(1, 10, 20)
)

# Create and register cluster
cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)

# Fit models in parallel
prediction.error <- foreach(
    # Iterate over the rows of the hyperparameter data frame
    num.trees = sensitiviy.df$num.trees,
    mtry = sensitiviy.df$mtry,
    min.node.size = sensitiviy.df$min.node.size,
    .combine = "c",
    .packages = "ranger"
) %dopar% {
    # Fit the model
    m.i <- ranger::ranger(
        data = penguins,
        dependent.variable.name = "species",
        num.trees = num.trees,
        mtry = mtry,
        min.node.size = min.node.size
    )
# Return the prediction error
return(m.i$prediction.error * 100)
}

# Add the prediction error to the data frame
sensitivity.df$prediction.error <- prediction.error

# Stop the cluster
parallel::stopCluster(cl)

# Plot the results
ggplot2::ggplot(data = sensitivity.df) +
    ggplot2::aes(
        x = mtry,
        y = as.factor(min.node.size),
        fill = prediction.error
    ) +
    ggplot2::facet_wrap(as.factor(sensitivity.df$num.trees)) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_discrete(breaks = c(1, 10, 20)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::ylab("min.node.size")

# Find the combination with the lower prediction error
best.hyperparameters <- sensitivity.df %>%
    dplyr::arrange(prediction.error) %>%
    dplyr::slice(1)
