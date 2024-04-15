# Load packages
library(parallel)
library(foreach)
library(doParallel)



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
