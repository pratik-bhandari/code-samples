
# Import eprime data files ------------------------------------------------

reduce_wm <- function(wm_path) {
    experiment_lines <- read_eprime(wm_path)
    experiment_data <- FrameList(experiment_lines)
    experimental_trials <- filter_in(experiment_data, "Procedure", "TestProcDS")
    experimental_trials_df <- to_data_frame(experimental_trials)
}

# Remove outliers (mean plus minus two SD) --------------------------------

msdminus <- function(x){
    a <- mean(x)
    b <- 2*sd(x)
    return(a-b)
}

msdplus <- function(x){
    a <- mean(x)
    b <- 2*sd(x)
    return(a+b)
}
    
