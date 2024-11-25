# This calculates the translational speed of a tropical cyclone's centroid.
# We use the haversine formula to compute the great-circle distance between consecutive latitude-longitude pairs
# Batch processing

library(geosphere) # Load necessary library for geodesic calculations
library(here)      # Defines the root folder as the location of .RProj

# Define input and output folders
DataFolder <- here("data", "test")
OutputFolder <- here("output", "test")

# Create OutputFolder if it doesn't exist
if (!dir.exists(OutputFolder)) {
    dir.create(OutputFolder, recursive = TRUE)
}

# Function to calculate translational speed
calculate_translational_speed <- function(lat1, lon1, lat2, lon2, time_diff) {
    # Convert longitudes to -180 to 180 range for geosphere package compatibility
    lon1 <- ifelse(lon1 > 180, lon1 - 360, lon1)
    lon2 <- ifelse(lon2 > 180, lon2 - 360, lon2)
    
    # Calculate distance using haversine formula (in meters)
    distance <- distHaversine(c(lon1, lat1), c(lon2, lat2))
    
    # Convert time difference from hours to seconds (3 hours = 3*3600 seconds)
    time_diff_seconds <- time_diff * 3600
    
    # Speed in meters per second
    speed <- distance / time_diff_seconds
    return(speed)
}

# Get a list of all .txt files in the DataFolder
file_list <- list.files(DataFolder, pattern = "\\.txt$", full.names = TRUE)

# Process each file
for (file_path in file_list) {
    # Extract file name for output
    file_name <- basename(file_path)
    
    # Read the dataset
    data <- read.table(file_path, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    
    # Name the columns based on the description provided
    colnames(data) <- c("Year", "Month", "TC_number", "Time_step", "Basin_ID", 
                        "Latitude", "Longitude", "Min_pressure", "Max_wind_speed", 
                        "Radius_to_max_winds", "Category", "Landfall", "Distance_to_land")
    
    # Initialize the translational speed column
    data$Translational_speed <- NA
    
    # Iterate over each row to calculate speed (skip the first time step)
    for (i in 2:nrow(data)) {
        if (data$TC_number[i] == data$TC_number[i - 1]) {
            lat1 <- data$Latitude[i - 1]
            lon1 <- data$Longitude[i - 1]
            lat2 <- data$Latitude[i]
            lon2 <- data$Longitude[i]
            time_diff <- data$Time_step[i] - data$Time_step[i - 1]
            
            # Calculate speed and store it in the new column
            data$Translational_speed[i] <- calculate_translational_speed(lat1, lon1, lat2, lon2, time_diff)
        }
    }
    
    # Write the updated dataset to the output folder
    output_path <- file.path(OutputFolder, file_name)
    write.table(data, output_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    cat("Processed and saved:", file_name, "\n")
}

