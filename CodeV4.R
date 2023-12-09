##############################################################################
install.packages("readr")
library(readr)


ProtestersDataSet1 <- read_csv("~/ProtesterData/data/ProtesterDataSet1_final.csv")
View(ProtestersDataSet1)

ProtestersDataSet2 <- read_csv("~/ProtesterData/data/ProtesterDataSet2_final.csv")
View(ProtestersDataSet2)

ProtestersDataSet3 <- read_csv("~/ProtesterData/data/ProtesterDataSet3_final.csv")
View(ProtestersDataSet3)

ProtestersDataSet4 <- read_csv("~/ProtesterData/data/ProtesterDataSet4_final.csv")
View(ProtestersDataSet4)

ProtestersDataSet5 <- read_csv("~/ProtesterData/data/ProtesterDataSet5_final.csv")
View(ProtestersDataSet5)

ProtestersDataSet6 <- read_csv("~/ProtesterData/data/ProtesterDataSet6_final.csv")
View(ProtestersDataSet5)

# Create an empty list to store the datasets
protesters_datasets <- list()

# Specify the number of datasets
num_datasets <- 6

# Loop through each dataset
for (i in 1:num_datasets) {
  # Construct the file path
  file_path <- sprintf("~/ProtesterData/data/ProtesterDataSet%d_final.csv", i)
  
  # Read the dataset
  dataset_name <- sprintf("ProtestersDataSet%d", i)
  assign(dataset_name, read_csv(file_path))
  
  # Store the dataset in the list
  protesters_datasets[[i]] <- get(dataset_name)
  
  # View the dataset
  View(get(dataset_name))
}

# Now, protesters_datasets is a list containing all datasets

##############################################################################################################

# Function to get the ProtesterJustified variable
get_ProtesterJustified <- function(dataset) {
  # Check if the dataset has the column "ProtesterJustified"
  if ("ProtesterJustified" %in% colnames(dataset)) {
    # Extract the column "ProtesterJustified"
    protesterjustified_column <- dataset$ProtesterJustified
    return(protesterjustified_column)
  } else {
    warning("Column 'ProtesterJustified' not found in the dataset.")
    return(NULL)
  }
}

# Create an empty list to store the ProtesterJustified variable from each dataset
protesterjustified_variables <- list()

# Loop through each dataset
for (i in 1:6) {
  # Construct the file path
  file_path <- sprintf("~/ProtesterData/data/ProtesterDataSet%d_final.csv", i)
  
  # Read the CSV file
  dataset_name <- sprintf("ProtestersDataSet%d", i)
  assign(dataset_name, read_csv(file_path))
  
  # Use the function to extract the ProtesterJustified variable
  protesterjustified_var <- get_ProtesterJustified(get(dataset_name))
  
  # Store the variable in the list
  protesterjustified_variables[[dataset_name]] <- protesterjustified_var
}

# Now, protesterjustified_variables is a list containing the ProtesterJustified variable from each dataset

################################################################################################################


