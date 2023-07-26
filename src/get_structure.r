# パッケージをインストールする関数
# install_packages <- function(package_name) {
#   if (!require(package_name, character.only = TRUE)) {
#     install.packages(package_name, lib = "/my_packages", repos = "http://cran.ism.ac.jp/")
#     library(package_name, character.only = TRUE)
#   }
# }

# # Check and install necessary packages
# necessary_packages <- c("styler")



# sapply(necessary_packages, install_packages)
.libPaths(c("/my_packages", .libPaths()))
library(readr)
library(dplyr)
library(showtext)
library(purrr)
showtext_auto()
font_add("IPAex", "/workspaces/jichitai-anon/ipaexm/ipaexm.ttf")
 


.get_setting <- function() {
  data <- read.csv("config/setting_plot.csv", header = FALSE, stringsAsFactors = FALSE)
  result_list <- list()
  for (i in 1:nrow(data)) {
    clean_data <- na.omit(data[i,])
    clean_data <- clean_data[clean_data != ""]
    result_list[[data[i, 1]]] <- clean_data[-1]
  }
  
  return(result_list)
}


.get_files <- function(filename) {
  file_list <- list.files(path = "data", pattern = paste0("^", filename, ".*\\.csv$"), full.names = TRUE)
}

.save_plot <- function(file, header) {
  encoding_list <- c("UTF-8", "Shift-JIS")

  for (encoding in encoding_list) {
    result <- tryCatch(
      {
        # Try to read the file with the current encoding
        data <- read_csv(file, show_col_types = FALSE, locale = locale(encoding = encoding))

        # Proceed if no error
        distribution <- table(data[[header]], useNA = "ifany")  # Add 'useNA = "ifany"' to count NA frequencies
        filename <- paste0(sub("\\.csv$", "", basename(file)), "_", header, ".png")
        filename_saved <- paste0("to_crepe/plot/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
        png(filename = filename_saved, family = "IPAex") # ここでフォントを指定
        barplot(distribution,
          xlab = filename,
          ylab = "Frequency",
          col = rainbow(length(distribution))
        )
        TRUE # return TRUE if everything went well
      },
      error = function(e) {
        FALSE # return FALSE if there was an error
      },
      finally = {
        # Make sure to close the device even if an error occurs
        dev.off()
      }
    )

    # If there was no error, exit the loop
    if (result) break
  }

  # If none of the encodings worked, guess the encoding
  if (!result) {
    guessed_encoding <- .guess_file_encofing(file)

    # Try to read the file with the guessed encoding
    data <- read_csv(file, locale = locale(encoding = guessed_encoding), show_col_types = FALSE)

    # Proceed if no error
    distribution <- table(data[[header]], useNA = "ifany")  # Add 'useNA = "ifany"' to count NA frequencies
    filename <- paste0(sub("\\.csv$", "", basename(file)), "_", header, ".png")
    filename_saved <- paste0("to_crepe/plot/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
    png(filename = filename_saved, family = "IPAex") # ここでフォントを指定
    barplot(distribution,
      xlab = filename,
      ylab = "Frequency",
      col = rainbow(length(distribution))
    )
    dev.off()
  }
}

.guess_file_encoding <- function(filename) {
  guess <- guess_encoding(filename, n_max = 1000)
  return(paste0(guess$encoding, ":", guess$confidence))
}

.create_random_sample <- function(input_file, num_samples = 100) {
  encoding_list <- c("UTF-8", "Shift-JIS")
  for (encoding in encoding_list) {
    result <- tryCatch(
      {
        # Try to read the file with the current encoding
        df <- read_csv(input_file, locale = locale(encoding = encoding)) 
        
        # Create a list to store the samples
        sample_list <- list()
        
        # Generate samples for each column and add them to the list
        for (col in names(df)) {
          sample_list[[col]] <- sample(df[[col]], num_samples, replace = TRUE)
        }

        # Convert the list to a data frame
        random_sample_df <- data.frame(sample_list)
        
        output_file <- paste0("to_crepe/random/", sub("\\.csv$", "", basename(input_file)), "_random.csv")
        write_csv(random_sample_df, output_file) 
        TRUE # return TRUE if everything went well
      },
      error = function(e) {
        FALSE # return FALSE if there was an error
      }
    )

    # If there was no error, exit the loop
    if (result) break
  }

  # If none of the encodings worked, guess the encoding
  if (!result) {
    guessed_encoding <- .guess_file_encoding(input_file)

    df <- read_csv(input_file, locale = locale(encoding = guessed_encoding)) 
    
    # Create a list to store the samples
    sample_list <- list()
    
    # Generate samples for each column and add them to the list
    for (col in names(df)) {
      sample_list[[col]] <- sample(df[[col]], num_samples, replace = TRUE)
    }

    # Convert the list to a data frame
    random_sample_df <- data.frame(sample_list)
    
    output_file <- paste0("to_crepe/random/", sub("\\.csv$", "", basename(input_file)), "_random.csv")
    write_csv(random_sample_df, output_file) 
  }
}


.save_random <- function(filename, header) {
  # Check the file encoding and read the file
  encoding_list <- c("UTF-8", "Shift-JIS")
  for (encoding in encoding_list) {
    result <- tryCatch(
      {
        # Try to read the file with the current encoding
        data <- read_csv(filename, locale = locale(encoding = encoding), show_col_types = FALSE)
        TRUE # return TRUE if everything went well
      },
      error = function(e) {
        FALSE # return FALSE if there was an error
      }
    )

    # If there was no error, exit the loop
    if (result) break
  }
  # If none of the encodings worked, guess the encoding
  if (!result) {
    guessed_encoding <- .guess_file_encofing(filename)
    data <- read_csv(filename, locale = locale(encoding = guessed_encoding), show_col_types = FALSE)
  }

  # Get a random sample of 100 rows or less
  if (nrow(data) > 100) {
    data <- data[sample(nrow(data), 100), ]
  }

  # Prepare the filename for saving
  filename_saved <- paste0("output/random/", sub("\\.csv$", "", basename(filename)), "_random.csv")

  # Check if the file already exists
  if (file.exists(filename_saved)) {
    # If the file exists, read it
    existing_data <- read_csv(filename_saved, show_col_types = FALSE)
    # Add the new column to the existing data
    existing_data[[header]] <- data[[header]]
    # Write the updated data back to the file
    write_csv(existing_data, file = filename_saved)
  } else {
    # If the file doesn't exist, create it with original header name
    new_data <- data.frame(data[[header]])
    names(new_data) <- header
    write_csv(new_data, file = filename_saved)
  }
}



main <- function() {
  setting <- .get_setting()
  walk(names(setting), function(name) {
    column <- setting[[name]]
    column <- column[!is.na(column) & column != ""]
    file_list <- .get_files(name)
    walk(column, function(header) {
      walk(file_list, function(file) {
        # .save_plot(file, header)
        .create_random_sample(file)
      })
    })
  })
}

main()