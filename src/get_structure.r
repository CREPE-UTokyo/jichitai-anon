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
library(crayon)
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

.guess_file_encoding <- function(filename) {
  guess <- guess_encoding(filename, n_max = 1000)
  return(paste0(guess$encoding[1]))
}


.get_files <- function(filename) {
  file_list <- list.files(path = "data", pattern = paste0("^", filename, ".*\\.csv$"), full.names = TRUE)
}

.save_plot <- function(file, header) {
  encoding <- "UTF-8"
  result <- tryCatch(
    {
      data <- read_csv(file, show_col_types = FALSE, locale = locale(encoding = encoding))
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
      print(paste0("Created ", filename_saved))
    },
    error = function(e) {
      print(paste0("Error: ", e))
      FALSE # return FALSE if there was an error
    },
    finally = {
      # Make sure to close the device even if an error occurs
      if (dev.cur() != 1) dev.off()
    }
  )
}


.create_random_sample <- function(input_file, num_samples = 1000) {
  encoding <- "UTF-8"
  result <- tryCatch(
    {
      df <- read_csv(input_file, locale = locale(encoding = encoding), show_col_types = FALSE)
      sample_list <- list()
      for (col in names(df)) {
        sample_list[[col]] <- sample(df[[col]], num_samples, replace = TRUE)
      }
      random_sample_df <- data.frame(sample_list)
      output_file <- paste0("to_crepe/random/", sub("\\.csv$", "", basename(input_file)), "_random.csv")
      write_csv(random_sample_df, output_file) 
      TRUE # return TRUE if everything went well
      print(paste0("Created ", output_file))
    },
    error = function(e) {
      FALSE # return FALSE if there was an error
    }
  )
}

.handle_warnings <- function(w) {
  lapply(w$warnings, function(x) {
    msg <- conditionMessage(x)
    print(msg)
    writeLines(paste(Sys.time(), msg), "warnings.log")
    invokeRestart("muffleWarning")
    warning("This is a test warning")
  })
}




main <- function() {
  options(warning.expression = quote(.handle_warnings(last.warning)))
  setting <- .get_setting()
  walk(names(setting), function(name) {
    column <- setting[[name]]
    column <- column[!is.na(column) & column != ""]
    file_list <- .get_files(name)
    walk(column, function(header) {
      walk(file_list, function(file) {
        .save_plot(file, header)
        .create_random_sample(file)
      })
    })
  })

  options(warning.expression = NULL)
  cat(green("**************************************************\n"))
  cat(green("********* All tasks completed successfully! *********\n"))
  cat(green("**************************************************\n"))
}

main()
