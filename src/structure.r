.libPaths(c("/my_packages", .libPaths()))

# install_packages <- function(package_name) {
#   if (!require(package_name, character.only = TRUE)) {
#     install.packages(package_name, lib = "/my_packages", repos = "http://cran.us.r-project.org")
#     library(package_name, character.only = TRUE)
#   }
# }

# # Check and install necessary packages
# necessary_packages <- c("showtext")

# sapply(necessary_packages, install_packages)
library(showtext)
showtext_auto()
font_add("IPAex", "/workspaces/jichitai-anon/ipaexm/ipaexm.ttf")

.get_setting <- function() {
  data <- read_csv("config/setting.csv")
  column_list <- lapply(data, as.character)
  return(column_list)
}

.get_files <- function(filename) {
  file_list <- list.files(path = "data", pattern = paste0("^", filename, ".*\\.csv$"), full.names = TRUE)
}

.check_encofing <- function() {



}


.save_structure <- function(file, header) {
    encoding_list <- c("UTF-8", "Shift-JIS")
    
    for (encoding in encoding_list) {
        result <- tryCatch({
            # Try to read the file with the current encoding
            data <- read_csv(file, locale = locale(encoding = encoding))
            
            # Proceed if no error
            distribution <- table(data[[header]])
            filename <- paste0(sub("\\.csv$", "", basename(file)), "_", header, ".png")
            filename_saved <- paste0("output/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
            png(filename = filename_saved, family = "IPAex")  # ここでフォントを指定
            barplot(distribution, 
                    xlab = filename, 
                    ylab = "Frequency", 
                    col = rainbow(length(distribution)))
            dev.off()
            
            TRUE  # return TRUE if everything went well
        },
        error = function(e) {
            FALSE  # return FALSE if there was an error
        })
        
        # If there was no error, exit the loop
        if (result) break
    }
    
    # If none of the encodings worked, guess the encoding
    if (!result) {
        guessed_encoding <- .guess_file_encofing(file)
        
        # Try to read the file with the guessed encoding
        data <- read_csv(file, locale = locale(encoding = guessed_encoding))
        
        # Proceed if no error
        distribution <- table(data[[header]])
        filename <- paste0(sub("\\.csv$", "", basename(file)), "_", header, ".png")
        filename_saved <- paste0("output/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
        png(filename = filename_saved, family = "IPAex")  # ここでフォントを指定
        barplot(distribution, 
                xlab = filename, 
                ylab = "Frequency", 
                col = rainbow(length(distribution)))
        dev.off()
    }
}



# 精度が低いので使いたくない
.guess_file_encofing <- function(filename) {
    # ファイルのエンコーディングを推測
    guess <- guess_encoding(filename, n_max = 1000)
    # エンコーディングを返す
    return(guess$encoding[1])  # encodingの最初の推測を返す
}


main <- function() {
  setting = .get_setting()
  for (i in seq_along(setting)) {
    for (j in seq_along(setting[[i]])) {
      # If the value is NA or an empty string, skip
      if (is.na(setting[[i]][j]) || setting[[i]][j] == "") {
        next
      }
      file_list = .get_files(names(setting)[i])
      for (k in seq_along(file_list)) {
        .save_structure(file_list[k], setting[[i]][j])
      }
    }
  }
}


main()
