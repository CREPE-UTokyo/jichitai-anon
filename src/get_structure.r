lib_path <- file.path(getwd(), "my_packages")
.libPaths(c(lib_path, .libPaths()))
library(readr)
library(dplyr)
library(showtext)
library(purrr)
library(crayon)
showtext_auto()
font_add("IPAex",file.path(getwd(), "/ipaexm/ipaexm.ttf"))
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


.log_error <- function(error_message, filename = "error.log") {
  log_path <- file.path("to_crepe", "logs", filename)
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  con <- file(log_path, "a")
  write(paste(Sys.time(), error_message), con)
  close(con)
}


.save_plot <- function(file, header) {
  encoding <- ("UTF-8")
  tryCatch(
    {
      data <- read_csv(file, show_col_types = FALSE, locale = locale(encoding = encoding))
      distribution <- table(data[[header]], useNA = "ifany")
      filename <- paste0(sub("\\.csv$", "", basename(file)), "_", header, ".png")
      filename_saved <- paste0("to_crepe/plot/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
      png(filename = filename_saved, family = "IPAex")
      barplot(distribution,
              xlab = filename,
              ylab = "Frequency",
              col = rainbow(length(distribution))
      )
      dev.off()
      TRUE
    },
    error = function(e) {
      .log_error(conditionMessage(e))
      FALSE
    }
  )
}

.create_random_sample <- function(input_file, num_samples = 1000) {
  encoding <- "UTF-8"
  tryCatch(
    {
      df <- read_csv(input_file, locale = locale(encoding = encoding), show_col_types = FALSE)
      sample_list <- list()
      for (col in names(df)) {
        sample_list[[col]] <- sample(df[[col]], num_samples, replace = TRUE)
      }
      random_sample_df <- data.frame(sample_list)
      output_file <- paste0("to_crepe/random/", sub("\\.csv$", "", basename(input_file)), "_random.csv")
      write_csv(random_sample_df, output_file)
      TRUE
    },
    error = function(e) {
      .log_error(conditionMessage(e))
      FALSE
    }
  )
}




.handle_warnings <- function() {
  if (exists("last.warning")) {
    lapply(last.warning$warnings, function(x) {
      msg <- conditionMessage(x)
      print(msg)
      writeLines(paste(Sys.time(), msg), "warnings.log")
      invokeRestart("muffleWarning")
    })
  }
}



main <- function() {
  tryCatch({
    options(warning.expression = quote(.handle_warnings()))
    
    setting <- .get_setting()
    all_successful <- TRUE

    walk(names(setting), function(name) {
      column <- setting[[name]]
      column <- column[!is.na(column) & column != ""]
      file_list <- .get_files(name)
      walk(column, function(header) {
        walk(file_list, function(file) {
          if (!.save_plot(file, header) || !.create_random_sample(file)) {
            all_successful <<- FALSE
          }
        })
      })
    })

    if (all_successful) {
      cat(green("**************************************************\n"))
      cat(green("********* All tasks completed successfully! *********\n"))
      cat(green("**************************************************\n"))
    } else {
      cat(red("**************************************************\n"))
      cat(red("********* Some tasks failed. Check logs for details. *********\n"))
      cat(red("**************************************************\n"))
    }

  }, error = function(e) {
    .log_error(conditionMessage(e))
    cat(red("**************************************************\n"))
    cat(red("********* An error occurred. Check logs for details. *********\n"))
    cat(red("**************************************************\n"))
  })
}

main()