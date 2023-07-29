.libPaths(c("/my_packages", .libPaths()))
library(readr)
library(dplyr)
library(purrr)
library(stringr)

.get_value_from_setting <- function(file_name, key){
  if (!file.exists(file_name)) {
    stop(paste0("File does not exist: ", file_name))
  }
  data <- read_csv(file_name, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  key <- "住民基本台帳"
  if (!(key %in% data$setting)) {
    stop(paste0("Key does not exist: ", key))
  }
  result <- data[data$setting == key, "value"]
  if (is.na(result)) {
    stop(paste0("No corresponding value for key: ", key))
  }
  return(result)
}



.get_merged_file <- function(jyuki) {
  data_dir <- "/workspaces/jichitai-anon/data"
  output_prefix <- "combined/"
  files <- list.files(path = data_dir, pattern = "*.csv", full.names = TRUE)
  jyuki_files <- grep(jyuki, files, value = TRUE)
  other_files <- setdiff(files, jyuki_files)
  for (jyuki_file in jyuki_files) {
    jyuki_data <- read_csv(jyuki_file, show_col_types = FALSE)
    year <- str_extract(jyuki_file, "\\d+")
    for (other_file in other_files) {
      file_type <- str_extract(other_file, "(?<=/)[^_/]+(?=_)")
      if (str_detect(other_file, year)) {
        other_data <- read_csv(other_file, show_col_types = FALSE)
        common_names <- intersect(names(jyuki_data), names(other_data))
        common_names <- setdiff(common_names, "宛名番号")
        other_data <- other_data %>% select(-one_of(common_names))
        combined_data <- left_join(jyuki_data, other_data, by = "宛名番号")
        output_file <- paste0(data_dir, "/", output_prefix, file_type, "_", year, ".csv")
        write_csv(combined_data, output_file)
        print(paste0("Saved: ", output_file))
      }
    }
  }
}

main <- function() {
  jyuki <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "住民基本台帳"
  )
  print(jyuki)
  # .get_merged_file(jyuki)
}

main()