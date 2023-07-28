.libPaths(c("/my_packages", .libPaths()))
library(readr)
library(dplyr)
library(purrr)


.merge_file <- function(){
  file_name <- "config/setting_anon.csv"
  data <- read_csv(file_name, show_col_types = FALSE, col_names = FALSE, locale = locale(encoding = "UTF-8"))
  key <- "住民基本台帳"
  result <- data[data$X1 == key,"X2"]
  
  return(result)
}

.get_files <- function(filename) {
  file_list <- list.files(path = "data", pattern = paste0("^", filename, ".*\\.csv$"), full.names = TRUE)
}

.get_merge_file <- function(year){
  base_file <- paste0("to_crepe/random/jyuki_", year, "_random.csv")
  df_base <- read_csv(base_file, show_col_types = FALSE, col_names = TRUE, locale = locale(encoding = "UTF-8"))
  base_column_names <- colnames(df_base)
  files_to_merge <- c("allkazei", "fuka1", "fuka2", "kokuzei") %>% 
    map_chr(~paste0("to_crepe/random/", ., "_", year, "_random.csv"))
  for(file in files_to_merge){
    df_temp <- read_csv(file, show_col_types = FALSE, col_names = TRUE, locale = locale(encoding = "UTF-8"))
    print(paste0("Columns in '", file, "':"))
    print(colnames(df_temp))  # Print column names of df_temp
    if (!"宛名番号" %in% colnames(df_temp)) {
      stop(paste0("File '", file, "' does not contain the merge key column '宛名番号'."))
    }
    temp_column_names <- colnames(df_temp)
    duplicate_column_names <- base_column_names[base_column_names %in% temp_column_names]
    duplicate_column_names <- duplicate_column_names[duplicate_column_names != "宛名番号"]
    df_temp <- df_temp[, !colnames(df_temp) %in% duplicate_column_names, drop=FALSE]
    df_base <- left_join(df_base, df_temp, by = "宛名番号")
    print(paste0("Processing file: ", file))
    print("Columns in df_temp after removing duplicates:")
    print(colnames(df_temp))
  }
  return(df_base)
}

# result <- .get_merge_file(2022)
# write.csv(result, "to_crepe/merged/merged_data_2022.csv", row.names = FALSE)

.top_coding <- function(){
  data <- read.csv("to_crepe/random/kokuzei_2022_random.csv")
  data$decade <- floor(data$年齢.1.1./10)*10
  top_coding_threshold <- data %>%
    group_by(decade) %>%
    summarise(threshold = quantile(税額合計, 0.5, na.rm = TRUE))
  new_data <- data %>%
    left_join(top_coding_threshold, by = "decade") %>%
    mutate(new_税額合計 = ifelse(税額合計 > threshold, threshold, 税額合計))
  write.csv(new_data, "to_crepe/top_coded/top_coded_kokuzei_2022.csv", row.names = FALSE)
  print("Done!")
}

.top_coding() 



# main <- function() {
#   result <- merge_file()
#   files <- .get_files(result)
#   print(files)
# }


# main()