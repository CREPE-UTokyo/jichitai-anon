.libPaths(c("/my_packages", .libPaths()))
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(openssl)
library(lubridate)

.get_value_from_setting <- function(file_name, key){
  if (!file.exists(file_name)) {
    stop(paste0("File does not exist: ", file_name))
  }
  data <- read_csv(file_name, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
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
  data_dir <- "data"
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

.get_target_category <- function(target_file, category_arg) {
  data <- read_csv( "config/setting_anon_classification.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  output <- list()
  items <- data %>%
      filter(file_name == target_file, category %in% category_arg) %>%
      pull(item)
    output[[target_file]] <- items
  return(output)
}

.get_target_format <- function(target_file) {
  classification = "config/setting_anon_classification.csv"
  data <- read_csv(classification, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  birth_format <- data %>%
    filter(file_name == target_file, category == "birth") %>%
    pull(format)
  return(birth_format)
}


.get_target_file_name <- function() {
  classification = "config/setting_anon_classification.csv"
  data <- read_csv(classification, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  unique_data <- unique(data$file_name)
  return(unique_data)
}


.apply_hash <- function(file_name, target_columns, password1, password2) {
  if (!file.exists(file_name)) {
    stop(paste0("File does not exist: ", file_name))
  }
  data <- read_csv(file_name, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  for(file in names(target_columns)) {
    for(column in target_columns[[file]]) {
      data[[column]] <- purrr::map_chr(data[[column]], function(x) {
        if(is.na(x)) {
          x
        } else {
          paste0(password1, x, password2) %>%
            openssl::sha256()
        }
      })
    }
  }
  print(
    paste0(
      "Hashed: ",
      file_name,
      " with ",
      paste0(target_columns, collapse = ", ")
    )
  )
  return(data)
}

.get_target_file_year <- function(target_file) {
  data_dir <- "data"
  files <- list.files(data_dir)
  target_files <- grep(paste0("^", target_file, "_\\d{4}\\.csv$"), files, value = TRUE)
  years <- gsub(paste0(target_file, "_|.csv"), "", target_files)
  return(years)
}

# TODO: 生年月日の処理.和暦使ってる場合は更に処理が必要
.apply_month <- function(data, target_file) {
  category_birth = c("birth")
  birth_columns = .get_target_category(target_file, category_birth)
  format_columns = .get_target_format(target_file)
  for (col in birth_columns) {
    if (col %in% colnames(data)) {
      data[[col]] <- as.Date(data[[col]], format_columns) 
      data[[col]] <- format(data[[col]], "%Y/%m")
    }
  }
  return(data) 
}

.save_simple_anon <- function(target_file, year, password1, password2) {
  # TODO: ハッシュ化したいカラムを＋で指定できる
  category_hash = c("atena", "setai")
  hashed_columns = .get_target_category(target_file, category_hash)
  hashed_data <- .apply_hash(
    file_name = paste0("to_crepe/random/", target_file, "_", year, "_random.csv"),
    target_column = hashed_columns,
    password1 = password1,
    password2 = password2
  )
  simple_data = .apply_month(hashed_data, target_file)
  return(simple_data)
}


.apply_top_coding <- function(data) {
  category_hash = c("setai")
  hashed_columns = .get_target_category(target_file, category_hash)
  print(hashed_columns)
}


# .apply_tokui <- function(data, target_file) {
#   category_hash = c("setai")
#   setai_columns = .get_target_category(target_file, category_hash)
#   for (setai_col in setai_columns) {
#     print(setai_col)
#     if (setai_col %in% colnames(data)) {
#       freq_table <- table(data[[setai_col]])
#       freq_10_over <- names(freq_table[freq_table >= 10])
#       data[[setai_col]] <- ifelse(data[[setai_col]] %in% freq_10_over, "same_value", data[[setai_col]])
#     }
#   }
#   return(data)
# }


.get_settings <- function() {
  jyuki <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "住民基本台帳"
  )
  password1 <- .get_value_from_setting(
    file_name = "config/setting_anon_password.csv",
    key = "password1"
  )
  password2 <- .get_value_from_setting(
    file_name = "config/setting_anon_password.csv",
    key = "password2"
  )
  top_coding <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "トップコーディング"
  )
  k_anon <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "k匿名化"
  )
  random_sample <- .get_value_from_setting(
      file_name = "config/setting_anon.csv",
      key = "ランダムサンプリング"
    )
  tokui <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "特異世帯"
  )
  koudo <- .get_value_from_setting(
    file_name = "config/setting_anon.csv",
    key = "高度な匿名化"
  )

  return(list(
    jyuki = jyuki,
    password1 = password1,
    password2 = password2,
    top_coding = top_coding,
    k_anon = k_anon,
    random_sample = random_sample,
    tokui = tokui,
    koudo = koudo
  ))
}

main <- function() {
  settings <- .get_settings()
  target_files <- .get_target_file_name()
  for (target_file in target_files) {
    years = .get_target_file_year(target_file)
    for (year in years) {
      simple_anon = .save_simple_anon(target_file, year, settings$password1, settings$password2)
      # if (settings$jyuki != target_file) {
      #   print("Need merge")
      # }
      
      # if (settings$tokui == "1") {
      #   .apply_tokui(simple_anon, target_file)
      # }
      # if (settings$top_coding == "1") {
      #   .apply_top_coding(simple_anon)
      #   print("Top coding")
      # }
      # if (settings$k_anon == "1") {
      #   print("k-anonymity")
      # }
      # if (settings$random_sample == "1") {
      #   print("Random sampling")
      # }
      # if (settings$koudo == "1") {
      #   print("Koudo")
      # }
      write_csv(simple_anon, paste0("to_crepe/anonymized/", target_file, "_", year, ".csv"))
      print(paste0("Saved: ", target_file, "_", year, ".csv"))
    }   
  }
}

main()