library(readr)
library(dplyr)
library(stringr)
data_dir <- "/workspaces/jichitai-anon/data"
output_prefix <- "combined/"
files <- list.files(path = data_dir, pattern = "*.csv", full.names = TRUE)
jyuki_files <- grep("jyuki", files, value = TRUE)
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
