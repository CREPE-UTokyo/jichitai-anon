library(readr)
library(dplyr)
library(stringr)  # 追加

# ディレクトリの設定
data_dir <- "/workspaces/jichitai-anon/data"

# 出力ファイル名のプレフィクス
output_prefix <- "combined_"

# ディレクトリ内の全てのファイルをリストアップ
files <- list.files(path = data_dir, pattern = "*.csv", full.names = TRUE)

# ファイルを jyuki と それ以外に分ける
jyuki_files <- grep("jyuki", files, value = TRUE)
other_files <- setdiff(files, jyuki_files)

# 各 jyuki ファイルに対して処理を行う
for (jyuki_file in jyuki_files) {
  # jyuki データを読み込む
  jyuki_data <- read_csv(jyuki_file, show_col_types = FALSE)

  # 年代を取得する
  year <- str_extract(jyuki_file, "\\d+")

  # 各その他のファイルについて処理を行う
  for (other_file in other_files) {
    # ファイル名からファイルタイプを取得
    file_type <- str_extract(other_file, "(?<=/)[^_/]+(?=_)")

    # ファイルが対応する年代のものであれば処理を行う
    if (str_detect(other_file, year)) {
      # データを読み込む
      other_data <- read_csv(other_file, show_col_types = FALSE)

      # "宛名番号"をキーにしてjyukiデータに他のデータを結合
      combined_data <- left_join(jyuki_data, other_data, by = "宛名番号")

      # 結合したデータを保存
      output_file <- paste0(data_dir, "/", output_prefix, file_type, "_", year, ".csv")
      write_csv(combined_data, output_file)
      print(paste0("Saved: ", output_file))
    }
  }
}
