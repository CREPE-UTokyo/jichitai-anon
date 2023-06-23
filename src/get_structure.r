.libPaths(c("/my_packages", .libPaths()))
library(readr)


# 必要となるファイル一覧を取得
.get_file_list <- function(directory, pattern) {
  files <- list.files(path = directory, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop(paste("次のパターンのファイルが存在していません'", pattern, "' ファイル名が間違っていないか確認してください。", sep = ""))
  }
  return(files)
}

# TODO:エンコーディングがことなってくる場合の対処
# ヘッダーを取得
.check_header <- function(file_list, headers) {
    for (file in files) {
    # ファイルを読み込む
    data <- read_csv(file, locale = locale(encoding = "shift-jis"), n_max = 0)
    # ヘッダーに"年度"が含まれていない場合、エラーを出す
    for (header in headers) {
        if (!header %in% names(data)) {
            stop(paste("エラー: ファイル", file, "には '", header, "' という単語がヘッダーに含まれていません。"))
        }
    }
    print(paste("ヘッダーの読み取り完了", file))
    }
}


# データ概要報告で必要となるヘッダーを取得
.get_config_headers <- function() {
    settings <- readr::read_csv("config/setting.csv", locale = locale(encoding = "utf-8"))
    headers <- settings$headers
    if (is.null(headers) || length(headers) == 0) {
        stop("No headers found in the settings file.")
    }
    return(headers)
}


.save_plot <- function(file, header) {
    # CSVファイルを読み込む
    data <- read_csv(file, locale = locale(encoding = "shift-jis"))
    # 指定されたヘッダーの分布を計算
    distribution <- table(data[[header]])
    # 分布を表示
    print(distribution)
    # ファイル名を生成
    file_name <- paste0("output/", sub("\\.csv$", "", basename(file)), "_", header, ".png")
    # 画像の保存先とファイル名を指定
    png(filename = file_name)
    # 分布をバープロットで表示（色を付ける）
    barplot(distribution, col = rainbow(length(distribution)))
    # グラフの保存を終了
    dev.off()
}



get_structure <- function() {
    jyuki_list <- .get_file_list("data", "^jyuki_.*\\.csv$")
    config_header <- .get_config_headers()
    .check_header(jyuki_list, config_header)
    for (file in jyuki_list) {
    .save_plot(file, "生年月日")
    .save_plot(file, "性別")
    .save_plot(file, "続柄")
    }

}

get_structure()