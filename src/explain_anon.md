# 匿名化関数の説明
### `top_coding`関数の解説
`top_coding`関数は、与えられたCSVデータの特定の列（`header`）に対してトップコーディングを行う。トップコーディングとは、指定された％（`percentile`）を超える値を一律に平均値に置き換える手法。この関数は複数のCSVファイル名（`filenames`）を引数として受け取り、それぞれに対してトップコーディングを実施。

1. **ファイルの読み込み**：`read.csv`関数を用いて、指定されたCSVファイルをデータフレーム形式で読み込む。
```r
  for (filename in filenames) {
    data <- read.csv(filename) # Load file
 ```
2. **ヘッダーチェック**：データフレームの列名に指定された`header`が存在するか確認。存在しない場合はエラーメッセージを出力し、関数の実行を停止。
```r
if (!header %in% colnames(data)) stop(paste("Header", header, "not found in file", filename)) # Check header
```
3. **トップコーディングの閾値と平均の計算**：`quantile`関数を用いて、データの指定％に相当する閾値を計算。次に、この閾値を超える値の平均（`top_coded_mean`）を計算。
```r
threshold <- quantile(data[[header]], probs = percentile, na.rm = TRUE)
top_coded_mean <- mean(data[data[[header]] > threshold, header], na.rm = TRUE)
```
4. **トップコーディングの適用**：閾値を超える`header`列の値を`top_coded_mean`に置き換える
```r 
data[data[[header]] > threshold, header] <- top_coded_mean 
```
5. **結果の保存**：トップコーディングを適用した後のデータフレームを、新たなCSVファイルとして書き出す。新しいファイルの名前は、"top_coded_"が元のファイル名の前に付与。
```r 
write.csv(data, paste0("top_coded_", filename)) # Write result to CSV
```
