# ベースとなるイメージを指定
FROM r-base:latest

# 必要なRパッケージをインストール
RUN R -e "install.packages(c('dplyr', 'ggplot2'), repos='http://cran.rstudio.com/')"
