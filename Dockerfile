# 基本となるイメージを指定
FROM r-base

# 必要なパッケージをローカルからコピー
COPY ./my_packages /my_packages

# コピーしたパッケージをインストール
RUN R -e "install.packages(list.files('/my_packages', full.names = TRUE), repos = NULL, type = 'source')"

# Rスクリプトをコピー
COPY ./src /src

# その他の設定（例えば作業ディレクトリの指定など）をここに書く
WORKDIR /usr/src/app
