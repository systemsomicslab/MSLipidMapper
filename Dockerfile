## ベースイメージとしてrocker/r-verを使用
FROM bioconductor/bioconductor_docker:RELEASE_3_19

# Update apt-get and install necessary libraries
RUN apt-get update \
    && apt-get install -y libcurl4-openssl-dev libxml2-dev openjdk-11-jdk \
    libcairo2-dev libxt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
# Install R packages
RUN R -e "install.packages(c('shinyjqui','BiocManager','remotes', 'devtools', 'shiny','shinyscreenshot', 'readr', 'shinyAce', 'ggplot2', 'dplyr', 'shinythemes', 'openintro', 'plotly', 'DT', 'ggprism', 'ggbeeswarm', 'shinyFiles', 'stringr', 'shinyBS', 'shinydashboard', 'shinyWidgets', 'tidyr', 'pheatmap', 'grid', 'gridExtra', 'ggeasy', 'ggtext', 'colorspace', 'shinyjqui', 'tidyverse', 'gprofiler2', 'colourpicker', 'shinydashboardPlus', 'jsonlite', 'reshape2'))"
RUN R -e 'BiocManager::install("graph")'
RUN R -e "source('https://install-github.me/dreamRs/esquisse')"

# Install plumber and cyjshiny
RUN R -e "install.packages(c('plumber','cyjShiny','svglite'))"


# 作業ディレクトリを設定

# Rスクリプトをコンテナにコピー
COPY ./app/ui.R /srv/app/
COPY ./app/server.R /srv/app/
COPY ./app/plumber.R /srv/app/
COPY ./app/R /srv/app/modules/
COPY ./app/data /srv/app/pathwaymap/
COPY ./app/svg /srv/app/svg/
COPY ./app/www /srv/app/www/
COPY ./scripts/start.sh /usr/local/bin/start.sh
# スクリプトを実行するためのデフォルトコマンド
RUN chmod +x /usr/local/bin/start.sh

# ポートを公開
EXPOSE 9000
EXPOSE 1028

# コンテナ起動時にシェルスクリプトを実行
CMD ["sh", "/usr/local/bin/start.sh"]