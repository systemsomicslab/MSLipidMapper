FROM bioconductor/bioconductor_docker:RELEASE_3_19

# Update apt-get and install necessary libraries
RUN apt-get update \
    && apt-get install -y libcurl4-openssl-dev libxml2-dev openjdk-11-jdk \
    libcairo2-dev libxt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
# Install R packages
RUN R -e "install.packages(c('khroma','ggthemes','igraph','data.table','topOnto.LION.db','topOnto','RSQLite','ggrepel','shinyjqui','BiocManager','bslib','rstatix','ggpubr','remotes', 'devtools', 'shiny','shinyscreenshot', 'readr', 'shinyAce', 'ggplot2', 'dplyr', 'shinythemes', 'openintro', 'plotly', 'DT', 'ggprism', 'ggbeeswarm', 'shinyFiles', 'stringr', 'shinyBS', 'shinydashboard', 'shinyWidgets', 'tidyr', 'pheatmap', 'grid', 'gridExtra', 'ggeasy', 'ggtext', 'colorspace', 'shinyjqui', 'tidyverse', 'gprofiler2', 'colourpicker', 'shinydashboardPlus', 'jsonlite', 'reshape2','shinyalert','here'))"
RUN R -e 'BiocManager::install("graph")'
RUN R -e "source('https://install-github.me/dreamRs/esquisse')"

RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com')"
RUN R -e "install.packages('ggplot2', repos = 'http://cran.rstudio.com')"
RUN R -e "devtools::install_github('martijnmolenaar/topOnto')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/RSQLite/RSQLite_0.11.4.tar.gz', repos = NULL, type = 'source')"
RUN R -e "devtools::install_github('martijnmolenaar/topOnto.LION2.db/topOnto.LION.db')"

# Install plumber and cyjshiny
RUN R -e "install.packages(c('plumber','cyjShiny','svglite'))"

COPY ./app/ui.R /srv/app/
COPY ./app/server.R /srv/app/
COPY ./app/plumber.R /srv/app/
COPY ./app/R /srv/app/modules/
COPY ./app/data /srv/app/pathwaymap/
COPY ./app/svg /srv/app/svg/
COPY ./app/www /srv/app/www/
COPY ./scripts/start.sh /usr/local/bin/start.sh

RUN chmod -R 777 /srv/app 

RUN chmod +x /usr/local/bin/start.sh

EXPOSE 9000
EXPOSE 7860

CMD ["sh", "/usr/local/bin/start.sh"]
