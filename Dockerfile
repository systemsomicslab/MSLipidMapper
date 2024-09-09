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

COPY ./app/ui.R /srv/app/
COPY ./app/server.R /srv/app/
COPY ./app/plumber.R /srv/app/
COPY ./app/R /srv/app/modules/
COPY ./app/data /srv/app/pathwaymap/
COPY ./app/svg /srv/app/svg/
COPY ./app/www /srv/app/www/
COPY ./scripts/start.sh /usr/local/bin/start.sh

RUN chmod +x /usr/local/bin/start.sh

EXPOSE 9000
EXPOSE 7860

CMD ["sh", "/usr/local/bin/start.sh"]
