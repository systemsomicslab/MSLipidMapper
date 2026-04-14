FROM bioconductor/bioconductor_docker:RELEASE_3_19

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# ---- OS deps ----
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    git make g++ gfortran \
    libcurl4-openssl-dev libxml2-dev libssl-dev \
    libcairo2-dev libxt-dev \
    libfontconfig1-dev libfreetype6-dev \
    libharfbuzz-dev libfribidi-dev \
    libpng-dev libjpeg-dev libtiff5-dev \
    libglpk-dev \
    libglib2.0-0 \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# ---- Dependency metadata copy (cache-friendly) ----
WORKDIR /srv/app
COPY DESCRIPTION NAMESPACE LICENCE README.md /srv/app/

# ---- R package deps install from DESCRIPTION ----
# Install only required package dependencies so optional Suggests do not
# pull in incompatible visualization stacks during image build.
RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
             install.packages(c('BiocManager', 'remotes')); \
             options(repos = BiocManager::repositories()); \
             remotes::install_deps('.', dependencies = c('Depends', 'Imports', 'LinkingTo'), upgrade = 'never')"

# ---- App copy ----
COPY . /srv/app

# ---- Local package install ----
RUN R CMD INSTALL /srv/app

# ---- start script ----
COPY ./scripts/start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh \
    && sed -i 's/\r$//' /usr/local/bin/start.sh

EXPOSE 3838
EXPOSE 7310

HEALTHCHECK --interval=5s --timeout=3s --start-period=20s --retries=20 \
  CMD curl -fsS http://localhost:3838/ >/dev/null || exit 1

CMD ["sh", "/usr/local/bin/start.sh"]
