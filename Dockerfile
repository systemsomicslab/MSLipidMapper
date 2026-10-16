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

RUN curl -fsSL -o /tmp/google-chrome.deb \
      https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
  && apt-get update \
  && apt-get install -y --no-install-recommends /tmp/google-chrome.deb \
  && rm -f /tmp/google-chrome.deb \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ENV MSLIPIDMAPPER_BROWSER=/usr/bin/google-chrome

# ---- Dependency metadata copy (cache-friendly) ----
WORKDIR /srv/app
COPY DESCRIPTION /srv/app/
COPY scripts/install_docker_dependencies.R /usr/local/bin/install_docker_dependencies.R

# ---- R package deps install from DESCRIPTION ----
# Fetch matching container binaries in parallel; compile only unavailable ones.
# Control package-level concurrency without multiplying compiler jobs per package.
ARG MSLIPIDMAPPER_BUILD_CPUS=4
RUN MSLIPIDMAPPER_BUILD_CPUS=${MSLIPIDMAPPER_BUILD_CPUS} \
    Rscript /usr/local/bin/install_docker_dependencies.R

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
EXPOSE 8000

HEALTHCHECK --interval=5s --timeout=3s --start-period=20s --retries=20 \
  CMD curl -fsS http://localhost:3838/ >/dev/null || exit 1

CMD ["sh", "/usr/local/bin/start.sh"]
