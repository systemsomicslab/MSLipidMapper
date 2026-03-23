#!/bin/sh
set -e

export LANG=C.UTF-8
export LC_ALL=C.UTF-8

export SHINY_PORT=3838
export API_PORT=7310

echo "[INFO] Starting MSLipidMapper..."
echo "[INFO] Shiny: http://localhost:${SHINY_PORT}"
echo "[INFO] API  : http://localhost:${API_PORT}"

exec R -q -e "options(shiny.port=as.integer(Sys.getenv('SHINY_PORT','3838')), shiny.host='0.0.0.0'); \
  MSLipidMapper::run_mslipidmapper(launch.browser=FALSE, host='0.0.0.0', port=as.integer(Sys.getenv('SHINY_PORT','3838')), api_enable=TRUE, api_host='0.0.0.0', api_port=as.integer(Sys.getenv('API_PORT','7310')), api_public_host='localhost')"
