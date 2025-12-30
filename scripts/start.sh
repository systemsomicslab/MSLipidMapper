#!/bin/sh
set -e

cd /srv/app

export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export MSLM_RULES_YAML=/srv/app/R/lipid_rules.yaml

# ports 
export SHINY_PORT=3838
export API_PORT=7310

echo "[INFO] Starting MSLipidMapper..."
echo "[INFO] Shiny: http://localhost:${SHINY_PORT}"
echo "[INFO] API  : http://localhost:${API_PORT}"

# 重要:
exec R -q -e "options(shiny.port=as.integer(Sys.getenv('SHINY_PORT','3838')), shiny.host='0.0.0.0'); \
  source('R/run_mslipidmapper_app.R', encoding='UTF-8'); \
  run_mslipidmapper_app(launch.browser=FALSE, api_enable=TRUE, api_host='0.0.0.0', api_port=as.integer(Sys.getenv('API_PORT','7310')), api_public_host='localhost', verbose_api=TRUE)"
