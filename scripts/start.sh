R -e "library(dplyr)"

R -e "plumber::pr_run(plumber::pr('/srv/app/plumber.R'), port=9000, host='0.0.0.0')" &

R -e "shiny::runApp('/srv/app', host='0.0.0.0', port=1028)"
