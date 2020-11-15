#!/usr/bin/bash
cd /srv/shiny-server/dashboard/data
Rscript -e 'source("download-prepare-data.R", encoding = "UTF-8")'
