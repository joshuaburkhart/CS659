#!/bin/bash

cat clustering_data.txt | tr -s ' ' | sed 's/^ //g' > clean_clustering_data.txt
