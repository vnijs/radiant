#!/bin/bash
cd `dirname $0`
Rscript ../../radiant-miniCRAN/dependencies-reinstall.R

Sys.sleep(60)
