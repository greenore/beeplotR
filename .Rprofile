#!/usr/bin/Rscript
# Purpose:         Setup R enviroment
# Date:            2014-02-27                                
# Author:          tim.hagmann@baloise.ch
# Data Used:       
# Packages Used:   
# Machine:         X10004122 (Win 32)
# Notes:           
# R Version:       R version 3.0.2 -- "Frisbee Sailing"
################################################################################

# Set Options
options(scipen = 15, stringsAsFactors = F)

# Set library
if(.Platform$OS.type == 'windows'){
  .libPaths('E:/99_Software/R/library_win')

  # If above library does not exist:
  if(length(list.files('E:/99_Software/R/library_win')) == 0 &
  	 length(list.files('C:/Users/b029580/NoBackupData/R/library')) > 0){
  	.libPaths('C:/Users/b029580/NoBackupData/R/library')
  }
}

if(.Platform$OS.type == 'unix'){
  .libPaths('/media/External-HD/99_Software/R/library_unix')  
}
