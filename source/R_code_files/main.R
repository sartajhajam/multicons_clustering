################################################
# STARTING SCRIPT FOR RUNNING THE MCC APPROACH #
################################################

# This implementation is based on the recommended default #
# configuration: MCC Method 2 and Merging Threshold       # 
# parameter set to 0.5.                                   #

#---------------------------------------------------#
# Dataset selection and base clusterings generation #
#---------------------------------------------------#
# Needs manual adjustment to generate an ensemble of
# base clusterings
source("base_clusterings.R")   

#------------------------------------------#
# Creation of the binary membership matrix #
#------------------------------------------#
source("binary_membership_matrix.R") 

#---------------------------------------------------#
# Generate clustering patterns from closed patterns #
#---------------------------------------------------#
source("closed_patterns.R")

#---------------------------------------------------#
# Generate the consensuses from clustering patterns #
#---------------------------------------------------#
MT <- 0.5; source("algorithm_2.R")

#------------------------------------------------#
# Generate the ConsTree graphical representation #
#------------------------------------------------#
source("build_ConsTree.R")

#--------------------------------------------------------#
# Generate the refined ConsTree graphical representation #
#--------------------------------------------------------#
source("build_refined_ConsTree.R")

#----------------------------#
# Cleaning the R environment #
#----------------------------#
rm(list=ls())
