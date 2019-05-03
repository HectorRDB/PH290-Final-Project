#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=30
#SBATCH --nodes=1

file='R/simulation_5.R'
R CMD BATCH --no-save ${file}.R ${file}.out
