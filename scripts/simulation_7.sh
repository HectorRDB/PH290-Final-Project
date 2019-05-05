#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=15
#SBATCH --nodes=1

file='R/simulation_7'
R CMD BATCH --no-save ${file}.R ${file}.out
