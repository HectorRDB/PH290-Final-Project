#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=30
#SBATCH --nodes=1

file='R/simulation_4'
R CMD BATCH --no-save ${file}.R ${file}.out
