#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

file=
R CMD BATCH --no-save ${file}.R ${file}.out
NA
file='R/simulation_7'
