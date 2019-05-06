#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=15
#SBATCH --nodes=1

file='R/simulation_5'
R --vanilla < ${file}.R > ${file}.Rout
