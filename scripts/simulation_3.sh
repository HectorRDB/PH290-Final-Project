#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=15
#SBATCH --nodes=1

file='R/simulation_3'
R --vanilla < ${file}.R > ${file}.Rout
