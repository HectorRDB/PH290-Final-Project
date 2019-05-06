#!/bin/bash
#
#$ -cwd
#$ -V
#$ -j y
#$ -S /bin/bash
#

file='R/simulation_4'
R --vanilla < ${file}.R > ${file}.Rout
