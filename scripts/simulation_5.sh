#!/bin/bash
#
#$ -cwd
#$ -V
#$ -j y
#$ -S /bin/bash
#

file='R/simulation_5'
R --vanilla < ${file}.R > ${file}.Rout
