#!/bin/bash
#
#$ -cwd
#$ -V
#$ -j y
#$ -S /bin/bash
#

file='R/simulation_3'
R --vanilla < ${file}.R > ${file}.Rout
