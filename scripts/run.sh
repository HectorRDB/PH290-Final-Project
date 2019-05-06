#!/bin/bash
#
#$ -cwd
#$ -V
#$ -j y
#$ -S /bin/bash
#

file=
R --vanilla < ${file}.R > ${file}.Rout
