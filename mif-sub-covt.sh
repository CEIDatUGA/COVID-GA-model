#PBS -S /bin/bash
#PBS -q covid19_q
#PBS -N fitcovidbeta1
#PBS -l nodes=1:ppn=30
#PBS -l walltime=24:00:00
#PBS -l mem=60gb
#PBS -M atredennick@west-inc.com
#PBS -m abe

cd $PBS_O_WORKDIR

module load R/3.6.2-foss-2018a-X11-20180131-GACRC

R CMD BATCH -1 ./code/MASTER-RUN-ROUTINE.R