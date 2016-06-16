git pull
mkdir ~/.R
cp -a Makevars ~/.R/
cd ..;
tar -cvvzf depmixS4.tar.gz depmixS4
R CMD INSTALL depmixS4.tar.gz 

