rm lossDev/R/*~
Rcmd BUILD lossDev
cp lossDev_0.7.0-1.tar.gz ../..
rm lossDev_0.7.0-1.tar.gz
Rcmd INSTALL ../../lossDev_0.7.0-1.tar.gz