rm lossDevB/R/*~
Rcmd BUILD lossDevB
cp lossDevB_0.0.1.tar.gz ../..
rm lossDevB_0.0.1.tar.gz
Rcmd INSTALL ../../lossDevB_0.0.1.tar.gz