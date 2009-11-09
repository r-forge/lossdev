setlocal
PATH=C:\Program Files\R\R-2.10.0\bin\;%PATH%
rm lossDev/R/*~
Rcmd build --binary lossDev
cp lossDev_0.7.0-1.zip ../..
rm lossDev_0.7.0-1.zip
