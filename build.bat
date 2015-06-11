@echo off
@echo off
echo Deleting old build...
del *.exe
del *.dcu
del *.~*
del *.bak
del *.ddp
echo Compiling...
dcc32 mozaa.dpr -CG -Q -B+ -$B- -$D- -$A8+ -$Q- -$R- -$L- -$M- -$O+ -$W- -$Y- -$Z1- -$J- -$C- -$U- -$I- -Ic:\coding\mozaa\patches
echo Deleting new dcu....
del *.dcu
del *.ddp
echo Packing...
call upx mozaa.exe 
echo Creating archive...

