@echo off

copy %2 %4 
pushd %4 
move /Y %3 %1
popd