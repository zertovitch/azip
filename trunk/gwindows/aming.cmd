rem  Special build for TDM-GCC (formerly MinGW), not GNAT GPL.
rem  This brings in the GMGPL license instead of GPL, which
rem  allows for distributing the executable without the sources.

rem Add TDM-GCC (or MinGW) bin directory in front of the PATH.
rem This is specific your installation of TDM-GCC / MinGW.

call ming_set

cd ..

rem make Ming object directories if not yet existing
md obj_ming
md obj_ming\fast
md obj_ming\debug
copy obj\fast\libwin32ada.a   obj_ming\fast
copy obj\debug\debug.pra      obj_ming\debug
copy obj\debug\libwin32ada.a  obj_ming\debug

cd gwindows

call build "-march=i686"
