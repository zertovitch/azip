@echo off

rem  Special build for TDM-GCC (formerly MinGW), not GNAT GPL.
rem  This brings in the GMGPL license instead of GPL, which
rem  allows for distributing the executable without the sources.

rem  The call to ming_set.cmd adds TDM-GCC (or MinGW) bin
rem  directory in front of the PATH.
rem  This is specific your installation of TDM-GCC / MinGW.
rem
call ming_set

echo  --- CAUTION ---
echo  The version 5.1.0 of TDM-GCC produces a USE_ERROR
echo  from Ada.Streams.Stream_IO (a-ststio.adb:413)
echo  due to another error in ftell64, for files larger
echo  than 2 GiB.
echo.
pause

cd ..

rem make Ming object directories if not yet existing
md obj\fast_ming
md obj\debug_ming
copy obj\fast\libwin32ada.a   obj\fast_ming
copy obj\debug\debug.pra      obj\debug_ming
copy obj\debug\libwin32ada.a  obj\debug_ming

cd gwindows

call build "-march=i686"
