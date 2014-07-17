rem Special build for MinGW, not GNAT GPL.

rem Add MinGW bin in front of the PATH
call ming_set

cd ..

rem make Ming object directories if not yet existing
md obj_ming
md obj_ming\fast
md obj_ming\debug
copy obj\debug\debug.pra obj_ming\debug

rem make Ming object directories the active ones
ren obj  obj_gpl
ren obj_ming obj
cd gwindows

call build "-march=i686"

cd ..
rem make GPL object directories the active ones
ren obj obj_ming
ren obj_gpl  obj
cd gwindows
