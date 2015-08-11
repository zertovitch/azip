rem Special build for MinGW, not GNAT GPL.

rem Add MinGW bin in front of the PATH
call ming_set

cd ..

rem make Ming object directories if not yet existing
md obj_ming
md obj_ming\fast
md obj_ming\debug
copy obj\fast\libwin32ada.a obj_ming\fast
copy obj\debug\debug.pra obj_ming\debug
copy obj\debug\libwin32ada.a obj_ming\debug

cd gwindows

call build "-march=i686"
