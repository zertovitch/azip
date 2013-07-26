rem Special build for MinGW, not GNAT GPL.

path C:\Ada\GNAT\MinGW\bin;%path%

cd ..
ren obj  obj_gpl
ren obj_ming obj
cd gwindows

call build "-march=i686"

cd ..
ren obj obj_ming
ren obj_gpl  obj
cd gwindows
