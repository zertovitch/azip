echo Option "%1"
rem e.g. "-march=i686" for MinGW 4.7.2

REM ADA_INCLUDE_PATH=
REM C:\Ada\zip-ada\zip_lib;C:\Ada\gnavi\gwindows\framework;C:\Ada\gnavi\gwindows\contrib;C:\Ada\gnavi\gnatcom\framework

set builder=gnatmake
if (%1)==() set builder=gprbuild

%builder% -P azip_gwindows %1 -XBuild_Mode=Debug
copy azip.exe AZip_Debug.exe
del azip.exe
%builder% -P azip_gwindows %1 -XBuild_Mode=Fast
ren azip.exe AZip.exe
copy AZip.exe "AZip (ver) win32.exe"
rem upx --ultra-brute "AZip (ver) win32.exe"
copy "AZip (ver) win32.exe" AZip.exe

