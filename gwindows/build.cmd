@echo off

if (%1)==(-gen) echo Option -gen
if (%1)==(-gen) echo Generate Ada code from resource script (dialogs, menus, ...).
if (%1)==(-gen) call rc2gw AZip.rc
if (%1)==(-gen) shift

rem Use another compiler.
rem was for "-march=i686" for MinGW 4.7.2

REM ADA_INCLUDE_PATH=
REM C:\Ada\zip-ada\zip_lib;C:\Ada\gnavi\gwindows\framework;C:\Ada\gnavi\gwindows\contrib;C:\Ada\gnavi\gnatcom\framework


set builder=gprbuild
set builder=gnatmake
set target=_MinGW
if (%1)==() set builder=gprbuild
if (%1)==() set target=_GPL
if (%1)==() echo Option for alternative compiler "%1"

del azip.exe
%builder% -P azip_gwindows -XAZip_Build_Mode=Debug%target%
copy azip.exe AZip_Debug%target%.exe

del azip.exe
%builder% -P azip_gwindows azip -XAZip_Build_Mode=Fast%target%
ren azip.exe AZip.exe
copy AZip.exe "AZip (ver) win32%target%.exe"
rem upx --ultra-brute "AZip (ver) win32.exe"
copy "AZip (ver) win32%target%.exe" AZip.exe

pause
