gnatmake -P azip_gwindows -XBuild_Mode=Debug
copy azip.exe AZip_Debug.exe
del azip.exe
gnatmake -P azip_gwindows -XBuild_Mode=Fast
ren azip.exe AZip.exe
copy AZip.exe "AZip (ver) win32.exe"
upx --ultra-brute "AZip (ver) win32.exe"
copy "AZip (ver) win32.exe" AZip.exe

