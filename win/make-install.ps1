mkdir C:\opensourcecobol4j\bin\
mkdir C:\opensourcecobol4j\config\

$env:PATH+=";C:\opensourcecobol4j\bin\;C:\opensourcecobol4j\config\"

copy .\x64\Debug\cobj.exe C:\opensourcecobol4j\bin\
copy ..\config\*.conf C:\opensourcecobol4j\config\