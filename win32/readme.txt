              opensource COBOL
        http://www.osscons.jp/osscobol/

This directory contains the project files for Windows

This package contains the following subdirectories:

    cobc        COBOL compiler project
    cobcrun     COBOL driver program project
    libcob      COBOL run-time library project
    vbisam      VBISAM file I/O library project

============
Requirements
============

opensource COBOL requires the following external libraries to be installed:

  o MPIR (libmpir) 2.6.0 or later
    http://mpir.org/index.html

    MPIR is distributed under GNU Lesser General Public License.

  o PDCurses (pdcurses)
    http://pdcurses.sourceforge.net
    
    If you want to use all of the opensource COBOL CUI features,
    you need to apply the following path:
    http://www.osscons.jp/osscobol/download/

    PDCurses is distributed under public domain.

============

============
Installation
============

1. To generate requirements libraries :

You need to create the following libraries.

**************************************

  o MPIR (libmpir)
    Download a MPIR package from (http://mpir.org/index.html).
    Open build.vc10/mpir.sln by Visual Studio.
    Build a project for your system ( Usually "dll_mpir_gc" ).

For more information, please refer to the README in MPIR

**************************************

  o PDCurses
    Download a MPIR package from (http://pdcurses.sourceforge.net).
    Move directory to pdcurs34\win32 on your console.
    run "nmake" command to build libraries.
       nmake -f vcwin32.mak WIDE=Y DLL=Y

For more information, please refer to the README in PDCurses

**************************************

2. To generate/install opensource COBOL on Windows :

**************************************

    Open win32/opencobol.sln by Visual Studio.
    run "Build Solution" on Visual Studio. 

**************************************

You need to set the following environment variables
when running "Build Solution".

    INCLUDE     Path of requirements headers.
                Deploy headers "curses.h","curspriv.h" from pdcurses
                and "gmp.h" from mpir.
    LIB         Path of requirements libraries.
                Deploy libraries "pdcurses.lib" from pdcurses
                and "mpir.lib" from mpir.

************************************** 

The "Build Solution" will default to "win32/BIN/x64/Debug" as
the install path. When you want to set files another path, 
changing the project properties on the Visual Studio 
or copying those file to any location.

You need to set the location of installed files 
to the environment variable "PATH" (ex. "c:\opensourceCOBOL/win32/BIN/x64/Debug")

************************************** 
