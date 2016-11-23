@echo off

rem generic compile batch file for (most) console compilers
rem pause after each compiler if %1 not empty
rem call self on low env space condition
rem (c) 2003-2010 W.Ehrhardt

rem test file
rem =========

set SRC=t_s20_st
::set SRC=t_s20cyc
::set SRC=t_xs20
::set SRC=t_chacha


rem log file (may be con or nul)
rem ============================
::set LOG=nul
set LOG=%SRC%.LOG


rem parameters for test file
rem ========================
set PARA=1

rem build options
rem ========================
::set OPT=-d


rem test whether enough space in environment
rem =========================================
set PCB=A_rather_long_environment_string_for_testing
if  (%PCB%)==(A_rather_long_environment_string_for_testing) goto OK


echo call self with 4096 byte env
echo ============================
set PCB=
%COMSPEC% /E:4096 /C %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto ende


:OK

echo Test %SRC% for (most) console compilers  >%LOG%
ver >>%LOG%

set PCB=bpc -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=bpc -CP -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc1 -B -TGO32V2
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc2 -B
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc22 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc222 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc224 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc240 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc240d -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call vpc -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

if (%OS%)==(Windows_NT) goto skip555
set PCB=D:\DMX\TP5\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\TP55\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%
:skip555

set PCB=D:\DMX\TP6\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M2\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M3\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M4\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M5\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M6\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%


set PCB=D:\DMX\M7\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M9\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

call wdosx %SRC%.exe
echo. >>%LOG%
echo Results for WDOSX >>%LOG%
%SRC%.exe %PARA% >>%LOG%


set PCB=D:\DMX\M10\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

if not (%OS%)==(Windows_NT) goto NO_D12
set PCB=D:\DMX\M12\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%
:NO_D12


echo.
echo **** Log file: %LOG%

:ende

set PCB=
set SRC=
set LOG=
set PARA=
set OPT=

