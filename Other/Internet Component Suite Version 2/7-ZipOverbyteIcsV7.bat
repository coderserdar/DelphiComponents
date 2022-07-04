@echo off
mode con lines=50
if not exist OverbyteIcsV7.zip goto ZipIt
if exist OverbyteIcsV7_21.zip del OverbyteIcsV7_21.zip
if exist OverbyteIcsV7_20.zip ren OverbyteIcsV7_20.zip OverbyteIcsV7_21.zip
if exist OverbyteIcsV7_19.zip ren OverbyteIcsV7_19.zip OverbyteIcsV7_20.zip
if exist OverbyteIcsV7_18.zip ren OverbyteIcsV7_18.zip OverbyteIcsV7_19.zip
if exist OverbyteIcsV7_17.zip ren OverbyteIcsV7_17.zip OverbyteIcsV7_18.zip
if exist OverbyteIcsV7_16.zip ren OverbyteIcsV7_16.zip OverbyteIcsV7_17.zip
if exist OverbyteIcsV7_15.zip ren OverbyteIcsV7_15.zip OverbyteIcsV7_16.zip
if exist OverbyteIcsV7_14.zip ren OverbyteIcsV7_14.zip OverbyteIcsV7_15.zip
if exist OverbyteIcsV7_13.zip ren OverbyteIcsV7_13.zip OverbyteIcsV7_14.zip
if exist OverbyteIcsV7_12.zip ren OverbyteIcsV7_12.zip OverbyteIcsV7_13.zip
if exist OverbyteIcsV7_11.zip ren OverbyteIcsV7_11.zip OverbyteIcsV7_12.zip
if exist OverbyteIcsV7_10.zip ren OverbyteIcsV7_10.zip OverbyteIcsV7_11.zip
if exist OverbyteIcsV7_09.zip ren OverbyteIcsV7_09.zip OverbyteIcsV7_10.zip
if exist OverbyteIcsV7_08.zip ren OverbyteIcsV7_08.zip OverbyteIcsV7_09.zip
if exist OverbyteIcsV7_07.zip ren OverbyteIcsV7_07.zip OverbyteIcsV7_08.zip
if exist OverbyteIcsV7_06.zip ren OverbyteIcsV7_06.zip OverbyteIcsV7_07.zip
if exist OverbyteIcsV7_05.zip ren OverbyteIcsV7_05.zip OverbyteIcsV7_06.zip
if exist OverbyteIcsV7_04.zip ren OverbyteIcsV7_04.zip OverbyteIcsV7_05.zip
if exist OverbyteIcsV7_03.zip ren OverbyteIcsV7_03.zip OverbyteIcsV7_04.zip
if exist OverbyteIcsV7_02.zip ren OverbyteIcsV7_02.zip OverbyteIcsV7_03.zip
if exist OverbyteIcsV7_01.zip ren OverbyteIcsV7_01.zip OverbyteIcsV7_02.zip
if exist OverbyteIcsV7.zip   ren OverbyteIcsV7.zip OverbyteIcsV7_01.zip
:ZipIt
7z a -tzip OverbyteIcsV7.zip @ZipOverbyteIcsV7.lst
REM 7z a -t7z OverbyteIcsV7.7z @ZipOverbyteIcsV7.lst
ren OverbyteIcsV7.zip OverbyteIcsV7.zip
pause
