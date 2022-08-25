@echo off
mode con lines=50
if not exist OverbyteIcsV8.zip goto ZipIt
if exist OverbyteIcsV8_21.zip del OverbyteIcsV8_21.zip
if exist OverbyteIcsV8_20.zip ren OverbyteIcsV8_20.zip OverbyteIcsV8_21.zip
if exist OverbyteIcsV8_19.zip ren OverbyteIcsV8_19.zip OverbyteIcsV8_20.zip
if exist OverbyteIcsV8_18.zip ren OverbyteIcsV8_18.zip OverbyteIcsV8_19.zip
if exist OverbyteIcsV8_17.zip ren OverbyteIcsV8_17.zip OverbyteIcsV8_18.zip
if exist OverbyteIcsV8_16.zip ren OverbyteIcsV8_16.zip OverbyteIcsV8_17.zip
if exist OverbyteIcsV8_15.zip ren OverbyteIcsV8_15.zip OverbyteIcsV8_16.zip
if exist OverbyteIcsV8_14.zip ren OverbyteIcsV8_14.zip OverbyteIcsV8_15.zip
if exist OverbyteIcsV8_13.zip ren OverbyteIcsV8_13.zip OverbyteIcsV8_14.zip
if exist OverbyteIcsV8_12.zip ren OverbyteIcsV8_12.zip OverbyteIcsV8_13.zip
if exist OverbyteIcsV8_11.zip ren OverbyteIcsV8_11.zip OverbyteIcsV8_12.zip
if exist OverbyteIcsV8_10.zip ren OverbyteIcsV8_10.zip OverbyteIcsV8_11.zip
if exist OverbyteIcsV8_09.zip ren OverbyteIcsV8_09.zip OverbyteIcsV8_10.zip
if exist OverbyteIcsV8_08.zip ren OverbyteIcsV8_08.zip OverbyteIcsV8_09.zip
if exist OverbyteIcsV8_07.zip ren OverbyteIcsV8_07.zip OverbyteIcsV8_08.zip
if exist OverbyteIcsV8_06.zip ren OverbyteIcsV8_06.zip OverbyteIcsV8_07.zip
if exist OverbyteIcsV8_05.zip ren OverbyteIcsV8_05.zip OverbyteIcsV8_06.zip
if exist OverbyteIcsV8_04.zip ren OverbyteIcsV8_04.zip OverbyteIcsV8_05.zip
if exist OverbyteIcsV8_03.zip ren OverbyteIcsV8_03.zip OverbyteIcsV8_04.zip
if exist OverbyteIcsV8_02.zip ren OverbyteIcsV8_02.zip OverbyteIcsV8_03.zip
if exist OverbyteIcsV8_01.zip ren OverbyteIcsV8_01.zip OverbyteIcsV8_02.zip
if exist OverbyteIcsV8.zip   ren OverbyteIcsV8.zip OverbyteIcsV8_01.zip
:ZipIt
wzzip -P OverbyteIcsV8.zip @ZipOverbyteIcsV8.lst > ziplog.log
ren OverbyteIcsV8.zip OverbyteIcsV8.zip
pause
