@echo off

@call clean.bat
del common.bat 2>nul
del dbxoodbc.jdbg 2>nul
del dbxoodbc.map 2>nul
del dbxoodbc.dll 2>nul
del /S *.dcu 2>nul
del /S *.drc 2>nul
