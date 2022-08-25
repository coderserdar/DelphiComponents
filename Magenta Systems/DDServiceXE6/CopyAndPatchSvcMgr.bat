@echo off

cd Diffs
CreateSetEnvBat
if exist setenv.bat call setenv.bat else goto NoCompiler
if not exist %SvcMgrPas% goto NoSource
copy %SvcMgrPas% CopyOf_SvcMgr.pas
patch -n -o DDSvcMgr.pas -i DDSvcMgrD%CompilerVersion%.diff -b CopyOf_SvcMgr.pas
if not exist ..\Source\DDSvcMgr.pas goto WriteFile
echo In order to ** NOT OVERWRITE ** file Source\DDSvcMgr.pas press CTRL+C
pause

:WriteFile
copy DDSvcMgr.pas ..\Source
del DDSvcMgr.pas
del CopyOf_SvcMgr.pas
goto end

:NoSource
echo %SvcMgrPas% not found
pause
goto end

:NoCompiler
echo A supported IDE version was not found
pause

:end