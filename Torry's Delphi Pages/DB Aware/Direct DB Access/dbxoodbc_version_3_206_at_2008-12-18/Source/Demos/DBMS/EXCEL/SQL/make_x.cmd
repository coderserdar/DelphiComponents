@if "%1"=="" goto :eof
@set path=.\..\..\..\..\make_tools;%path%
@.\..\..\..\..\make_tools\make_prj.bat %1 Project1.dpr
