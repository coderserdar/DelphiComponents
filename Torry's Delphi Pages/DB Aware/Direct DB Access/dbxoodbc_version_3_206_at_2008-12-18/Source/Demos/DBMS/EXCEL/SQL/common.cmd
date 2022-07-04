@echo off

rem Common options

  set UsePack=0
  set EurekaLog=0
  set DEBUG=1
  set MAPFILE=1
  set JDBG=0
  set CleanDcc32Log=1
  set DEBUG_BATCH=0
  set TRACE_STACK_SOURCE=1

  set UserLib=.\..\..\..\..;.\..\..\..\..\jcl
  
  if %DXVER% LSS 8 set UserLib=%UserLib%;DNULL08
  rem if %DXVER% LSS 11 set UserLib=%UserLib%;DNULL11
  
  rem set UserLib=%UserLib%;.\RBuilder
  rem set TL=C:\Borland\Components\Typhoon2
  rem set UserLib=%UserLib%;%TL%\RBuilder\D%DXVER%\Languages\Eng;%TL%\RBuilder\D%DXVER%
  rem set UserLib=%UserLib%;%TL%\_Lib\D%DXVER%

rem set Include and Resource path
  set UserLibI=%UserLib%
  set UserLibR=%UserLib%

  @rem set UserCOpt=-D_DEBUG_;_TRACE_CALLS_;_CHECK_LEAKS_ -$R-,O-,D+,L+