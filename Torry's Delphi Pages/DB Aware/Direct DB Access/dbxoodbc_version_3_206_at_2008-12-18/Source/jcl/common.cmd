@echo off

rem Common options

  set UsePack=0
  set EurekaLog=0
  set DEBUG=0
  set MAPFILE=0
  set JDBG=0
  set CleanDcc32Log=1
  set DEBUG_BATCH=0
  set TRACE_STACK_SOURCE=0

  rem set UserLib=.

  set UserLib=.\D%DXVER%;C:\Borland\Components\DevEx\_libs\D%DBASE%
  rem set UserLib=%UserLib%;.\RBuilder
  rem set TL=C:\Borland\Components\Typhoon2
  rem set UserLib=%UserLib%;%TL%\RBuilder\D%DXVER%\Languages\Eng;%TL%\RBuilder\D%DXVER%
  rem set UserLib=%UserLib%;%TL%\_Lib\D%DXVER%

rem set Include and Resource path
  set UserLibI=%UserLib%
  set UserLibR=%UserLib%

  @rem set UserCOpt=-D_DEBUG_;_TRACE_CALLS_;_CHECK_LEAKS_ -$R-,O-,D+,L+