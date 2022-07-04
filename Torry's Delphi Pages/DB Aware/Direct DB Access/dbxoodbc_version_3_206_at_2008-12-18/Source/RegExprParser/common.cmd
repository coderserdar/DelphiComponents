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

  set UserLib=..

rem set Include and Resource path
  set UserLibI=%UserLib%
  set UserLibR=%UserLib%

  @set UserCOpt=-D_RegExpr_DumpMatchPos_ -$R-,O-,D-
