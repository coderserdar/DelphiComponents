@echo off

@set comp_root=.\Optional.Libs

@set comp_compress=%comp_root%\COMPRESS
@set comp_graphicex=%comp_root%\GraphicEx
@set comp_hexedit=%comp_root%\HexEdit
@set comp_synedit=%comp_root%\SynEdit

@set comp_jcl=..\..\jcl
@rem set comp_jcl=%comp_root%\JCL
@rem set jclroot=C:\Borland\Delphi\Components\JEDI\JCL_1_96
@rem set jclsrc=%jclroot%\jcl\source
@rem set comp_jcl=%jclsrc%;%jclsrc%\common;%jclsrc%\windows;%jclsrc%\windows\obj;%jclsrc%\vcl;%jclroot%\jcl\experts\debug\dialog

@set comp_xpmenu=%comp_root%\XPMenu

@rem Common options

  @set UsePack=0
  @set DEBUG=1
  @set MAPFILE=1
  @set CleanDcc32Log=1
  @set DEBUG_BATCH=1
  @set TRACE_STACK_SOURCE=1

  @set UserLib=..\..;..\..\DXFixes
  @set UserLib=%UserLib%;%comp_compress%
  @set UserLib=%UserLib%;%comp_graphicex%
  @set UserLib=%UserLib%;%comp_hexedit%
  @set UserLib=%UserLib%;%comp_synedit%
  @set UserLib=%UserLib%;%comp_jcl%
  @set UserLib=%UserLib%;%comp_xpmenu%

  @set UserLibI=%UserLib%
  @set UserLibR=%UserLib%

  @set UserCOpt=-D_DEBUG_;_TRACE_CALLS_;_CHECK_LEAKS_ -$R-,O-,D+,L+
