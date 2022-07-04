{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Patched by Jaro Benes                                 }
{*******************************************************}

unit RXConst;

interface

{$I RX.INC}

uses Controls;

const
  RX_VERSION = $0002004B;  { 2.75 }
  RX_REVISION = '9.2010';  {last visit September 2010}

const
{ Command message for Speedbar editor }
  CM_SPEEDBARCHANGED = CM_BASE + 80;
{ Command message for TRxSpeedButton }
  CM_RXBUTTONPRESSED = CM_BASE + 81;
{ Command messages for TRxWindowHook }
  CM_RECREATEWINDOW  = CM_BASE + 82;
  CM_DESTROYHOOK     = CM_BASE + 83;
{ Notify message for TRxTrayIcon }
  CM_TRAYICON        = CM_BASE + 84;

const
  crHand     = TCursor(14000);
  crDragHand = TCursor(14001);

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS uses this value }
  PaletteMask = $02000000;

{$IFDEF VER90}
const
  SDelphiKey = 'Software\Borland\Delphi\2.0';
{$ENDIF}

{$IFDEF VER93}
const
  SDelphiKey = 'Software\Borland\C++Builder\1.0';
{$ENDIF}

{$IFDEF VER100}
const
  SDelphiKey = 'Software\Borland\Delphi\3.0';
{$ENDIF}

{$IFDEF VER110}
const
  SDelphiKey = 'Software\Borland\C++Builder\3.0';
{$ENDIF}

{$IFDEF VER120}
const
  SDelphiKey = 'Software\Borland\Delphi\4.0';
{$ENDIF}

{$IFDEF VER125}
const
  SDelphiKey = 'Software\Borland\C++Builder\4.0';
{$ENDIF}

{$IFDEF VER130}
const
  {$IFDEF BCB}
  SDelphiKey = 'Software\Borland\C++Builder\5.0';
  {$ELSE}
  SDelphiKey = 'Software\Borland\Delphi\5.0';
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140} // Polaris
const
  SDelphiKey = 'Software\Borland\Delphi\6.0'; {Delphi 6}
{$ENDIF}

{$IFDEF VER150} // JB
const
  SDelphiKey = 'Software\Borland\Delphi\7.0'; {Delphi 7}
{$ENDIF}

{$IFDEF VER170} // JB
const
  SDelphiKey = 'Software\Borland\BDS\3.0'; {Delphi 2005}
{$ENDIF}

{$IFDEF VER180} // JB
const
  {$IFDEF VER185} // JB
  SDelphiKey = 'Software\Borland\BDS\5.0'; {Delphi 2007}
  {$ELSE}
  SDelphiKey = 'Software\Borland\BDS\4.0'; {BDS 2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200} // JB
const
  SDelphiKey = 'Software\Codegear\BDS\6.0'; {Delphi 2009}
{$ENDIF}

{$IFDEF VER210} // JB
const
  SDelphiKey = 'Software\Codegear\BDS\7.0';  {Delphi 2010}
{$ENDIF}

{$IFDEF VER220} // JB
const
  SDelphiKey = 'Software\Embarcadero\BDS\8.0'; {Delphi XE}
{$ENDIF}

implementation

uses {$IFDEF WIN32} Windows, {$ELSE} WinProcs, {$ENDIF} Forms;

{$R *.RES}

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'RX_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'RX_DRAGCUR');
end.