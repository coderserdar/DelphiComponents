{*********************************************************}
{* Persistently Stored SQL Window Configuration Info     *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit usqlcfg;

interface

uses
  Classes,
  Forms,
  Windows,
  Graphics,
  SysUtils,
  ffllbase;

const
  sqlCfgKeyOptions         = 'SQLOpts:';
  sqlCfgSplitterPosition   = 'SQLSplitterPos';
  sqlCfgWindow             = 'SQLWindow';
  sqlCfgWindowState        = 'SQLWindowState';
  sqlCfgWindowFontName     = 'SQLFontName';
  sqlCfgWindowFontSize     = 'SQLFontSize';

  defWindowState           = wsNormal;
  defSplitterPos           = 129;

type
  TffeSQLConfig = class(TPersistent)
  protected {private}
    FSplitterPos : Integer;
    FWindowRect  : TRect;
    FWindowState : TWindowState;
    FFontName    : string;
    FFontSize    : Integer;
    FServerName  : string;
    FDBName      : string;
    FINIFilename : TFileName;
    FINISection  : string;
  protected
    procedure ParseWindowString(aWindowStr : TffShStr);
    procedure SetWindowPos(aRect : TRect);
  public
    constructor Create(const aServerName, aDBName : string);
    procedure Refresh;
    {- Reload all settings from persistent storage}
    procedure Save;
    {- Save the configuration to persistent storage}

    property FontName : string
      read  FFontName
      write FFontName;
    property FontSize : Integer
      read  FFontSize
      write FFontSize;
    property SplitterPos : Integer
      read  FSplitterPos
      write FSplitterPos;
    property WindowPos : TRect
      read  FWindowRect
      write FWindowRect;
    property WindowState : TWindowState
      read  FWindowState
      write FWindowState;
  end;

implementation

uses
  Dialogs,
  Inifiles,
  uConfig;                  {!!.11}

{ TffeSQLConfig }

{====================================================================}
constructor TffeSQLConfig.Create(const aServerName, aDBName : string);
begin
  FServerName := aServerName;
  FDBName := aDBName;
  FINISection := sqlCfgKeyOptions + aServerName + aDBName;
  {Begin !!.11}
  FINIFilename := Config.WorkingDirectory + ChangeFileExt(ExtractFileName(Application.ExeName), '.');
  FINIFilename := Copy(FINIFilename, 1, Length(FINIFilename)-1) + 'SQL.INI';
  {End !!.11}
  Refresh;
end;
{--------}
procedure TffeSQLConfig.ParseWindowString(aWindowStr : TffShStr);
type
  TElement = (teLeft, teTop, teRight, teBottom);
var
  J       : TElement;
  Element : TffShStr;
begin
  try
    J := teLeft;
    repeat
      FFShStrSplit(aWindowStr, ' ', Element, aWindowStr);
      case J of
        teLeft   : FWindowRect.Left   := StrToInt(Element);
        teTop    : FWindowRect.Top    := StrToInt(Element);
        teRight  : FWindowRect.Right  := StrToInt(Element);
        teBottom : FWindowRect.Bottom := StrToInt(Element);
      end;
      if J < High(J) then Inc(J);
    until aWindowStr = '';
  except
  end;
end;
{--------}
procedure TffeSQLConfig.Refresh;
var
  Window : TffShStr;
begin
  with TINIFile.Create(FINIFilename) do begin
    try
      {get the window settings}
      FWindowState := TWindowState(ReadInteger(FINISection,
                                               sqlCfgWindowState,
                                               Ord(defWindowState)));
      Window := ReadString(FINISection, sqlCfgWindow, '');
      if Window <> '' then
        ParseWindowString(Window);
      {get the font settings}
      FFontName := ReadString(FINISection, sqlCfgWindowFontName, '');
      FFontSize := ReadInteger(FINISection, sqlCfgWindowFontSize, 8);
      {get the height of the SQL window}
      FSplitterPos :=
        ReadInteger(FINISection, sqlCfgSplitterPosition, 129);
    finally
      free;
    end;
  end; {with}
end;
{--------}
procedure TffeSQLConfig.Save;
begin
  with TINIFile.Create(FINIFilename) do
  try
    try
      with FWindowRect do
        WriteString(FINISection, sqlCfgWindow, Format('%d %d %d %d', [Left, Top, Right, Bottom]));
      WriteString(FINISection, sqlCfgWindowFontName, FFontName);
      WriteInteger(FINISection, sqlCfgWindowFontSize, FFontSize);
      WriteInteger(FINISection, sqlCfgWindowState, Ord(FWindowState));
      WriteInteger(FINISection, sqlCfgSplitterPosition, FSplitterPos);
    finally
      Free;
    end;
  except
    on E:Exception do
      ShowMessage('Error writing INI file: '+E.Message);
  end;
end;
{--------}
procedure TffeSQLConfig.SetWindowPos(aRect : TRect);
begin
  with FWindowRect do begin
    Left   := aRect.Left;
    Right  := aRect.Right;
    Top    := aRect.Top;
    Bottom := aRect.Bottom;
  end;
end;
{====================================================================}

end.
