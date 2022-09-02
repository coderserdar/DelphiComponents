{*********************************************************}
{* FlashFiler: Config interface for FFRebuild210         *}
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

unit uConfig;

interface

uses
  IniFiles;

type
  TFallbackConfig = class
  protected

    FIni : TINIFile;

    procedure IniCreate;
    procedure IniFree;

    function GetAllowChangeDir : Boolean;
    function GetAutoRun : Boolean;
    function GetInitialDir : string;
    function GetOutputDir : string;

    procedure SetAllowChangeDir(const Value : Boolean);
    procedure SetAutoRun(const Value : Boolean);
    procedure SetInitialDir(const Value : string);
    procedure SetOutputDir(const Value : string);

  public

    property AllowChangeDir : Boolean
      read GetAllowChangeDir
      write SetAllowChangeDir;

    property AutoRun : Boolean
      read GetAutoRun
      write SetAutoRun;

    property InitialDir : string
      read GetInitialDir
      write SetInitialDir;

    property OutputDir : string
      read GetOutputDir
      write SetOutputDir;

  end;

implementation

uses
  Forms,
  SysUtils;

const
  csAllowChangeDir = 'AllowChangeDirectory';
  csAutoRun = 'AutoRun';
  csIniName = 'FFRebuild210.ini';
  csInitialDir = 'InitialDirectory';
  csOutputDir = 'OutputDirectory';
  csSection = 'Config';

{====================================================================}
function TFallbackConfig.GetAllowChangeDir : Boolean;
begin
  IniCreate;
  try
    Result := FIni.ReadBool(csSection, csAllowChangeDir, False);
  finally
    IniFree;
  end;
end;
{--------}
function TFallbackConfig.GetAutoRun : Boolean;
begin
  IniCreate;
  try
    Result := FIni.ReadBool(csSection, csAutoRun, False);
  finally
    IniFree;
  end;
end;
{--------}
function TFallbackConfig.GetInitialDir : string;
begin
  IniCreate;
  try
    Result := FIni.ReadString(csSection, csInitialDir, '');
  finally
    IniFree;
  end;
end;
{--------}
function TFallbackConfig.GetOutputDir : string;
begin
  IniCreate;
  try
    Result := FIni.ReadString(csSection, csOutputDir, '');
  finally
    IniFree;
  end;
end;
{--------}
procedure TFallbackConfig.IniCreate;
begin
  FIni := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
end;
{--------}
procedure TFallbackConfig.IniFree;
begin
  FIni.Free;
end;
{--------}
procedure TFallbackConfig.SetAllowChangeDir(const Value : Boolean);
begin
  IniCreate;
  try
    FIni.WriteBool(csSection, csAllowChangeDir, Value);
  finally
    IniFree;
  end;
end;
{--------}
procedure TFallbackConfig.SetAutoRun(const Value : Boolean);
begin
  IniCreate;
  try
    FIni.WriteBool(csSection, csAutoRun, Value);
  finally
    IniFree;
  end;
end;
{--------}
procedure TFallbackConfig.SetInitialDir(const Value : string);
begin
  IniCreate;
  try
    FIni.WriteString(csSection, csInitialDir, Value);
  finally
    IniFree;
  end;
end;
{--------}
procedure TFallbackConfig.SetOutputDir(const Value : string);
begin
  IniCreate;
  try
    FIni.WriteString(csSection, csOutputDir, Value);
  finally
    IniFree;
  end;
end;
{====================================================================}

end.
