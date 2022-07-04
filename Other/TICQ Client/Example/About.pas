(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExeStamp;

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Bevel1: TBevel;
    btnClose: TButton;
    Image: TImage;
    lblCaption: TLabel;
    lblCopyright: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblGNU: TLabel;
    edBuild: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label7: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lblGNUClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  protected
    procedure CreateParams(var Params : TCreateParams); override;
  public
    function __TIMESTAMP__: String;
    function GetFileVer: String;
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  Main;

{$R *.dfm}

function TAboutForm.GetFileVer: String;
var
  dwSize: DWORD;
  lpData, productVersion: PChar;
  uiSize: UINT;
  LangCharSetIDArray: array[1..2] of Word;
begin
 Result := '';
 dwSize := GetFileVersionInfoSize(PChar(ParamStr(0)), dwSize);
 if dwSize <> 0 then
 begin
  GetMem(lpData, dwSize);
  if GetFileVersionInfo(PChar(ParamStr(0)), 0, dwSize, lpData) then begin
    if VerQueryValue(lpData, '\VarFileInfo\Translation', Pointer(productVersion), uiSize) then begin
      LangCharSetIDArray[1] := LoWord(PLongint(productVersion)^);
      LangCharSetIDArray[2] := HiWord(PLongint(productVersion)^);
    end;
    if VerQueryValue(lpData, PChar(Format('\StringFileInfo\0%x0%x\ProductVersion',
                          [LangCharSetIDArray[1],LangCharSetIDArray[2] ])), Pointer(productVersion), uiSize) then
    Result := productVersion;
  end;
  FreeMem(lpData, dwSize);
 end else
   Result := '';
end;

function TAboutForm.__TIMESTAMP__: String;
var
  FStamp : DWORD;
begin
  FStamp := GetExeTimeStamp(Pointer(HInstance));
  Result := Format('Built %s',[MainForm.ConvertDateTime(FileDateToDateTime(FStamp))]);
end;

procedure TAboutForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params); //Don't ever forget to do this!!!
  Params.WindowClass.hIcon := Icon.Handle;
  Params.WndParent := GetDesktopWindow;
end;

procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  AboutForm := nil;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  //System HandPoint Cursor
  if IsWinVer2000Plus then begin
    Screen.Cursors[NIDC_HAND] := LoadCursor(0, IDC_HAND);
    lblGNU.Cursor := NIDC_HAND;
  end
  else
    lblGNU.Cursor := crHandPoint;

  edBuild.Text := __TIMESTAMP__;
  lblCaption.Caption := Format('eICQ %s', [GetFileVer]);
end;

procedure TAboutForm.lblGNUClick(Sender: TObject);
begin
  MainForm.OpenURL(ExtractFilePath(ParamStr(0)) + 'Docs\GNU_GPL.txt', False);
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.
