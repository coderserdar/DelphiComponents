{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       This program is a demo for TMimeDecode component.
              TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 component.
              MIME is described in RFC-1521. headers are described if RFC-822.
Creation:     March 08, 1998
Version:      1.01
EMail:        francois.piette@pophost.eunet.be    
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.
Updates:
Apr 22, 1998  V1.01 Added an option box to save the parts in files. The files
              are writen in the current directory with a name like Part0.dat,
              Part1.dat and so on.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Pop3Mim1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniFiles, MimeDec;

type
  TMimeDecodeForm = class(TForm)
    Panel1: TPanel;
    FileEdit: TEdit;
    DecodeButton: TButton;
    Memo1: TMemo;
    MimeDecode1: TMimeDecode;
    Label1: TLabel;
    ClearButton: TButton;
    SaveToFileCheckBox: TCheckBox;
    procedure DecodeButtonClick(Sender: TObject);
    procedure MimeDecode1PartBegin(Sender: TObject);
    procedure MimeDecode1PartEnd(Sender: TObject);
    procedure MimeDecode1PartHeaderLine(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure MimeDecode1HeaderLine(Sender: TObject);
    procedure MimeDecode1PartLine(Sender: TObject; Data: PChar;
      DataLen: Integer);
    procedure MimeDecode1HeaderBegin(Sender: TObject);
    procedure MimeDecode1HeaderEnd(Sender: TObject);
    procedure MimeDecode1PartHeaderBegin(Sender: TObject);
    procedure MimeDecode1PartHeaderEnd(Sender: TObject);
  private
    FInitialized   : Boolean;
    FIniFileName   : String;
    FDstStream     : TStream;
    procedure Display(Msg: String);
  end;

var
  MimeDecodeForm: TMimeDecodeForm;

implementation

{$R *.DFM}
const
    SectionData   = 'Data';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    KeyFile       = 'FileName';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    Memo1.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        IniFile   := TIniFile.Create(FIniFileName);
        Top       := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        Left      := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        Width     := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height    := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        FileEdit.Text := IniFile.ReadString(SectionData,  KeyFile,   '');
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteString(SectionData,    KeyFile,   FileEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.DecodeButtonClick(Sender: TObject);
begin
    Memo1.Clear;
    MimeDecode1.DecodeFile(Trim(FileEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.Display(Msg: String);
begin
    { TMemo cannot hold too much data. Limit to 200 lines }
    if Memo1.Lines.count > 200 then
        Memo1.Clear;
    Memo1.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartBegin(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' BEGIN ----------');
    if SaveToFileCheckBox.Checked then
        FDstStream := TFileStream.Create('Part' + IntToStr(MimeDecode1.PartNumber) + '.dat', fmCreate	);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartEnd(Sender: TObject);
begin
    Display('--------- PART END ----------');
    if FDstStream <> nil then begin
        FDstStream.Destroy;
        FDstStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartLine(Sender: TObject; Data: PChar;
  DataLen: Integer);
begin
    Display(StrPas(Data));
    if (FDstStream <> nil) and (DataLen > 0) then
        FDstStream.WriteBuffer(Data^, DataLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderLine(Sender: TObject);
begin
    Display('Part header: ' + StrPas(MimeDecode1.CurrentData));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.ClearButtonClick(Sender: TObject);
begin
    Memo1.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderLine(Sender: TObject);
begin
    Display('Msg header: ' + StrPas(MimeDecode1.CurrentData));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderBegin(Sender: TObject);
begin
    Display('--------- HEADER BEGIN ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderEnd(Sender: TObject);
begin
    Display('--------- HEADER END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderBegin(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' HEADER BEGIN ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderEnd(Sender: TObject);
begin
    Display('--------- PART HEADER END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

