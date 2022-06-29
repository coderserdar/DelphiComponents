{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       This program is a demo for TMimeDecode component.
              TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 component.
              MIME is described in RFC-1521. headers are described if RFC-822.
Creation:     March 08, 1998
Version:      1.02
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
Sep 13, 1998  V1.01 Added part and header end numbering
Feb 16/02/99  V1.02 In OnPartLine event handler, assemble line of text for
              display.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MimeDmo1;

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
    procedure MimeDecode1InlineDecodeBegin(Sender: TObject;
      Filename: String);
    procedure MimeDecode1InlineDecodeEnd(Sender: TObject;
      Filename: String);
    procedure MimeDecode1InlineDecodeLine(Sender: TObject; Line: PChar);
  private
    FInitialized   : Boolean;
    FIniFileName   : String;
    FLineBuf       : array [0..255] of char;
    FCharCnt       : Integer;
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
    Update;
    MimeDecode1.DecodeFile(FileEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.Display(Msg: String);
begin
    { TMemo cannot hold too much data. Limit to 400 lines }
    if Memo1.Lines.count > 400 then
        Memo1.Clear;
    Memo1.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartBegin(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' BEGIN ----------');
    FCharCnt := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartEnd(Sender: TObject);
begin
    if FCharCnt > 0 then begin
        Display(StrPas(FLineBuf));
        FCharCnt := 0;
    end;

    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Decoded data arrives here. This routine suppose that we have text data    }
{ organized in lines.                                                       }
procedure TMimeDecodeForm.MimeDecode1PartLine(
    Sender  : TObject;
    Data    : PChar;
    DataLen : Integer);
var
    I : Integer;
begin
    { Copy data to LineBuf until CR/LF }
    I := 0;
    while (I < DataLen) do begin
        if Data[I] = #13 then   { Just ignre CR }
            Inc(I)
        else if Data[I] = #10 then begin { LF is end of line }
            FLineBuf[FCharCnt] := #0;
            Display(StrPas(FLineBuf));
            FCharCnt := 0;
            Inc(I);
        end
        else begin
            FLineBuf[FCharCnt] := Data[I];
            Inc(FCharCnt);
            Inc(I);
        end;
        if FCharCnt >= (High(FLineBuf) - 1) then begin
            { Buffer overflow, display data accumulated so far }
            FLineBuf[High(FLineBuf) - 1] := #0;
            Display(StrPas(FLineBuf));
            FCharCnt := 0;
        end;
    end;
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
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' HEADER END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TMimeDecodeForm.MimeDecode1InlineDecodeBegin(Sender: TObject;
  Filename: String);
begin
    Display('--------- INLINE begin. Filename is ''' + FileName + '''');
    Display('');
end;

procedure TMimeDecodeForm.MimeDecode1InlineDecodeEnd(Sender: TObject;
  Filename: String);
begin
    Display('--------- INLINE end');
end;

procedure TMimeDecodeForm.MimeDecode1InlineDecodeLine(
  Sender : TObject;
  Line   : PChar);
var
    LastLine : String;
begin
    if Memo1.Lines.Count < 1 then
        Memo1.Lines.Add(StrPas(Line))
    else begin
        LastLine := Memo1.Lines.Strings[Memo1.Lines.Count - 2];
        Memo1.Lines.Delete(Memo1.Lines.Count - 1);
        Memo1.Lines.Delete(Memo1.Lines.Count - 1);
        LastLine := LastLine + StrPas(Line);
        Memo1.Lines.Add(LastLine);
    end;
end;

end.

