{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
              Using code donated by Brad Choate <choate@delphiexchange.com>
Object:       Demo for MIME support (files attach) in sending files.
              This demo use TSyncSmtpCli for simplicity. High performnace
              programs should use the async SMTP component TSmtpCli.
Creation:     February 14th, 1998
Version:      1.12
EMail:        francois.piette@pophost.eunet.be francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1998, 1999 by François PIETTE
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
Sep 12, 1998  V1.10 Revised to use the new TSyncSmtpCli component.
Mar 06, 1999  V1.11 Replaced LastDelimiter by FindLastDelim for D2 compatibility
              Added Trim functions for Delphi 1. Used GetText/SetText for
              TStrings because of Delphi 1.
Aug 21, 1999  V1.12 Disposed memory got with GetText.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MimeTst1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SmtpProt, StdCtrls, ExtCtrls;

type
  TMimeTestForm = class(TForm)
    FSmtp: TSyncSmtpCli;
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    FromEdit: TEdit;
    ToEdit: TEdit;
    Label3: TLabel;
    SubjectEdit: TEdit;
    SendMailButton: TButton;
    HostEdit: TEdit;
    Label4: TLabel;
    Panel2: TPanel;
    FileListMemo: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Panel3: TPanel;
    Label7: TLabel;
    procedure SendMailButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FSmtpDisplay(Sender: TObject; Msg: String);
    procedure FSmtpGetData(Sender: TObject; LineNum: Integer;
      MsgLine: PChar; MaxLen: Integer; var More: Boolean);
  end;

var
  MimeTestForm: TMimeTestForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeTestForm.FormCreate(Sender: TObject);
begin
    FileListMemo.Clear;
    FileListMemo.Lines.Add('c:\temp\brol.txt');
    FileListMemo.Lines.Add('c:\temp\test.txt');
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindLastDelim(Delim : Char; const S : String) : Integer;
begin
    Result := Length(S);
    while (Result > 0) and (S[Result] <> Delim) do
        Dec(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
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
{ General-purpose routine that reformats the text portion                     }
{ of our message to a particular margin.                                      }
procedure Reformat(stl: TStrings; iMargin: integer);
var
    sOverage, sUnderage: string;
    i, iBreak: integer;
begin
    i := 0;
    while i < stl.Count do begin
        if Length(stl[i]) > iMargin then begin
            iBreak := FindLastDelim(' ', Copy(stl[i], 1, iMargin));
            if iBreak > 0 then begin
                sOverage  := Trim(Copy(stl[i], iBreak, Length(stl[i])));
                sUnderage := Copy(stl[i], 1, iBreak - 1);
                if Trim(sUnderage) <> '' then begin
                    stl[i] := sUnderage;
                    stl.Insert(i + 1, sOverage);
                end;
            end;
        end;
        Inc(i);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeTestForm.SendMailButtonClick(Sender: TObject);
var
    TextPtr : PChar;
begin
    TextPtr := FileListMemo.Lines.GetText;
    FSmtp.EmailFiles.SetText(TextPtr);
    StrDispose(TextPtr);
    FSmtp.RcptName.Clear;
    FSmtp.RcptName.Add(ToEdit.text);
    FSmtp.HdrSubject      := SubjectEdit.text;
    FSmtp.FromName        := FromEdit.Text;
    FSmtp.HdrFrom         := FromEdit.Text;
    FSmtp.HdrTo           := ToEdit.Text;
    FSmtp.Host            := HostEdit.Text;
    FSmtp.Port            := 'smtp';
    FSmtp.ConnectSync;
    FSmtp.MailSync;
    FSmtp.QuitSync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeTestForm.FSmtpDisplay(Sender: TObject; Msg: String);
begin
    if DisplayMemo.Lines.count > 200 then
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeTestForm.FSmtpGetData(Sender: TObject; LineNum: Integer;
  MsgLine: PChar; MaxLen: Integer; var More: Boolean);
var
    Len : Integer;
begin
    if LineNum > MsgMemo.Lines.count then
        More := FALSE
    else begin
        Len := Length(MsgMemo.Lines[LineNum - 1]);
        { Truncate the line if too long (should wrap to next line) }
        if Len >= MaxLen then
            StrPCopy(MsgLine, Copy(MsgMemo.Lines[LineNum - 1], 1, MaxLen - 1))
        else
            StrPCopy(MsgLine, MsgMemo.Lines[LineNum - 1]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

