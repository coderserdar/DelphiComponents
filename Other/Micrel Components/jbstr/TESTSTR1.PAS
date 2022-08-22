unit TestStr1;

{
(c) 1998-99 Jaro Benes, All Right Reserved

Test program for specific routines from jbStr.Pas module
}

interface

uses
  SysUtils, Classes,
  {$IFNDEF CLR}
  Windows, Menus, Controls, StdCtrls, ExtCtrls, Forms, Dialogs, Graphics
  {$ELSE}
  Borland.Vcl.Messages,
  Borland.Vcl.Menus,
  Borland.Vcl.StdCtrls,
  Borland.Vcl.Controls,
  Borland.Vcl.ExtCtrls,
  Borland.Vcl.Forms,
  Borland.Vcl.Dialogs,
  System.ComponentModel
  {$ENDIF};

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    _ShortPath: TButton;
    _ShortFileName: TButton;
    _FileNameOnly: TButton;
    _TrimRight: TButton;
    _TrimBoth: TButton;
    _TrimLeft: TButton;
    _PadRight: TButton;
    _Center: TButton;
    _PadLeft: TButton;
    _ExtensionOnly: TButton;
    _NameOnly: TButton;
    _PathOnly: TButton;
    _Capitalize: TButton;
    _StrLoCase: TButton;
    _StrUpCase: TButton;
    Label2: TLabel;
    Label1: TLabel;
    OrigStr: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    NewStr: TLabel;
    _Zip: TButton;
    _Smash: TButton;
    _Change: TButton;
    _Form: TButton;
    _Strip: TButton;
    _InsertWord: TButton;
    _Push: TButton;
    _Hash: TButton;
    _MaskAndZeroClip: TButton;
    _Count: TButton;
    _PopWord: TButton;
    _ExtractWord: TButton;
    _GetPosition: TButton;
    _GetEnd: TButton;
    _WordCount: TButton;
    _ChangeWord: TButton;
    _GetLastWord: TButton;
    _GetFirstWord: TButton;
    _Romanum: TButton;
    _ExtractEmail: TButton;
    Notes: TLabel;
    _Alter: TButton;
    _Reduce: TButton;
    _ExtractNumber: TButton;
    _FindWord: TButton;
    _JoinTo: TButton;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    _PackNum: TButton;
    MainMenu1: TMainMenu;
    Files1: TMenuItem;
    Exit1: TMenuItem;
    procedure _StrUpCaseClick(Sender: TObject);
    procedure _StrLoCaseClick(Sender: TObject);
    procedure _CapitalizeClick(Sender: TObject);
    procedure _PathOnlyClick(Sender: TObject);
    procedure _NameOnlyClick(Sender: TObject);
    procedure _ExtensionOnlyClick(Sender: TObject);
    procedure _FileNameOnlyClick(Sender: TObject);
    procedure _ShortPathClick(Sender: TObject);
    procedure _ShortFileNameClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure _TrimLeftClick(Sender: TObject);
    procedure _TrimBothClick(Sender: TObject);
    procedure _TrimRightClick(Sender: TObject);
    procedure _PadLeftClick(Sender: TObject);
    procedure _CenterClick(Sender: TObject);
    procedure _PadRightClick(Sender: TObject);
    procedure _ZipClick(Sender: TObject);
    procedure _ChangeClick(Sender: TObject);
    procedure _SmashClick(Sender: TObject);
    procedure _StripClick(Sender: TObject);
    procedure _FormClick(Sender: TObject);
    procedure _InsertWordClick(Sender: TObject);
    procedure _PushClick(Sender: TObject);
    procedure _HashClick(Sender: TObject);
    procedure _MaskAndZeroClipClick(Sender: TObject);
    procedure _CountClick(Sender: TObject);
    procedure _ExtractWordClick(Sender: TObject);
    procedure _PopWordClick(Sender: TObject);
    procedure _GetPositionClick(Sender: TObject);
    procedure _GetEndClick(Sender: TObject);
    procedure _WordCountClick(Sender: TObject);
    procedure _ChangeWordClick(Sender: TObject);
    procedure _GetLastWordClick(Sender: TObject);
    procedure _GetFirstWordClick(Sender: TObject);
    procedure _RomanumClick(Sender: TObject);
    procedure _ExtractEmailClick(Sender: TObject);
    procedure _AlterClick(Sender: TObject);
    procedure _ReduceClick(Sender: TObject);
    procedure _ExtractNumberClick(Sender: TObject);
    procedure _FindWordClick(Sender: TObject);
    procedure _JoinToClick(Sender: TObject);
    procedure _PackNumClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses jbStr;

procedure TForm1.Button1Click(Sender: TObject);
  function GetRandomWord(iSize:Integer): string;
  var
    I: Integer;
    C: Char;
  begin
    Result := '';
    if iSize <=1 then Exit;
    for I := 1 to iSize do
    begin
      C := ' ';
      while not CharInSet(C, ['A'..'Z']) do
        C := Char(Random(Ord('Z')));
      Result := Result + Trim(C)
    end
  end;
  function GetRandomSentence(AboutWords: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    if AboutWords < 1 then Exit;
    Randomize;
    for I := 1 to AboutWords do
      Result := Result + GetRandomWord(Random(8)) + ' ';
    Result := Trim(Result);
  end;
var
  InSt, OutSt, Overlap, S: string;
  Margin: Integer;
  PadToMargin: Boolean;
  I: Integer;
begin
  PadToMargin := True;
  Margin := 30;
  Overlap := '';
  outst := '';
  InSt := '';
  S := '';
  Overlap := GetRandomSentence(80);
  {test}
  repeat
    InSt := Overlap;
    Overlap := '';
    OutSt := '';
    WordWrap(InSt, OutSt, Overlap, Margin, PadToMargin);
    if OutSt <> '' then
      S := S + OutSt + ccCrLf;
  until Length(Overlap) = 0;
  ShowMessage(S);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
end;

{using constant for test pass}
{Czech code is possible change, table definition is included in pascal text
 and size is 8*128 byte on data}
Const
  cname = 'Joachim Abraham Šikula øeèený Šamšulièíøínek';
  sname = #1#1#1#1#1#1#1#1#1#1'Joachim Šikula' + #1#1#1#1#1#1#1#1#1#1;
  fname = 'c:\upro\libr\great\test\m1y\set\as\test\my.doc';

procedure TForm1._StrUpCaseClick(Sender: TObject);
begin
  NewStr.Caption := cname;
  OrigStr.Caption := WrapQuoteStr(cname);
  NewStr.Caption := WrapQuoteStr(StrUpCase(cname));
  Notes.Caption := 'Mask ()';
end;

procedure TForm1._StrLoCaseClick(Sender: TObject);
begin
  NewStr.Caption := cname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(StrLoCase(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')'
end;

procedure TForm1._CapitalizeClick(Sender: TObject);
begin
  NewStr.Caption := StrLoCase(sname);
  OrigStr.Caption := WrapQuoteStr(Trim(NewStr.Caption));
  NewStr.Caption := WrapQuoteStr(CapitalizeWord(Trim(NewStr.Caption)));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._PathOnlyClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(JustPathName(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._NameOnlyClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(ExtractNameOfFileOnly(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._ExtensionOnlyClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(JustExtension(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._FileNameOnlyClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(JustFileName(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._ShortPathClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(ShortDirName(25, NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._ShortFileNameClick(Sender: TObject);
begin
  NewStr.Caption := fname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(ShortFileName(25, NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._TrimLeftClick(Sender: TObject);
begin
  NewStr.Caption := sname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(TrimLead(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._TrimBothClick(Sender: TObject);
begin
  NewStr.Caption := sname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(Trim(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._TrimRightClick(Sender: TObject);
begin
  NewStr.Caption := sname;
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(TrimTrail(NewStr.Caption));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._PackNumClick(Sender: TObject);
{$IFNDEF CLR}
const
  ccV = '12563254';
var
  S: AnsiString;
  C: AnsiChar;
  Z: string;
{$ENDIF}
begin
{$IFNDEF CLR}
{$IFNDEF UNICODE}
  OrigStr.Caption := WrapQuoteStr(ccV);
  S := PackNum(ccV);
  Z := '';
  for C in S do
    Z := Format('%2x', [Ord(C)]) + Z;
  NewStr.Caption := WrapQuoteStr(Z);
  Notes.Caption := 'revert (' + Z + ' to '  + UnpackNum(S) + ')';
{$ENDIF}
{$ENDIF}
end;

procedure TForm1._PadLeftClick(Sender: TObject);
begin
  NewStr.Caption := Trim(sname);
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(LeftPadCh(NewStr.Caption, '_', 24));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._CenterClick(Sender: TObject);
begin
  NewStr.Caption := Trim(sname);
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := CenterCh(NewStr.Caption, '_', 34);
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._PadRightClick(Sender: TObject);
begin
  NewStr.Caption := Trim(sname);
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := PadCh(NewStr.Caption, '_', 24);
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._ZipClick(Sender: TObject);
begin
  NewStr.Caption := '1234567899955881236AH33';
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(Zip('XXX,XXX,XXX $ XX.XX.XX/XX XX - XX-XX',
    NewStr.Caption));
  Notes.Caption := 'Mask (' + '"XXX,XXX,XXX $ XX.XX.XX/XX XX - XX-XX"' + ')';
end;

procedure TForm1._ChangeClick(Sender: TObject);
begin
  NewStr.Caption := 'Kokorokokoo';
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(Change(NewStr.Caption, 'o', 'u'));
  Notes.Caption := 'Mask (' + '"o"-->"u"' + ')';
end;

procedure TForm1._SmashClick(Sender: TObject);
begin
  NewStr.Caption := '     CU     11  A.  123456   , 11';
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(Smash(' ', NewStr.Caption));
  Notes.Caption := 'Mask (' + '" "' + ')';
end;

procedure TForm1._StripClick(Sender: TObject);
begin
  NewStr.Caption := 'sedesCU.....11.aA.--123456 as,L11';
  OrigStr.Caption := WrapQuoteStr(NewStr.Caption);
  NewStr.Caption := WrapQuoteStr(Strip('     XX     XX  XX  XXXXXX   X XX',
    NewStr.Caption));
  Notes.Caption := 'Mask (' + '"     XX     XX  XX  XXXXXX   X XX"' + ')';
end;

procedure TForm1._FormClick(Sender: TObject);
begin
  OrigStr.Caption := WrapQuoteStr('0.15639');
  NewStr.Caption := WrapQuoteStr(Form('XX.XXXX', 0.15639));
  Notes.Caption := 'Mask (' + 'XX.XXXX' + ')';
end;

procedure TForm1._InsertWordClick(Sender: TObject);
const
  ccV = 'Lidaeip';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(InsWord('aei', 'oo', ccV));
  Notes.Caption := 'Mask (' + '"aei"-->"oo"' + ')';
end;

procedure TForm1._PushClick(Sender: TObject);
const
  ccV = 'Lidoop';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Push('Paar', 12, ccV));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._HashClick(Sender: TObject);
const
  ccV = 'Lidoop';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Long2Str(Hash(ccV)));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._MaskAndZeroClipClick(Sender: TObject);
const
  ccV = 'JEDN-0031-12345';
var
  Next: Integer;
begin
  Next := 1;
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := Format('"%s"', [ZeroClip(Mask('y', 'uuuu-yyyy=iiiii',
        ccV, Next))]);
  Notes.Caption := 'Mask (' + 'uuuu-yyyy=iiiii' + ')';
end;

procedure TForm1._CountClick(Sender: TObject);
Var
  S, S1, S2: String;
  Posic, Len: Integer;
begin
  S := 'This is string for "change" or "store".';
  S1 := '';
  S2 := '';
  OrigStr.Caption := WrapQuoteStr(S);
  Posic := 0;
  { Note: this is use for special scanning for telecomunicacions servicies }
  While Count('"', S, Posic, Len) Do
  Begin
    If S1 = '' Then
    Begin
      S1 := Copy(Copy(S, Posic + Len, 255), 1, Pos
          ('"', Copy(S, Posic + Len, 255)) - 1);
      Inc(Posic, Length(S1) + 1);
    End
    Else If S2 = '' Then
    Begin
      S2 := Copy(Copy(S, Posic + Len, 255), 1, Pos
          ('"', Copy(S, Posic + Len, 255)) - 1);
      Break;
    End;
    Inc(Posic, Len) { for skip over " }
  End;
  { standard string manipulation }
  Posic := Pos(S1, S);
  Len := Pos(S2, S);
  Delete(S, Len, Length(S2));
  Insert(S1, S, Len);
  Delete(S, Posic, Length(S1));
  Insert(S2, S, Posic);
  NewStr.Caption := '"' + S + '"';
  Notes.Caption := 'Note: "' + S1 + '" swap "' + S2 + '"';
end;

procedure TForm1._ExtractWordClick(Sender: TObject);
const
  ccV = 'Honest Abe Lincoln';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(ExtractWord(2, ccV, [' ']));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._PopWordClick(Sender: TObject);
const
  ccV = 'Honest Abe Lincoln';
var
  S: string;
begin
  OrigStr.Caption := WrapQuoteStr(ccV); S := ccV;
  NewStr.Caption := WrapQuoteStr(PopWord(2, S, [' ']));
  Notes.Caption := 'Mask (' + WrapQuoteStr(S) + ')';
end;

procedure TForm1._GetPositionClick(Sender: TObject);
const
  ccV = 'Honest Abe Lincoln';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Long2Str(GetPos(2, ccV, [' '])));
  Notes.Caption := 'Mask (' + 'of "' + ExtractWord(2, ccV, [' ']) + '"' + ')';
end;

procedure TForm1._GetEndClick(Sender: TObject);
const
  ccV = 'Honest Abe Lincoln';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Long2Str(GetEnd(2, ccV, [' '])));
  Notes.Caption := 'Mask (' + 'of "' + ExtractWord(2, ccV, [' ']) + '"' + ')';
end;

procedure TForm1._WordCountClick(Sender: TObject);
const
  ccV = 'Honest Abe Lincoln';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Long2Str(WordCount(ccV, [' '])));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._ChangeWordClick(Sender: TObject);
const
  ccV = 'Anna Hana Jana Dana Marcela Petra';
  ccS = 'Kamila';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := ChangeWord(3, ccS, ccV, [' ']);
  Notes.Caption := 'Mask (' + '"Jana"-->"Kamila"' + ')';
end;

procedure TForm1._GetLastWordClick(Sender: TObject);
const
  ccV = 'Anna Hana Jana Dana Marcela Petra';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(GetLastWord(ccV, [' ']));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._GetFirstWordClick(Sender: TObject);
const
  ccV = 'Anna Hana Jana Dana Marcela Petra';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(GetFirstWord(ccV, [' ']));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1._RomanumClick(Sender: TObject);
begin
  OrigStr.Caption := '"4568" "MDCIX"';
  NewStr.Caption := WrapQuoteStr(Int2Roman(4568)) + ' ' + WrapQuoteStr(IntToStr(Roman2Int('MDCIX')));
  Notes.Caption := 'Mask ("4568" to ' + Int2Roman(4568) + ' and "MDCIX" to ' + IntToStr(Roman2Int('MDCIX')) + ')';
end;

procedure TForm1._ExtractEmailClick(Sender: TObject);
const
  ccV = '<A HREF="mailto:micrel@micrel.cz"><IMG BORDER=0 SRC="email.gif"></A>';
begin
  OrigStr.Caption := ccV;
  NewStr.Caption := WrapQuoteStr(htmlSrcEmail(StrLoCase(ccV)));
  Notes.Caption := 'Mask (' + '' + ')';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF CLR}
  _PackNum.Enabled := False;
  {$ENDIF}
end;

procedure TForm1._AlterClick(Sender: TObject);
begin
  OrigStr.Caption := 'Orig : '#1'    Alternative : Joachim';
  NewStr.Caption := Alter(Trim(#1), 'Joachim');
  Notes.Caption := '';
end;

procedure TForm1._ReduceClick(Sender: TObject);
const
  ccV = 'Anna Hana Jana Dana Marcela Petra';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(Reduce(ccV, 6));
  Notes.Caption := 'Reduce(S,6)';
end;

procedure TForm1._ExtractNumberClick(Sender: TObject);
const
  ccV = 'dgg4fdg7g4h7rt4h4rt7h4t';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  NewStr.Caption := WrapQuoteStr(ExtractNumber(ccV));
  Notes.Caption := 'Note: numbers only';
end;

procedure TForm1._FindWordClick(Sender: TObject);
const
  ccV = 'Anna Hana Jana Dana Marcela Petra';
begin
  OrigStr.Caption := WrapQuoteStr(ccV);
  If FindWord(ExtractWord(4, ccV, [' ']), ccV, [' ']) Then
    NewStr.Caption := WrapQuoteStr(ExtractWord(4, ccV, [' ']));
  Notes.Caption := 'Note: find 4th word of string';
end;

procedure TForm1._JoinToClick(Sender: TObject);
begin
  OrigStr.Caption := 'First: "Anna" Second: "Hana"';
  NewStr.Caption := '"' + JoinTo('Anna', ',', 'Hana') + '"';
  Notes.Caption := 'Delims = ['','']';
end;

end.