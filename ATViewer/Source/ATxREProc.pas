{$I ATViewerOptions.inc}

unit ATxREProc;

interface

uses
  Windows, Classes, ComCtrls;

function RE_CurrentLine(Edit: TRichEdit): Integer;
function RE_LineFromPos(Edit: TRichEdit; Pos: Integer): Integer;
function RE_PosFromLine(Edit: TRichEdit; Line: Integer): Integer;

procedure RE_ScrollToStart(Edit: TRichEdit);
procedure RE_ScrollToEnd(Edit: TRichEdit);
procedure RE_ScrollToPos(Edit: TRichEdit; Pos, IndentVert, IndentHorz: Integer);
procedure RE_ScrollToLine(Edit: TRichEdit; Line, Indent: Integer);
procedure RE_ScrollToPercent(Edit: TRichEdit; N: Integer);

procedure RE_LoadStream(Edit: TRichEdit;
  AStream: TStream;
  ASelStart, ASelLength: Integer);
procedure RE_LoadFile(Edit: TRichEdit;
  const AFileName: WideString;
  ASelStart, ASelLength: Integer);

procedure RE_Print(Edit: TRichEdit;
  AOnlySel: Boolean;
  ACopies: Integer;
  const ACaption: AnsiString);

type
  TRELastSearch = record
    ResultStart,
    ResultLength: Integer;
    SavedText: AnsiString;
    SavedOptions: TSearchTypes;
  end;

function RE_FindFirst(
  AEdit: TRichEdit;
  const AText: AnsiString;
  AStartPos: Integer;
  AOptions: TSearchTypes;
  AIndentVert,
  AIndentHorz: Integer;
  var ALastSearch: TRELastSearch): Boolean;

function RE_FindNext(
  AEdit: TRichEdit;
  AIndentVert,
  AIndentHorz: Integer;
  var ALastSearch: TRELastSearch): Boolean;


implementation

uses
  SysUtils, Messages,
  {$ifdef TNT} TntClasses, {$endif}
  Printers, RichEdit,
  ATxSProc, ATxFProc, ATViewerMsg;


function RE_CurrentLine(Edit: TRichEdit): Integer;
begin
  Result := Edit.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
end;

function RE_LineFromPos(Edit: TRichEdit; Pos: Integer): Integer;
begin
  Result := Edit.Perform(EM_EXLINEFROMCHAR, 0, Pos);
end;

function RE_PosFromLine(Edit: TRichEdit; Line: Integer): Integer;
begin
  Result := Edit.Perform(EM_LINEINDEX, Line, 0);
end;

procedure RE_HScroll(Edit: TRichEdit; HPos: Integer);
begin
  Edit.Perform(WM_HSCROLL, SB_THUMBPOSITION + (HPos shl 16), 0);
end;

function RE_HPos(Edit: TRichEdit; Pos: Integer): Integer;
var
  Pnt: TPoint;
begin
  FillChar(Pnt, SizeOf(Pnt), 0);
  Edit.Perform(EM_POSFROMCHAR, Integer(@Pnt), Pos);
  Result := Pnt.X;
end;


procedure RE_ScrollToStart(Edit: TRichEdit);
var
  n: DWORD;
begin
  repeat
    n := Edit.Perform(EM_SCROLL, SB_PAGEUP, 0);
  until (n and $FFFF) = 0;

  RE_HScroll(Edit, 0);
end;

procedure RE_ScrollToEnd(Edit: TRichEdit);
var
  n: DWORD;
begin
  repeat
    n := Edit.Perform(EM_SCROLL, SB_PAGEDOWN, 0);
  until (n and $FFFF) = 0;

  RE_HScroll(Edit, 0);
end;

procedure RE_ScrollToPos(Edit: TRichEdit; Pos, IndentVert, IndentHorz: Integer);
var
  LineNum,
  LineStart,
  Pos2: Integer;
begin
  RE_ScrollToStart(Edit);
  
  LineNum := RE_LineFromPos(Edit, Pos);
  LineStart := RE_PosFromLine(Edit, LineNum);

  Dec(LineNum, IndentVert);
  if LineNum < 0 then LineNum := 0;

  Edit.Perform(EM_LINESCROLL, 0, LineNum);

  //Horz scroll
  if RE_HPos(Edit, Pos) >= Edit.ClientWidth then
  begin
    Pos2 := Pos - IndentHorz;
    if Pos2 < LineStart then Pos2 := LineStart;
    RE_HScroll(Edit, RE_HPos(Edit, Pos2));
  end;

  //Caret
  Edit.SelStart := LineStart;
  Edit.SelLength := 0;
end;

procedure RE_ScrollToLine(Edit: TRichEdit; Line, Indent: Integer);
begin
  RE_ScrollToStart(Edit);
  Dec(Line, Indent);
  if Line < 0 then Line := 0;
  Edit.Perform(EM_LINESCROLL, 0, Line);

  //Caret
  Edit.SelStart := RE_PosFromLine(Edit, Line);
  Edit.SelLength := 0;
end;


procedure RE_ScrollToPercent(Edit: TRichEdit; N: Integer);
var
  Num: Integer;
begin
  if N <= 0 then
  begin
    RE_ScrollToStart(Edit);
    Edit.SelStart := 0;
    Edit.SelLength := 0;
  end
  else
  if N >= 100 then
  begin
    RE_ScrollToEnd(Edit);
    Edit.SelStart := RE_PosFromLine(Edit, Edit.Lines.Count - 1);
    Edit.SelLength := 0;
  end
  else
  begin
    Num := (Edit.Lines.Count - 1) * N div 100;
    if Num > 0 then
    begin
      RE_ScrollToLine(Edit, Num, 0);
      Edit.SelStart := RE_PosFromLine(Edit, Num);
      Edit.SelLength := 0;
    end;
  end;
end;

procedure RE_LoadStream(Edit: TRichEdit; AStream: TStream; ASelStart, ASelLength: Integer);
begin
  AStream.Position := 0;
  with Edit do
  begin
    MaxLength := MaxInt - 2; //fix to load 70K file
    Lines.Clear;
    Lines.LoadFromStream(AStream);

    RE_ScrollToStart(Edit);
    SelStart := ASelStart;
    SelLength := ASelLength;
  end;
end;


procedure RE_LoadFile(Edit: TRichEdit; const AFileName: WideString; ASelStart, ASelLength: Integer);
const
  Sign = #$EF#$BB#$BF;
var
  IsRTF, IsUTF8: Boolean;
  StreamFile, StreamData: TStream;
begin
  IsFileRTFAndUTF8(AFileName, IsRTF, IsUTF8);
  Edit.PlainText := not IsRTF;

  with Edit do
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
      (not IsRTF) and (not IsUTF8) then
    //Treat file as UTF8 and add missing UTF8 signature
    begin
      StreamFile := {$ifdef TNT}TTntFileStream{$else}TFileStream{$endif}.Create(AFileName, fmOpenRead or fmShareDenyNone);
      StreamData := TMemoryStream.Create;
      try
        StreamData.WriteBuffer(Sign, Length(Sign));
        StreamData.CopyFrom(StreamFile, 0);
        RE_LoadStream(Edit, StreamData, 0, 0);
      finally
        StreamFile.Free;
        StreamData.Free;
      end;
    end
    else
    //Treat file as is, RTF of UTF8 (UTF8 signature is present)
    begin
      StreamFile := {$ifdef TNT}TTntFileStream{$else}TFileStream{$endif}.Create(AFileName, fmOpenRead or fmShareDenyNone);
      try
        RE_LoadStream(Edit, StreamFile, 0, 0);
      finally
        StreamFile.Free;
      end;
    end;

  RE_ScrollToStart(Edit);
  Edit.SelStart := ASelStart;
  Edit.SelLength := ASelLength;
end;


procedure RE_Print(Edit: TRichEdit;
  AOnlySel: Boolean;
  ACopies: Integer;
  const ACaption: AnsiString);
var
  ASelStart, ASelLength: Integer;
begin
  if ACopies <= 0 then
    ACopies := 1;

  Printer.Copies := ACopies;
  Printer.Canvas.Font := Edit.Font;

  if AOnlySel then
  begin
    ASelStart := Edit.SelStart;
    ASelLength := Edit.SelLength;
    Edit.SelStart := ASelStart + ASelLength;
    Edit.SelLength := MaxInt;
    Edit.SelText := '';
    Edit.SelStart := 0;
    Edit.SelLength := ASelStart;
    Edit.SelText := '';
  end;

  Edit.Print(ACaption);
end;


function RE_FindText(
  AEdit: TRichEdit;
  const AText: AnsiString;
  AStartPos: Integer;
  AOptions: TSearchTypes;
  AIndentVert, AIndentHorz: Integer;
  var ALastSearch: TRELastSearch): Boolean;
var
  Pos: Integer;
begin
  with ALastSearch do
  begin
    ResultStart := -1;
    ResultLength := 0;
    SavedText := AText;
    SavedOptions := AOptions;
  end;

  Pos := AEdit.FindText(AText, AStartPos, MaxInt - AStartPos, AOptions);
  Result := Pos >= 0;

  if Result then
  begin
    ALastSearch.ResultStart := Pos;
    ALastSearch.ResultLength := Length(AText);

    AEdit.Lines.BeginUpdate;

    //We select text *after* scrolling
    AEdit.SelStart := ALastSearch.ResultStart;
    RE_ScrollToPos(AEdit, ALastSearch.ResultStart, AIndentVert, AIndentHorz);
    AEdit.SelStart := ALastSearch.ResultStart;
    AEdit.SelLength := ALastSearch.ResultLength;

    AEdit.Lines.EndUpdate;
  end;
end;


function RE_FindFirst(
  AEdit: TRichEdit;
  const AText: AnsiString;
  AStartPos: Integer;
  AOptions: TSearchTypes;
  AIndentVert, AIndentHorz: Integer;
  var ALastSearch: TRELastSearch): Boolean;
begin
  Result := RE_FindText(
    AEdit,
    AText,
    AStartPos,
    AOptions,
    AIndentVert,
    AIndentHorz,
    ALastSearch);
end;

function RE_FindNext(
  AEdit: TRichEdit;
  AIndentVert, AIndentHorz: Integer;
  var ALastSearch: TRELastSearch): Boolean;
begin
  with ALastSearch do
  begin
    Assert(SavedText <> '', 'Search text is empty');
    Result := RE_FindText(
      AEdit,
      SavedText,
      ResultStart + 1,
      SavedOptions,
      AIndentVert,
      AIndentHorz,
      ALastSearch);
  end;
end;


end.
