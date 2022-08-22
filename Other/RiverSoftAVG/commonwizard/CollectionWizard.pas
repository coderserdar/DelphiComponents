unit CollectionWizard;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG (www.RiverSoftAVG.com).
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//      09/2002 T. Grubb
//              - Fixed bug in TNewCollectionWizard.Execute where if the form file
//                was being displayed, the unit would be unchanged
//                (Thanks Marc S.)
//      07/2002 T. Grubb
//              Initial Version
//
//      File Contents:
//           Classes for the New Collection Wizard
//
//
//--- Warning ----------------------------------------------------------------
//	This software is property of RiverSoftAVG. Unauthorized use or
//      duplication of this software is strictly prohibited. Authorized users
//      are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//      © 2002, Thomas G. Grubb
//
//=== End File Prolog ========================================================

interface

uses
    Windows, Classes, SysUtils, Controls, Forms, ExptIntf, ToolIntf;

type
  TNewCollectionWizard = class(TIExpert)
  public
    { Expert UI strings }
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetStyle: TExpertStyle; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;

    { Launch the Expert }
    procedure Execute; override;
  end; { TNewCollectionWizard }

implementation

uses
    CollectionItemWizMain, mwCachedPasLex, mwPasLexTypes, EditIntf, Dialogs;

resourcestring
  sAuthor = 'RiverSoftAVG';
  sComment = 'Creates New Collection code from your inputs';
  sIDString = 'RiverSoftAVG.New Collection Wizard';
  sName = 'New Collection Wizard';
  sPage = 'New';

type

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  TEditorStrings and TEditReaderStream are taken from "Hidden Paths of Delphi 3"
}
  TEditReaderStream = class(TStream)
  private
    fSize: LongInt;
    fPosition: LongInt;
    fReader: TIEditReader;
    function GetERSize: LongInt;
  public
    constructor Create(EditIntf: TIEditorInterface);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt;
        override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property Size: LongInt read GetERSize;
  end;  TEditorStrings = class(TStrings)
  private
    fStrings: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Str: string); override;
    procedure PutObject(Index: Integer; Obj: TObject); override;
    function GetPosition(Index: Integer): LongInt; virtual;
    function GetCharPos(Index: Integer): TCharPos; virtual;
    property Strings: TStrings read fStrings;
  public
    constructor Create(Editor: TIEditorInterface);
    destructor Destroy; override;
    procedure LoadFromEditor(Editor: TIEditorInterface);
    procedure SaveToEditor(Editor: TIEditorInterface);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Str: string); override;
    function PosToCharPos(Pos: LongInt): TCharPos;
//    function CharPosToPos(CharPos: TCharPos): LongInt;
    property Position[Index: Integer]: LongInt read GetPosition;
    property CharPos[Index: Integer]: TCharPos read GetCharPos;
  end;

{ TNewCollectionWizard }

procedure TNewCollectionWizard.Execute;
   function FindResourceOrType( Lex: TmwCachedPasLex; FindString: String;
                                var InsertString: String;
                                var Span: Integer ): Integer;
   begin
       Span := 0;
       InsertString := NewLineString;
       // find FindString
       result := Lex.FindPattern( 0, FindString, Span, False );
       if result = -1 then        // FindString not found!  try to find end of "uses" statement
       begin
            result := Lex.FindPattern( 0, 'uses', Span, False );
            if result = -1 then   // no uses either!  how about interface?
            begin
                 result := Lex.FindPattern( 0, 'interface', Span, False );
                 if result = -1 then
                 begin
                      InsertString := InsertString + 'interface' + NewLineString + NewLineString;
                 end
                 else
                     InsertString := InsertString + NewLineString;
                 InsertString := InsertString + 'uses' + NewLineString+
                           '  Classes;'+
                           NewLineString+NewLineString;
            end
            else           // uses found, increment i until semi-colon
            begin
                 while Lex.Tokens[result].TokenID <> ptSemiColon do
                       Inc(result);
                 InsertString := InsertString + NewLineString;
            end;
            InsertString := InsertString + FindString + NewLineString;
       end;
   end;
   function InsertResourceString( UnitText, ResourceStr: String ): String;
   var
      Lex: TmwCachedPasLex;
      i: Integer;
      Span: Integer;
      InsertString: String;
   begin
        Lex := TmwCachedPasLex.Create;
        try
           // Parse the pascal file to be able to find the relevant parts
           // of the file to insert our function
           Lex.Origin := @UnitText[1];
           Lex.Parse;
           i := FindResourceOrType( Lex, 'resourcestring', InsertString, Span );
           InsertString := InsertString + ResourceStr;
           if i > -1 then
           begin
                i := Lex.Tokens[i+Span].TokenPos;
                result := Copy(UnitText, 1, i);
                result := result + InsertString + Copy( UnitText, i+1, MaxInt );
           end
           else
               result := InsertString + UnitText;
        finally
           Lex.Free;
        end;
   end;
   function InsertTypeString( UnitText, TypeStr: String ): String;
   var
      Lex: TmwCachedPasLex;
      i: Integer;
      Span: Integer;
      InsertString: String;
   begin
        Lex := TmwCachedPasLex.Create;
        try
           // Parse the pascal file to be able to find the relevant parts
           // of the file to insert our function
           Lex.Origin := @UnitText[1];
           Lex.Parse;
           i := FindResourceOrType( Lex, 'type', InsertString, Span );
           InsertString := InsertString + TypeStr;
           if i > -1 then
           begin
                i := Lex.Tokens[i+Span].TokenPos;
                result := Copy(UnitText, 1, i);
                result := result + InsertString + Copy( UnitText, i+1, MaxInt );
           end
           else
               result := InsertString + UnitText;
        finally
           Lex.Free;
        end;
   end;
   function InsertImplementationString( UnitText, ImplStr: String ): String;
   var
      Lex: TmwCachedPasLex;
      i, j: Integer;
      Span: Integer;
      InsertString: String;
   begin
        Lex := TmwCachedPasLex.Create;
        try
           // Parse the pascal file to be able to find the relevant parts
           // of the file to insert our function
           Lex.Origin := @UnitText[1];
           Lex.Parse;
           InsertString := NewLineString + NewLineString;
           // find implementation
           i := Lex.FindPattern( 0, 'implementation', Span, False );
           if i = -1 then        // implementation not found!  try to find end of "initialization" statement
           begin
                i := Lex.FindPattern( 0, 'initialization', Span, False );
                if i = -1 then   // no initialization either!  how about end.?
                begin
                     i := Lex.FindPattern( 0, 'end.', Span, False );
                     if i = -1 then
                     begin
                          i := Lex.Tokens.Count - 1;
                     end;
                end
                else           // initialization found, insert before
                begin
                     Dec(i);
                end;
                InsertString := InsertString + 'implementation' + NewLineString + NewLineString;
           end
           else // implementation found, if there is a uses clause, go to end
           begin
                j := Lex.FindPattern( i+1, 'uses', Span, False );
                if j > -1 then
                   while Lex.Tokens[i].TokenID <> ptSemiColon do
                         Inc(i);
           end;

           InsertString := InsertString + ImplStr;
           if i > -1 then
           begin
                i := Lex.Tokens[i+Span].TokenPos;
                result := Copy(UnitText, 1, i);
                result := result + InsertString + Copy( UnitText, i+1, MaxInt );
           end
           else
               result := InsertString + UnitText;
        finally
           Lex.Free;
        end;
   end;
   function InsertInitializationString( UnitText, InitStr: String ): String;
   var
      Lex: TmwCachedPasLex;
      i, j: Integer;
      Span: Integer;
      InsertString: String;
   begin
        Lex := TmwCachedPasLex.Create;
        try
           // Parse the pascal file to be able to find the relevant parts
           // of the file to insert our function
           Lex.Origin := @UnitText[1];
           Lex.Parse;
           InsertString := '';
           // find implementation
           i := Lex.FindPattern( 0, 'initialization', Span, False );
           if i = -1 then        // initialization not found!  try to find end of "end." statement
           begin
                i := Lex.FindPattern( 0, 'end.', Span, False );
                if i = -1 then   // no end. either!  how about end.?
                begin
                     i := Lex.Tokens.Count - 1;
                end
                else           // end found, insert before
                begin
                     Dec(i);
                end;
                InsertString := NewLineString + 'initialization' + NewLineString +
                  '{'+NewLineString+
                  '  Component writers call RegisterClass to register custom classes so that the'+NewLineString+
                  '  class type can be obtained from the class name by calling the GetClass'+NewLineString+
                  '  function. If a class is not registered, GetClass returns nil when passed the'+NewLineString+
                  '  class name.  This is important for read/write from/to streams.'+NewLineString+
                  '}'+
                  NewLineString + NewLineString;
           end
           else   // initialization section found, increment i until 'end.' or 'finalization'
           begin
                j := Lex.FindPattern( i+1, 'finalization', Span, False );
                if j = -1 then
                begin
                     j := Lex.FindPattern( i+1, 'end.', Span, False );
                     if j = -1 then
                        Dec(i)
                     else
                         i := j-1;
                end
                else
                    i := j - 1;
           end;
           InsertString := InsertString + InitStr;
           if i > -1 then
           begin
                i := Lex.Tokens[i+Span].TokenPos;
                result := Copy(UnitText, 1, i);
                result := result + InsertString + Copy( UnitText, i+1, MaxInt );
           end
           else
               result := InsertString + UnitText;
        finally
           Lex.Free;
        end;
   end;
   function EndsText( const SubText, Text: String ): Boolean;
   var
      i: Integer;
   begin
        i := Length(Text) - Length(SubText) + 1;
        result := i >= 0;
        if result and (SubText <> '') then
           result := CompareText(SubText, Copy(Text,i,Length(SubText))) = 0;
   end;
var
   UnitText: String;
   ModuleIntf: TIModuleInterface;
   EditorIntf: TIEditorInterface;
   EditorStrings: TEditorStrings;
begin
     frmNewCollection := TfrmNewCollection.Create(Application);
     try
        if frmNewCollection.ShowModal = mrOk then
        begin
             // get the current view
             ModuleIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile);
             try
                if ModuleIntf = nil then
                begin
                     // if file not found, it may be because the user is showing
                     // a form file, try and get the pas file
                     UnitText := ToolServices.GetCurrentFile;
                     if (Length(UnitText) > 4) and
                        (EndsText('.dfm',UnitText) or EndsText('.xfm',UnitText)) then
                        UnitText := Copy(UnitText, 1, Length(UnitText) - 3) + 'pas';
                     ModuleIntf := ToolServices.GetModuleInterface(UnitText);
                     if ModuleIntf <> nil then
                        ModuleIntf.ShowSource;
                end;
                if ModuleIntf <> nil then
                begin
                     // get the editor
                     EditorIntf := ModuleIntf.GetEditorInterface;
                     try
                        if EditorIntf <> nil then
                        begin
                             EditorStrings := TEditorStrings.Create( EditorIntf );
                             try
                                UnitText := EditorStrings.Text;
                                UnitText := InsertTypeString( UnitText, frmNewCollection.InterfaceStr );
//                                UnitText := InsertResourceString( UnitText );
                                UnitText := InsertImplementationString( UnitText, frmNewCollection.ImplementationStr );
//                                UnitText := InsertInitializationString( UnitText );
                                EditorStrings.Text := UnitText;
                                EditorStrings.SaveToEditor( EditorIntf );
                             finally
                                EditorStrings.Free;
                             end;
                        end
                        else
                            ShowMessage('Missing Editor Interface');
                     finally
                        EditorIntf.Free;
                     end;
               end
               else
                   ShowMessage('Missing Unit to Edit');
             finally
                ModuleIntf.Free;
             end;
        end;
     finally
        frmNewCollection.Free;
     end;
end;

function TNewCollectionWizard.GetAuthor: string;
begin
     result := sAuthor;
end;

function TNewCollectionWizard.GetComment: string;
begin
     result := sComment;
end;

function TNewCollectionWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'MAINICON')
end;

function TNewCollectionWizard.GetIDString: string;
begin
     result := sIDString;
end;

function TNewCollectionWizard.GetMenuText: string;
begin
     result := '';
end;

function TNewCollectionWizard.GetName: string;
begin
     result := sName;
end;

function TNewCollectionWizard.GetPage: string;
begin
     result := sPage;
end;

function TNewCollectionWizard.GetState: TExpertState;
begin
     result := [];
end;

function TNewCollectionWizard.GetStyle: TExpertStyle;
begin
     result := esForm;
end;

{ TEditReaderStream }
{ Construct the stream from an editor interface. }
constructor TEditReaderStream.Create(EditIntf: TIEditorInterface);
begin
  inherited Create;
  fReader := EditIntf.CreateReader;
  fSize := -1; { size is unknown }
end;

{ Destroy the stream and free all the interfaces that the stream
  created. }
destructor TEditReaderStream.Destroy;
begin
  fReader.Free;
  inherited Destroy;
end;
{ Read from the file stream or the editor. }
function TEditReaderStream.Read(var Buffer; Count: LongInt): LongInt;
const
  MaxCount = 31*1024;
var
  NRead: Integer;
  NRequest: Integer;
  BufPtr: PChar;
begin
  { The initial release of D3 does not handle calls to GetText
    where Count >= 32K. It returns a result equal to Count
    without actually retrieving any text. To circumvent this
    problem, grab buffers of 31K at a time. }
  Result := 0;
  BufPtr := @Buffer;
  while Count > 0 do
  begin
    if Count > MaxCount then
      NRequest := MaxCount
    else
      NRequest := Count;
    NRead := fReader.GetText(fPosition, BufPtr, NRequest);
    Inc(fPosition, NRead);
    Inc(BufPtr, NRead);
    Inc(Result, NRead);
    Dec(Count, NRead);
    { Partially completed read means end-of-buffer, so remember
      the buffer size. If NRead = 0, the position might be past
      the end of file, so save the size only when NRead > 0. }
    if (fSize < 0) and (NRead > 0) and (NRead < NRequest) then
      fSize := fPosition;
  end;
end;

{ Seek to a new position. }
function TEditReaderStream.Seek(Offset: LongInt; Origin: Word):
  LongInt;
begin
  case Origin of
  soFromBeginning:    fPosition := Offset;
  soFromCurrent:      fPosition := fPosition + Offset;
  soFromEnd:          fPosition := Size + Offset;
  else
    raise Exception.CreateFmt('Invalid seek origin, %d', [Origin]);
  end;
  Result := fPosition;
end;

function TEditReaderStream.Write(const Buffer; Count: LongInt):
  LongInt;
begin
  raise Exception.Create('Attempt to write to readonly stream!');
end;
{ If the stream user must seek relative to the end of the
  stream, then you need to know the size of the stream.
  There is no simple way to determine this. Instead, use
  a binary search to find a position where a single byte
  read is valid, and a read of the subsequent byte is invalid.
  Since this is such a pain, cache the size after the first call,
  and return the cached size for subsequent calls. }
function TEditReaderStream.GetERSize: LongInt;
var
  Hi, Lo, Mid: LongInt;
  Ch: Char;
begin
  if fSize < 0 then
  begin
    Hi := High(LongInt);
    Lo := 0;
    while Lo <= Hi do
    begin
      Mid := (Hi + Lo) div 2;
      if fReader.GetText(Mid, @Ch, 1) = 1 then
        Lo := Mid+1
      else
        Hi := Mid-1;
    end;
    fSize := Lo;
  end;
  Result := fSize;
end;

{ TEditorStrings }
{ Create an edit reader string list. }
constructor TEditorStrings.Create(Editor: TIEditorInterface);
begin
  inherited Create;
  fStrings := TStringList.Create;
  LoadFromEditor(Editor);
end;

destructor TEditorStrings.Destroy;
begin
  Strings.Free;
  inherited Destroy;
end;

{ Load a string list from an editor interface. Read the edit
  reader as a stream. As each line is added to the string list,
  remember the position of that line in the stream. }
procedure TEditorStrings.LoadFromEditor(Editor: TIEditorInterface);
var
  ERStream: TEditReaderStream;
  StrStream: TStringStream;
  Str: PChar;
  Pos, I: Integer;
begin
  ERStream := TEditReaderStream.Create(Editor);
  try
    StrStream := TStringStream.Create('');
    try
      { Read the entire buffer into StrStream. }
      StrStream.CopyFrom(ERStream, 0);
      { Copy every line from StrStream to the string list. }
      Strings.Text := StrStream.DataString;

      { Scan the string to find the buffer position of each line. }
      Str := PChar(StrStream.DataString);
      Pos := 0;
      for I := 0 to Count-1 do
      begin
        Strings.Objects[I] := TObject(Pos);
        Inc(Pos, Length(Strings[I]));
        if Str[Pos] = #13 then
          Inc(Pos);
        if Str[Pos] = #10 then
          Inc(Pos);
      end;
    finally
      StrStream.Free;
    end;
  finally
    ERStream.Free;
  end;
end;

{ Save the string list to an editor interface. The string list
  does not keep track of specific changes, so replace the entire
  file with the text of the string list. }
procedure TEditorStrings.SaveToEditor(Editor: TIEditorInterface);
var
  Writer: TIEditWriter;
begin
  Writer := Editor.CreateUndoableWriter;
  try
    Writer.DeleteTo(High(LongInt));
    Writer.Insert(PChar(fStrings.Text));
  finally
    Writer.Free;
  end;
end;

{ Get a string. }
function TEditorStrings.Get(Index: Integer): string;
begin
  Result := Strings[Index]
end;

{ Get an object, which is really the string position. }
function TEditorStrings.GetObject(Index: Integer): TObject;
begin
  Result := Strings.Objects[Index]
end;

{ Set a string. }
procedure TEditorStrings.Put(Index: Integer; const Str: string);
begin
  Strings[Index] := Str
end;

{ Set a string's position. }
procedure TEditorStrings.PutObject(Index: Integer; Obj: TObject);
begin
  Objects[Index] := Obj;
end;

{ Return the number of lines in the list. }
function TEditorStrings.GetCount: Integer;
begin
  Result := Strings.Count
end;

procedure TEditorStrings.Clear;
begin
  Strings.Clear;
end;

procedure TEditorStrings.Delete(Index: Integer);
begin
  Strings.Delete(Index)
end;

procedure TEditorStrings.Insert(Index: Integer; const Str: string);
begin
  Strings.Insert(Index, Str);
end;

{ For convenience, return a position as an integer. }
function TEditorStrings.GetPosition(Index: Integer): LongInt;
begin
  Result := LongInt(Strings.Objects[Index]);
end;

{ Return a position as a character position. }
function TEditorStrings.GetCharPos(Index: Integer): TCharPos;
begin
  Result := PosToCharPos(GetPosition(Index));
end;

{ Convert a buffer position to a character position.
  Search for the line such that Pos is between the start
  and end positions of the line. That specifies the line
  number. The char index is the offset within the line.
  If Pos lies within a line ending, return the character
  index of the end of the line.

  Line indices are 1-based, and string list indices are
  0-based, so add 1 to get the true line number.

  Use binary search to locate the desired line quickly. }
function TEditorStrings.PosToCharPos(Pos: LongInt): TCharPos;
var
  Lo, Mid, Hi: Integer;
begin
  Lo := 0;
  Hi := Strings.Count-1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Position[Mid] <= Pos then
      Lo := Mid+1
    else
      Hi := Mid-1
  end;

  Result.Line := Lo;
  if Pos >= Position[Lo-1]+Length(Strings[Lo-1]) then
    Result.CharIndex := Length(Strings[Lo-1])
  else
    Result.CharIndex := Pos - Position[Lo-1];
end;

end.
