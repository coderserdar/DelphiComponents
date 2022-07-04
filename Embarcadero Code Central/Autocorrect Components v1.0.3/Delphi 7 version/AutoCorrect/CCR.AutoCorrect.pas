{**************************************************************************************}
{                                                                                      }
{ AutoCorrect components for Delphi 7                                                  }
{ Version 1.0.1 (2009-08-26)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.AutoCorrect.pas.                                            }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.AutoCorrect;
{
  While this will compile in more recent Delphis, I advise you to use the D2005+ version
  of the code if you are not running Delphi 7.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ComCtrls, IniFiles;

type
  TAutoCorrectOption = (acAutoIndent{*}, acConvertHyphensToDashes,
    acCorrectTwoInitialCaps, acCurlQuoteChars, acPreferFractionChars,
    acUseCustomEntries); //{*} = concerns rich edits only
  TAutoCorrectOptions = set of TAutoCorrectOption;

const
  AllAutoCorrectOptions = [Low(TAutoCorrectOption)..High(TAutoCorrectOption)];

type
  TAutoCorrectEngine = class;

  EDuplicateAutoCorrectEntry = class(Exception);

  TAutoCorrectEntry = class(TCollectionItem)
  private
    FFindText: string;
    FReplaceText: string;
    FFontColorToSet: TColor;
    FFontNameToSet: TFontName;
    FFontSizeToSet: Integer;
    FHasFormatting: (hfDontKnow, hfNo, hfYes);
    FHighlightColorToSet: TColor;
    FStylesToAdd: TFontStyles;
    procedure SetFindText(const Value: string);
    procedure SetFontColorToSet(const Value: TColor);
    procedure SetFontNameToSet(const Value: TFontName);
    procedure SetFontSizeToSet(const Value: Integer);
    procedure SetHighlightColorToSet(const Value: TColor);
    procedure SetReplaceText(const Value: string);
    procedure SetStylesToAdd(const Value: TFontStyles);
    function IsReplaceTextStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(ACollection: TCollection; const AFindText, AReplaceText: string); reintroduce; overload;
    constructor Create(const AFindText, AReplaceText: string); reintroduce; overload;
    procedure ApplyFormatting(RichEditWnd: HWND); overload;
    procedure ApplyFormatting(Dest: TCustomRichEdit); overload; 
    function HasFormatting: Boolean;
  published
    property FindText: string read FFindText write SetFindText;
    property ReplaceText: string read FReplaceText write SetReplaceText stored IsReplaceTextStored;
    property FontColorToSet: TColor read FFontColorToSet write SetFontColorToSet default clNone;
    property FontNameToSet: TFontName read FFontNameToSet write SetFontNameToSet;
    property FontSizeToSet: Integer read FFontSizeToSet write SetFontSizeToSet default 0;
    property HighlightColorToSet: TColor read FHighlightColorToSet write SetHighlightColorToSet default clNone;
    property StylesToAdd: TFontStyles read FStylesToAdd write SetStylesToAdd default [];
  end;

  TAutoCorrectEntries = class(TCollection)
  private
    FFindTextHash: TStringHash;
    FOwner: TAutoCorrectEngine;
    function GetItem(Index: Integer): TAutoCorrectEntry;
    procedure SetItem(Index: Integer; Value: TAutoCorrectEntry);
  protected
    procedure FindTextChanging(Entry: TAutoCorrectEntry; const NewFindText: string);
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TAutoCorrectEngine);
    destructor Destroy; override;
    function Add: TAutoCorrectEntry; overload;
    function Add(const FindText: string; const ReplaceText: string = ''): TAutoCorrectEntry; overload;
    function Find(const FindText: string; out Entry: TAutoCorrectEntry): Boolean;
    property Items[Index: Integer]: TAutoCorrectEntry read GetItem write SetItem; default;
  end;

  TAutoIndentSize = 1..MaxInt;

  TAutoCorrectionKind = (ckIndentation, ckHyphenToDash, ckTwoInitialCaps,
    ckCurlQuoteChar, ckFractionChar, ckCustomEntry);

  TAutoCorrectionMadeEvent = procedure (Sender: TAutoCorrectEngine;
    CorrectionKind: TAutoCorrectionKind; Control: TWinControl;
    Entry: TAutoCorrectEntry) of object; //Entry will always be valid
  TCheckForTriggerKeyEvent = procedure (Sender: TAutoCorrectEngine; Control: TWinControl;
    Key: Char; var IsTriggerKey: Boolean) of object;
  TUndoTwoInitialCapsCorrection = procedure (Sender: TAutoCorrectEngine;
    const Word: string; var AddToIgnoredList: Boolean) of object;

  IAutoCorrectEngine = interface
  ['{2FA1BAAF-75D5-4B75-B33E-2C2CD9168FE3}']
    procedure KeyDownOccurred(Control: TWinControl; var Key: Word; Shift: TShiftState);
    procedure KeyPressOccurred(Control: TWinControl; var Key: Char);
    procedure UndoOccurred(Control: TWinControl);
  end;

  TACControlInfo = record
    Source: TWinControl;
    EditWnd: HWND;
    IsRichEdit: Boolean;
  end;

  TLoadFromIniFileBehaviour = (lbClearFirst, lbKeepExisting);

  TAutoCorrectEngine = class(TComponent, IAutoCorrectEngine)
  private
    FBackSpaceEntry, FLeftSingleQuoteEntry, FRightSingleQuoteEntry, FLeftDoubleQuoteEntry,
      FRightDoubleQuoteEntry, FTabEntry, FTwoInitialCapsEntry: TAutoCorrectEntry;
    FAutoIndentSize: TAutoIndentSize;
    FCustomEntries: TAutoCorrectEntries;
    FEntryCache: array[Char] of TList; //entries are indexed by their final character
    FHasEntryCache: Boolean;
    FIgnoredTwoInitialCaps: TStrings;
    FLastInitialCapsControl: TWinControl;
    FMaxWordOrCachedEntryLen: Integer;
    FModified: Boolean;
    FOptions: TAutoCorrectOptions;
    FOnCorrectionMade: TAutoCorrectionMadeEvent;
    FOnCheckForTriggerKey: TCheckForTriggerKeyEvent;
    FOnUndoTwoInitialCapsCorrection: TUndoTwoInitialCapsCorrection;
    function AddToEntryCache(Kind: TAutoCorrectionKind; const AFindText, AReplaceText: string): TAutoCorrectEntry; overload;
    procedure AddToEntryCache(Kind: TAutoCorrectionKind; Entry: TAutoCorrectEntry); overload;
    procedure CheckForAnyCorrection(Control: TWinControl; var Key: Char);
    procedure CheckForAutoIndent(const Info: TACControlInfo; var Key: Char);
    procedure CheckForCurlQuote(const Info: TACControlInfo; var Key: Char);
    procedure SetIgnoredTwoInitialCaps(Value: TStrings);
    procedure SetCustomEntries(Value: TAutoCorrectEntries);
    procedure SetOptions(Value: TAutoCorrectOptions);
  protected
    procedure CorrectionMade(Kind: TAutoCorrectionKind; Control: TWinControl;
      Entry: TAutoCorrectEntry); virtual;
    procedure CheckForTriggerKey(Control: TWinControl; Key: Char;
      var IsTriggerKey: Boolean); virtual; //just calls out to the OnCheckForTriggerKey event handler by default
    function GetDummyEntry(Key: Char): TAutoCorrectEntry; 
    procedure EntryCacheNeeded;
    procedure FreeEntryCache;
    procedure Loaded; override;
    property HasEntryCache: Boolean read FHasEntryCache;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromIniFile(IniFile: TCustomIniFile;
      Behaviour: TLoadFromIniFileBehaviour); overload;
    procedure LoadFromIniFile(const FileName: string;
      Behaviour: TLoadFromIniFileBehaviour); overload;
    procedure SaveToIniFile(IniFile: TCustomIniFile); overload;
    procedure SaveToIniFile(const FileName: string); overload; //saves to a UTF-8 INI file
    function IsTriggerKey(Control: TWinControl; Key: Char): Boolean;
    procedure KeyDownOccurred(Control: TWinControl; var Key: Word; Shift: TShiftState);
    procedure KeyPressOccurred(Control: TWinControl; var Key: Char);
    procedure UndoOccurred(Control: TWinControl);
    property Modified: Boolean read FModified write FModified;
  published
    property AutoIndentSize: TAutoIndentSize read FAutoIndentSize write FAutoIndentSize default 10;
    property CustomEntries: TAutoCorrectEntries read FCustomEntries write SetCustomEntries;
    property IgnoredTwoInitialCaps: TStrings read FIgnoredTwoInitialCaps
      write SetIgnoredTwoInitialCaps;
    property Options: TAutoCorrectOptions read FOptions write SetOptions default AllAutoCorrectOptions;
    property OnCorrectionMade: TAutoCorrectionMadeEvent read FOnCorrectionMade write FOnCorrectionMade;
    property OnCheckForTriggerKey: TCheckForTriggerKeyEvent read FOnCheckForTriggerKey write FOnCheckForTriggerKey;
    property OnUndoTwoInitialCapsCorrection: TUndoTwoInitialCapsCorrection
      read FOnUndoTwoInitialCapsCorrection write FOnUndoTwoInitialCapsCorrection;
  end;

procedure OffsetFirstIndent(RichEditWnd: HWND; Amount: Integer); overload;
procedure OffsetFirstIndent(RichEdit: TCustomRichEdit; Amount: Integer);  overload;
procedure OffsetLeftIndent(RichEditWnd: HWND; Amount: Integer); overload;
procedure OffsetLeftIndent(RichEdit: TCustomRichEdit; Amount: Integer);  overload;

const
  EnDash = '–';
  EmDash = '—';
  TabChar = #9;
  LeftSingleQuote = '‘';
  LeftDoubleQuote = '“';
  RightSingleQuote = '’';
  RightDoubleQuote = '”';

implementation

uses
  Contnrs, Math, RichEdit, Themes, CCR.AutoCorrect.Consts;

const
  MaxWordLength = 48; //used by the TWo INitial CAps correction code; if a custom entry
                      //is longer, then that length will be used instead
type
  ITextDocument = interface(IDispatch)
    ['{8CC497C0-A1DF-11CE-8098-00AA0047BE5D}']
    function GetName: WideString; safecall;
    function GetSelection: IUnknown; safecall;
    function GetStoryCount: Integer; safecall;
    function GetStoryRanges: IUnknown; safecall;
    function GetSaved: Integer; safecall;
    procedure SetSaved(pValue: Integer); safecall;
    function GetDefaultTabStop: Single; safecall;
    procedure SetDefaultTabStop(pValue: Single); safecall;
    procedure New; safecall;
    procedure Open(var pVar: OleVariant; Flags: Integer; CodePage: Integer); safecall;
    procedure Save(var pVar: OleVariant; Flags: Integer; CodePage: Integer); safecall;
    function Freeze: Integer; safecall;
    function Unfreeze: Integer; safecall;
  end; //there are more methods, but that's all we need

  TTextRange = RichEdit.TEXTRANGE;

  TCustomComboAccess = class(TCustomCombo);
  TCustomEditAccess = class(TCustomEdit);
  TCustomMemoAccess = class(TCustomMemo);

  TIgnoredTwoInitialCaps = class(TStringList)
  protected
    FOwner: TAutoCorrectEngine;
    procedure Changed; override;
    procedure InsertItem(Index: Integer; const S: string;
      AObject: TObject); override;
  public
    constructor Create(AOwner: TAutoCorrectEngine);
  end;

  TEditControlFreezer = class(TInterfacedObject)
  private
    FFrozen: Boolean;
    FTextDocument: ITextDocument;
  public
    constructor Create(EditWnd: HWND; IsRichEdit: Boolean);
    destructor Destroy; override;
    procedure Unfreeze;
  end;

  TEntryCacheItem = class
  private
    FCaseSensitiveFind: Boolean;
    FCaseSensitiveReplace: Boolean;
    FEntry: TAutoCorrectEntry;
    FKind: TAutoCorrectionKind;
  public
    constructor Create(AEntry: TAutoCorrectEntry; AKind: TAutoCorrectionKind);
    destructor Destroy; override;
    property CaseSensitiveFind: Boolean read FCaseSensitiveFind;
    property CaseSensitiveReplace: Boolean read FCaseSensitiveReplace;
    property Kind: TAutoCorrectionKind read FKind;
    property Entry: TAutoCorrectEntry read FEntry;
  end;

{$IF NOT DECLARED(TCharacter)}
function IsLetterOrDigit(Ch: Char): Boolean; 
begin
  Result := IsCharAlphaNumeric(Ch);
end;

function ToLower(Ch: Char): Char;
var
  PtrValue: PChar;
begin
  PtrValue := nil;
  Move(Ch, PtrValue, SizeOf(Ch));
  Result := Char(CharLower(PtrValue));
end;

function ToUpper(Ch: Char): Char;
var
  PtrValue: PChar;
begin
  PtrValue := nil;
  Move(Ch, PtrValue, SizeOf(Ch));
  Result := Char(CharUpper(PtrValue));
end;
{$IFEND}

function GetTextRange(const Info: TACControlInfo; StartPos, EndPos: Integer): string;
var
  TextHandle: HLOCAL;
  TextRange: TTextRange;
  TotalLen: Integer;
  WideSeekPtr: PWideChar;
begin
  Result := '';
  if StartPos < 0 then StartPos := 0;
  if (EndPos - StartPos) <= 0 then Exit;
  if Info.IsRichEdit then
  begin
    TextRange.chrg.cpMin := StartPos;
    TextRange.chrg.cpMax := EndPos;
    SetLength(Result, EndPos - StartPos);
    TextRange.lpstrText := PChar(Result);
    SendMessage(Info.EditWnd, EM_GETTEXTRANGE, 0, LPARAM(@TextRange))
  end
  else
  begin
    TotalLen := SendMessage(Info.EditWnd, WM_GETTEXTLENGTH, 0, 0);
    if EndPos > TotalLen then EndPos := TotalLen - StartPos;
    TextHandle := HLOCAL(SendMessage(Info.EditWnd, EM_GETHANDLE, 0, 0));
    if (TextHandle <> 0) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
      { The API documentation says the buffer will be ANSI if the control was created as
        ANSI, unicode otherwise. What it doesn't say is that if the app is themed, then
        the buffer will always be unicode. }
      TextRange.lpstrText := LocalLock(TextHandle);
      if (SizeOf(Char) = 2) or not ThemeServices.ThemesEnabled then
      begin
        Inc(TextRange.lpstrText, StartPos);
        SetString(Result, TextRange.lpstrText, EndPos - StartPos);
      end
      else
      begin
        WideSeekPtr := Pointer(TextRange.lpstrText);
        Inc(WideSeekPtr, StartPos);
        Result := WideCharLenToString(WideSeekPtr, EndPos - StartPos);
      end;
      LocalUnlock(TextHandle);
    end
    else
    begin
      SetLength(Result, TotalLen);
      SendMessage(Info.EditWnd, WM_GETTEXT, TotalLen + 1, LPARAM(Pointer(Result))); //1 for null terminator
      Result := Copy(Result, StartPos + 1, EndPos - StartPos) //strings are 1-indexed
    end;
  end;
end;

function GetEditChar(const Info: TACControlInfo; StartPos: Integer): Char;
var
  S: string;
begin
  S := GetTextRange(Info, StartPos, StartPos + 1);
  if S <> '' then
    Result := S[1]
  else
    Result := #0;
end;

procedure Edit_GetSel(EditWnd: HWND; var SelStart, SelEnd: Integer); 
begin
  SendMessage(EditWnd, EM_GETSEL, WPARAM(@SelStart), LPARAM(@SelEnd));
end;

procedure Edit_ReplaceSel(EditWnd: HWND; const NewText: string); 
begin
  SendMessage(EditWnd, EM_REPLACESEL, -1, LPARAM(PChar(NewText)));
end;

procedure Edit_SetSel(EditWnd: HWND; const SelStart, SelEnd: Integer); 
begin
  SendMessage(EditWnd, EM_SETSEL, WPARAM(SelStart), LPARAM(SelEnd));
end;

function IsKeyDown(Key: Word): Boolean; 
begin
  Result := (GetKeyState(Key) < 0);
end;

function FindWriteableEdit(Source: TWinControl; var Info: TACControlInfo): Boolean;
begin
  Result := False;
  Info.Source := Source;
  if Source is TCustomEdit then
  begin
    if TCustomEditAccess(Source).ReadOnly then Exit;
    Info.EditWnd := Source.Handle;
    //support the JVCL's rich edit v3 wrapper, which doesn't descend from TCustomRichEdit
    Info.IsRichEdit := (Source is TCustomRichEdit) or (Pos('RichEdit', Source.ClassName) > 0);
  end
  else if Source is TCustomCombo then
  begin
    Info.EditWnd := TCustomComboAccess(Source).EditHandle;
    if (Info.EditWnd = 0) or (Info.EditWnd <> GetFocus) then Exit;
    Info.IsRichEdit := False;
  end
  else
    Exit;
  Result := True;
end;

procedure InsertEntryText(const Info: TACControlInfo; const Text: string;
  Source: TEntryCacheItem; TextIncludesTriggerChar: Boolean);
var
  EditControlFreezer: IInterface;
  OrigFormat: TCharFormat2;
  SelStart, SelEnd: Integer;
begin
  FillChar(OrigFormat, SizeOf(OrigFormat), 0);
  if Source.Entry.HasFormatting and Info.IsRichEdit then
  begin
    OrigFormat.cbSize := SizeOf(OrigFormat);
    SendMessage(Info.EditWnd, EM_GETCHARFORMAT, SCF_SELECTION, LPARAM(@OrigFormat));
  end;
  Edit_GetSel(Info.EditWnd, SelStart, SelEnd);
  Dec(SelStart, Length(Source.Entry.FindText));
  if TextIncludesTriggerChar then
    Inc(SelStart);
  EditControlFreezer := TEditControlFreezer.Create(Info.EditWnd, Info.IsRichEdit);
  Edit_SetSel(Info.EditWnd, SelStart, SelEnd);  //sleep(500);
  if OrigFormat.cbSize <> 0 then
    Source.Entry.ApplyFormatting(Info.EditWnd);
  { if just changing the formatting, then don't overwrite the text anyway since it may
    have funky casing that should be preserved }
  if (OrigFormat.cbSize = 0) or Source.CaseSensitiveReplace or
      not AnsiSameText(Text, GetTextRange(Info, SelStart, SelEnd)) then
    Edit_ReplaceSel(Info.EditWnd, Text)
  else
    Edit_SetSel(Info.EditWnd, SelEnd, SelEnd);
  if OrigFormat.cbSize <> 0 then
    SendMessage(Info.EditWnd, EM_SETCHARFORMAT, SCF_SELECTION, LPARAM(@OrigFormat));
end;

function IsLowerCaseWithTwoInitialCaps(const S: string): Boolean;
var
  SeekPtr: PChar;
begin
  Result := False;
  SeekPtr := PChar(S);
  if (Length(S) < 3) or (SeekPtr^ = ToLower(SeekPtr^)) or
    (SeekPtr[1] = ToLower(SeekPtr[1])) then Exit;
  Inc(SeekPtr);
  repeat
    Inc(SeekPtr);
  until (SeekPtr^ = #0) or (SeekPtr^ <> ToLower(SeekPtr^));
  Result := (SeekPtr^ = #0);
end;

{ rich edit utility routines that are exposed in the unit's interface section }

procedure OffsetFirstIndent(RichEditWnd: HWND; Amount: Integer);
var
  Format: TParaFormat;
begin
  FillChar(Format, SizeOf(Format), 0);
  Format.cbSize := SizeOf(Format);
  SendMessage(RichEditWnd, EM_GETPARAFORMAT, 0, LPARAM(@Format));
  Amount := Amount * 20;
  Format.dwMask := PFM_STARTINDENT or PFM_OFFSET;
  Dec(Format.dxOffset, Amount);
  Inc(Format.dxStartIndent, Amount);
  SendMessage(RichEditWnd, EM_SETPARAFORMAT, 0, LPARAM(@Format));
end;

procedure OffsetFirstIndent(RichEdit: TCustomRichEdit; Amount: Integer);
begin
  OffsetFirstIndent(RichEdit.Handle, Amount);
end;

procedure OffsetLeftIndent(RichEditWnd: HWND; Amount: Integer);
var
  Format: TParaFormat2;
begin
  FillChar(Format, SizeOf(Format), 0);
  Format.cbSize := SizeOf(Format);
  SendMessage(RichEditWnd, EM_GETPARAFORMAT, 0, LPARAM(@Format));
  Amount := Amount * 20;
  if (Amount < 0) and (Format.dxStartIndent + Format.dxOffset + Amount < 0) then
    OffsetFirstIndent(RichEditWnd, Amount div 20)
  else
  begin
    Format.dwMask := PFM_OFFSETINDENT;
    Format.dxStartIndent := Amount;
    SendMessage(RichEditWnd, EM_SETPARAFORMAT, 0, LPARAM(@Format));
  end;
end;

procedure OffsetLeftIndent(RichEdit: TCustomRichEdit; Amount: Integer);
begin
  OffsetLeftIndent(RichEdit.Handle, Amount);
end;

{ TIgnoredTwoInitialCaps }

constructor TIgnoredTwoInitialCaps.Create(AOwner: TAutoCorrectEngine);
begin
  inherited Create;
  CaseSensitive := True;
  Duplicates := dupIgnore;
  Sorted := True;
  FOwner := AOwner;
end;

procedure TIgnoredTwoInitialCaps.Changed;
begin
  FOwner.Modified := True;
  inherited;
end;

procedure TIgnoredTwoInitialCaps.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  if Length(S) <= 2 then Exit;
  inherited InsertItem(Index, AnsiUpperCase(Copy(S, 1, 2)) +
    AnsiLowerCase(Copy(S, 3, MaxInt)), AObject);
end;

{ TEditControlFreezer }

constructor TEditControlFreezer.Create(EditWnd: HWND; IsRichEdit: Boolean);
var
  RichEditOle: IUnknown;
begin
  inherited Create;
  if IsRichEdit and (SendMessage(EditWnd, EM_GETOLEINTERFACE, 0, LPARAM(@RichEditOle)) <> 0) and
    (RichEditOle.QueryInterface(ITextDocument, FTextDocument) = S_OK) then
    FTextDocument.Freeze
  else
    LockWindowUpdate(EditWnd);
  FFrozen := True;
end;

destructor TEditControlFreezer.Destroy;
begin
  Unfreeze;
  inherited;
end;

procedure TEditControlFreezer.Unfreeze;
begin
  if not FFrozen then Exit;
  FFrozen := False;
  if FTextDocument <> nil then
    FTextDocument.Unfreeze
  else
    LockWindowUpdate(0);
end;

{ TEntryCacheItem }

constructor TEntryCacheItem.Create(AEntry: TAutoCorrectEntry; AKind: TAutoCorrectionKind);
  function CustomCase(const S: string): Boolean;
  begin
    Result := S <> AnsiLowerCase(S);
  end;
begin
  inherited Create;
  FCaseSensitiveFind := CustomCase(AEntry.FindText);
  FCaseSensitiveReplace := FCaseSensitiveFind or CustomCase(AEntry.ReplaceText);
  FEntry := AEntry;
  FKind := AKind;
end;

destructor TEntryCacheItem.Destroy;
begin
  if FEntry.Collection = nil then FEntry.Free;
  inherited;
end;

{ TAutoCorrectEntry }

constructor TAutoCorrectEntry.Create(Collection: TCollection);
begin
  inherited;
  FHasFormatting := hfNo;
  FHighlightColorToSet := clNone;
  FFontColorToSet := clNone;
end;

constructor TAutoCorrectEntry.Create(ACollection: TCollection;
  const AFindText, AReplaceText: string);
begin
  Create(ACollection);
  SetFindText(AFindText);
  SetReplaceText(AReplaceText);
end;

constructor TAutoCorrectEntry.Create(const AFindText, AReplaceText: string);
begin
  Create(nil, AFindText, AReplaceText);
end;

procedure TAutoCorrectEntry.ApplyFormatting(RichEditWnd: HWND);
var
  Format: TCharFormat2;
begin
  with Format do
  begin
    FillChar(Format, SizeOf(Format), 0);
    cbSize := SizeOf(Format);
    if fsBold in StylesToAdd then
    begin
      dwMask := dwMask or CFM_BOLD;
      dwEffects := dwEffects or CFE_BOLD;
    end;
    if fsItalic in StylesToAdd then
    begin
      dwMask := dwMask or CFM_ITALIC;
      dwEffects := dwEffects or CFE_Italic;
    end;
    if fsStrikeOut in StylesToAdd then
    begin
      dwMask := dwMask or CFM_STRIKEOUT;
      dwEffects := dwEffects or CFE_StrikeOut;
    end;
    if fsUnderline in StylesToAdd then
    begin
      dwMask := dwMask or CFM_UNDERLINE;
      dwEffects := dwEffects or CFE_Underline;
    end;
    if HighlightColorToSet <> clNone then
    begin
      dwMask := dwMask or CFM_BACKCOLOR;
      crBackColor := TColorRef(ColorToRGB(HighlightColorToSet));
    end;
    if FontColorToSet <> clNone then
    begin
      dwMask := dwMask or CFM_COLOR;
      case FontColorToSet of
        clWindowText, clDefault: dwEffects := dwEffects or CFE_AUTOCOLOR;
      else crTextColor := TColorRef(ColorToRGB(FontColorToSet));
      end;
    end;
    if FontNameToSet <> '' then
    begin
      dwMask := dwMask or CFM_FACE;
      StrPLCopy(szFaceName, FontNameToSet, Length(szFaceName) - 1);//has D2009's ComCtrls.pas been updated to use Length rather than SizeOf in TTextAttributes.SetName...?
    end;
    if FontSizeToSet <> 0 then
    begin
      dwMask := dwMask or CFM_SIZE;
      yHeight := FontSizeToSet * 20;
    end;
    if dwMask <> 0 then
      SendMessage(RichEditWnd, EM_SETCHARFORMAT, SCF_SELECTION, LPARAM(@Format));
  end;
end;

procedure TAutoCorrectEntry.ApplyFormatting(Dest: TCustomRichEdit);
begin
  ApplyFormatting(Dest.Handle);
end;

procedure TAutoCorrectEntry.AssignTo(Dest: TPersistent);
var
  DestEntry: TAutoCorrectEntry absolute Dest;
begin
  if not (Dest is TAutoCorrectEntry) then inherited;
  if DestEntry.FindText <> FindText then
  begin
    if DestEntry.Collection is TAutoCorrectEntries then
      TAutoCorrectEntries(DestEntry.Collection).FindTextChanging(DestEntry, FindText);
    DestEntry.FFindText := FindText;
  end;
  DestEntry.FReplaceText := ReplaceText;
  DestEntry.FFontColorToSet := FontColorToSet;
  DestEntry.FFontNameToSet := FontNameToSet;
  DestEntry.FHighlightColorToSet := HighlightColorToSet;
  DestEntry.FFontSizeToSet := FontSizeToSet;
  DestEntry.FStylesToAdd := StylesToAdd;
  DestEntry.FHasFormatting := FHasFormatting;
  DestEntry.Changed(False);
end;

function TAutoCorrectEntry.GetDisplayName: string;
begin
  Result := FindText;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TAutoCorrectEntry.HasFormatting: Boolean;
begin
  if FHasFormatting = hfDontKnow then
    if (FontColorToSet <> clNone) or (FontNameToSet <> '') or (FontSizeToSet <> 0) or
       (HighlightColorToSet <> clNone) or (StylesToAdd <> []) then
      FHasFormatting := hfYes
    else
      FHasFormatting := hfNo;
  Result := (FHasFormatting = hfYes);
end;

function TAutoCorrectEntry.IsReplaceTextStored: Boolean;
begin
  Result := (ReplaceText <> FindText) and (ReplaceText <> '');
end;

procedure TAutoCorrectEntry.SetFindText(const Value: string);
begin
  if Value = FindText then Exit;
  if Collection is TAutoCorrectEntries then
    TAutoCorrectEntries(Collection).FindTextChanging(Self, Value);
  FFindText := Value;
  if FReplaceText = '' then FReplaceText := Value;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetFontColorToSet(const Value: TColor);
begin
  if Value = FontColorToSet then Exit;
  FFontColorToSet := Value;
  FHasFormatting := hfDontKnow;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetFontNameToSet(const Value: TFontName);
begin
  if Value = FontNameToSet then Exit;
  FFontNameToSet := Value;
  FHasFormatting := hfDontKnow;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetFontSizeToSet(const Value: Integer);
begin
  if Value = FontSizeToSet then Exit;
  FFontSizeToSet := Value;
  FHasFormatting := hfDontKnow;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetHighlightColorToSet(const Value: TColor);
begin
  if Value = HighlightColorToSet then Exit;
  FHighlightColorToSet := Value;
  FHasFormatting := hfDontKnow;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetReplaceText(const Value: string);
begin
  if Value = ReplaceText then Exit;
  if Value <> '' then
    FReplaceText := Value
  else
    FReplaceText := FindText;
  Changed(False);
end;

procedure TAutoCorrectEntry.SetStylesToAdd(const Value: TFontStyles);
begin
  if Value = StylesToAdd then Exit;
  FStylesToAdd := Value;
  FHasFormatting := hfDontKnow;
  Changed(False);
end;

{ TAutoCorrectEntries }

constructor TAutoCorrectEntries.Create(AOwner: TAutoCorrectEngine);
begin
  FOwner := AOwner;
  inherited Create(TAutoCorrectEntry);
  FFindTextHash := TStringHash.Create;
end;

destructor TAutoCorrectEntries.Destroy;
begin
  FreeAndNil(FFindTextHash);
  inherited;
end;

function TAutoCorrectEntries.Add: TAutoCorrectEntry;
begin
  Result := (inherited Add as TAutoCorrectEntry);
end;

function TAutoCorrectEntries.Add(const FindText: string; const ReplaceText: string = ''): TAutoCorrectEntry;
begin
  Result := TAutoCorrectEntry.Create(Self, FindText, ReplaceText);
end;

function TAutoCorrectEntries.Find(const FindText: string;
  out Entry: TAutoCorrectEntry): Boolean;
var
  Value: Integer;
begin
  Value := FFindTextHash.ValueOf(FindText);
  Result := (Value <> -1);
  if Result then
    Entry := TAutoCorrectEntry(Value)
  else
    Entry := nil;
end;

procedure TAutoCorrectEntries.FindTextChanging(Entry: TAutoCorrectEntry;
  const NewFindText: string);
begin
  if NewFindText <> '' then
  begin
    if FFindTextHash.ValueOf(NewFindText) <> -1 then
      raise EDuplicateAutoCorrectEntry.CreateFmt(SDuplicateEntry, [NewFindText]);
    FFindTextHash.Add(NewFindText, Integer(Entry));
  end;
  if Entry.FindText <> '' then FFindTextHash.Remove(Entry.FindText);
end;

function TAutoCorrectEntries.GetItem(Index: Integer): TAutoCorrectEntry;
begin
  Result := inherited GetItem(Index) as TAutoCorrectEntry;
end;

procedure TAutoCorrectEntries.SetItem(Index: Integer; Value: TAutoCorrectEntry);
begin
  inherited SetItem(Index, Value);
end;

function TAutoCorrectEntries.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TAutoCorrectEntries.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if (Action in [cnDeleting, cnExtracting]) and (FFindTextHash <> nil) then
    FFindTextHash.Remove((Item as TAutoCorrectEntry).FindText);
end;

procedure TAutoCorrectEntries.Update(Item: TCollectionItem);
begin
  if FOwner = nil then Exit;
  FOwner.FreeEntryCache;
  FOwner.Modified := True;
end;

{ TAutoCorrectEngine }

constructor TAutoCorrectEngine.Create;
begin
  inherited;
  FAutoIndentSize := 10;
  FCustomEntries := TAutoCorrectEntries.Create(Self);
  FIgnoredTwoInitialCaps := TIgnoredTwoInitialCaps.Create(Self);
  FOptions := AllAutoCorrectOptions;
end;

destructor TAutoCorrectEngine.Destroy;
begin
  FreeEntryCache;
  FIgnoredTwoInitialCaps.Free;
  FCustomEntries.Free;
  FBackSpaceEntry.Free;
  FLeftSingleQuoteEntry.Free;
  FRightSingleQuoteEntry.Free;
  FLeftDoubleQuoteEntry.Free;
  FRightDoubleQuoteEntry.Free;
  FTabEntry.Free;
  FTwoInitialCapsEntry.Free;
  inherited;
end;

procedure TAutoCorrectEngine.AddToEntryCache(Kind: TAutoCorrectionKind;
  Entry: TAutoCorrectEntry);
var
  EntryLen: Integer;
  LastChar: Char;
  NewItem: TEntryCacheItem;
begin
  EntryLen := Length(Entry.FindText);
  if (EntryLen > 0) and (Entry.ReplaceText <> '') then
  begin
    NewItem := TEntryCacheItem.Create(Entry, Kind);
    if not Entry.HasFormatting and (Entry.ReplaceText = Entry.FindText) then
      NewItem.Free
    else
    begin
      LastChar := Entry.FindText[EntryLen];
      if FEntryCache[LastChar] = nil then
        FEntryCache[LastChar] := TObjectList.Create(True);
      FEntryCache[LastChar].Add(NewItem);
      if EntryLen > FMaxWordOrCachedEntryLen then FMaxWordOrCachedEntryLen := EntryLen;
    end;
    Exit;
  end;
  if Entry.Collection = nil then Entry.Free;
end;

function TAutoCorrectEngine.AddToEntryCache(Kind: TAutoCorrectionKind;
  const AFindText, AReplaceText: string): TAutoCorrectEntry;
begin
  Result := TAutoCorrectEntry.Create(nil);
  Result.FindText := AFindText;
  Result.ReplaceText := AReplaceText;
  AddToEntryCache(Kind, Result);
end;

procedure TAutoCorrectEngine.CheckForAnyCorrection(Control: TWinControl; var Key: Char);
var
  Buffer: PChar;
  BufferLen: Integer;
  Info: TACControlInfo;
  SelStart: Integer;

  function DoIt(Ch: Char; IncludeKey: Boolean): Boolean;
  const
    StrLComp: array[Boolean] of function (S1, S2: PChar; MaxLen: Cardinal): Integer =
      (AnsiStrLIComp, AnsiStrLComp);
  var
    Item: TEntryCacheItem;
    ReplaceText: string;
    I, EntryLen: Integer;
    SeekPtr: PChar;
  begin
    Result := False;
    if FEntryCache[Ch] = nil then Exit;
    for I := FEntryCache[Ch].Count - 1 downto 0 do
    begin
      Item := FEntryCache[Ch].List[I];
      EntryLen := Length(Item.Entry.FindText);
      if IncludeKey then Dec(EntryLen);
      if EntryLen > BufferLen then Continue;
      SeekPtr := @Buffer[BufferLen - EntryLen];

      if (BufferLen > EntryLen) and IsLetterOrDigit(SeekPtr^) and //Second check is so don't test
        IsLetterOrDigit(SeekPtr[-1]) then Continue;               //when find text is ..., ->, etc..

      if StrLComp[Item.CaseSensitiveFind](SeekPtr,
        PChar(Item.Entry.FindText), EntryLen) <> 0 then Continue;
      if IncludeKey and (StrLComp[Item.CaseSensitiveFind](@Key,
        @Item.Entry.FindText[EntryLen + 1], 1) <> 0) then Continue;

      if Item.Entry.HasFormatting and not Info.IsRichEdit then Break;

      ReplaceText := Item.Entry.ReplaceText;
      if not Item.CaseSensitiveReplace and (SeekPtr^ <> ToLower(SeekPtr^)) then
      begin
        repeat
          Inc(SeekPtr);
        until (SeekPtr^ = #0) or (SeekPtr^ <> ToUpper(SeekPtr^));
        if SeekPtr^ = #0 then
          ReplaceText := AnsiUpperCase(ReplaceText)
        else
          ReplaceText[1] := ToUpper(ReplaceText[1]); //don't attempt to second guess any funky casing
      end;
      if IncludeKey then
        Key := #0 //the value of Key is part of the replacement string.
      else if not Item.Entry.HasFormatting then
      begin
        ReplaceText := ReplaceText + Key; //might as well
        Key := #0;
      end;
      InsertEntryText(Info, ReplaceText, Item, IncludeKey);
      FLastInitialCapsControl := nil;
      CorrectionMade(Item.Kind, Control, Item.Entry);
      Result := True;
      Break;
    end;
  end;
var
  BufferStr: string;
  MaybeAutoIndent, MaybeCurlQuote: Boolean;
  EditControlFreezer: IInterface;
  I, SecondCharPos: Integer;
  LoweredChar: Char;
  SelEnd: Integer;
begin
  MaybeAutoIndent := False;
  MaybeCurlQuote := False;
  case Key of
    Char(VK_BACK), TabChar: MaybeAutoIndent := True;
    '''', '"': MaybeCurlQuote := True;
  else
    if not IsTriggerKey(Control, Key) then Exit;
  end;
  if not FindWriteableEdit(Control, Info) then Exit;
  EntryCacheNeeded;
  if MaybeAutoIndent then
  begin
    CheckForAutoIndent(Info, Key);
    if (Key = #0) or not IsTriggerKey(Control, Key) then Exit;
  end;
  if MaybeCurlQuote then
  begin
    CheckForCurlQuote(Info, Key);
    if not IsTriggerKey(Control, Key) then Exit;
  end;
  Edit_GetSel(Info.EditWnd, SelStart, SelEnd);
  if SelStart = 0 then Exit;
  BufferStr := GetTextRange(Info, SelStart - FMaxWordOrCachedEntryLen - 1,
    SelStart); //1 for the prev char, which we check isn't part of a word
  BufferLen := Length(BufferStr);
  Buffer := PChar(BufferStr);
  if (acCorrectTwoInitialCaps in Options) and (BufferLen > 2) then
    for I := BufferLen - 1 downto -1 do //-1 for when the word is right at the beginning
      if (I = -1) or not IsLetterOrDigit(Buffer[I]) then
      begin
        FTwoInitialCapsEntry.FindText := Copy(BufferStr, I + 2, MaxInt);
        if IsLowerCaseWithTwoInitialCaps(FTwoInitialCapsEntry.FindText) and
           (IgnoredTwoInitialCaps.IndexOf(FTwoInitialCapsEntry.FindText) < 0) then
        begin
          FLastInitialCapsControl := Control;
          Buffer[I + 2] := ToLower(Buffer[I + 2]);
          SecondCharPos := SelStart - BufferLen + I + 2;
          EditControlFreezer := TEditControlFreezer.Create(Info.EditWnd, Info.IsRichEdit); //keep it active until the proc exits
          Edit_SetSel(Info.EditWnd, SecondCharPos, SecondCharPos + 1);
          Edit_ReplaceSel(Info.EditWnd, string(Buffer[I + 2]));
          Edit_SetSel(Info.EditWnd, SelStart, SelEnd);
          FTwoInitialCapsEntry.ReplaceText := Copy(BufferStr, I + 2, MaxInt);
          CorrectionMade(ckTwoInitialCaps, Control, FTwoInitialCapsEntry)
        end;
        Break;
      end;
  if DoIt(Key, True) or DoIt(Buffer[BufferLen - 1], False) then Exit;
  LoweredChar := ToLower(Buffer[BufferLen - 1]);
  if (LoweredChar <> Buffer[BufferLen - 1]) then DoIt(LoweredChar, False);
end;

procedure TAutoCorrectEngine.CheckForAutoIndent(const Info: TACControlInfo; var Key: Char);

  procedure CorrectionMade;
  begin
    Self.CorrectionMade(ckIndentation, Info.Source, GetDummyEntry(Key));
    Key := #0;
  end;
var
  Amount: Integer;
  IncreaseIndent: Boolean;
  ParaFormat: TParaFormat;
  LeftIndentInUserTerms: Integer;
  SelStart, SelEnd: Integer;
  WantLeftIndent: Boolean;
begin
  if not (Info.IsRichEdit and (acAutoIndent in Options)) then Exit;
  case Key of
    Char(VK_BACK): IncreaseIndent := False;
    TabChar: IncreaseIndent := not IsKeyDown(VK_SHIFT);
  else Exit;
  end;
  Edit_GetSel(Info.EditWnd, SelStart, SelEnd);
  if SelEnd <> SelStart then Exit;
  if SelStart <> 0 then
    case GetEditChar(Info, SelStart - 1) of
      #13, #10:{rich edit v1 uses CRLF, later versions just CR};
    else Exit;
    end;
  FillChar(ParaFormat, SizeOf(ParaFormat), 0);
  ParaFormat.cbSize := SizeOf(ParaFormat);
  SendMessage(Info.EditWnd, EM_GETPARAFORMAT, 0, LPARAM(@ParaFormat));
  if ParaFormat.wAlignment <> PFA_LEFT then Exit;

  LeftIndentInUserTerms := ParaFormat.dxStartIndent;
  if ParaFormat.dxStartIndent < 0 then Inc(LeftIndentInUserTerms, ParaFormat.dxOffset);
  LeftIndentInUserTerms := LeftIndentInUserTerms div 20;
  if IncreaseIndent then
    Amount := AutoIndentSize
  else
  begin
    if ParaFormat.dxStartIndent = 0 then
    begin
      if (ParaFormat.dxOffset > 0) and (ParaFormat.wNumbering <> 0) then
      begin
        ParaFormat.wNumbering := 0;
        ParaFormat.dxOffset := 0;
        ParaFormat.dwMask := PFM_STARTINDENT or PFM_OFFSET or PFM_NUMBERING;
        SendMessage(Info.EditWnd, EM_SETPARAFORMAT, 0, LPARAM(@ParaFormat));
        CorrectionMade;
      end;
      Exit;
    end;
    Amount := -AutoIndentSize;
    if LeftIndentInUserTerms + Amount < 0 then
      Amount := -LeftIndentInUserTerms;
    IncreaseIndent := False;
  end;
  WantLeftIndent := not IncreaseIndent and (ParaFormat.dxOffset = 0);
  if (ParaFormat.wNumbering = 0) and 
     (not WantLeftIndent or (IncreaseIndent and (ParaFormat.dxOffset > 0))) then
    OffsetFirstIndent(Info.EditWnd, Amount)
  else
    OffsetLeftIndent(Info.EditWnd, Amount);
  CorrectionMade;
end;

procedure TAutoCorrectEngine.CheckForCurlQuote(const Info: TACControlInfo;
  var Key: Char);
const
  LeftSmartQuoteChars: array[Boolean] of Char = (LeftSingleQuote, LeftDoubleQuote);
  RightSmartQuoteChars: array[Boolean] of Char = (RightSingleQuote, RightDoubleQuote);
var
  IsDoubleQuote: Boolean;
  Pos: Integer;
  PrevChar: Char;
begin
  if not (acCurlQuoteChars in Options) then Exit;
  case Key of
    '"', '''':{what we're looking for};
  else Exit;
  end;
  IsDoubleQuote := (Key = '"');
  Key := LeftSmartQuoteChars[IsDoubleQuote];
  Edit_GetSel(Info.EditWnd, Pos, PInteger(nil)^);
  if Pos > 0 then
  begin
    PrevChar := GetEditChar(Info, Pos - 1);
    case PrevChar of
      ' ', '[', '(', '{', '/', '|', '-', EnDash, EmDash, TabChar,
      '<', '«', '"', ':', #13, #10: {should left quote after any of these};
    else if PrevChar <> LeftSmartQuoteChars[not IsDoubleQuote] then
      Key := RightSmartQuoteChars[IsDoubleQuote];
    end;
  end;
  CorrectionMade(ckCurlQuoteChar, Info.Source, GetDummyEntry(Key));
end;

procedure TAutoCorrectEngine.CheckForTriggerKey(Control: TWinControl; Key: Char;
  var IsTriggerKey: Boolean);
begin
  if Assigned(FOnCheckForTriggerKey) then
    FOnCheckForTriggerKey(Self, Control, Key, IsTriggerKey);
end;

procedure TAutoCorrectEngine.CorrectionMade(Kind: TAutoCorrectionKind;
  Control: TWinControl; Entry: TAutoCorrectEntry);
begin
  if Assigned(FOnCorrectionMade) then FOnCorrectionMade(Self, Kind, Control, Entry);
end;

function TAutoCorrectEngine.GetDummyEntry(Key: Char): TAutoCorrectEntry;
begin
  case Key of
    Char(VK_BACK): Result := FBackSpaceEntry;
    LeftSingleQuote: Result := FLeftSingleQuoteEntry;
    LeftDoubleQuote: Result := FLeftDoubleQuoteEntry;
    RightSingleQuote: Result := FRightSingleQuoteEntry;
    RightDoubleQuote: Result := FRightDoubleQuoteEntry;
    TabChar: Result := FTabEntry;
  else Result := nil;
  end;
end;

function CompareCacheListItems(Item1, Item2: TEntryCacheItem): Integer;
begin //sort by length in ascending order (shorted first, longest last)
  Result := Length(Item1.Entry.FindText) - Length(Item2.Entry.FindText);
end;

procedure TAutoCorrectEngine.EntryCacheNeeded;
var
  Ch: Char;
  I: Integer;
begin
  if FHasEntryCache then Exit;
  FHasEntryCache := True;
  if FBackSpaceEntry = nil then
  begin
    FBackSpaceEntry := TAutoCorrectEntry.Create(Char(VK_BACK), '');
    FLeftSingleQuoteEntry := TAutoCorrectEntry.Create('''', LeftSingleQuote);
    FRightSingleQuoteEntry := TAutoCorrectEntry.Create('''', RightSingleQuote);
    FLeftDoubleQuoteEntry := TAutoCorrectEntry.Create('"', LeftDoubleQuote);
    FRightDoubleQuoteEntry := TAutoCorrectEntry.Create('"', RightDoubleQuote);
    FTabEntry := TAutoCorrectEntry.Create(TabChar, '');
    FTwoInitialCapsEntry := TAutoCorrectEntry.Create('', ' ');
  end;
  FMaxWordOrCachedEntryLen := MaxWordLength;
  if acConvertHyphensToDashes in Options then
  begin //the following is similar to what Word 2007 does
    AddToEntryCache(ckHyphenToDash, '--', EmDash);
    AddToEntryCache(ckHyphenToDash, ' - ', ' ' + EnDash + ' ');
  end;
  if acPreferFractionChars in Options then
  begin
    AddToEntryCache(ckFractionChar, '1/4', '¼');
    AddToEntryCache(ckFractionChar, '1/2', '½');
    AddToEntryCache(ckFractionChar, '3/4', '¾');
  end;
  if acUseCustomEntries in Options then
    for I := 0 to CustomEntries.Count - 1 do
      AddToEntryCache(ckCustomEntry, CustomEntries[I]);
  for Ch := Low(FEntryCache) to High(FEntryCache) do
    if (FEntryCache[Ch] <> nil) and (FEntryCache[Ch].Count > 1) then
      FEntryCache[Ch].Sort(@CompareCacheListItems);
end;

procedure TAutoCorrectEngine.FreeEntryCache;
var
  Ch: Char;
begin
  if not FHasEntryCache then Exit;
  FHasEntryCache := False;
  for Ch := Low(FEntryCache) to High(FEntryCache) do
    FreeAndNil(FEntryCache[Ch]);
end;

procedure TAutoCorrectEngine.KeyDownOccurred(Control: TWinControl; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_BACK, VK_RETURN: //check return key here, since by the time of KeyPress, rich edit v2+ will have processed it
      CheckForAnyCorrection(Control, PChar(@Key)^); //the joys of small endianness...
  end;
end;

procedure TAutoCorrectEngine.KeyPressOccurred(Control: TWinControl; var Key: Char);
begin
  FLastInitialCapsControl := nil;
  case Key of
    Char(VK_BACK), #13: { checked in KeyDownOccurred };
  else CheckForAnyCorrection(Control, Key);
  end;
end;

procedure TAutoCorrectEngine.UndoOccurred(Control: TWinControl);
var
  AddToIgnoredList: Boolean;
  Freezer: IInterface;
  Info: TACControlInfo;
  SelStart, SelEnd, CorrectionStart: Integer;
begin
  if (FLastInitialCapsControl <> nil) and (Control = FLastInitialCapsControl) and
    FindWriteableEdit(Control, Info) then
  begin //put things back if we can
    Edit_GetSel(Info.EditWnd, SelStart, SelEnd);
    CorrectionStart := SelStart - Length(FTwoInitialCapsEntry.FindText);
    if FTwoInitialCapsEntry.ReplaceText = GetTextRange(Info, CorrectionStart, SelStart) then
    begin
      Freezer := TEditControlFreezer.Create(Info.EditWnd, Info.IsRichEdit);
      Edit_SetSel(Info.EditWnd, CorrectionStart, SelStart);
      Edit_ReplaceSel(Info.EditWnd, FTwoInitialCapsEntry.FindText);
      Edit_SetSel(Info.EditWnd, SelStart, SelEnd);
      Freezer := nil;
      AddToIgnoredList := True;
      if Assigned(FOnUndoTwoInitialCapsCorrection) then
        FOnUndoTwoInitialCapsCorrection(Self, FTwoInitialCapsEntry.FindText,
          AddToIgnoredList);
      if AddToIgnoredList then IgnoredTwoInitialCaps.Add(FTwoInitialCapsEntry.FindText);
    end;
  end;
  FLastInitialCapsControl := nil;
end;

function TAutoCorrectEngine.IsTriggerKey(Control: TWinControl; Key: Char): Boolean;
begin
  case Key of
    TabChar, '''', '"', '.', '>', '!', '?', ' ', '/', '|', ')', ']', ',', ';', ':',
    LeftSingleQuote, RightSingleQuote, LeftDoubleQuote, RightDoubleQuote,
    '-': Result := True; //hyphen for corrections such as 'psuedo-' to 'pseudo-'
    #13: Result := (Control is TCustomMemo) and TCustomMemoAccess(Control).WantReturns;
  else Result := False;
  end;
  CheckForTriggerKey(Control, Key, Result);
end;

procedure TAutoCorrectEngine.Loaded;
begin
  inherited;
  Modified := False;
end;

procedure TAutoCorrectEngine.SetIgnoredTwoInitialCaps(Value: TStrings);
begin
  FIgnoredTwoInitialCaps.Assign(Value);
end;

procedure TAutoCorrectEngine.SetCustomEntries(Value: TAutoCorrectEntries);
begin
  FCustomEntries.Assign(Value);
end;

procedure TAutoCorrectEngine.SetOptions(Value: TAutoCorrectOptions);
begin
  if Value = FOptions then Exit;
  FOptions := Value;
  FreeEntryCache;
end;

{ TAutoCorrectEngine - INI file persistence }

const
  IFS_GENERAL = 'General';
  IFK_IGNOREDINITIALCAPS = 'IgnoredTwoInitialCaps';
  IFK_REPLACETEXT = 'Replacement';
  IFK_FONTCOLOR = 'FontColor';
  IFK_FONTNAME = 'FontName';
  IFK_FONTSIZE = 'FontSize';
  IFK_HIGHLIGHTCOLOR = 'HighlightColor';
  IFK_EMBOLDEN = 'Embolden';
  IFK_ITALICIZE = 'Italicize';
  IFK_STRIKEOUT = 'StrikeOut';
  IFK_UNDERLINE = 'Underline';

  UTF8BOM: array[1..3] of Byte = ($EF, $BB, $BF);

function StringToColorDef(const Str: string; Default: TColor): TColor;
begin
  if Str = '' then
    Result := Default
  else
    try
      Result := StringToColor(Str);
    except
      on EConvertError do
        Result := Default
      else
        raise;
    end;
end;

procedure TAutoCorrectEngine.LoadFromIniFile(const FileName: string;
  Behaviour: TLoadFromIniFileBehaviour);
var
  IniFile: TMemIniFile;
  Lines: TStringList;
  S: string;
begin
  if FileExists(FileName) then
    with TMemoryStream.Create do
    try
      LoadFromFile(FileName);
      SetString(S, PChar(Memory), Size);
      if (Size > 3) and CompareMem(@UTF8BOM, Memory, 3) then
        S := Utf8ToAnsi(S);
    finally
      Free;
    end
  else if Behaviour = lbClearFirst then
    S := ''
  else
    Exit;
  IniFile := nil;
  Lines := TStringList.Create;
  try
    Lines.Text := S;
    IniFile := TMemIniFile.Create('');
    IniFile.SetStrings(Lines);
    LoadFromIniFile(IniFile, Behaviour);
  finally
    IniFile.Free;
    Lines.Free;
  end;
end;

procedure TAutoCorrectEngine.LoadFromIniFile(IniFile: TCustomIniFile;
  Behaviour: TLoadFromIniFileBehaviour);
var
  Entry: TAutoCorrectEntry;
  I: Integer;
  Items: TStringList;
  Section, Text: string;
begin
  Items := nil;
  CustomEntries.BeginUpdate;
  try
    if Behaviour = lbClearFirst then
    begin
      CustomEntries.Clear;
      IgnoredTwoInitialCaps.Clear;
    end;
    if IniFile = nil then Exit;
    Items := TStringList.Create;
    Items.CommaText := IniFile.ReadString(IFS_GENERAL, IFK_IGNOREDINITIALCAPS, '');
    IgnoredTwoInitialCaps.AddStrings(Items);
    IniFile.ReadSections(Items);
    for I := 0 to Items.Count - 1 do
    begin
      Section := Items[I];
      Text := IniFile.ReadString(Section, IFK_REPLACETEXT, '');
      if Text = '' then Continue;
      if not CustomEntries.Find(Section, Entry) then
      begin //an exception is raised if you attempt to add a duplicate entry
        Entry := CustomEntries.Add;
        Entry.FindText := Section;
      end;
      Entry.ReplaceText := Text;
      Entry.FontColorToSet := StringToColorDef(IniFile.ReadString(Section,
        IFK_FONTCOLOR, ''), clNone);
      Entry.FontNameToSet := IniFile.ReadString(Section, IFK_FONTNAME, '');
      Entry.FontSizeToSet := IniFile.ReadInteger(Section, IFK_FONTSIZE, 0);
      Entry.HighlightColorToSet := StringToColorDef(IniFile.ReadString(Section,
        IFK_HIGHLIGHTCOLOR, ''), clNone);
      if IniFile.ReadBool(Section, IFK_EMBOLDEN, False) then
        Entry.StylesToAdd := Entry.StylesToAdd + [fsBold];
      if IniFile.ReadBool(Section, IFK_ITALICIZE, False) then
        Entry.StylesToAdd := Entry.StylesToAdd + [fsItalic];
      if IniFile.ReadBool(Section, IFK_STRIKEOUT, False) then
        Entry.StylesToAdd := Entry.StylesToAdd + [fsStrikeOut];
      if IniFile.ReadBool(Section, IFK_UNDERLINE, False) then
        Entry.StylesToAdd := Entry.StylesToAdd + [fsUnderline];
    end;
  finally
    Items.Free;
    CustomEntries.EndUpdate;
  end;
  Modified := False;
end;

procedure TAutoCorrectEngine.SaveToIniFile(const FileName: string);
var
  IniFile: TMemIniFile;
  Strings: TStringList;
  UTF8: UTF8String;
begin
  Strings := nil;
  IniFile := TMemIniFile.Create('');
  try
    SaveToIniFile(IniFile);
    Strings := TStringList.Create;
    IniFile.GetStrings(Strings);
    with TFileStream.Create(FileName, fmCreate) do
    try
      UTF8 := AnsiToUtf8(Strings.Text);
      WriteBuffer(UTF8BOM, SizeOf(UTF8BOM));
      WriteBuffer(PAnsiChar(UTF8)^, Length(UTF8));
    finally
      Free;
    end;
  finally
    IniFile.Free;
    Strings.Free;
  end;
end;

procedure TAutoCorrectEngine.SaveToIniFile(IniFile: TCustomIniFile);
var
  I: Integer;
  OldSections: TStringList;
begin
  if IniFile is TMemIniFile then
    TMemIniFile(IniFile).Clear
  else
  begin
    OldSections := TStringList.Create;
    try
      IniFile.ReadSections(OldSections);
      for I := 0 to OldSections.Count - 1 do
        IniFile.EraseSection(OldSections[I]);
    finally
      OldSections.Free;
    end;
  end;
  IniFile.WriteString(IFS_GENERAL, IFK_IGNOREDINITIALCAPS, IgnoredTwoInitialCaps.CommaText);
  for I := 0 to CustomEntries.Count - 1 do
    with CustomEntries[I] do
    begin
      IniFile.WriteString(FindText, IFK_REPLACETEXT, ReplaceText);
      if FontColorToSet <> clNone then
        IniFile.WriteString(FindText, IFK_FONTCOLOR, ColorToString(FontColorToSet));
      if FontNameToSet <> '' then
        IniFile.WriteString(FindText, IFK_FONTNAME, FontNameToSet);
      if FontSizeToSet <> 0 then
        IniFile.WriteInteger(FindText, IFK_FONTSIZE, FontSizeToSet);
      if HighlightColorToSet <> clNone then
        IniFile.WriteString(FindText, IFK_HIGHLIGHTCOLOR, ColorToString(HighlightColorToSet));
      if fsBold in StylesToAdd then
        IniFile.WriteBool(FindText, IFK_EMBOLDEN, True);
      if fsItalic in StylesToAdd then
        IniFile.WriteBool(FindText, IFK_ITALICIZE, True);
      if fsUnderline in StylesToAdd then
        IniFile.WriteBool(FindText, IFK_UNDERLINE, True);
      if fsStrikeOut in StylesToAdd then
        IniFile.WriteBool(FindText, IFK_STRIKEOUT, True);
    end;
  if IniFile.FileName <> '' then IniFile.UpdateFile;
end;

end.
