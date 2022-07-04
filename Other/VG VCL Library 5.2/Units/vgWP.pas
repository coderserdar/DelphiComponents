{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Report components for MS Word: core           }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgWP;

interface

uses
  SysUtils, Classes, vgOleUtl;

type
  TBookmarkKind = (bkField, bkGroup, bkSQL);
  TReplaceMode  = (rmPrint, rmText, rmClear);
  TWordVersion  = (wvVersion7, wvVersion8);

  TWordBookmarkDataSource = class;
  TvgWordPrint            = class;

  TWordBookmark = class
  private
    FBookmarks: TList;
    FWordPrint: TvgWordPrint;
    FDataSource: TWordBookmarkDataSource;
    FGroup, FParentGroup: TWordBookmark;
    FKind: TBookmarkKind;
    FLevel: Integer;
    FFieldName, FMask, FName, FSQL: string;
    FPrinted: Boolean;
    FSelStart, FSelEnd: Integer;
    FSuppressDupes: Boolean;
    FOldText: string;
    function GetBookmark(Index: Integer): TWordBookmark;
    function GetCount: Integer;
  protected
    constructor Create(AName: string; AWordPrint: TvgWordPrint);
  public
    destructor Destroy; override;
    function BookmarkByName(AName: string): TWordBookmark;
    function FindBookmark(AName: string): TWordBookmark;
    function FindBookmarkSQL: TWordBookmark;
    function BookmarkSQL: TWordBookmark;
    property Bookmarks[Index: Integer]: TWordBookmark read GetBookmark; default;
    property Count: Integer read GetCount;
    property DataSource: TWordBookmarkDataSource read FDataSource;
    property FieldName: string read FFieldName write FFieldName;
    property Group: TWordBookmark read FGroup;
    property Kind: TBookmarkKind read FKind;
    property Level: Integer read FLevel;
    property Mask: string read FMask write FMask;
    property Name: string read FName;
    property ParentGroup: TWordBookmark read FParentGroup;
    property SelEnd: Integer read FSelEnd;
    property SelStart: Integer read FSelStart;
    property SQL: string read FSQL write FSQL;
    property SuppressDupes: Boolean read FSuppressDupes;
    property WordPrint: TvgWordPrint read FWordPrint;
  end;

  TWordBookmarkDataSource = class
  private
    FBookmark: TWordBookmark;
    FRecordCount: Integer;
  protected
    procedure DataChanged; virtual;
    function GetBOF: Boolean; virtual;
    function GetEOF: Boolean; virtual;
    property Bookmark: TWordBookmark read FBookmark;
  public
    constructor Create(ABookmark: TWordBookmark); virtual;
    destructor Destroy; override;
    procedure Close; virtual;
    procedure First; virtual;
    procedure Next; virtual;
    procedure Open; virtual;
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property RecordCount: Integer read FRecordCount;
  end;

  TSingleRecordDataSource = class(TWordBookmarkDataSource)
  protected
    function GetEOF: Boolean; override;
  end;

  TBookmarkTextEvent =  procedure(Sender: TObject; Bookmark: TWordBookmark;
    var Text: string) of object;

  TBookmarkClipboardEvent = procedure(Sender: TObject; Bookmark: TWordBookmark;
    var Copied: Boolean) of object;

  TBookmarkPrintEvent = procedure(Sender: TObject; Bookmark: TWordBookmark;
    var Print: Boolean) of object;

  TReplaceModeEvent =  procedure(Sender: TObject; Bookmark: TWordBookmark;
    var ReplaceMode: TReplaceMode; var Text: string) of object;

  TBookmarkEvent = procedure(Sender: TObject; Bookmark: TWordBookmark) of object;

  TvgWordPrint = class(TComponent)
  private
    FClearNotPrinted, FActiveOleObject,
    FCloseSourceWindow, FCloseDestWindowOnError: Boolean;
    FBookmarks: TList;
    FConnected, FStreamedConnected: Boolean;
    FGroups: array [TBookmarkKind] of TList;
    FDispInvokeCache: TDispInvokeCache;
    FMacroChar, FMaskSeparator: Char;
    FMaxLevel: Integer;
    FPrinting: Boolean;
    FSourceDoc, FDestDoc: TFileName;
    FSourceWindow, FTmpWindow: Integer;
    FSourceWindowName, FTmpWindowName: string;
    FWordBasic: Variant;
    FWordVersionStr: string;
    FWordVisible, FStreamedWordVisible: Boolean;
    { Events }
    FOnBeginAnalize, FOnEndAnalize: TNotifyEvent;
    FOnBeginBookmarkCreate, FOnEndBookmarkCreate: TNotifyEvent;
    FOnBeginBookmarksCreate, FOnEndBookmarksCreate: TNotifyEvent;
    FOnBeginPrint, FOnEndPrint: TNotifyEvent;
    FOnBeginPrintGroup, FOnEndPrintGroup: TBookmarkEvent;
    FOnBeginPrintField, FOnEndPrintField: TBookmarkEvent;
    FOnBeginPrintRecord, FOnEndPrintRecord: TBookmarkEvent;
    FOnCopyToClipboard: TBookmarkClipboardEvent;
    FOnDataChange, FOnDataOpen, FOnDataClose: TBookmarkEvent;
    FOnFieldPrint: TBookmarkPrintEvent;
    FOnFieldText: TBookmarkTextEvent;
    FOnGroupReplace: TReplaceModeEvent;
    procedure CheckConnected;
    procedure CheckFileExists(FileName: TFileName);
    procedure CheckFileName(FileName: TFileName);
    procedure CreateWordBookmarks;
    procedure DoEditClear(Bookmark: TWordBookmark);
    procedure DoEditCut(Bookmark: TWordBookmark);
    procedure DoEditGoto(Dest: string);
    procedure DoEditPaste(Bookmark: TWordBookmark);
    procedure DoInsert(Bookmark: TWordBookmark; Text: string);
    procedure FreeWordBookmarks;
    function GetBookmark(Index: Integer): TWordBookmark;
    function GetBookmarkCount: Integer;
    function GetGroup(Kind: TBookmarkKind; Index: Integer): TWordBookmark;
    function GetGroupCount(Kind: TBookmarkKind): Integer;
    function GetWordVersion: TWordVersion;
    function GetWordVersionStr: string;
    procedure NewDoc(var Window: Integer; var WindowName: string);
    procedure OpenDoc(var Window: Integer; var WindowName: string);
    procedure PrintGroup(Group: TWordBookmark);
    procedure PrintGroups(Group: TWordBookmark; Level: Integer);
    procedure PrintField(Bookmark: TWordBookmark);
    procedure PrintFields(Group: TWordBookmark);
    procedure SetConnected(Value: Boolean);
    procedure SetDestDoc(Value: TFileName);
    procedure SetMacroChar(Value: Char);
    procedure SetMaskSeparator(Value: Char);
    procedure SetSourceDoc(Value: TFileName);
    procedure SetPrinted(Group:TWordBookmark; Printed: Boolean);
    procedure SetPrinting(Value: Boolean);
    procedure SetWordVisible(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure BeginAnalize; virtual;
    procedure BeginBookmarkCreate; virtual;
    procedure BeginBookmarksCreate; virtual;
    procedure BeginPrint; virtual;
    procedure BeginPrintGroup(Group: TWordBookmark); virtual;
    procedure BeginPrintField(Bookmark: TWordBookmark); virtual;
    procedure BeginPrintRecord(Group: TWordBookmark); virtual;
    function BookmarkToClipboard(ABookmark: TWordBookmark): Boolean; virtual;
    procedure CheckNotPrinting;
    function CopyToClipboard(ABookmark: TWordBookmark): Boolean; virtual;
    procedure CreateDataSource(Group: TWordBookmark); virtual;
    function CreateWordBookmark(AName: string): TWordBookmark; virtual;
    function DataBOF(Group: TWordBookmark): Boolean;
    function DataEOF(Group: TWordBookmark): Boolean;
    procedure DataChanged(Group: TWordBookmark); virtual;
    procedure DataClose(Group: TWordBookmark); virtual;
    procedure DataFirst(Group: TWordBookmark);
    procedure DataNext(Group: TWordBookmark);
    procedure DataOpen(Group: TWordBookmark); virtual;
    procedure DoCopyText(var Str: string);
    procedure EndAnalize; virtual;
    procedure EndBookmarkCreate; virtual;
    procedure EndBookmarksCreate; virtual;
    procedure EndPrint; virtual;
    procedure EndPrintGroup(Group: TWordBookmark); virtual;
    procedure EndPrintField(Bookmark: TWordBookmark); virtual;
    procedure EndPrintRecord(Group: TWordBookmark); virtual;
    function FieldText(Bookmark: TWordBookmark; Text: string): string; virtual;
    function FieldPrint(Bookmark: TWordBookmark): Boolean; virtual;
    function GroupReplaceMode(Bookmark: TWordBookmark; var Text: string): TReplaceMode; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckConnection: Boolean;
    procedure Connect; virtual;
    function DefaultCopyToClipboard(ABookmark: TWordBookmark): Boolean; virtual;
    procedure Disconnect; virtual;
    function IsParent(Bookmark, ParentBookmark: TWordBookmark): Boolean;
    procedure Print;
    procedure ReadDispInvokeCache(Stream: TStream);
    procedure WriteDispInvokeCache(Stream: TStream);
    { Properties }
    property ActiveOleObject: Boolean read FActiveOleObject;
    property BookmarkCount: Integer read GetBookmarkCount;
    property Bookmarks[Index: Integer]: TWordBookmark read GetBookmark;
    property GroupCount[Kind: TBookmarkKind]: Integer read GetGroupCount;
    property Groups[Kind: TBookmarkKind; Index: Integer]: TWordBookmark read GetGroup;
    property MaxLevel: Integer read FMaxLevel;
    property SourceWindow: Integer read FSourceWindow;
    property SourceWindowName: string read FSourceWindowName;
    property TmpWindow: Integer read FTmpWindow;
    property TmpWindowName: string read FTmpWindowName;
    property WordBasic: Variant read FWordBasic;
    property WordVersion: TWordVersion read GetWordVersion;
    property WordVersionStr: string read GetWordVersionStr;
  published
    property ClearNotPrinted: Boolean read FClearNotPrinted write FClearNotPrinted default True;
    property CloseSourceWindow: Boolean read FCloseSourceWindow write FCloseSourceWindow default True;
    property CloseDestWindowOnError: Boolean read FCloseDestWindowOnError write FCloseDestWindowOnError default True;
    property Connected: Boolean read FConnected write SetConnected;
    property DestDoc: TFileName read FDestDoc write SetDestDoc;
    property MacroChar: Char read FMacroChar write SetMacroChar default '%';
    property MaskSeparator: Char read FMaskSeparator write SetMaskSeparator default '@';
    property SourceDoc: TFileName read FSourceDoc write SetSourceDoc;
    property Printing: Boolean read FPrinting write SetPrinting stored False;
    property WordVisible: Boolean read FWordVisible write SetWordVisible;
    { Events }
    property OnBeginAnalize: TNotifyEvent read FOnBeginAnalize write FOnBeginAnalize;
    property OnBeginBookmarkCreate: TNotifyEvent read FOnBeginBookmarkCreate write FOnBeginBookmarkCreate;
    property OnBeginBookmarksCreate: TNotifyEvent read FOnBeginBookmarksCreate write FOnBeginBookmarksCreate;
    property OnBeginPrint: TNotifyEvent read FOnBeginPrint write FOnBeginPrint;
    property OnBeginPrintGroup: TBookmarkEvent read FOnBeginPrintGroup write FOnBeginPrintGroup;
    property OnBeginPrintField: TBookmarkEvent read FOnBeginPrintField write FOnBeginPrintField;
    property OnBeginPrintRecord: TBookmarkEvent read FOnBeginPrintRecord write FOnBeginPrintRecord;
    property OnCopyToClipboard: TBookmarkClipboardEvent read FOnCopyToClipboard write FOnCopyToClipboard;
    property OnDataClose: TBookmarkEvent read FOnDataClose write FOnDataClose;
    property OnDataChange: TBookmarkEvent read FOnDataChange write FOnDataChange;
    property OnDataOpen: TBookmarkEvent read FOnDataOpen write FOnDataOpen;
    property OnEndAnalize: TNotifyEvent read FOnEndAnalize write FOnEndAnalize;
    property OnEndBookmarkCreate: TNotifyEvent read FOnEndBookmarkCreate write FOnEndBookmarkCreate;
    property OnEndBookmarksCreate: TNotifyEvent read FOnEndBookmarksCreate write FOnEndBookmarksCreate;
    property OnEndPrint: TNotifyEvent read FOnEndPrint write FOnEndPrint;
    property OnEndPrintGroup: TBookmarkEvent read FOnEndPrintGroup write FOnEndPrintGroup;
    property OnEndPrintField: TBookmarkEvent read FOnEndPrintField write FOnEndPrintField;
    property OnEndPrintRecord: TBookmarkEvent read FOnEndPrintRecord write FOnEndPrintRecord;
    property OnFieldPrint: TBookmarkPrintEvent read FOnFieldPrint write FOnFieldPrint;
    property OnFieldText: TBookmarkTextEvent read FOnFieldText write FOnFieldText;
    property OnGroupReplace: TReplaceModeEvent read FOnGroupReplace write FOnGroupReplace;
  end;

  EWordPrintError       = class(Exception);
  EBookmarkNotFound     = class(EWordPrintError);
  EConnectError         = class(EWordPrintError);
  EInvalidConnectState  = class(EConnectError);
  EInvalidChar          = class(EWordPrintError);
  EPrintingError        = class(EWordPrintError);
  EFileNotFound         = class(EPrintingError);
  EInvalidFileName      = class(EPrintingError);
  EWordDocumentError    = class(EPrintingError);
  ENewDocError          = class(EPrintingError);
  EOpenDocError         = class(EPrintingError);
  EBookmarksNotFound    = class(EPrintingError);
  EGroupsNotFound       = class(EPrintingError);
  EBookmarkSQLNotFound  = class(EPrintingError);
  EInvalidBookmark      = class(EPrintingError);
  EInvalidBookmarkText  = class(EPrintingError);
  EInvalidNesting       = class(EPrintingError);
  EDuplicateSQL         = class(EPrintingError);

implementation
uses vgWPRes, Windows, vgUtils, vgVCLUtl, Clipbrd, vgClpbrd, Controls, Forms,
  {$IFDEF _D3_}ComObj{$ELSE}OleAuto{$ENDIF};

const
  SignatureSQL          = '_SQL';
  SignatureField        = '_';
  SpecialChars          = ['@', '#', '$', '%', '^', '&', '*', '<', '>'];
  SignatureSuppresDupes = 'NODUPES';

{  TWordBookmark  }

constructor TWordBookmark.Create(AName: string; AWordPrint: TvgWordPrint);
  procedure CheckBookmarkName(Str: string);
  var
    I, Count: Integer;
  begin
    if (Length(Str) = 0) then
      raise EInvalidBookmark.Create(Format(LoadStr(SInvalidBookmark), [Str]));
    Count := 0;
    for I := 1 to Length(Str) do
    begin
      if Str[I] = SignatureField then Inc(Count);
      if Count > 2 then
        raise EInvalidBookmark.Create(Format(LoadStr(SInvalidBookmark), [Str]));
    end;
  end;
begin
  CheckBookmarkName(AName);
  FBookmarks := TList.Create;
  FName := AName;
  FWordPrint := AWordPrint;
end;

destructor TWordBookmark.Destroy;
begin
  FDataSource.Free;
  FBookmarks.Free;
  inherited;
end;

function TWordBookmark.BookmarkByName(AName: string): TWordBookmark;
begin
  Result := FindBookmark(AName);
  if not Assigned(Result) then
    raise EBookmarkNotFound.Create(Format(LoadStr(SBookmarkNotFound), [AName]));
end;

function TWordBookmark.FindBookmark(AName: string): TWordBookmark;
var
  I: Integer;
begin
  for I := 0 to FBookmarks.Count - 1 do
  begin
    Result := FBookmarks[I];
    if AnsiCompareText(Result.Name, AName) = 0 then Exit;
  end;
  Result := nil;
end;

function TWordBookmark.FindBookmarkSQL: TWordBookmark;
var
  I: Integer;
  Tmp: TWordBookmark;
begin
  Result := nil;
  for I := 0 to FBookmarks.Count - 1 do
  begin
    Tmp := FBookmarks[I];
    if Tmp.Kind = bkSQL then
    begin
      Result := Tmp;
      Break;
    end;
  end;
end;

function TWordBookmark.BookmarkSQL: TWordBookmark;
begin
  Result := FindBookmarkSQL;
  if not Assigned(Result) then
    raise EBookmarkSQLNotFound(Format(LoadStr(SBookmarkSQLNotFound), [FName]));
end;

function TWordBookmark.GetBookmark(Index: Integer): TWordBookmark;
begin
  Result := FBookmarks[Index];
end;

function TWordBookmark.GetCount: Integer;
begin
  Result := FBookmarks.Count;
end;

{ TWordBookmarkDataSource }
constructor TWordBookmarkDataSource.Create(ABookmark: TWordBookmark);
begin
  ABookmark.FDataSource := Self;
  FBookmark := ABookmark;
end;

destructor TWordBookmarkDataSource.Destroy;
begin
  Close;
  inherited;
end;

procedure TWordBookmarkDataSource.DataChanged;
begin
  FBookmark.FWordPrint.DataChanged(FBookmark);
end;

function TWordBookmarkDataSource.GetBOF: Boolean;
begin
  Result := True;
end;

function TWordBookmarkDataSource.GetEOF: Boolean;
begin
  Result := True;
end;

procedure TWordBookmarkDataSource.Close;
begin
  DataChanged;
end;

procedure TWordBookmarkDataSource.First;
begin
  FRecordCount := 0;
  DataChanged;
end;

procedure TWordBookmarkDataSource.Next;
begin
  Inc(FRecordCount);
  if not GetEOF then DataChanged;
end;

procedure TWordBookmarkDataSource.Open;
begin
  First;
end;

{ TSingleRecordDataSource }
function TSingleRecordDataSource.GetEOF: Boolean;
begin
  Result := FRecordCount = 1;
end;

{  TvgWordPrint  }

constructor TvgWordPrint.Create(AOwner: TComponent);
var
  I: TBookmarkKind;
begin
  inherited;
  FClearNotPrinted := True;
  FCloseSourceWindow := True;
  FCloseDestWindowOnError := True;
  FMacroChar := '%';
  FMaskSeparator := '@';
  FDispInvokeCache := TDispInvokeCache.Create;
  FBookmarks := TList.Create;
  for I := Low(TBookmarkKind) to High(TBookmarkKind) do FGroups[I] := TList.Create;
end;

destructor TvgWordPrint.Destroy;
var
  I: TBookmarkKind;
begin
  Disconnect;
  FDispInvokeCache.Free;
  FBookmarks.Free;
  for I := Low(TBookmarkKind) to High(TBookmarkKind) do FGroups[I].Free;
  inherited;
end;

procedure TvgWordPrint.Loaded;
begin
  inherited;
  try
    SetWordVisible(FStreamedWordVisible);
    SetConnected(FStreamedConnected);
  except
    if (csDesigning in ComponentState) then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TvgWordPrint.BeginAnalize;
begin
  if Assigned(FOnBeginAnalize) then FOnBeginAnalize(Self);
end;

procedure TvgWordPrint.BeginBookmarkCreate;
begin
  if Assigned(FOnBeginBookmarkCreate) then FOnBeginBookmarkCreate(Self);
end;

procedure TvgWordPrint.BeginBookmarksCreate;
begin
  if Assigned(FOnBeginBookmarksCreate) then FOnBeginBookmarksCreate(Self);
end;

procedure TvgWordPrint.BeginPrint;
begin
  if Assigned(FOnBeginPrint) then FOnBeginPrint(Self);
end;

procedure TvgWordPrint.BeginPrintGroup(Group: TWordBookmark);
begin
  if Assigned(FOnBeginPrintGroup) then FOnBeginPrintGroup(Self, Group);
end;

procedure TvgWordPrint.BeginPrintField(Bookmark: TWordBookmark);
begin
  if Assigned(FOnBeginPrintField) then FOnBeginPrintField(Self, Bookmark);
end;

procedure TvgWordPrint.BeginPrintRecord(Group: TWordBookmark);
begin
  if Assigned(FOnBeginPrintRecord) then FOnBeginPrintRecord(Self, Group);
end;

function TvgWordPrint.BookmarkToClipboard(ABookmark: TWordBookmark): Boolean;
var
  Print: Boolean;
begin
  Print := FieldPrint(ABookmark);
  if Print then
    Result := CopyToClipboard(ABookmark) else
    Result := False;
end;

procedure TvgWordPrint.CheckConnected;
begin
  if not FConnected then
    raise EInvalidConnectState.Create(LoadStr(SNotConnected));
end;

procedure TvgWordPrint.CheckFileExists(FileName: TFileName);
begin
  if not FileExists(FileName) then
    raise EFileNotFound.Create(Format(LoadStr(SFileNotFound), [FileName]));
end;

procedure TvgWordPrint.CheckFileName(FileName: TFileName);
  { ValidFileName source was copied from RxLib library }
  function ValidFileName(const FileName: string): Boolean;
    function HasAny(const Str, Substr: string): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      for I := 1 to Length(Substr) do begin
        if Pos(Substr[I], Str) > 0 then begin
          Result := True;
          Break;
        end;
      end;
    end;
  begin
    Result := (FileName <> '') and (not HasAny(FileName, ';,=+<>"[]|'));
    if Result then Result := Pos('\', ExtractFileName(FileName)) = 0;
  end;
begin
  if not ValidFileName(FileName) then
    raise EInvalidFileName.Create(Format(LoadStr(SInvalidFileName), [FileName]));
end;

function TvgWordPrint.CheckConnection: Boolean;
var
  S: string;
begin
  try
    S := WordBasic.AppInfo(2);
    Result := True;
  except
    Result := False;
  end;
end;
procedure TvgWordPrint.CheckNotPrinting;
begin
  if FPrinting then
    raise EPrintingError.Create(LoadStr(SPrinting));
end;

procedure TvgWordPrint.Connect;
begin
  SetConnected(True);
end;

function TvgWordPrint.DefaultCopyToClipboard(ABookmark: TWordBookmark): Boolean;
var
  Text: string;
begin
  Text := FieldText(ABookmark, '');
  ClipboardCopy(Text);
  Result := (Text <> '');
end;

function TvgWordPrint.CopyToClipboard(ABookmark: TWordBookmark): Boolean;
begin
  Result := False;
  if Assigned(FOnCopyToClipboard) then
    FOnCopyToClipboard(Self, ABookmark, Result) else
    Result := DefaultCopyToClipboard(ABookmark);
end;

procedure TvgWordPrint.CreateDataSource(Group: TWordBookmark);
begin
  TSingleRecordDataSource.Create(Group);
end;

function TvgWordPrint.CreateWordBookmark(AName: string): TWordBookmark;
begin
  Result := TWordBookmark.Create(AName, Self);
end;

procedure TvgWordPrint.Disconnect;
begin
  SetConnected(False);
end;

procedure TvgWordPrint.DataChanged(Group: TWordBookmark);
begin
  if Assigned(FOnDataChange) then FOnDataChange(Self, Group);
end;

function TvgWordPrint.DataBOF(Group: TWordBookmark): Boolean;
begin
  Result := Group.DataSource.BOF;
end;

function TvgWordPrint.DataEOF(Group: TWordBookmark): Boolean;
begin
  Result := Group.DataSource.EOF;
end;

procedure TvgWordPrint.DataClose(Group: TWordBookmark);
begin
  if Assigned(FOnDataClose) then FOnDataClose(Self, Group);
  Group.DataSource.Close;
end;

procedure TvgWordPrint.DataFirst(Group: TWordBookmark);
begin
  Group.DataSource.First;
end;

procedure TvgWordPrint.DataNext(Group: TWordBookmark);
begin
  Group.DataSource.Next;
end;

procedure TvgWordPrint.DataOpen(Group: TWordBookmark);
begin
  if Assigned(FOnDataOpen) then FOnDataOpen(Self, Group);
  Group.DataSource.Open;
end;

procedure TvgWordPrint.DoCopyText(var Str: string);
begin
  Str := Trim(FWordBasic.Selection);
end;

procedure TvgWordPrint.DoEditClear(Bookmark: TWordBookmark);
begin
  FWordBasic.EditClear;
  SetPrinted(Bookmark, True);
end;

procedure TvgWordPrint.DoEditCut(Bookmark: TWordBookmark);
begin
  FWordBasic.EditCut;
  SetPrinted(Bookmark, True);
end;

procedure TvgWordPrint.DoEditGoto(Dest: string);
begin
  case WordVersion of
    wvVersion7:
      FWordBasic.EditGoto(Destination := Dest);
    wvVersion8:
      FWordBasic.WW7_EditGoto(Destination := Dest);
  end;
end;

procedure TvgWordPrint.DoEditPaste(Bookmark: TWordBookmark);
begin
  FWordBasic.EditPaste;
  SetPrinted(Bookmark, True);
end;

procedure TvgWordPrint.DoInsert(Bookmark: TWordBookmark; Text: string);
begin
  FWordBasic.Insert(Text);
  SetPrinted(Bookmark, True);
end;

procedure TvgWordPrint.EndAnalize;
begin
  if Assigned(FOnEndAnalize) then FOnEndAnalize(Self);
end;

procedure TvgWordPrint.EndBookmarkCreate;
begin
  if Assigned(FOnEndBookmarkCreate) then FOnEndBookmarkCreate(Self);
end;

procedure TvgWordPrint.EndBookmarksCreate;
begin
  if Assigned(FOnEndBookmarksCreate) then FOnEndBookmarksCreate(Self);
end;

procedure TvgWordPrint.EndPrint;
begin
  if Assigned(FOnEndPrint) then FOnEndPrint(Self);
end;

procedure TvgWordPrint.EndPrintGroup(Group: TWordBookmark);
begin
  if Assigned(FOnEndPrintGroup) then FOnEndPrintGroup(Self, Group);
end;

procedure TvgWordPrint.EndPrintField(Bookmark: TWordBookmark);
begin
  if Assigned(FOnEndPrintField) then FOnEndPrintField(Self, Bookmark);
end;

procedure TvgWordPrint.EndPrintRecord(Group: TWordBookmark);
begin
  if Assigned(FOnEndPrintRecord) then FOnEndPrintRecord(Self, Group);
end;

function TvgWordPrint.FieldText(Bookmark: TWordBookmark; Text: string): string;
begin
  Result := Text;
  if Assigned(FOnFieldText) then FOnFieldText(Self, Bookmark, Result);
end;

function TvgWordPrint.FieldPrint(Bookmark: TWordBookmark): Boolean;
begin
  if Bookmark.Kind = bkSQL then
    Result := False else
    Result := True;
  if Assigned(FOnFieldPrint) then FOnFieldPrint(Self, Bookmark, Result);
end;

procedure TvgWordPrint.FreeWordBookmarks;
var
  I: TBookmarkKind;
  J: Integer;
  Tmp: TObject;
begin
  for I := Low(TBookmarkKind) to High(TBookmarkKind) do FGroups[I].Clear;
  for J := FBookmarks.Count - 1 downto 0 do
  begin
    Tmp := TObject(FBookmarks[J]);
    Tmp.Free;
    FBookmarks.Delete(J);
  end;
end;

function TvgWordPrint.GroupReplaceMode(Bookmark: TWordBookmark; var Text: string): TReplaceMode;
begin
  Result := rmPrint; Text := '';
  if Assigned(FOnGroupReplace) then FOnGroupReplace(Self, Bookmark, Result, Text);
end;

function TvgWordPrint.GetBookmark(Index: Integer): TWordBookmark;
begin
  Result := FBookmarks[Index];
end;

function TvgWordPrint.GetBookmarkCount: Integer;
begin
  Result := FBookmarks.Count;
end;

function TvgWordPrint.GetGroup(Kind: TBookmarkKind; Index: Integer): TWordBookmark;
begin
  Result := FGroups[Kind][Index];
end;

function TvgWordPrint.GetGroupCount(Kind: TBookmarkKind): Integer;
begin
  Result := FGroups[Kind].Count;
end;

function TvgWordPrint.GetWordVersion: TWordVersion;
begin
  CheckConnected;
  Result := wvVersion7;
  if FWordVersionStr >= '8.0' then Result := wvVersion8;
end;

function TvgWordPrint.GetWordVersionStr: string;
begin
  CheckConnected;
  Result := FWordVersionStr;
end;

function TvgWordPrint.IsParent(Bookmark, ParentBookmark: TWordBookmark): Boolean;
begin
  Result := False;
  if (Bookmark.Kind = bkGroup) and (ParentBookmark.Kind = bkGroup) then
  while Bookmark <> nil do
  begin
    if Bookmark.ParentGroup = ParentBookmark then
    begin
      Result := True;
      Break;
    end;
    Bookmark := Bookmark.ParentGroup;
  end;
end;

procedure TvgWordPrint.NewDoc(var Window: Integer; var WindowName: string);
begin
  CheckConnected;
  try
    FWordBasic.FileNew(Template := FSourceDoc);
    Window := FWordBasic.Window;
    WindowName := FWordBasic.WindowName;
  except
    raise ENewDocError.Create(Format(LoadStr(SNewDocError), [Exception(ExceptObject).Message]));
  end;
end;

procedure TvgWordPrint.OpenDoc(var Window: Integer; var WindowName: string);
begin
  CheckConnected;
  try
    FWordBasic.FileOpen(Name := FSourceDoc, ReadOnly := True);
    Window := FWordBasic.Window;
    WindowName := FWordBasic.WindowName;
  except
    raise EOpenDocError.Create(Format(LoadStr(SOpenDocError), [Exception(ExceptObject).Message]));
  end;
end;

procedure TvgWordPrint.Print;
begin
  CheckNotPrinting;
  BeginPrint;
  FPrinting := True;
  try
    CheckConnected;
    CheckFileName(FSourceDoc);
    if (FDestDoc <> '') then CheckFileName(FDestDoc);
    CheckFileExists(FSourceDoc);
    OpenDoc(FSourceWindow, FSourceWindowName);
    try
      CreateWordBookmarks;
      NewDoc(FTmpWindow, FTmpWindowName);
      if AnsiCompareStr(FSourceWindowName, FTmpWindowName) > 0 then
        Inc(FSourceWindow);
      try
        try
          PrintGroups(nil, 0);
          FWordBasic.StartOfDocument;
        except
          if FCloseDestWindowOnError then
          try
            FWordBasic.WindowList(FTmpWindow);
            SetWordVisible(True);
            FWordBasic.FileClose(1);
            if AnsiCompareStr(FSourceWindowName, FTmpWindowName) > 0 then
              Dec(FSourceWindow);
          except end;
          raise;
        end;
      finally
        if FCloseSourceWindow then
        try
          FWordBasic.WindowList(FSourceWindow);
          FWordBasic.FileClose(2);
        except end;
      end;
    finally
      FreeWordBookmarks;
    end;
  finally
    Clipboard.Clear;
    FPrinting := False;
    EndPrint;
  end;
end;

procedure TvgWordPrint.PrintGroup(Group: TWordBookmark);

  procedure ClearChildBookmarks;
  var
    I: Integer;
    Bookmark: TWordBookmark;
  begin
    for I := 0 to FBookmarks.Count - 1 do
    begin
      Bookmark := FBookmarks[I];
      if (Bookmark.ParentGroup = Group) and not Bookmark.FPrinted then
      begin
        DoEditGoto(Bookmark.Name);
        DoEditClear(Bookmark);
      end;
    end;
  end;

  procedure CopyBookmark;
  var
    SelStart, SelEnd: Integer;
  begin
    { Copy bookmark for each record in dataset }
    FWordBasic.WindowList(FSourceWindow);
    DoEditGoto(Group.Name);
    FWordBasic.EditCopy;
    FWordBasic.WindowList(FTmpWindow);
    SelStart := FWordBasic.GetSelEndPos;
    DoEditPaste(Group);
    SetPrinted(Group, False);
    SelEnd := FWordBasic.GetSelEndPos;
    FWordBasic.SetSelRange(SelStart, SelEnd);
    FWordBasic.EditBookmark(Name := Group.Name, Add := 1);
  end;

  procedure DeleteBookmarks;
  var
    I: Integer;
    Bookmark: TWordBookmark;
  begin
    if FClearNotPrinted then
    begin
      for I := 0 to Group.Count - 1 do
      begin
        Bookmark := Group[I];
        if not Bookmark.FPrinted then
        begin
          DoEditGoto(Bookmark.Name);
          DoEditClear(Bookmark);
        end;
      end;
    end;
  end;

var
  I: Integer;
  Text: string;
begin
  BeginPrintGroup(Group);
  try
    { Select and cut off group }
    FWordBasic.WindowList(FTmpWindow);
    DoEditGoto(Group.Name);
    { Replace mode group }
    { Print group }
    case GroupReplaceMode(Group, Text) of
      rmPrint:
        begin
          DataOpen(Group);

          for I := 0 to Group.Count - 1 do
            Group[I].FOldText := '';

          if not DataEOF(Group) then
          begin
            while not DataEOF(Group) do
            begin
              if not DataBOF(Group) then CopyBookmark;
              BeginPrintRecord(Group);
              try
                PrintFields(Group);
              finally
                EndPrintRecord(Group);
              end;
              DeleteBookmarks;
              PrintGroups(Group, Group.Level + 1);
              ClearChildBookmarks;
              DoEditGoto(Group.Name);
              DoEditGoto('\EndOfSel');
              DataNext(Group);
            end;
            FWordBasic.WindowList(FTmpWindow);
            DoEditGoto(Group.Name);
            FWordBasic.EditBookmark(Name := Group.Name, Delete := 1);
          end else
            DoEditCut(Group);
          DataClose(Group);
        end;
      rmText:
        begin
          BeginPrintRecord(Group);
          try
            DoEditClear(Group);
            DoInsert(Group, Text);
          finally
            EndPrintRecord(Group);
          end;
        end;
      rmClear:
        DoEditCut(Group);
    end;
    { Delete printed bookmark }
    SetPrinted(Group, True);
  finally
    EndPrintGroup(Group);
  end;
end;

procedure TvgWordPrint.PrintGroups(Group: TWordBookmark; Level: Integer);
var
  I: Integer;
  TmpGroup: TWordBookmark;
begin
  if Level > FMaxLevel then Exit;
  for I := 0 to FGroups[bkGroup].Count - 1 do
  begin
    TmpGroup := TWordBookmark(FGroups[bkGroup][I]);
    if (TmpGroup.ParentGroup = Group) and (TmpGroup.Level = Level) then PrintGroup(TmpGroup);
  end;
end;

procedure TvgWordPrint.PrintField(Bookmark: TWordBookmark);
var
  Text: string;
  NotEmpty, HasText: Boolean;
begin
  if Bookmark.FPrinted then Exit;
  HasText := False;
  BeginPrintField(Bookmark);
  try
    DoEditGoto(Bookmark.Name);
    repeat until OpenClipboard(Application.Handle);
    try
      Clipboard.Open;
      try
        EmptyClipboard;
        NotEmpty := BookmarkToClipboard(Bookmark);
        if NotEmpty then
        begin
          HasText := Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT);
          if HasText then ClipboardPaste(Text);
        end;
      finally
        Clipboard.Close;
      end;
    finally
      CloseClipboard;
    end;
    if NotEmpty then
    begin
      if HasText then
      begin
        with Bookmark do
        begin
          if FSuppressDupes and (Text = FOldText) then
            Text := '' else FOldText := Text;
          if Text = '' then
            DoEditClear(Bookmark) else
          begin
            DoInsert(Bookmark, Text);
            { There is the strange thing with multiline text -   }
            { Word doesn't clears the bookmark                   }
            if (Pos(#10, Text) > 0) then
            try
              FWordBasic.EditBookmark(Name := Name, Delete := 1);
            except end;
          end;
        end;
      end else
        DoEditPaste(Bookmark);
    end else
      DoEditClear(Bookmark);
  finally
    EndPrintField(Bookmark);
  end;
end;

procedure TvgWordPrint.PrintFields(Group: TWordBookmark);
var
  I: Integer;
begin
  for I := 0 to Group.Count - 1 do PrintField(Group[I]);
end;

procedure TvgWordPrint.SetConnected(Value: Boolean);
const
  OleClassName = 'Word.Basic';
begin
  if (csLoading in ComponentState) then
    FStreamedConnected := Value
  else if not Value or (FConnected <> Value) then
  begin
    AppSetCursor(crHourglass);
    try
      if Value then
      try
        FActiveOleObject := False;
        try
          FActiveOleObject := vgOleUtl.GetActiveOleObject(OleClassName, FWordBasic) = S_OK;
        except end;
        if not FActiveOleObject then
          FWordBasic := CreateOleObject(OleClassName);
        FDispInvokeCache.Reset(FWordBasic);
        FWordVersionStr := FWordBasic.AppInfo(2);
      except
        VarClear(FWordBasic);
        raise EConnectError.Create(LoadStr(SConnectError));
      end else begin
        if not (FActiveOleObject or VarIsEmpty(FWordBasic)) then
        try
          FWordBasic.AppClose;
        except end;
        FDispInvokeCache.Reset(Unassigned);
        VarClear(FWordBasic);
      end;
      FConnected := Value;
      SetWordVisible(FWordVisible);
    finally
      AppRestoreCursor;
    end;
  end;
end;

procedure TvgWordPrint.SetDestDoc(Value: TFileName);
begin
  if FDestDoc <> Value then
  begin
    CheckNotPrinting;
    FDestDoc := Value;
  end;
end;

procedure TvgWordPrint.SetMacroChar(Value: Char);
begin
  if (FMaskSeparator <> Value) then
  begin
    CheckNotPrinting;
    if not (Value in SpecialChars) then
      raise EInvalidChar.Create(LoadStr(SInvalidChar));
    FMacroChar := Value;
  end;
end;

procedure TvgWordPrint.SetMaskSeparator(Value: Char);
begin
  if (FMaskSeparator <> Value) then
  begin
    CheckNotPrinting;
    if not (Value in SpecialChars) then
      raise EInvalidChar.Create(LoadStr(SInvalidChar));
    FMaskSeparator := Value;
  end;
end;

procedure TvgWordPrint.SetPrinted(Group: TWordBookmark; Printed: Boolean);
var
  I: Integer;
begin
  Group.FPrinted := Printed;
  for I := 0 to FBookmarks.Count - 1 do
    if Bookmarks[I].ParentGroup = Group then SetPrinted(FBookmarks[I], Printed);
end;

procedure TvgWordPrint.SetPrinting(Value: Boolean);
begin
  Print;
end;

procedure TvgWordPrint.SetSourceDoc(Value: TFileName);
begin
  if FSourceDoc <> Value then
  begin
    CheckNotPrinting;
    FSourceDoc := Value;
  end;
end;

procedure TvgWordPrint.SetWordVisible(Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FStreamedWordVisible := Value
  else begin
    if not VarIsEmpty(FWordBasic) then
    begin
      if Value then
        FWordBasic.AppShow
      else
        FWordBasic.AppHide;
    end;
    FWordVisible := Value;
  end;
end;

procedure TvgWordPrint.CreateWordBookmarks;
  procedure CompareBookmarks(Bookmark1, Bookmark2: TWordBookmark);
  begin
    if
      (Bookmark1.SelStart <= Bookmark2.SelStart) and
      (Bookmark1.SelEnd >= Bookmark2.SelEnd) then
    begin
      if Bookmark2.Kind = bkGroup then Inc(Bookmark2.FLevel);
      if Bookmark1.Kind <> bkGroup then
        raise EInvalidNesting.Create(Format(LoadStr(SInvalidNesting), [Bookmark1.Name, Bookmark2.Name]));
      Bookmark2.FParentGroup := Bookmark1;
    end else if
      (Bookmark2.SelStart <= Bookmark1.SelStart) and
      (Bookmark2.SelEnd >= Bookmark1.SelEnd) then
    begin
      if Bookmark1.Kind = bkGroup then Inc(Bookmark1.FLevel);
      if Bookmark2.Kind <> bkGroup then
        raise EInvalidNesting.Create(Format(LoadStr(SInvalidNesting), [Bookmark1.Name, Bookmark2.Name]));
      Bookmark1.FParentGroup := Bookmark2;
    end else if
      (Bookmark1.SelStart >= Bookmark2.SelEnd) or
      (Bookmark2.SelStart >= Bookmark1.SelEnd) then
    begin
      ;
    end else
      raise EInvalidNesting.Create(Format(LoadStr(SInvalidNesting), [Bookmark1.Name, Bookmark2.Name]));
  end;
var
  I, J, Count, SeparatorPos: Integer;
  Group, Tmp, Bookmark1, Bookmark2: TWordBookmark;
  GroupName, TmpName, Text, TmpSign: string;
begin
  BeginAnalize;
  try
    { Bookmark list }
    Count := FWordBasic.CountBookmarks;
    if Count = 0 then
      raise EBookmarksNotFound.Create(LoadStr(SBookmarksNotFound));
    BeginBookmarksCreate;
    try
    { Bookmark kinds }
      for I := 1 to Count do
      begin
        BeginBookmarkCreate;
        try
          Tmp := CreateWordBookmark(Trim(FWordBasic.BookmarkName(I)));
          FBookmarks.Add(Tmp);
          DoEditGoto(Tmp.Name);
          Tmp.FSelStart := FWordBasic.GetSelStartPos;
          Tmp.FSelEnd := FWordBasic.GetSelEndPos;
          if Pos(SignatureSQL, AnsiUpperCase(Tmp.Name)) <> 0 then
          begin
            Tmp.FKind := bkSQL;
            FWordBasic.Hidden(0);
            DoCopyText(Tmp.FSQL);
          end else if Pos(SignatureField, AnsiUpperCase(Tmp.Name)) <> 0 then
          begin
            Tmp.FKind := bkField;
            DoCopyText(Text);

            TmpSign := MaskSeparator + SignatureSuppresDupes;
            SeparatorPos := Pos(TmpSign, AnsiUpperCase(Text));
            if SeparatorPos > 0 then
            begin
              Tmp.FSuppressDupes := True;
              Delete(Text, SeparatorPos, Length(TmpSign));
            end;

            SeparatorPos := Pos(MaskSeparator, Text);
            if (SeparatorPos > 0) then
            begin
              if (Length(Text) < SeparatorPos + 1) then
                raise EInvalidBookmarkText.Create(Format(LoadStr(SInvalidBookmarkText), [Tmp.Name]));
              Tmp.FMask := Copy(Text, SeparatorPos + 1, Length(Text));
              Delete(Text, SeparatorPos, Length(Text));
              Text := Trim(Text);
            end;
            if (Length(Text) < 1) then
              raise EInvalidBookmarkText.Create(Format(LoadStr(SInvalidBookmarkText), [Tmp.Name]));
            Tmp.FFieldName := Text;
          end else
            Tmp.FKind := bkGroup;
          FGroups[Tmp.FKind].Add(Tmp);
        finally
          EndBookmarkCreate;
        end;
      end;
    finally
      EndBookmarksCreate;
    end;
    if FGroups[bkGroup].Count = 0 then
      raise EGroupsNotFound.Create(LoadStr(SGroupsNotFound));
    { Bookmark groups }
    for I := 0 to FBookmarks.Count - 1 do
    begin
      Group := FBookmarks[I];
      if Group.Kind = bkGroup then
      begin
        GroupName := AnsiUpperCase(Group.Name);
        for J := 0 to FBookmarks.Count - 1 do
        begin
          Tmp := FBookmarks[J];
          if not Assigned(Tmp.Group) then
          begin
            TmpName := AnsiUpperCase(Tmp.Name);
            if Pos(GroupName, TmpName) = 1 then
            begin
              Delete(TmpName, 1, Length(GroupName));
              if Pos(SignatureField, TmpName) = 1 then
              begin
                Tmp.FGroup := Group;
                Group.FBookmarks.Add(Tmp);
              end;
            end;
          end;
        end;
      end;
    end;
    { Check SQL existance }
    for I := 0 to FGroups[bkGroup].Count - 1 do
    begin
      Group := TWordBookmark(FGroups[bkGroup][I]);
      Count := 0;
      for J := 0 to Group.Count - 1 do
      begin
        Tmp := Group.Bookmarks[J];
        if Tmp.Kind = bkSQL then Inc(Count);
        if Count > 1 then
          raise EDuplicateSQL.Create(Format(LoadStr(SDuplicateSQL), [Group.Name]));
      end;
      CreateDataSource(Group);
    end;
    { Nesting }
    for I := 0 to FBookmarks.Count - 1 do
    begin
      Bookmark1 := FBookmarks[I];
      for J := I + 1 to FBookmarks.Count - 1 do
      begin
        Bookmark2 := FBookmarks[J];
        CompareBookmarks(Bookmark1, Bookmark2);
      end;
    end;
    { Max nesting level }
    FMaxLevel := 0;
    for I := 0 to FGroups[bkGroup].Count - 1 do
    begin
      Group  := TWordBookmark(FGroups[bkGroup][I]);
      if Group.Level > MaxLevel then FMaxLevel := Group.Level;
    end;
  finally
    EndAnalize;
  end;
end;

procedure TvgWordPrint.ReadDispInvokeCache(Stream: TStream);
begin
  FDispInvokeCache.Read(Stream);
end;

procedure TvgWordPrint.WriteDispInvokeCache(Stream: TStream);
begin
  FDispInvokeCache.Write(Stream);
end;

end.

