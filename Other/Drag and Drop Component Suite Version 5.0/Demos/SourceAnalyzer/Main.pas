unit Main;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop, DropTarget,
  ActiveX,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ToolWin, ImgList, ActnList, Menus;

const
  MAX_DATA = 32768; // Max bytes to render in preview

type
  TOmnipotentDropTarget = class(TCustomDropMultiTarget)
  protected
    function DoGetData: boolean; override;
  public
    function HasValidFormats(const ADataObject: IDataObject): boolean; override;
  end;

  TDropData = record
    FormatEtc: TFormatEtc;
    Data: AnsiString;
  end;
  PDropData = ^TDropData;

  TFormMain = class(TForm)
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    EditHexView: TRichEdit;
    ListViewDataFormats: TListView;
    ActionList1: TActionList;
    ActionClear: TAction;
    ActionPaste: TAction;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionSave: TAction;
    ToolButton3: TToolButton;
    SaveDialog1: TSaveDialog;
    ButtonDirection: TToolButton;
    PopupMenuDirection: TPopupMenu;
    ActionDirTarget: TAction;
    ActionDirSource: TAction;
    otarget1: TMenuItem;
    osource1: TMenuItem;
    ActionDir: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ListViewDataFormatsDeletion(Sender: TObject;
      Item: TListItem);
    procedure ListViewDataFormatsSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionDirTargetExecute(Sender: TObject);
    procedure ActionDirSourceExecute(Sender: TObject);
    procedure ActionDirExecute(Sender: TObject);
  private
    FDataObject: IDataObject;
    FDropTarget: TCustomDropTarget;
    procedure OnDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    function DataToHexDump(const Data: AnsiString): string;
    function GetDataSize(const FormatEtc: TFormatEtc): integer;
    procedure OnDragOver(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure OnDragEnter(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure OnDragLeave(Sender: TObject);
  protected
    procedure Init;
    procedure Clear;
    function GetDropData(DropData: PDropData): AnsiString;
    property DataObject: IDataObject read FDataObject;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  DragDropFormats;

resourcestring
  sIntro = '{\rtf1\ansi\ansicpg1252\deff0\deflang1030{\fonttbl{\f0\fswiss\fcharset0 Arial;}{\f1\fnil\fcharset2 Symbol;}}'+#13+
    '\viewkind4\uc1\pard{\pntext\f1\''B7\tab}{\*\pn\pnlvlblt\pnf1\pnindent0{\pntxtb\''B7}}\fi-284\li284\f0\fs20Drag data from any application and drop it on this window.\line'+#13+
    'The data formats supported by the drop source will be displayed in the list above.\par '+#13+
    '{\pntext\f1\''B7\tab}Select a data format from the list to display its content here.\par }';

{ TOmnipotentDropTarget }

function TOmnipotentDropTarget.DoGetData: boolean;
begin
  Result := True;
end;

function TOmnipotentDropTarget.HasValidFormats(const ADataObject: IDataObject): boolean;
begin
  Result := True;
end;

{ TFormMain }

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDropTarget := TOmnipotentDropTarget.Create(Self);
  FDropTarget.DragTypes := [dtCopy, dtLink, dtMove];
  FDropTarget.OptimizedMove := True;
  FDropTarget.Target := Self;
  FDropTarget.OnEnter := Self.OnDragEnter;
  FDropTarget.OnDrop := Self.OnDrop;
  FDropTarget.OnDragOver := Self.OnDragOver;
  FDropTarget.OnLeave := Self.OnDragLeave;
  FDropTarget.OnGetDropEffect := Self.OnDragOver;
{$ifdef VER18_PLUS}
  ToolBar1.DrawingStyle := dsGradient;
{$endif}
  Init;
end;

function TFormMain.GetDataSize(const FormatEtc: TFormatEtc): integer;
var
  Medium: TStgMedium;
begin
  FillChar(Medium, SizeOf(Medium), 0);
  if (Succeeded(DataObject.GetData(FormatEtc, Medium))) then
  begin
    try
      Result := GetMediumDataSize(Medium);
    finally
      ReleaseStgMedium(Medium);
    end;
  end else
    Result := -1;
end;

procedure TFormMain.OnDragEnter(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
begin
  Clear;
  StatusBar1.SimpleText := 'Drag detected - Drop to analyze the drop source';
end;

procedure TFormMain.OnDragLeave(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Drop cancelled';
  Init;
end;

procedure TFormMain.OnDragOver(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
begin
  // Prefer the copy drop effect
  if (Effect and DROPEFFECT_COPY <> 0) then
    Effect := DROPEFFECT_COPY;
end;

function AspectsToString(Aspects: DWORD): string;
const
  AspectNames: array[0..3] of string =
    ('Content', 'Thumbnail', 'Icon', 'Print');
var
  Aspect: DWORD;
  AspectNum: integer;
begin
  AspectNum := 0;
  Aspect := $0001;
  Result := '';
  while (Aspects >= Aspect) do
  begin
    if (Aspects and Aspect <> 0) then
    begin
      if (Result <> '') then
        Result := Result+'+';
      Result := Result+AspectNames[AspectNum];
    end;
    inc(AspectNum);
    Aspect := Aspect shl 1;
  end;
end;

function MediaToString(Media: DWORD): string;
const
  MediaNames: array[0..7] of string =
    ('GlobalMem', 'File', 'IStream', 'IStorage', 'GDI', 'MetaFile', 'EnhMetaFile', 'Unknown');
var
  Medium: DWORD;
  MediumNum: integer;
begin
  Result := '';
  MediumNum := 0;
  Medium := $0001;
  while (Media >= Medium) do
  begin
    if (Media and Medium <> 0) then
    begin
      if (Result <> '') then
        Result := Result+', ';
      Result := Result+MediaNames[MediumNum];
    end;
    inc(MediumNum);
    Medium := Medium shl 1;
  end;
end;

procedure TFormMain.OnDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  GetNum, GotNum: longInt;
  FormatEnumerator: IEnumFormatEtc;
  SourceFormatEtc: TFormatEtc;
  DropData: PDropData;
  Item: TListItem;
  s: string;
  Size: integer;
  Direction: Longint;
const
 ClipNames: array[CF_TEXT..CF_MAX-1] of string =
   ('CF_TEXT', 'CF_BITMAP', 'CF_METAFILEPICT', 'CF_SYLK', 'CF_DIF', 'CF_TIFF',
   'CF_OEMTEXT', 'CF_DIB', 'CF_PALETTE', 'CF_PENDATA', 'CF_RIFF', 'CF_WAVE',
   'CF_UNICODETEXT', 'CF_ENHMETAFILE', 'CF_HDROP', 'CF_LOCALE');
begin
  StatusBar1.SimpleText := 'Data dropped';
  ListViewDataFormats.Items.BeginUpdate;
  try
    ListViewDataFormats.Items.Clear;

    // Save a reference to the data object for later use. We need it to fetch
    // data from the drop source when the user selects an item from the list.
    FDataObject := TCustomDropTarget(Sender).DataObject;

    if (ActionDirTarget.Checked) then
      Direction := DATADIR_GET
    else
      Direction := DATADIR_SET;

    // Bail out if the drop source won't allow us to enumerate the formats.
    if (FDataObject.EnumFormatEtc(Direction, FormatEnumerator) <> S_OK) or
      (FormatEnumerator.Reset <> S_OK) then
    begin
      Clear;
      exit;
    end;

    GetNum := 1; // Get one format at a time.

    // Enumerate all data formats offered by the drop source.
    while (FormatEnumerator.Next(GetNum, SourceFormatEtc, @GotNum) = S_OK) and
      (GetNum = GotNum) do
    begin
      Item := ListViewDataFormats.Items.Add;

      // Format ID
      Item.Caption := IntToStr(SourceFormatEtc.cfFormat);

      // Format name
      if (SourceFormatEtc.cfFormat < CF_MAX) then
        Item.SubItems.Add(ClipNames[SourceFormatEtc.cfFormat])
      else
        Item.SubItems.Add(GetClipboardFormatNameStr(SourceFormatEtc.cfFormat));

      // Aspect names
      Item.SubItems.Add(AspectsToString(SourceFormatEtc.dwAspect));

      // Media names
      Item.SubItems.Add(MediaToString(SourceFormatEtc.tymed));

      // Data size
      Size := GetDataSize(SourceFormatEtc);
      if (Size > 0) then
        s := Format('%.0n', [Int(Size)])
      else
        s := '-';
      Item.SubItems.Add(s);

      // Save a copy of the format descriptor in the listview
      New(DropData);
      Item.Data := DropData;
      DropData.FormatEtc := SourceFormatEtc;
    end;
  finally
    ListViewDataFormats.Items.EndUpdate;
  end;

  // Reject the drop so the drop source doesn't think we actually did something
  // usefull with it.
  // This is important when moving data or when dropping from the recycle bin;
  // If we do not reject the drop, the source will assume that it is safe to
  // delete the source data. See also "Optimized move".
  // Note: The code below has been disabled as we now handle the above scenario
  // as an optimized move (OptimizedMove has been set to True) and break our
  // part (that is, the drop target's) of the optmized move contract by not
  // actually deleting the dropped data.
  (*
  if ((Effect and not(DROPEFFECT_SCROLL)) = DROPEFFECT_MOVE) then
    Effect := DROPEFFECT_NONE
  else
    Effect := DROPEFFECT_COPY;
  *)
end;

procedure TFormMain.ActionClearExecute(Sender: TObject);
begin
  Clear;
  Init;
end;

procedure TFormMain.ActionDirExecute(Sender: TObject);
begin
  ButtonDirection.CheckMenuDropdown;
end;

procedure TFormMain.ActionDirSourceExecute(Sender: TObject);
begin
  ActionDir.ImageIndex := TAction(Sender).ImageIndex;
end;

procedure TFormMain.ActionDirTargetExecute(Sender: TObject);
begin
  ActionDir.ImageIndex := TAction(Sender).ImageIndex;
end;

procedure TFormMain.ActionPasteExecute(Sender: TObject);
begin
  FDropTarget.PasteFromClipboard;
end;

procedure TFormMain.ActionPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FDropTarget.CanPasteFromClipboard);
end;

procedure TFormMain.ActionSaveExecute(Sender: TObject);
var
  Strings: TStrings;
  i: integer;
  ClipFormat: TRawClipboardFormat;
  DropData: PDropData;
const
  sFormat =     'Name  : %s'+#13#10+
                'ID    : %d'+#13#10+
                'Medium: %s'+#13#10+
                'Aspect: %s'+#13#10+
                'Index : %d'+#13#10+
                'Size  : %s'+#13#10+
                '============================================================================'+#13;
  sDataHeader = 'Offset    Bytes                                             ASCII'+#13#10+
                '--------  ------------------------------------------------  ----------------'+#13;
  sSeparator =  '============================================================================'+#13;
begin
  if (SaveDialog1.Execute) then
  begin
    Strings := TStringList.Create;
    try
      for i := 0 to ListViewDataFormats.Items.Count-1 do
        if (ListViewDataFormats.Items[i].Data <> nil) then
        begin
          DropData := PDropData(ListViewDataFormats.Items[i].Data);
          ClipFormat := TRawClipboardFormat.CreateFormatEtc(DropData.FormatEtc);
          try
            ClipFormat.GetData(DataObject);
            Strings.Add(Format(sFormat,
              [ClipFormat.ClipboardFormatName, ClipFormat.ClipboardFormat,
              MediaToString(DropData.FormatEtc.tymed),
              AspectsToString(DropData.FormatEtc.dwAspect),
              DropData.FormatEtc.lindex,
              ListViewDataFormats.Items[i].SubItems[3]]));
            Strings.Add(sDataHeader);
            Strings.Add(DataToHexDump(GetDropData(DropData)));
            Strings.Add(sSeparator);
          finally
            ClipFormat.Free;
          end;
      end;
      Strings.SaveToFile(SaveDialog1.FileName);
    finally
      Strings.Free;
    end;
  end;
end;

procedure TFormMain.ActionSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ListViewDataFormats.Items.Count > 0);
end;

procedure TFormMain.Clear;
begin
  EditHexView.Text := '';
  ListViewDataFormats.Items.Clear;
  FDataObject := nil;
end;

function TFormMain.DataToHexDump(const Data: AnsiString): string;
var
  i: integer;
  Offset: integer;
  Hex: string;
  ASCII: string;
  LineLength: integer;
  Size: integer;
begin
  Result := '';
  LineLength := 0;
  Hex := '';
  ASCII := '';
  Offset := 0;

  Size := Length(Data);
  if (Size > MAX_DATA) then
    Size := MAX_DATA;

  for i := 0 to Size-1 do
  begin
    Hex := Hex+IntToHex(ord(Data[i+1]), 2)+' ';
    if (Data[i+1] in [' '..#$7F]) then
      ASCII := ASCII+Data[i+1]
    else
      ASCII := ASCII+'.';
    inc(LineLength);
    if (LineLength = 16) or (i = Length(Data)-1) then
    begin
      Result := Result+Format('%.8x  %-48.48s  %-16.16s'+#13+#10, [Offset, Hex, ASCII]);
      inc(Offset, LineLength);
      LineLength := 0;
      Hex := '';
      ASCII := '';
    end;
  end;
end;

procedure TFormMain.ListViewDataFormatsDeletion(Sender: TObject;
  Item: TListItem);
begin
  if (Item.Data <> nil) then
  begin
    Finalize(PDropData(Item.Data)^);
    Dispose(Item.Data);
    Item.Data := nil;
  end;
end;

procedure TFormMain.ListViewDataFormatsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  DropData: PDropData;
begin
  EditHexView.PlainText := True;
  // Force RichEdit to recreate window handle in PlainText mode.
  EditHexView.Perform(CM_RECREATEWND, 0, 0);
  if (Selected) and (Item.Data <> nil) then
  begin
    DropData := PDropData(Item.Data);
    EditHexView.Text := DataToHexDump(GetDropData(DropData));
  end else
    EditHexView.Text := '';
end;

function TFormMain.GetDropData(DropData: PDropData): AnsiString;
var
  ClipFormat: TRawClipboardFormat;
begin
  if (Length(DropData.Data) = 0) then
  begin
    // Create a temporary clipboard format object to retrieve the raw data
    // from the drop source.
    ClipFormat := TRawClipboardFormat.CreateFormatEtc(DropData.FormatEtc);
    try
      // Note: It would probably be better (more efficient & safer) if we used a
      // custom clipboard format class which only copied a limited amount of data
      // from the source data object. However, I'm lazy and this solution works
      // fine most of the time.
      ClipFormat.GetData(DataObject);
      DropData.Data := Copy(ClipFormat.AsString, 1, MAX_DATA);
    finally
      ClipFormat.Free;
    end;
  end;
  Result := DropData.Data;
end;

procedure TFormMain.Init;
var
  Stream: TStream;
begin
  EditHexView.Text := '';
  EditHexView.PlainText := False;
  Stream := TStringStream.Create(sIntro);
  try
    EditHexView.Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.

