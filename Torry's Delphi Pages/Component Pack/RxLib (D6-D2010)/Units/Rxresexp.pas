{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

'NOTE: This expert is currently broken' +
'      I tried to convert it to using the ToolsAPI but never finished'
unit RxResExp;

interface

{$I RX.INC}

{$ifdef rx_d7}
  {$define use_toolsapi}
{$endif}

{$IFNDEF RX_D3}
  ERROR! This unit is intended for Delphi 3.0 or higher only!
  { Resource expert doesn't work properly in Delphi 2.0 and in
    C++Builder 1.0 and I don't know why. }
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, ComCtrls, EditIntf,
{$IFDEF use_toolsapi}
  ToolsApi,
{$ELSE}
  ExptIntf, ToolIntf,
{$ENDIF}
  Menus, StdCtrls, rxPlacemnt
  {$IFDEF RX_D4}, ImgList {$ENDIF};

type
  TRxProjectResExpert = class;
  TResourceType = (rtpCustom, rtpCursor, rtpGroupCursor, rtpBitmap,
    rtpIcon, rtpGroupIcon, rtpRCData, rtpVersion, rtpAniCursor,
    rtpPredefined);

  TResSelection = record
    ResName: string;
    ResType: string;
  end;

{$IFDEF use_toolsapi}
  TAddInNotifier = class(TNotifierObject, IOTAIDENotifier)
{$else}
  TAddInNotifier = class(TIAddInNotifier)
{$endif}
  private
    FProjectResources: TRxProjectResExpert;
  public
    constructor Create(AProjectResources: TRxProjectResExpert);
{$IFDEF use_toolsapi}
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
{$else}
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
{$IFDEF RX_D3}
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
{$ENDIF}
{$endif}
  end;

  TProjectNotifier = class(TIModuleNotifier)
  private
    FProjectResources: TRxProjectResExpert;
  public
    constructor Create(AProjectResources: TRxProjectResExpert);
    procedure Notify(NotifyCode: TNotifyCode); override;
{$IFDEF RX_D6}   // Polaris
    procedure ComponentRenamed(const AComponent: TComponent;
      const OldName, NewName: string); override;
{$ELSE}
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
{$ENDIF}
  end;

  TResourceEntry = class(TObject)
  private
    FHandle: Pointer;
    FName: string;
    FType: string;
    FNameId: Word;
    FTypeId: Word;
    FSize: Integer;
    FEntryNode: TTreeNode;
    FResType: TResourceType;
    FChildren: TList;
    FParent: TResourceEntry;
    function GetBitmap(ResFile: TIResourceFile): TBitmap;
    function GetCursorOrIcon(ResFile: TIResourceFile; IsIcon: Boolean): HIcon;
  public
    constructor Create(AEntry: TIResourceEntry);
    destructor Destroy; override;
    function Rename(ResFile: TIResourceFile; const NewName: string): Boolean;
    function GetGraphic(ResFile: TIResourceFile): TGraphic;
    procedure GetData(ResFile: TIResourceFile; Stream: TStream);
    procedure GetIconData(ResFile: TIResourceFile; Stream: TStream);
    function GetName: string;
    function GetTypeName: string;
    function GetResourceName: PChar;
    function GetResourceType: PChar;
    function EnableEdit: Boolean;
    function EnableRenameDelete: Boolean;
  end;

{$ifdef use_toolsapi}
  TIMenuItemIntf = TMenuItem;
{$endif}

{$ifdef use_toolsapi}
  TRxProjectResExpert = class(TNotifierObject, IOTAWizard)
{$else}
  TRxProjectResExpert = class(TIExpert)
{$endif}
  private
{$ifdef use_toolsapi}
    FNotifierIdx: integer;
{$endif}
    ProjectResourcesItem: TIMenuItemIntf;
    AddInNotifier: TAddInNotifier;
    ProjectNotifier: TProjectNotifier;
    ProjectModule: TIModuleInterface;
    FResourceList: TStringList;
    FSelection: TResSelection;
    FResFileName: string;
    FProjectName: string;
    FLockCount: Integer;
    procedure RegisterNotifier;
    procedure UnregisterNotifier;
    procedure FindChildren(ResFile: TIResourceFile; Entry: TResourceEntry);
    procedure LoadProjectResInfo;
    procedure ClearProjectResInfo;
    procedure UpdateProjectResInfo;
    procedure OpenProject(const FileName: string);
    procedure CloseProject;
{$IFNDEF RX_D4}
    procedure LoadDesktop(const FileName: string);
    procedure SaveDesktop(const FileName: string);
{$ENDIF}
{$ifdef use_toolsapi}
    procedure ProjectResourcesClick(Sender: TObject);
{$else}
    procedure ProjectResourcesClick(Sender: TIMenuItemIntf);
{$endif}
    procedure CreateResourcesMenuItem;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName: string; {$ifndef use_toolsapi}override; {$endif}
    function GetAuthor: string;  {$ifndef use_toolsapi}override; {$endif}
 {$ifndef use_toolsapi}
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetStyle: TExpertStyle; override;
    function GetMenuText: string; override;
{$endif}
{$ifdef use_toolsapi}
    function GetState: TWizardState;
{$else}
    function GetState: TExpertState; override;
{$endif}
    function GetStyle: TExpertStyle; override;
    function GetIDString: string; {$ifndef use_toolsapi}override; {$endif}
    procedure Execute; {$ifndef use_toolsapi}override; {$endif}
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MarkModified;
{$ifdef use_toolsapi}
{$else}
    function GetResFile: TIResourceFile;
{$endif}
    function UniqueName(ResFile: TIResourceFile; ResType: PChar;
      var Index: Integer): string;
    procedure CheckRename(ResFile: TIResourceFile; ResType, NewName: PChar);
    function DeleteEntry(ResFile: TIResourceFile; Entry: TResourceEntry): Boolean;
    procedure CreateEntry(ResFile: TIResourceFile; ResType, ResName: PChar;
      ADataSize: Integer; AData: Pointer; SetToEntry: Boolean);
    procedure NewBinaryRes(ResFile: TIResourceFile; ResName, ResType: PChar;
      Stream: TMemoryStream);
    procedure EditBinaryRes(Entry: TResourceEntry; Stream: TMemoryStream);
    procedure NewBitmapRes(ResFile: TIResourceFile; ResName: PChar;
      Bitmap: TBitmap);
    procedure EditBitmapRes(Entry: TResourceEntry; Bitmap: TBitmap);
    procedure NewCursorIconRes(ResFile: TIResourceFile; ResName: PChar;
      IsIcon: Boolean; Stream: TStream);
    procedure EditCursorIconRes(Entry: TResourceEntry; IsIcon: Boolean;
      Stream: TStream);
  end;

  TRxResourceEditor = class(TForm)
    StatusBar: TStatusBar;
    ResTree: TTreeView;
    PopupMenu: TPopupMenu;
    NewItem: TMenuItem;
    EditItem: TMenuItem;
    RenameItem: TMenuItem;
    DeleteItem: TMenuItem;
    TreeImages: TImageList;
    N1: TMenuItem;
    NewBitmapItem: TMenuItem;
    NewIconItem: TMenuItem;
    NewCursorItem: TMenuItem;
    NewUserDataItem: TMenuItem;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    Placement: TFormStorage;
    PreviewItem: TMenuItem;
    SaveItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ResTreeExpanded(Sender: TObject; Node: TTreeNode);
    procedure ResTreeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure ResTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ResTreeEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure PopupMenuPopup(Sender: TObject);
    procedure RenameItemClick(Sender: TObject);
    procedure EditItemClick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure NewBitmapItemClick(Sender: TObject);
    procedure NewIconItemClick(Sender: TObject);
    procedure NewCursorItemClick(Sender: TObject);
    procedure NewUserDataItemClick(Sender: TObject);
    procedure ResTreeKeyPress(Sender: TObject; var Key: Char);
    procedure ResTreeDblClick(Sender: TObject);
    procedure ResTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewItemClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure SaveItemClick(Sender: TObject);
  private
    { Private declarations }
    FExpert: TRxProjectResExpert;
    function GetResourceTypeName: string;
    procedure CheckResourceType(Sender: TObject; var TypeName: string;
      var Apply: Boolean);
  public
    { Public declarations }
  end;

var
  RxResourceEditor: TRxResourceEditor = nil;

procedure RegisterResourceExpert;

implementation

uses
  Consts, rxVCLUtils, rxStrUtils, rxMaxMin, rxPictEdit;

{$R *.DFM}
{$R *.R32}
{$D-}

{$I RXRESEXP.INC}

const
  sExpertID = 'RX.ProjectResourceExpert';
  sVisible = 'Visible';

{ Library registration }

procedure RegisterResourceExpert;
begin
{$ifdef use_toolsapi}
  RegisterPackageWizard(TRxProjectResExpert.Create);
{$else}
  RegisterLibraryExpert(TRxProjectResExpert.Create);
{$endif}
end;

{ TInputBox }

type
  TApplyEvent = procedure(Sender: TObject; var Value: string;
    var Apply: Boolean) of object;

  TInputBox = class(TForm)
  private
    FPrompt: TLabel;
    FEdit: TComboBox;
    FValue: string;
    FOnApply: TApplyEvent;
    function GetPrompt: string;
    procedure SetPrompt(const Value: string);
    function GetStrings: TStrings;
    procedure SetStrings(Value: TStrings);
    procedure OkButtonClick(Sender: TObject);
  public
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
    property Caption;
    property Value: string read FValue write FValue;
    property Prompt: string read GetPrompt write SetPrompt;
    property Strings: TStrings read GetStrings write SetStrings;
    property OnApply: TApplyEvent read FOnApply write FOnApply;
  end;

constructor TInputBox.Create(AOwner: TComponent);
var
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
{$IFDEF CBUILDER}
  inherited CreateNew(AOwner, 0);
{$ELSE}
  inherited CreateNew(AOwner);
{$ENDIF}
  Canvas.Font := Self.Font;
  DialogUnits := GetAveCharSize(Canvas);
  BorderStyle := bsDialog;
  ClientWidth := MulDiv(180, DialogUnits.X, 4);
  ClientHeight := MulDiv(63, DialogUnits.Y, 8);
  Position := poScreenCenter;
  FPrompt := TLabel.Create(Self);
  with FPrompt do begin
    Parent := Self;
    AutoSize := True;
    Left := MulDiv(8, DialogUnits.X, 4);
    Top := MulDiv(8, DialogUnits.Y, 8);
  end;
  FEdit := TComboBox.Create(Self);
  with FEdit do begin
    Parent := Self;
    Left := FPrompt.Left;
    Top := MulDiv(19, DialogUnits.Y, 8);
    Width := MulDiv(164, DialogUnits.X, 4);
    MaxLength := 255;
    Style := csDropDown;
  end;
  FPrompt.FocusControl := FEdit;
  ButtonTop := MulDiv(41, DialogUnits.Y, 8);
  ButtonWidth := MulDiv(50, DialogUnits.X, 4);
  ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
  with TButton.Create(Self) do begin
    Parent := Self;
    Caption := SMsgDlgOK;
    ModalResult := mrNone;
    OnClick := OkButtonClick;
    Default := True;
    SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
      ButtonHeight);
  end;
  with TButton.Create(Self) do begin
    Parent := Self;
    Caption := SMsgDlgCancel;
    ModalResult := mrCancel;
    Cancel := True;
    SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
      ButtonHeight);
  end;
end;

procedure TInputBox.OkButtonClick(Sender: TObject);
var
  Apply: Boolean;
  Value: string;
begin
  Apply := True;
  if Assigned(FOnApply) then begin
    Value := FEdit.Text;
    FOnApply(Self, Value, Apply);
    if FEdit.Text <> Value then FEdit.Text := Value;
  end;
  if Apply then ModalResult := mrOk;
end;

function TInputBox.Execute: Boolean;
begin
  with FEdit do begin
    Text := FValue;
    SelectAll;
  end;
  Result := ShowModal = mrOk;
  if Result then FValue := FEdit.Text;
end;

function TInputBox.GetPrompt: string;
begin
  Result := FPrompt.Caption;
end;

procedure TInputBox.SetPrompt(const Value: string);
begin
  FPrompt.Caption := Value;
end;

function TInputBox.GetStrings: TStrings;
begin
  Result := FEdit.Items;
end;

procedure TInputBox.SetStrings(Value: TStrings);
begin
  if Value = nil then FEdit.Items.Clear
  else FEdit.Items.Assign(Value);
end;

{ Utility routines }

{$IFNDEF RX_D3}
const
  RT_ANICURSOR = MakeIntResource(21);
  RT_ANIICON = MakeIntResource(22);
{$ENDIF}
const
  FIRST_CUSTOM_RESTYPE = 25;

function IsValidIdent(const Ident: string): Boolean;
const
  Numeric = ['0'..'9'];
  AlphaNumeric = Numeric + ['A'..'Z', 'a'..'z', '_', '.'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) then Exit;
  for I := 1 to Length(Ident) do
    if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

function IsValidResType(const Ident: string): Boolean;
var
  Val: Longint;
begin
  Result := IsValidIdent(Ident);
  if Result then begin
    Val := StrToIntDef(Ident, FIRST_CUSTOM_RESTYPE);
    Result := (Val >= FIRST_CUSTOM_RESTYPE) and (Val <= High(Word));
  end;
end;

procedure CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  if TComponent(Reference) = nil then begin
    TComponent(Reference) := TComponent(InstanceClass.NewInstance);
    try
      TComponent(Reference).Create(Application);
    except
      TComponent(Reference).Free;
      TComponent(Reference) := nil;
      raise;
    end;
  end;
end;

function PadUp(Value: Longint): Longint;
begin
  Result := Value + (Value mod 4);
end;

function StrText(P: PChar): string;
begin
  if HiWord(Longint(P)) = 0 then
    Result := IntToStr(LoWord(Longint(P)))
  else Result := StrPas(P);
end;

function ResIdent(const Name: string): PChar;
var
  Id: Word;
  Code: Integer;
begin
  Val(Name, Id, Code);
  if Code = 0 then Result := MakeIntResource(Id)
  else Result := PChar(AnsiUpperCase(Name));
end;

function CheckResType(ResType: Integer): TResourceType;
begin
  case ResType of
    Integer(RT_CURSOR): Result := rtpCursor;
    Integer(RT_BITMAP): Result := rtpBitmap;
    Integer(RT_ICON): Result := rtpIcon;
    Integer(RT_RCDATA): Result := rtpRCData;
    Integer(RT_GROUP_CURSOR): Result := rtpGroupCursor;
    Integer(RT_GROUP_ICON): Result := rtpGroupIcon;
    Integer(RT_VERSION): Result := rtpVersion;
    Integer(RT_ANICURSOR): Result := rtpAniCursor;
    else Result := rtpCustom; { user-defined resource type }
  end;
  if (Result = rtpCustom) and (ResType > 0) and
    (ResType < FIRST_CUSTOM_RESTYPE) then
    Result := rtpPredefined;
end;

function ResourceTypeName(ResType: Integer): string;
begin
  case ResType of
    Integer(RT_CURSOR): Result := 'CURSOR';
    Integer(RT_BITMAP): Result := 'BITMAP';
    Integer(RT_ICON): Result := 'ICON';
    Integer(RT_MENU): Result := 'MENU';
    Integer(RT_DIALOG): Result := 'DIALOG';
    Integer(RT_STRING): Result := 'STRINGS';
    Integer(RT_FONTDIR): Result := 'FONTDIR';
    Integer(RT_FONT): Result := 'FONT';
    Integer(RT_ACCELERATOR): Result := 'ACCELERATOR';
    Integer(RT_RCDATA): Result := 'RCDATA';
    Integer(RT_MESSAGETABLE): Result := 'MESSAGE TABLE';
    Integer(RT_GROUP_CURSOR): Result := 'CURSOR';
    Integer(RT_GROUP_ICON): Result := 'ICON';
    Integer(RT_VERSION): Result := 'VERSIONINFO';
    Integer(RT_DLGINCLUDE): Result := 'DLGINCLUDE';
    Integer(RT_PLUGPLAY): Result := 'PLUG-AND-PLAY';
    Integer(RT_VXD): Result := 'VXD';
    Integer(RT_ANICURSOR): Result := 'ANICURSOR';
    Integer(RT_ANIICON): Result := 'ANIICON';
    else Result := IntToStr(ResType);
  end;
end;

function ResTypeName(ResType: PChar): string;
begin
  if HiWord(Longint(ResType)) = 0 then
    Result := ResourceTypeName(LoWord(Longint(ResType)))
  else Result := StrPas(ResType);
end;

function FindNode(TreeView: TCustomTreeView; Node: TTreeNode;
  const ResName, ResType: string): TTreeNode;

  function SearchNodes(Node: TTreeNode): TTreeNode;
  var
    ChildNode: TTreeNode;
    Entry: TResourceEntry;
  begin
    Result := nil;
    if Node = nil then Exit;
    Entry := TResourceEntry(Node.Data);
    if ((Entry <> nil) and (Entry.GetName = ResName) and
      (Entry.GetTypeName = ResType)) or ((Entry = nil) and (ResName = '') and
      (Node.Text = ResType)) then
      Result := Node
    else
    begin
      ChildNode := Node.GetFirstChild;
      while ChildNode <> nil do begin
        Result := SearchNodes(ChildNode);
        if Result <> nil then Break
        else ChildNode := Node.GetNextChild(ChildNode);
      end;
    end;
  end;

begin
  if Node = nil then Node := TTreeView(TreeView).Items.GetFirstNode;
  Result := SearchNodes(Node);
end;

const
  ResImages: array[TResourceType] of Integer = (2, 4, 4, 5, 3, 3, 2, 8, 4, 2);
{$ifndef use_toolsapi}
  AllMenuFlags = [mfInvalid, mfEnabled, mfVisible, mfChecked, mfBreak,
    mfBarBreak, mfRadioItem];
{$endif}

const
  MOVEABLE    = $0010;
  PURE        = $0020;
  PRELOAD     = $0040;
  DISCARDABLE = $1000;

const
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;

type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconDirectory = ^TIconDirectory;
  TIconDirectory = packed record
    case Integer of
      rc3_Cursor:
        (cWidth: Word;
        cHeight: Word);
      rc3_Icon:
        (Width: Byte;
        Height: Byte;
        Colors: Byte;
        Reserved: Byte;
        Planes: Word;
        BitCount: Word;
        BytesInRes: Longint;
        NameOrdinal: Word);
  end;

  PCursorHeader = ^TCursorHeader;
  TCursorHeader = packed record
    xHotspot: Word;
    yHotspot: Word;
  end;

  PDirectory = ^TDirectory;
  TDirectory = array[0..64] of TIconDirectory;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word; { xHotspot }
    Reserved2: Word; { yHotspot }
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

  PIconList = ^TIconList;
  TIconList = array[0..64] of TIconRec;

procedure InvalidIcon; near;
begin
  raise EInvalidGraphic.Create(ResStr(SInvalidIcon));
end;

{ TIconData }

type
  TIconData = class
  private
    FHeader: TCursorOrIcon;
    FList: Pointer;
    FNames: PWordArray;
    FData: TList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCount: Integer;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function BuildResourceGroup(var Size: Integer): Pointer;
    function BuildResourceItem(Index: Integer; var Size: Integer): Pointer;
    procedure LoadResourceGroup(Data: Pointer; Size: Integer);
    procedure LoadResourceItem(Index: Integer; Data: Pointer; Size: Integer);
    procedure SetNameOrdinal(Index: Integer; Name: Word);
  end;

constructor TIconData.Create;
begin
  inherited Create;
  FData := TList.Create;
end;

destructor TIconData.Destroy;
begin
  Clear;
  FData.Free;
  inherited Destroy;
end;

procedure TIconData.Clear;
begin
  if FNames <> nil then FreeMem(FNames);
  FNames := nil;
  if FList <> nil then FreeMem(FList);
  FList := nil;
  while FData.Count > 0 do begin
    if Pointer(FData[0]) <> nil then FreeMem(Pointer(FData[0]));
    FData.Delete(0);
  end;
  FillChar(FHeader, SizeOf(FHeader), 0);
end;

function TIconData.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TIconData.BuildResourceGroup(var Size: Integer): Pointer;
var
  P: PDirectory;
  List: PIconList;
  I: Integer;
  BI: PBitmapInfoHeader;
begin
  Size := SizeOf(FHeader) + SizeOf(TIconDirectory) * FHeader.Count;
  Result := AllocMem(Size);
  try
    Move(FHeader, Result^, SizeOf(FHeader));
    P := PDirectory(PChar(Result) + SizeOf(FHeader));
    List := PIconList(FList);
    for I := 0 to FHeader.Count - 1 do begin
      BI := PBitmapInfoHeader(Pointer(FData[I]));
      with P^[I] do begin
        if FHeader.wType = rc3_Cursor then begin
          cWidth := List^[I].Width;
          cHeight := List^[I].Height * 2;
        end
        else begin
          Width := List^[I].Width;
          Height := List^[I].Height;
          Colors := List^[I].Colors;
          Reserved := 0;
        end;
        Planes := BI^.biPlanes;
        BitCount := BI^.biBitCount;
        BytesInRes := List^[I].DIBSize;
        if FHeader.wType = rc3_Cursor then
          Inc(BytesInRes, SizeOf(TCursorHeader));
        NameOrdinal := 0;
        if FNames <> nil then NameOrdinal := FNames^[I];
      end;
    end;
  except
    FreeMem(Result);
    raise;
  end;
end;

function TIconData.BuildResourceItem(Index: Integer;
  var Size: Integer): Pointer;
var
  Icon: PIconRec;
  P: Pointer;
begin
  Icon := @(PIconList(FList)^[Index]);
  Size := Icon^.DIBSize;
  if FHeader.wType = rc3_Cursor then Inc(Size, SizeOf(TCursorHeader));
  Result := AllocMem(Size);
  try
    P := Result;
    if FHeader.wType = rc3_Cursor then begin
      with PCursorHeader(Result)^ do begin
        xHotspot := Icon^.Reserved1;
        yHotspot := Icon^.Reserved2;
      end;
      Inc(PChar(P), SizeOf(TCursorHeader));
    end;
    Move(Pointer(FData[Index])^, P^, Icon^.DIBSize);
  except
    FreeMem(Result);
    raise;
  end;
end;

procedure TIconData.SetNameOrdinal(Index: Integer; Name: Word);
begin
  if (FNames <> nil) and (Index >= 0) and (Index < FData.Count) then
    FNames^[Index] := Name;
end;

procedure TIconData.LoadResourceGroup(Data: Pointer; Size: Integer);
var
  P: PDirectory;
  List: PIconList;
  I: Integer;
begin
  FHeader.Count := (Size - SizeOf(FHeader)) div SizeOf(TIconDirectory);
  Move(Data^, FHeader, SizeOf(FHeader));
  if FList <> nil then FreeMem(FList);
  FList := AllocMem(SizeOf(TIconRec) * FHeader.Count);
  while FData.Count > 0 do begin
    if Pointer(FData[0]) <> nil then FreeMem(Pointer(FData[0]));
    FData.Delete(0);
  end;
  P := PDirectory(PChar(Data) + SizeOf(FHeader));
  List := PIconList(FList);
  if FNames <> nil then FreeMem(FNames);
  FNames := AllocMem(FHeader.Count * SizeOf(Word));
  for I := 0 to FHeader.Count - 1 do begin
    with List^[I] do begin
      if FHeader.wType = rc3_Cursor then begin
        Width := P^[I].cWidth;
        Height := P^[I].cHeight div 2;
      end
      else begin
        Width := P^[I].Width;
        Height := P^[I].Height;
        Colors := P^[I].Colors;
      end;
      DIBSize := P^[I].BytesInRes;
      if FHeader.wType = rc3_Cursor then Dec(DIBSize, SizeOf(TCursorHeader));
      Reserved1 := 0;
      Reserved2 := 0;
    end;
    FData.Add(nil);
    SetNameOrdinal(I, P^[I].NameOrdinal);
  end;
end;

procedure TIconData.LoadResourceItem(Index: Integer; Data: Pointer;
  Size: Integer);
var
  P: Pointer;
  Rec: PIconRec;
  BI: PBitmapInfoHeader;
begin
  if (Index < 0) or (Index >= FData.Count) then Exit;
  Rec := @(PIconList(FList)^[Index]);
  P := Data;
  if FHeader.wType = rc3_Cursor then begin
    with Rec^ do begin
      Reserved1 := PCursorHeader(Data).xHotspot;
      Reserved2 := PCursorHeader(Data).yHotspot;
    end;
    Inc(PChar(P), SizeOf(TCursorHeader));
    Dec(Size, SizeOf(TCursorHeader));
  end;
  FData[Index] := AllocMem(Size);
  Move(P^, Pointer(FData[Index])^, Min(Rec^.DIBSize, Size));
  BI := PBitmapInfoHeader(Pointer(FData[Index]));
  case BI^.biBitCount of
    1, 4, 8: Rec^.Colors := (1 shl BI^.biBitCount) * BI^.biPlanes;
    else Rec^.Colors := BI^.biBitCount * BI^.biPlanes;
  end;
end;

procedure TIconData.SaveToStream(Stream: TStream);
var
  I, J: Integer;
  Data: Pointer;
begin
  FHeader.Count := FData.Count;
  Stream.WriteBuffer(FHeader, SizeOf(FHeader));
  for I := 0 to FHeader.Count - 1 do begin
    PIconList(FList)^[I].DIBOffset := SizeOf(FHeader) + (SizeOf(TIconRec) *
      FHeader.Count);
    for J := 0 to I - 1 do
      Inc(PIconList(FList)^[I].DIBOffset, PIconList(FList)^[I - 1].DIBSize);
  end;
  Stream.WriteBuffer(FList^, SizeOf(TIconRec) * FHeader.Count);
  for I := 0 to FHeader.Count - 1 do begin
    Data := FData[I];
    Stream.WriteBuffer(Data^, PIconList(FList)^[I].DIBSize);
  end;
end;

procedure TIconData.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Data: Pointer;
begin
  Clear;
  Stream.ReadBuffer(FHeader, SizeOf(FHeader));
  if (not (FHeader.wType in [rc3_Icon, rc3_Cursor])) or
    (FHeader.Count < 1) then InvalidIcon;
  FList := AllocMem(SizeOf(TIconRec) * FHeader.Count);
  try
    Stream.ReadBuffer(FList^, SizeOf(TIconRec) * FHeader.Count);
    for I := 0 to FHeader.Count - 1 do begin
      Stream.Seek(PIconList(FList)^[I].DIBOffset, 0);
      Data := AllocMem(PIconList(FList)^[I].DIBSize);
      try
        FData.Add(TObject(Data));
      except
        FreeMem(Data);
        raise;
      end;
      Stream.ReadBuffer(Data^, PIconList(FList)^[I].DIBSize);
    end;
    FNames := AllocMem(FData.Count * SizeOf(Word));
    FillChar(FNames^, FData.Count * SizeOf(Word), 0);
  except
    Clear;
    raise;
  end;
end;

{ TAddInNotifier }

procedure EnableMenuItem(Expert: TRxProjectResExpert;
  AEnable: Boolean);
var
  pri: TIMenuItemIntf;
begin
  pri := Expert.ProjectResourcesItem;
  if (Expert.FResFileName <> '') and AEnable then
    pri.Enabled := true // SetFlags(AllMenuFlags, GetFlags + [mfEnabled])
  else
    pri.Enabled := false; // SetFlags(AllMenuFlags, GetFlags - [mfEnabled]);
end;

constructor TAddInNotifier.Create(AProjectResources: TRxProjectResExpert);
begin
  inherited Create;
  FProjectResources := AProjectResources;
end;

{$IFDEF use_toolsapi}
procedure TAddInNotifier.FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
begin
  if FProjectResources = nil then Exit;
  case NotifyCode of
    ofnFileOpened:
      begin
        FProjectResources.OpenProject(FileName);
        EnableMenuItem(FProjectResources, True);
      end;
  end;
end;

procedure TAddInNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  // nothing to do here, but must be implemented
end;

procedure TAddInNotifier.AfterCompile(Succeeded: Boolean);
begin
  // nothing to do here, but must be implemented
end;

{$else}

procedure TAddInNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if FProjectResources = nil then Exit;
  case NotifyCode of
    fnProjectOpened:
      begin
        FProjectResources.OpenProject(FileName);
        EnableMenuItem(FProjectResources, True);
      end;
{$IFNDEF RX_D4}
    fnProjectDesktopLoad:
      FProjectResources.LoadDesktop(FileName);
    fnProjectDesktopSave:
      FProjectResources.SaveDesktop(FileName);
{$ENDIF}
  end;
end;

{$IFDEF RX_D3}
procedure TAddInNotifier.EventNotification(NotifyCode: TEventNotification;
  var Cancel: Boolean);
begin
  { Nothing to do here but needs to be overridden anyway }
end;
{$ENDIF}
{$endif}

{ TProjectNotifier }

constructor TProjectNotifier.Create(AProjectResources: TRxProjectResExpert);
begin
  inherited Create;
  FProjectResources := AProjectResources;
end;

procedure TProjectNotifier.Notify(NotifyCode: TNotifyCode);
begin
  if FProjectResources = nil then Exit;
  case NotifyCode of
    ncModuleDeleted:
      begin
        if RxResourceEditor <> nil then RxResourceEditor.Close;
        EnableMenuItem(FProjectResources, False);
        FProjectResources.CloseProject;
      end;
    ncModuleRenamed, ncProjResModified:
      begin
        FProjectResources.UpdateProjectResInfo;
        EnableMenuItem(FProjectResources, True);
      end;
  end;
end;

{$IFDEF RX_D6}   // Polaris
procedure TProjectNotifier.ComponentRenamed(const AComponent: TComponent;
  const OldName, NewName: string);
{$ELSE}
procedure TProjectNotifier.ComponentRenamed(ComponentHandle: Pointer;
  const OldName, NewName: string);
{$ENDIF}
begin
  { Nothing to do here but needs to be overridden anyway }
end;

{ TResourceEntry }

constructor TResourceEntry.Create(AEntry: TIResourceEntry);
var
  P: PChar;
begin
  inherited Create;
  FChildren := TList.Create;
  FHandle := AEntry.GetEntryHandle;
  P := AEntry.GetResourceType;
  if HiWord(Longint(P)) = 0 then begin
    FResType := CheckResType(LoWord(Longint(P)));
    FTypeId := LoWord(Longint(P));
  end;
  FType := ResTypeName(P);
  P := AEntry.GetResourceName;
  if HiWord(Longint(P)) = 0 then
    FNameId := LoWord(Longint(P));
  FName := StrText(P);
  FSize := AEntry.GetDataSize;
end;

destructor TResourceEntry.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TResourceEntry.GetResourceName: PChar;
begin
  if FNameId > 0 then Result := MakeIntResource(FNameId)
  else Result := PChar(FName);
end;

function TResourceEntry.GetResourceType: PChar;
begin
  if FTypeId > 0 then Result := MakeIntResource(FTypeId)
  else Result := PChar(FType);
end;

function TResourceEntry.GetName: string;
begin
  Result := FName;
end;

function TResourceEntry.GetTypeName: string;
begin
  Result := FType;
end;

function TResourceEntry.EnableEdit: Boolean;
begin
  Result := FResType in [rtpGroupCursor, rtpBitmap, rtpGroupIcon, rtpRCData,
    rtpAniCursor, rtpCustom];
end;

function TResourceEntry.EnableRenameDelete: Boolean;
begin
  Result := FResType in [rtpCustom, rtpGroupCursor, rtpBitmap, rtpGroupIcon,
    rtpRCData, rtpAniCursor, rtpPredefined];
  if (FResType = rtpGroupIcon) then
    Result := CompareText(GetName, 'MAINICON') <> 0;
end;

function TResourceEntry.GetCursorOrIcon(ResFile: TIResourceFile;
  IsIcon: Boolean): HIcon;
var
  Entry, ChildEntry: TIResourceEntry;
  I: Integer;
begin
  Result := 0;
  if not (FResType in [rtpGroupIcon, rtpGroupCursor]) then Exit;
  Entry := ResFile.FindEntry(GetResourceType, GetResourceName);
  try
    I := LookupIconIdFromDirectory(Entry.GetData, IsIcon);
    if I > 0 then begin
      if IsIcon then
        ChildEntry := ResFile.FindEntry(RT_ICON, PChar(I))
      else
        ChildEntry := ResFile.FindEntry(RT_CURSOR, PChar(I));
      if ChildEntry <> nil then
      try
        with ChildEntry do
          Result := CreateIconFromResourceEx(GetData, GetDataSize,
            IsIcon, $30000, 0, 0, $80);
      finally
        ChildEntry.Free;
      end;
    end;
  finally
    Entry.Free;
  end;
end;

procedure TResourceEntry.GetIconData(ResFile: TIResourceFile; Stream: TStream);
var
  Data: TIconData;
  Entry: TIResourceEntry;
  I: Integer;
  P: PChar;
begin
  if not (FResType in [rtpGroupIcon, rtpGroupCursor]) then Exit;
  Data := TIconData.Create;
  try
    Entry := ResFile.FindEntry(GetResourceType, GetResourceName);
    try
      Data.LoadResourceGroup(Entry.GetData, Entry.GetDataSize);
    finally
      Entry.Free;
    end;
    for I := 0 to Data.FHeader.Count - 1 do begin
      P := MakeIntResource(Data.FNames^[I]);
      if FResType = rtpGroupIcon then
        Entry := ResFile.FindEntry(RT_ICON, P)
      else {rtpGroupCursor}
        Entry := ResFile.FindEntry(RT_CURSOR, P);
      try
        Data.LoadResourceItem(I, Entry.GetData, Entry.GetDataSize);
      finally
        Entry.Free;
      end;
    end;
    Data.SaveToStream(Stream);
  finally
    Data.Free;
  end;
end;

function TResourceEntry.GetBitmap(ResFile: TIResourceFile): TBitmap;

  function GetDInColors(BitCount: Word): Integer;
  begin
    case BitCount of
      1, 4, 8: Result := 1 shl BitCount;
      else Result := 0;
    end;
  end;

var
  Header: PBitmapFileHeader;
  BI: PBitmapInfoHeader;
  BC: PBitmapCoreHeader;
  Entry: TIResourceEntry;
  Mem: TMemoryStream;
  ClrUsed: Integer;
begin
  Result := nil;
  if FResType <> rtpBitmap then Exit;
  Mem := TMemoryStream.Create;
  try
    Entry := ResFile.FindEntry(GetResourceType, GetResourceName);
    try
      Mem.SetSize(Entry.GetDataSize + SizeOf(TBitmapFileHeader));
      Move(Entry.GetData^, Pointer(PChar(Mem.Memory) +
        SizeOf(TBitmapFileHeader))^, Mem.Size);
      Header := PBitmapFileHeader(Mem.Memory);
      BI := PBitmapInfoHeader(PChar(Mem.Memory) + SizeOf(TBitmapFileHeader));
      { fill header }
      with Header^ do begin
        if BI^.biSize = SizeOf(TBitmapInfoHeader) then begin
          ClrUsed := BI^.biClrUsed;
          if ClrUsed = 0 then ClrUsed := GetDInColors(BI^.biBitCount);
          bfOffBits :=  ClrUsed * SizeOf(TRGBQuad) +
            SizeOf(TBitmapInfoHeader) + SizeOf(TBitmapFileHeader);
        end
        else begin
          BC := PBitmapCoreHeader(PChar(Mem.Memory) +
            SizeOf(TBitmapFileHeader));
          ClrUsed := GetDInColors(BC^.bcBitCount);
          bfOffBits :=  ClrUsed * SizeOf(TRGBTriple) +
            SizeOf(TBitmapCoreHeader) + SizeOf(TBitmapFileHeader);
        end;
        bfSize := bfOffBits + BI^.biSizeImage;
        bfType := $4D42; { BM }
      end;
    finally
      Entry.Free;
    end;
    Result := TBitmap.Create;
    try
      Result.LoadFromStream(Mem);
    except
      Result.Free;
      raise;
    end;
  finally
    Mem.Free;
  end;
end;

procedure TResourceEntry.GetData(ResFile: TIResourceFile; Stream: TStream);
var
  Entry: TIResourceEntry;
begin
  Entry := ResFile.FindEntry(GetResourceType, GetResourceName);
  try
    Stream.WriteBuffer(Entry.GetData^, Entry.GetDataSize);
  finally
    Entry.Free;
  end;
end;

function TResourceEntry.GetGraphic(ResFile: TIResourceFile): TGraphic;
begin
  Result := nil;
  case FResType of
    rtpBitmap: Result := GetBitmap(ResFile);
    rtpGroupIcon:
      begin
        Result := TIcon.Create;
        try
          TIcon(Result).Handle := GetCursorOrIcon(ResFile, True);
        except
          Result.Free;
          raise;
        end;
      end;
  end;
end;

function TResourceEntry.Rename(ResFile: TIResourceFile;
  const NewName: string): Boolean;
var
  P: PChar;
  AName: string;
  Id: Word;
  Code: Integer;
  Entry: TIResourceEntry;
begin
  Result := False;
  Entry := ResFile.FindEntry(GetResourceType, GetResourceName);
  try
    Val(NewName, Id, Code);
    if Code = 0 then P := MakeIntResource(Id)
    else begin
      if not IsValidIdent(NewName) then
        raise Exception.Create(Format(sInvalidName, [NewName]));
      AName := AnsiUpperCase(NewName);
      P := PChar(AName);
    end;
    Result := Entry.Change(Entry.GetResourceType, P);
    if Result then begin
      P := Entry.GetResourceName;
      if HiWord(Longint(P)) = 0 then FNameId := LoWord(Longint(P));
      FName := StrText(P);
    end;
  finally
    Entry.Free;
  end;
end;

{ TRxProjectResExpert }

{$ifdef use_toolsapi}

procedure TRxProjectResExpert.CreateResourcesMenuItem;
var
  MainMenu: TMainMenu;
  ViewMenu: TMenuItem;
  NTAServices: INTAServices;
begin
  if not Supports(BorlandIDEServices, INTAServices, NTAServices) then
    exit;
  MainMenu := NTAServices.GetMainMenu;
  if not Assigned(MainMenu) then
    exit;
  ViewMenu := MainMenu.Owner.FindComponent('ViewsMenu') as TMenuItem;
  if not Assigned(ViewMenu) then
    exit;
  ProjectResourcesItem := TMenuItem.Create(MainMenu.Owner);
  ProjectResourcesItem.Name := 'ViewPrjResourceItem';
  ProjectResourcesItem.Caption := sMenuItemCaption;
  ProjectResourcesItem.OnClick := ProjectResourcesClick;
  ViewMenu.Insert(1, ProjectResourcesItem);
end;

procedure TRxProjectResExpert.RegisterNotifier;
var
  OTAServices: IOTAServices;
begin
  if not Supports(BorlandIDEServices, IOTAServices, OTAServices) then
    exit;
  FNotifierIdx := OTAServices.AddNotifier(AddInNotifier);
end;

procedure TRxProjectResExpert.UnRegisterNotifier;
var
  OTAServices: IOTAServices;
begin
  if FNotifierIdx <> 0 then begin
    if not Supports(BorlandIDEServices, IOTAServices, OTAServices) then
      exit;
    OTAServices.RemoveNotifier(FNotifierIdx);
    FNotifierIdx := 0;
  end;
end;

{$else}

procedure TRxProjectResExpert.CreateResourcesMenuItem;
var
  MainMenu: TIMainMenuIntf;
  ProjSrcMenu: TIMenuItemIntf;
  ViewMenu: TIMenuItemIntf;
  MenuItems: TIMenuItemIntf;
begin
  inherited Create;
  FResourceList := TStringList.Create;
  if not Assigned(ToolServices) then
    exit;
  MainMenu := ToolServices.GetMainMenu;
  if MainMenu <> nil then
  try
    MenuItems := MainMenu.GetMenuItems;
    if MenuItems <> nil then
    try
      ProjSrcMenu := MainMenu.FindMenuItem('ViewPrjSourceItem');
      if ProjSrcMenu <> nil then
      try
        ViewMenu := ProjSrcMenu.GetParent;
        if ViewMenu <> nil then
        try
          if (MainMenu.FindMenuItem('ViewPrjResourceItem')=nil) then // Polaris
          ProjectResourcesItem := ViewMenu.InsertItem(
            ProjSrcMenu.GetIndex, GetMenuText, 'ViewPrjResourceItem',
            '', 0, 0, 0, [mfVisible], ProjectResourcesClick);
        finally
          ViewMenu.Free;
        end;
      finally
        ProjSrcMenu.Free;
      end;
    finally
      MenuItems.Free;
    end;
  finally
    MainMenu.Free;
  end;
end;

procedure TRxProjectResExpert.RegisterNotifier;
begin
  if Assigned(ToolServices) then begin
    AddInNotifier := TAddInNotifier.Create(Self);
{$IFDEF RX_D4}
    ToolServices.AddNotifierEx(AddInNotifier);
{$ELSE}
    ToolServices.AddNotifier(AddInNotifier);
{$ENDIF}
  end;
end;

procedure TRxProjectResExpert.UnRegisterNotifier;
begin
  if Assigned(ToolServices) and Assigned(AddInNotifier) then
    ToolServices.RemoveNotifier(AddInNotifier);
end;

{$endif}

constructor TRxProjectResExpert.Create;
begin
  inherited Create;
  FResourceList := TStringList.Create;
  CreateResourcesMenuItem;
  RegisterNotifier;
end;

destructor TRxProjectResExpert.Destroy;
begin
  try
    if RxResourceEditor <> nil then RxResourceEditor.Free;
    UnregisterNotifier;
    CloseProject;
    ProjectResourcesItem.Free;
    AddInNotifier.Free;
    FResourceList.Free;
  except
    on e: exception do
      ;
    // for whatever reason there can be exceptions here, ignore them
  end;
  inherited Destroy;
end;

function TRxProjectResExpert.GetName: string;
begin
  Result := sExpertName;
end;

function TRxProjectResExpert.GetAuthor: string;
begin
  Result := '';
end;

{$ifndef use_toolsapi}
function TRxProjectResExpert.GetComment: string;
begin
  Result := '';
end;

function TRxProjectResExpert.GetPage: string;
begin
  Result := '';
end;

function TRxProjectResExpert.GetGlyph: HICON;
begin
  Result := 0;
end;

function TRxProjectResExpert.GetMenuText: string;
begin
  Result := sMenuItemCaption;
end;
{$endif}

{$ifdef use_toolsapi}
function TRxProjectResExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;
{$else}
function TRxProjectResExpert.GetState: TExpertState;
begin
  Result := [esEnabled];
end;

function TRxProjectResExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn;
end;
{$endif}

function TRxProjectResExpert.GetIDString: string;
begin
  Result := sExpertID;
end;

procedure TRxProjectResExpert.Execute;
begin
end;

procedure TRxProjectResExpert.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TRxProjectResExpert.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then UpdateProjectResInfo;
end;

{$ifdef use_toolsapi}
function GetProjectResource(Project: IOTAProject): IOTAProjectResource;
var
  i: Integer;
  Editor: IOTAEditor;
begin
  Result := nil;
  for i:= 0 to (Project.GetModuleFileCount - 1) do
  begin
    Editor := Project.GetModuleFileEditor(i);
    if Supports(Editor, IOTAProjectResource, Result) then
      Break;
  end;
end;
{$else}
function TRxProjectResExpert.GetResFile: TIResourceFile;
begin
  Result := nil;
  try
    if Assigned(ProjectModule) and ProjectModule.IsProjectModule then
      Result := ProjectModule.GetProjectResource;
  except
    Result := nil;
  end;
end;
{$endif}

procedure TRxProjectResExpert.FindChildren(ResFile: TIResourceFile;
  Entry: TResourceEntry);
var
  I, Idx: Integer;
  Header: PCursorOrIcon;
  Directory: PDirectory;
  Data: Pointer;
  Child: TResourceEntry;
  ResEntry: TIResourceEntry;
begin
  if Entry = nil then Exit;
  if Entry.FResType in [rtpGroupCursor, rtpGroupIcon] then begin
    ResEntry := ResFile.GetEntryFromHandle(Entry.FHandle);
    if ResEntry <> nil then
    try
      Data := ResEntry.GetData;
      if Data <> nil then begin
        Header := PCursorOrIcon(Data);
        Directory := PDirectory(PChar(Data) + SizeOf(TCursorOrIcon));
        for I := 0 to Header^.Count - 1 do begin
          for Idx := 0 to FResourceList.Count - 1 do begin
            Child := TResourceEntry(FResourceList.Objects[Idx]);
            if (Child <> nil) and (Child.FParent = nil) and
              (((Entry.FResType = rtpGroupIcon) and (Child.FResType = rtpIcon)) or
              ((Entry.FResType = rtpGroupCursor) and (Child.FResType = rtpCursor)))
              and (Child.GetName = IntToStr(Directory^[I].NameOrdinal)) then
            begin
              Entry.FChildren.Add(Child);
              Inc(Entry.FSize, Child.FSize);
              Child.FParent := Entry;
            end;
          end;
        end;
      end;
    finally
      ResEntry.Free;
    end;
  end;
end;

procedure TRxProjectResExpert.LoadProjectResInfo; //!!!!!
var
  I, Cnt: Integer;
  RootNode, TypeNode: TTreeNode;
  Entry: TResourceEntry;
  ResEntry: TIResourceEntry;
  TypeList: TStringList;
  ResourceFile: TIResourceFile;
{$IFDEF RX_V110}
  EditInt: TIEditorInterface;
  IsNewProject: Boolean;
{$ENDIF}
begin
  Cnt := -1;
  try
  ResourceFile := GetResFile;
  except
    ResourceFile := nil;
  end;
  try
    if ResourceFile <> nil then
      with ResourceFile do begin
        FResFileName := FileName;
{$IFDEF RX_V110}
        EditInt := ProjectModule.GetEditorInterface;
        try
          IsNewProject := not FileExists(EditInt.FileName);
        finally
          EditInt.Free;
        end;
        if IsNewProject or FileExists(FResFileName) then begin
          try
            Cnt := GetEntryCount;
            if not FileExists(FResFileName) and (Cnt = 0) then begin
              Cnt := -1;
              FResFileName := '';
            end;
          except
            Cnt := -1;
            FResFileName := '';
          end;
          { Access violation error is occured when specified }
          { resource file doesn't exist }
        end
        else begin
          Cnt := -1;
          FResFileName := '';
        end;
{$ELSE}
        Cnt := GetEntryCount;
{$ENDIF}
        for I := 0 to Cnt - 1 do begin
          ResEntry := GetEntry(I);
          if ResEntry <> nil then begin
            try
              Entry := TResourceEntry.Create(ResEntry);
            finally
              ResEntry.Free;
            end;
            FResourceList.AddObject(Entry.GetName, Entry);
          end;
        end;
        for I := 0 to FResourceList.Count - 1 do begin
          Entry := TResourceEntry(FResourceList.Objects[I]);
          FindChildren(ResourceFile, Entry);
        end;
      end;
    if (RxResourceEditor <> nil) and (ResourceFile <> nil) and (Cnt >= 0) then
    begin
      with RxResourceEditor do begin
        StatusBar.Panels[0].Text := FResFileName;
        ResTree.Items.BeginUpdate;
        try
          TypeList := TStringList.Create;
          try
            TypeList.Sorted := True;
            TypeList.Duplicates := dupIgnore;
            RootNode := ResTree.Items.Add(nil, ExtractFileName(FResFileName));
            RootNode.ImageIndex := 9; { Delphi Project }
            RootNode.SelectedIndex := RootNode.ImageIndex;
            for I := 0 to FResourceList.Count - 1 do begin
              Entry := TResourceEntry(FResourceList.Objects[I]);
              if (Entry = nil) or (Entry.FParent <> nil) then
                Continue; { ignore cursors and icons, use groups }
              Cnt := TypeList.IndexOf(Entry.GetTypeName);
              if Cnt < 0 then begin
                TypeNode := ResTree.Items.AddChildObject(RootNode,
                  Entry.GetTypeName, nil);
                TypeNode.ImageIndex := 0; { Collapsed Folder }
                TypeNode.SelectedIndex := TypeNode.ImageIndex;
                TypeList.AddObject(Entry.GetTypeName, TypeNode);
              end
              else
                TypeNode := TTreeNode(TypeList.Objects[Cnt]);
              Entry.FEntryNode := ResTree.Items.AddChildObject(TypeNode,
                Entry.GetName, Entry);
              Entry.FEntryNode.ImageIndex := ResImages[Entry.FResType];
              Entry.FEntryNode.SelectedIndex := Entry.FEntryNode.ImageIndex;
            end;
            RootNode.Expanded := True;
          finally
            TypeList.Free;
          end;
        finally
          ResTree.Items.EndUpdate;
        end;
      end;
    end;
  finally
    ResourceFile.Free;
  end;
end;

procedure TRxProjectResExpert.ClearProjectResInfo;
var
  I: Integer;
begin
  FResFileName := '';
  if RxResourceEditor <> nil then begin
    RxResourceEditor.ResTree.Items.Clear;
    RxResourceEditor.StatusBar.Panels[0].Text := '';
  end;
  for I := 0 to FResourceList.Count - 1 do
    TResourceEntry(FResourceList.Objects[I]).Free;
  FResourceList.Clear;
end;

procedure TRxProjectResExpert.UpdateProjectResInfo;
var
  TreeState: TStringList;
  Node, ChildNode: TTreeNode;
  I: Integer;
begin
  if FLockCount > 0 then Exit;
  if RxResourceEditor <> nil then
    RxResourceEditor.ResTree.Items.BeginUpdate;
  try
    TreeState := TStringList.Create;
    try
      if RxResourceEditor <> nil then begin
        if FSelection.ResType = '' then begin
          { save selection }
          Node := RxResourceEditor.ResTree.Selected;
          if Node <> nil then begin
            if (Node.Data <> nil) then begin
              FSelection.ResName := TResourceEntry(Node.Data).GetName;
              FSelection.ResType := TResourceEntry(Node.Data).GetTypeName;
            end
            else begin
              FSelection.ResName := '';
              FSelection.ResType := Node.Text;
            end;
          end;
        end;
        { save tree state }
        Node := RxResourceEditor.ResTree.Items.GetFirstNode;
        if Node <> nil then ChildNode := Node.GetFirstChild
        else ChildNode := nil;
        while ChildNode <> nil do begin
          TreeState.AddObject(ChildNode.Text, TObject(ChildNode.Expanded));
          ChildNode := Node.GetNextChild(ChildNode);
        end;
      end;
      Inc(FLockCount);
      try
        ClearProjectResInfo;
        try
          LoadProjectResInfo;
        except
          ClearProjectResInfo;
        end;
      finally
        Dec(FLockCount);
      end;
      if (RxResourceEditor <> nil) then begin
        { restore tree state }
        Node := RxResourceEditor.ResTree.Items.GetFirstNode;
        if Node <> nil then begin
          ChildNode := Node.GetFirstChild;
          while ChildNode <> nil do begin
            I := TreeState.IndexOf(ChildNode.Text);
            if I >= 0 then
              ChildNode.Expanded := Boolean(TreeState.Objects[I]);
            ChildNode := Node.GetNextChild(ChildNode);
          end;
        end;
        if (FSelection.ResName <> '') or (FSelection.ResType <> '') then
        begin { restore selection }
          with FSelection do
            Node := FindNode(RxResourceEditor.ResTree, nil, ResName, ResType);
          if Node <> nil then begin
            if Node.Parent <> nil then Node.Parent.Expanded := True;
            Node.Selected := True;
          end;
        end;
      end;
    finally
      TreeState.Free;
      with FSelection do begin
        ResName := '';
        ResType := '';
      end;
    end;
  finally
    if RxResourceEditor <> nil then
      RxResourceEditor.ResTree.Items.EndUpdate;
  end;
end;

{$ifdef use_toolsapi}
procedure TRxProjectResExpert.OpenProject(const FileName: string);
var
  Project: IOTAProject;
begin
  Project := GetActiveProject;
//  Project.
  { TODO -otwm : implement }
end;
{$else}
procedure TRxProjectResExpert.OpenProject(const FileName: string);
begin
  CloseProject;
  if not Assigned(ToolServices) then
    exit;
  ProjectModule := ToolServices.GetModuleInterface(FileName);
  if ProjectModule <> nil then begin
    ProjectNotifier := TProjectNotifier.Create(Self);
    ProjectModule.AddNotifier(ProjectNotifier);
    try
      LoadProjectResInfo;
      FProjectName := FileName;
    except
      ClearProjectResInfo;
    end;
  end;
end;
{$endif}
procedure TRxProjectResExpert.CloseProject;
begin
  if ProjectModule <> nil then begin
    ClearProjectResInfo;
    ProjectModule.RemoveNotifier(ProjectNotifier);
    ProjectNotifier.Free;
    ProjectModule.Free;
    ProjectNotifier := nil;
    ProjectModule := nil;
    FProjectName := '';
  end;
end;

{$IFNDEF RX_D4}

procedure TRxProjectResExpert.LoadDesktop(const FileName: string);
var
  Desktop: TIniFile;
begin
  Desktop := TIniFile.Create(FileName);
  try
    if DeskTop.ReadBool(sExpertName, sVisible, False) then
      ProjectResourcesClick(nil)
    else if RxResourceEditor <> nil then RxResourceEditor.Close;
  finally
    Desktop.Free;
  end;
end;

procedure TRxProjectResExpert.SaveDesktop(const FileName: string);
var
  Desktop: TIniFile;
  Visible: Boolean;
begin
  Desktop := TIniFile.Create(FileName);
  try
    Visible := (RxResourceEditor <> nil) and RxResourceEditor.Visible;
    DeskTop.WriteBool(sExpertName, sVisible, Visible);
  finally
    Desktop.Free;
  end;
end;

{$ENDIF}

{$ifdef use_toolsapi}
procedure TRxProjectResExpert.ProjectResourcesClick(Sender: TObject);
var
  Reopen: Boolean;
  ProjectName: string;
  ResourceFile: TIResourceFile;
  ActiveProject: IOTAProject;
begin
  ResourceFile := GetResFile;
  try
    if Assigned(ResourceFile) then begin
      Reopen := RxResourceEditor = nil;
      CreateForm(TRxResourceEditor, RxResourceEditor);
      RxResourceEditor.FExpert := Self;
      ActiveProject := GetActiveProject;
      if Assigned(ActiveProject) then
      begin
        ProjectName := ActiveProject.Filename;
        if Reopen or (FProjectName <> ProjectName) then begin
          if ProjectName <> '' then OpenProject(ProjectName);
        end;
      end;
      RxResourceEditor.Show;
    end;
  finally
    ResourceFile.Free;
  end;
end;
{$else}
procedure TRxProjectResExpert.ProjectResourcesClick(Sender: TIMenuItemIntf);
var
  Reopen: Boolean;
  ProjectName: string;
  ResourceFile: TIResourceFile;
begin
  if not Assigned(ToolServices) then
    Exit;
  ResourceFile := GetResFile;
  try
    if Assigned(ResourceFile) then begin
      Reopen := RxResourceEditor = nil;
      CreateForm(TRxResourceEditor, RxResourceEditor);
      RxResourceEditor.FExpert := Self;
      ProjectName := ToolServices.GetProjectName;
      if Reopen or (FProjectName <> ProjectName) then begin
        if ProjectName <> '' then OpenProject(ProjectName);
      end;
      RxResourceEditor.Show;
    end;
  finally
    ResourceFile.Free;
  end;
end;
{$ENDIF}

procedure TRxProjectResExpert.MarkModified;
var
  EditorInterface: TIEditorInterface;
begin
  if ProjectModule <> nil then begin
    EditorInterface := ProjectModule.GetEditorInterface;
    try
      EditorInterface.MarkModified;
    finally
      EditorInterface.Free;
    end;
  end;
end;

procedure TRxProjectResExpert.CheckRename(ResFile: TIResourceFile;
  ResType, NewName: PChar);
var
  Entry: TIResourceEntry;
begin
  Entry := ResFile.FindEntry(ResType, NewName);
  try
    if Entry <> nil then
      raise Exception.Create(Format(sCannotRename, [NewName]));
  finally
    Entry.Free;
  end;
end;

function TRxProjectResExpert.UniqueName(ResFile: TIResourceFile;
  ResType: PChar; var Index: Integer): string;
var
  N: Integer;
  Entry: TIResourceEntry;

  procedure CheckItemName;
  begin
    if (ResType = RT_ICON) or (ResType = RT_CURSOR) then begin
      Result := IntToStr(N);
      Entry := ResFile.FindEntry(ResType, PChar(N));
    end
    else begin
      Result := Format(ResTypeName(ResType) + '_%d', [N]);
      Entry := ResFile.FindEntry(ResType, PChar(Result));
    end;
  end;

begin
  N := 1;
  Index := 0;
  CheckItemName;
  while Entry <> nil do begin
    Entry.Free;
    Inc(N);
    CheckItemName;
  end;
  if (ResType = RT_ICON) or (ResType = RT_CURSOR) then Index := N;
end;

function TRxProjectResExpert.DeleteEntry(ResFile: TIResourceFile;
  Entry: TResourceEntry): Boolean;
var
  I: Integer;
  P: Pointer;
  Child: TResourceEntry;
  ResourceFile: TIResourceFile;
begin
  Result := False;
  if ResFile = nil then ResourceFile := GetResFile
  else ResourceFile := ResFile;
  try
    if (ResourceFile <> nil) and (Entry <> nil) then begin
      BeginUpdate;
      try
        P := Entry.FHandle;
        Result := ResourceFile.DeleteEntry(P);
        if Result then
        try
          { delete children }
          for I := 0 to Entry.FChildren.Count - 1 do begin
            Child := TResourceEntry(Entry.FChildren[I]);
            if Child <> nil then
              ResourceFile.DeleteEntry(Child.FHandle);
          end;
        finally
          MarkModified;
        end;
      finally
        EndUpdate;
      end;
    end;
  finally
    if ResFile = nil then ResourceFile.Free;
  end;
end;

procedure TRxProjectResExpert.CreateEntry(ResFile: TIResourceFile;
  ResType, ResName: PChar; ADataSize: Integer; AData: Pointer;
  SetToEntry: Boolean);
var
  I: Integer;
  S: string;
  ResourceFile: TIResourceFile;
  Entry: TIResourceEntry;
begin
  BeginUpdate;
  try
    if ResFile = nil then ResourceFile := GetResFile
    else ResourceFile := ResFile;
    try
      if ResName = nil then begin
        S := UniqueName(ResourceFile, ResType, I);
        if I > 0 then ResName := PChar(I)
        else ResName := PChar(S);
      end;
      if not IsValidIdent(StrText(ResName)) then
        raise Exception.Create(Format(sInvalidName, [StrText(ResName)]));
      CheckRename(ResourceFile, ResType, ResName);
{$IFNDEF RX_D3}
      if ResourceFile.GetEntryCount > 0 then begin
        for I := 0 to ResourceFile.GetEntryCount - 1 do
          ResourceFile.GetEntry(I).Free;
      end;
{$ENDIF}
      Entry := ResourceFile.CreateEntry(ResType, ResName,
        MOVEABLE or DISCARDABLE, LANG_NEUTRAL, 0, 0, 0);
      if (Entry = nil) then
        raise Exception.Create(Format(sCannotRename, [StrText(ResName)]));
      with Entry do
      try
        if SetToEntry then begin
          FSelection.ResName := StrText(GetResourceName);
          FSelection.ResType := ResTypeName(GetResourceType);
        end;
        SetDataSize(PadUp(ADataSize));
        FillChar(GetData^, GetDataSize, 0);
        if GetDataSize < ADataSize then ADataSize := GetDataSize;
        Move(AData^, GetData^, ADataSize);
      finally
        Free;
      end;
      MarkModified;
    finally
      if ResFile = nil then ResourceFile.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRxProjectResExpert.NewCursorIconRes(ResFile: TIResourceFile;
  ResName: PChar; IsIcon: Boolean; Stream: TStream);
var
  ResType: PChar;
  Data: TIconData;
  ResData: Pointer;
  I, ResSize, NameOrd: Integer;
  ResourceFile: TIResourceFile;
  GroupName: string;
begin
  Data := TIconData.Create;
  try
    Data.LoadFromStream(Stream);
    if IsIcon then Data.FHeader.wType := rc3_Icon
    else Data.FHeader.wType := rc3_Cursor;
    if Data.GetCount > 0 then begin
      BeginUpdate;
      try
        if ResFile = nil then ResourceFile := GetResFile
        else ResourceFile := ResFile;
        try
          if IsIcon then ResType := RT_ICON
          else ResType := RT_CURSOR;
          for I := 0 to Data.GetCount - 1 do begin
            ResData := Data.BuildResourceItem(I, ResSize);
            try
              UniqueName(ResourceFile, ResType, NameOrd);
              CreateEntry(ResourceFile, ResType, PChar(NameOrd), ResSize,
                ResData, False);
              Data.SetNameOrdinal(I, NameOrd);
            finally
              FreeMem(ResData);
            end;
          end;
          if IsIcon then ResType := RT_GROUP_ICON
          else ResType := RT_GROUP_CURSOR;
          if ResName = nil then begin
            GroupName := UniqueName(ResourceFile, ResType, NameOrd);
            ResName := PChar(GroupName);
          end;
          ResData := Data.BuildResourceGroup(ResSize);
          try
            CreateEntry(ResourceFile, ResType, ResName, ResSize,
              ResData, True);
          finally
            FreeMem(ResData);
          end;
        finally
          if ResFile = nil then ResourceFile.Free;
        end;
      finally
        EndUpdate;
      end;
    end;
  finally
    Data.Free;
  end;
end;

procedure TRxProjectResExpert.EditCursorIconRes(Entry: TResourceEntry;
  IsIcon: Boolean; Stream: TStream);
var
  ResFile: TIResourceFile;
  CI: TCursorOrIcon;
begin
  BeginUpdate;
  try
    ResFile := GetResFile;
    try
      if not Entry.EnableRenameDelete { 'MAINICON' } then begin
        Stream.ReadBuffer(CI, SizeOf(CI));
        Stream.Seek(-SizeOf(CI), soFromCurrent);
        if (CI.Count < 1) or not (CI.wType in [rc3_Icon, rc3_Cursor]) then
          InvalidIcon;
      end;
      DeleteEntry(ResFile, Entry);
      NewCursorIconRes(ResFile, Entry.GetResourceName, IsIcon, Stream);
    finally
      ResFile.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRxProjectResExpert.NewBitmapRes(ResFile: TIResourceFile;
  ResName: PChar; Bitmap: TBitmap);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Mem);
    Mem.Position := 0;
    CreateEntry(ResFile, RT_BITMAP, ResName, Mem.Size - SizeOf(TBitmapFileHeader),
      Pointer(PChar(Mem.Memory) + SizeOf(TBitmapFileHeader)), True);
  finally
    Mem.Free;
  end;
end;

procedure TRxProjectResExpert.EditBitmapRes(Entry: TResourceEntry;
  Bitmap: TBitmap);
var
  ResFile: TIResourceFile;
begin
  BeginUpdate;
  try
    ResFile := GetResFile;
    try
      DeleteEntry(ResFile, Entry);
      NewBitmapRes(ResFile, Entry.GetResourceName, Bitmap);
    finally
      ResFile.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRxProjectResExpert.NewBinaryRes(ResFile: TIResourceFile;
  ResName, ResType: PChar; Stream: TMemoryStream);
begin
  Stream.Position := 0;
  CreateEntry(ResFile, ResType, ResName, Stream.Size, Stream.Memory, True);
end;

procedure TRxProjectResExpert.EditBinaryRes(Entry: TResourceEntry;
  Stream: TMemoryStream);
var
  ResFile: TIResourceFile;
begin
  BeginUpdate;
  try
    ResFile := GetResFile;
    try
      DeleteEntry(ResFile, Entry);
      NewBinaryRes(ResFile, Entry.GetResourceName, Entry.GetResourceType,
        Stream);
    finally
      ResFile.Free;
    end;
  finally
    EndUpdate;
  end;
end;

{ TRxResourceEditor }

{$ifdef use_toolsapi}
function GetBaseRegistryKey: string;
var
  OTAServices: IOTAServices50;
begin
  if Supports(BorlandIDEServices, IOTAServices50, OTAServices) then
    Result := OTAServices.GetBaseRegistryKey
  else
    Result := 'GetBaseRegistryKeyFailed';
end;
{$else}
function GetBaseRegistryKey: string;
begin
  if Assigned(ToolServices) then
    Result := ToolServices.GetBaseRegistryKey
  else
    Result := 'GetBaseRegistryKeyFailed';
end;
{$endif}

procedure TRxResourceEditor.FormCreate(Sender: TObject);
{$IFDEF RX_D4}
var
  I: Integer;
{$ENDIF}
begin
  TreeImages.ResourceLoad(rtBitmap, 'RXRESEXPIMG', clFuchsia);
{$IFDEF RX_D3}
  ResTree.RightClickSelect := True;
{$ENDIF}
{$IFDEF RX_D4}
  PopupMenu.Images := TreeImages;
  for I := 0 to PopupMenu.Items.Count - 1 do
    if PopupMenu.Items[I].Tag > 0 then
      PopupMenu.Items[I].ImageIndex := PopupMenu.Items[I].Tag;
  for I := 0 to NewItem.Count - 1 do
    if NewItem.Items[I].Tag > 0 then
      NewItem.Items[I].ImageIndex := NewItem.Items[I].Tag;
{$ENDIF RX_D4}
  with Placement do begin
    IniFileName := GetBaseRegistryKey;
    IniSection := sExpertID;
  end;
end;

procedure TRxResourceEditor.FormDestroy(Sender: TObject);
begin
  RxResourceEditor := nil;
end;

procedure TRxResourceEditor.ResTreeExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.ImageIndex = 0 then begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := Node.ImageIndex;
  end;
end;

procedure TRxResourceEditor.ResTreeCollapsed(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.ImageIndex = 1 then begin
    Node.ImageIndex := 0;
    Node.SelectedIndex := Node.ImageIndex;
  end;
end;

procedure TRxResourceEditor.ResTreeEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var
  Entry: TResourceEntry;
begin
  if (Node.Data = nil) then AllowEdit := False
  else begin
    Entry := TResourceEntry(Node.Data);
    AllowEdit := Entry.EnableRenameDelete;
  end;
end;

procedure TRxResourceEditor.ResTreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  Entry: TResourceEntry;
  RF: TIResourceFile;
begin
  if (Node.Data <> nil) then begin
    Entry := TResourceEntry(Node.Data);
    Inc(FExpert.FLockCount);
    try
      RF := FExpert.GetResFile;
      try
        S := AnsiUpperCase(S);
        FExpert.CheckRename(RF, Entry.GetResourceType, ResIdent(S));
        if Entry.Rename(RF, S) then begin
          Node.Text := Entry.GetName;
          FExpert.MarkModified;
        end
        else Beep;
      finally
        RF.Free;
      end;
    finally
      Dec(FExpert.FLockCount);
      S := Node.Text;
    end;
  end;
end;

procedure TRxResourceEditor.PopupMenuPopup(Sender: TObject);
var
  Node: TTreeNode;
  Entry: TResourceEntry;
begin
  Node := ResTree.Selected;
  if (Node <> nil) and (Node.Data <> nil) then begin
    Entry := TResourceEntry(Node.Data);
    EditItem.Enabled := Entry.EnableEdit;
    RenameItem.Enabled := Entry.EnableRenameDelete;
    DeleteItem.Enabled := RenameItem.Enabled;
    PreviewItem.Enabled := Entry.FResType in [rtpBitmap, rtpGroupIcon,
      rtpGroupCursor];
    SaveItem.Enabled := Entry.FResType in [rtpGroupCursor, rtpGroupIcon,
      rtpBitmap, rtpAniCursor, rtpRCData, rtpCustom];
    ResTree.Selected := Node;
  end
  else begin
    EditItem.Enabled := False;
    RenameItem.Enabled := False;
    DeleteItem.Enabled := False;
    PreviewItem.Enabled := False;
    SaveItem.Enabled := False;
  end;
end;

procedure TRxResourceEditor.RenameItemClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := ResTree.Selected;
  if Node <> nil then Node.EditText;
end;

procedure TRxResourceEditor.EditItemClick(Sender: TObject);
var
  Node: TTreeNode;
  ResFile: TIResourceFile;
  Entry: TResourceEntry;
  Graphic: TGraphic;
  Stream: TStream;
begin
  Node := ResTree.Selected;
  if Node <> nil then begin
    Entry := TResourceEntry(Node.Data);
    if (Entry <> nil) and Entry.EnableEdit then begin
      case Entry.FResType of
        rtpGroupCursor,
        rtpGroupIcon:
          begin
            if Entry.FResType = rtpGroupCursor then
              OpenDlg.Filter := sCursorFilesFilter
            else
              OpenDlg.Filter := sIconFilesFilter + '|' + sCursorFilesFilter;
            OpenDlg.FileName := '';
            if OpenDlg.Execute then begin
              Stream := TFileStream.Create(OpenDlg.FileName, fmOpenRead +
                fmShareDenyNone);
              try
                FExpert.EditCursorIconRes(Entry, Entry.FResType =
                  rtpGroupIcon, Stream);
              finally
                Stream.Free;
              end;
            end;
          end;
        rtpBitmap:
          begin
            ResFile := FExpert.GetResFile;
            try
              Graphic := Entry.GetGraphic(ResFile);
            finally
              ResFile.Free;
            end;
            try
              if EditGraphic(Graphic, nil, Entry.GetName) then begin
                if not Graphic.Empty then
                  FExpert.EditBitmapRes(Entry, TBitmap(Graphic))
                else if Entry.EnableRenameDelete then
                  FExpert.DeleteEntry(nil, Entry);
              end;
            finally
              Graphic.Free;
            end;
          end;
        rtpAniCursor,
        rtpRCData,
        rtpCustom:
          begin
            if Entry.FResType = rtpAniCursor then
              OpenDlg.Filter := sAniCursorFilesFilter
            else
              OpenDlg.Filter := sAllFilesFilter;
            OpenDlg.FileName := '';
            if OpenDlg.Execute then begin
              Stream := TMemoryStream.Create;
              try
                TMemoryStream(Stream).LoadFromFile(OpenDlg.FileName);
                FExpert.EditBinaryRes(Entry, TMemoryStream(Stream));
              finally
                Stream.Free;
              end;
            end;
          end;
        else Exit;
      end;
    end;
  end;
end;

procedure TRxResourceEditor.DeleteItemClick(Sender: TObject);
var
  Node: TTreeNode;
  Entry: TResourceEntry;
begin
  Node := ResTree.Selected;
  if Node <> nil then begin
    Entry := TResourceEntry(Node.Data);
    if (Entry <> nil) and Entry.EnableRenameDelete then
      FExpert.DeleteEntry(nil, Entry);
  end;
end;

procedure TRxResourceEditor.NewBitmapItemClick(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    if EditGraphic(Bitmap, TBitmap, sNewBitmap) then begin
      if not Bitmap.Empty then
        FExpert.NewBitmapRes(nil, nil, Bitmap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TRxResourceEditor.NewIconItemClick(Sender: TObject);
var
  Stream: TStream;
begin
  OpenDlg.Filter := sIconFilesFilter + '|' + sCursorFilesFilter;
  OpenDlg.FileName := '';
  if OpenDlg.Execute then begin
    Stream := TFileStream.Create(OpenDlg.FileName, fmOpenRead +
      fmShareDenyNone);
    try
      FExpert.NewCursorIconRes(nil, nil, True, Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TRxResourceEditor.NewCursorItemClick(Sender: TObject);
var
  Stream: TStream;
begin
  OpenDlg.Filter := sCursorFilesFilter + '|' + sAniCursorFilesFilter;
  OpenDlg.FileName := '';
  if OpenDlg.Execute then begin
    if AnsiCompareText(ExtractFileExt(OpenDlg.FileName), '.ani') = 0 then begin
      Stream := TMemoryStream.Create;
      try
        TMemoryStream(Stream).LoadFromFile(OpenDlg.FileName);
        FExpert.NewBinaryRes(nil, nil, RT_ANICURSOR, TMemoryStream(Stream));
      finally
        Stream.Free;
      end;
    end
    else begin
      Stream := TFileStream.Create(OpenDlg.FileName, fmOpenRead +
        fmShareDenyNone);
      try
        FExpert.NewCursorIconRes(nil, nil, False, Stream);
      finally
        Stream.Free;
      end;
    end;
  end;
end;

procedure TRxResourceEditor.CheckResourceType(Sender: TObject;
  var TypeName: string; var Apply: Boolean);
begin
  TypeName := AnsiUpperCase(TypeName);
  Apply := IsValidResType(TypeName) or (TypeName = ResTypeName(RT_RCDATA));
  if not Apply then
    raise Exception.Create(Format(sInvalidType, [TypeName]));
end;

function TRxResourceEditor.GetResourceTypeName: string;
var
  I: Integer;
  Entry: TResourceEntry;
begin
  Result := ResTypeName(RT_RCDATA);
  with TInputBox.Create(Application) do
  try
    Value := Result;
    Caption := SNewResource;
    Prompt := sResType;
    OnApply := CheckResourceType;
    with FExpert do
      for I := 0 to FResourceList.Count - 1 do begin
        Entry := TResourceEntry(FResourceList.Objects[I]);
        if (Entry <> nil) and (Entry.FResType in [rtpCustom, rtpRCData]) then
          if Strings.IndexOf(ResTypeName(Entry.GetResourceType)) < 0 then
            Strings.Add(ResTypeName(Entry.GetResourceType));
      end;
    if Execute then Result := Value
    else Result := '';
  finally
    Free;
  end;
end;

procedure TRxResourceEditor.NewUserDataItemClick(Sender: TObject);
var
  Mem: TMemoryStream;
  TypeName: string;
  Code: Integer;
  Id: Word;
  P: PChar;
begin
  TypeName := AnsiUpperCase(GetResourceTypeName);
  if TypeName = '' then Exit;
  Val(TypeName, Id, Code);
  if TypeName = ResTypeName(RT_RCDATA) then P := RT_RCDATA
  else if Code = 0 then P := MakeIntResource(Id)
  else P := PChar(TypeName);
  OpenDlg.Filter := sAllFilesFilter;
  OpenDlg.FileName := '';
  if OpenDlg.Execute then begin
    Mem := TMemoryStream.Create;
    try
      Mem.LoadFromFile(OpenDlg.FileName);
      FExpert.NewBinaryRes(nil, nil, P, Mem);
    finally
      Mem.Free;
    end;
  end;
end;

procedure TRxResourceEditor.PreviewItemClick(Sender: TObject);
begin
  { not implemented yet, item is invisible }
end;

procedure TRxResourceEditor.SaveItemClick(Sender: TObject);
var
  Node: TTreeNode;
  ResFile: TIResourceFile;
  Entry: TResourceEntry;
  Graphic: TGraphic;
  Stream: TStream;
begin
  { save resource }
  Node := ResTree.Selected;
  if Node <> nil then begin
    Entry := TResourceEntry(Node.Data);
    if (Entry <> nil) then begin
      with SaveDlg do begin
        case Entry.FResType of
          rtpGroupCursor:
            begin
              Filter := sCursorFilesFilter + '|' + sAllFilesFilter;
              DefaultExt := 'cur';
            end;
          rtpGroupIcon:
            begin
              Filter := sIconFilesFilter + '|' + sAllFilesFilter;
              DefaultExt := GraphicExtension(TIcon);
            end;
          rtpBitmap:
            begin
              Filter := GraphicFilter(TBitmap) + '|' + sAllFilesFilter;
              DefaultExt := GraphicExtension(TBitmap);
            end;
          rtpAniCursor:
            begin
              Filter := sAniCursorFilesFilter + '|' + sAllFilesFilter;
              DefaultExt := 'ani';
            end;
          else
            begin
              Filter := sAllFilesFilter;
              DefaultExt := '';
            end;
        end;
        FileName := '';
      end;
      if SaveDlg.Execute then begin
        ResFile := FExpert.GetResFile;
        try
          case Entry.FResType of
            rtpBitmap:
              begin
                Graphic := Entry.GetGraphic(ResFile);
                try
                  Graphic.SaveToFile(SaveDlg.FileName);
                finally
                  Graphic.Free;
                end;
              end;
            rtpGroupCursor, rtpGroupIcon,
            rtpAniCursor, rtpRCData, rtpCustom:
              begin
                Stream := TFileStream.Create(SaveDlg.FileName, fmCreate);
                try
                  if Entry.FResType in [rtpGroupCursor, rtpGroupIcon] then
                    Entry.GetIconData(ResFile, Stream)
                  else Entry.GetData(ResFile, Stream);
                finally
                  Stream.Free;
                end;
              end;
            else Exit;
          end;
        finally
          ResFile.Free;
        end;
      end;
    end;
  end;
end;

procedure TRxResourceEditor.ResTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = Char(VK_RETURN)) then begin
    EditItemClick(Sender);
    Key := #0;
  end;
end;

procedure TRxResourceEditor.ResTreeDblClick(Sender: TObject);
begin
  EditItemClick(Sender);
end;

procedure TRxResourceEditor.ResTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  Entry: TResourceEntry;
  S: string;
begin
  S := '';
  if Node <> nil then begin
    Entry := TResourceEntry(Node.Data);
    if Entry <> nil then begin
      if Entry.FResType in [rtpGroupCursor, rtpGroupIcon] then
        S := Format('%d image(s)  ', [Entry.FChildren.Count]);
      S := S + Format('%d byte(s)', [Entry.FSize]);
    end;
  end;
  if S = '' then S := FExpert.FResFileName;
  StatusBar.Panels[0].Text := S;
end;

procedure TRxResourceEditor.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  Offset: Integer;
begin
  with StatusBar do begin
    Offset := Max(0, (HeightOf(Rect) - Canvas.TextHeight('Wg')) div 2);
    WriteText(Canvas, Rect, Offset, Offset, MinimizeText(Panels[0].Text,
      Canvas, WidthOf(Rect) - Height), taLeftJustify, False);
  end;
end;

initialization
  RxResourceEditor := nil;
end.