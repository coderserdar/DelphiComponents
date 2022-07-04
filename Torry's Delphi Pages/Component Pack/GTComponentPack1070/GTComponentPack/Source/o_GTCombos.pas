{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       o_GTCombos                                      }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTCombos;

interface

uses
   Classes
  ,Controls
  ,StdCtrls
  ,Graphics
  ,Types
  ,Messages
  ;

type
{------------------------------------------------------------------------------}
  TgtCustomComboBox = class(TCustomComboBox)
  private
    FReadOnly: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyPress(var Key : Char);override;
    procedure Select;override;
    procedure InitializeItems;virtual;abstract;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure SetParent(AParent : TWinControl);override;
    procedure RefreshItems;
  published
    { Published declarations}
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
  published //Derived Properties;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Items;
    property Visible;
  end;
{------------------------------------------------------------------------------}
  TgtPrinterCombo = class(TgtCustomComboBox)
  private
    FPrinterName: string;
    procedure SetPrinterName(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure InitializeItems;override;
    procedure Select;override;
  public
    { Public declarations }
  published
    { Published declarations}
    property PrinterName : string read FPrinterName write SetPrinterName;
  end;
{------------------------------------------------------------------------------}
  TgtFontCombo = class(TgtCustomComboBox)
  private
    FSelectedFontName: string;
    FFont: TFont;
    procedure SetSelectedFontName(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure InternalOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure InitializeItems;override;
    procedure Select;override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property SelectedFont     : TFont  read FFont;
    property SelectedFontName : string read FSelectedFontName write SetSelectedFontName;
  end;
{------------------------------------------------------------------------------}
  TgtComboEnumThread = class(TThread)
  protected
    { Protected declarations }
    FResultList : TStrings;
  public
    { Public declarations }
    procedure Execute;override;
  public
    constructor Create(ResultList : TStrings);
    destructor  Destroy;override;
  end;
{------------------------------------------------------------------------------}
  TgtSQLServerEnumThread = class(TgtComboEnumThread)
  public
    { Public declarations }
    procedure Execute;override;
  end;
{------------------------------------------------------------------------------}
  TgtSQLServerCombo = class(TgtCustomComboBox)
  private
    FSelectedServer: string;
    procedure SetSelectedServer(const Value: string);
    { Private declarations }
  protected
    FEnumThread : TgtSQLServerEnumThread;
  protected
    { Protected declarations }
    procedure EnumerateSQLServers(Names : TStrings);
    procedure InitializeItems;override;
    procedure Select;override;
  public
    { Public declarations }
  published
    { Published declarations}
    property SelectedServer   : string  read FSelectedServer   write SetSelectedServer;
  end;
{------------------------------------------------------------------------------}
  TgtNetResourceEnumThread = class(TgtComboEnumThread)
  private
    FDisplayType: DWord;
    FResourceType: DWord;
  public
    { Public declarations }
    procedure Execute;override;
  public
    property ReSourceType : DWord read FResourceType write FResourceType;
    property DisplayType  : DWord read FDisplayType  write FDisplayType;
  end;
{------------------------------------------------------------------------------}
  TgtNetResourceCombo = class(TgtCustomComboBox)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FEnumThread : TgtNetResourceEnumThread;
    procedure EnumerateNetResources(Names : TStrings);
    procedure InitializeItems;override;
    procedure Select;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
  end;
{------------------------------------------------------------------------------}



implementation
uses
   Forms
  ,Printers
  ,ActiveX
  ,OleDB
  ,AdoDB
  ,AdoInt
  ,ComObj
  ,DB
  ,Windows
  ,SysUtils
  ;

{ TgtCustomComboBox }
{------------------------------------------------------------------------------}
constructor TgtCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReadOnly   := True;
  Self.Sorted := True;
end;
{------------------------------------------------------------------------------}
destructor TgtCustomComboBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FReadOnly then
    Key := #0;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomComboBox.RefreshItems;
begin
  InitializeItems;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomComboBox.Select;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(AParent) then
  begin
    Text      := '';
    InitializeItems;
  end;
end;
{------------------------------------------------------------------------------}


{ TgtPrinterCombo }
{------------------------------------------------------------------------------}
procedure TgtPrinterCombo.InitializeItems;
begin
  inherited;
  Items.Clear;
  Items.AddStrings(Printer.Printers);
end;
{------------------------------------------------------------------------------}
procedure TgtPrinterCombo.Select;
begin
  inherited;
  FPrinterName := Self.Text;
end;
{------------------------------------------------------------------------------}
procedure TgtPrinterCombo.SetPrinterName(const Value: string);
var
  Idx : Integer;
begin
  Idx := Items.IndexOf(Value);
  if Idx >= 0 then
    ItemIndex := Idx;
end;
{------------------------------------------------------------------------------}






{ TgtFontCombo }
{------------------------------------------------------------------------------}
constructor TgtFontCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont           := TFont.Create;
  Self.OnDrawItem := InternalOnDrawItem;
  Self.Style      := csOwnerDrawFixed;
end;
{------------------------------------------------------------------------------}
destructor TgtFontCombo.Destroy;
begin
  FFont.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFontCombo.InitializeItems;
begin
  Items.Clear;
  Items.AddStrings(Screen.Fonts);
end;
{------------------------------------------------------------------------------}
procedure TgtFontCombo.InternalOnDrawItem(Control: TWinControl;Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
 with (Control as TgtCustomComboBox).Canvas do
 begin
   Font.Name := Items[Index];
   Font.Size := 10;
   FillRect(Rect) ;
   TextOut(Rect.Left, Rect.Top, PChar(Items[Index]))
 end;
end;
{------------------------------------------------------------------------------}
procedure TgtFontCombo.Select;
begin
  inherited;
  FSelectedFontName := Text;
  FFont.Name := FSelectedFontName;
end;
{------------------------------------------------------------------------------}
procedure TgtFontCombo.SetSelectedFontName(const Value: string);
var
  Idx : Integer;
begin
  Idx := Items.IndexOf(Value);
  if Idx >= 0 then
    ItemIndex := Idx;
end;
{------------------------------------------------------------------------------}



{ TgtSQLServerCombo }
{------------------------------------------------------------------------------}
procedure TgtSQLServerCombo.EnumerateSQLServers(Names: TStrings);
begin
  if Assigned(FEnumThread) then
  begin
    FEnumThread.Terminate;
    FEnumThread := nil;
  end;
  FEnumThread := TgtSQLServerEnumThread.Create(Names);
  try
    FEnumThread.Resume;
  finally
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSQLServerCombo.InitializeItems;
begin
  inherited;
  Items.Clear;
  EnumerateSQLServers(Items);
end;
{------------------------------------------------------------------------------}
procedure TgtSQLServerCombo.Select;
begin
  inherited;
  FSelectedServer := Text;
end;
{------------------------------------------------------------------------------}
procedure TgtSQLServerCombo.SetSelectedServer(const Value: string);
var
 Idx : Integer;
begin
  Idx := Items.IndexOf(Value);
  if Idx >= 0 then
    ItemIndex := Idx;
end;
{------------------------------------------------------------------------------}

{ TgtSQLServeEnumThread }
{------------------------------------------------------------------------------}
constructor TgtComboEnumThread.Create(ResultList: TStrings);
begin
  inherited Create(True);
  FResultList := ResultList;
  FreeOnTerminate := True;
end;
{------------------------------------------------------------------------------}
destructor TgtComboEnumThread.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtComboEnumThread.Execute;
begin
  inherited;
end;
{------------------------------------------------------------------------------}

{ TgtSQLServerEnumThread }
{------------------------------------------------------------------------------}
procedure TgtSQLServerEnumThread.Execute;
{
 I can not remember from where i took the code so i can give credit about it.
 But if the author of this code reads this lines mail me at info@gtdelphicomponents.gr to write your name.
}
var
  RSCon: ADORecordsetConstruction;
  Rowset: IRowset;
  SourcesRowset: ISourcesRowset;
  SourcesRecordset: _Recordset;
  SourcesName, SourcesType: TField;

    function PtCreateADOObject
             (const ClassID: TGUID): IUnknown;
    var
      Status: HResult;
      FPUControlWord: Word;
    begin
      asm
        FNSTCW FPUControlWord
      end;
      Status := CoCreateInstance(
                  CLASS_Recordset,
                  nil,
                  CLSCTX_INPROC_SERVER or
                  CLSCTX_LOCAL_SERVER,
                  IUnknown,
                  Result);
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
      OleCheck(Status);
    end;
begin
  if not Self.Terminated then
  begin
    CoInitialize(nil);
    try
    SourcesRecordset :=
        PtCreateADOObject(CLASS_Recordset)
        as _Recordset;
    RSCon :=
        SourcesRecordset
        as ADORecordsetConstruction;
    SourcesRowset :=
        CreateComObject(ProgIDToClassID('SQLOLEDB Enumerator'))
        as ISourcesRowset;
    OleCheck(SourcesRowset.GetSourcesRowset(
         nil,
         IRowset, 0,
         nil,
         IUnknown(Rowset)));
    RSCon.Rowset := RowSet;
    with TADODataSet.Create(nil) do
    try
      Recordset   := SourcesRecordset;
      SourcesName := FieldByName('SOURCES_NAME');
      SourcesType := FieldByName('SOURCES_TYPE');
      FResultList.BeginUpdate;
      try
        while not EOF do
        begin
          if
             (SourcesType.AsInteger = DBSOURCETYPE_DATASOURCE)
             and (Length(SourcesName.AsString) > 0) then
            if FResultList.IndexOf(SourcesName.AsString) < 0 then
              FResultList.Add(SourcesName.AsString);
          Next;
        end;
      finally
        FResultList.EndUpdate;
      end;
    finally
      Free;
    end;
    finally
      CoUnInitialize;
    end;
  end;
end;
{------------------------------------------------------------------------------}




{ TgtComputerEnumThread }
type
  PNetArr = Array of PNetResource;
{------------------------------------------------------------------------------}
procedure TgtNetResourceEnumThread.Execute;
 { Code Taken from http://cc.embarcadero.com/Item/16392 }
  function EnumerateFunc( hwnd: HWND; hdc: HDC ; lpnr: PNetResource ): Boolean;
 // const
//    cbBuffer : DWORD  	 = 16384;      // 16K is a good size
  var
    //, dwResultEnum:
    hEnum, dwResult : DWORD;
    lpnrLocal: array[0..16384 div SizeOf(TNetResource)] of TNetResource;     // pointer to enumerated structures
    i: integer;
    cEntries : Longint;                // enumerate all possible entries
    TempStr  : string;
    cbBuffer : DWORD;
  begin
    centries := -1;
    cbBuffer := 16384;
    // Call the WNetOpenEnum function to begin the enumeration.
    dwResult := WNetOpenEnum(
                            RESOURCE_CONTEXT,  // Enumerate currently connected resources.
                            RESOURCETYPE_DISK, // all resources
                            0,                 // enumerate all resources
                            lpnr,              // NULL first time the function is called
                            hEnum              // handle to the resource
                            );

    if (dwResult <> NO_ERROR) then
    begin
      // Process errors with an application-defined error handler
      result := FALSE;
      Exit;
    end;

    // Initialize the buffer.
    FillChar( lpnrLocal, cbBuffer, 0 );

    // Call the WNetEnumResource function to continue
    //  the enumeration.
//    dwResultEnum :=
                    WNetEnumResource(hEnum,           // resource handle
                                    DWORD(cEntries),  // defined locally as -1
                                    @lpnrLocal,       // LPNETRESOURCE
                                    cbBuffer);        // buffer size

    // Thiss is just printing
    for i := 0 to cEntries - 1 do
    begin
      TempStr := StrPas(lpnrLocal[i].lpRemoteName);
      if (FResultList.IndexOf(TempStr) < 0) and (Length(TempStr) > 0) then
        FResultList.Add( StrPas(lpnrLocal[i].lpRemoteName) );
    end;

    // Call WNetCloseEnum to end the enumeration.
    dwResult := WNetCloseEnum(hEnum);

    if(dwResult <> NO_ERROR) then
    begin
      // Process errors... some user defined function here
      result := FALSE;
    end
    else
      result :=  TRUE;
  end;
var
  Alpnr: PNetResource;
  Ahdc : HDC;
begin
  if not Self.Terminated then
  begin
    Alpnr := nil;
    Ahdc  := 0;
    EnumerateFunc( HWND(nil), Ahdc , Alpnr );
  end;
end;
{------------------------------------------------------------------------------}

{ TgtNetResourceCombo }
{------------------------------------------------------------------------------}
constructor TgtNetResourceCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtNetResourceCombo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtNetResourceCombo.EnumerateNetResources(Names: TStrings);
begin
  if Assigned(FEnumThread) then
  begin
    FEnumThread.Terminate;
    FEnumThread := nil;
  end;
  FEnumThread := TgtNetResourceEnumThread.Create(Names);
  try
    FEnumThread.ResourceType := RESOURCETYPE_ANY;
    FEnumThread.DisplayType  := RESOURCEDISPLAYTYPE_GENERIC;
    FEnumThread.Resume;
  finally
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtNetResourceCombo.InitializeItems;
begin
  inherited;
  Items.Clear;
  EnumerateNetResources(Items);
end;
{------------------------------------------------------------------------------}
procedure TgtNetResourceCombo.Select;
begin
  inherited;
end;
{------------------------------------------------------------------------------}

end.
