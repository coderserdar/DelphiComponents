{*******************************************************}
{                                                       }
{       DB Edit version componets                       }
{                                                       }
{       Copyright (C) 2003 Jaro.Benes                   }
{                                                       }
{*******************************************************}

unit jbDBEdit;

{$W-}

interface

{$I jb.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Menus, Forms, {$IFDEF VER6UP}Types,{$ENDIF}
  {$ifdef Ver17Up}System.UITypes,{$EndIf}
  StdCtrls, ExtCtrls, Mask, Buttons, jbEdit, DB, DBCtrls, Dialogs
  {$IFDEF USE_RXLIB}, RxToolEdit, RxCurrEdit, RxCtrls, FileCtrl, RxDateUtil, RXSpin,
    RxDBCurrEdit
  {$ENDIF USE_RXLIB}
{$IFDEF VER6UP}, MaskUtils{$ENDIF};

type

  {  TDBPubEdit  }

  TDBPubEdit = class(TPubEdit)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubLabeledEdit  }

  TDBPubLabeledEdit = class(TPubLabeledEdit)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubMaskEdit  }

  TDBPubMaskEdit = class(TPubMaskEdit)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubLabeledMaskEdit  }

  TDBPubLabeledMaskEdit = class(TPubLabeledMaskEdit)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubComboBox  }

  TDBPubComboBox = class(TPubComboBox)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubLabeledComboBox  }

  TDBPubLabeledComboBox = class(TPubLabeledComboBox)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoDataChange(Sender : TObject);
    procedure DoEditingChange(Sender : TObject);
    procedure DoUpdateData(Sender : TObject);
    procedure DoActiveChange(Sender : TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  {  TDBPubLookupComboBox  }

  TDBPubLookupComboBox = class(TDBLookupComboBox)
  private
    {$IFDEF CB_HINTER}
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro cancas}
    FHintText: Boolean; {zvlastni instance}
    {$ENDIF}
    {}
    FErrorOccur: TErrorOccur;
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FOnListPick: TNotifyEvent;
    FOnItemsChanged: TNotifyEvent;
    FDropDownFlexWidth: Boolean;
    FValidate: TValidate;
    procedure SetRequired(Value: Boolean);
    function GetAsInteger: Integer;
  protected
    procedure ListPick; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure SetItems(const Value: TStrings); virtual; //full hide!
    {$WARNINGS ON}
    {$IFDEF CB_HINTER}
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF CB_HINTER}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WndProc(var Message: TMessage); override;
    {$IFDEF CB_HINTER}
    procedure ReleaseIt;
    {$ENDIF CB_HINTER}
    function DoValidate: Boolean;
  public
    {$IFDEF CB_HINTER}
    HintWin: TPaintLineHintWindow;
    {$ENDIF CB_HINTER}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure DropDown; {$IFDEF VER5UP}reintroduce;{$ENDIF}//full hide!
    {$WARNINGS ON}
    procedure Clear;
    property AsInteger: Integer read GetAsInteger;
    {$IFDEF CB_HINTER}
    procedure DefaultHandler(var message); override;
    {$ENDIF CB_HINTER}
    function Validator: Boolean;
  published
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DropDownFlexWidth:Boolean read FDropDownFlexWidth write FDropDownFlexWidth default False;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    {$IFDEF CB_HINTER}
    property HintText: Boolean read FHintText write FHintText default False;
    {$ENDIF CB_HINTER}
    property Left;
    {$IFDEF CB_HINTER}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$ENDIF CB_HINTER}
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnListPick: TNotifyEvent read FOnListPick write FOnListPick;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  {  TDBPubLabeledLookupComboBox  }

  TDBPubLabeledLookupComboBox = class(TDBPubLookupComboBox)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubDBEdit  }

  TPubDBEdit = class(TDBEdit)
  private
    FScrollBars: TScrollStyle;
    FAlignment: TAlignment;
    FMultiline: Boolean;
    FWordWrap: Boolean;
    FAfterPaint: TNotifyEvent;
    FValidate: TValidate;
    FErrorOccur: TErrorOccur;
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FAcceptEmpty: Boolean; {neze byt prazdne} {8.8.2001}
    FErrMsg: string;
    procedure SetRequired(Value: Boolean);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetMultiline(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    function DoValidate: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Canvas: TCanvas; {public version of the canvas}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(var Message: TWMPaint); message WM_PAINT;
    function Validator: Boolean;
    property ErrMsg: string read FErrMsg write FErrMsg;
  published
    property AcceptEmpty: Boolean read FAcceptEmpty write FAcceptEmpty default False;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Required: Boolean read FRequired write SetRequired default False;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnValidate: TValidate read FValidate write FValidate;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Multiline: Boolean read FMultiline write SetMultiline default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnAfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
  end;

  {  TPubLabeledDBEdit  }

  TPubLabeledDBEdit = class(TPubDBEdit)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

{$IFDEF USE_RXLIB}

  {  TPubCurrencyEdit  }

  TPubDBCurrencyEdit = class(TDBCurrencyEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FValidate: TValidate;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function DoValidate: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validator: Boolean;
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Left;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Value;
    property Visible;
    property Width;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  { TPubLabeledCurrencyEdit }

  TPubLabeledDBCurrencyEdit = class(TPubDBCurrencyEdit)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;
{$ENDIF}

procedure Register;

implementation

uses
  Consts;

{$IFDEF VER3UP}
resourcestring
{$ELSE}
const
{$ENDIF}
{$IFNDEF msgAsEngList}
  msgFieldRequired = 'Pole je požadováno vyplnit';
  StrExpectedIntegerValue = 'Vstupní hodnota není typu integer!';
{$ELSE}
  msgFieldRequired = 'Field required to fill';
  StrExpectedIntegerValue = 'Integer type value expected!';
{$ENDIF}

{ TDBPubEdit }

procedure TDBPubEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubEdit.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBPubEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubEdit.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    then begin
    if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
    begin
      raise Exception.Create('DataField can be string type only');
    end;
  end;
end;

procedure TDBPubEdit.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubEdit.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubEdit.DoUpdateData(Sender: TObject);
begin
  //try
    //FDataLink.OnDataChange := nil;
    //FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
  //finally
    //FDataLink.OnDataChange := DoDataChange;
  //end;
end;

function TDBPubEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ TDBPubLabeledEdit }

procedure TDBPubLabeledEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubLabeledEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubLabeledEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubLabeledEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubLabeledEdit.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubLabeledEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBPubLabeledEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubLabeledEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubLabeledEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubLabeledEdit.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
  then
  begin
      if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
      begin
        raise Exception.Create('DataField can be string type only');
      end;
  end;
end;

procedure TDBPubLabeledEdit.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubLabeledEdit.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubLabeledEdit.DoUpdateData(Sender: TObject);
begin
//  try
//    FDataLink.OnDataChange := nil;
//    FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
//  finally
//    FDataLink.OnDataChange := DoDataChange;
//  end;
end;

function TDBPubLabeledEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubLabeledEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubLabeledEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ TDBPubMaskEdit }

procedure TDBPubMaskEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubMaskEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubMaskEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubMaskEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubMaskEdit.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubMaskEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBPubMaskEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubMaskEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubMaskEdit.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    then begin
    if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
    begin
      raise Exception.Create('DataField can be string type only');
    end;
  end;
end;

procedure TDBPubMaskEdit.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubMaskEdit.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubMaskEdit.DoUpdateData(Sender: TObject);
begin
//  try
//    FDataLink.OnDataChange := nil;
//    FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
//  finally
//    FDataLink.OnDataChange := DoDataChange;
//  end;
end;

function TDBPubMaskEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubMaskEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ TDBPubLabeledMaskEdit }

procedure TDBPubLabeledMaskEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubLabeledMaskEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubLabeledMaskEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubLabeledMaskEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubLabeledMaskEdit.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubLabeledMaskEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBPubLabeledMaskEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubLabeledMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubLabeledMaskEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubLabeledMaskEdit.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    then begin
    if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
    begin
      raise Exception.Create('DataField can be string type only');
    end;
  end;
end;

procedure TDBPubLabeledMaskEdit.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubLabeledMaskEdit.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubLabeledMaskEdit.DoUpdateData(Sender: TObject);
begin
//  try
//    FDataLink.OnDataChange := nil;
//    FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
//  finally
//    FDataLink.OnDataChange := DoDataChange;
//  end;
end;

function TDBPubLabeledMaskEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubLabeledMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubLabeledMaskEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ TDBPubComboBox }

procedure TDBPubComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubComboBox.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubComboBox.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubComboBox.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBPubComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubComboBox.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubComboBox.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    then begin
    if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
    begin
      raise Exception.Create('DataField can be string type only');
    end;
  end;
end;

procedure TDBPubComboBox.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubComboBox.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubComboBox.DoUpdateData(Sender: TObject);
begin
//  try
//    FDataLink.OnDataChange := nil;
//    FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
//  finally
//    FDataLink.OnDataChange := DoDataChange;
//  end;
end;

function TDBPubComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubComboBox.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{ TDBPubLabeledComboBox }

procedure TDBPubLabeledComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBPubLabeledComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPubLabeledComboBox.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBPubLabeledComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPubLabeledComboBox.SetDataField(const Value: string);
begin
//  if not (csDesigning in ComponentState) then
//    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBPubLabeledComboBox.Change;
begin
  FDataLink.Modified;
  inherited Change;

end;

procedure TDBPubLabeledComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

constructor TDBPubLabeledComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DoDataChange;
  FDataLink.OnEditingChange := DoEditingChange;
  FDataLink.OnUpdateData := DoUpdateData;
  FDataLink.OnActiveChange := DoActiveChange;
end;

destructor TDBPubLabeledComboBox.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBPubLabeledComboBox.DoActiveChange(Sender: TObject);
begin
  if (FDataLink <> nil) and (FDataLink.Active)
    and (DataField <> '') and (FDataLink.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    then begin
    if not (FDataLink.DataSet.FieldByName(DataField).DataType in [ftString, ftWideString]) then
    begin
      raise Exception.Create('DataField can be string type only');
    end;
  end;
end;

procedure TDBPubLabeledComboBox.DoDataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.AsString
  else Text := '';
end;

procedure TDBPubLabeledComboBox.DoEditingChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      FDataLink.Field.AsString := Text;
end;

procedure TDBPubLabeledComboBox.DoUpdateData(Sender: TObject);
begin
//  try
//    FDataLink.OnDataChange := nil;
//    FDataLink.DataSet.Edit;
    FDataLink.Field.AsString := Text;
//  finally
//    FDataLink.OnDataChange := DoDataChange;
//  end;
end;

function TDBPubLabeledComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPubLabeledComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not ReadOnly and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_END,
    VK_HOME, VK_PRIOR, VK_NEXT]) and FDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBPubLabeledComboBox.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{  TDBPubLookupComboBox  }

constructor TDBPubLookupComboBox.Create(AOwner: TComponent);
{$IFDEF CB_HINTER}
var
  TheForm: TComponent; {The form that ultimately owns this component}
{$ENDIF}
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
  FDropDownFlexWidth := False;
  {$IFDEF CB_HINTER}
  { start hint window functionality }
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;

  FHintText := False; //as default for combo
  {$ENDIF}
end;

{$IFDEF CB_HINTER}
procedure TDBPubLookupComboBox.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;
{$ENDIF}

destructor TDBPubLookupComboBox.Destroy;
begin
  FColorOptionsSet.Free;
  { stop hint window functionality }
  {$IFDEF CB_HINTER}HintWin.Free;{$ENDIF}
  inherited Destroy;
end;

procedure TDBPubLookupComboBox.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

function TDBPubLookupComboBox.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TDBPubLookupComboBox.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TDBPubLookupComboBox.DoExit;
var
  S: string;
  CanExit: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end
  else
    if Assigned(FValidate) then
    begin
      S := Text;
      FValidate(Self, S, CanExit);
      if not CanExit then
        if CanFocus then SetFocus;
    end;
end;

function TDBPubLookupComboBox.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

procedure TDBPubLookupComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
end;
{$IFDEF CB_HINTER}
procedure TDBPubLookupComboBox.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
end;
{$ENDIF}
procedure TDBPubLookupComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TDBPubLookupComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{ lets see if there is something to do? }

procedure TDBPubLookupComboBox.ListPick;
begin
  if Assigned(FOnListPick) then FOnListPick(Self);
end;
{$IFDEF CB_HINTER}
procedure TDBPubLookupComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (FHintText) then begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or
    not Application.Active then Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > (ClientWidth - 16) then begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TDBPubLookupComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FSelectAll and FWider and Focused then begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

procedure TDBPubLookupComboBox.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;
{$ENDIF}
procedure TDBPubLookupComboBox.Clear;
begin
  inherited;
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TDBPubLookupComboBox.SetItems(const Value: TStrings);
begin
  //{$IFDEF VER7UP}inherited{$ENDIF} SetItems(Value);
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
  //DoDropDownFlexWidth;
end;

{ slightly modified CNCommand handler }

procedure TDBPubLookupComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
end;

procedure TDBPubLookupComboBox.DropDown;
{invoke dropdown programaticaly}
begin
  // Check whether DropedDown
  if SendMessage(Self.Handle, CB_GETDROPPEDSTATE, 0, 0) <> 1 then
    // nop, so drop it
    SendMessage(Self.Handle, CB_SHOWDROPDOWN, 1, 0)
end;

function TDBPubLookupComboBox.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TDBPubLookupComboBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CB_ADDSTRING, CB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        if Assigned(FOnItemsChanged) then
          FOnItemsChanged(Self);
        //DoDropDownFlexWidth;
      end;
    else
      inherited WndProc(Message);
  end;
end;

{  TDBPubLabeledLookupComboBox  }

procedure TDBPubLabeledLookupComboBox.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TDBPubLabeledLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TDBPubLabeledLookupComboBox.Destroy;
begin
  FEditLabel.Free;
  inherited;
end;

procedure TDBPubLabeledLookupComboBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
//  if csDesigning in ComponentState then
//    Text := '';
end;

procedure TDBPubLabeledLookupComboBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TDBPubLabeledLookupComboBox.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TDBPubLabeledLookupComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TDBPubLabeledLookupComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TDBPubLabeledLookupComboBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TDBPubLabeledLookupComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TDBPubLabeledLookupComboBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TDBPubLabeledLookupComboBox.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TDBPubLabeledLookupComboBox.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{  TPubDBEdit  }

constructor TPubDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
  FAcceptEmpty := False;
  FScrollBars := ssNone;
  FAlignment := taLeftJustify;
  FMultiline := False;
  FWordWrap := False;
  FErrMsg := '';
  Canvas := TControlCanvas.Create;
  {aby se dalo kreslit, musi se to napojit takto !!!}
  TControlCanvas(Canvas).Control := Self;
end;

destructor TPubDBEdit.Destroy;
begin
  FColorOptionsSet.Free;
  Canvas.Free;
  inherited Destroy;
end;

procedure TPubDBEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubDBEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  if not AutoSelect then
    if Trim(Text) = '' then SelStart := 0
    else SelStart := GetTextLen;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubDBEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end;

end;

function TPubDBEdit.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

procedure TPubDBEdit.CMExit(var Message: TCMExit);
var
  CanExit: Boolean;
  S: string;
{$IFDEF emptymaskrequired}
  CurValue: string;
  CurMask: string;
{$ENDIF emptymaskrequired}
begin
  {tento kod doplnen pro pozadavek prazdneho pole}
{$IFDEF emptymaskrequired}
  CurValue := Text;
  if FAcceptEmpty and ((Text = FormatMaskText(EditMask, '')) or (Text = '')) then
  begin
    CurMask := EditMask;
    EditMask := '';
    try
      inherited;
    except
      Text := CurValue;
      raise;
      Exit;
    end;
    EditMask := CurMask;
    Exit;
  end;
{$ENDIF emptymaskrequired}
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit; {out without any message}
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}

  if Assigned(FValidate) then begin
    S := Text;
    FValidate(Self, S, CanExit);
    if S <> Text then {in validate can be changed, update it !}
      Text := S;
    if not CanExit then begin {fatal error in validate}
      MessageBeep($FFFF);
      if Self.CanFocus then Self.SetFocus;
      Exit;
    end;
  end;
end;

procedure TPubDBEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubDBEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPubDBEdit.CreateParams(var Params: TCreateParams);
const
  aAlignments: array[TAlignment] of DWORD = ( ES_LEFT, ES_RIGHT, ES_CENTER );
  aMultiline: array[boolean] of DWORD = ( 0, ES_MULTILINE );
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or aMultiline[FMultiline] or WS_CLIPCHILDREN
    or aAlignments[FAlignment] or ScrollBar[FScrollBars] or WordWraps[FWordWrap];
end;

procedure TPubDBEdit.SetMultiline(Value: Boolean);
begin
  FMultiline := Value;
  RecreateWnd;
end;

procedure TPubDBEdit.SetScrollBars(Value: TScrollStyle);
begin
  FScrollBars := Value;
  RecreateWnd;
end;

procedure TPubDBEdit.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  RecreateWnd;
end;

procedure TPubDBEdit.SetWordWrap(Value: Boolean);
begin
  FWordWrap := Value;
  RecreateWnd;
end;

function TPubDBEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubDBEdit.Paint(var Message: TWMPaint);
begin
  inherited;
  if Assigned(FAfterPaint) then
    FAfterPaint(Self);
end;

{  TPubLabeledDBEdit  }

procedure TPubLabeledDBEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledDBEdit.Destroy;
begin
  FEditLabel.Free;
  inherited;
end;

procedure TPubLabeledDBEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
//  if csDesigning in ComponentState then
//    Text := '';
end;

procedure TPubLabeledDBEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledDBEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledDBEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TPubLabeledDBEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledDBEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledDBEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledDBEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledDBEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledDBEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{$IFDEF USE_RXLIB}

{  TPubDBCurrencyEdit  }

constructor TPubDBCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubDBCurrencyEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

function TPubDBCurrencyEdit.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

function TPubDBCurrencyEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubDBCurrencyEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubDBCurrencyEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubDBCurrencyEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then begin
    if BeepOnError then MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubDBCurrencyEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then begin
    if BeepOnError then MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubDBCurrencyEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubDBCurrencyEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{ TPubLabeledDBCurrencyEdit }

procedure TPubLabeledDBCurrencyEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledDBCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledDBCurrencyEdit.Destroy;
begin
{$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
{$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
{$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledDBCurrencyEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledDBCurrencyEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledDBCurrencyEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledDBCurrencyEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledDBCurrencyEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledDBCurrencyEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledDBCurrencyEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledDBCurrencyEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledDBCurrencyEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{$ENDIF}

{  Register  }

procedure Register;
begin
  RegisterComponents('Library', [
    TDBPubEdit,
    TDBPubLabeledEdit,
    TDBPubMaskEdit,
    TDBPubLabeledMaskEdit,
    TDBPubComboBox,
    TDBPubLabeledComboBox,
    TDBPubLookupComboBox,
    TDBPubLabeledLookupComboBox,
  {$IFDEF USE_RXLIB}
    TPubDBCurrencyEdit,
    TPubLabeledDBCurrencyEdit,
  {$ENDIF}
    TPubDBEdit,
    TPubLabeledDBEdit 
    ]);
end;

{end-of-file}
end.