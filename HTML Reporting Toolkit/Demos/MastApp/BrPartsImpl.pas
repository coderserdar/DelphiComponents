unit BrPartsImpl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, BrParts_TLB, StdVcl, Db, DBTables, StdCtrls, DBCtrls,
  ExtCtrls, Buttons, Grids, DBGrids, Edparts;

type
  TBrPartsFrm = class(TActiveForm, IBrPartsFrm)
    PartsGrid: TDBGrid;
    Panel1: TPanel;
    ActivateBtn: TSpeedButton;
    Bevel1: TBevel;
    Navigator: TDBNavigator;
    EditBtn: TButton;
    Parts: TTable;
    PartsPartNo: TFloatField;
    PartsDescription: TStringField;
    PartsVendorNo: TFloatField;
    PartsOnHand: TFloatField;
    PartsOnOrder: TFloatField;
    PartsBackOrd: TBooleanField;
    PartsCost: TCurrencyField;
    PartsListPrice: TCurrencyField;
    PartsSource: TDataSource;
    PartsQuery: TQuery;
    PartsQueryPartNo: TFloatField;
    PartsQueryDescription: TStringField;
    PartsQueryVendorNo: TFloatField;
    PartsQueryOnHand: TFloatField;
    PartsQueryOnOrder: TFloatField;
    PartsQueryBackOrd: TBooleanField;
    PartsQueryCost: TCurrencyField;
    PartsQueryListPrice: TCurrencyField;
    procedure ActivateBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure PartsCalcFields(DataSet: TDataSet);
    procedure PartsQueryCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
    FEvents: IBrPartsFrmEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(const Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(var Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TBrPartsFrm }

procedure TBrPartsFrm.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_BrPartsFrmPage); }
end;

procedure TBrPartsFrm.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IBrPartsFrmEvents;
end;

procedure TBrPartsFrm.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TBrPartsFrm.Get_Active: WordBool;
begin
  Result := Active;
end;

function TBrPartsFrm.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TBrPartsFrm.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TBrPartsFrm.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TBrPartsFrm.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TBrPartsFrm.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TBrPartsFrm.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TBrPartsFrm.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TBrPartsFrm.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TBrPartsFrm.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TBrPartsFrm.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TBrPartsFrm.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TBrPartsFrm.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TBrPartsFrm.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TBrPartsFrm.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TBrPartsFrm.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TBrPartsFrm.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TBrPartsFrm.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TBrPartsFrm._Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TBrPartsFrm.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TBrPartsFrm.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TBrPartsFrm.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TBrPartsFrm.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TBrPartsFrm.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TBrPartsFrm.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TBrPartsFrm.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TBrPartsFrm.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TBrPartsFrm.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TBrPartsFrm.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TBrPartsFrm.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TBrPartsFrm.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TBrPartsFrm.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TBrPartsFrm.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TBrPartsFrm.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TBrPartsFrm.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TBrPartsFrm.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TBrPartsFrm.Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TBrPartsFrm.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TBrPartsFrm.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TBrPartsFrm.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TBrPartsFrm.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TBrPartsFrm.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TBrPartsFrm.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TBrPartsFrm.ActivateBtnClick(Sender: TObject);
begin
  if not ActivateBtn.Down then
    PartsSource.Dataset := Parts
  else
    try
      PartsQuery.Close;
      PartsQuery.Open;
      PartsSource.Dataset := PartsQuery;
    except
      PartsSource.Dataset := Parts;
      raise;
    end;
end;

procedure TBrPartsFrm.EditBtnClick(Sender: TObject);
begin
 Application.CreateForm(TEdPartsForm, EdPartsForm);
 try
   EdPartsForm.PartsSource1.DataSet := Parts;
   if ActivateBtn.Down then
    begin
      EdPartsForm.Edit(PartsQueryPartNo.Value);
      PartsQuery.Close;
      PartsQuery.Open;
    end else
      EdPartsForm.Edit(PartsPartNo.Value);
 finally
   EdPartsForm.Free;
 end;
end;

procedure TBrPartsFrm.PartsCalcFields(DataSet: TDataSet);
begin
  PartsBackOrd.Value := PartsOnOrder.Value > PartsOnHand.Value;
end;

procedure TBrPartsFrm.PartsQueryCalcFields(DataSet: TDataSet);
begin
  PartsQueryBackOrd.Value := PartsOnOrder.Value > PartsOnHand.Value;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TBrPartsFrm,
    Class_BrPartsFrm,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
