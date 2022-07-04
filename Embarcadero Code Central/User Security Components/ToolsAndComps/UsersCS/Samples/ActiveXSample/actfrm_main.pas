unit actfrm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ActiveFormProj_TLB, StdCtrls, users_basic, users_cs;

type
  TActiveFormTeste = class(TActiveForm, IActiveFormX)
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    Button1: TButton;
  private
    { Private declarations }
    FEvents: IActiveFormXEvents;
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
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure Initialize; override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: Integer; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_WindowState: TxWindowState; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: Integer); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure Set_WindowState(Value: TxWindowState); safecall;
  public
    { Public declarations }
  end;

implementation

uses ComServ;

{$R *.DFM}

{ TActiveFormX }

procedure TActiveFormTeste.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IActiveFormXEvents;
end;

procedure TActiveFormTeste.Initialize;
begin
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TActiveFormTeste.Get_Active: WordBool;
begin
  Result := Active;
end;

function TActiveFormTeste.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TActiveFormTeste.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TActiveFormTeste.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TActiveFormTeste.Get_Color: Integer;
begin
  Result := Integer(Color);
end;

function TActiveFormTeste.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TActiveFormTeste.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TActiveFormTeste.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TActiveFormTeste.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TActiveFormTeste.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TActiveFormTeste.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TActiveFormTeste.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TActiveFormTeste.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TActiveFormTeste.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TActiveFormTeste.Get_WindowState: TxWindowState;
begin
  Result := Ord(WindowState);
end;

procedure TActiveFormTeste.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TActiveFormTeste.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TActiveFormTeste.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TActiveFormTeste.Set_Color(Value: Integer);
begin
  Color := TColor(Value);
end;

procedure TActiveFormTeste.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TActiveFormTeste.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TActiveFormTeste.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TActiveFormTeste.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TActiveFormTeste.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TActiveFormTeste.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TActiveFormTeste.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TActiveFormTeste.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TActiveFormTeste.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TActiveFormTeste.Set_WindowState(Value: TxWindowState);
begin
  WindowState := TWindowState(Value);
end;

procedure TActiveFormTeste.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TActiveFormTeste.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TActiveFormTeste.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TActiveFormTeste.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TActiveFormTeste.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TActiveFormTeste.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TActiveFormTeste.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TActiveFormTeste.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TActiveFormTeste,
    Class_ActiveFormX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL);
end.
