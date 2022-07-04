unit AxFrms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, StdVcl, ComObj;

type
  TStandardActiveForm = class(TActiveForm, IUnknown, IDispatch)
  private
    // FRefCount: Integer;
  protected
    { Active Form }
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
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
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(const Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
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
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    {function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;}
  end;

implementation

uses
  SysConst;

{ TStandardActiveForm }

function TStandardActiveForm.Get_Active: WordBool;
begin
  Result := Active;
end;

function TStandardActiveForm.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TStandardActiveForm.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TStandardActiveForm.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TStandardActiveForm.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TStandardActiveForm.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TStandardActiveForm.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TStandardActiveForm.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TStandardActiveForm.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TStandardActiveForm.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TStandardActiveForm.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TStandardActiveForm.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TStandardActiveForm.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TStandardActiveForm.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TStandardActiveForm.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TStandardActiveForm.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TStandardActiveForm._Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TStandardActiveForm.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TStandardActiveForm.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TStandardActiveForm.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TStandardActiveForm.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TStandardActiveForm.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TStandardActiveForm.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TStandardActiveForm.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TStandardActiveForm.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TStandardActiveForm.Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TStandardActiveForm.Set_HelpFile(const Value: WideString);
begin
  HelpFile := string(Value);
end;

procedure TStandardActiveForm.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TStandardActiveForm.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TStandardActiveForm.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TStandardActiveForm.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

{ TStandardActiveForm.IUnknown }

function TStandardActiveForm.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

{function TStandardActiveForm._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TStandardActiveForm._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;}

end.
