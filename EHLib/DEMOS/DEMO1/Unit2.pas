unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF VER140} Variants, {$ENDIF}
{$IFDEF CIL}
  Types, System.ComponentModel, Variants,
{$ELSE}
{$ENDIF}
  Grids, DBGridEh, Buttons, Db, DBTables, ExtCtrls, ComCtrls;

type
  TForm2 = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    Panel2: TPanel;
    DBGridEh1: TDBGridEh;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure DBGridEh1DblClick(Sender: TObject);
    procedure DBGridEh1ColWidthsChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure OnActivate(var msg: TWMActivate); message WM_ACTIVATE;
    { Private declarations }
  public
    function Execute(HostControl: TControl; var VendorNo: String; var VendorName: String):Boolean;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

{ TForm2 }

procedure AdjustDropDownForm(AControl : TControl; HostControl: TControl);
var
   WorkArea: TRect;
   HostP, PDelpta: TPoint;
begin
{$IFDEF CIL}
   SystemParametersInfo(SPI_GETWORKAREA,0,WorkArea,0);
{$ELSE}
   SystemParametersInfo(SPI_GETWORKAREA,0,@WorkArea,0);
{$ENDIF}
   HostP := HostControl.ClientToScreen(Point(0,0));
   PDelpta := AControl.ClientToScreen(Point(0,0));

   AControl.Left := HostP.x;
   AControl.Top := HostP.y + HostControl.Height + 1;

   if (AControl.Width > WorkArea.Right - WorkArea.Left) then
     AControl.Width := WorkArea.Right - WorkArea.Left;

   if (AControl.Left + AControl.Width > WorkArea.Right) then
     AControl.Left := WorkArea.Right - AControl.Width;
   if (AControl.Left < WorkArea.Left) then
     AControl.Left := WorkArea.Left;


   if (AControl.Top + AControl.Height > WorkArea.Bottom) then
   begin
     if (HostP.y - WorkArea.Top > WorkArea.Bottom - HostP.y - HostControl.Height) then
       AControl.Top := HostP.y - AControl.Height;
   end;

   if (AControl.Top < WorkArea.Top) then
   begin
     AControl.Height := AControl.Height - (WorkArea.Top - AControl.Top);
     AControl.Top := WorkArea.Top;
   end;

   if (AControl.Top + AControl.Height > WorkArea.Bottom) then
   begin
     AControl.Height := WorkArea.Bottom - AControl.Top;
   end;

end;

function TForm2.Execute(HostControl: TControl; var VendorNo: String; var VendorName: String):Boolean;
var
  VNo: Variant;
begin
  VNo := VendorNo;
  if VendorNo <> '' then
    Table1.Locate('VendorNo',VNo,[]);

  AdjustDropDownForm(Self,HostControl);
  Visible := True;
  ModalResult := mrCancel;
  while (Visible) do Application.ProcessMessages;
  Result := False;
  if ModalResult = mrOk then
  begin
    VendorNo := Table1.FieldByName('VendorNo').AsString;
    VendorName := Table1.FieldByName('VendorName').AsString;
    Result := True;
  end;
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TForm2.SpeedButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.DBGridEh1DblClick(Sender: TObject);
begin
  SpeedButton1Click(Sender);
end;

procedure TForm2.OnActivate(var msg: TWMActivate);
begin
  inherited;
  if (msg.Active=WA_INACTIVE) then
    Close;
end;

procedure TForm2.DBGridEh1ColWidthsChanged(Sender: TObject);
begin
  ClientWidth := DBGridEh1.Columns[0].Width + DBGridEh1.Columns[1].Width +
  (DBGridEh1.Width-DBGridEh1.ClientWidth) + Panel1.Width + 3;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  DBGridEh1ColWidthsChanged(Sender);
end;

end.
