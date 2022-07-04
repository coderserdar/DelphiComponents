
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            OLE Add-In Object            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_OLE;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtnrs, StdCtrls, ExtCtrls, FR_DBRel, FR_Class,
{$IFDEF Delphi2}
  Ole2;
{$ELSE}
  ActiveX;
{$ENDIF}


type
  TfrOLEObject = class(TComponent)  // fake component
  end;

  TfrOLEView = class(TfrView)
  protected
    procedure GetBlob(b: TfrTField); override;
  public
    OleContainer: TOleContainer;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure Resized; override;
  end;

  TfrOleForm = class(TfrObjEditorForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    OleContainer1: TOleContainer;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

uses FR_Intrp, FR_Utils, FR_Const;

{$R *.DFM}

var
  frOleForm: TfrOleForm;

procedure AssignOle(Cont1, Cont2: TOleContainer);
var
  st: TMemoryStream;
begin
  if Cont2.OleObjectInterface = nil then
  begin
    Cont1.DestroyObject;
    Exit;
  end;
  st := TMemoryStream.Create;
  Cont2.SaveToStream(st);
  st.Position := 0;
  Cont1.LoadFromStream(st);
  st.Free;
end;


{----------------------------------------------------------------------------}
constructor TfrOLEView.Create;
begin
  inherited Create;
  OleContainer := TOleContainer.Create(nil);
  with OleContainer do
  begin
    Parent := frOleForm;
    Visible := False;
    AllowInPlace := False;
    AutoVerbMenu := False;
    BorderStyle := bsNone;
    SizeMode := smClip;
  end;
  Flags := 1;
  BaseName := 'Ole';
  Typ := gtAddIn;
end;

destructor TfrOLEView.Destroy;
begin
  if frOleForm <> nil then OleContainer.Free;
  inherited Destroy;
end;

procedure TfrOLEView.Assign(From: TfrView);
begin
  inherited Assign(From);
  AssignOle(OleContainer,(From as TfrOLEView).OleContainer);
end;

procedure TfrOLEView.Draw(Canvas: TCanvas);
begin
  BeginDraw(Canvas);
  CalcGaps;
  OleContainer.Width := dx;
  OleContainer.Height := dy;
  with Canvas do
  begin
    ShowBackground;
    with OleContainer do
    if OleObjectInterface <> nil then
      OleDraw(OleObjectInterface, DVASPECT_CONTENT, Canvas.Handle, DRect)
    else
    begin
      Font.Name := 'Arial';
      Font.Size := 8;
      Font.Style := [];
      Font.Color := clBlack;
      TextOut(x + 2, y + 2, '[OLE]');
    end;
    ShowFrame;
  end;
  RestoreCoord;
end;

procedure TfrOLEView.LoadFromStream(Stream: TStream);
var
  b: Byte;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(b, 1);
  if b <> 0 then
    OleContainer.LoadFromStream(Stream);
end;

procedure TfrOLEView.SaveToStream(Stream: TStream);
var
  b: Byte;
begin
  inherited SaveToStream(Stream);
  b := 0;
  if OleContainer.OleObjectInterface <> nil then
  begin
    b := 1;
    Stream.Write(b, 1);
    OleContainer.SaveToStream(Stream);
  end
  else
    Stream.Write(b, 1);
end;

procedure TfrOLEView.Resized;
var
  VS: TPoint;
begin
  if (Flags and flStretched) = 0 then
    with OleContainer do
    if OleObjectInterface <> nil then
    begin
      Run;
      VS.X := MulDiv(dx, 2540, Screen.PixelsPerInch);
      VS.Y := MulDiv(dy, 2540, Screen.PixelsPerInch);
      OleObjectInterface.SetExtent(DVASPECT_CONTENT, VS);
    end;
end;

procedure TfrOLEView.GetBlob(b: TfrTField);
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
{$IFDEF IBO}
  TfrTBlobField(b).AssignTo(s);
{$ELSE}
  TfrTBlobField(b).SaveToStream(s);
{$ENDIF}
  s.Position := 0;
  OleContainer.LoadFromStream(s);
  s.Free;
end;


{----------------------------------------------------------------------------}
procedure TfrOleForm.ShowEditor(t: TfrView);
begin
  AssignOle(OleContainer1, (t as TfrOLEView).OleContainer);
  ShowModal;
  AssignOle((t as TfrOLEView).OleContainer, OleContainer1);
  OleContainer1.DestroyObject;
end;

procedure TfrOleForm.Button1Click(Sender: TObject);
begin
  with OleContainer1 do
    if InsertObjectDialog then
      DoVerb(PrimaryVerb);
end;

procedure TfrOleForm.Button2Click(Sender: TObject);
begin
  if OleContainer1.OleObjectInterface <> nil then
    OleContainer1.DoVerb(ovPrimary);
end;

procedure TfrOleForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 550);
  Button1.Caption := LoadStr(frRes + 551);
  Button2.Caption := LoadStr(frRes + 552);
  Button4.Caption := LoadStr(frRes + 553);
end;

initialization
  frOleForm := TfrOleForm.Create(nil);
  frRegisterObject(TfrOLEView, frOleForm.Image1.Picture.Bitmap,
    LoadStr(SInsOLEObject), frOleForm);

finalization
  frOleForm.Free;
  frOleForm := nil;

end.

