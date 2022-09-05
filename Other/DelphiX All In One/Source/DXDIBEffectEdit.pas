unit DXDIBEffectEdit;
//(c)2007 Jaro Benes
//All Rights Reserved

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) create effect for store into dximagelist.

}
interface

{$INCLUDE DelphiXcfg.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DIB, ExtCtrls, Math, ComCtrls, Buttons, ExtDlgs, Spin;

type
  {injected class}
  EInvalidTypeConvert = class(Exception);
  TEdit = class(StdCtrls.TEdit)
  private
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;
  TTDelphiXDIBEffectEditForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    LSpokes: TLabel;
    LRaHUE: TLabel;
    LCentr: TLabel;
    eR: TEdit;
    eG: TEdit;
    eB: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    randSpok: TEdit;
    Label7: TLabel;
    RandGauss: TEdit;
    Label8: TLabel;
    rS_max: TSpeedButton;
    rG_max: TSpeedButton;
    Bevel1: TBevel;
    PictSize: TRadioGroup;
    SavePictureDialog1: TSavePictureDialog;
    Bevel2: TBevel;
    R_updown: TSpinButton;
    G_updown: TSpinButton;
    B_updown: TSpinButton;
    btnOK: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Bevel3: TBevel;
    Spokes: TProgressBar;
    RaHUE: TProgressBar;
    Centr: TProgressBar;
    SpokesPlus: TSpeedButton;
    SpokesMinus: TSpeedButton;
    RaHUEPlus: TSpeedButton;
    RaHUEMinus: TSpeedButton;
    CentrPlus: TSpeedButton;
    CentrMinus: TSpeedButton;
    LName: TLabel;
    eName: TEdit;
    procedure CentrMinusClick(Sender: TObject);
    procedure CentrPlusClick(Sender: TObject);
    procedure RaHUEMinusClick(Sender: TObject);
    procedure RaHUEPlusClick(Sender: TObject);
    procedure SpokesMinusClick(Sender: TObject);
    procedure SpokesPlusClick(Sender: TObject);
    procedure CentrMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RaHUEMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpokesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ImageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rS_maxClick(Sender: TObject);
    procedure rG_maxClick(Sender: TObject);
    procedure R_updownUpClick(Sender: TObject);
    procedure R_updownDownClick(Sender: TObject);
    procedure G_updownUpClick(Sender: TObject);
    procedure G_updownDownClick(Sender: TObject);
    procedure B_updownUpClick(Sender: TObject);
    procedure B_updownDownClick(Sender: TObject);
  private
    { Private declarations }
    FSpokes: Integer;
    FRaHUE: Integer;
    FCentr: Integer;
    procedure Calculate;
  public
    { Public declarations }
    ResultDIB: TDIB;
  end;

var
  DelphiXDIBEffectEditForm: TTDelphiXDIBEffectEditForm;

implementation

{$R *.DFM}

{ TEdit }

{injected class functionality like in jbEdit.TPubEdit component}

procedure TEdit.SetAsInteger(Value: Integer);
begin
  Text := IntToStr(Value)
end;

function TEdit.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create('Expected integer value !');
  end
end;

{  TTDelphiXDIBEffectEditForm  }

procedure TTDelphiXDIBEffectEditForm.Calculate;
const
  wSize: array[0..5] of Word = (256, 128, 64, 32, 16, 8);
begin
// call this
// cx,cy : center x and y
// radius : 1-100
// spokes : 1-1024
// randomhue : 0-360
  ResultDIB.Clear;
  ResultDIB.SetSize(wSize[PictSize.ItemIndex], wSize[PictSize.ItemIndex], 24);
  ResultDIB.Canvas.Brush.Color := clBlack;
  ResultDIB.Canvas.FillRect(ResultDIB.Canvas.ClipRect);

  LSpokes.Caption := Format('Spokes (%d)', [FSpokes]);
  LRaHUE.Caption := Format('Random HUE (%d)', [FRaHUE]);
  LCentr.Caption := Format('Nova radius (%d)', [FCentr]);

  ResultDIB.DoNovaEffect(eR.AsInteger, eG.AsInteger, eB.AsInteger, ResultDIB.Width div 2,
    ResultDIB.Height div 2, FCentr, FSpokes, FRaHUE,
    randSpok.AsInteger, randGauss.AsInteger, nil);

  Image1.Picture.Assign(ResultDIB);
end;

procedure TTDelphiXDIBEffectEditForm.Button1Click(Sender: TObject);
begin
  Calculate;
  if SavePictureDialog1.Execute then
    ResultDIB.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TTDelphiXDIBEffectEditForm.FormCreate(Sender: TObject);
begin
  Tag := 0;
  ResultDIB := TDIB.Create;
  Randomize;
  randspok.AsInteger := Random(MAXSHORT);
  randgauss.AsInteger := Random(MAXSHORT);
  FSpokes := 255; Spokes.Position := FSpokes; {$IFDEF VER4UP} Spokes.Smooth := True; {$ENDIF}
  FRaHUE := 1; RaHUE.Position := FRaHUE; {$IFDEF VER4UP} RaHUE.Smooth := True; {$ENDIF}
  FCentr := 18; Centr.Position := FCentr; {$IFDEF VER4UP} Centr.Smooth := True; {$ENDIF}
  LSpokes.Caption := Format('Spokes (%d)', [FSpokes]);
  LRaHUE.Caption := Format('Random HUE (%d)', [FRaHUE]);
  LCentr.Caption := Format('Nova radius (%d)', [FCentr]);
  Calculate;
  randSpok.OnChange := ImageChange;
  randgauss.OnChange := ImageChange;
  {$IFDEF VER4UP}
  Spokes.Smooth := True;
  RaHUE.Smooth := True;
  Centr.Smooth := True;
  {$ENDIF}
end;

procedure TTDelphiXDIBEffectEditForm.rS_maxClick(Sender: TObject);
begin
  RandSpok.AsInteger := maxint;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.rG_maxClick(Sender: TObject);
begin
  RandGauss.AsInteger := maxint;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.R_updownUpClick(Sender: TObject);
begin
  if eR.AsInteger < 255 then eR.AsInteger := eR.AsInteger + 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.R_updownDownClick(Sender: TObject);
begin
  if eR.AsInteger > 0 then eR.AsInteger := eR.AsInteger - 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.G_updownUpClick(Sender: TObject);
begin
  if eG.AsInteger < 255 then eG.AsInteger := eG.AsInteger + 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.G_updownDownClick(Sender: TObject);
begin
  if eG.AsInteger > 0 then eG.AsInteger := eG.AsInteger - 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.B_updownUpClick(Sender: TObject);
begin
  if eB.AsInteger < 255 then eB.AsInteger := eB.AsInteger + 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.B_updownDownClick(Sender: TObject);
begin
  if eB.AsInteger > 0 then eB.AsInteger := eB.AsInteger - 1;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTDelphiXDIBEffectEditForm.btnOKClick(Sender: TObject);
begin
  if eName.Text <> '' then begin
    Tag := 1;
    Close;
  end
  else
    MessageDlg('Image name required!', mtWarning, [mbOK], 0);;
end;

procedure TTDelphiXDIBEffectEditForm.FormDestroy(Sender: TObject);
begin
  ResultDIB.Free;
end;

procedure TTDelphiXDIBEffectEditForm.ImageChange(Sender: TObject);
begin
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.SpokesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  newPosition: integer;
begin
  with Sender as TProgressBar do begin
    if ssLeft in Shift then
    begin
      Cursor := crHSplit;
      newPosition := Round(x * Max / ClientWidth);
      Position := newPosition;
    end
    else
    begin
      Cursor := crDefault;
    end;
    FSpokes := Position;
    LSpokes.Caption := Format('Spokes (%d)', [FSpokes]);
    if ssLeft in Shift then Calculate;
  end;
end;

procedure TTDelphiXDIBEffectEditForm.RaHUEMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  newPosition: integer;
begin
  with Sender as TProgressBar do begin
    if ssLeft in Shift then
    begin
      Cursor := crHSplit;
      newPosition := Round(x * Max / ClientWidth);
      Position := newPosition;
    end
    else
    begin
      Cursor := crDefault;
    end;
    FRaHUE := Position;
    LRaHUE.Caption := Format('Random HUE (%d)', [FRaHUE]);
    if ssLeft in Shift then Calculate;
  end;
end;

procedure TTDelphiXDIBEffectEditForm.CentrMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  newPosition: integer;
begin
  with Sender as TProgressBar do begin
    if ssLeft in Shift then
    begin
      Cursor := crHSplit;
      newPosition := Round(x * Max / ClientWidth);
      Position := newPosition;
    end
    else
    begin
      Cursor := crDefault;
    end;
    FCentr := Position;
    LCentr.Caption := Format('Nova radius (%d)', [FCentr]);
    if ssLeft in Shift then Calculate;
  end;
end;

procedure TTDelphiXDIBEffectEditForm.SpokesPlusClick(Sender: TObject);
begin
  Inc(FSpokes); if FSpokes > 1024 then FSpokes := 1024;
  Spokes.Position := FSpokes;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.SpokesMinusClick(Sender: TObject);
begin
  Dec(FSpokes); if FSpokes < 1 then FSpokes := 1;
  Spokes.Position := FSpokes;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.RaHUEPlusClick(Sender: TObject);
begin
  Inc(FRaHUE); if FRaHUE > 360 then FRaHUE := 360;
  RaHUE.Position := FRaHUE;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.RaHUEMinusClick(Sender: TObject);
begin
  Dec(FRaHUE); if FRaHUE < 0 then FRaHUE := 0;
  RaHUE.Position := FRaHUE;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.CentrPlusClick(Sender: TObject);
begin
  Inc(FCentr); if FCentr > 100 then FCentr := 100;
  Centr.Position := FCentr;
  Calculate;
end;

procedure TTDelphiXDIBEffectEditForm.CentrMinusClick(Sender: TObject);
begin
  Dec(FCentr); if FCentr < 1 then FCentr := 1;
  Centr.Position := FCentr;
  Calculate;
end;

end.