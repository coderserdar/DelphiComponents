unit LeGradEd;

{
  This unit implements a property editor for the TLabelEffect
  component TGraduateOption type.
  Written by Keith Wood - 17 Feb 1996.
}

interface

uses
  Classes, Forms,
{$IFDEF VER140}  { Delphi 6 }
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  LblEffct, Controls, StdCtrls, Buttons;

type
  { The property editor for the TGraduateOption type }
  TGraduateOptionPropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { The form to be displayed when choosing a TGraduateOption value }
  TGraduateOptionPropEd = class(TForm)
    lefNone: TLabelEffect;
    lefVertical: TLabelEffect;
    lefHorizontal: TLabelEffect;
    lefFDiagonal: TLabelEffect;
    lefBDiagonal: TLabelEffect;
    lefBoxed: TLabelEffect;
    lefRIndented: TLabelEffect;
    lefLIndented: TLabelEffect;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabelEffectClick(Sender: TObject);
    procedure LabelEffectDblClick(Sender: TObject);
  private
    FGraduateOption: TGraduateOption;
    procedure SetGraduateOption(goOption: TGraduateOption);
  public
    { Declare a public property to allow communication with the form }
    property GraduateOption: TGraduateOption read FGraduateOption
      write SetGraduateOption default goNone;
    constructor Create(AOwner:TComponent); override;
  end;

implementation

{$R *.DFM}

{ Tell Delphi that we now have a dialog box for editing as well }
function TGraduateOptionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList];
end;

{ The procedure invoked when the property is double-clicked }
procedure TGraduateOptionPropertyEditor.Edit;
var
  GraduateOptionPropEd: TGraduateOptionPropEd;
begin
  { Create a dialog box as defined above }
  GraduateOptionPropEd := TGraduateOptionPropEd.Create(Application);
  try
    { Initialise with the current style }
    GraduateOptionPropEd.GraduateOption := TGraduateOption(GetOrdValue);
    if GraduateOptionPropEd.ShowModal = mrOK then
      { If OK then update with the new value }
      SetOrdValue(Ord(GraduateOptionPropEd.GraduateOption));
  finally
    GraduateOptionPropEd.Free;    { Tidy up }
  end;
end;

{ Initialise default values for internal fields }
constructor TGraduateOptionPropEd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraduateOption := goNone;
end;

{ Set the GraduateOption property of the form and highlight appropriate label }
procedure TGraduateOptionPropEd.SetGraduateOption(goOption: TGraduateOption);
var
  iIndex: Integer;
begin
  FGraduateOption := goOption;
  for iIndex := 0 to ComponentCount -1 do
    if Components[iIndex] is TLabelEffect then
      if TLabelEffect(Components[iIndex]).GraduateFace = goOption then
        TLabelEffect(Components[iIndex]).Transparent := False
      else
        TLabelEffect(Components[iIndex]).Transparent := True;
end;

{ Respond to keystrokes for selection }
procedure TGraduateOptionPropEd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('B'): LabelEffectClick(lefBDiagonal);
    Ord('F'): LabelEffectClick(lefFDiagonal);
    Ord('H'): LabelEffectClick(lefHorizontal);
    Ord('L'): LabelEffectClick(lefLIndented);
    Ord('N'): LabelEffectClick(lefNone);
    Ord('R'): LabelEffectClick(lefRIndented);
    Ord('V'): LabelEffectClick(lefVertical);
    Ord('X'): LabelEffectClick(lefBoxed);
  end;
end;

{ Set the form's GraduateOption property based on the value of the label selected }
procedure TGraduateOptionPropEd.LabelEffectClick(Sender: TObject);
begin
  GraduateOption := (Sender as TLabelEffect).GraduateFace;
end;

{ As above and exit }
procedure TGraduateOptionPropEd.LabelEffectDblClick(Sender: TObject);
begin
  btnOK.Click;
end;

end.
