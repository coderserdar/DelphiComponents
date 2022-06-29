unit LeEfctEd;

{
  This unit implements a property editor for the TLabelEffect
  component TEffectOption type.
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
  LblEffct, Controls, StdCtrls, Buttons, ExtCtrls;

type
  { The property editor for the TEffectOption type }
  TEffectOptionPropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { The form to be displayed when choosing a TEffectOption value }
  TEffectOptionPropEd = class(TForm)
    lefNormal: TLabelEffect;
    lefExtruded: TLabelEffect;
    lefGraduated: TLabelEffect;
    lefReal: TLabelEffect;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Shape1: TShape;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabelEffectClick(Sender: TObject);
    procedure LabelEffectDblClick(Sender: TObject);
  private
    FEffectOption: TEffectOption;
    procedure SetEffectOption(eoOption: TEffectOption);
  public
    { Declare a public property to allow communication with the form }
    property EffectOption: TEffectOption read FEffectOption write SetEffectOption
      default eoNormal;
    constructor Create(AOwner:TComponent); override;
  end;

implementation

{$R *.DFM}

{ Tell Delphi that we now have a dialog box for editing as well }
function TEffectOptionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList];
end;

{ The procedure invoked when the property is double-clicked }
procedure TEffectOptionPropertyEditor.Edit;
var
  EffectOptionPropEd: TEffectOptionPropEd;
begin
  { Create a dialog box as defined above }
  EffectOptionPropEd := TEffectOptionPropEd.Create(Application);
  try
    { Initialise with the current style }
    EffectOptionPropEd.EffectOption := TEffectOption(GetOrdValue);
    if EffectOptionPropEd.ShowModal = mrOK then
      { If OK then update with the new value }
      SetOrdValue(Ord(EffectOptionPropEd.EffectOption));
  finally
    EffectOptionPropEd.Free;    { Tidy up }
  end;
end;

{ Initialise default values for internal fields }
constructor TEffectOptionPropEd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEffectOption := eoNormal;
end;

{ Set the EffectOption property of the form and highlight appropriate label }
procedure TEffectOptionPropEd.SetEffectOption(eoOption: TEffectOption);
var
  iIndex: Integer;
begin
  FEffectOption := eoOption;
  for iIndex := 0 to ComponentCount -1 do
    if Components[iIndex] is TLabelEffect then
      if TLabelEffect(Components[iIndex]).StyleShadow = eoOption then
        TLabelEffect(Components[iIndex]).Transparent := False
      else
        TLabelEffect(Components[iIndex]).Transparent := True;
end;

{ Respond to keystrokes for selection }
procedure TEffectOptionPropEd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('E'): LabelEffectClick(lefExtruded);
    Ord('G'): LabelEffectClick(lefGraduated);
    Ord('N'): LabelEffectClick(lefNormal);
    Ord('R'): LabelEffectClick(lefReal);
  end;
end;

{ Set the form's EffectOption property based on the value of the label selected }
procedure TEffectOptionPropEd.LabelEffectClick(Sender: TObject);
begin
  EffectOption := (Sender as TLabelEffect).StyleShadow;
end;

{ As above and exit }
procedure TEffectOptionPropEd.LabelEffectDblClick(Sender: TObject);
begin
  btnOK.Click;
end;

end.
