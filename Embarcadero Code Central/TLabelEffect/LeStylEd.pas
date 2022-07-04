unit LeStylEd;

{
  This unit implements a property editor for the TLabelEffect
  component TEffectStyle type.
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
  { The property editor for the TEffectStyle type }
  TEffectStylePropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { The form to be displayed when choosing a TEffectStyle value }
  TEffectStylePropEd = class(TForm)
    lefRaised: TLabelEffect;
    lefSunken: TLabelEffect;
    lefSHadow: TLabelEffect;
    lefFlying: TLabelEffect;
    lefNone: TLabelEffect;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabelEffectClick(Sender: TObject);
    procedure LabelEffectDblClick(Sender: TObject);
  private
    FEffectStyle: TEffectStyle;
    procedure SetEffectStyle(esStyle: TEffectStyle);
  public
    { Declare a public property to allow communication with the form }
    property EffectStyle: TEffectStyle read FEffectStyle write SetEffectStyle
      default esCustom;
    constructor Create(AOwner:TComponent); override;
  end;

implementation

{$R *.DFM}

{ Tell Delphi that we now have a dialog box for editing as well }
function TEffectStylePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList];
end;

{ The procedure invoked when the property is double-clicked }
procedure TEffectStylePropertyEditor.Edit;
var
  EffectStylePropEd: TEffectStylePropEd;
begin
  { Create a dialog box as defined above }
  EffectStylePropEd := TEffectStylePropEd.Create(Application);
  try
    { Initialise with the current style }
    EffectStylePropEd.EffectStyle := TEffectStyle(GetOrdValue);
    if EffectStylePropEd.ShowModal = mrOK then
      { If OK then update with the new value }
      SetOrdValue(Ord(EffectStylePropEd.EffectStyle));
  finally
    EffectStylePropEd.Free;    { Tidy up }
  end;
end;

{ Initialise default values for internal fields }
constructor TEffectStylePropEd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEffectStyle := esCustom;
end;

{ Set the EffectStyle property of the form and highlight appropriate label }
procedure TEffectStylePropEd.SetEffectStyle(esStyle: TEffectStyle);
var
  iIndex: Integer;
begin
  if FEffectStyle <> esStyle then
  begin
    FEffectStyle := esStyle;
    for iIndex := 0 to ComponentCount -1 do
      if Components[iIndex] is TLabelEffect then
        if TLabelEffect(Components[iIndex]).EffectStyle = esStyle then
          TLabelEffect(Components[iIndex]).Transparent := False
        else
          TLabelEffect(Components[iIndex]).Transparent := True;
  end;
end;

{ Respond to keystrokes for selection }
procedure TEffectStylePropEd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('F'): LabelEffectClick(lefFlying);
    Ord('H'): LabelEffectClick(lefShadow);
    Ord('N'): LabelEffectClick(lefNone);
    Ord('R'): LabelEffectClick(lefRaised);
    Ord('S'): LabelEffectClick(lefSunken);
  end;
end;

{ Set the form's EffectStyle property based on the value of the label selected }
procedure TEffectStylePropEd.LabelEffectClick(Sender: TObject);
begin
  EffectStyle := (Sender as TLabelEffect).EffectStyle;
end;

{ As above and exit }
procedure TEffectStylePropEd.LabelEffectDblClick(Sender: TObject);
begin
  btnOK.Click;
end;

end.
