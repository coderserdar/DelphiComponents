unit LeColrEd;

{
  This unit implements a property editor for the TLabelEffect
  component TColourScheme type.
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
  { The property editor for the ColourScheme property }
  TColourSchemePropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { The form to be displayed when choosing the value of the TColourScheme type }
  TColourSchemePropEd = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lefText: TLabelEffect;
    lefWindows: TLabelEffect;
    lefEmbossed: TLabelEffect;
    lefGold: TLabelEffect;
    lefSteel: TLabelEffect;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabelEffectClick(Sender: TObject);
    procedure LabelEffectDblClick(Sender: TObject);
  private
    FColourScheme: TColourScheme;
    procedure SetColourScheme(csScheme: TColourScheme);
  public
    { Declare a public property to allow communication with the form }
    property ColourScheme: TColourScheme read FColourScheme write SetColourScheme
      default csCustom;
    constructor Create(AOwner:TComponent); override;
  end;

implementation

{$R *.DFM}

{ Tell Delphi that we now have a dialog box for editing as well }
function TColourSchemePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList];
end;

{ The procedure invoked when the property is double-clicked }
procedure TColourSchemePropertyEditor.Edit;
var
  ColourSchemePropEd: TColourSchemePropEd;
begin
  { Create a dialog box as defined above }
  ColourSchemePropEd := TColourSchemePropEd.Create(Application);
  try
    { Initialise with the current style }
    ColourSchemePropEd.ColourScheme := TColourScheme(GetOrdValue);
    if ColourSchemePropEd.ShowModal = mrOK then
      { If OK then update with the new value }
      SetOrdValue(Ord(ColourSchemePropEd.ColourScheme));
  finally
    ColourSchemePropEd.Free;    { Tidy up }
  end;
end;

{ Initialise default values for internal fields }
constructor TColourSchemePropEd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColourScheme := csCustom;
end;

{ Set the ColourScheme property of the form and highlight appropriate label }
procedure TColourSchemePropEd.SetColourScheme(csScheme: TColourScheme);
var
  iIndex: Integer;
begin
  if FColourScheme <> csScheme then
  begin
    FColourScheme := csScheme;
    for iIndex := 0 to ComponentCount -1 do
      if Components[iIndex] is TLabelEffect then
        if TLabelEffect(Components[iIndex]).ColourScheme = csScheme then
          TLabelEffect(Components[iIndex]).Transparent := False
        else
          TLabelEffect(Components[iIndex]).Transparent := True;
  end;
end;

{ Respond to keystrokes for selection }
procedure TColourSchemePropEd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('E'): LabelEffectClick(lefEmbossed);
    Ord('G'): LabelEffectClick(lefGold);
    Ord('S'): LabelEffectClick(lefSteel);
    Ord('T'): LabelEffectClick(lefText);
    Ord('W'): LabelEffectClick(lefWindows);
  end;
end;

{ Set the form's ColourScheme property based on the value of the label selected }
procedure TColourSchemePropEd.LabelEffectClick(Sender: TObject);
begin
  ColourScheme := (Sender as TLabelEffect).ColourScheme;
end;

{ As above and exit }
procedure TColourSchemePropEd.LabelEffectDblClick(Sender: TObject);
begin
  btnOK.Click;
end;

end.
