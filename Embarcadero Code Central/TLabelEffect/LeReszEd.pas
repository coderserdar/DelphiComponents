unit LeReszEd;

{
  This unit implements a property editor for the TLabelEffect
  component TResizeOption type.
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
  { The property editor for the TResizeOption type }
  TResizeOptionPropertyEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { The form to be displayed when choosing a TResizeOption value }
  TResizeOptionPropEd = class(TForm)
    lefNone: TLabelEffect;
    lefExpand: TLabelEffect;
    lefReduce: TLabelEffect;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure LabelEffectClick(Sender: TObject);
    procedure LabelEffectDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FResizeOption: TResizeOption;
    procedure SetResizeOption(rsOption: TResizeOption);
  public
    { Declare a public property to allow communication with the form }
    property ResizeOption: TResizeOption read FResizeOption
      write SetResizeOption default LblEffct.rsNone;
    constructor Create(AOwner:TComponent); override;
  end;

implementation

{$R *.DFM}

{ Tell Delphi that we now have a dialog box for editing as well }
function TResizeOptionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList];
end;

{ The procedure invoked when the property is double-clicked }
procedure TResizeOptionPropertyEditor.Edit;
var
  ResizeOptionPropEd: TResizeOptionPropEd;
begin
  { Create a dialog box as defined above }
  ResizeOptionPropEd := TResizeOptionPropEd.Create(Application);
  try
    { Initialise with the current style }
    ResizeOptionPropEd.ResizeOption := TResizeOption(GetOrdValue);
    if ResizeOptionPropEd.ShowModal = mrOK then
      { If OK then update with the new value }
      SetOrdValue(Ord(ResizeOptionPropEd.ResizeOption));
  finally
    ResizeOptionPropEd.Free;    { Tidy up }
  end;
end;

{ Initialise default values for internal fields }
constructor TResizeOptionPropEd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResizeOption := LblEffct.rsNone;
end;

{ Set the ResizeOption property of the form and highlight appropriate label }
procedure TResizeOptionPropEd.SetResizeOption(rsOption: TResizeOption);
var
  iIndex: Integer;
begin
  FResizeOption := rsOption;
  for iIndex := 0 to ComponentCount -1 do
    if Components[iIndex] is TLabelEffect then
      if TLabelEffect(Components[iIndex]).ResizeHighlight = rsOption then
        TLabelEffect(Components[iIndex]).Transparent := False
      else
        TLabelEffect(Components[iIndex]).Transparent := True;
end;

{ Respond to keystrokes for selection }
procedure TResizeOptionPropEd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('E'): LabelEffectClick(lefExpand);
    Ord('N'): LabelEffectClick(lefNone);
    Ord('R'): LabelEffectClick(lefReduce);
  end;
end;

{ Set the form's ResizeOption property based on the value of the label selected }
procedure TResizeOptionPropEd.LabelEffectClick(Sender: TObject);
begin
  ResizeOption := (Sender as TLabelEffect).ResizeHighlight;
end;

{ As above and exit }
procedure TResizeOptionPropEd.LabelEffectDblClick(Sender: TObject);
begin
  btnOK.Click;
end;

end.
