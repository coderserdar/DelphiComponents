unit ColorPresets;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TColorPreset = (cpUser, cpDefault, cpBlue, cpGreen, cpRed);
  TFontPreset = (fpUser, fpDefault, fpBlue);
  TOwnersList = array of TObject;


  TColorChangeEvent = procedure (Sender: TComponent; FieldName: String; var Value: TColor) of object;
  TFontChangeEvent = procedure (Sender: TComponent; FieldName: String; var Value: TFont) of object;

  TColorPresets = class(TComponent)
  private
    fColorActive1: TColor;
    fColorActive2: TColor;
    fColorInactive1: TColor;
    fColorInactive2: TColor;
    fColorSelectedActive: TColor;
    fColorSelectedInactive: TColor;
    fColorMultiSelectedActive1: TColor;
    fColorMultiSelectedActive2: TColor;
    fColorMultiSelectedInactive1: TColor;
    fColorMultiSelectedInactive2: TColor;
    fColorPreset: TColorPreset;

    fFontActive1: TFont;
    fFontActive2: TFont;
    fFontInactive1: TFont;
    fFontInactive2: TFont;
    fFontSelectedActive: TFont;
    fFontSelectedInactive: TFont;
    fFontMultiSelectedActive1: TFont;
    fFontMultiSelectedActive2: TFont;
    fFontMultiSelectedInactive1: TFont;
    fFontMultiSelectedInactive2: TFont;
    fFontPreset: TFontPreset;
    fFramePenInactive: TPen;
    fFramePenActive: TPen;
    fFrameOverride: boolean;
    fAfterColorChange: TColorChangeEvent;
    fAfterFontChange: TFontChangeEvent;
    fBeforeColorChange: TColorChangeEvent;
    fBeforeFontChange: TFontChangeEvent;
    fColorInplaceEditor: TColor;

    procedure SetColorPreset(const Value: TColorPreset);
    procedure SetFontPreset(const Value: TFontPreset);
    procedure SetNewColor(const Index: Integer; const Value: TColor);
    procedure SetNewFont(const Index: Integer; const Value: TFont);
    procedure SetNewPen(const Index: Integer; const Value: TPen);
    procedure SetFrameOverride(const Value: boolean);

  public
    MyOwners: array of TObject;
    procedure AddOwner(Sender: TObject);
    procedure DeleteOwner(Sender: TObject);
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateOwners;
  published
    property ColorActive1: TColor index 1 read fColorActive1 write SetNewColor;
    property ColorActive2: TColor index 2 read fColorActive2 write SetNewColor;
    property ColorInactive1: TColor index 3 read fColorInactive1 write SetNewColor;
    property ColorInactive2: TColor index 4 read fColorInactive2 write SetNewColor;
    property ColorSelectedActive: TColor index 5 read fColorSelectedActive write SetNewColor;
    property ColorSelectedInactive: TColor index 6 read fColorSelectedInactive write SetNewColor;
    property ColorMultiSelectedActive1: TColor index 7 read fColorMultiSelectedActive1 write SetNewColor;
    property ColorMultiSelectedActive2: TColor index 8 read fColorMultiSelectedActive2 write SetNewColor;
    property ColorMultiSelectedInactive1: TColor index 9 read fColorMultiSelectedInactive1 write SetNewColor;
    property ColorMultiSelectedInactive2: TColor index 10 read fColorMultiSelectedInactive2 write SetNewColor;
    property ColorInplaceEditor: TColor index 11 read fColorInplaceEditor write SetNewColor;

    property FontActive1: TFont index 1 read fFontActive1 write SetNewFont;
    property FontActive2: TFont index 2 read fFontActive2 write SetNewFont;
    property FontInactive1: TFont index 3 read fFontInactive1 write SetNewFont;
    property FontInactive2: TFont index 4 read fFontInactive2 write SetNewFont;
    property FontSelectedActive: TFont index 5 read fFontSelectedActive write SetNewFont;
    property FontSelectedInactive: TFont index 6 read fFontSelectedInactive write SetNewFont;
    property FontMultiSelectedActive1: TFont index 7 read fFontMultiSelectedActive1 write SetNewFont;
    property FontMultiSelectedActive2: TFont index 8 read fFontMultiSelectedActive2 write SetNewFont;
    property FontMultiSelectedInactive1: TFont index 9 read fFontMultiSelectedInactive1 write SetNewFont;
    property FontMultiSelectedInactive2: TFont index 10 read fFontMultiSelectedInactive2 write SetNewFont;

    property FramePenActive: TPen index 1 read fFramePenActive write SetNewPen;
    property FramePenInactive: TPen index 2 read fFramePenInactive write SetNewPen;
    property FrameOverride: boolean read fFrameOverride write SetFrameOverride;

    property _ColorPreset: TColorPreset read fColorPreset write SetColorPreset;
    property _FontPreset: TFontPreset read fFontPreset write SetFontPreset;


    property BeforeColorChange: TColorChangeEvent read fBeforeColorChange write fBeforeColorChange;
    property AfterColorChange: TColorChangeEvent read fAfterColorChange write fAfterColorChange;
    property BeforeFontChange: TFontChangeEvent read fBeforeFontChange write fBeforeFontChange;
    property AfterFontChange: TFontChangeEvent read fAfterFontChange write fAfterFontChange;


  end;

procedure Register;

implementation
uses ColorListBox, ColorCheckListBox, ColorDBGrid,
  {$IFDEF VER140}
  ColorValueListEditor,
  {$ENDIF}
  {$IFDEF VER150}
  ColorValueListEditor,
  {$ENDIF}
  ColorStringGrid, ColorDrawGrid, ColorComboBox, ColorDBListBox, ColorDBComboBox,
  ColorDBLookup;

{$R ColorControls.RES}

procedure Register;
begin
  RegisterComponents('Color controls', [TColorPresets]);
  RegisterComponents('Color controls', [TColorListBox]);
  RegisterComponents('Color controls', [TColorCheckListBox]);
  RegisterComponents('Color controls', [TColor_ComboBox]);
  {$IFDEF VER140}
  RegisterComponents('Color controls', [TColorValueListEditor]);
  {$ENDIF}
  {$IFDEF VER150}
  RegisterComponents('Color controls', [TColorValueListEditor]);
  {$ENDIF}
  RegisterComponents('Color controls', [TColorStringGrid]);
  RegisterComponents('Color controls', [TColorDrawGrid]);
  RegisterComponents('Color controls', [TColorDBGrid]);
  RegisterComponents('Color controls', [TColorDBListBox]);
  RegisterComponents('Color controls', [TColorDBComboBox]);
  RegisterComponents('Color controls', [TColorDBLookupListBox]);
  RegisterComponents('Color controls', [TColorDBLookupComboBox]);
end;

{ TColorPresets }

procedure TColorPresets.AddOwner(Sender: TObject);
var i: integer;
begin
   for i := 0 to Length(MyOwners) - 1 do
     if MyOwners[i] = Sender then exit;

   SetLength(MyOwners, Length(MyOwners) + 1);
   MyOwners[Length(MyOwners) - 1] := Sender;
end;

constructor TColorPresets.Create(Owner: TComponent);
begin
  inherited create(owner);
  fFontActive1 := TFont.Create;
  fFontActive2 := TFont.Create;
  fFontInactive1 := TFont.Create;
  fFontInactive2 := TFont.Create;
  fFontSelectedActive := TFont.Create;
  fFontSelectedInactive := TFont.Create;
  fFontMultiSelectedActive1 := TFont.Create;
  fFontMultiSelectedActive2 := TFont.Create;
  fFontMultiSelectedInactive1 := TFont.Create;
  fFontMultiSelectedInactive2 := TFont.Create;

  fFramePenActive := TPen.Create;
  fFramePenInactive := TPen.Create;

  fFrameOverride := true;
  
  _ColorPreset := cpDefault;
end;

procedure TColorPresets.DeleteOwner(Sender: TObject);
var i, j: integer;
begin
  for i := 0 to Length(MyOwners) - 1 do
    if MyOwners[i] = Sender then break;
  if i = Length(MyOwners) - 1 then exit;
  for j := i to Length(MyOwners) - 2 do
    MyOwners[j] := MyOwners[j + 1];

  SetLength(MyOwners, Length(MyOwners) - 1);
end;

destructor TColorPresets.Destroy;
begin
  fFontActive1.Free;
  fFontActive2.Free;
  fFontInactive1.Free;
  fFontInactive2.Free;
  fFontSelectedActive.Free;
  fFontSelectedInactive.Free;
  fFontMultiSelectedActive1.Free;
  fFontMultiSelectedActive2.Free;
  fFontMultiSelectedInactive1.Free;
  fFontMultiSelectedInactive2.Free;
  inherited Destroy;
end;

procedure TColorPresets.InvalidateOwners;
var i: integer;
begin
  for i := 0 to Length(MyOwners) - 1 do
    if MyOwners[i] is TControl then TControl(MyOwners[i]).Invalidate;
end;

procedure TColorPresets.SetColorPreset(const Value: TColorPreset);
begin
  fColorPreset := Value;
  case fColorPreset of
    cpDefault:
    begin
      fColorActive1 := $00F2FFFF;
      fColorActive2 := $00E1FFFE;
      fColorInactive1 := $00F5F5F5;
      fColorInactive2 := $00E8E8E8;
      fColorSelectedActive := $00FEB396;
      fColorSelectedInactive := $00D9BCBB;
      fColorMultiSelectedActive1 := $00FFDBD5;
      fColorMultiSelectedActive2 := $00FFC5AE;
      fColorMultiSelectedInactive1 := $00DFDBD2;
      fColorMultiSelectedInactive2 := $00D3CCC0;
      fColorInplaceEditor := $00D5FFDB;

      fFramePenActive.Color := $00C1C1D2;
      fFramePenInactive.Color := $00B4B4B4;
      fFrameOverride := true;
    end;
    cpBlue:
    begin
      fColorActive1 := $00FFF9F2;
      fColorActive2 := $00FFF0E1;
      fColorInactive1 := $00F5F5F5;
      fColorInactive2 := $00E8E8E8;
      fColorSelectedActive := $00FEB396;
      fColorSelectedInactive := $00D9BCBB;
      fColorMultiSelectedActive1 := $00FFDBD5;
      fColorMultiSelectedActive2 := $00FFC5AE;
      fColorMultiSelectedInactive1 := $00DFDBD2;
      fColorMultiSelectedInactive2 := $00D3CCC0;
      fColorInplaceEditor := $00FDEFD7;

      fFramePenActive.Color := $00C1C1D2;
      fFramePenInactive.Color := $00B4B4B4;
      fFrameOverride := true;
    end;
    cpGreen:
    begin
      fColorActive1 := $00F2FFFB;
      fColorActive2 := $00E2FEF3;
      fColorInactive1 := $00F5F5F5;
      fColorInactive2 := $00E8E8E8;
      fColorSelectedActive := $0060F986;
      fColorSelectedInactive := $00A3BAA3;
      fColorMultiSelectedActive1 := $00B7FFD3;
      fColorMultiSelectedActive2 := $0080FFAF;
      fColorMultiSelectedInactive1 := $00CFE2D3;
      fColorMultiSelectedInactive2 := $00C1D3C0;
      fColorInplaceEditor := $00D5FFDB;

      fFramePenActive.Color := $00C1C1D2;
      fFramePenInactive.Color := $00B4B4B4;
      fFrameOverride := true;
    end;
    cpRed:
    begin
      fColorActive1 := $00F2FCFF;
      fColorActive2 := $00E2F7FE;
      fColorInactive1 := $00F1F2FA;
      fColorInactive2 := $00E4E9EB;
      fColorSelectedActive := $0059B5FF;
      fColorSelectedInactive := $00A9A9B4;
      fColorMultiSelectedActive1 := $00B8EBFE;
      fColorMultiSelectedActive2 := $0097D0FF;
      fColorMultiSelectedInactive1 := $00D8D8D8;
      fColorMultiSelectedInactive2 := $00C9C9C9;
      fColorInplaceEditor := $00D5F1FF;

      fFramePenActive.Color := $00C1C1D2;
      fFramePenInactive.Color := $00B4B4B4;
      fFrameOverride := true;
    end;
  end;
  InvalidateOwners;
end;

procedure TColorPresets.SetFontPreset(const Value: TFontPreset);
begin
  fFontPreset := Value;
  case fFontPreset of
    fpDefault:
    begin
       fFontActive1.Color := clWindowText;
       fFontActive1.Name := 'MS Sans Serif';
       fFontActive1.Size := 8;
       fFontActive1.Style := [];

       fFontActive2.Assign(fFontActive1);
       fFontInactive1.Assign(fFontActive1);
       fFontInactive2.Assign(fFontActive1);
       fFontSelectedActive.Assign(fFontActive1);
       fFontSelectedInactive.Assign(fFontActive1);
       fFontMultiSelectedActive1.Assign(fFontActive1);
       fFontMultiSelectedActive2.Assign(fFontActive1);
       fFontMultiSelectedInactive1.Assign(fFontActive1);
       fFontMultiSelectedInactive2.Assign(fFontActive1);
    end;
  end;
  InvalidateOwners;
end;

procedure TColorPresets.SetFrameOverride(const Value: boolean);
begin
  fFrameOverride := Value;
  InvalidateOwners;
end;

procedure TColorPresets.SetNewColor(const Index: Integer; const Value: TColor);
var sFieldName: String;
    NewValue: TColor;
begin
  NewValue := value;
  case Index of
    1: sFieldName := 'ColorActive1';
    2: sFieldName := 'ColorActive2';
    3: sFieldName := 'ColorInactive1';
    4: sFieldName := 'ColorInactive2';
    5: sFieldName := 'ColorSelectedActive';
    6: sFieldName := 'ColorSelectedInactive';
    7: sFieldName := 'ColorMultiSelectedActive1';
    8: sFieldName := 'ColorMultiSelectedActive2';
    9: sFieldName := 'ColorMultiSelectedInactive1';
   10: sFieldName := 'ColorMultiSelectedInactive2';
   11: sFieldName := 'ColorInplaceEditor';
  end;

  if Assigned(fBeforeColorChange) then BeforeColorChange(Self, sFieldName, NewValue);

  case Index of
    1: fColorActive1 := NewValue;
    2: fColorActive2 := NewValue;
    3: fColorInactive1 := NewValue;
    4: fColorInactive2 := NewValue;
    5: fColorSelectedActive := NewValue;
    6: fColorSelectedInactive := NewValue;
    7: fColorMultiSelectedActive1 := NewValue;
    8: fColorMultiSelectedActive2 := NewValue;
    9: fColorMultiSelectedInactive1 := NewValue;
   10: fColorMultiSelectedInactive2 := NewValue;
   11: fColorInplaceEditor := NewValue;
  end;

  if Assigned(fAfterColorChange) then AfterColorChange(Self, sFieldName, NewValue);

  fColorPreset := cpUser;
  InvalidateOwners;
end;

procedure TColorPresets.SetNewFont(const Index: Integer; const Value: TFont);
var sFieldName: String;
    NewValue: TFont;
begin
  NewValue.Assign(Value);
  case Index of
    1: sFieldName := 'FontActive1';
    2: sFieldName := 'FontActive2';
    3: sFieldName := 'FontInactive1';
    4: sFieldName := 'FontInactive2';
    5: sFieldName := 'FontSelectedActive';
    6: sFieldName := 'FontSelectedInactive';
    7: sFieldName := 'FontMultiSelectedActive1';
    8: sFieldName := 'FontMultiSelectedActive2';
    9: sFieldName := 'FontMultiSelectedInactive1';
   10: sFieldName := 'FontMultiSelectedInactive2';
  end;

  if Assigned(fBeforeFontChange) then BeforeFontChange(Self, sFieldName, NewValue);

  case Index of
    1: fFontActive1.Assign(NewValue);
    2: fFontActive2.Assign(NewValue);
    3: fFontInactive1.Assign(NewValue);
    4: fFontInactive2.Assign(NewValue);
    5: fFontSelectedActive.Assign(NewValue);
    6: fFontSelectedInactive.Assign(NewValue);
    7: fFontMultiSelectedActive1.Assign(NewValue);
    8: fFontMultiSelectedActive2.Assign(NewValue);
    9: fFontMultiSelectedInactive1.Assign(NewValue);
   10: fFontMultiSelectedInactive2.Assign(NewValue);
  end;

  if Assigned(fAfterFontChange) then AfterFontChange(Self, sFieldName, NewValue);

  fFontPreset := fpUser;
  InvalidateOwners;
end;


procedure TColorPresets.SetNewPen(const Index: Integer; const Value: TPen);
begin
  case Index of
    1: fFramePenActive.Assign(Value);
    2: fFramePenInactive.Assign(Value);
  end;
  InvalidateOwners;
end;

end.
