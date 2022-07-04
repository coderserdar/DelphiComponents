unit SRDlgs;

{$R-,H+,X+}

{ SRDialogs (C)opyright 2000 Version 2.11

  TExtOpenDialog, TExtSaveDialog,
  TCSVOpenDialog, TCSVSaveDialog,
  TSliderOpenDialog, TSliderSaveDialog,

  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponenten sind von TOpenDialog abgeleitete, erweiterte
  Dialoge. Sie sind Public Domain, das Urheberrechtt liegt aber
  beim Autor. }

interface

uses Messages, Windows, SysUtils, Classes, Controls, StdCtrls, Graphics,
     ExtCtrls, Buttons, Dialogs, ComCtrls;

type

  { Extended Dialogs }
  TExtOpenDialog = class(TOpenDialog)
  private
    FExtraPanel  : TPanel;
    FDescrLabel  : TLabel;
    FExtraEdit   : TEdit;
    FDescription,
    FExtraText   : string;

  protected
    procedure DoClose; override;
    procedure DoShow; override;
    procedure SetDescription(NewValue:string);
    procedure SetExtraText(NewValue:string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

  published
    property Description: string read FDescription write SetDescription;
    property ExtraText: string read FExtraText write SetExtraText;
  end;

  TExtSaveDialog = class(TExtOpenDialog)
  public
    function Execute: Boolean; override;
  end;

  { CSV-Dialogs }
  TCSVOpenDialog = class(TOpenDialog)
  private
    FExtraPanel   : TPanel;
    FDescrLabel   : TLabel;
    FExtraEdit    : TEdit;
    FTabCheckBox  : TCheckBox;
    FSepDescr,
    FTabDescr     : string;
    FTabSeparator : boolean;
    FSeparator    : char;

  protected
    procedure DoClose; override;
    procedure DoShow; override;
    procedure SetTabSeparator(NewValue:boolean);
    procedure SetSeparator(NewValue:char);
    procedure TabCheckBoxClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

  published
    property Separator: char read FSeparator write SetSeparator;
    property SepDescription: string read FSepDescr write FSepDescr;
    property TabSeparator: boolean read FTabSeparator write SetTabSeparator;
    property TabDescription: string read FTabDescr write FTabDescr;
  end;

  TCSVSaveDialog = class(TCSVOpenDialog)
  public
    function Execute: Boolean; override;
  end;

  { Slider Dialogs }
  TSliderOpenDialog = class(TOpenDialog)
  private
    FExtraPanel  : TPanel;
    FDescrLabel,
    FMinLabel,
    FValueLabel,
    FMaxLabel    : TLabel;
    FScrollBar   : TScrollBar;
    FLargeChange,
    FSmallChange,
    FMinValue,
    FMaxValue,
    FValue       : integer;
    FDescription,
    FMinDescr,
    FMaxDescr    : string;
    procedure ScrollBarChange(Sender: TObject);

  protected
    procedure DoClose; override;
    procedure DoShow; override;
    procedure SetLargeChange(NewValue:integer);
    procedure SetMaxValue(NewValue:integer);
    procedure SetMinValue(NewValue:integer);
    procedure SetSmallChange(NewValue:integer);
    procedure SetValue(NewValue:integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

  published
    property Description: string read FDescription write FDescription;
    property LargeChange: integer read FLargeChange write SetLargeChange;
    property MaxDescr: string read FMaxDescr write FMaxDescr;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property MinDescr: string read FMinDescr write FMinDescr;
    property MinValue: integer read FMinValue write SetMinValue;
    property SmallChange: integer read FSmallChange write SetSmallChange;
    property Value: integer read FValue write SetValue;
  end;

  TSliderSaveDialog = class(TSliderOpenDialog)
  public
    function Execute: Boolean; override;
  end;

procedure Register;

implementation

uses Consts, Forms, CommDlg, Dlgs;

{$R *.RES}

{ TExtOpenDialog }
constructor TExtOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescription:= '';
  FExtraPanel := TPanel.Create(Self);
  with FExtraPanel do begin
    Name := 'ExtraPanel';
    Caption := '';
    BevelOuter := bvNone;
    BorderWidth := 2;
    TabOrder := 1;
    FDescrLabel := TLabel.Create(Self);
    with FDescrLabel do begin
      Name := 'DescrLabel';
      Caption := FDescription;
      SetBounds(6, 4, 80, 20);
      Align := alNone;
      Ctl3D:=true;
      AutoSize := True;
      Parent := FExtraPanel;
    end;
    FExtraEdit := TEdit.Create(Self);
    with FExtraEdit do begin
      Name := 'ExtraEdit';
      SetBounds(81, 1, 233, 21);
      Text := FExtraText;
      Enabled := True;
      Parent := FExtraPanel;
    end;
  end;
end;

destructor TExtOpenDialog.Destroy;
begin
  FDescrLabel.Free;
  FExtraEdit.Free;
  FExtraPanel.Free;
  inherited Destroy;
end;

procedure TExtOpenDialog.SetDescription(NewValue:string);
begin
  if NewValue<>FDescription then begin
    FDescription:=NewValue;
    FDescrLabel.Caption:=FDescription;
  end;
end;

procedure TExtOpenDialog.SetExtraText(NewValue:string);
begin
  if NewValue<>FExtraText then begin
    FExtraText:=NewValue;
    FExtraEdit.Text:=FExtraText;
  end;
end;

procedure TExtOpenDialog.DoClose;
begin
  FExtraText:=FExtraEdit.Text;
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TExtOpenDialog.DoShow;
var ExtRect,
    StaticRect: TRect;
begin
  StaticRect := GetStaticRect;
  { Set ExtraText area to bottom of static area }
  ExtRect.Top := StaticRect.Bottom;
  ExtRect.Left := StaticRect.Left;
  ExtRect.Bottom := ExtRect.Top+30;
  ExtRect.Right := StaticRect.Right;
  FExtraPanel.ParentWindow := Handle;
  FExtraPanel.BoundsRect := ExtRect;
  inherited DoShow;
end;

function TExtOpenDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE' else
    Template := nil;
  Result := inherited Execute;
end;


{ TExtSaveDialog }

function TExtSaveDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE' else
    Template := nil;
  Result := DoExecute(@GetSaveFileName);
end;

{ TCSVOpenDialog }

constructor TCSVOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeparator := ';';
  FSepDescr := 'Trennzeichen:';
  FTabDescr := 'als Trennzeichen';
  FTabSeparator := false;
  FExtraPanel := TPanel.Create(Self);
  with FExtraPanel do begin
    Name := 'ExtraPanel';
    Caption := '';
    BevelOuter := bvNone;
    BorderWidth := 2;
    TabOrder := 1;
    FDescrLabel := TLabel.Create(Self);
    with FDescrLabel do begin
      Name := 'DescrLabel';
      Caption := FSepDescr;
      SetBounds(6, 4, 80, 20);
      Align := alNone;
      AutoSize := True;
      Parent := FExtraPanel;
    end;
    FTabCheckBox := TCheckBox.Create(Self);
    with FTabCheckBox do begin
      Name := 'TabCheckBox';
      SetBounds(150, 2, 150, 20);
      Caption := '[Tab] '+FTabDescr;
      Enabled := True;
      TabOrder := 1;
      Parent := FExtraPanel;
      OnClick := TabCheckBoxClick;
    end;
    FExtraEdit := TEdit.Create(Self);
    with FExtraEdit do begin
      Name := 'ExtraEdit';
      SetBounds(81, 1, 50, 21);
      Text := FSeparator;
      Enabled := True;
      Ctl3D:=true;
      MaxLength:=1;
      TabOrder := 0;
      Parent := FExtraPanel;
    end;
  end;
end;

destructor TCSVOpenDialog.Destroy;
begin
  FDescrLabel.Free;
  FExtraEdit.Free;
  FTabCheckBox.Free;
  FExtraPanel.Free;
  inherited Destroy;
end;

procedure TCSVOpenDialog.SetTabSeparator(NewValue:boolean);
begin
  if NewValue<>FTabSeparator then begin
    FTabSeparator:=NewValue;
    FTabCheckbox.Checked:=FTabSeparator;
    FDescrLabel.Enabled:=not FTabSeparator;
    FExtraEdit.Enabled:=not FTabSeparator;
  end;
end;

procedure TCSVOpenDialog.SetSeparator(NewValue:char);
begin
  if NewValue<>FSeparator then begin
    FSeparator:=NewValue;
    FExtraEdit.Text:=FSeparator;
  end;
end;

procedure TCSVOpenDialog.DoClose;
begin
  FTabSeparator:=FTabCheckBox.Checked;
  if FTabSeparator then
    FSeparator:=#9
  else
    if FExtraEdit.Text<>'' then
      FSeparator:=FExtraEdit.Text[1];
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TCSVOpenDialog.DoShow;
var SepRect,
    StaticRect: TRect;
begin
  StaticRect := GetStaticRect;
  { Set ExtraText area to bottom of static area }
  SepRect.Top := StaticRect.Bottom;
  SepRect.Left := StaticRect.Left;
  SepRect.Bottom := SepRect.Top+30;
  SepRect.Right := StaticRect.Right;
  FExtraPanel.ParentWindow := Handle;
  FExtraPanel.BoundsRect := SepRect;
  FExtraEdit.Text := FSeparator;
  FDescrLabel.Caption := FSepDescr;
  FTabCheckBox.Caption := '[Tab] '+FTabDescr;
  inherited DoShow;
end;

procedure TCSVOpenDialog.TabCheckBoxClick(Sender: TObject);
begin
  FDescrLabel.Enabled:=not FTabCheckBox.Checked;
  FExtraEdit.Enabled:=not FTabCheckBox.Checked;
end;

function TCSVOpenDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE'
  else
    Template := nil;
  Result := inherited Execute;
end;

{ TCSVSaveDialog }

function TCSVSaveDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE'
  else
    Template := nil;
  Result := DoExecute(@GetSaveFileName);
end;

{ TSliderOpenDialog }

constructor TSliderOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescription := '';
  FValue := 0;
  FLargeChange := 5;
  FMinValue := 0;
  FMaxValue := 10;
  FMinDescr := '0';
  FMaxDescr := '10';
  FSmallChange := 1;
  FExtraPanel := TPanel.Create(Self);
  with FExtraPanel do begin
    Name := 'ExtraPanel';
    Caption := '';
    BevelOuter := bvNone;
    BorderWidth := 2;
    TabOrder := 1;
    FDescrLabel := TLabel.Create(Self);
    with FDescrLabel do begin
      Name := 'DescrLabel';
      Caption := FDescription;
      SetBounds(6, 4, 80, 20);
      Align := alNone;
      AutoSize := True;
      Parent := FExtraPanel;
    end;
    FMinLabel := TLabel.Create(Self);
    with FMinLabel do begin
      Name := 'MinLabel';
      SetBounds(81, 20, 40, 34);
      Alignment := taLeftJustify;
      Caption := FMinDescr;
      Align := alNone;
      AutoSize := True;
      Parent := FExtraPanel;
    end;
    FMaxLabel := TLabel.Create(Self);
    with FMaxLabel do begin
      Name := 'MaxLabel';
      SetBounds(284, 20, 30, 34);
      Alignment := taRightJustify;
      Caption := FMaxDescr;
      Align := alNone;
      AutoSize := True;
      Parent := FExtraPanel;
    end;
    FValueLabel := TLabel.Create(Self);
    with FValueLabel do begin
      Name := 'ValueLabel';
      AutoSize := false;
      SetBounds(120, 20, 140, 34);
      Font.Style := [fsBold];
      Alignment := taCenter;
      Caption := IntToStr(FValue);
      Transparent := true;
      Align := alNone;
      Parent := FExtraPanel;
    end;
    FScrollBar := TScrollBar.Create(Self);
    with FScrollBar do begin
      Name := 'ScrollBar';
      SetBounds(81, 1, 233, 16);
      LargeChange := FLargeChange;
      SmallChange := FSmallChange;
      Min := FMinValue;
      Max := FMaxValue;
      Kind := sbHorizontal;
      Position := FValue;
      Parent := FExtraPanel;
      OnChange := ScrollBarChange;
    end;
  end;
end;

destructor TSliderOpenDialog.Destroy;
begin
  FDescrLabel.Free;
  FMinLabel.Free;
  FMaxLabel.Free;
  FScrollBar.Free;
  FExtraPanel.Free;
  inherited Destroy;
end;

procedure TSliderOpenDialog.SetLargeChange(NewValue:integer);
begin
  if NewValue<>FLargeChange then begin
    FLargeChange:=NewValue;
    FScrollBar.LargeChange:=FLargeChange;
  end;
end;

procedure TSliderOpenDialog.SetMaxValue(NewValue:integer);
begin
  if NewValue<>FMaxValue then begin
    if FValue>NewValue then begin
      FValue:=NewValue;
      FScrollBar.Position:=FValue;
    end;
    FMaxValue:=NewValue;
  end;
end;

procedure TSliderOpenDialog.SetMinValue(NewValue:integer);
begin
  if NewValue<>FMinValue then begin
    if FValue<NewValue then begin
      FValue:=NewValue;
      FScrollBar.Position:=FValue;
    end;
    FMinValue:=NewValue;
  end;
end;

procedure TSliderOpenDialog.SetSmallChange(NewValue:integer);
begin
  if NewValue<>FSmallChange then begin
    FSmallChange:=NewValue;
    FScrollBar.SmallChange:=FSmallChange;
  end;
end;

procedure TSliderOpenDialog.SetValue(NewValue:integer);
begin
  if NewValue<>FValue then begin
    FValue:=NewValue;
    FScrollBar.Position:=FValue;
    FValueLabel.Caption:=IntToStr(FValue);
  end;
end;

procedure TSliderOpenDialog.ScrollBarChange(Sender: TObject);
begin
  FValueLabel.Caption:=IntToStr(FScrollBar.Position);
end;

procedure TSliderOpenDialog.DoClose;
begin
  FValue:=FScrollBar.Position;
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TSliderOpenDialog.DoShow;
var SlidRect,
    StaticRect: TRect;
begin
  StaticRect := GetStaticRect;
  { Set ExtraText area to bottom of static area }
  SlidRect.Top := StaticRect.Bottom-2;
  SlidRect.Left := StaticRect.Left;
  SlidRect.Bottom := SlidRect.Top+40;
  SlidRect.Right := StaticRect.Right;
  FExtraPanel.ParentWindow := Handle;
  FExtraPanel.BoundsRect := SlidRect;
  FDescrLabel.Caption := FDescription;
  FMinLabel.Caption := FMinDescr;
  FMaxLabel.Caption := FMaxDescr;
  FScrollBar.Min := FMinValue;
  FScrollBar.Max := FMaxValue;
  FScrollBar.Position := FValue;
  FScrollBar.LargeChange := FLargeChange;
  FScrollBar.SmallChange := FSmallChange;
  inherited DoShow;
end;

function TSliderOpenDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE'
  else
    Template := nil;
  Result := inherited Execute;
end;

{ TSliderSaveDialog }

function TSliderSaveDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'EXTDLGTEMPLATE'
  else
    Template := nil;
  Result := DoExecute(@GetSaveFileName);
end;

procedure Register;
begin
  RegisterComponents('Simon', [TExtOpenDialog]);
  RegisterComponents('Simon', [TExtSaveDialog]);
  RegisterComponents('Simon', [TCSVOpenDialog]);
  RegisterComponents('Simon', [TCSVSaveDialog]);
  RegisterComponents('Simon', [TSliderOpenDialog]);
  RegisterComponents('Simon', [TSliderSaveDialog]);
end;

end.
