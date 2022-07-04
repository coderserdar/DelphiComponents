{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       GT IP Edit Control                              }
{                                                       }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTIPEdit;

interface
uses
   Classes
  ,Controls
  ,StdCtrls
  ,Contnrs
  ;
type
  TgtIPEdit = class(TWinControl)
  private
    FIP: string;
    FOnIpChange: TNotifyEvent;
    FOnSecondPartChange: TNotifyEvent;
    FOnThirdPartChange: TNotifyEvent;
    FOnFirstPartChange: TNotifyEvent;
    FOnFourthPartChange: TNotifyEvent;
    FIPEditorWidth: Integer;
    procedure SetIP(const Value: string);
    function GetDotCount(IP: string): Integer;
    function GetFirstPart(IP: string): Integer;
    function GetFourthPart(IP: string): Integer;
    function GetSecondPart(IP: string): Integer;
    function GetThirdPart(IP: string): Integer;
    { Private declarations }
  protected
    { Protected declarations }
    FLastLeft        : Integer;
    FControlsCreated : Boolean;
    FDotIndex        : array [0..2] of Integer;
    FEditors         : TComponentList;
    FDotCount  : Integer;
    FFirstPart : Integer;
    FSecondPart: Integer;
    FThirdPart : Integer;
    FFourthPart: Integer;
    procedure CreateControls;
    procedure CreateDot;
    procedure CreateEditor;
    procedure ApplyIP;
    function  GetSubString(AString :string; FromIndex : Integer ; ToIndex: Integer):string;
    function  GetProperWidth:Integer;
  protected
    procedure SetParent(AParent : TWinControl);override;
    function  IsValidIP(Ip : string):Boolean;
    procedure InternalOnChange(Sender : TObject);
    procedure InternalOnExit(Sender   : TObject);
    procedure InternalOnKeyPress(Sender : TObject ; var Key : Char);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property IP            : string  read FIP            write SetIP;
    property IPFirstPart   : Integer read FFirstPart;
    property IPSecondPart  : Integer read FSecondPart;
    property IPThirdPart   : Integer read FThirdPart;
    property IPFourthPart  : Integer read FFourthPart;
    property IPEditorWidth : Integer read FIPEditorWidth write FIPEditorWidth;
  published
    property OnIPFirstPartChange   : TNotifyEvent read FOnFirstPartChange  write FOnFirstPartChange;
    property OnIPSecondPartChange  : TNotifyEvent read FOnSecondPartChange write FOnSecondPartChange;
    property OnIPThirdPartChange   : TNotifyEvent read FOnThirdPartChange  write FOnThirdPartChange;
    property OnIPFourthPartChange  : TNotifyEvent read FOnFourthPartChange write FOnFourthPartChange;
    property OnIPChange            : TNotifyEvent read FOnIpChange         write FOnIPChange;
  published
    property Align;
    property Color;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


implementation
uses
  SysUtils
  ;

const
  IP_MAX_NUMBER = 255;
  IP_DELIMITER  = '.';

  ERR_WRONG_IP  =
  'IP Address %s is invalid!';




{ TgtIPEdit }
{------------------------------------------------------------------------------}
constructor TgtIPEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 21;
  Self.Width  := 219;
  {Self.Constraints.MaxHeight := 21;
  Self.Constraints.MaxWidth  := 220;
  Self.Constraints.MinHeight := 21;
  Self.Constraints.MinWidth  := 220;}
  Self.ParentColor           := True;
  FLastLeft                  := 0;
  FIPEditorWidth             := 30;
  FControlsCreated           := False;
  FEditors                   := TComponentList.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtIPEdit.Destroy;
begin
  FEditors.Clear;
  FEditors.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.CreateControls;
begin
  if not FControlsCreated then
  begin
    CreateEditor;
    CreateDot;
    CreateEditor;
    CreateDot;
    CreateEditor;
    CreateDot;
    CreateEditor;
    FControlsCreated := True;
    Self.Width       := GetProperWidth;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.CreateDot;
var
  Lbl : TLabel;
begin
  Lbl             := TLabel.Create(Self);
  Lbl.Parent      := Self;
  Lbl.Caption     := '.';
  Lbl.Font.Size   := 16;
  Lbl.Left        := FLastLeft;
  Lbl.Transparent := True;
  Lbl.ParentColor := True;
  FLastLeft       := Lbl.Width + FLastLeft;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.CreateEditor;
var
  Edt : TEdit;
begin
  Edt            := TEdit.Create(Self);
  Edt.Text       := '';
  Edt.Parent     := Self;
  Edt.Width      := FIPEditorWidth;
  Edt.Left       := FLastLeft;
  Edt.OnChange   := InternalOnChange;
  Edt.OnKeyPress := InternalOnKeyPress;
  Edt.OnExit     := InternalOnExit;
  FLastLeft      := Edt.Width + FLastLeft;
  FEditors.Add(Edt);
  Edt.Tag        := FEditors.Count;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.ApplyIP;
begin
  TEdit(FEditors[0]).Text := IntToStr(FFirstPart);
  TEdit(FEditors[1]).Text := IntToStr(FSecondPart);
  TEdit(FEditors[2]).Text := IntToStr(FThirdPart);
  TEdit(FEditors[3]).Text := IntToStr(FFourthPart);
  if Assigned(FOnIPChange) then
    FOnIPChange(Self);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.IsValidIP(Ip: string): Boolean;
begin
  FDotCount   := GetDotCount(IP);
  FFirstPart  := GetFirstPart(IP);
  FSecondPart := GetSecondPart(IP);
  FThirdPart  := GetThirdPart(IP);
  FFourthPart := GetFourthPart(IP);

  Result     := (FDotCount = 3) and (FFirstPart <= IP_MAX_NUMBER) and (FSecondPart <= IP_MAX_NUMBER) and (FThirdPart <= IP_MAX_NUMBER) and (FFourthPart <= IP_MAX_NUMBER);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetDotCount(IP :string):Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 1 to Length(IP) do
    if IP[i] = IP_DELIMITER then
    begin
      FDotIndex[Result] := i;
      inc(Result);
    end;
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetFirstPart(IP :string):Integer;
var
  Temp : string;
begin
  Result := IP_MAX_NUMBER+1;
  Temp   := GetSubString(IP,1,Pred(FDotIndex[0]));
  TryStrToInt(Temp,Result);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetSecondPart(IP :string):Integer;
var
  Temp : string;
begin
  Result := IP_MAX_NUMBER+1;
  Temp   := GetSubString(IP,FDotIndex[0]+1,FDotIndex[1]-1);
  TryStrToInt(Temp,Result);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetThirdPart(IP :string):Integer;
var
  Temp : string;
begin
  Result := IP_MAX_NUMBER+1;
  Temp   := GetSubString(IP,FDotIndex[1]+1,FDotIndex[2]-1);
  TryStrToInt(Temp,Result);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetFourthPart(IP :string):Integer;
var
  Temp : string;
begin
  Result := IP_MAX_NUMBER+1;
  Temp   := GetSubString(IP,FDotIndex[2]+1,Length(IP));
  TryStrToInt(Temp,Result);
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetSubString(AString: string; FromIndex, ToIndex: Integer): string;
var
  i : Integer;
begin
  Result := '';
  for i := FromIndex to ToIndex do
  begin
    Result := Result + AString[i];
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.InternalOnChange(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    1 : if Assigned(FOnFirstPartChange)  then FOnFirstPartChange(Self);
    2 : if Assigned(FOnSecondPartChange) then FOnSecondPartChange(Self);
    3 : if Assigned(FOnThirdPartChange)  then FOnThirdPartChange(Self);
    4 : if Assigned(FOnFourthPartChange) then FOnFourthPartChange(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.InternalOnExit(Sender: TObject);
const
  IP_STRING = '%s.%s.%s.%s';
begin
  case TComponent(Sender).Tag of
    4 : IP := Format(IP_STRING,[TEdit(FEditors[0]).Text,TEdit(FEditors[1]).Text,TEdit(FEditors[2]).Text,TEdit(FEditors[3]).Text])
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.InternalOnKeyPress(Sender: TObject; var Key: Char);
const
  IP_STRING = '%s.%s.%s.%s';
begin
  case Key of
    '0'..'9'  :
      begin
        if Length(TEdit(Sender).Text) >= 3 then
          Key :=#0;
      end;
    #8        : ;
    #13       : IP := Format(IP_STRING,[TEdit(FEditors[0]).Text,TEdit(FEditors[1]).Text,TEdit(FEditors[2]).Text,TEdit(FEditors[3]).Text])
  else
    Key := #0;
  end;
end;
{------------------------------------------------------------------------------}
function TgtIPEdit.GetProperWidth: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i:= 0 to Pred(Self.ControlCount) do
  begin
    Result := Result + Self.Controls[i].Width;
  end;
end;
{------------------------------------------------------------------------------}





//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtIPEdit.SetIP(const Value: string);
begin
  if not SameText(FIP,Value) and (Length(Trim(Value)) > 0 ) then
  begin
    FIP := Value;
    if Self.IsValidIP(FIP) then
      ApplyIP
    else
    begin
      Self.SetFocus;
      raise Exception.CreateFmt(ERR_WRONG_IP,[FIP]);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtIPEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(Parent) then
    CreateControls;
end;
{------------------------------------------------------------------------------}
















end.
