{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtListBox                                      }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}

unit o_GTListBox;

interface
uses
   Classes
  ,StdCtrls
  ,Graphics
  ;
type
{------------------------------------------------------------------------------}
  TgtListBox = class(TListBox)
  private
    FEditorEnabled: Boolean;
    FEditorColor: TColor;
    FEditorFontColor: TColor;
    { Private declarations }
  protected
    { Protected declarations }
    FInplaceEdit : TEdit;
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    procedure InternalOnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property EditorColor      : TColor  read FEditorColor     write FEditorColor;
    property EditorFontColor  : TColor  read FEditorFontColor write FEditorFontColor;
    property EditorEnabled    : Boolean read FEditorEnabled   write FEditorEnabled;
  end;
{------------------------------------------------------------------------------}

implementation

{ TgtListBox }
{------------------------------------------------------------------------------}
constructor TgtListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditorEnabled   := False;
  EditorColor      := Color;
  FEditorFontColor := Font.Color;
  Style            := lbOwnerDrawFixed;
  ItemHeight       := 21;
end;
{------------------------------------------------------------------------------}
destructor TgtListBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FEditorEnabled then
    if (Items.Count > 0) and (ItemIndex >=0) and (Key = 13) then //the enter key
    begin
      if not Assigned(FInplaceEdit) then
      begin
        FInplaceEdit            := TEdit.Create(Self);
        FInplaceEdit.OnKeyDown  := InternalOnEditorKeyDown;
      end;

      FInplaceEdit.Parent     := Self;
      FInplaceEdit.Color      := FEditorColor;
      FInplaceEdit.Font.Color := FEditorFontColor;
      FInplaceEdit.BoundsRect := ItemRect(ItemIndex);

      FInplaceEdit.Visible    := True;
      FInplaceEdit.Text       := Items[ItemIndex];
      FInplaceEdit.SetFocus;
    end;
  inherited KeyDown(Key,Shift);
end;
{------------------------------------------------------------------------------}
procedure TgtListBox.InternalOnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Items.Count > 0) and (ItemIndex >=0) and (Key = 13) then //the enter key
  begin
    Items[ItemIndex] := TEdit(Sender).Text;
    TEdit(Sender).Visible := False;
    SetFocus;
    ItemIndex        := ItemIndex;
  end;
end;
{------------------------------------------------------------------------------}
end.

