unit ElasticPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls;

type
  TSizeType = (stLeft, stRight, stUp, stUpLeft, stUpRight, stDown, stDownLeft, stDownRight, stMove);

  TElasticPanel = class(TPanel)
  private
    { Private declarations }
//    FAlignComponents : Boolean;
    FMovable         : Boolean;
    FOperation       : TSizeType;
    FPopUp           : TPopUpMenu;
    FOldLeft         : Integer;
    FOldTop          : Integer;
    FOldWidth        : Integer;
    FOldHeight       : Integer;

  protected
    { Protected declarations }
    procedure SetOperation (Value : TsizeType);
    procedure DoMenuClick(Sender : TObject);
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    { Public declarations }
    constructor Create (AOwner : TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations }
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);override;

    property  Movable          : Boolean    read FMovable         write FMovable;
    property  Operation        : TSizeType  read FOperation       write SetOperation default stMove;
//    property  AlignComponents  : Boolean    read FAlignComponents write FAlignComponents;
  end;

procedure Register;

implementation

const
     MaxMenu  = 9;

constructor TElasticPanel.Create (AOwner : TComponent);
var
   XMenuItem : TMenuItem;
   I         : Word;
const
   Captions : Array [1..9] of String =
            ('SizeLeft','SizeRight','SizeUp',
             'SizeUpLeft','SizeUpRight',
             'SizeDown','SizeDownLeft','SizeDownRight','SizeMove');//,'AlignControls');
begin
  inherited Create(AOwner);
  FMovable := True;
  FOperation := stMove;
//  FAlignComponents := False;

  FPopUp := TPopUpMenu.Create(Self);

  for i:=1 to MaxMenu do
      begin
        XMenuItem := TMenuItem.Create(GetParentForm(Self));
        XMenuItem.Caption := Captions[i];
        XMenuItem.Tag     := i;
//        if i=10 then
//           begin
//             XMenuItem.Checked := False;
//           end;
        XMenuItem.OnClick := DoMenuClick;

        FPopUp.Items.Add(XMenuItem);
      end;
  PopUpMenu := FPopUp;
//  XMenuItem.Free;
end;

destructor TElasticPanel.Destroy;
begin
  FPopUp.Destroy;
  inherited Destroy;
end;

procedure TElasticPanel.DoMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
       1..9: begin
               Operation := TSizeType(TMenuItem(Sender).Tag-1);
               Movable   := True;
             end;
{       10  : begin
               TMenuItem(Sender).Checked := not(TMenuItem(Sender).Checked);
               AlignComponents := TMenuItem(Sender).Checked;
             end;}
  end;

//  case TMenuItem(Sender).Tag of
end;

procedure TElasticPanel.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const
  SC_DragMove = $F012;  { a magic number }
  Ops         : Array[0..8] of Word =
              (61441, 61442, 61443, 61444, 61445, 61446, 61447, 61448, 61449);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FMovable and (Shift <> Shift - [ssShift]) then
     begin
       FOldLeft    := Left;
       FOldTop     := Top;
       FOldWidth   := Width;
       FOldHeight  := Height;
       ReleaseCapture;
       perform(WM_SysCommand, Ops[Ord(FOperation)], 0);
     end;

{  if  Button = mbRight then
      FPopUp.PopUp(10,10);}
end;

procedure TElasticPanel.SetOperation (Value : TsizeType);
begin
  if Value <> FOperation then
     FOperation := Value;
end;

procedure TElasticPanel.WMMove(var Message: TWMMove);
begin
end;

procedure TElasticPanel.WMSize(var Message: TWMSize);
{var
   I      : Word;
   C      : TCheckBox;}
begin
{  if (FOldLeft   <> Left  ) or
     (FOldTop    <> Top   ) or
     (FOldWidth  <> Width ) or
     (FOldHeight <> Height) then
     begin
       FOldLeft    := Left;
       FOldTop     := Top;
       FOldWidth   := Width;
       FOldHeight  := Height;
       if FAlignComponents then
          begin
            for i:=0 to ComponentCount - 1 do
                begin
                  if Components[i] is TEdit then showmessage('^d');
                  TControl(Self.Components[i]).Left := TControl(Components[i]).Left * round(Left / FOldLeft);
                  TControl(Self.Components[i]).Top := TControl(Components[i]).Top * round(Top / FOldTop);
                  TControl(Self.Components[i]).Width := TControl(Components[i]).Width * round(Width / FOldWidth);
                  TControl(Self.Components[i]).Height := TControl(Components[i]).Height * round(Height / FOldHeight);
                end;
          end;
     end;}
end;

procedure Register;
begin
  RegisterComponents('Cc', [TElasticPanel]);
end;

end.


