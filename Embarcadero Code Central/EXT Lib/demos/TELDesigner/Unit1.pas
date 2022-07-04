{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELDesigner                                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELControls, ImgList, Menus, ExtCtrls, ELDsgnr,
  ColorGrd, ComCtrls;

type
  TForm1 = class(TForm)
    Bevel1: TBevel;
    Label4: TLabel;
    ELDesigner1: TELDesigner;
    Button1: TButton;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    N1: TMenuItem;
    SetVisibletoTrue1: TMenuItem;
    SetVisibletoFalse1: TMenuItem;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    ColorGrid2: TColorGrid;
    Label8: TLabel;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    UpDown2: TUpDown;
    Label2: TLabel;
    UpDown1: TUpDown;
    GroupBox2: TGroupBox;
    Memo2: TMemo;
    Button3: TButton;
    GroupBox3: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Edit1: TEdit;
    GroupBox4: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Button4: TButton;
    N2: TMenuItem;
    Nodelete1: TMenuItem;
    NoMove1: TMenuItem;
    Noresize1: TMenuItem;
    Aligntogrid1: TMenuItem;
    Noinsertin1: TMenuItem;
    Nocopy1: TMenuItem;
    N3: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Memo1: TMemo;
    Label3: TLabel;
    lmCustom11: TMenuItem;
    lmCustom21: TMenuItem;
    Panel1: TPanel;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Button5: TButton;
    CheckBox10: TCheckBox;
    SetEnabledtoTrue1: TMenuItem;
    SetEnabledtoFalse1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure SetVisibletoTrue1Click(Sender: TObject);
    procedure SetVisibletoFalse1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure ColorGrid2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ELDesigner1ChangeSelection(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure ELDesigner1ControlHint(Sender: TObject; AControl: TControl;
      var AHint: String);
    procedure Button4Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Nodelete1Click(Sender: TObject);
    procedure NoMove1Click(Sender: TObject);
    procedure Noresize1Click(Sender: TObject);
    procedure Aligntogrid1Click(Sender: TObject);
    procedure Noinsertin1Click(Sender: TObject);
    procedure Nocopy1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure ELDesigner1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ELDesigner1ControlInserted(Sender: TObject);
    procedure ELDesigner1ControlInserting(Sender: TObject;
      var AControlClass: TControlClass);
    procedure ELDesigner1Change(Sender: TObject);
    procedure ELDesigner1Notification(Sender: TObject;
      AnObject: TPersistent; Operation: TOperation);
    procedure ELDesigner1ValidateName(Sender: TObject; const AName: String;
      var AIsValidName: Boolean);
    procedure ELDesigner1GetUniqueName(Sender: TObject;
      const ABaseName: String; var AUniqueName: String);
    procedure ELDesigner1DblClick(Sender: TObject);
    procedure lmCustom11Click(Sender: TObject);
    procedure lmCustom21Click(Sender: TObject);
    procedure ELDesigner1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ELDesigner1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    {$IFDEF VER140}
    procedure ELDesigner1KeyPress(Sender: TObject; var Key: Char);
    {$ENDIF}
    {$IFDEF VER150}
    procedure ELDesigner1KeyPress(Sender: TObject; var Key: AnsiChar);
    {$ENDIF}
    procedure ELDesigner1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ELDesigner1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ELDesigner1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ELDesigner1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ELDesigner1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ELDesigner1MouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ELDesigner1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Button5Click(Sender: TObject);
    procedure SetEnabledtoTrue1Click(Sender: TObject);
    procedure SetEnabledtoFalse1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMyPanel = class(TPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;


var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  LStream: TMemoryStream;
begin
  if not ELDesigner1.Active then
  begin
    if CheckBox10.Checked then
    begin
      LStream := TMemoryStream.Create;
      try
        LStream.WriteComponent(Form2);
        Form2.DestroyComponents;
        ELDesigner1.DesignControl := Form2;
        ELDesigner1.Active := True;
        LStream.Seek(0, soFromBeginning);
        LStream.ReadComponent(Form2);
        Form2.Name := 'Form2';
      finally
        LStream.Free;
      end;
    end
    else
    begin
      ELDesigner1.DesignControl := Form2;
      ELDesigner1.Active := True;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LStream: TMemoryStream;
begin
  if ELDesigner1.Active then
  begin
    if CheckBox10.Checked then
    begin
      LStream := TMemoryStream.Create;
      try
        LStream.WriteComponent(Form2);
        Form2.DestroyComponents;
        ELDesigner1.Active := False;
        LStream.Seek(0, soFromBeginning);
        LStream.ReadComponent(Form2);
        Form2.Name := 'Form2';
      finally
        LStream.Free;
      end;
    end
    else
      ELDesigner1.Active := False;
  end;
end;

procedure TForm1.Delete1Click(Sender: TObject);
begin
  ELDesigner1.DeleteSelectedControls;
end;

procedure TForm1.SetVisibletoTrue1Click(Sender: TObject);
var
  LI: Integer;
begin
  for LI := 0 to ELDesigner1.SelectedControls.Count - 1 do
    ELDesigner1.SelectedControls[LI].Visible := True;
end;

procedure TForm1.SetVisibletoFalse1Click(Sender: TObject);
var
  LI: Integer;
begin
  for LI := 0 to ELDesigner1.SelectedControls.Count - 1 do
    ELDesigner1.SelectedControls[LI].Visible := False;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  ELDesigner1.Grid.Visible := CheckBox2.Checked;
end;

procedure TForm1.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  Label1.Caption := IntToStr(UpDown2.Position);
  ELDesigner1.Grid.XStep := UpDown2.Position;
end;

procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  Label2.Caption := IntToStr(UpDown1.Position);
  ELDesigner1.Grid.YStep := UpDown1.Position;
end;

procedure TForm1.ColorGrid2Change(Sender: TObject);
begin
  ELDesigner1.Grid.Color := ColorGrid2.ForegroundColor;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  ELDesigner1.SnapToGrid := CheckBox1.Checked;
end;

procedure TForm1.ELDesigner1ChangeSelection(Sender: TObject);
var
  LI: Integer;
begin
  Memo2.Lines.BeginUpdate;
  Memo2.Lines.Clear;
  for LI := 0 to ELDesigner1.SelectedControls.Count - 1 do
    Memo2.Lines.Add(
      ELDesigner1.SelectedControls[LI].Name + ': ' +
        ELDesigner1.SelectedControls[LI].ClassName
    );
  Memo2.Lines.EndUpdate;
  Memo1.Lines.Add('OnChangeSelection');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if ELDesigner1.Active then
    ELDesigner1.SelectedControls.Clear;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
    ELDesigner1.Hints := ELDesigner1.Hints + [dhControl]
  else
    ELDesigner1.Hints := ELDesigner1.Hints - [dhControl];
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    ELDesigner1.Hints := ELDesigner1.Hints + [dhSize]
  else
    ELDesigner1.Hints := ELDesigner1.Hints - [dhSize];
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
    ELDesigner1.Hints := ELDesigner1.Hints + [dhMove]
  else
    ELDesigner1.Hints := ELDesigner1.Hints - [dhMove];
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then
    ELDesigner1.Hints := ELDesigner1.Hints + [dhInsert]
  else
    ELDesigner1.Hints := ELDesigner1.Hints - [dhInsert];
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
  Edit1.Enabled := CheckBox7.Checked;
end;

procedure TForm1.ELDesigner1ControlHint(Sender: TObject;
  AControl: TControl; var AHint: String);
begin
  if CheckBox7.Checked then
    AHint := Format(Edit1.Text, [AHint]);
end;

{ TMyPanel }

constructor TMyPanel.Create(AOwner: TComponent);
begin
  inherited;
  Color := clRed;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if ELDesigner1.Active then
    ELDesigner1.SelectAll;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  Nodelete1.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  Nomove1.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  Noresize1.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  Noinsertin1.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  Nocopy1.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  lmCustom11.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  lmCustom21.Enabled := (ELDesigner1.SelectedControls.Count = 1);
  if ELDesigner1.SelectedControls.Count = 1 then
  begin
    LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
    Nodelete1.Checked := lmNoDelete in LLockMode;
    Nomove1.Checked := lmNoMove in LLockMode;
    Noresize1.Checked := lmNoResize in LLockMode;
    Noinsertin1.Checked := lmNoInsertIn in LLockMode;
    NoCopy1.Checked := lmNoCopy in LLockMode;
    lmCustom11.Checked := lmCustom1 in LLockMode;
    lmCustom21.Checked := lmCustom2 in LLockMode;
  end
  else
  begin
    Nodelete1.Checked := False;
    Nomove1.Checked := False;
    Noresize1.Checked := False;
    Noinsertin1.Checked := False;
    NoCopy1.Checked := False;
    lmCustom11.Checked := False;
    lmCustom21.Checked := False;
  end;
  Copy1.Enabled := ELDesigner1.CanCopyToClipboard;
  Cut1.Enabled := ELDesigner1.CanCutToClipboard;
  Paste1.Enabled := ELDesigner1.CanPasteFromClipboard;
end;

procedure TForm1.Nodelete1Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmNoDelete in LLockMode then
    Exclude(LLockMode, lmNoDelete)
  else
    Include(LLockMode, lmNoDelete);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.NoMove1Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmNoMove in LLockMode then
    Exclude(LLockMode, lmNoMove)
  else
    Include(LLockMode, lmNoMove);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.Noresize1Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmNoResize in LLockMode then
    Exclude(LLockMode, lmNoResize)
  else
    Include(LLockMode, lmNoResize);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.Aligntogrid1Click(Sender: TObject);
begin
  ELDesigner1.SelectedControls.AlignToGrid;
end;

procedure TForm1.Noinsertin1Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmNoInsertIn in LLockMode then
    Exclude(LLockMode, lmNoInsertIn)
  else
    Include(LLockMode, lmNoInsertIn);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.Nocopy1Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmNoCopy in LLockMode then
    Exclude(LLockMode, lmNoCopy)
  else
    Include(LLockMode, lmNoCopy);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  ELDesigner1.CopyToClipboard;
end;

procedure TForm1.Cut1Click(Sender: TObject);
begin
  ELDesigner1.CutToClipboard;
end;

procedure TForm1.Paste1Click(Sender: TObject);
begin
  ELDesigner1.PasteFromClipboard;
end;

procedure TForm1.ELDesigner1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckBox9.Checked then
    Memo1.Lines.Add(Format('OnKeyDown: (Key: $%x)', [Key]));
  if Shift = [ssCtrl] then
  begin
    if (Key = Ord('C')) then ELDesigner1.CopyToClipboard;
    if (Key = Ord('X')) then ELDesigner1.CutToClipboard;
    if (Key = Ord('V')) then ELDesigner1.PasteFromClipboard;
  end;
end;

procedure TForm1.ELDesigner1ControlInserted(Sender: TObject);
begin
  RadioButton1.Checked := True;

  { This is TRadioButton bag. It always become checked when resieve focus.
    Setting ActiveControl to nil prevent another radio buttons of resieving
    focus when form is activated (focus changed from another form to this). }
  ActiveControl := nil;
end;

procedure TForm1.ELDesigner1ControlInserting(Sender: TObject;
  var AControlClass: TControlClass);
begin
  if RadioButton2.Checked then
    AControlClass := TEdit
  else
    if RadioButton3.Checked then
      AControlClass := TLabel
    else
      if RadioButton4.Checked then
        AControlClass := TMyPanel;
end;

procedure TForm1.ELDesigner1Change(Sender: TObject);
begin
  Memo1.Lines.Add('OnChange');
end;

procedure TForm1.ELDesigner1Notification(Sender: TObject;
  AnObject: TPersistent; Operation: TOperation);
var
  LS1, LS2: string;
begin
  if AnObject is TControl then
    LS1 := TControl(AnObject).Name;
  if Operation = opInsert then
    LS2 := 'Insert'
  else
    LS2 := 'Remove';
  Memo1.Lines.Add(Format('OnNotification: (AnObject.Name: %s Operation: %s)', [LS1, LS2]));
end;

procedure TForm1.ELDesigner1ValidateName(Sender: TObject;
  const AName: String; var AIsValidName: Boolean);
begin
  Memo1.Lines.Add(Format('OnValidateName: (AName: %s)', [AName]));
end;

procedure TForm1.ELDesigner1GetUniqueName(Sender: TObject;
  const ABaseName: String; var AUniqueName: String);
begin
  Memo1.Lines.Add(format('OnGetUniqueName: (ABaseName: %s)', [ABaseName]));
end;

procedure TForm1.ELDesigner1DblClick(Sender: TObject);
begin
  Memo1.Lines.Add('OnDblClick');
end;

procedure TForm1.lmCustom11Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmCustom1 in LLockMode then
    Exclude(LLockMode, lmCustom1)
  else
    Include(LLockMode, lmCustom1);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.lmCustom21Click(Sender: TObject);
var
  LLockMode: TELDesignerLockMode;
begin
  LLockMode := ELDesigner1.GetLockMode(ELDesigner1.SelectedControls[0]);
  if lmCustom2 in LLockMode then
    Exclude(LLockMode, lmCustom2)
  else
    Include(LLockMode, lmCustom2);
  ELDesigner1.LockControl(ELDesigner1.SelectedControls[0], LLockMode);
end;

procedure TForm1.ELDesigner1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LSource, LTarget: string;
begin
  if Source <> nil then
    LSource := (Source as TDragControlObject).Control.Name
  else
    LSource := 'nil';

  if (Source as TDragControlObject).DragTarget <> nil then
    LTarget := (TObject((Source as TDragControlObject).DragTarget) as TComponent).Name
  else
    LTarget := 'nil';
  Memo1.Lines.Add(Format('OnDragDrop: %s -> %s (X: %d, Y: %d)', [LSource, LTarget, X, Y]));
end;

procedure TForm1.ELDesigner1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  LSource, LTarget: string;
begin
  if Source <> nil then
    LSource := (Source as TDragControlObject).Control.Name
  else
    LSource := 'nil';
  if (Source as TDragControlObject).DragTarget <> nil then
    LTarget := (TObject((Source as TDragControlObject).DragTarget) as TComponent).Name
  else
    LTarget := 'nil';
  Memo1.Lines.Add(Format('OnDragOver: %s -> %s (X: %d, Y: %d)', [LSource, LTarget, X, Y]));
end;

{$IFDEF VER140}
procedure TForm1.ELDesigner1KeyPress(Sender: TObject; var Key: Char);
{$ENDIF}
{$IFDEF VER150}
procedure TForm1.ELDesigner1KeyPress(Sender: TObject; var Key: AnsiChar);
{$ENDIF}
begin
  if CheckBox9.Checked then
    Memo1.Lines.Add(Format('OnKeyPress: (Key: "%s")', [Key]));
end;

procedure TForm1.ELDesigner1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckBox9.Checked then
    Memo1.Lines.Add(Format('OnKeyUp: (Key: %x)', [Key]));
end;

procedure TForm1.ELDesigner1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LControl: TControl;
  LS: string;
begin
  if CheckBox8.Checked then
  begin
    LControl := ELDesigner1.ControlAtPos(Point(X, Y));
    if LControl <> nil then
      LS := LControl.Name
    else
      LS := 'nil';
    Memo1.Lines.Add(Format('OnMouseDown: (X: %d, Y: %d, ControlAtPos: "%s")', [X, Y, LS]));
  end;
end;

procedure TForm1.ELDesigner1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  LControl: TControl;
  LS: string;
begin
  if CheckBox8.Checked then
  begin
    LControl := ELDesigner1.ControlAtPos(Point(X, Y));
    if LControl <> nil then
      LS := LControl.Name
    else
      LS := 'nil';
    Memo1.Lines.Add(Format('OnMouseMove: (X: %d, Y: %d, ControlAtPos: "%s")', [X, Y, LS]));
  end;
end;

procedure TForm1.ELDesigner1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LControl: TControl;
  LS: string;
begin
  if CheckBox8.Checked then
  begin
    LControl := ELDesigner1.ControlAtPos(Point(X, Y));
    if LControl <> nil then
      LS := LControl.Name
    else
      LS := 'nil';
    Memo1.Lines.Add(Format('OnMouseUp: (X: %d, Y: %d, ControlAtPos: "%s")', [X, Y, LS]));
  end;
end;

procedure TForm1.ELDesigner1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if CheckBox8.Checked then
    Memo1.Lines.Add(Format('OnMouseWheel: (WheelDelta: %d, MousePos: (%d, %d))',
      [WheelDelta, MousePos.X, MousePos.Y]));
end;

procedure TForm1.ELDesigner1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if CheckBox8.Checked then
    Memo1.Lines.Add(Format('OnMouseWheelDown: (MousePos: (%d, %d))',
      [MousePos.X, MousePos.Y]));
end;

procedure TForm1.ELDesigner1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if CheckBox8.Checked then
    Memo1.Lines.Add(Format('OnMouseWheelUp: (MousePos: (%d, %d))',
      [MousePos.X, MousePos.Y]));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.SetEnabledtoTrue1Click(Sender: TObject);
var
  LI: Integer;
begin
  for LI := 0 to ELDesigner1.SelectedControls.Count - 1 do
    ELDesigner1.SelectedControls[LI].Enabled := True;
end;

procedure TForm1.SetEnabledtoFalse1Click(Sender: TObject);
var
  LI: Integer;
begin
  for LI := 0 to ELDesigner1.SelectedControls.Count - 1 do
    ELDesigner1.SelectedControls[LI].Enabled := False;
end;

initialization
  RegisterClasses([TEdit, TLabel, TMyPanel]);

end.
