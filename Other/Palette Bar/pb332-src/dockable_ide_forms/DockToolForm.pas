unit DockToolForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DeskForm, ExtCtrls, ToolWin, ComCtrls, IniFiles, IDECommandButton, DockForm,
  ActnList, Menus;

type
  TDockableToolbarForm = class(TDockableForm)
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    ToolbarPopupMenu: TPopupMenu;
    TextLabels1: TMenuItem;
    ToolActionList: TActionList;
    ToolbarCmd: TAction;
    TextLabelsCmd: TAction;
    PopupMenu1: TPopupMenu;
    Toolbar2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);    
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ToolbarCmdExecute(Sender: TObject);
    procedure TextLabelsCmdExecute(Sender: TObject);
    procedure ToolbarCmdUpdate(Sender: TObject);
    procedure TextLabelsCmdUpdate(Sender: TObject);
  private
    FLargeButtons: Boolean;
    function GetLargeButtons: Boolean;
    procedure SetLargeButtons(Value: Boolean);
  protected
    procedure ResizeButtons(Large: Boolean); virtual;
  public
    property LargeButtons: Boolean read GetLargeButtons write SetLargeButtons;
  end;

implementation

uses DeskStrs, DeskUtil;

{$R *.DFM}

const
  SmallToolbarSize = 30;
  SmallButtonHeight = 22;
  SmallButtonWidth = 23;
  LargeToolbarSize = 44;
  LargeButtonHeight = 36;
  LargeButtonWidth = 56;

procedure TDockableToolbarForm.FormCreate(Sender: TObject);
begin
  inherited;
  // Make sure default window size contains all speed buttons
  Toolbar1.Visible := IDEIniFile.ReadBool(Classname, ivToolBar, True);
  LargeButtons := IDEIniFile.ReadBool(Classname, ivTextLabels, True);
  Splitter1.Top := Toolbar1.Top + Toolbar1.Height;
  Splitter1.Visible := Toolbar1.Visible;
  { Toggle to force update }
  ResizeButtons(not FLargeButtons);
  ResizeButtons(not FLargeButtons);
end;

function TDockableToolbarForm.GetLargeButtons: Boolean;
begin
  Result := ToolBar1.Height > SmallToolBarSize;
end;

procedure TDockableToolbarForm.SetLargeButtons(Value: Boolean);
begin
  ResizeButtons(Value);
  IDEIniFile.WriteBool(Classname, ivTextLabels, Value);
end;

procedure TDockableToolbarForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  with Toolbar1 do
    if (Height >= LargeToolbarSize) then
      if (NewSize <= SmallToolbarSize) then
        NewSize := SmallToolbarSize
      else
        NewSize := LargeToolbarSize
    else
      if(NewSize >= LargeToolbarSize) then
        NewSize := LargeToolbarSize
      else
        NewSize := SmallToolbarSize;
end;

procedure TDockableToolbarForm.Splitter1Moved(Sender: TObject);
begin
  ResizeButtons(ToolBar1.Height >= LargeToolbarSize);
  IDEIniFile.WriteBool(Classname, ivTextLabels, LargeButtons);
end;

procedure TDockableToolbarForm.ResizeButtons(Large: Boolean);
var
  I, X: Integer;
  NewLargeWidth, NewLargeHeight: Integer;
begin
  if Large <> FLargeButtons then
  begin
    with ToolBar1 do
    begin
      Perform(WM_SETREDRAW, 0, 0);
      try
        if Large then
        begin
          NewLargeWidth := MulDiv(LargeButtonWidth, PixelsPerInch, 96);
          NewLargeHeight := LargeButtonHeight + MulDiv(13{Height of text}, PixelsPerInch, 96) - 13;
          { Large buttons }
          for I := 0 to ControlCount - 1 do
            if Controls[I] is TCommandButton then
            begin
              { Preserve any differences in width from the standard width }
              if TCommandButton(Controls[I]).DropDownMenu <> nil then
                X := 13 else
                X := 0;
              Controls[I].Width := NewLargeWidth + X{! + (Controls[I].Width - SmallButtonWidth)!};
              TCommandButton(Controls[I]).ShowCaption := True;
            end;
          ButtonHeight := NewLargeHeight;
          //! Take into account large font systems
          Height := ButtonHeight + 8;//!LargeToolbarSize;
          ShowHint := False;
        end
        else
        begin
          { Small buttons }
          for I := 0 to ControlCount - 1 do
            if Controls[I] is TCommandButton then
            begin
              { Preserve any differences in width from the standard width }
              if TCommandButton(Controls[I]).DropDownMenu <> nil then
                X := 13 else
                X := 0;
              Controls[I].Width := SmallButtonWidth + X{! + (Controls[I].Width - LargeButtonWidth)!};
              TCommandButton(Controls[I]).ShowCaption := False;
            end;
          ButtonHeight := SmallButtonHeight;
          Height := SmallToolbarSize;
          ShowHint := True;
        end;
      finally
        Toolbar1.Perform(CM_RECREATEWND, 0, 0);
        Perform(WM_SETREDRAW, 1, 0);
        Invalidate;
      end;
    end;
    FLargeButtons := Large;
  end;
end;

procedure TDockableToolbarForm.ToolbarCmdExecute(Sender: TObject);
begin
  with ToolbarCmd do
  begin
    ToolBar1.Visible := not Checked;
    Splitter1.Top := Toolbar1.Top + Toolbar1.Height;
    Splitter1.Visible := Toolbar1.Visible;
    IDEIniFile.WriteBool(Self.Classname, ivToolBar, ToolBar1.Visible);
  end;
end;

procedure TDockableToolbarForm.TextLabelsCmdExecute(Sender: TObject);
begin
  with TextLabelsCmd do
    LargeButtons := not Checked;
end;

procedure TDockableToolbarForm.ToolbarCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Toolbar1.Visible;
end;

procedure TDockableToolbarForm.TextLabelsCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := LargeButtons;
end;

procedure TDockableToolbarForm.FormDestroy(Sender: TObject);
begin
  inherited;
  {}
end;

end.
