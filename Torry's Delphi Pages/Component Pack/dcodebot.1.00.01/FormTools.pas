
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FormTools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, TypInfo;

{ AnchorData }

type
  TAnchorData = record
    Control: TControl;
    Anchors: TAnchors;
  end;

  TAnchorDataArray = array of TAnchorData;

function SaveAnchors(Container: TWinControl): TAnchorDataArray;
procedure RestoreAnchors(const Data: TAnchorDataArray);

type
	TVisibleData = record
    Control: TControl;
    Visible: Boolean;
  end;

  TVisibleDataArray = array of TVisibleData;

function SaveVisible(Container: TWinControl): TVisibleDataArray;
procedure RestoreVisible(const Data: TVisibleDataArray);

{ TControlTemplate class }

type
	TControlTemplate = class
  private
    FParent: TWinControl;
    FTemplate: TWinControl;
  	FControls: TList;
  public
  	constructor Create(Template: TWinControl);
    destructor Destroy; override;
    procedure Insert(Parent: TWinControl);
  end;

implementation

function SaveAnchors(Container: TWinControl): TAnchorDataArray;
var
  I: Integer;
begin
  SetLength(Result, Container.ControlCount);
  for I := 0 to Container.ControlCount - 1 do
  begin
    Result[I].Control := Container.Controls[I];
    with Result[I] do
    begin
      Anchors := Control.Anchors;
      Control.Anchors := [akTop, akLeft];
    end;
  end;
end;

procedure RestoreAnchors(const Data: TAnchorDataArray);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
    with Data[I] do
	     Control.Anchors := Anchors;
end;

function SaveVisible(Container: TWinControl): TVisibleDataArray;
var
  I: Integer;
begin
  SetLength(Result, Container.ControlCount);
  for I := 0 to Container.ControlCount - 1 do
  begin
    Result[I].Control := Container.Controls[I];
    with Result[I] do
    begin
      Visible := Control.Visible;
      Control.Visible := False;
    end;
  end;
end;

procedure RestoreVisible(const Data: TVisibleDataArray);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
    with Data[I] do
	     Control.Visible := Visible;
end;

constructor TControlTemplate.Create(Template: TWinControl);
var
	I: Integer;
begin
  FParent := Template;
	FTemplate := Template;
	FControls := TList.Create;
  for I := 0 to Template.ControlCount - 1 do
  	FControls.Add(Template.Controls[I]);
end;

destructor TControlTemplate.Destroy;
begin
	FControls.Free;
end;

procedure TControlTemplate.Insert(Parent: TWinControl);
var
	Control: TControl;
  AnchorsProp: PPropInfo;
  Anchors: TAnchors;
	X1, X2, Y1, Y2: Integer;
  I: Integer;
begin
	if FParent = Parent then
  	Exit;
  FParent := Parent;
  if FParent = nil then
  	FParent := FTemplate;
  for I := 0 to FControls.Count - 1 do
  begin
    Control := TControl(FControls[I]);
    if Control.Parent = FParent then Continue;
    AnchorsProp := GetPropInfo(Control, 'Anchors', [tkSet]);
    if AnchorsProp <> nil then
    begin
	    Byte(Anchors) := GetOrdProp(Control, AnchorsProp);
      X1 := Control.Left;
      X2 := Control.Parent.ClientWidth - (Control.Left + Control.Width);
      Y1 := Control.Top;
      Y2 := Control.Parent.ClientHeight - (Control.Top + Control.Height);
    end
    else
    begin
    	Anchors := [];
      X1 := 0;
      X2 := 0;
      Y1 := 0;
      Y2 := 0;
    end;
    Control.Parent := FParent;
  	if [akLeft, akRight] * Anchors  = [akLeft, akRight] then
    begin
    	Control.Left := X1;
      Control.Width := FParent.ClientWidth - (X1 + X2);
    end
    else if [akLeft] * Anchors  = [akLeft] then
    	Control.Left := X1
    else if [akRight] * Anchors  = [akRight] then
    	Control.Left := FParent.ClientWidth - (Control.Width + X2);
  	if [akTop, akBottom] * Anchors  = [akTop, akBottom] then
    begin
    	Control.Top := Y1;
      Control.Height := FParent.ClientHeight - (Y1 + Y2);
    end
    else if [akTop] * Anchors  = [akTop] then
    	Control.Top := Y1
    else if [akBottom] * Anchors  = [akBottom] then
    	Control.Top := FParent.ClientHeight - (Control.Height + Y2);
  end;
end;

end.
 