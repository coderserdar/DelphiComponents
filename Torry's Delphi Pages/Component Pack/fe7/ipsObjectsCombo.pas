unit ipsObjectsCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  iFormEditor;

type
  TpsObjectsCombo = class(TCustomComboBox)
  private
    FFormEdit: TpsFormEditor;
    FImageList: TImageList;
    procedure SetFormEdit(const Value: TpsFormEditor);
    procedure BuildObjectsCombo;
  protected
  public
        Constructor Create(AOwner:TComponent); override;
        procedure DrawItem(Index: Integer; Rect: TRect;
                State: TOwnerDrawState); override;
        procedure DoEnter; override;
        procedure Change; override;
  published
        property FormEdit:TpsFormEditor read FFormEdit write SetFormEdit;
        property ImageList:TImageList read FImageList write FImageList;
        property ItemHeight;
  end;

implementation


{ TpsObjectsCombo }

procedure TpsObjectsCombo.SetFormEdit(const Value: TpsFormEditor);
begin
  FFormEdit := Value;
end;

procedure TpsObjectsCombo.BuildObjectsCombo;
var i:Integer;
    C:TControl;
begin
        Items.Clear;
        if (FFormEdit<>nil) and (FFormEdit.EditedControl<>nil) then
            for i:= 0 to FFormEdit.EditedControl.ControlCount-1 do begin
                C := FFormEdit.EditedControl.Controls[i];
                if not C.InheritsFrom(TpsFeWindow) then
                        Items.AddObject('', C);
            end;
end;

constructor TpsObjectsCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormEdit  := nil;
  FImageList := nil;
  Style      := csOwnerDrawFixed;
  ItemHeight := 18;
end;

procedure TpsObjectsCombo.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var R:TRect;
    C:TControl;
    I:TpsClassItem;
begin
        inherited;
        if (FFormEdit=nil) or (FFormEdit.ClassList=nil) then Exit;

        C:=TControl(Items.Objects[Index]);
        if C<>nil then begin
                I:=FFormEdit.ClassList.GetClassControl(C);

                R       := Rect;
                R.Right := R.Left + 30;

                if FImageList<>nil then
                        FImageList.Draw(Canvas, R.Left, R.Top, I.ImageIndex, True); 
                Inc(R.Left, 32);
                R.Right := Rect.Right;
                Canvas.TextRect(R, R.Left, R.Top, C.Name+' : '+I.Name);
        end;
end;


procedure TpsObjectsCombo.DoEnter;
begin
  inherited;
  BuildObjectsCombo;

end;

procedure TpsObjectsCombo.Change;
begin
  inherited Change;
  if FFormEdit<>nil then
        if ItemIndex>=0 then
                FFormEdit.AddHandledControl(TControl(Items.Objects[ItemIndex]), True);      
end;

end.
