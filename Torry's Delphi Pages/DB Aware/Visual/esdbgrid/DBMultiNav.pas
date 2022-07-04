unit DBMultiNav;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, DBCtrls, Forms, ESDBGrids, DB;

type
  TDBMultiNav = class(TDBNavigator)
  private
    { Private declarations }
    fConfirmDelete: Boolean;
    fDBGrid: TESDBGrid;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BtnClick(Index: TNavigateBtn); override;
  published
    { Published declarations }
    property ConfirmDelete: Boolean read fConfirmDelete write fConfirmDelete;
    property DBGrid: TESDBGrid read fDBGrid write fDBGrid;
  end;

///procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Data Controls', [TDBMultiNav]);
//end;

{ TDBMultiNav }

Constructor TDBMultiNav.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ConfirmDelete:=False;
  Hints.Clear;
  Hints.Add('Первая');
  Hints.Add('Предыдущая');
  Hints.Add('Следующая');
  Hints.Add('Последняя');
  Hints.Add('Добавить');
  Hints.Add('Удалить');
  Hints.Add('Изменить');
  Hints.Add('Записать');
  Hints.Add('Отменить изменения');
  Hints.Add('Обновить');
  ShowHint:=True;
  fConfirmDelete:=True;
end;

procedure TDBMultiNav.BtnClick(Index: TNavigateBtn);
begin
  If Index=nbDelete then
  begin
    If fConfirmDelete then
      If (DataSource.DataSet.State<>dsInsert) then
      begin
        If Application.MessageBox('Удалить выбранные записи?','Внимание',MB_YESNO)=ID_NO then
          exit;
      end
      else
      begin
        DataSource.DataSet.Refresh;
        Exit;
      end;
    if DBGrid<>nil then
    begin
      If DBGrid.SelectedRows.Count=0 then
      begin
        DataSource.DataSet.UpdateCursorPos;
        DataSource.DataSet.Resync([]);
        DataSource.DataSet.Delete;
        if Assigned(OnClick) then
          OnClick(Self, Index);
        Exit;
      end;
      DBGrid.SelectedRows.Delete;
      if Assigned(OnClick) then
        OnClick(Self, Index);
      exit;
    end;
  end;
  inherited;
end;

end.
