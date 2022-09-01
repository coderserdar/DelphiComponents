unit bvSaveTableUnit;

interface

{$ifdef LINUX}
 ERROR: not compatible with LINUX
{$endif}

uses
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,DBTables,
  Classes,DB,bvdbgrid,bvLocalization,SysUtils;

type
  TSaveTableForm = class(TForm)
    BitBtnOk: TBitBtn;
    SaveDialog1: TSaveDialog;
    procedure BitBtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Grid:TDBGrid;
    ThData:TDBDataSet;
    EditFileFileName: String;
    SaverFileName:string;
  end;

procedure SaveTable(ThComp:TComponent;VarName:string='');

var
  SaveTableForm: TSaveTableForm;

implementation

uses {bvStringUtils,}bvSaveTablegetColumnsUnit, bvMessageUnit;

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure SaveTable(ThComp:TComponent;VarName:string);
var thFileName:string;
begin
    with TSaveTableForm.Create(Application) do
    try
      if ThComp is TDBGrid then begin
        Grid:=TDBGrid(ThComp);
        if Assigned(Grid.DataSource) and Assigned(Grid.DataSource.dataSet)
        then ThData:=TDBDataSet(Grid.DataSource.DataSet)
        else ThData:=nil;
      end
      else
      if ThComp is TDBDataSet then ThData:=TDBDataSet(ThComp)
      else ThData:=nil;

      thFileName:=VarName;
      if VarName<>'' then begin
         thFilename:=ExtractFilePath(Application.ExeName)+TableSaverDir+thFileName+TableSaverEXT;
      end;

  //    Grid:=ThGrid;
  //    ThData:=ThDataSet;

    If not SaveDialog1.Execute then Exit; //////////


      EditFileFileName:= SaveDialog1.FileName; //////////
      SaverFileName:=thFilename;
      BitBtnOk.Click; //////////

  //////////    ShowModal;
    finally
      free;
    end;
end;

procedure TSaveTableForm.BitBtnOkClick(Sender: TObject);
begin
    if not Self.ThData.Active then begin
      bvMessage(strErrorTableIsClosed);
      exit
    end
    else
    if not Assigned(grid) then bvMessage(StrErrorNotDefinedGrid)
    else if trim(EditFileFileName)='' then bvMessage(StrErrorNotDefinedSource)
  //////////  else if fileexists(EditFileFilename)
  //////////          and (MessageDlg('Вы хотите заменить существующий файл?',mtConfirmation,[mbOk,mbcancel],0)<>mrOk)
  //////////  then exit
    else
      with TSaveTableGetColumnsForm.Create(Self) do begin
        Grid:=Self.Grid;
        SaverFileName:=Self.SaverFileName;
        ThData:=Self.ThData;
        table.TableName:= EditFileFileName;  //////////
        Self.hide;

  //      if ExtractFileExt(EditFileFileName)='.XLS' then isExcel50:=true
  //      {else} IsExcel50:=false;

        ShowModal;
        free;
      end;
end;

procedure TSaveTableForm.FormCreate(Sender: TObject);
begin
  Grid:=nil;
  ThData:=nil;
end;

end.
