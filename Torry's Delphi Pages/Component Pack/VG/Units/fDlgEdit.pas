{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Standart dialog  for editing TDataSet         }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit fDlgEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, fDlgStd, Db;

const
  WM_UPDATERECORD = WM_USER + 1;

type
  TDialogEditForm = class(TDialogForm)
    dsEdit: TDataSource;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure StateChange(Sender: TObject);
  private
    { Private declarations }
    procedure WMUpdateRecord(var Message: TMessage); message WM_UPDATERECORD;
  protected
    function PostRecord(Apply: Boolean): Boolean; virtual;
    function UpdateRecord: Boolean;
    procedure UpdateRecordPaused;
    procedure OpenTables; virtual;
  public
    { Public declarations }
  end;

  TDialogEditFormClass = class of TDialogEditForm;

implementation
uses vgVCLUtl, vgDBUtl;

{$R *.DFM}

procedure TDialogEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  try
    CanClose := PostRecord(ModalResult = mrOK);
  except
    Application.HandleException(Self);
    CanClose := False;
  end;
end;

procedure TDialogEditForm.FormShow(Sender: TObject);
begin
  inherited;
  AppSetCursor(crSQLWait);
  try
    try
      OpenTables;
    except
      CloseForm;
      raise;
    end;
    StateChange(nil);
  finally
    AppRestoreCursor;
  end;
end;

procedure TDialogEditForm.OpenTables;
begin
end;

function TDialogEditForm.PostRecord(Apply: Boolean): Boolean;
begin
  if dsEdit.DataSet = nil then
  begin
    Result := True;
    Exit;
  end;

  if Apply then UpdateRecord;
  Result := DataSetPost(dsEdit.DataSet, Apply);
end;

function TDialogEditForm.UpdateRecord: Boolean;
begin
  try
    vgDBUtl.UpdateRecord(dsEdit.DataSet);
    Result := True;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

procedure TDialogEditForm.UpdateRecordPaused;
begin
  PostMessage(Handle, WM_UPDATERECORD, 0, 0);
end;

procedure TDialogEditForm.WMUpdateRecord(var Message: TMessage);
begin
  inherited;
  UpdateRecord;
end;

procedure TDialogEditForm.StateChange(Sender: TObject);
begin
  SetLabelColors(Self);
end;

end.
