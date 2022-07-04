unit FileNameDialog;

interface

uses
  SysUtils, Classes, Forms;

type
  TFileNameDialog = class(TComponent)
  private
    FTitle: string;
    FKom: string;
    FFileName: TFileName;
    FFormPosition: TPosition;
    FThread: TThread;
    FResultSyn: Boolean;
    { Private declarations }
  protected
    procedure ExecuteSyn;
  public
    { Public declarations }
    function Execute: Boolean;
  published
    { Published declarations }
    property FileName: TFileName read FFileName write FFileName;
    property Title: string read FTitle write FTitle;
    property Kom: string read FKom write FKom;
    property FormPosition: TPosition read FFormPosition write FFormPosition default poDesigned;
    property Thread: TThread read FThread write FThread;
  end;

implementation
uses Controls, F_FileName, Dialogs;

{ TFileNameDialog }

function TFileNameDialog.Execute: Boolean;
begin
  Result:= False;
  if FThread <> nil then
  begin
    FResultSyn:= False;
    FThread.Synchronize(FThread, ExecuteSyn);
    Result:= FResultSyn;
  end else
  with TFormFileName.Create(Self) do
  try
    Position:= FFormPosition;
    Caption:= FTitle;
    LabelKom.Caption:= FKom;
    EditFileName.Text:= FFileName;
    if ShowModal = mrOk then
    begin
      FFileName:= EditFileName.Text;
      Result:= True;
    end;
  finally
    Free;
  end;
end;

procedure TFileNameDialog.ExecuteSyn;
begin
  FResultSyn:= False;
  with TFormFileName.Create(Self) do
  try
    Position:= FFormPosition;
    Caption:= FTitle;
    LabelKom.Caption:= FKom;
    EditFileName.Text:= FFileName;
    if ShowModal = mrOk then
    begin
      FFileName:= EditFileName.Text;
      FResultSyn:= True;
    end;
  finally
    Free;
  end;
end;

end.
