unit PersonEdit;

interface
{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grids, DBGrids, Mask, DBCtrls, ComCtrls, Buttons, ToolWin, ImgList,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls,
  QGrids, QDBGrids, QMask, QDBCtrls, QComCtrls, QButtons, QImgList,
{$ENDIF}
  ContactEdit, DB, InstantPresentation, Model;

type
  TPersonEditForm = class(TContactEditForm)
    BirthDateEdit: TDBEdit;
    BirthDateLabel: TLabel;
    EmployerEdit: TDBEdit;
    EmployerLabel: TLabel;
    EmailsGrid: TDBGrid;
    EmailsExposer: TInstantExposer;
    EmailsLabel: TLabel;
    EmailsSource: TDataSource;
    EmployerToolBar: TToolBar;
    EmployerEditButton: TToolButton;
    EmployerLookupButton: TToolButton;
    EmployerClearButton: TToolButton;
    EmployerToolImages: TImageList;
    PictureButton: TButton;
    SalaryLabel: TLabel;
    SalaryEdit: TDBEdit;
    PicturePanel: TPanel;
    PictureImage: TImage;
    ClearButton: TButton;
    procedure EmployerClearButtonClick(Sender: TObject);
    procedure EmployerEditButtonClick(Sender: TObject);
    procedure EmployerLookupButtonClick(Sender: TObject);
    procedure SubjectExposerIncludeField(Sender: TObject;
      const FieldName: String; var Include: Boolean);
    procedure PictureButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    function GetSubject: TPerson;
    procedure SetSubject(const Value: TPerson);
  protected
    procedure UpdateActions; override;
  public
    class function ObjectClass: TClass; override;
    property Subject: TPerson read GetSubject write SetSubject;
  end;

implementation

{$R *.dfm}

uses
  BasicEdit, CompanyBrowse,
{$IFDEF MSWINDOWS}
  ExtDlgs, JPeg,
{$ENDIF}
  InstantImageUtils;

{ TPersonEditForm }

procedure TPersonEditForm.EmployerClearButtonClick(Sender: TObject);
begin
  Subject.EmployBy(nil);
end;

procedure TPersonEditForm.EmployerEditButtonClick(Sender: TObject);
var
  Employer: TCompany;
begin
  Employer := Subject.Employer;
  if not Assigned(Employer) then
  begin
    Employer := TCompany.Create;
    try
      if EditObject(Employer) then
      begin
        SubjectExposer.PostChanges;
        Subject.EmployBy(Employer);
      end;
    finally
      Employer.Free;
    end;
  end else
    EditObject(Employer);
end;

procedure TPersonEditForm.EmployerLookupButtonClick(Sender: TObject);
begin
  with TCompanyBrowseForm.Create(nil) do
  try
    LookupMode := True;
    Selected := Subject.Employer;
    if Execute then
    begin
      SubjectExposer.PostChanges;
      Subject.EmployBy(Selected);
    end;
  finally
    Free;
  end;
end;

function TPersonEditForm.GetSubject: TPerson;
begin
  Result := inherited Subject as TPerson;
end;

class function TPersonEditForm.ObjectClass: TClass;
begin
  Result := TPerson;
end;

procedure TPersonEditForm.PictureButtonClick(Sender: TObject);
var
  Picture: TPicture;
begin
{$IFDEF MSWINDOWS}
  with TOpenPictureDialog.Create(nil) do
{$ENDIF}
{$IFDEF LINUX}
  with TOpenDialog.Create(nil) do
{$ENDIF}
  try
    if Execute then
    begin
//      SubjectExposer.Edit;
      Picture := TPicture.Create;
      try
        Picture.LoadFromFile(FileName);
        PictureImage.Picture.Assign(Picture);
        Subject._Picture.AssignPicture(Picture);
      finally
        Picture.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TPersonEditForm.SetSubject(const Value: TPerson);
begin
  inherited Subject := Value;
end;

procedure TPersonEditForm.SubjectExposerIncludeField(Sender: TObject;
  const FieldName: String; var Include: Boolean);
begin
  inherited;
  if FieldName = 'MainEmailAddress' then
    Include := False;
end;

procedure TPersonEditForm.UpdateActions;
begin
  inherited;
  EmployerClearButton.Enabled := Assigned(Subject.Employer);
end;

procedure TPersonEditForm.FormCreate(Sender: TObject);
begin
  inherited;
{$IFDEF MSWINDOWS}
  LoadMultipleImages(EmployerToolImages,'PERSONEMPLOYERTOOLIMAGES',HInstance);
  {$IFNDEF VER130}
  PictureImage.Proportional := True;
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  LoadMultipleImages(EmployerToolImages,ExtractFilePath(Application.ExeName)+'PERSONEMPLOYERTOOLIMAGES.BMP');
{$ENDIF}
end;

procedure TPersonEditForm.FormShow(Sender: TObject);
begin
  inherited;
  Subject._Picture.AssignToPicture(PictureImage.Picture);
end;

procedure TPersonEditForm.ClearButtonClick(Sender: TObject);
begin
  inherited;
  PictureImage.Picture := nil;
  Subject._Picture.Clear;
end;

initialization
  TPersonEditForm.RegisterClass;

end.
