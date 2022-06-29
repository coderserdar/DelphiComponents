
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Picture editor             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_GEdit;

interface

{$I FR.inc}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, FR_Const;

type
  TfrGEditorForm = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    OpenDlg: TOpenDialog;
    CB1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frGEditorForm: TfrGEditorForm;

implementation

{$R *.DFM}

uses FR_Class, FR_Desgn;

procedure TfrGEditorForm.BitBtn1Click(Sender: TObject);
begin
{$IFDEF JPEG}
  OpenDlg.Filter := LoadStr(SPictFile) +
    ' (*.bmp *.jpg *.ico *.wmf *.emf)|*.bmp;*.jpg;*.ico;*.wmf;*.emf|' +
    LoadStr(SAllFiles) + '|*.*';
{$ELSE}
  OpenDlg.Filter := LoadStr(SPictFile) +
    ' (*.bmp *.ico *.wmf *.emf)|*.bmp;*.ico;*.wmf;*.emf|' +
    LoadStr(SAllFiles) + '|*.*';
{$ENDIF}
  if OpenDlg.Execute then
    Image1.Picture.LoadFromFile(OpenDlg.FileName);
end;

procedure TfrGEditorForm.CB1Click(Sender: TObject);
begin
  Image1.Stretch := CB1.Checked;
end;

procedure TfrGEditorForm.Button4Click(Sender: TObject);
begin
  Image1.Picture.Assign(nil);
end;

procedure TfrGEditorForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 460);
  CB1.Caption := LoadStr(frRes + 461);
  Button3.Caption := LoadStr(frRes + 462);
  Button4.Caption := LoadStr(frRes + 463);
  Button5.Caption := LoadStr(frRes + 464);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

procedure TfrGEditorForm.Button5Click(Sender: TObject);
begin
  TfrDesignerForm(frDesigner).ShowMemoEditor;
end;

end.

