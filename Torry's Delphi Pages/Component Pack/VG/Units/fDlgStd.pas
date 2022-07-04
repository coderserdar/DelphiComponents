{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Standart dialog                               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit fDlgStd;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls;

type
  TDialogForm = class(TForm)
    paBottom: TPanel;
    paBottomRight: TPanel;
    cmOK: TButton;
    cmCancel: TButton;
    cmHelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure cmHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure CloseForm;
  end;

implementation
uses Consts, vgUtils, vgVCLUtl;

{$R *.DFM}

constructor TDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  cmOK.Caption := ResStr(SOKButton);
  cmCancel.Caption := ResStr(SCancelButton);
  cmHelp.Caption := ResStr(SHelpButton);
end;

procedure TDialogForm.CloseForm;
begin
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TDialogForm.FormShow(Sender: TObject);
begin
  AdjustLabelsBounds(Self);
end;

procedure TDialogForm.cmHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
