{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.02.01 / 01.11.1999                        }
{Comment:   NC OCI8 VCL: Oracle8 error handling dialog  }
{Copyright: Portions (c) 1999, Dmitry Arefiev           }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{           Based on Borland Delphi DBExcept.pas        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciErrorDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, NCOciWrapper;

type
  TNCOciErrorFrm = class(TForm)
    BasicPanel: TPanel;
    DetailsPanel: TPanel;
    NativeLabel: TLabel;
    DbMessageText: TMemo;
    NativeResult: TEdit;
    ButtonPanel: TPanel;
    IconPanel: TPanel;
    IconImage: TImage;
    TopPanel: TPanel;
    ErrorText: TLabel;
    RightPanel: TPanel;
    OKBtn: TBitBtn;
    DetailsBtn: TBitBtn;
    NextBtn: TBitBtn;
    BackBtn: TBitBtn;
    Label1: TLabel;
    ServerObject: TEdit;
    procedure FormShow(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPrevOnException: TExceptionEvent;
    FOCINativeError: EOCINativeError;
    FDetailsHeight, CurItem: Integer;
    FDetails: string;
    FPrevWidth: Integer;
    FSwitchingDetails: Boolean;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure SwitchDetails;
    procedure ShowError;
    function ShowException(Error: EOCINativeError): TModalResult;
    property OCINativeError: EOCINativeError read FOCINativeError write FOCINativeError;
  public
    class procedure HookExceptions;
    class procedure UnHookExceptions;
  end;

var
  NCOciErrorFrm: TNCOciErrorFrm;

implementation

{$R *.DFM}

procedure TNCOciErrorFrm.HandleException(Sender: TObject; E: Exception);
begin
    if (E is EOCINativeError) and (OCINativeError = nil) and
       not Application.Terminated then
        ShowException(EOCINativeError(E))
    else if Assigned(FPrevOnException) then
        FPrevOnException(Sender, E)
    else
        Application.ShowException(E);
end;

procedure TNCOciErrorFrm.SwitchDetails;
const
    DetailsOn: array [Boolean] of string = ('%s >>', '<< %s');
var
    DEnabling: Boolean;
begin
    DEnabling := not DetailsPanel.Visible;
    FSwitchingDetails := True;
    try
        if DEnabling then
            ClientHeight := FDetailsHeight
        else
            ClientHeight := DetailsPanel.Top;
        DetailsPanel.Visible := DEnabling;
        ButtonPanel.Top := 0;
        DetailsBtn.Caption := Format(DetailsOn[DEnabling], [FDetails]);
    finally
        FSwitchingDetails := False;
    end;
end;

procedure TNCOciErrorFrm.ShowError;
var
    errMsg: TOCIErrorMessage;
    s: String;
    i: Integer;
begin
    BackBtn.Enabled := CurItem > 0;
    NextBtn.Enabled := CurItem < OCINativeError.ErrorCount - 1;
    if OCINativeError.ErrorCount > 0 then begin
        errMsg := OCINativeError.Errors[CurItem];
        DbMessageText.Text := errMsg.Message;
        s := IntToStr(errMsg.ErrorCode);
        for i := 1 to 5 - Length(s) do
            s := '0' + s;
        NativeResult.Text := s;
        ServerObject.Text := errMsg.ServerObject;
    end
    else begin
        DbMessageText.Text := '';
        NativeResult.Text := '';
        ServerObject.Text := '';
    end;
end;

procedure TNCOciErrorFrm.FormCreate(Sender: TObject);
begin
    FDetailsHeight := ClientHeight;
    FDetails := DetailsBtn.Caption;
    SwitchDetails;
end;

procedure TNCOciErrorFrm.FormShow(Sender: TObject);
begin
    ErrorText.Caption := OCINativeError.Message;
    if DetailsPanel.Visible then begin
        CurItem := 0;
        ShowError;
    end;
    FPrevWidth := ClientWidth;
end;

procedure TNCOciErrorFrm.BackClick(Sender: TObject);
begin
    Dec(CurItem);
    ShowError;
end;

procedure TNCOciErrorFrm.NextClick(Sender: TObject);
begin
    Inc(CurItem);
    ShowError;
end;

procedure TNCOciErrorFrm.DetailsBtnClick(Sender: TObject);
begin
    SwitchDetails;
    if DetailsPanel.Visible then begin
        CurItem := 0;
        ShowError;
    end;
end;

class procedure TNCOciErrorFrm.HookExceptions;
begin
    if NCOciErrorFrm = nil then begin
        NCOciErrorFrm := TNCOciErrorFrm.Create(nil);
        NCOciErrorFrm.FPrevOnException := Application.OnException;
        Application.OnException := NCOciErrorFrm.HandleException;
    end;
end;

class procedure TNCOciErrorFrm.UnHookExceptions;
begin
    if NCOciErrorFrm <> nil then begin
        Application.OnException := NCOciErrorFrm.FPrevOnException;
        NCOciErrorFrm.Free;
        NCOciErrorFrm := nil;
    end;
end;

function TNCOciErrorFrm.ShowException(Error: EOCINativeError): TModalResult;
begin
    OCINativeError := Error;
    Result := ShowModal;
    OCINativeError := nil;
end;

procedure TNCOciErrorFrm.FormResize(Sender: TObject);
begin
    if (FPrevWidth <> ClientWidth) and not FSwitchingDetails then begin
        DetailsBtn.Left := DetailsBtn.Left + ClientWidth - FPrevWidth;
        OKBtn.Left := OKBtn.Left + ClientWidth - FPrevWidth;
        ServerObject.Width := ServerObject.Width + ClientWidth - FPrevWidth;
        DbMessageText.Width := DbMessageText.Width + ClientWidth - FPrevWidth;
        BackBtn.Left := BackBtn.Left + ClientWidth - FPrevWidth;
        NextBtn.Left := NextBtn.Left + ClientWidth - FPrevWidth;
    end;
    FPrevWidth := ClientWidth;
end;

initialization
    NCOciErrorFrm := nil;

finalization
    TNCOciErrorFrm.UnHookExceptions;

end.
