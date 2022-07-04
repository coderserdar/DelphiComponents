unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgScript, vgMSScr, ComCtrls, ExtCtrls, vgCtrls;

type
  TMainForm = class(TForm)
    sc: TMSSCScript;
    pc: TPageControl;
    tsVB: TTabSheet;
    meVB: TMemo;
    GroupBox1: TGroupBox;
    cmDelphiMsgBox: TButton;
    Label1: TLabel;
    Label2: TLabel;
    cmDelphiFont: TButton;
    gbExec: TGroupBox;
    cmExecute: TButton;
    GroupBox2: TGroupBox;
    vgLabel1: TvgLabel;
    Label3: TLabel;
    procedure cmExecuteClick(Sender: TObject);
    procedure scCreateObject(Sender: TObject; ProxyServer: TObjectProxyServer;
      var Obj: OleVariant);
    procedure TFormShowModal(Method: TDispatchMethod;
      AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant; var ErrorCode: HRESULT);
    procedure cmDelphiMsgBoxClick(Sender: TObject);
    procedure cmDelphiFontClick(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  MainForm: TMainForm;

implementation
uses vgUtils, ComObj;

{$R *.DFM}

const
  STFormShowModal         = 'ShowModal';
  DISPID_TFormShowModal   = DISPID_OBJECTPROXUSER;

procedure TMainForm.cmExecuteClick(Sender: TObject);
var
  Params: Variant;
begin
  with sc.ProxyServer do
  begin
    { Component classes to create from script }
    ComponentClasses.RegisterClass(TForm, nil, '', False);
    ComponentClasses.RegisterClass(TPanel, nil, '', False);
    ComponentClasses.RegisterClass(TButton, nil, '', False);
    ComponentClasses.RegisterClass(TLabel, nil, '', False);
    ComponentClasses.RegisterClass(TListBox, nil, '', False);
  end;

  if pc.ActivePage = tsVB then
  begin
    sc.Lines := meVB.Lines;
    sc.Language := LangVBScript;
  end;
  sc.Execute(Params);
end;

procedure TMainForm.scCreateObject(Sender: TObject; ProxyServer: TObjectProxyServer; var Obj: OleVariant);
begin
  Obj := TObjectProxy.Create(Self, ProxyServer) as IDispatch;
end;

procedure TMainForm.TFormShowModal(Method: TDispatchMethod;
  AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant; var ErrorCode: HRESULT);
var
  MR: TModalResult;
begin
  if Args.Count = 0 then
  begin
    MR := (AObjectProxy.GetObject as TForm).ShowModal;
    SetVarResult(VarResult, MR);
    ErrorCode := S_OK;
  end else
    ErrorCode := DISP_E_BADPARAMCOUNT;
end;

procedure TMainForm.cmDelphiMsgBoxClick(Sender: TObject);
const
  SMsg = 'Event handler in Delphi code is invoked by %s';
begin
  ShowMessage(Format(SMsg, [TComponent(Sender).Name]));
end;

procedure TMainForm.cmDelphiFontClick(Sender: TObject);
begin
  ShowMessage('Please execute the script first');
end;

end.
