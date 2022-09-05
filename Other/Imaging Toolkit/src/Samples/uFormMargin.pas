// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17607: uFormMargin.pas 
//
//    Rev 1.1    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.0    27-05-2002 16:22:36  mcm

unit uFormMargin;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, 
     {$ENDIF}
     umcmIntE;

type
  TFormPageMargin = class(TForm)
    btnOK         : TButton;
    btnCancel     : TButton;
    gbMargin      : TGroupBox;
    lLeft         : TLabel;
    lTop          : TLabel;
    lRight        : TLabel;
    lBottom       : TLabel;
    rsLeft        : TmcmRealSpin;
    rsTop         : TmcmRealSpin;
    rsRight       : TmcmRealSpin;
    rsBottom      : TmcmRealSpin;
    cbForceMargin : TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    function    GetForceMargin : boolean;
    function    GetMarginBottom : double;
    function    GetMarginLeft : double;
    function    GetMarginRight : double;
    function    GetMarginTop : double;
    procedure   SetForceMargin(Value : boolean);
    procedure   SetMarginBottom(Value : double);
    procedure   SetMarginLeft(Value : double);
    procedure   SetMarginRight(Value : double);
    procedure   SetMarginTop(Value : double);
  public
    { Public declarations }
    property    ForceMargin : boolean
      read      GetForceMargin
      write     SetForceMargin;
    property    MarginBottom : double
      read      GetMarginBottom
      write     SetMarginBottom;
    property    MarginLeft : double
      read      GetMarginLeft
      write     SetMarginLeft;
    property    MarginRight : double
      read      GetMarginRight
      write     SetMarginRight;
    property    MarginTop : double
      read      GetMarginTop
      write     SetMarginTop;
  end;

var
  FormPageMargin: TFormPageMargin;

implementation

{$R *.DFM}

procedure TFormPageMargin.FormCreate(Sender : TObject);
begin
;
end; // TFormPageMargin.FormCreate.


procedure TFormPageMargin.btnOKClick(Sender : TObject);
begin
;
end; // TFormPageMargin.btnOKClick.


function TFormPageMargin.GetForceMargin : boolean;
begin
  Result := cbForceMargin.Checked;
end; // TFormPageMargin.GetForceMargin.


procedure TFormPageMargin.SetForceMargin(Value : boolean);
begin
  cbForceMargin.Checked := Value;
end; // TFormPageMargin.SetForceMargin.


function TFormPageMargin.GetMarginBottom : double;
begin
  Result := rsBottom.Value;
end; // TFormPageMargin.GetMarginBottom.


function TFormPageMargin.GetMarginLeft : double;
begin
  Result := rsLeft.Value;
end; // TFormPageMargin.GetMarginLeft.


function TFormPageMargin.GetMarginRight : double;
begin
  Result := rsRight.Value;
end; // TFormPageMargin.GetMarginRight.


function TFormPageMargin.GetMarginTop : double;
begin
  Result := rsTop.Value;
end; // TFormPageMargin.GetMarginTop.


procedure TFormPageMargin.SetMarginBottom(Value : double);
begin
  rsBottom.Value := Value;
end; // TFormPageMargin.SetMarginBottom.


procedure TFormPageMargin.SetMarginLeft(Value : double);
begin
  rsLeft.Value := Value;
end; // TFormPageMargin.SetMarginLeft.


procedure TFormPageMargin.SetMarginRight(Value : double);
begin
  rsRight.Value := Value;
end; // TFormPageMargin.SetMarginRight.


procedure TFormPageMargin.SetMarginTop(Value : double);
begin
  rsTop.Value := Value;
end; // TFormPageMargin.SetMarginTop.



end.
