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
// $Log:  21509: uFormOCRText.pas 
//
//   Rev 1.0    25-09-2003 23:25:48  mcm    Version: IMG 1.5

unit uFormOCRText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormOCRText = class(TForm)
    Memo : TMemo;
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FormOCRText : TFormOCRText;

implementation

{$R *.DFM}

procedure TFormOCRText.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  Action := caFree;
end;

procedure TFormOCRText.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  CanClose := True;
end;

end.
