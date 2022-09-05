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
// $Log:  15908: USABOUT.pas 
//
//    Rev 1.2    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//   Rev 1.1    14-06-2003 10:36:30  mcm    Version: DT 2.4
// Updated version info.

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

unit Usabout;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, WinApi.Messages, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     Vcl.ExtCtrls,
     {$ELSE}
     Windows, SysUtils, Messages, Classes, Graphics, Controls,
     Forms, Dialogs, StdCtrls, ExtCtrls,
     {$ENDIF}
     mcmAboutConst,
     twain;
type
  TFormAbout = class(TForm)
    Panel1      : TPanel;
    ProductName : TLabel;
    Version     : TLabel;
    Copyright   : TLabel;
    Image1      : TImage;
    OkButton    : TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FormAbout : TFormAbout;

implementation

{$R *.DFM}

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Version.Caption := defVersion;
  Copyright.Caption := defCopyRight;
end;

end.
