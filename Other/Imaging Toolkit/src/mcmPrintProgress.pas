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
// $Log:  17579: mcmPrintProgress.pas 
//
//    Rev 1.1    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.0    27-05-2002 16:22:24  mcm

unit mcmPrintProgress;

{$Include 'mcmDefines.pas'}

interface

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls, ComCtrls;
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Controls,
      Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;
     {$ENDIF}

type
  TmcmPrintProgress = class(TForm)
    btnCancel       : TButton;
    ProgressBar     : TProgressBar;
    Bevel           : TBevel;
    lStatus         : TLabel;
    lPageNumber     : TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var PrintProgress : TmcmPrintProgress;

implementation

{$R *.DFM}

end.
