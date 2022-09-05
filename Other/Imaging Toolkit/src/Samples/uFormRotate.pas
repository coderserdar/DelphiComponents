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
// $Log:  20267: uFormRotate.pas 
//
//    Rev 1.5    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.4    23-01-2005 09:35:36  mcm
// Modified to support BW images.

//
//   Rev 1.3    07-01-2005 13:46:54  mcm
// Modified UI behaviour for "Free" degree editor.

//
//   Rev 1.2    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    25-09-2003 23:36:26  mcm    Version: IMG 1.5
// Included interpolation.

//
//   Rev 1.0    12-05-2003 15:33:36  mcm    Version: IMG 1.3.4
// Initial code.

unit uFormRotate;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
     {$ENDIF}
     umcmIntE, mcmImageTypeDef, mcmImage;

type
  TFormRotate = class(TForm)
    bntOK         : TButton;
    bntCancel     : TButton;
    rgDirection   : TRadioGroup;
    gbDegree      : TGroupBox;
    rb90          : TRadioButton;
    rb180         : TRadioButton;
    rb270         : TRadioButton;
    rsDegree      : TmcmRealSpin;
    rbFree        : TRadioButton;
    gbMethod      : TGroupBox;
    lResizeMethod : TLabel;
    cbMethod      : TComboBox;
    procedure FormCreate(Sender : TObject);
    procedure rbFreeClick(Sender : TObject);
    procedure rb90Click(Sender: TObject);
    procedure rb180Click(Sender: TObject);
    procedure rb270Click(Sender: TObject);
    procedure rsDegreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FImage : TmcmImage;
    function  GetDirection : boolean;
    procedure SetDirection(Value : boolean);
    function  GetDegrees : double;
    procedure SetDegrees(Value : double);
    function  GetInterpolate : TmcmInterpolate;
    procedure SetInterpolate(Value : TmcmInterpolate);
    procedure SetImage(Value : TmcmImage);
  public
    { Public declarations }
    property Direction : boolean
      read   GetDirection
      write  SetDirection;
    property Degrees : double
      read   GetDegrees
      write  SetDegrees;
    property Image : TmcmImage
      read   FImage
      write  SetImage;
    property Interpolate : TmcmInterpolate
      read   GetInterpolate
      write  SetInterpolate;
  end;

var FormRotate : TFormRotate;

implementation

{$R *.DFM}

procedure TFormRotate.FormCreate(Sender : TObject);
begin
  FImage := Nil;
  rgDirection.ItemIndex := 0;
  rb90.Checked := True;
end; // TFormRotate.FormCreate.


procedure TFormRotate.rsDegreeMouseDown(Sender : TObject;
                                        Button: TMouseButton;
                                        Shift: TShiftState;
                                        X, Y: Integer);
begin
  rbFree.Checked := True;
  rsDegree.SetFocus;
end; // TFormRotate.rsDegreeMouseDown.



procedure TFormRotate.rb90Click(Sender : TObject);
begin
  rsDegree.Value := 90;
end; // TFormRotate.rb90Click.


procedure TFormRotate.rb180Click(Sender : TObject);
begin
  rsDegree.Value := 180;
end; // TFormRotate.rb180Click.


procedure TFormRotate.rb270Click(Sender : TObject);
begin
  rsDegree.Value := 270;
end; // TFormRotate.rb270Click.


procedure TFormRotate.rbFreeClick(Sender : TObject);
begin
  rsDegree.SetFocus;
end; // TFormRotate.rbFreeClick.


function TFormRotate.GetDirection : boolean;
begin
  Result := (rgDirection.ItemIndex = 0);
end; // TFormRotate.GetDirection.


procedure TFormRotate.SetDirection(Value : boolean);
begin
  if Value
  then rgDirection.ItemIndex := 0
  else rgDirection.ItemIndex := 1;
end; // TFormRotate.SetDirection.


function TFormRotate.GetDegrees : double;
begin
  Result := rsDegree.Value
end; // TFormRotate.GetDegrees.


procedure TFormRotate.SetDegrees(Value : double);
begin
  rsDegree.Value := Value;
  if (Value = 90)
  then rb90.Checked := True
  else if (Value = 180)
       then rb180.Checked := True
       else if (Value = 270)
            then rb270.Checked := True
            else rbFree.Checked := True;
end; // TFormRotate.SetDegrees.


function TFormRotate.GetInterpolate : TmcmInterpolate;
begin
  Result := TmcmInterpolate(cbMethod.ItemIndex);
end; // TFormRotate.GetInterpolate.


procedure TFormRotate.SetInterpolate(Value : TmcmInterpolate);
begin
  if cbMethod.Enabled
  then cbMethod.ItemIndex := integer(Value);
end; // TFormRotate.SetInterpolate.


procedure TFormRotate.SetImage(Value : TmcmImage);
begin
  FImage := Value;
  if (Value <> Nil)
  then begin
       if (Value.ImageFormat = IF_BW) or
          (Value.ImageFormat = IF_PAL8)
       then begin
            cbMethod.ItemIndex := integer(ST_NEAREST);
            cbMethod.Enabled := False;
       end;
  end;
end; // TFormRotate.SetImage.


end.
