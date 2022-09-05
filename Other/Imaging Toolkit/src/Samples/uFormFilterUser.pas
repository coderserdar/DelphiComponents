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
// $Log:  17965: uFormFilterUser.pas 
//
//    Rev 1.4    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    15-05-2005 20:22:30  mcm    Version: IMG 2.9
// Modified to support 1 pixel Width/Height filters.

//
//   Rev 1.2    20-12-2004 22:58:08  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    27-01-2003 13:47:06  mcm

//
//   Rev 1.0    09-09-2002 10:12:52  mcm    Version: IMG 1.2

unit uFormFilterUser;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ComCtrls, Spin, Grids, 
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Samples.Spin,
      Vcl.Grids,
     {$ENDIF}
     mcmImage,
     mcmImageFilter,
     umcmIntE;

type
  TFormFilterUser = class(TForm)
    btnOK          : TButton;
    btnCancel      : TButton;
    gbKernel       : TGroupBox;
    lKernelHeight  : TLabel;
    lKernelWidth   : TLabel;
    lFilterKernel  : TLabel;
    cbFilterKernel : TComboBox;
    lScale         : TLabel;
    lBias          : TLabel;
    pcKernel       : TPageControl;
    tsKernel1      : TTabSheet;
    sgKernel1      : TStringGrid;
    tsKernel2      : TTabSheet;
    sgKernel2      : TStringGrid;
    seKernelHeight : TmcmIntSpin;
    seKernelWidth  : TmcmIntSpin;
    seScale        : TmcmIntSpin;
    seBias         : TmcmIntSpin;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure seKernelHeightChange(Sender : TObject);
    procedure seKernelWidthChange(Sender : TObject);
    procedure cbFilterKernelChange(Sender: TObject);
  private
    { Private declarations }
    FImage          : TmcmImage;
    // FSample         : TmcmImage;
    // FImageFilter    : TmcmImageFilter;
    function  GetBias : integer;
    function  GetFilter : TmcmFilter;
    function  GetFilterHeight : word;
    function  GetFilterWidth  : word;
    function  GetKernel(Index, x, y : integer) : integer;
    function  GetScaleFactor : integer;

    procedure SetBias(Value : integer);
    procedure SetFilter(Value : TmcmFilter);
    procedure SetFilterHeight(Value : word);
    procedure SetFilterWidth(Value : word);
    procedure SetImage(Value : TmcmImage);
    procedure SetKernel(Index, x, y : integer; Value : integer);
    procedure SetScaleFactor(Value : integer);
  public
    { Public declarations }
    property    Bias : integer
      read      GetBias
      write     SetBias;
    property    Filter : TmcmFilter
      read      GetFilter
      write     SetFilter;
    property    FilterHeight : word
      read      GetFilterHeight
      write     SetFilterHeight;
    property    FilterWidth : word
      read      GetFilterWidth
      write     SetFilterWidth;
    property    Image : TmcmImage
      read      FImage
      write     SetImage;
    property    Kernel[Index, x, y : integer] : integer
      read      GetKernel
      write     SetKernel;
    property    ScaleFactor : integer
      read      GetScaleFactor
      write     SetScaleFactor;
  end;

var FormFilterUser : TFormFilterUser;

implementation

uses mcmImageTypeDef;

{$R *.DFM}

procedure TFormFilterUser.FormCreate(Sender : TObject);
var i, j : integer;
begin
  // FSample := TmcmImage.Create;
  // FImageFilter := TmcmImageFilter.Create(Self);
  pcKernel.ActivePage := tsKernel1;

  cbFilterKernel.Items.AddObject('Single kernel', Pointer(FLT_USERDEF));
  cbFilterKernel.Items.AddObject('Single kernel ABS', Pointer(FLT_USERABS));
  cbFilterKernel.Items.AddObject('Dual kernel ABS', Pointer(FLT_USERDBLABS));
  cbFilterKernel.Items.AddObject('Dual kernel DIF', Pointer(FLT_USERDBLDIF));
  cbFilterKernel.Items.AddObject('Dual kernel RMS', Pointer(FLT_USERDBLRMS));
  cbFilterKernel.Items.AddObject('Dual kernel SUM', Pointer(FLT_USERDBLSUM));
  cbFilterKernel.ItemIndex := 0;

  seKernelHeight.Value := 15;
  seKernelWidth.Value  := 15;
  for j := 0 to (seKernelHeight.Value - 1)
  do for i := 0 to (seKernelWidth.Value - 1)
     do begin
        sgKernel1.Cells[i,j] := '0';
        sgKernel2.Cells[i,j] := '0';
     end;

  sgKernel1.DefaultColWidth  := 24;
  sgKernel1.DefaultRowHeight := 18;
  sgKernel2.DefaultColWidth  := 24;
  sgKernel2.DefaultRowHeight := 18;
  seKernelHeight.Value := 3;
  seKernelWidth.Value  := 3;
  sgKernel1.RowCount := seKernelHeight.Value;
  sgKernel2.RowCount := seKernelHeight.Value;
  sgKernel1.ColCount := seKernelWidth.Value;
  sgKernel2.ColCount := seKernelWidth.Value;

  seScale.Value := 1;
end; // TFormFilterUser.FormCreate.


procedure TFormFilterUser.FormDestroy(Sender : TObject);
begin
;
end; // TFormFilterUser.FormDestroy.


procedure TFormFilterUser.seKernelHeightChange(Sender : TObject);
begin
  sgKernel1.RowCount := seKernelHeight.Value;
  sgKernel2.RowCount := seKernelHeight.Value;
end; // TFormFilterUser.seKernelHeightChange.


procedure TFormFilterUser.seKernelWidthChange(Sender : TObject);
begin
  sgKernel1.ColCount := seKernelWidth.Value;
  sgKernel2.ColCount := seKernelWidth.Value;
end; // TFormFilterUser.seKernelWidthChange.


procedure TFormFilterUser.SetImage(Value : TmcmImage);
begin
  FImage := Value;
end; // TFormFilterUser.SetImage.


procedure TFormFilterUser.cbFilterKernelChange(Sender : TObject);
begin
  case cbFilterKernel.ItemIndex of
  0, 1 : begin
           pcKernel.ActivePage := tsKernel1;
           tsKernel2.Enabled := False;
         end;
  else tsKernel2.Enabled := True;
  end;
end; // TFormFilterUser.cbFilterKernelChange.


function TFormFilterUser.GetBias : integer;
begin
  Result := seBias.Value;
end; // TFormFilterUser.GetBias.


procedure TFormFilterUser.SetBias(Value : integer);
begin
  seBias.Value := Value;
end; // TFormFilterUser.SetBias.


function TFormFilterUser.GetScaleFactor : integer;
begin
  Result := seScale.Value;
end; // TFormFilterUser.GetScaleFactor.


procedure TFormFilterUser.SetScaleFactor(Value : integer);
begin
  seScale.Value := Value;
end; // TFormFilterUser.SetScaleFactor.


function TFormFilterUser.GetFilter : TmcmFilter;
begin
  Result := TmcmFilter(cbFilterKernel.Items.Objects[cbFilterKernel.ItemIndex]);
end; // TFormFilterUser.GetFilter.


procedure TFormFilterUser.SetFilter(Value : TmcmFilter);
var i : integer;
begin
  i := cbFilterKernel.Items.IndexOfObject(pointer(Value));
  if (i >= 0)
  then cbFilterKernel.ItemIndex := i;
end; // TFormFilterUser.SetFilter.


function TFormFilterUser.GetFilterHeight : word;
begin
  Result := seKernelHeight.Value;
end; // TFormFilterUser.GetFilterHeight.


procedure TFormFilterUser.SetFilterHeight(Value : word);
begin
  seKernelHeight.Value := Value;
end; // TFormFilterUser.SetFilterHeight.


function TFormFilterUser.GetFilterWidth : word;
begin
  Result := seKernelWidth.Value;
end; // TFormFilterUser.GetFilterWidth.


procedure TFormFilterUser.SetFilterWidth(Value : word);
begin
  seKernelWidth.Value := Value;
end; // TFormFilterUser.SetFilterWidth.


function TFormFilterUser.GetKernel(Index, x, y : integer) : integer;
begin
  case Index of
  0 : Result := StrToInt(sgKernel1.Cells[x,y]);
  1 : Result := StrToInt(sgKernel2.Cells[x,y]);
  else Result := 0;
  end;
end; // TFormFilterUser.GetKernel.


procedure TFormFilterUser.SetKernel(Index, x, y : integer; Value : integer);
begin
  case Index of
  0 : sgKernel1.Cells[x,y] := IntToStr(Value);
  1 : sgKernel2.Cells[x,y] := IntToStr(Value);
  end;
end; // TFormFilterUser.SetKernel.


end.
