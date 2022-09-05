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
// $Log:  15928: uTwnDlgSend.pas 
//
//    Rev 1.8    2014-03-28 17:54:04  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.7    2014-01-15 13:42:06  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.6    2013-12-04 23:16:20  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.5    01-03-2011 20:41:04  mcm    Version: DT 3.11
//
//   Rev 1.4    06-11-2003 09:51:32  mcm    Version: DT3.0

//
//   Rev 1.3    06-03-2003 11:06:30  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.2    07-10-2002 14:59:12  mcm    Version: DT2.1

//
//   Rev 1.1    16-01-2002 09:12:46  mcm    Version: DT 2.0
// commented out GetResponse and DisplayResponse.

//
//   Rev 1.0    04-12-2001 16:49:14  mcm    Version: DT 2.0

unit uTwnDlgSend;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     Vcl.ExtCtrls, Vcl.Buttons, Vcl.Grids, System.Inifiles, 
     {$ELSE}
     Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
     Buttons, ExtCtrls, Dialogs, Grids,
     inifiles,
     {$ENDIF}
     umcmIntE,
     Twain, mcmTWAIN, mcmTWAINKernel, mcmTWAINLog;

type
  TDlgTwnSend    = class(TForm)
    btnClose     : TButton;
    btnSend      : TButton;
    btnStatus    : TButton;
    gbSend       : TGroupBox;
    lResult      : TLabel;
    lResultVal   : TLabel;
    lStatus      : TLabel;
    lStatusVal   : TLabel;

    lDest        : TLabel;
    lDG          : TLabel;
    lDAT         : TLabel;
    lMSG         : TLabel;
    cbDest       : TComboBox;
    cbDG         : TComboBox;
    cbDAT        : TComboBox;
    cbMSG        : TComboBox;
    sgData       : TStringGrid;
    gbCapability : TGroupBox;
    lCapability  : TLabel;
    lContainer   : TLabel;
    lItemType    : TLabel;
    lNumItems    : TLabel;
    cbCAP        : TComboBox;
    cbCON        : TComboBox;
    cbItemType   : TComboBox;
    rsNumItems   : TmcmRealSpin;
    lState       : TLabel;
    lStateVal    : TLabel;

    procedure FormCreate(Sender: TObject);
    procedure cbDGChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbDestChange(Sender: TObject);
    procedure cbDATChange(Sender: TObject);
    procedure cbMSGChange(Sender: TObject);
    procedure cbCAPChange(Sender: TObject);
    procedure cbCONChange(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure rsNumItemsChange(Sender: TObject);
  private
    { Private declarations }
    mcmTWAIN    : TmcmTWAIN;
    mcmTWAINLog : TmcmTWAINLog;
    SaveFailure : TFailureEvent;
    FDest       : integer;
    FDG         : integer;
    FDAT        : integer;
    FOldDAT     : integer;
    FMSG        : integer;
    FCAP        : integer;
    FOldCAP     : integer;
    FCON        : integer;

    FDestSec    : string;
    FDGSec      : string;
    FDATSec     : string;

    FPixelType  : word;

    IniFile     : TIniFile;
    sgIndex     : integer; // Used by Array and Enumeration containers.

    function  ControlMsg : TW_UINT16;
    function  ImageMsg   : TW_UINT16;
    function  AudioMsg   : TW_UINT16;
    procedure SetCapConState;
    procedure SetStructure;
    procedure SetNumItems      (Value        : integer);
    procedure SetItemType      (Value        : TW_UINT16);
    function  ItemType2Value   (ItemStr      : string) : TW_UINT16;
    function  Token2Value      (CapStr       : string;
                                Token        : string;
                                ItemType     : TW_UINT16) : Variant;
    procedure DisplayContainer (Cap          : TW_UINT16);
    function  GetCapabilitySend(Cap          : TW_UINT16;
                                Msg          : TW_UINT16;
                                ConType      : TW_UINT16) : TW_UINT16;
    function  SetCapabilitySend(Cap          : TW_UINT16;
                                Msg          : TW_UINT16;
                                ConType      : TW_UINT16) : TW_UINT16;
    (*
    function  GetResponse      (hDlg         : hWnd;
                                pResponse    : pTW_GRAYRESPONSE;
                                Bits         : TW_UINT16) : boolean;
    function  DisplayResponse  (hDlg         : hWnd;
                                pResponse    : pTW_GRAYRESPONSE;
                                Bits         : TW_UINT16) : boolean;
    *)
    procedure ClearData;
  public
    { Public declarations }
  end;

var DlgTwnSend : TDlgTwnSend;

implementation

{$R *.DFM}

uses mcmTWAINContainer, mcmTWAINIntf, mcmTWAINFix;

type
  TTypeCastTWAINKernel = class(TmcmTWAINKernel);
  TTypeCastTWAINIntf   = class(TmcmTWAINIntf);
  TTypeCastContainer   = class(TTwnContainer);

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

procedure TDlgTwnSend.FormCreate(Sender : TObject);
begin
  mcmTWAIN := TmcmTWAIN(Owner);
  mcmTWAINLog := TmcmTWAINLog.Create(Self);

  SaveFailure := mcmTWAIN.OnFailure;
  mcmTWAIN.OnFailure := Nil;
  FPixelType := 65535;

  lResultVal.Caption := '-';
  lStatusVal.Caption := '-';
  lStateVal.Caption := IntToStr(mcmTWAIN.State);
  sgData.ColWidths[0] := 112;
  sgData.ColWidths[1] := 128 + 36;
  sgData.Cells[0, 0] := 'Description';
  sgData.Cells[1, 0] := 'Data';
  if FileExists(mcmTWAINLog.INIFile)
  then begin
       IniFile := TIniFile.Create(mcmTWAINLog.INIFile);

       IniFile.ReadSection('Dest', cbDest.Items);
       IniFile.ReadSection('CAP', cbCAP.Items);
       IniFile.ReadSection('ConType', cbCON.Items);
       IniFile.ReadSection('ItemType', cbItemType.Items);

       cbDest.ItemIndex := 1;
       cbDestChange(Self);
       cbDG.ItemIndex   := 0;
       cbDGChange(Self);
       cbDAT.ItemIndex  := 0;
       cbMSG.ItemIndex  := 0;
       cbCAP.ItemIndex  := 0;
       cbCON.ItemIndex  := cbCON.Items.Count - 2;
       cbDATChange(Self);
       cbCAPChange(Self);
       cbCONChange(Self);
  end;
end; { End TDlgTwnSend.FormCreate.                                             }


procedure TDlgTwnSend.FormClose(    Sender : TObject;
                                var Action : TCloseAction);
begin
  mcmTWAIN.OnFailure := SaveFailure;
  IniFile.Free;
end; { End TDlgTwnSend.FormClose.                                              }


procedure TDlgTwnSend.btnSendClick(Sender : TObject);
begin
  case FDG of
  DG_CONTROL : ControlMsg;
  DG_IMAGE   : ImageMsg;
  DG_AUDIO   : AudioMsg;
  end;
  lStateVal.Caption := IntToStr(mcmTWAIN.State);
end; { End TDlgTwnSend.btnSendClick.                                           }


procedure TDlgTwnSend.btnStatusClick(Sender : TObject);
var twRC   : TW_UINT16;
    Status : TW_UINT16;
begin
  // Check status.
  twRC := TTypeCastTWAINKernel(mcmTWAIN).GetStatus(Status, @TTypeCastTWAINKernel(mcmTWAIN).FSourceID);

  // Convert return code, to text.
  lResultVal.Caption := mcmTWAINLog.MatchTwainInt('ReturnCode', twRC, 1024);

  // Convert condition code, to text.
  lStatusVal.Caption := mcmTWAINLog.MatchTwainInt('ConditionCode', Status, 1024);
end; { End TDlgTwnSend.btnStatusClick.                                         }


procedure TDlgTwnSend.cbDestChange(Sender : TObject);
var Identifier : string;
begin
  if (cbDest.ItemIndex >= 0)
  then begin
       Identifier := cbDest.Items.Strings[cbDest.ItemIndex];
       FDest := IniFile.ReadInteger('Dest', Identifier, -1);
       case FDest of
       0 : FDestSec := 'DG/DSM';
       1 : FDestSec := 'DG/DS';
       end;
       IniFile.ReadSection(FDestSec, cbDG.Items);
       cbDG.ItemIndex := 0;
       cbDGChange(Self);
  end;
end; { End TDlgTwnSend.cbDestChange.                                           }


procedure TDlgTwnSend.cbDGChange(Sender : TObject);
var Identifier : string;
begin
  if (cbDG.ItemIndex >= 0)
  then begin
       ClearData;
       Identifier := cbDG.Items.Strings[cbDG.ItemIndex];
       FDG    := IniFile.ReadInteger('DG', cbDG.Items.Strings[cbDG.ItemIndex], -1);
       FDGSec := FDestSec + '/' + Identifier;
       IniFile.ReadSection(FDGSec, cbDAT.Items);
       cbDAT.ItemIndex := 0;
       cbDATChange(Self);
  end;
end; { End TDlgTwnSend.cbDGChange.                                             }


procedure TDlgTwnSend.cbDATChange(Sender : TObject);
var Identifier : string;
begin
  if (cbDAT.ItemIndex >= 0)
  then begin
       Identifier := cbDAT.Items.Strings[cbDAT.ItemIndex];
       FDAT    := IniFile.ReadInteger(FDGSec, cbDAT.Items.Strings[cbDAT.ItemIndex], -1);
       if (FOldDAT <> FDAT)
       then ClearData;
       FOldDAT := FDAT;
       FDATSec := Copy(Identifier, 5, Length(Identifier));
       if (Length(FDATSec) > 0)
       then begin
            IniFile.ReadSection(FDATSec, cbMSG.Items);
            cbMSG.ItemIndex := 0;
            cbMSGChange(Self);
            SetCapConState;
            if (FDAT = DAT_CAPABILITY)
            then cbCONChange(Self)
            else SetStructure;
       end;
  end;
end; { End TDlgTwnSend.cbDATChange.                                            }


procedure TDlgTwnSend.cbMSGChange(Sender : TObject);
begin
  if (cbMSG.ItemIndex >= 0)
  then FMSG := IniFile.ReadInteger(FDATSec, cbMSG.Items.Strings[cbMSG.ItemIndex], -1);
end; { End TDlgTwnSend.cbMSGChange.                                            }


procedure TDlgTwnSend.cbCAPChange(Sender : TObject);
begin
  if (cbCAP.ItemIndex >= 0)
  then FCAP := IniFile.ReadInteger('CAP', cbCAP.Items.Strings[cbCAP.ItemIndex], -1);
  if (FOldCAP <> FCAP)
  then ClearData;
  FOldCAP := FCAP;
  SetStructure;
end; { End TDlgTwnSend.cbCAPChange.                                            }


procedure TDlgTwnSend.cbCONChange(Sender : TObject);
var Container : TtwnContainer;
begin
  if (cbCON.ItemIndex >= 0)
  then begin
       FCON := IniFile.ReadInteger('ConType', cbCON.Items.Strings[cbCON.ItemIndex], -1);
       if (FCON = TWON_ARRAY) or (FCON = TWON_ENUMERATION)
       then rsNumItems.Enabled := True
       else rsNumItems.Enabled := False;

       Container := mcmTWAIN.Containers.Items[FCAP];
       if Assigned(Container)
       then Container.ContainerType := FCON;


       SetStructure;
  end
  else FCON := 0;
end; { End TDlgTwnSend.cbCONChange.                                            }


procedure TDlgTwnSend.SetCapConState;
var Identifier : string;
    i          : integer;
begin
  i := 1;
  if (cbDG.ItemIndex >= 0)
  then begin
       Identifier := cbDG.Items.Strings[cbDG.ItemIndex];
       i := abs(CompareStr(Identifier, 'DG_CONTROL'));

       if (cbDAT.ItemIndex >= 0)
       then begin
            Identifier := cbDAT.Items.Strings[cbDAT.ItemIndex];
            i := i + abs(CompareStr(Identifier, 'DAT_CAPABILITY'));
       end;
  end;
  cbCAP.Enabled := (i = 0);
  cbCON.Enabled := (i = 0);
  cbItemType.Enabled := (i = 0);
  rsNumItems.Enabled := (i = 0);
end; { End TDlgTwnSend.SetCapConState.                                         }


procedure TDlgTwnSend.SetNumItems(Value : integer);
begin
  if (Value > 0) or (FCON = TWON_ARRAY) or (FCON = TWON_ENUMERATION)
  then begin
       rsNumItems.Enabled := True;
       rsNumItems.Value := Value;
  end
  else begin
       rsNumItems.Enabled := False;
  end;
end; { End TDlgTwnSend.SetNumItems.                                            }


procedure TDlgTwnSend.SetItemType(Value : TW_UINT16);
var i           : integer;
    ItemTypeStr : string;
begin
  ItemTypeStr := mcmTWAINLog.MatchTwainInt('ItemType', Value, 1024);
  i := cbItemType.Items.IndexOf(ItemTypeStr);
  if (i >= 0)
  then cbItemType.ItemIndex := i;
end; { End TDlgTwnSend.SetItemType.                                            }


procedure TDlgTwnSend.SetStructure;
var Section   : string;
    i, j      : integer;
    TmpStr    : string;
    Container : TtwnContainer;
begin
  if (FDAT <> DAT_PALETTE8)
  then begin
       sgData.ColCount := 2;
       sgData.ColWidths[0] := 112;
       sgData.ColWidths[1] := 164;
       sgData.Cells[1,0] := 'Data';
  end;
  case FDAT of
  DAT_CAPABILITY      : begin
                          Section := cbCON.Items.Strings[cbCON.ItemIndex];
                          Container := TtwnContainer.Create(Self);
                          Container.Capability := FCAP;
                          SetItemType(Container.ItemType);

                          case FCON of
                          TWON_ONEVALUE    : begin
                                               sgIndex := 1;
                                               if (FCAP = ICAP_FRAMES)
                                               then sgData.RowCount := 5
                                               else sgData.RowCount := 2;
                                             end;
                          TWON_ENUMERATION : begin
                                               sgIndex := 3;
                                               if (FCAP = ICAP_FRAMES)
                                               then sgData.RowCount := sgIndex + 4 * round(rsNumItems.Value)
                                               else sgData.RowCount := sgIndex + round(rsNumItems.Value);
                                               sgData.Cells[0,1] := 'CurrentIndex';
                                               sgData.Cells[0,2] := 'DefaultIndex';
                                             end;
                          TWON_RANGE       : begin
                                               sgIndex := -1;
                                               sgData.RowCount := 6;
                                               sgData.Cells[0,1] := 'MinValue';
                                               sgData.Cells[0,2] := 'MaxValue';
                                               sgData.Cells[0,3] := 'StepSize';
                                               sgData.Cells[0,4] := 'DefaultValue';
                                               sgData.Cells[0,5] := 'CurrentValue';
                                             end;
                          TWON_ARRAY       : begin
                                               sgIndex := 1;
                                               i := sgIndex + round(rsNumItems.Value);
                                               if (i < 2)
                                               then i := 2;
                                               sgData.RowCount := i;
                                             end;
                          else               begin
                                               sgIndex := -1;
                                               sgData.RowCount := 2;
                                               sgData.Cells[0,1] := '';
                                             end;
                          end;

                          if (sgIndex >= 0)
                          then begin
                               if (FCAP = ICAP_FRAMES)
                               then begin
                                    for i := 0 to (round(rsNumItems.Value) - 1)
                                    do begin
                                       j := i * 4;
                                       TmpStr := 'Item[' + IntToStr(i) + ']';
                                       sgData.Cells[0,j+sgIndex] := TmpStr + '.Left';
                                       sgData.Cells[0,j+1+sgIndex] := TmpStr + '.Top';
                                       sgData.Cells[0,j+2+sgIndex] := TmpStr + '.Right';
                                       sgData.Cells[0,j+3+sgIndex] := TmpStr + '.Bottom';
                                    end;
                               end
                               else begin
                                    for i := 0 to (round(rsNumItems.Value) - 1)
                                    do sgData.Cells[0,i+sgIndex] := 'Item[' + IntToStr(i) + ']';
                               end;
                          end;
                          Container.Free;
                          
                          DisplayContainer(FCAP);
                        end;
  // DAT_EVENT
  DAT_IDENTITY        : begin
                          sgData.Cells[0,1]  := 'Id';
                          sgData.Cells[0,2]  := 'Version No';
                          sgData.Cells[0,3]  := 'Language';
                          sgData.Cells[0,4]  := 'Country';
                          sgData.Cells[0,5]  := 'Info';
                          sgData.Cells[0,6]  := 'Protocol';
                          sgData.Cells[0,7]  := 'Supported Groups';
                          sgData.Cells[0,8]  := 'Manufacturer';
                          sgData.Cells[0,9]  := 'Product Family';
                          sgData.Cells[0,10] := 'Product Name';
                          sgData.RowCount := 11;
                        end;
  DAT_PARENT          : begin
                          sgData.Cells[0,1]  := 'DSM';
                          sgData.RowCount := 2;
                        end;
  DAT_PENDINGXFERS    : begin
                          sgData.Cells[0,1]  := 'Count';
                          sgData.Cells[0,2]  := 'Reserved';
                          sgData.RowCount := 3;
                        end;
  DAT_SETUPMEMXFER    : begin
                          sgData.Cells[0,1]  := 'MinBufSize';
                          sgData.Cells[0,2]  := 'MaxBufSize';
                          sgData.Cells[0,3]  := 'Preferred';
                          sgData.RowCount := 4;
                        end;
  DAT_SETUPFILEXFER   : begin
                          sgData.Cells[0,1]  := 'FileName';
                          sgData.Cells[0,2]  := 'Format';
                          sgData.Cells[0,3]  := 'VRefNum';
                          sgData.RowCount := 4;
                        end;
  DAT_STATUS          : begin
                          sgData.Cells[0,1]  := 'ConditionCode';
                          sgData.Cells[0,2]  := 'Reserved';
                          sgData.RowCount := 3;
                        end;
  DAT_USERINTERFACE   : begin
                          sgData.Cells[0,1]  := 'ShowUI';
                          if mcmTWAIN.ShowUI
                          then sgData.Cells[1,1]  := 'TRUE'
                          else sgData.Cells[1,1]  := 'FALSE';
                          sgData.Cells[0,2]  := 'ModalUI';
                          sgData.Cells[1,2]  := '0';
                          sgData.Cells[0,3]  := 'hParent';
                          sgData.Cells[1,3]  := IntToHex(TTypeCastTWAINKernel(mcmTWAIN).WinHandle, 8);
                          sgData.RowCount := 4;
                        end;
  DAT_XFERGROUP       : begin
                          sgData.Cells[0,1]  := 'Groups';
                          sgData.RowCount := 2;
                        end;
(* Depricated
  DAT_TWUNKIDENTITY   : begin
                          sgData.Cells[0,1]  := 'dsPath';
                          sgData.RowCount := 2;
                        end;
*)
  // DAT_DEVICEEVENT
  DAT_FILESYSTEM      : begin
                          sgData.Cells[0,1]  := 'InputName';
                          sgData.Cells[0,2]  := 'OutputName';
                          sgData.Cells[0,3]  := 'Context';
                          sgData.Cells[0,4]  := 'Recursive';
                          sgData.Cells[0,5]  := 'FileType';
                          sgData.Cells[0,6]  := 'Size';
                          sgData.Cells[0,7]  := 'CreateTimeDate';
                          sgData.Cells[0,8]  := 'ModifiedTimeDate';
                          sgData.Cells[0,9]  := 'FreeSpace';
                          sgData.Cells[0,10] := 'NewImageSize';
                          sgData.Cells[0,11] := 'NumberOfFiles';
                          sgData.Cells[0,12] := 'NumberOfSnippets';
                          sgData.Cells[0,13] := 'Reserved';
                          sgData.RowCount := 14;
                        end;
  // DAT_PASSTHRU
  DAT_IMAGEINFO       : begin
                          sgData.Cells[0,1]  := 'XResolution';
                          sgData.Cells[0,2]  := 'YResolution';
                          sgData.Cells[0,3]  := 'ImageWidth';
                          sgData.Cells[0,4]  := 'ImageLength';
                          sgData.Cells[0,5]  := 'SamplesPerPixel';
                          sgData.Cells[0,6]  := 'BitsPerSample[0]';
                          sgData.Cells[0,7]  := 'BitsPerSample[1]';
                          sgData.Cells[0,8]  := 'BitsPerSample[2]';
                          sgData.Cells[0,9]  := 'BitsPerSample[3]';
                          sgData.Cells[0,10] := 'BitsPerSample[4]';
                          sgData.Cells[0,11] := 'BitsPerSample[5]';
                          sgData.Cells[0,12] := 'BitsPerSample[6]';
                          sgData.Cells[0,13] := 'BitsPerSample[7]';
                          sgData.Cells[0,14] := 'BitsPerPixel';
                          sgData.Cells[0,15] := 'Planar';
                          sgData.Cells[0,16] := 'PixelType';
                          sgData.Cells[0,17] := 'Compression';
                          sgData.RowCount := 18;
                        end;
  DAT_IMAGELAYOUT     : begin
                          sgData.Cells[0,1]  := 'Frame.Left';
                          sgData.Cells[0,2]  := 'Frame.Top';
                          sgData.Cells[0,3]  := 'Frame.Right';
                          sgData.Cells[0,4]  := 'Frame.Bottom';
                          sgData.Cells[0,5]  := 'DocumentNumber';
                          sgData.Cells[0,6]  := 'PageNumber';
                          sgData.Cells[0,7]  := 'FrameNumber';
                          sgData.RowCount := 8;
                        end;
  // DAT_IMAGEMEMXFER
  // DAT_IMAGENATIVEXFER
  // DAT_IMAGEFILEXFER
  DAT_CIECOLOR        : begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  DAT_GRAYRESPONSE    : begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  DAT_RGBRESPONSE     : begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  DAT_JPEGCOMPRESSION : begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  DAT_PALETTE8        : begin
                          sgData.ColCount := 4;
                          sgData.Cells[1,0] := 'Red';
                          sgData.Cells[2,0] := 'Green';
                          sgData.Cells[3,0] := 'Blue';

                          sgData.ColWidths[1] := 53;
                          sgData.ColWidths[2] := 53;
                          sgData.ColWidths[3] := 53;
                          for i := 0 to 255
                          do sgData.Cells[0,1+i] := 'Entry[' + IntToStr(i) + ']';
                          sgData.RowCount := 257;
                        end;
  DAT_EXTIMAGEINFO    : begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  // DAT_AUDIOFILEXFER
  // DAT_AUDIOINFO
  // DAT_AUDIONATIVEXFER
  DAT_SETUPFILEXFER2  : begin
                          sgData.Cells[0,1]  := 'FileName';
                          sgData.Cells[0,2]  := 'FileNameType';
                          sgData.Cells[0,3]  := 'Format';
                          sgData.Cells[0,4]  := 'VRefNum';
                          sgData.Cells[0,5]  := 'parID';
                          sgData.RowCount := 6;
                        end;
  else                  begin
                          sgData.Cells[0,1]  := '';
                          sgData.RowCount := 2;
                        end;
  end;
end; { End TDlgTwnSend.SetStructure.                                           }


function TDlgTwnSend.ControlMsg : TW_UINT16;
var FileName    : string;
    FileFormat  : TtwnFileFmt;
    MemSetup    : TW_SETUPMEMXFER;
    Buffer      : string;
    wValue      : smallint;
    ShowUI      : boolean;
    ModalUI     : boolean;
    RetCode     : word;
    StatCode    : word;
    XferGroup   : longint;
begin
  Result := 0;
  case FDAT of
  // DAT_NULL
  DAT_CAPABILITY      : case FMSG of
                        MSG_GET,
                        MSG_GETCURRENT,
                        MSG_GETDEFAULT,
                        MSG_RESET        : begin // Get message.
                                             Result := GetCapabilitySend(FCap, FMsg, FCon);
                                           end;
                        MSG_QUERYSUPPORT : begin // Query which messages are supported.
                                             FCon := TWON_DONTCARE16;
                                             Result := GetCapabilitySend(FCap, FMsg, FCon);
                                           end;
                        MSG_SET          : begin // Set message.
                                             Result := SetCapabilitySend(FCap, FMsg, FCon);
                                           end;
                        else               begin // WHAT ?
                                           end;
                        end;
  // DAT_EVENT
  DAT_IDENTITY        : begin
                          case FMSG of
                          MSG_CLOSEDS    : TTypeCastTWAINIntf(mcmTWAIN).CloseDS;
                          MSG_GETDEFAULT : TTypeCastTWAINIntf(mcmTWAIN).GetDefaultSource(Buffer);
                          MSG_GETFIRST   : TTypeCastTWAINIntf(mcmTWAIN).GetFirstSource(Buffer);
                          MSG_GETNEXT    : TTypeCastTWAINIntf(mcmTWAIN).GetNextSource(Buffer);
                          MSG_OPENDS     : TTypeCastTWAINIntf(mcmTWAIN).OpenDS;
                          MSG_USERSELECT : TTypeCastTWAINIntf(mcmTWAIN).SelectDS;
                          end;
                          if (FMSG <> MSG_CLOSEDS) or True
                          then begin
                               sgData.Cells[1,1]  := IntToStr(mcmTWAIN.SourceInfo.Id);
                               sgData.Cells[1,2]  := mcmTWAIN.SourceInfo.Version;
                               sgData.Cells[1,3]  := IntToStr(integer(mcmTWAIN.SourceInfo.Language));
                               sgData.Cells[1,4]  := IntToStr(integer(mcmTWAIN.SourceInfo.Country));
                               sgData.Cells[1,5]  := mcmTWAIN.SourceInfo.Info;
                               sgData.Cells[1,6]  := mcmTWAIN.SourceInfo.Protocol;
                               Buffer := '';
                               if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_CONTROL) <> 0)
                               then Buffer := Buffer + 'DG_CONTROL ';
                               if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_IMAGE) <> 0)
                               then Buffer := Buffer + 'DG_IMAGE ';
                               if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_AUDIO) <> 0)
                               then Buffer := Buffer + 'DG_AUDIO';
                               sgData.Cells[1,7]  := Buffer;
                               sgData.Cells[1,8]  := mcmTWAIN.SourceInfo.Manufacturer;
                               sgData.Cells[1,9]  := mcmTWAIN.SourceInfo.ProductFamily;
                               sgData.Cells[1,10] := mcmTWAIN.SourceInfo.ProductName;
                          end
                          else begin
                               sgData.Cells[1,1] := 'Closed';
                               sgData.RowCount   := 2;
                          end;
                        end;
  DAT_PARENT          : begin
                          case FMSG of
                          MSG_CLOSEDSM : begin
                                           if mcmTWAIN.CloseSourceMgr
                                           then sgData.Cells[1,1]  := 'Closed'
                                           else sgData.Cells[1,1]  := 'FAILED';
                                         end;
                          MSG_OPENDSM  : if mcmTWAIN.OpenSourceMgr
                                         then sgData.Cells[1,1]  := 'Open'
                                         else sgData.Cells[1,1]  := 'FAILED';

                          end;
                        end;
  DAT_PENDINGXFERS    : begin
//mxm                          TTypeCastTWAINIntf(mcmTWAIN).PendingXfers(FMsg, wValue);
                          wValue := 0;
                          sgData.Cells[1,1]  := IntToStr(shortint(wValue));
                          sgData.Cells[1,2]  := ''; // Reserved.
                        end;
  DAT_SETUPMEMXFER    : begin
//mxm                          TTypeCastTWAINIntf(mcmTWAIN).SetupMemXfer(@MemSetup);
                          MemSetup.MinBufSize:= 0;
                          MemSetup.MaxBufSize:= 0;
                          MemSetup.Preferred := 0;
                          sgData.Cells[1,1]  := IntToStr(MemSetup.MinBufSize);
                          sgData.Cells[1,2]  := IntToStr(MemSetup.MaxBufSize);
                          sgData.Cells[1,3]  := IntToStr(MemSetup.Preferred);
                        end;
  DAT_SETUPFILEXFER   : begin
                          case FMSG of
                          MSG_GET,
                          MSG_GETDEFAULT,
                          MSG_RESET      : begin
                                             FileName   := '';
                                             FileFormat := TWFF_BMP;
//mxm                                             TTypeCastTWAINIntf(mcmTWAIN).SetupFileXferA(FMSG, FileName, FileFormat);
                                             sgData.Cells[1,1]  := FileName;
                                             sgData.Cells[1,2]  := mcmTWAINLog.MatchTwainInt('ICAP_IMAGEFILEFORMAT', integer(FileFormat), 1024);
                                             sgData.Cells[1,3]  := '-'; // VRefNum
                                           end;
                          MSG_SET        : begin
{mxm
                                             FileName := sgData.Cells[1,1];
                                             FileFormat := Token2Value('ICAP_IMAGEFILEFORMAT', sgData.Cells[1,2], TWTY_UINT16);
}                                             
//mxm                                             TTypeCastTWAINIntf(mcmTWAIN).SetupFileXferA(FMSG, FileName, FileFormat);
                                           end;
                          end;
                        end;
  DAT_STATUS          : begin
                          if (FDest = 0)
                          then RetCode := TTypeCastTWAINKernel(mcmTWAIN).GetStatus(StatCode, Nil)
                          else RetCode := TTypeCastTWAINKernel(mcmTWAIN).GetStatus(StatCode, @TTypeCastTWAINKernel(mcmTWAIN).FSourceID);
                          sgData.Cells[1,1]  := mcmTWAINLog.MatchTwainInt('ReturnCode', RetCode, 1024);
                          sgData.Cells[1,2]  := mcmTWAINLog.MatchTwainInt('ConditionCode', StatCode, 1024);
                        end;
  DAT_USERINTERFACE   : begin
                          case FMSG of
                          MSG_DISABLEDS      : begin
                                                 TTypeCastTWAINIntf(mcmTWAIN).DisableDS;
                                               end;
                          MSG_ENABLEDS       : begin
                                                 if (UpperCase(sgData.Cells[1,1]) = 'TRUE')
                                                 then ShowUI := True
                                                 else ShowUI := False;
                                                 ModalUI := False;
                                                 TTypeCastTWAINIntf(mcmTWAIN).EnableDS(ShowUI, ModalUI);
                                                 if ShowUI
                                                 then sgData.Cells[1,1] := 'TRUE'
                                                 else sgData.Cells[1,1] := 'FALSE';
                                                 sgData.Cells[1,2]  := '0';
                                                 sgData.Cells[1,3]  := IntToHex(TTypeCastTWAINKernel(mcmTWAIN).WinHandle, 8);
                                               end;
                          MSG_ENABLEDSUIONLY : begin
                                               end;
                          end;
                        end;
  DAT_XFERGROUP       : begin
                          case FMSG of
                          MSG_GET : begin
                                      TTypeCastTWAINIntf(mcmTWAIN).XferGroup(FMSG, XferGroup);
                                      Buffer := '';
                                      if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_CONTROL) <> 0)
                                      then Buffer := Buffer + 'DG_CONTROL ';
                                      if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_IMAGE) <> 0)
                                      then Buffer := Buffer + 'DG_IMAGE ';
                                      if ((mcmTWAIN.SourceInfo.SupportedGroups and DG_AUDIO) <> 0)
                                      then Buffer := Buffer + 'DG_AUDIO';
                                      sgData.Cells[1,1]  := Buffer;
                                    end;
                          MSG_SET : begin
                                      Buffer := sgData.Cells[1,1];
                                      XferGroup := 0;
                                      if (Pos('DG_CONTROL', Buffer) > 0)
                                      then XferGroup := XferGroup or $01;
                                      if (Pos('DG_IMAGE', Buffer) > 0)
                                      then XferGroup := XferGroup or $02;
                                      if (Pos('DG_AUDIO', Buffer) > 0)
                                      then XferGroup := XferGroup or $04;
                                      TTypeCastTWAINIntf(mcmTWAIN).XferGroup(FMSG, XferGroup);
                                    end;
                          end;
                        end;
(* Depricated
  DAT_TWUNKIDENTITY   : begin
                          TTypeCastTWAINKernel(mcmTWAIN).GetDSPath(FileName);
                          sgData.Cells[1,1]  := FileName;
                        end;
*)
  // DAT_CUSTOMDSDATA
  DAT_DEVICEEVENT     : begin
                        end;
  DAT_FILESYSTEM      : begin
                        end;
  DAT_SETUPFILEXFER2  : begin
                        end;
  else                  begin
                          ShowMessage('Send Error' + chr($0D) +
                                      'Control or Capability unknown or just not supported here.');
                          Result := TWRC_FAILURE;
                        end;
  end;
  // Convert return code, to text.
  lResultVal.Caption := mcmTWAINLog.MatchTwainInt('ReturnCode', mcmTWAIN.DSResult, 1024);

  // Convert condition code, to text.
  lStatusVal.Caption := mcmTWAINLog.MatchTwainInt('ConditionCode', mcmTWAIN.DSStatus, 1024);
end; { End TDlgTwnSend.ControlMsg.                                             }


function TDlgTwnSend.ImageMsg : TW_UINT16;
var i             : integer;
    ImageLayout   : TImageLayout;
    TWImageLayout : TW_IMAGELAYOUT;
    ImageInfo     : TImageInfo;
    pPalette      : PLogPalette;

(*
var hBmp           : THandle;
    ImageInfo      : TW_IMAGEINFO;
    ImageLayout    : TW_IMAGELAYOUT;
    Palette        : TW_PALETTE8;
    pGrayResponse  : pTW_GRAYRESPONSE;
    twCIE          : TW_CIECOLOR;
    fl, fx, fy, fz : extended;
    i              : integer;
    Buffer         : string;
    BufStr         : string;
    ItemStr        : string;
    twHandle       : TW_HANDLE;
      *)
begin
  Result := 0;
  case FDAT of
  DAT_CIECOLOR        : begin { check ImageInfo for TWPT_CIEXYZ.               }
  (*
                          mStruct.Clear;

//                          Result := TWCIEColor(@twCIE, Handle);
			  if (Result = TWRC_SUCCESS)
			  then begin
			       { Display MSG_GET result.                       }
                               mStruct.Lines.Add('ColorSpace='      + IntToStr(twCIE.ColorSpace));
                               mStruct.Lines.Add('LowEndian='       + IntToStr(twCIE.LowEndian));
                               mStruct.Lines.Add('DeviceDependent=' + IntToStr(twCIE.DeviceDependent));
                               mStruct.Lines.Add('VersionNumber='   + IntToStr(twCIE.VersionNumber));

			       { Only show first of Transforms.                }
{
                               for i := 0 to 0
                               do begin
                                  with twCIE.StageABC.Decode[i]
                                  do begin
                                     fl := FIX32ToFloat(StartIn);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].StartIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(BreakIn);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].BreakIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(EndIn);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].EndIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(StartOut);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].StartOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(BreakOut);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].BreakOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(EndOut);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].EndOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(Gamma);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].Gamma=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(SampleCount);
                                     mStruct.Lines.Add(Format('StageABC.Decode[%d].SampleCount=%4.4f', [i,fl]));
                                  end;
                               end;

                               for i := 0 to 0
                               do begin
                                  with twCIE.StageLMN.Decode[i]
                                  do begin
                                     fl := FIX32ToFloat(StartIn);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].StartIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(BreakIn);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].BreakIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(EndIn);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].EndIn=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(StartOut);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].StartOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(BreakOut);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].BreakOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(EndOut);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].EndOut=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(Gamma);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].Gamma=%4.4f', [i,fl]));
                                     fl := FIX32ToFloat(SampleCount);
                                     mStruct.Lines.Add(Format('StageLMN.Decode[%d].SampleCount=%4.4f', [i,fl]));
                                  end;
                               end;

                               with twCIE.WhitePoint
                               do begin
                                  fx := FIX32ToFloat(x);
                                  fy := FIX32ToFloat(y);
                                  fz := FIX32ToFloat(z);
                                  Buffer := Format('WhitePoint (x, y, z)=%4.4f, %4.4f, %4.4F', [fx, fy, fz]);
                                  mStruct.Lines.Add(Buffer);
                               end;
                               with twCIE.BlackPoint
                               do begin
                                  fx := FIX32ToFloat(x);
                                  fy := FIX32ToFloat(y);
                                  fz := FIX32ToFloat(z);
                                  Buffer := Format('BlackPoint (x, y, z)=%4.4f, %4.4f, %4.4F', [fx, fy, fz]);
                                  mStruct.Lines.Add(Buffer);
                               end;
                               with twCIE.WhitePaper
                               do begin
                                  fx := FIX32ToFloat(x);
                                  fy := FIX32ToFloat(y);
                                  fz := FIX32ToFloat(z);
                                  Buffer := Format('WhitePaper (x, y, z)=%4.4f, %4.4f, %4.4F', [fx, fy, fz]);
                                  mStruct.Lines.Add(Buffer);
                               end;
                               with twCIE.BlackInk
                               do begin
                                  fx := FIX32ToFloat(x);
                                  fy := FIX32ToFloat(y);
                                  fz := FIX32ToFloat(z);
                                  Buffer := Format('BlackInc (x, y, z)=%4.4f, %4.4f, %4.4F', [fx, fy, fz]);
                                  mStruct.Lines.Add(Buffer);
                               end;
}
			       // GlobalFree(twCIE.Samples);
                          end;
  *)
                        end;
  DAT_GRAYRESPONSE    : begin
  (*
			  { Normally, the size of GrayResponse is related to   }
                          { the ImageInfo bitsPerPixel for purposes of testing }
                          { the functionality to a source, the bitsPerPixel is }
			  { assumed to be 4, or 16 colors.  Additionally, the  }
                          { PixelType must be set to TWPT_GRAY.                }

                          // ImageInfo(@twImageInfo);
                          // if (twImageInfo.PixelType = TWPT_GRAY)
                          // then begin
                                  twHandle := GlobalAlloc(GHND, SizeOf(TW_ELEMENT8) * 16);
				  if (twHandle <> 0)
                                  then begin
					pGrayResponse := GlobalLock(twHandle);
					GetResponse(Handle, pGrayResponse, 16);
	            	                GlobalUnlock(twHandle);

//                                        Result := TWGrayResponse(pGrayResponse, FMSG, Handle);

                                        mStruct.Clear;

	            	                if (Result = TWRC_SUCCESS)
					then DisplayResponse(Handle, pGrayResponse, 16);

					GlobalFree(twHandle);
                                  end
                                  else Result := TWRC_FAILURE;
                          // end;
  *)
                        end;
  DAT_IMAGEFILEXFER   : begin
//mxm                          TTypeCastTWAINIntf(mcmTWAIN).ImageFileXfer;
                          // ON SUCCESS - HOW TO NOTIFY MAIN WINDOW THAT IMAGE
                          // AVAILABLE.
                        end;
  DAT_IMAGEMEMXFER    : begin
  (*
                          mStruct.Clear;
//                          Result := TWImageMemXfer(TW_MEMREF(hBmp), Nil, 0, 0, 0, Handle);
{
                          if (Result = TWRC_SUCCESS)
                          then SendMessage(Application.Handle, PM_XFERDONE, WPARAM(hBmp), 0);
}
  *)
                        end;
  DAT_IMAGENATIVEXFER : begin
  (*
                          mStruct.Clear;
//                          Result := TWImageNativeXfer(TW_MEMREF(hBmp), Handle);
{
                          if (Result = TWRC_SUCCESS)
                          then SendMessage(Application.Handle, PM_XFERDONE, WPARAM(hBmp), 0);
}
  *)
                        end;
  DAT_IMAGEINFO       : begin
                          ImageInfo := mcmTWAIN.GetImageInfo;
                          sgData.Cells[1,1]  := FloatToStrF(ImageInfo.XResolution, FFFixed, 15, 2);
                          sgData.Cells[1,2]  := FloatToStrF(ImageInfo.YResolution, FFFixed, 15, 2);
                          sgData.Cells[1,3]  := IntToStr(ImageInfo.ImageWidth);
                          sgData.Cells[1,4]  := IntToStr(ImageInfo.ImageLength);
                          sgData.Cells[1,5]  := IntToStr(ImageInfo.SamplesPerPixel);
                          sgData.Cells[1,6]  := IntToStr(ImageInfo.BitsPerSample[0]);
                          sgData.Cells[1,7]  := IntToStr(ImageInfo.BitsPerSample[1]);
                          sgData.Cells[1,8]  := IntToStr(ImageInfo.BitsPerSample[2]);
                          sgData.Cells[1,9]  := IntToStr(ImageInfo.BitsPerSample[3]);
                          sgData.Cells[1,10] := IntToStr(ImageInfo.BitsPerSample[4]);
                          sgData.Cells[1,11] := IntToStr(ImageInfo.BitsPerSample[5]);
                          sgData.Cells[1,12] := IntToStr(ImageInfo.BitsPerSample[6]);
                          sgData.Cells[1,13] := IntToStr(ImageInfo.BitsPerSample[7]);
                          sgData.Cells[1,14] := IntToStr(ImageInfo.BitsPerPixel);
                          sgData.Cells[1,15] := mcmTWAINLog.MatchTwainInt('ICAP_PLANARCHUNKY', integer(ImageInfo.Planar), 1024);
                          sgData.Cells[1,16] := mcmTWAINLog.MatchTwainInt('ICAP_PIXELTYPE', ImageInfo.PixelType, 1024);
                          sgData.Cells[1,17] := mcmTWAINLog.MatchTwainInt('ICAP_COMPRESSION', ImageInfo.Compression, 1024);
                        end;
  DAT_IMAGELAYOUT     : begin
                          case FMSG of
                          MSG_GET,
                          MSG_GETDEFAULT,
                          MSG_RESET       : begin
                                              // ImageLayout := mcmTWAIN.GetImageLayout;
                                              if (TTypeCastTWAINIntf(mcmTWAIN).ImageLayoutA(@TWImageLayout, FMSG) = 0)
                                              then begin
                                                   sgData.Cells[1,1] := FloatToStrF(FIX32ToFloat(TWImageLayout.Frame.Left), FFFixed, 15, 2);
                                                   sgData.Cells[1,2] := FloatToStrF(FIX32ToFloat(TWImageLayout.Frame.Top), FFFixed, 15, 2);
                                                   sgData.Cells[1,3] := FloatToStrF(FIX32ToFloat(TWImageLayout.Frame.Right), FFFixed, 15, 2);
                                                   sgData.Cells[1,4] := FloatToStrF(FIX32ToFloat(TWImageLayout.Frame.Bottom), FFFixed, 15, 2);
                                                   sgData.Cells[1,5] := IntToStr(TWImageLayout.DocumentNumber);
                                                   sgData.Cells[1,6] := IntToStr(TWImageLayout.PageNumber);
                                                   sgData.Cells[1,7] := IntToStr(TWImageLayout.FrameNumber);
                                              end;
                                            end;
                          MSG_SET         : begin
                                              ImageLayout.Frame.Left     := StrToFloat(sgData.Cells[1,1]);
                                              ImageLayout.Frame.Top      := StrToFloat(sgData.Cells[1,2]);
                                              ImageLayout.Frame.Right    := StrToFloat(sgData.Cells[1,3]);
                                              ImageLayout.Frame.Bottom   := StrToFloat(sgData.Cells[1,4]);
                                              ImageLayout.DocumentNumber := StrToInt(sgData.Cells[1,5]);
                                              ImageLayout.PageNumber     := StrToInt(sgData.Cells[1,6]);
                                              ImageLayout.FrameNumber    := StrToInt(sgData.Cells[1,7]);
                                              mcmTWAIN.SetImageLayout(ImageLayout);
                                            end;
                          end;
                        end;
  DAT_JPEGCOMPRESSION : begin
                          Result := TWRC_FAILURE;
                        end;
  DAT_PALETTE8        : begin
                          GetMem(pPalette, SizeOf(TLogPalette) + 256 * SizeOf(TPaletteEntry));
                          try
                            case FMSG of
                            MSG_GET,
                            MSG_GETDEFAULT,
                            MSG_RESET       : begin
                                                if (TTypeCastTWAINIntf(mcmTWAIN).Palette8(pPalette, FPixelType, FMSG) = TWRC_SUCCESS)
                                                then begin
                                                     for i := 0 to (pPalette^.palNumEntries - 1)
                                                     do begin
                                                        sgData.Cells[1,1+i] := IntToStr(pPalette^.palPalEntry[i].peRed);
                                                        sgData.Cells[2,1+i] := IntToStr(pPalette^.palPalEntry[i].peGreen);
                                                        sgData.Cells[3,1+i] := IntToStr(pPalette^.palPalEntry[i].peBlue);
                                                     end;
                                                end;
                                              end;
                            MSG_SET         : begin
                                                {
                                                Palette8(pPalette, PixelType, FMSG);
                                                then begin

                                                end;
                                                }
                                              end;
                            end;
                          finally
                            FreeMem(pPalette);
                          end;
  (*
                          mStruct.Clear;
//                          Result := TWPalette8(@Palette, FMSG, Handle);
			  if (Result = TWRC_SUCCESS)
			  then begin
{
                               mStruct.Lines.Add('NumColors='   + IntToStr(Palette.NumColors));
                               mStruct.Lines.Add('PaletteType=' + IntToStr(Palette.PaletteType));
                               for i := 0 to (Palette.NumColors - 1)
			       do begin
                                  with Palette.Colors[i]
                                  do Buffer := Format('Color[%d]=[%d, %d, %d]', [i,Channel1,Channel2,Channel3]);
                                  mStruct.Lines.Add(Buffer);
                               end;
}
                          end;
  *)
                        end;
  DAT_RGBRESPONSE     : begin
  (*
        	          { The BitsPerPixel is assumed to be 4 or 16 colors.  }
                          { Normally use ImageInfo to determine size of        }
                          { structure required.  Since RGBResponse and         }
                          { GrayResponse are identical structures in all ways  }
                          { except name, the GrayResponse structure and        }
                          { functions are used here for simplicity.            }

                          // ImageInfo(@twImageInfo);
                          // if (twImageInfo.PixelType = TWPT_GRAY)
                          // then begin
                                  twHandle := GlobalAlloc(GHND, SizeOf(TW_ELEMENT8) * 16);
				  if (twHandle <> 0)
                                  then begin
					pGrayResponse := GlobalLock(twHandle);
					GetResponse(Handle, pGrayResponse, 16);
	            	                GlobalUnlock(twHandle);

//                                        Result := TWRGBResponse(pGrayResponse, FMSG, Handle);

                                        mStruct.Clear;

	            	                if (Result = TWRC_SUCCESS)
					then DisplayResponse(Handle, pGrayResponse, 16);

					GlobalFree(twHandle);
                                  end
                                  else Result := TWRC_FAILURE;
                          // end;
  *)
                        end;
  // DAT_PASSTHRU
  DAT_EXTIMAGEINFO  : begin
                      end;
  else                begin
                        Result := TWRC_FAILURE;
                      end;
  end;

  // Convert return code, to text.
  lResultVal.Caption := mcmTWAINLog.MatchTwainInt('ReturnCode', mcmTWAIN.DSResult, 1024);
  // Convert condition code, to text.
  lStatusVal.Caption := mcmTWAINLog.MatchTwainInt('ConditionCode', mcmTWAIN.DSStatus, 1024);
end; { End TDlgTwnSend.ImageMsg.                                               }


function TDlgTwnSend.AudioMsg : TW_UINT16;
begin
  Result := 0;
  ShowMessage('NOT SUPPORTED');
  (*
  case FDAT of
  // DAT_AUDIOFILEXFER
  // DAT_AUDIOINFO
  // DAT_AUDIONATIVEXFER
  else begin
       end;
  end;
  *)
end; { End TDlgTwnSend.AudioMsg.                                               }


procedure TDlgTwnSend.DisplayContainer(Cap : TW_UINT16);
var ItemStr      : string;
    CapStr       : string;
    ConStr       : string;
    index, j     : integer;
    Container    : TtwnContainer;
begin
  try
    Container := mcmTWAIN.Containers.Items[Cap];
    if Assigned(Container)
    then begin
         ConStr := mcmTWAINLog.Container2Str(Container.ContainerType);
         index := cbCON.Items.IndexOf(ConStr);
         cbCON.ItemIndex := index;
         FCON := Container.ContainerType;
         CapStr  := mcmTWAINLog.Cap2Str(Cap);

         if (FCAP = ICAP_FRAMES)
         then begin
              case Container.ContainerType of
              TWON_ONEVALUE    : begin
                                   sgIndex := 1;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(-1);
                                   sgData.Cells[1,1] := FloatToStrF(Container.Frames[0].Left, ffFixed, 15, 2);
                                   sgData.Cells[1,2] := FloatToStrF(Container.Frames[0].Top, ffFixed, 15, 2);
                                   sgData.Cells[1,3] := FloatToStrF(Container.Frames[0].Right, ffFixed, 15, 2);
                                   sgData.Cells[1,4] := FloatToStrF(Container.Frames[0].Bottom, ffFixed, 15, 2);
                                 end;
              TWON_ENUMERATION : begin
                                   sgIndex := 3;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(Container.NumItems);
                                   sgData.Cells[0,1] := 'CurrentIndex';
                                   sgData.Cells[1,1] := IntToStr(Container.CurrentIndex);
                                   sgData.Cells[0,2] := 'DefaultIndex';
                                   sgData.Cells[1,2] := IntToStr(Container.DefaultIndex);

                                   for index := 0 to (Container.NumItems - 1)
                                   do begin
                                      j := 4 * index;
                                      sgData.Cells[1,sgIndex+j]   := FloatToStrF(Container.Frames[index].Left, ffFixed, 15, 2);
                                      sgData.Cells[1,sgIndex+j+1] := FloatToStrF(Container.Frames[index].Top, ffFixed, 15, 2);
                                      sgData.Cells[1,sgIndex+j+2] := FloatToStrF(Container.Frames[index].Right, ffFixed, 15, 2);
                                      sgData.Cells[1,sgIndex+j+3] := FloatToStrF(Container.Frames[index].Bottom, ffFixed, 15, 2);
                                   end;
                                 end;
              end;
              index := sgIndex + 4 * Container.NumItems;
         end
         else begin
              case Container.ContainerType of
              TWON_ONEVALUE    : begin
                                   sgIndex := 1;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(-1);
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.Items[0]);
                                   sgData.Cells[1,1] := ItemStr;
                                 end;
              TWON_ENUMERATION : begin
                                   sgIndex := 3;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(Container.NumItems);
                                   sgData.Cells[1,1] := IntToStr(Container.CurrentIndex);
                                   sgData.Cells[1,2] := IntToStr(Container.DefaultIndex);
                                   for index := 0 to (Container.NumItems - 1)
                                   do begin
                                      ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                           Container.Capability,
                                                                           Container.ItemType,
                                                                           Container.Items[index]);
                                      sgData.Cells[1,sgIndex+index] := ItemStr;
                                   end;
                                 end;
              TWON_RANGE       : begin
                                   sgIndex := 5;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(-1);
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.MinValue);
                                   sgData.Cells[1,1] := ItemStr;
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.MaxValue);
                                   sgData.Cells[1,2] := ItemStr;
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.StepValue);
                                   sgData.Cells[1,3] := ItemStr;
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.DefaultValue);
                                   sgData.Cells[1,4] := ItemStr;
                                   ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                        Container.Capability,
                                                                        Container.ItemType,
                                                                        Container.CurrentValue);
                                   sgData.Cells[1,5] := ItemStr;
                                 end;
              TWON_ARRAY       : begin
                                   sgIndex := 1;
                                   SetItemType(Container.ItemType);
                                   SetNumItems(Container.NumItems);
                                   for index := 0 to (Container.NumItems - 1)
                                   do begin
                                      ItemStr := mcmTWAINLog.Attribute2Str(CapStr,
                                                                           Container.Capability,
                                                                           Container.ItemType,
                                                                           Container.Items[index]);
                                      sgData.Cells[1,1+index] := ItemStr;
                                   end;
                                 end;
              end;
              index := sgIndex + Container.NumItems;
         end;
         if (index < 2)
         then index := 2;
         sgData.RowCount := index;
    end;
  except
    On E:Exception
    do ShowMessage(E.Message);
  end;
end; { End TDlgTwnSend.DisplayContainer.                                       }


function TDlgTwnSend.GetCapabilitySend(Cap     : TW_UINT16;
                                       Msg     : TW_UINT16;
                                       ConType : TW_UINT16) : TW_UINT16;
var ConStr       : string;
    index        : integer;
    Container    : TtwnContainer;
begin
  Result := TWRC_FAILURE;
  try
    Container := mcmTWAIN.Containers.Items[Cap];
    if Not(Assigned(Container))
    then Container := mcmTWAIN.Containers.CreateItem(Cap);
    try
      if (Msg <> MSG_QUERYSUPPORT)
      then Container.ContainerType := ConType;

      ClearData;
      Result := mcmTWAIN.GetCapabilityMsg(Cap, Msg, Container);
      if (Result = TWRC_SUCCESS) and (Msg <> MSG_QUERYSUPPORT)
      then begin
           ConStr := mcmTWAINLog.Container2Str(Container.ContainerType);
           index := cbCON.Items.IndexOf(ConStr);
           cbCON.ItemIndex := index;
           FCON := Container.ContainerType;

           DisplayContainer(Cap);
           SetStructure;
      end;

      if (Result = TWRC_SUCCESS) and (Msg = MSG_QUERYSUPPORT)
      then begin
           sgData.Cells[0,1] := 'MSG_GET';
           sgData.Cells[0,2] := 'MSG_SET';
           sgData.Cells[0,3] := 'MSG_GETDEFAULT';
           sgData.Cells[0,4] := 'MSG_GETCURRENT';
           sgData.Cells[0,5] := 'MSG_RESET';
           if (Container.QuerySupport >= 0)
           then begin
           for index := 0 to 4
           do begin
              if ((Container.QuerySupport and (1 shl index)) <> 0)
              then sgData.Cells[1,index+1] := 'YES'
              else sgData.Cells[1,index+1] := 'NO';
           end;
           end
           else begin
                sgData.Cells[1,1] := '?';
                sgData.Cells[1,2] := '?';
                sgData.Cells[1,3] := '?';
                sgData.Cells[1,4] := '?';
                sgData.Cells[1,5] := '?';
           end;
           sgData.RowCount := 6;
      end;
    finally
     //
    end;
  except
    On E:Exception
    do ShowMessage(E.Message);
  end;
end; { End TDlgTwnSend.GetCapabilitySend.                                      }


function TDlgTwnSend.ItemType2Value(ItemStr : string) : TW_UINT16;
//------------------------------------------------------------------------------
// ItemType2Value - Converts the string representation of the data type to the
// TWAIN define value.
//------------------------------------------------------------------------------
begin
  try
    if (ItemStr <> '')
    then Result := GetPrivateProfileInt('ItemType', PChar(ItemStr), 99, PChar(mcmTWAINLog.INIFile))
    else Result := 99;
  except
    Result := 99;
  end;
end; { End TDlgTwnSend.ItemType2Value.                                         }


function TDlgTwnSend.Token2Value(CapStr   : string;
                                 Token    : string;
                                 ItemType : TW_UINT16) : Variant;
//------------------------------------------------------------------------------
// Token2Value - Either converts the string Token to a number representing
// the TWAIN define value or to the actual number.
//------------------------------------------------------------------------------
begin
  Result := 0;
  if (Length(Token) <= 0)
  then begin
       // ErrorMsg(CapStr, BADFORMAT);
  end
  else begin
       case ItemType of
       TWTY_STR32,
       TWTY_STR64,
       TWTY_STR128,
       TWTY_STR255  : Result := Token;
       TWTY_STR1024 : Result := Token;
       TWTY_UNI512  : Result := Token;
       else begin
            if (IsCharAlpha(Token[1]))
            then Result := integer(GetPrivateProfileInt(PChar(CapStr), PChar(Token), 99, PChar(mcmTWAINLog.INIFile)))
            else begin
                 case ItemType of
                 TWTY_UINT8,
                 TWTY_INT16,
                 TWTY_UINT16,
                 TWTY_INT32,
                 TWTY_UINT32 : Result := StrToInt(Token);
                 TWTY_FIX32  : begin
                                 {$IFDEF GE_DXE}
                                   if ('.' <> FormatSettings.DecimalSeparator)
                                   then if (Pos('.', Token) <> 0)
                                        then Token[Pos('.', Token)] := FormatSettings.DecimalSeparator;
                                   if (',' <> FormatSettings.DecimalSeparator)
                                   then if (Pos(',', Token) <> 0)
                                        then Token[Pos('.', Token)] := FormatSettings.DecimalSeparator;
                                   Result := StrToFloat(Token);
                                 {$ELSE}
                                   if ('.' <> DecimalSeparator)
                                   then if (Pos('.', Token) <> 0)
                                        then Token[Pos('.', Token)] := DecimalSeparator;
                                   if (',' <> DecimalSeparator)
                                   then if (Pos(',', Token) <> 0)
                                        then Token[Pos('.', Token)] := DecimalSeparator;
                                   Result := StrToFloat(Token);
                                 {$ENDIF}
                               end;
                 end;
            end;
       end;
       end;
  end;
end; { End TDlgTwnSend.Token2Value.                                            }


function TDlgTwnSend.SetCapabilitySend(Cap     : TW_UINT16;
                                       Msg     : TW_UINT16;
                                       ConType : TW_UINT16) : TW_UINT16;

    function GetStrToken(var a : string; b : string) : string;
    begin
      if (Length(a) > 0)
      then begin
           if (Pos(b, a) <> 0)
           then begin
                Result := Copy(a, 1, Pos(b, a) - 1);
                a := Copy(a, Pos(b, a) + 1, Length(a));
           end
           else begin
                Result := a;
                a := '';
           end;
      end
      else Result := '';
    end; { End StrTok.                                                             }

var index, j     : integer;
    ItemStr      : string;
    CapStr       : string;
    Container    : TtwnContainer;
begin
  Result := TWRC_FAILURE;
  Container := mcmTWAIN.Containers.Items[Cap];
  if Not(Assigned(Container))
  then Container := mcmTWAIN.Containers.CreateItem(Cap);
  try
    Container.Clear;
    Container.Capability    := Cap;
    Container.ContainerType := ConType;

    if (cbItemType.ItemIndex >= 0)
    then Container.ItemType := ItemType2Value(cbItemType.Items[cbItemType.ItemIndex]);

    try
      CapStr  := mcmTWAINLog.Cap2Str(Cap);
      if (FCAP = ICAP_FRAMES)
      then begin
           case Container.ContainerType of
           TWON_ONEVALUE    : begin
                                ItemStr := sgData.Cells[1,1];
                                Container.Frames[0].Left   := StrToFloat(ItemStr);
                                ItemStr := sgData.Cells[1,2];
                                Container.Frames[0].Top    := StrToFloat(ItemStr);
                                ItemStr := sgData.Cells[1,3];
                                Container.Frames[0].Right  := StrToFloat(ItemStr);
                                ItemStr := sgData.Cells[1,4];
                                Container.Frames[0].Bottom := StrToFloat(ItemStr);
                              end;
           TWON_ENUMERATION : begin
                                Container.NumItems := Round(rsNumItems.Value);
                                for index := 0 to (Container.NumItems - 1)
                                do begin
                                   j := 3 + index * 4;
                                   ItemStr := sgData.Cells[1,j];
                                   Container.Frames[index].Left   := StrToFloat(ItemStr);
                                   ItemStr := sgData.Cells[1,j+1];
                                   Container.Frames[index].Top    := StrToFloat(ItemStr);
                                   ItemStr := sgData.Cells[1,j+2];
                                   Container.Frames[index].Right  := StrToFloat(ItemStr);
                                   ItemStr := sgData.Cells[1,j+3];
                                   Container.Frames[index].Bottom := StrToFloat(ItemStr);
                                end;
                                ItemStr := sgData.Cells[1,1]; // CurrentIndex
                                Container.CurrentIndex := Token2Value(CapStr, ItemStr, Container.ItemType);
                                ItemStr := sgData.Cells[1,2]; // DefaultIndex
                                Container.DefaultIndex := Token2Value(CapStr, ItemStr, Container.ItemType);
                              end;
           end;
      end
      else begin
           case Container.ContainerType of
           TWON_ONEVALUE    : begin
                                ItemStr := sgData.Cells[1,1];
                                Container.CurrentValue := Token2Value(CapStr, ItemStr, Container.ItemType);
                              end;
           TWON_ENUMERATION : begin
                                Container.NumItems := Round(rsNumItems.Value);
                                for index := 0 to (Container.NumItems - 1)
                                do begin
                                   ItemStr := sgData.Cells[1,3+index];
                                   Container.Items[index] := Token2Value(CapStr, ItemStr, Container.ItemType);
                                end;
                                ItemStr := sgData.Cells[1,1]; // CurrentIndex
                                Container.CurrentIndex := Token2Value(CapStr, ItemStr, Container.ItemType);
                                ItemStr := sgData.Cells[1,2]; // DefaultIndex
                                Container.DefaultIndex := Token2Value(CapStr, ItemStr, Container.ItemType);
                              end;
           TWON_RANGE       : begin
                                ItemStr := sgData.Cells[1,1]; // MinValue
                                Container.MinValue := Token2Value(CapStr, ItemStr, Container.ItemType);

                                ItemStr := sgData.Cells[1,2]; // MaxValue
                                Container.MaxValue := Token2Value(CapStr, ItemStr, Container.ItemType);

                                // Don't change Step value - set by data source.
                                // ItemStr := sgData.Cells[1,3]; // StepValue
                                // Container.StepValue := Token2Value(CapStr, ItemStr, Container.ItemType);

                                ItemStr := sgData.Cells[1,4]; // DefaultValue
                                Container.DefaultValue := Token2Value(CapStr, ItemStr, Container.ItemType);

                                ItemStr := sgData.Cells[1,5]; // CurrentValue
                                Container.CurrentValue := Token2Value(CapStr, ItemStr, Container.ItemType);
                              end;
           TWON_ARRAY       : begin
                                Container.NumItems := Round(rsNumItems.Value);
                                for index := 0 to (Container.NumItems - 1)
                                do begin
                                   ItemStr := sgData.Cells[1,1+index];
                                   Container.Items[index] := Token2Value(CapStr, ItemStr, Container.ItemType);
                                end;
                              end;
           end;
      end;
      if (mcmTWAIN.SetCapabilityMsg(Msg, False, Container) <> TWRC_SUCCESS)
      then ;
    finally
      //
    end;
  except
    On E:Exception
    do ShowMessage(E.Message);
  end;
end; { End TDlgTwnSend.SetCapabilitySend.                                      }

{------------------------------------------------------------------------------}
{ GetResponse -                                                                }
{------------------------------------------------------------------------------}
(*
function TDlgTwnSend.GetResponse(hDlg      : hWnd;
                                 pResponse : pTW_GRAYRESPONSE;
                                 Bits      : TW_UINT16) : boolean;
var x : TW_UINT16;
begin
  for x := 0 to (Bits - 1)
  do begin
     //SendDlgItemMessage(hDlg,IDEDIT,EM_GETLINE,x,(DWORD)(pTW_STR255)string1);
     //RemoveEquals(string1);
     pResponse^.Response[x].Index    := TW_UINT8(x);
     pResponse^.Response[x].Channel1 := TW_UINT8(x);
     pResponse^.Response[x].Channel2 := TW_UINT8(x);
     pResponse^.Response[x].Channel3 := TW_UINT8(x);
  end;
  Result := True;
end; { End TDlgTwnSend.GetResponse.                                            }


{------------------------------------------------------------------------------}
{ DisplayResponse -                                                            }
{------------------------------------------------------------------------------}

function TDlgTwnSend.DisplayResponse(hDlg      : hWnd;
                                     pResponse : pTW_GRAYRESPONSE;
                                     Bits      : TW_UINT16) : boolean;
var x      : TW_UINT16;
    Buffer : string;
begin
  for x := 0 to (Bits - 1)
  do begin
     with pResponse^.Response[x]
     do Buffer := Format('Response[%d]=%d,%d,%d', [x, Channel1, Channel2, Channel3]);
  end;
  Result := True;
end; { End TDlgTwnSend.DisplayResponse.                                        }
*)

procedure TDlgTwnSend.ClearData;
var i, j : integer;
begin
  for i := 0 to sgData.ColCount
  do for j := 1 to sgData.RowCount
     do sgData.Cells[i, j] := '';
end; { End TDlgTwnSend.ClearData.                                              }


procedure TDlgTwnSend.rsNumItemsChange(Sender : TObject);
var i, j   : integer;
    TmpStr : string;
begin
  if (sgIndex >= 0)
  then begin
       if (FCAP = ICAP_FRAMES)
       then begin
            for i := 0 to (round(rsNumItems.Value) - 1)
            do begin
               j := i * 4;
               TmpStr := 'Item[' + IntToStr(i) + ']';
               sgData.Cells[0,j+sgIndex] := TmpStr + '.Left';
               sgData.Cells[0,j+1+sgIndex] := TmpStr + '.Top';
               sgData.Cells[0,j+2+sgIndex] := TmpStr + '.Right';
               sgData.Cells[0,j+3+sgIndex] := TmpStr + '.Bottom';
            end;
            i := sgIndex + 4 * round(rsNumItems.Value);
       end
       else begin
            for i := 0 to (round(rsNumItems.Value) - 1)
            do sgData.Cells[0,i+sgIndex] := 'Item[' + IntToStr(i) + ']';
            i := sgIndex + round(rsNumItems.Value);
       end;
       if (i < 2)
       then i := 2;
       sgData.RowCount := i;
  end;
end; { End TDlgTwnSend.rsNumItemsChange.                                       }


{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

end.

