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
// $Log:  21606: uFormFileAssociate.pas 
//
//    Rev 1.3    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    23-08-2005 21:56:48  mcm    Version: IMG 2.9
// Modified to replace use of TmcmFileFormats with
// TmcmImageFileMgr.GetFormatList (TmcmFileFormatList and TmcmFileFormatItem).

//
//   Rev 1.1    22-12-2003 16:00:06  mcm

//
//   Rev 1.0    29-09-2003 18:34:38  mcm    Version: IMG 1.6

unit uFormFileAssociate;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, checklst,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
     {$ENDIF}
     mcmImageFile;

{$IFDEF VER100} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER110} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER120} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER145} {$DEFINE DCB3_6} {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

type
  TFormFormatAssociate = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    clbFileFormats: TCheckListBox;
    btnSelectAll: TButton;
    btnRemoveAll: TButton;
    btnHelp: TButton;
    lAssociateFile: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnRemoveAllClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
    FRegKey : string;
  public
    { Public declarations }
    property RegKey : string
      read   FRegKey
      write  FRegKey;
  end;

var FormFormatAssociate : TFormFormatAssociate;

implementation

{$R *.DFM}
{$R mcmFileFormat.RES}

uses mcmImageTypeDef;


procedure TFormFormatAssociate.FormCreate(Sender : TObject);
var i           : integer;
    FileStr     : string;
    FileDesc    : string;
begin
  // Fill check list box with supported file formats.
  clbFileFormats.Clear;

  for i := 0 to (ImageFileManager.GetFormatList.Count - 1)
  do begin
     if ImageFileManager.GetFormatList.Items[i].ReadEnabled
     then begin
          // Get extension string from file manager.
          FileStr  := ImageFileManager.GetFormatList.Items[i].GetFilter;
          FileDesc := ImageFileManager.GetFormatList.Items[i].Description;
          FileDesc := FileDesc + ' (' + FileStr + ')';

          if (FileStr <> '') and (FileStr <> '')
          then begin
               clbFileFormats.Items.AddObject(FileDesc, pointer(ImageFileManager.GetFormatList.Items[i].FileClassID));

               // Check if the file format is associated with our application.
               if ImageFileManager.IsFileFormatAssociated(ImageFileManager.GetFormatList.Items[i].FileClassID, Application.ExeName)
               then clbFileFormats.Checked[clbFileFormats.Items.Count-1] := True;
          end;
     end;
  end;
end; // TFormFormatAssociate.FormCreate.


procedure TFormFormatAssociate.btnOKClick(Sender : TObject);
var i           : integer;
    FileFormat  : TmcmFileFormat;
begin
  // Set or remove file format association.
  for i := 0 to (clbFileFormats.Items.Count - 1)
  do begin
     FileFormat := TmcmFileFormat(clbFileFormats.Items.Objects[i]);
     // Associate file format with our application if clbFileFormats.Checked[i] = True
     // otherwise remove association.
     ImageFileManager.AssociateFileFormat(FileFormat,
                                          Application.ExeName,
                                          1 + integer(FileFormat),
                                          clbFileFormats.Checked[i]);
  end;
end; // TFormFormatAssociate.btnOKClick.


procedure TFormFormatAssociate.btnSelectAllClick(Sender : TObject);
var i : integer;
begin
  for i := 0 to (clbFileFormats.Items.Count - 1)
  do clbFileFormats.Checked[i] := True;
end; // TFormFormatAssociate.btnSelectAllClick.


procedure TFormFormatAssociate.btnRemoveAllClick(Sender : TObject);
var i : integer;
begin
  for i := 0 to (clbFileFormats.Items.Count - 1)
  do clbFileFormats.Checked[i] := False;
end; // TFormFormatAssociate.btnRemoveAllClick.


procedure TFormFormatAssociate.btnHelpClick(Sender : TObject);
begin
  ShowMessage('Enter your help topic here.');
end; // TFormFormatAssociate.btnHelpClick.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}
end.
