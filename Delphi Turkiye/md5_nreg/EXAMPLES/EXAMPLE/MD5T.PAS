{*******************************************************************************
* Unit      : MD5T                                                             *
********************************************************************************
* Purpose   : Demonstrates the use of the MD5 component by hashing a simple    *
*             string or the contents of a file                                 *
********************************************************************************
* Copyright : This component is copyright TSM Inc. 1998-1999                   *
*             This source code may not be distributed to third parties in      *
*             or in part without the written permission of TSM Inc.            *
*             All rights reserved. Liability limited to replacement of         *
*             this original source code in the case of loss or damage because  *
*             the use or misuse of this software.                              *
********************************************************************************
* Version   : 07.06.98  - 1.00  Original component                             *
*             27.08.98  - 1.10  Delphi4 support added                          *
*             30.10.98  - 1.11  Bug fixed in Hashfile (wrong hashes), optimise *
*             14.11.98  - 1.11a Bugfix - finish didn't produce correct digests *
*             03.04.99  - 1.11b Code tidy up                                   *
********************************************************************************
* Switches  : None                                                             *
*******************************************************************************}

unit MD5T;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MD5, ExtCtrls;

{*******************************************************************************
* Type      : TfrmMD5Test                                                      *
********************************************************************************
* Purpose   : Defines the form                                                 *
*******************************************************************************}
type
  TfrmMD5Test = class(TForm)
    MD51: TMD5;
    grpInput:      TGroupBox;
    edtInput:      TEdit;
    btnDoText:     TButton;
    btnDoFile:     TButton;
    lblInput:      TLabel;
    grpHash:       TGroupBox;
    edtHashValue:  TEdit;
    odlFileToHash: TOpenDialog;
    lblHash:       TLabel;
    Bevel1:        TBevel;
    edtVersion:    TEdit;
    lblVersion:    TLabel;
    procedure      btnDoTextClick(Sender: TObject);
    procedure      btnDoFileClick(Sender: TObject);
    procedure      FormCreate(Sender: TObject);
  end;

var
  frmMD5Test:      TfrmMD5Test;

implementation

{$R *.DFM}

{*******************************************************************************
* Procedure : btnDoTextClick                                                   *
********************************************************************************
* Purpose   : Creates the hash of the text in the input box                    *
********************************************************************************
* Paramters : None                                                             *
********************************************************************************
* Returns   : None                                                             *
*******************************************************************************}
procedure TfrmMD5Test.btnDoTextClick(Sender: TObject);
var
     Digest: TDigest;
begin
     // initialise MD5 - this must be done every time you create a hash
     MD51.Init;

     // add the data into the hash - you can mix the calls "HashString" "HashFile"
     // "HashStream" and "HashByte" as you wish
     MD51.HashString(edtInput.Text);

     // Finalise the hash and store the result in Digest
     Digest := MD51.Finish;

     // get the string version of the hash and display it
     edtHashValue.Text := MD51.GetHashString;
end; {TfrmMD5Test.btnDoTextClick}

{*******************************************************************************
* Procedure : btnDoFileClick                                                   *
********************************************************************************
* Purpose   : Creates the hash of the file specified by the open dialog        *
********************************************************************************
* Paramters : None                                                             *
********************************************************************************
* Returns   : None                                                             *
*******************************************************************************}
procedure TfrmMD5Test.btnDoFileClick(Sender: TObject);
var
     Digest: TDigest;
begin
     // get the file name of the file to hash
     if odlFileToHash.Execute then
     begin
          // initialise MD5 - this must be done every time you create a hash
          MD51.Init;

          // add the data into the hash - you can mix the calls "HashString" "HashFile"
          // "HashStream" and "HashByte" as you wish
          MD51.HashFile(odlFileToHash.FileName);

          // Finalise the hash and store the result in Digest
          Digest := MD51.Finish;

          // get the string version of the hash and display it
          edtHashValue.Text := MD51.GetHashString;
     end; {if}
end; {TfrmMD5Test.btnDoFileClick}

{*******************************************************************************
* Procedure : TfrmMD5Test.FormCreate                                           *
********************************************************************************
* Purpose   : Gets the version number of the MD5 component for display         *
********************************************************************************
* Paramters : None                                                             *
********************************************************************************
* Returns   : None                                                             *
*******************************************************************************}
procedure TfrmMD5Test.FormCreate(Sender: TObject);
begin
     // display the version number of the component
     edtVersion.Text := MD51.GetVersion;
end; {TfrmMD5Test.FormCreate}

end.
