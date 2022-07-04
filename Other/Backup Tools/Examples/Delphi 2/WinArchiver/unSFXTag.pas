unit unSFXTag;

interface
uses
  Windows,
  SysUtils,
  Classes,
  ArchiverRoot,
  CustExtractor;

type
  DWord = Integer;
  TOverwritemode=(confirm,overwrite,skip,update,existing,updateexisting);

  TSfxMessages = packed record
                 STR_CANNOTOPEN    :STRING[30]; //'Cannot open file ..';
                 STR_CANNOTCLOSE   :STRING[30]; //'Cannot close the file';
                 STR_E             :STRING[15]; //'Error..';
                 STR_EXISTS        :STRING[30]; //'...already exists, overwrite ?';
                 STR_EARCHIVE      :STRING[30]; //'Error reading archive.';
                 STR_INVALIDNAME   :STRING[20]; //'Invalid filename.';
                 STR_EDIRECTORY    :STRING[25]; //'Error in directory ..';
                 STR_PREXT         :STRING[15]; //'Extracting: ';
                 STR_ALLEXT        :STRING[30]; //'All files have been extracted.';
                 STR_OK            :STRING[15]; //'Finished.';
                 STR_NOTSELEXT     :STRING[50]; //'The selected file(s) couldn''t get extracted.';
                 STR_MP            :STRING[20]; //'MP-Zip Tool';
                 STR_RUN           :STRING[25]; //'After extraction, run : ';
                 STR_EINC_SIZE     :STRING[70]; //'Incorrect file size, please try to download this file again.';

                 DLGNEW_CAPTION    :STRING[20]; //'Create subdirectory';
                 DLGNEW_8001       :STRING[20]; // 'Current directory :';
                 DLGNEW_8002       :STRING[20]; // 'New subdirectory :';
                 OK                :STRING[20]; // '&OK';
                 CANCEL            :STRING[20]; // '&Cancel';
                 DLGSEL_CAPTION    :STRING[30]; // 'Select extract directory';
                 DLGSEL_2002       :STRING[15]; // '&Network...';
                 DLGSEL_2004       :STRING[15]; // 'New...';
                 MAINDIALOG_100    :STRING[25]; // 'Extract to :';
                 MAINDIALOG_8008   :STRING[10]; // 'Files :';
                 MAINDIALOG_1      :STRING[10]; // 'Start';
                 MAINDIALOG_2      :STRING[10]; // 'C&lose';
                 confirm           :STRING[20]; // 'Confirm what to do';
                 overwrite         :STRING[25]; // 'Overwrite existing files';
                 skip              :STRING[25]; // 'Don't extract the files';
                 update            :STRING[25]; // 'Overwrite only if newer';
                 existing          :STRING[45]; // 'Extract the file only if it already exists';
                 updateexisting    :STRING[60]; // 'Extract the file only if it already exists and is newer';
                 existingfiles     :STRING[30]; // 'Existing file(s) :';
                 SelectAll         :STRING[20]; // 'Select All';
                 DeSelectAll       :STRING[20]; // 'Deselect All';
            end;

  TTagInfo = packed record
                ExecuteFileAfterExtract:boolean;
                UserChooseFilesToExtract:boolean;
                UserChooseOverwriteMode:boolean;
                UserAllowedToDontRunTheFile:boolean;
                DefaultOwerwriteMode:TOverwritemode;
                SFXFileSize:DWord; //Dateigröße des fertigen SFX, also: SFX.EXE-Code, sizeof(TTagefile) and Archivesize
                CommandLine:string[80];
                Caption:string[60];
                DefaultExtractPath:string[80];
                CopyrightLine:string[80];
                Messages:TSfxMessages;
                Language:TLanguage;
                end;



var
  TagInfo : TTaginfo;

implementation

initialization
  // Prepare SFX Tag
  FillChar( TagInfo, sizeof(TagInfo), 0 );
TagInfo.Messages.STR_CANNOTOPEN    :='Cannot open file ..';
TagInfo.Messages.STR_CANNOTCLOSE   :='Cannot close the file';
TagInfo.Messages.STR_E             :='Error..';
TagInfo.Messages.STR_EXISTS        :='...already exists, overwrite ?';
TagInfo.Messages.STR_EARCHIVE      :='Error reading archive.';
TagInfo.Messages.STR_INVALIDNAME   :='Invalid filename.';
TagInfo.Messages.STR_EDIRECTORY    :='Error in directory ..';
TagInfo.Messages.STR_PREXT         :='Extracting: ';
TagInfo.Messages.STR_ALLEXT        :='All files have been extracted.';
TagInfo.Messages.STR_OK            :='Finished.';
TagInfo.Messages.STR_NOTSELEXT     :='The selected file(s) couldn''t get extracted.';
TagInfo.Messages.STR_MP            :='TArchiver SFX Tool';
TagInfo.Messages.STR_RUN           :='After extraction, run : ';
TagInfo.Messages.STR_EINC_SIZE     :='Incorrect file size, please try to download this file again.';

TagInfo.Messages.DLGNEW_CAPTION    :='Create subdirectory';
TagInfo.Messages.DLGNEW_8001       :='Current directory :';
TagInfo.Messages.DLGNEW_8002       :='New subdirectory :';
TagInfo.Messages.OK                :='&OK';
TagInfo.Messages.CANCEL            :='&Cancel';
TagInfo.Messages.DLGSEL_CAPTION    :='Select extract directory';
TagInfo.Messages.DLGSEL_2002       :='&Network...';
TagInfo.Messages.DLGSEL_2004       :='New...';
TagInfo.Messages.MAINDIALOG_100    :='Extract to :';
TagInfo.Messages.MAINDIALOG_8008   :='Files :';
TagInfo.Messages.MAINDIALOG_1      :='Start';
TagInfo.Messages.MAINDIALOG_2      :='C&lose';
TagInfo.Messages.confirm           :='Ask confirmation';
TagInfo.Messages.overwrite         :='Overwrite existing files';
TagInfo.Messages.skip              :='Skip existing files';
TagInfo.Messages.update            :='Overwrite only if newer';
TagInfo.Messages.existing          :='Restore existing files only';
TagInfo.Messages.updateexisting    :='Extract the file only if it already exists and is newer';
TagInfo.Messages.existingfiles     :='Existing file(s) :';
TagInfo.Messages.SelectAll         :='Select All';
TagInfo.Messages.DeSelectAll       :='Deselect All';


TagInfo.ExecuteFileAfterExtract:=True;
TagInfo.UserChooseFilesToExtract:=True;
TagInfo.UserChooseOverwriteMode:=True;
TagInfo.UserAllowedToDontRunTheFile:=True;
TagInfo.DefaultOwerwriteMode:=confirm;
TagInfo.SFXFileSize:=0;
TagInfo.CommandLine:='c:\test\Ja - es funktioniert.exe';
TagInfo.Caption:='Hello this is the Caption';
TagInfo.DefaultExtractPath:='>PF<Hello this is the ExtractionPath';
TagInfo.CopyrightLine:='This is the CopyrightLine. © by Oliver Buschjost (autor_oliver@iname.com)';
TagInfo.Language:=lgGerman;

finalization
end.
