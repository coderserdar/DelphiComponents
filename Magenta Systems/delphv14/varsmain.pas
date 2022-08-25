// Magenta Systems Ltd - 18th August 2002 - 1.3
// 1.4 - 11th May 2010 - rebuilt with Delphi 2007

unit varsmain;

{Copyright Information
----------------------

Delphi Registry and Initialisation Variables Code Generator
is copyright Magenta Systems Ltd, England, 2010.

Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636
Fax 020 8656 8127, International Fax +44 20 8656 8127

Email: delphi@magsys.co.uk
Web: http://www.magsys.co.uk/delphi/

}

{ outlines of data that is expected and how it's processed

Form Component Types,

copied from *.pas type
  AFileName: TFilenameEdit;
  AEditBox: TEdit;
  ASpinEdit: TSpinEdit;
  ACheckBox: TCheckBox ;
  AMemo: TMemo ;

or copied from RX FormStorage tstringlist:
  'AEditBox.Text'
  'ACheckbox.Checked'

Saved Properties, created from Form Component Types
  AFileName.FileName - String
  AEditBox.Text - String
  ASpinEdit.Value - Integer
  ACheckBox.Checked - Boolean
  AMemo.Lines - TStringList

Defined Properties, set in program but editable
  TEdit.Text - String
  TMemo.Lines - TStringList

Defined Files, set in program to match masks, but editable
  RV - readvars

Defined Masks
  RCString - $comp := ReadString (section, '$lit', '') ;

Variable Declarations
  AEditBox_Text: String ;

Read Code Lines
  AEditBox_Text := ReadString (section, 'AEditBox_Text', '') ;
  AEditBox.Text := ReadString (section, 'AEditBox_Text', '') ;
  ASpinEdit_Value := ReadInteger (section, 'ASpinEdit_Value', 0) ;
  ACheckBox_Checked := ReadBool (section, 'ACheckBox_Checked', false) ;

Write Code Lines
  WriteString (section, 'AEditBox_Text', AEditBox_Text) ;
  WriteBool (section, 'ACheckBox_Checked', ACheckBox_Checked) ;

  EraseSection (section + '.AMemo_Lines') ;
  if AMemo.Lines.Count <> 0 then
    for I := 0 to AMemo.Lines.Count - 1 do
    WriteString (section + '.AMemo_Lines',
    'Item' + IntToStr (I), AMemo_Lines [I]) ;
  WriteInteger (section + '.AMemo_Lines', 'Count', AMemo.Lines.Count) ;

  AMemo_Lines.Clear ;
  I := ReadInteger (section + '.AMemo_Lines', 'Count', 0) ;
  if I <> 0 then for I := 0 to I - 1 do
     AMemo_Lines.Add (ReadSting (section + '.AMemo_Lines',
     'Item' + IntToStr (I), '' )) ;

  ACheckBox.State := TCheckBoxState (ReadInteger (section, 'ACheckBox_State', Ord (cbUnchecked))) ;
  WriteInteger (section, 'ACheckBox_State', Ord (ACheckBox.State)) ;

}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RxDualList, Placemnt, StdCtrls, ComCtrls, Mask, ToolEdit,
  StrHlder, RxStrUtils, Spin, IniFiles, Registry, RXCombos, Appinit ;

// this program needs RX Library to be installed for recompilation.
// use the Project JEDI (http://jvcl.sourceforge.net) version 


type
  TMainForm = class(TForm)
    FormStorage: TFormStorage;
    DualListDialog: TRxDualListDialog;
    DefSuppProps: TStrHolder;
    OpenDialog: TOpenDialog;
    Status: TStatusBar;
    DefFiles: TStrHolder;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    FormComps: TMemo;
    SavedProps: TMemo;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    SuppProps: TMemo;
    CodeMasks: TMemo;
    TabSheet5: TTabSheet;
    OldSpecs: TComboBox;
    Label8: TLabel;
    Label4: TLabel;
    OutDir: TDirectoryEdit;
    Label7: TLabel;
    ArraySize: TSpinEdit;
    Label5: TLabel;
    FilePrefix: TEdit;
    doClear: TButton;
    doOpenOld: TButton;
    doOpenAny: TButton;
    doVarCreate: TButton;
    doSelectSave: TButton;
    doSave: TButton;
    doExit: TButton;
    OutFiles: TMemo;
    Defmasks: TStrHolder;
    LabelCopyright: TLabel;
    Label2: TLabel;
    FontDialog: TFontDialog;
    doFont: TButton;
    TabSheet6: TTabSheet;
    procedure doSelectSaveClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure SuppPropsDblClick(Sender: TObject);
    procedure doVarCreateClick(Sender: TObject);
    procedure CodeMasksDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Changed(Sender: TObject);
    procedure doSaveClick(Sender: TObject);
    procedure doOpenOldClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure doOpenAnyClick(Sender: TObject);
    procedure FormCompsDblClick(Sender: TObject);
    procedure SavedPropsDblClick(Sender: TObject);
    procedure OutFilesDblClick(Sender: TObject);
    procedure FontMemosChange ;
    procedure doFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    procedure GetProperties ;
    procedure GetMasks ;
    procedure GetFiles ;
    procedure ReadSpec (filename: string) ;
    procedure WriteSpec (filename: string) ;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

  proptot: integer = 0 ;
  propnamelist: array [1..100] of string ;
  propproplist: array [1..100] of string ;
  proptypelist:  array [1..100] of string ;

  masktot: integer = 0 ;
  masktypelist: array [1..200] of string ;
  maskproplist: array [1..200] of string ;

  filestot: integer = 0 ;
  filecodelist: array [1..20] of string ;
  filesuffixlist:  array [1..20] of string ;

  changedflag: boolean ;

const
  NULL            =  #0;
  BACKSPACE       =  #8;
  TAB             =  #9;
  LF              = #10;
  FF			   = #12;
  CR              = #13;
  EOF_            = #26;   
  ESC             = #27;
  BLANK           = #32;
  SQUOTE          = #39 ;
  DQUOTE          = #34 ;
  SPACE           = BLANK;
  CRLF            : PChar = CR+LF;

RegKey = 'Software\Magenta-Systems\Delphvar' ;


implementation

{$R *.DFM}

procedure TMainForm.FontMemosChange ;
begin
    SuppProps.Font := FontDialog.Font ;
    CodeMasks.Font := FontDialog.Font ;
    OutFiles.Font := FontDialog.Font ;
    SavedProps.Font := FontDialog.Font ;
    FormComps.Font := FontDialog.Font ;
end ;

procedure TMainForm.FormCreate(Sender: TObject);
begin
// are we running?
	if NOT AppInit.IsFirstInstance then
    begin
	   	Hide ;
		MessageDlg('DelphVar is Already Running', mtError, [mbCancel], 0) ;
		Application.Terminate ;
        exit ;
	end ;
    if SuppProps.Lines.Count = 0 then SuppPropsDblClick (Sender) ;
    if CodeMasks.Lines.Count = 0 then CodeMasksDblClick (Sender) ;
    if OutFiles.Lines.Count = 0 then OutFilesDblClick (Sender) ;
    changedflag := false ;
end;

procedure TMainForm.SuppPropsDblClick(Sender: TObject);
begin
    SuppProps.Lines.Assign (DefSuppProps.Strings) ;
end;

procedure TMainForm.CodeMasksDblClick(Sender: TObject);
begin
    CodeMasks.Lines.Assign (DefMasks.Strings) ;
end;

procedure TMainForm.OutFilesDblClick(Sender: TObject);
begin
    OutFiles.Lines.Assign (DefFiles.Strings) ;
end;

procedure TMainForm.doExitClick(Sender: TObject);
begin
    Application.Terminate ;
end;


procedure TMainForm.ReadSpec (filename: string) ;
var
    IniFile: TMemIniFile ;
    I, J, X: integer ;
    section, temp, styles: string ;

    function LoadFont (CName: string): TFont ;
    begin
        with IniFile do
        begin
            Result := TFont.Create ;
            Result.Height := ReadInteger (section, CName + 'Height', 8) ;
            Result.Color := ReadInteger (section, CName + 'Color', 0) ;
            Result.Name := ReadString (section, CName + 'Name', '') ;
            Result.Pitch := TFontPitch (ReadInteger (section, CName + 'Pitch', 0)) ;
            Result.Size := ReadInteger (section, CName + 'Size', 0) ;
            styles := ReadString (section, CName + 'Styles', '') ;
            Result.Style := [] ;
            if length (styles) = 4 then
            begin
                if styles [1] = 'B' then
                               Result.Style := Result.Style + [fsBold];
                if styles [2] = 'I' then
                             Result.Style := Result.Style + [fsItalic];
                if styles [3] = 'U' then
                           Result.Style := Result.Style + [fsUnderline];
                if styles [4] = 'S' then
                           Result.Style := Result.Style + [fsStrikeout];
            end ;
        end ;
    end ;

    function LoadSList (CName: string): TStringList ;
    var
        I, J: integer ;
    begin
        with IniFile do
        begin
            Result := TStringList.Create ;
            J := ReadInteger (CName, 'Count', 0) ;
            if J <> 0 then for I := 0 to J - 1 do
	            Result.Add (ReadString (CName, 'Item' + IntToStr (I), '' )) ;
       end ;
    end ;

    begin
    section := 'Properties' ;   // section name
    IniFile := TMemIniFile.Create (filename);
    with IniFile do
    begin

// following lines were automatically created by this program....
  ArraySize.Value := ReadInteger (section, 'ArraySize_Value', 0) ;
  CodeMasks.Lines := LoadSList (section + '.CodeMasks_Lines') ;
  FilePrefix.Text := ReadString (section, 'FilePrefix_Text', '') ;
  FontDialog.Font := LoadFont ('FontDialog_Font') ;
  FormComps.Lines := LoadSList (section + '.FormComps_Lines') ;
  OutDir.Text := ReadString (section, 'OutDir_Text', '') ;
  OutFiles.Lines := LoadSList (section + '.OutFiles_Lines') ;
  SavedProps.Lines := LoadSList (section + '.SavedProps_Lines') ;
  SuppProps.Lines := LoadSList (section + '.SuppProps_Lines') ;
// end of automatic lines
    end ;
    IniFile.Free ;
end ;

procedure TMainForm.WriteSpec (filename: string) ;
var
    IniFile: TMemIniFile ;
    I, J, X: integer ;
    section, temp, styles: string ;

    procedure SaveFont (CName: string; SFont: TFont) ;
    begin
        with IniFile do
        begin
            WriteInteger (section, CName + 'Color', SFont.Color) ;
            WriteInteger (section, CName + 'Height', SFont.Height) ;
            WriteString (section, CName + 'Name', SFont.Name) ;
            WriteInteger (section, CName + 'Pitch', Ord (SFont.Pitch)) ;
            WriteInteger (section, CName + 'Size', SFont.Size) ;
            styles := 'xxxx' ;
            if fsBold in SFont.Style then styles [1] := 'B' ;
            if fsItalic in SFont.Style then styles [2] := 'I' ;
            if fsUnderline in SFont.Style then styles [3] := 'U' ;
            if fsStrikeout in SFont.Style then styles [4] := 'S' ;
            WriteString (section, CName + 'Styles', styles) ;
        end ;
    end ;

    procedure SaveSList (CName: string; SList: TStrings) ;
    var
        I: integer ;
    begin
        with IniFile do
        begin
            EraseSection (CName) ;
            if SList.Count <> 0 then
            for I := 0 to SList.Count - 1 do
                WriteString (CName, 'Item' + IntToStr (I), SList [I]) ;
            WriteInteger (CName, 'Count', SList.Count) ;
        end ;
    end ;
begin
    section := 'Properties' ;   // section name
    IniFile := TMemIniFile.Create (filename);
    with IniFile do
    begin
// following lines were automatically created by this program....
  WriteInteger (section, 'ArraySize_Value', ArraySize.Value) ;
  SaveSList (section + '.CodeMasks_Lines', CodeMasks.Lines) ;
  WriteString (section, 'FilePrefix_Text', FilePrefix.Text) ;
  SaveFont ('FontDialog_Font', FontDialog.Font) ;
  SaveSList (section + '.FormComps_Lines', FormComps.Lines) ;
  WriteString (section, 'OutDir_Text', OutDir.Text) ;
  SaveSList (section + '.OutFiles_Lines', OutFiles.Lines) ;
  SaveSList (section + '.SavedProps_Lines', SavedProps.Lines) ;
  SaveSList (section + '.SuppProps_Lines', SuppProps.Lines) ;
// end of automatic lines
       UpdateFile;   // write to disk
    end ;
    IniFile.Free ;
end ;


procedure TMainForm.GetProperties ;
var
    count, dotpos, seppos: integer ;
    tempcomp, compname, typename: string ;
begin
    proptot := 0 ;
    for count := 0 to (SuppProps.Lines.Count - 1) do
    begin
        tempcomp :=  DelSpace (Trim (SuppProps.Lines [count])) ;
        if length (tempcomp) < 3 then continue ;
        dotpos := pos ('.', tempcomp) ;
        if dotpos = 0 then continue ;  // not a statement
        seppos := pos ('-', tempcomp) ;
        if seppos = 0 then continue ;  // not a statement
        if seppos < dotpos then continue ;  // illegal
        inc (proptot) ;
        propnamelist [proptot] := lowercase
                                    (copy (tempcomp, 1, dotpos - 1)) ;
        propproplist [proptot] := copy (tempcomp, dotpos + 1,
                                                seppos - dotpos - 1) ;
        proptypelist [proptot] := copy (tempcomp,
                                                seppos + 1, 999) ;
    end ;
end ;

procedure TMainForm.GetMasks ;
var
    count, seppos: integer ;
    tempcomp: string ;
begin
    masktot := 0 ;
    for count := 0 to (CodeMasks.Lines.Count - 1) do
    begin
        tempcomp := Trim (CodeMasks.Lines [count]) ;
        if length (tempcomp) < 3 then continue ;
        seppos := pos ('-', tempcomp) ;
        if seppos = 0 then continue ;  // not a statement
        inc (masktot) ;
        masktypelist [masktot] := trim (copy (tempcomp, 1, seppos - 1)) ;
        maskproplist [masktot] := trim (copy (tempcomp, seppos + 1, 999)) ;
    end ;
end ;

procedure TMainForm.GetFiles ;
var
    count, seppos: integer ;
    tempcomp: string ;
begin
    filestot := 0 ;
    for count := 0 to (OutFiles.Lines.Count - 1) do
    begin
        tempcomp := Trim (OutFiles.Lines [count]) ;
        if length (tempcomp) < 3 then continue ;
        seppos := pos ('-', tempcomp) ;
        if seppos = 0 then continue ;  // not a statement
        inc (filestot) ;
        filecodelist [filestot] := trim (copy (tempcomp, 1, seppos - 1)) ;
        filesuffixlist [filestot] := trim (copy (tempcomp, seppos + 1, 999)) ;
    end ;
end ;

procedure TMainForm.doSelectSaveClick(Sender: TObject);
var
    count, count2: integer ;
    tempcomp, tempprop, compname, varname, propname: string ;
begin
    GetProperties ;
    if proptot = 0 then exit ;

// process form component type info into saved properties format
    FormCompsDblClick (Sender) ; // clear up lines
    DualListDialog.List1.Clear ;
    for count := 0 to (FormComps.Lines.Count - 1) do
    begin
        tempcomp := Trim (FormComps.Lines [count]) ;
        tempprop := '// ' + tempcomp ;
        tempcomp := DelSpace (tempcomp) ;  // remove spaces
        if length (tempcomp) <= 3 then continue ;
        if tempcomp [1] = SQUOTE then
        begin
            if tempcomp [Length (tempcomp)] <> SQUOTE then continue ;
            tempcomp := DelChars (tempcomp, SQUOTE) ;
            compname := tempcomp ;
            varname := trim (Copy2SymbDel (tempcomp, '.')) ;
            if tempcomp = '' then continue ;  // nothing left
            propname := copy (tempcomp, 2, 999) ;
            for count2 := 1 to proptot do
            begin
                if propname = propproplist [count2] then
                begin
                    tempprop := compname +
                                     ' - ' + proptypelist [count2] ;
                    DualListDialog.List1.Add (tempprop) ;
                    propname := '' ;   // only need property once
                    tempprop := '' ;
                end ;
            end ;
        end
        else
        begin
            if tempcomp [length (tempcomp)] <> ';' then continue ; // ditto
            delete (tempcomp, length (tempcomp), 1);  // remove it
            varname := Copy2SymbDel (tempcomp, ':') ;
            if tempcomp = '' then continue ;
            compname := lowercase (copy (tempcomp, 2, 999)) ;
            for count2 := 1 to proptot do
            begin
                if compname = propnamelist [count2] then
                begin
                    tempprop := varname + '.' + propproplist [count2] +
                                     ' - ' + proptypelist [count2] ;
                    DualListDialog.List1.Add (tempprop) ;
                    tempprop := '' ;
                end ;
            end ;
        end ;
        if tempprop <> '' then DualListDialog.List1.Add (tempprop) ;
    end ;

// dual dialog
   DualListDialog.List2.Assign (SavedProps.Lines) ;
   if DualListDialog.Execute then
               SavedProps.Lines.Assign (DualListDialog.List2) ;
   SavedPropsDblClick (Sender) ;
end;


procedure TMainForm.doVarCreateClick(Sender: TObject);
var
    filenr, filecount, count, count2: integer ;
    tempcomp, compname, proptype, varname, outtype: string ;
    codeline, outfile, litname: string ;
    fileblock : TextFile ;
    fileflag: boolean ;
begin
    doVarCreate.Enabled := false ;
    Status.SimpleText := '' ;
    fileflag := false ;
    try
    GetMasks ;
    GetFiles ;
    if FilesTot = 0 then exit ;
    filecount := 0 ;
    for filenr := 1 to FilesTot do
    begin
        outfile := OutDir.Text + '\' + FilePrefix.Text +
                                   FileSuffixList [filenr] + '.inc' ;
        AssignFile (fileblock, outfile) ;
        Rewrite (fileblock) ;
        fileflag := true ;

        for count := 0 to (SavedProps.Lines.Count - 1) do
        begin
            tempcomp := DelSpace (Trim (SavedProps.Lines [count])) ;
            if length (tempcomp) < 4 then continue ;
            compname := Copy2SymbDel (tempcomp, '-') ;
            if tempcomp = '' then exit ;   // nothing left to process
            varname := ReplaceStr (compname, '.', '_') ;
            litname := varname ;
            proptype := copy (tempcomp, 2, 999) ;
            outtype := FileCodeList [filenr] + proptype ;  // case sensitive
            if ArraySize.Value <> 0 then
            begin
                if outtype [1] = 'D' then
                    proptype := 'array [0..' + InttoStr
                              (ArraySize.Value) + '] of ' + proptype 
                else
                    varname := varname + ' [X]' ;
            end ;

        // now process masks
            for count2 := 1 to masktot do
            begin
                if outtype = masktypelist [count2] then
                begin
                    codeline := space + space + maskproplist [count2] ;
                    codeline := ReplaceStr (codeline, '$comp', compname) ;
                    codeline := ReplaceStr (codeline, '$type', proptype) ;
                    codeline := ReplaceStr (codeline, '$var', varname) ;
                    codeline := ReplaceStr (codeline, '$lit', litname) ;
                    codeline := ReplaceStr (codeline, '$line', CRLF) ;
                    codeline := ReplaceStr (codeline, '$tab',
                                                           CRLF + tab) ;
                    Writeln (fileblock, codeline) ;
                    continue ;
                end ;
            end ;
        end ;
        CloseFile (fileblock) ;
        fileflag := false ;
        inc (filecount) ;
    end ;
    finally
       if fileflag then CloseFile (fileblock) ;
       doVarCreate.Enabled := true ;
       beep ;
       Status.SimpleText := IntToStr (filecount) +  ' Files Written' ;
    end ;
end;

procedure TMainForm.Changed(Sender: TObject);
begin
    changedflag := true ;
end;

procedure TMainForm.doSaveClick(Sender: TObject);
var
    filename: string ;
begin
    filename := OutDir.Text + '\' + FilePrefix.Text + '-props.sdt' ;
    WriteSpec (filename) ;
    if OldSpecs.Items.IndexOf (filename) < 0 then
                            OldSpecs.Items.Add (filename) ;
    OldSpecs.Text := filename ;
    beep ;
    Status.SimpleText := 'Details Saved in ' + filename ;
end;

procedure TMainForm.doOpenOldClick(Sender: TObject);
begin
    Status.SimpleText := '' ;
   if OldSpecs.Text = '' then
   begin
       Status.SimpleText := 'Must Select Saved Details File' ;
       beep ;
       exit ;
   end ;
   if NOT FileExists (OldSpecs.Text) then
   begin
       Status.SimpleText := 'File Not Found' ;
       beep ;
       exit ;
   end ;
   ReadSpec (OldSpecs.Text) ;
   FontMemosChange ;
   beep ;
end;

procedure TMainForm.doClearClick(Sender: TObject);
begin
    FormComps.Lines.Clear ;
    SavedProps.Lines.Clear ;
    ArraySize.Value := 0 ;
    OutDir.Text := '' ;
    FilePrefix.Text := 'Unit' ;
    SuppPropsDblClick (Sender) ;
    CodeMasksDblClick (Sender) ;
    OutFilesDblClick (Sender) ;
    changedflag := false ;
end;

procedure TMainForm.doOpenAnyClick(Sender: TObject);
begin
    if OpenDialog.Execute then
    begin
       ReadSpec (OpenDialog.FileName) ;
       FontMemosChange ;
       beep ;
   end ;
end;

procedure TMainForm.FormCompsDblClick(Sender: TObject);
var
    count: integer ;
begin
   if FormComps.Lines.Count = 0 then exit ;
   for count := 0 to (FormComps.Lines.Count - 1) do
        FormComps.Lines [count] := Trim (FormComps.Lines [count]) ;
 end;

procedure TMainForm.SavedPropsDblClick(Sender: TObject);
var
    count: integer ;
begin
   if SavedProps.Lines.Count = 0 then exit ;
   for count := 0 to (SavedProps.Lines.Count - 1) do
       SavedProps.Lines [count] := Trim (SavedProps.Lines [count]) ;
   while (SavedProps.Lines.Count <> 0) and
                    (SavedProps.Lines [0] = '') do
                                      SavedProps.Lines.Delete (0) ;

end;

procedure TMainForm.doFontClick(Sender: TObject);
begin
    if FontDialog.Execute then FontMemosChange ;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
// handle bug with disappearing component in Delphi 4
//  PageControl1Change (Sender) ;   - stopped using tab2 instead
    FormStorage.RestoreFormPlacement ;
    FontMemosChange ;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin

// stupid code to workaround a Delphi4 bug that cause a client
// aligned component of TabSheet2 to disappear
{   if (SavedProps.Height = 0) or (SavedProps.Width = 0) then
    begin
        SavedProps.Align := alNone ;
        SavedProps.Align := alClient ;
    end ;  }
end;

end.
   