Delphi Registry and Initialisation Variables Code Generator - Delphvar
Version 1.4  - 2nd September 2010
======================================================================

Delphvar is a Pascal code generator for the Borland Delphi programming language,
designed to assist in the saving and restoring of component properties to and from
both the registry and INI files.  Given a list of component types copied from the
Interface section of form, Delphvar allows the specification of which component
properties to process, and will then automatically create Pascal insert files
containing the statements needed to read and write these properties. It will also
write files to declare variables of similar name to the component properties and
read and write from variables as well, and clear both component properties and
variables.  Delphvar is a standalone Delphi program, supplied both as source and
executable, so it totally independent of different versions of Delphi.  It has
been tested on Delphi 2007, but not 2009 or later.


Background
----------

Nearly all applications need to save program settings of some kind or another,
typically entered using visual components.  Delphi provides classes to assist in
writing INI files or the registry using various methods, but writing the Pascal
code to save component properties can be tedious and repetitive.

An improvement on manually coding each line is to maintain a list of the
properties to be saved, and have code that runs through the list separately
processing each property.  This however starts to get very complicated when
needing to save the properties from third party components or offer choice of
registry and INI interfaces.

The 'ultimate' solution is to make use of Delphi run time tables to save any
possible properties, and this is how the RX Library TFormStorage component works
You just drop it on a form, double click and select the properties to be saved,
and everything is handled automatically.   Unfortunately, there's a lot of code
behind TFormStorage which adds to the executable size, the performance is poor on
slow PCs and most importantly it will only store the properties of visible
components.  So in the typical program with Main and Preferences forms, the latter
must be created at program initialisation time, occupying memory, for the
component properties to be available to the Main form.  There also complications
if multiple records are to be stored, although changing TFormStorage properties
does make this possible.

Delphvar creates pure Pascal code and does not require any additional components
or code, other than the Delphi classes and two small functions to avoid duplicate
code processing TStringList and TFont.  Having code is also more versatile since
it can be customised if necessary and the executable will almost certainly be
smaller than adding TFormStorage and the program will be faster.  Delphvar user
registry/INI names with identical format to TFormStorage, and saves most
properties identically
so that an existing application can be updated with Delphvar code.  The only non-
compatiblity is typed properties where TFormStorage saves the ASCII string but
Delphvar saves an integer (to avoid looking up the name), with the exception of
boolean types which are stored literally as true or false.  But the code produced
by Delphvar is fully customisable on a project by project basis, and so could be
easily changed for full compatibility.


How Delphvar Works
------------------

Delphvar has a text window into which the raw list of components may be copied. It
accepts input data in two forms:

1 - copied from the Interface section of a Pascal form, ie:

  AFileName: TFilenameEdit;
  AEditBox: TEdit;
  ASpinEdit: TSpinEdit;
  ACheckBox: TCheckBox ;
  AMemo: TMemo ;

2 - copied from the RX FormStorage TStringList, from an application 'view form as
text':
  'AEditBox.Text'
  'ACheckbox.Checked'

These two formats are parsed, and any lines not in one format or the other are
ignored.  During parsing, the property type is identified and present as follows:

  AFileName.FileName - String
  AEditBox.Text - String
  ASpinEdit.Value - Integer
  ACheckBox.Checked - Boolean
  AMemo.Lines - TStringList

All the form properties are displayed in a dual list dialog box, allowing those to
be saved to be selected.  Once selected, pressing a button will write up to seven
Pascal source insert files, as follows:

File 1 - Read Component Properties

  AEditBox.Text := ReadString (section, 'AEditBox_Text', '') ;
  ASpinEdit.Value := ReadInteger (section, 'ASpinEdit_Value', 0) ;

File 2 - Write Component Properties

  WriteString (section, 'AEditBox_Text', AEditBox_Text) ;
  WriteInteger (section, 'ASpinEdit.Value', ASpinEdit_Value) ;

File 3 - Clear Component Properties

  AEditBox.Text :=  '' ;
  ASpinEdit.Value :=  0 ;

File 4 - Variable Declarations

  AEditBox_Text: String ;
  ASpinEdit_Value: Integer ;

File 5 - Read Variables

  AEditBox_Text := ReadString (section, 'AEditBox_Text', '') ;
  ASpinEdit_Value := ReadInteger (section, 'ASpinEdit_Value', 0) ;

File 6 - Write Variables

  WriteString (section, 'AEditBox_Text', AEditBox_Text) ;
  WriteInteger (section, 'ASpinEdit_Value', ASpinEdit_Value) ;

File 7 - Clear Variables

  AEditBox_Text :=  '' ;
  ASpinEdit_Value :=  0 ;

When using variables, it is also possible to use arrays to handle multiple
records, with the code being generated adjusted accordingly:

  AEditBox_Text: Array [1..99] of String ;
  AEditBox_Text [X] := ReadString (section, 'AEditBox_Text', '') ;

As mentioned earlier, the ability to read and write the same registry/INI settings
into either components or variables gives flexibility in creating forms as
required. Being able to clear all the components or variables may also be useful
to revert to defaults or create a new record.


Using the Code in a Delphi Application
--------------------------------------

The seven files created with the file extension INC so the may be included as: {$I
FILE.INC} or alternatively the code may be copied into the application as
illustrated in the source for Delphvar, VARSMAIN.PAS.  Some variables and possibly
functions are required to support the generated source code.  The two basic
procedures to save and restore values are as follows, with the actual code copied
into the procedure :

procedure TMainForm.ReadInfo (filename: string) ;
var
    IniFile: TIniFile ;
    X: integer ;
    section, temp, styles: string ;

    function LoadFont (CName: string): TFont ;
    begin
        with IniFile do
        begin
            Result := TFont.Create ;
            Result.Height := ReadInteger (section, CName + 'Height', 8) ;
            Result.Color := ReadInteger (section, CName + 'Color', 0) ;
            Result.Name := ReadString (section, CName + 'Name', '') ;
            Result.Pitch := TFontPitch (ReadInteger (section, CName + 'Pitch',
            0));
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
    IniFile := TIniFile.Create (filename);
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


procedure TMainForm.WriteInfo (filename: string) ;
var
    IniFile: TIniFile ;
    X: integer ;
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
    IniFile := TIniFile.Create (filename);
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
    end ;
    IniFile.Free ;
end ;

If the application does not need to handle TStringList (Items or Lines) and/or
TFont, the appropriate functions may be removed.  Likewise, rather than TIniFile,
you could instead use TRegIniFile or TMemIniFile (D4 and later only).


Customising Delphvar
--------------------

The files and code that are generated are based on three lists of information, all
of which can be customised on a per project basis: supported properties, output
files/masks, and code statement masks.

1 - Supported Properties

The format of this list is the component name, a period, it's property, a hyphen,
then the property type.  The default list contains properties for all the standard
Delphi components, and some from third party libraries, as follows:

TBCGFilterDialog.Brightness - Integer
TBCGFilterDialog.Contrast - Double
TBCGFilterDialog.Gamma - Double
TCheckBox.Checked - Boolean
TCheckBox.State - TCheckBoxState
TColorComboBox.ColorValue - Integer
TColorComboBox.ItemIndex - Integer
TColorDialog.Color - Integer
TColorDialog.CustomColors - TStringList
TComboBox.ItemIndex - Integer
TComboBox.Text - String
TDirectoryEdit.Text - String
TDirectoryListBox.Directory - String
TDriveComboBox.Drive - String
TEdit.Text - String
TFilenameEdit.Text - String
TFontComboBox.Font - TFont
TFontDialog.Font - TFont
TLabel.Caption - String
TLabel.Font - TFont
TListBox.ItemIndex - Integer
TListBox.Items - TStringList
TMaskedEdit.Text - String
TMemo.Lines - TStringList
TMenuItem.Checked - Boolean
TOpenDialog.Filename - String
TOpenDialog.InitialDir - String
TRadioButton.Checked - Boolean
TRadioGroup.ItemIndex - Integer
TSaveDialog.Filename - String
TSaveDialog.InitialDir - String
TSpinEdit.Value - Integer

2 - Output Files/Masks

There is one line here per file that will be created, with the first two letters
determining which code statement masks will be used, followed by a hyphen and the
partial file name for the created file, which will be prefixed with something for
each project and suffixed with .INC.

DV - defvars
RV - readvars
WV - writevars
CV - clearvars
RC - readcomp
WC - writecomp
CC - clearcomp


3 - Code Statement Masks

These lines determine the actual Pascal source code that will be written for each
different file and property.  There are several masks that are replaced: $comp is
component name, $var variable name, $type component type, $lit is registry/INI
literal name, $line a new line, $tab a new line followed by tab.  Most properties
are handled with a single line of code (maybe two or more statements), but as
mentioned before TStringList and TFont instead call functions to save space.

CCBoolean - $comp := false ;
CCDouble - $comp := 0 ;
CCInteger - $comp := 0 ;
CCString - $comp := '' ;
CCTCheckBoxState - $comp := cbUnchecked ;
CCTFont - $comp := LoadFont ('x') ;
CCTStringList - $comp.Clear ;
CVBoolean - $var := false ;
CVDouble - $var := 0 ;
CVInteger - $var := 0 ;
CVString - $var := '' ;
CVTCheckBoxState - $var := cbUnchecked ;
CVTFont - $var := LoadFont ('x') ;
CVTStringList - $var.Clear ;
DVBoolean - $var: $type ;
DVDouble - $var: $type ;
DVInteger - $var: $type ;
DVString - $var: $type ;
DVTFont - $var: $type ;
DVTStringList - $var: $type ;
RCBoolean - if ReadString (section, '$lit', 'False') = 'True' then $comp := true else $comp := false ;
RCDouble - $comp := ReadFloat (section, '$lit', 0) ;
RCInteger - $comp := ReadInteger (section, '$lit', 0) ;
RCString - $comp := ReadString (section, '$lit', '') ;
RCTCheckBoxState - $comp := TCheckBoxState (ReadInteger (section, '$lit', Ord(cbUnchecked))) ;
RCTFont - $comp := LoadFont ('$lit') ;
RCTStringList - $comp := LoadSList (section + '.$lit') ;
RVBoolean - if ReadString (section, '$lit', 'False') = 'True' then $var := true else $var := false ;
RVDouble - $var := ReadFloat (section, '$lit', 0) ;
RVInteger - $var := ReadInteger (section, '$lit', 0) ;
RVString - $var := ReadString (section, '$lit', '') ;
RVTCheckBoxState - $var := TCheckBoxState (ReadInteger (section, '$lit', Ord (cbUnchecked))) ;
RVTFont - $var := LoadFont ('$lit') ;
RVTStringList - $var := LoadSList (section + '.$lit');
WCBoolean - if $comp then temp := 'True' else temp := 'False' ; WriteString (section, '$lit', temp) ;
WCDouble - WriteFloat (section, '$lit', $comp) ;
WCInteger - WriteInteger (section, '$lit', $comp) ;
WCString - WriteString (section, '$lit', $comp) ;
WCTCheckBoxState - WriteInteger (section, '$lit', Ord ($comp)) ;
WCTFont - SaveFont ('$lit', $comp) ;
WCTStringList - SaveSList (section + '.$lit', $comp) ;
WVBoolean - if $var then temp := 'True' else temp := 'False' ; WriteString (section, '$lit', temp) ;
WVDouble - WriteFloat (section, '$lit', $var) ;
WVInteger - WriteInteger (section, '$lit', $var) ;
WVString - WriteString (section, '$lit', $var) ;
WVTCheckBoxState - WriteInteger (section, '$lit', Ord ($var)) ;
WVTFont - SaveFont ('$lit', $var) ;
WVTStringList - SaveSList (section + '.$lit', $var) ;

The default masks also include some prefixed with X, as alternative means of
saving Boolean or TStringList properties, but these are ignored unless the X is
removed and the primary mask removed.


Installation
------------

Delphvar is a 32-bit application for Windows 9.x/NT.  It is distributed as a ZIP
file which should first be unzipped into a appropriate folder.  The program source
code will only compile under Delphi 4 and later also requires that RX Tools are
available on the component palette.  Since the executable is provided, it should
not be necessary to recompile the application.


Distribution
------------

Delphvar is freeware, but is still copyrighted by Magenta Systems Ltd who may
change the status or withdraw it at any time, without notice.  Any source code
created is your own.

Delphvar may be freely distributed via web pages, FTP sites, BBS and
conferencing systems or on CD-ROM in unaltered zip format, but no charge may be
made other than reasonable media or bandwidth cost. Please email Magenta Systems
Ltd if you distribute Delphvar in some way, so you can be notified of upgrades or
other important changes.


Copyright Information
---------------------

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




