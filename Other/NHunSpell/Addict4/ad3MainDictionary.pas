(*************************************************************

Main dictionary class for reading Hunspell dictionaries with Addict v4

This unit is a replacement for "ad3MainDictionary.pas" which is part
of the commercial spell checker Addict (www.addictive-software.com).
It declares a TMainDictionary class for Addict with the same structure
and properties, but uses Hunspell dictionaries instead.

This unit:
  Wrapper for NHunspell for Delphi 2009+
  Version 1.1.0 (c) 2011 Alexander Halser, EC Software GmbH

Original unit:
  Addict 4.2,  (c) 1996-2010, Addictive Software
  Contact: support@addictivesoftware.com

**************************************************************)

unit ad3MainDictionary;

{$IFNDEF UNICODE} Warning: Using Addict with Hunspell requires the Unicode VCL! {$ENDIF}

interface

uses
    classes,
    Windows,
    SysUtils,
    Dialogs,
    ad3Util,
    ad3PhoneticsMap,
    NHunspell;


type
    TMainDictionary = class(TObject)
    private
        FSpellDictionary: TNHSpellDictionary;
        function GetTitle: String;
        function GetLanguage: DWORD;
        function GetFilename: String;
        function GetLoaded: Boolean;
    protected
        FConfigurationName: String;
        FWordsFoundCount: LongInt;
    protected
        procedure WriteFilename(Filename: String);
        procedure WriteLoaded(Loaded: Boolean);
    public
        constructor Create;
        destructor Destroy; override;

        function WordExists(const Word: String):Boolean;
        function WordLength(const Word: String):LongInt;

        procedure Suggest(Word: String; Phonetics: TPhoneticsMap; Suggestions: TStrings);
        procedure WordFound;

        property ConfigurationName: String read FConfigurationName write FConfigurationName;
        property Filename: String read GetFilename write WriteFilename;
        property Loaded: Boolean read GetLoaded write WriteLoaded;
        property Title: String read GetTitle;
        property Language: DWORD read GetLanguage;
        property WordsFoundCount: LongInt read FWordsFoundCount;
    end;

implementation


//************************************************************
// Main Dictionary Class
//************************************************************

constructor TMainDictionary.Create;
begin
    inherited Create;
    FWordsFoundCount    := 0;
end;

//************************************************************

destructor TMainDictionary.Destroy;
begin
    WriteLoaded(False);
    inherited destroy;
end;

function TMainDictionary.GetTitle: String;
begin
    if assigned(FSpellDictionary) then
      result := Format('%s %s', [FSpellDictionary.LanguageName, FSpellDictionary.DisplayName]);
end;

function TMainDictionary.GetLanguage: DWORD;
begin
    if assigned(FSpellDictionary) then
      result := PRIMARYLANGID(FSpellDictionary.LangID);  //For Addict, return primary lang ID only, such as "French" ($000C)
end;

//************************************************************
// Property Read/Write Functions
//************************************************************

function TMainDictionary.GetFilename: String;
begin
    if assigned(FSpellDictionary) then
      result := FSpellDictionary.GetInternalFileID
    else
      result := '';
end;

procedure TMainDictionary.WriteFilename(Filename: String);
var
  i: integer;
begin
    FWordsFoundCount := 0;

    { Filename: what we need here is not a real file name, but the internal ID of the dictionary!
      This internal ID must have been assigned to TAddictSpell.Configuration.MainDictionaries (a simple stringlist).

      Example for an internal ID: "dict-en.oxt::en_US.aff::en_US.dic" - this references the US English
                                  dictionary "en_US.aff" + "en_US.dic" in the container file "dict-en.oxt".

      However, when creating the main dictionary, TAddictSpell makes a filename with a fully qualied path out of
      that. That path comes from TAddictSpell.ConfigDictionaryDir. For Hunspell, we do NOT need the path here,
      as our list of available dictionaries has already been created (in TNHunspell.ReadFolder and TNHunspell.ReadOXT)
      and we refer to them with the internal ID only. So we have to get rid of the preceeding path. }

    i := pos('::', Filename);
    if i > 0 then
      Filename := ExtractFileName(copy(Filename,1,i-1)) + copy(Filename,i,length(Filename)-i+1);

    FSpellDictionary := Hunspell.FindSpellDictionary(FileName);  //Filename = "english.oxt::en_en.aff::en_en.dic"
end;

//************************************************************
function TMainDictionary.GetLoaded: Boolean;
begin
    result := assigned(FSpellDictionary) and FSpellDictionary.Active; //not "FSpellDictionary.Loaded"
end;

procedure TMainDictionary.WriteLoaded(Loaded: Boolean);
begin
    if not assigned(FSpellDictionary) then
      exit;

    { We just set the TNHSpellDictionary.Active property here and, in case
      it needs to be loaded, we load it. But we do NOT UNLOAD it when loaded
      is false, because it costs us precious time. Addict loads and unloads
      its main dictionaries pretty often and since Addict dictionaries are just
      memory-mapped files, this is super-fast for Addict.

      But with Hunspell dictionaries, we really unzip the OXT container, extract
      the data and initialize Hunspell every time a load command comes along.

      So when you make changes to the Addict configuration at runtime, assign the
      TAddictSpell.Configuration.MainDictionaries property, then make call
      "Hunspell.UpdateAndLoadDictionaries" to update the dictionaries currently used. }

    FSpellDictionary.Active := Loaded;

    if Loaded and not FSpellDictionary.Loaded then
      FSpellDictionary.Load;
end;

function TMainDictionary.WordExists(const Word: String):Boolean;
begin
    if assigned(FSpellDictionary) then
    begin
      if not Loaded then
        WriteLoaded(True);
      result := FSpellDictionary.spell(Word);
    end
    else
      result := false;
end;

function TMainDictionary.WordLength(const Word: String): LongInt;
var
  AWordList: TUnicodeStrings;
begin
    result := 0;
    if assigned(FSpellDictionary) and WordExists(Word) then
    begin
      AWordList := TUnicodeStrings.create;
      try
        FSpellDictionary.Stem(Word, AWordList);
  //    result := length of word  - not implemented
      finally
        AWordList.free;
      end;
    end;
end;

procedure TMainDictionary.Suggest(Word: String; Phonetics: TPhoneticsMap; Suggestions: TStrings);
begin
    // TPhoneticsMap is ignored, we don't have that in Hunspell
    if assigned(FSpellDictionary) then
      FSpellDictionary.Suggest(Word, Suggestions);
end;

//************************************************************

procedure TMainDictionary.WordFound;
begin
    inc( FWordsFoundCount );
end;



end.

