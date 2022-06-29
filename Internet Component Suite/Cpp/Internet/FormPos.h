//---------------------------------------------------------------------------
#ifndef FormPosH
#define FormPosH
//---------------------------------------------------------------------------
void __fastcall LoadFormPos(TForm *Form,
                            const AnsiString IniFilename,
                            const AnsiString SectionName,
                            const AnsiString KeyName);
void __fastcall SaveFormPos(TForm *Form,
                            const AnsiString IniFilename,
                            const AnsiString SectionName,
                            const AnsiString KeyName);
//---------------------------------------------------------------------------
#endif
