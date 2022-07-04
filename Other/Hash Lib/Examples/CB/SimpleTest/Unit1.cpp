//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "CryptoAPI.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{

}
//---------------------------------------------------------------------------


void __fastcall TForm1::Button1Click(TObject *Sender)
{

  Char* buf;
  DWord ret;
  THashContext ctx;
  AnsiString S;

  buf = "abc";
  ret = HashInit(&ctx, HASH_MD5);              //Initialize
  if (ret == HASH_NOERROR)
    ret = HashUpdate(&ctx, buf, StrLen(buf));  //Update
  if (ret == HASH_NOERROR)
    ret = HashFinal(&ctx, S);                  //Final
  if (ret == HASH_NOERROR)
    Edit1->Text = S;  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit1->Text = HashErrorToStr(ret);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  DWord ret;
  AnsiString S;
  ret = HashStr(HASH_MD5, "abc", S);
  if (ret == NO_ERROR)
    Edit2->Text = S;  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit2->Text = HashErrorToStr(ret);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  DWord ret;
  AnsiString S;

  ret = HashFilePartial(HASH_MD5, "file.txt", 5, 7, S); //file.txt contains 'abc' substr from offset at 5-th byte
  if (ret == NO_ERROR)
    Edit3->Text = S;  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit3->Text = HashErrorToStr(ret);
}
//---------------------------------------------------------------------------

