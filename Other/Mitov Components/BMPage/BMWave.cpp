//---------------------------------------------------------------------------
#include <vcl.h>
#include <mmsystem.h>
#pragma hdrstop

#include "BMWave.h"

#pragma package(smart_init)


//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TBMWave *)
{
    new TBMWave(NULL);
}
//---------------------------------------------------------------------------
namespace Bmwave
{

__fastcall TBMCustomSound::TBMCustomSound(TComponent* Owner)
    : inherited (Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomSound::Play (void)
{
}
//---------------------------------------------------------------------------
bool __fastcall TBMCustomSound::Empty (void)
{
  return true;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMWave::TBMWave(TComponent* Owner)
    : inherited (Owner)
{
  FWave = new TBMWaveData;
}
//---------------------------------------------------------------------------
__fastcall TBMWave::~TBMWave()
{
  delete FWave;
}
//---------------------------------------------------------------------------
void __fastcall TBMWave::Play (void)
{
  FWave->Play();
}
//---------------------------------------------------------------------------
bool __fastcall TBMWave::Empty (void)
{
  return FWave->Empty();
}
//---------------------------------------------------------------------------
void __fastcall TBMWave::SetWave ( TBMWaveData* Value )
{
  FWave->Assign ( Value );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMWaveData::TBMWaveData(void) :
  Classes::TPersistent(),
  FSize ( 0 ),
  FStoredInExe ( true )
{
}
//---------------------------------------------------------------------------
__fastcall TBMWaveData::~TBMWaveData()
{
  Clear();
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::Clear()
{
  if ( ! Empty() )
    {
    delete [] FData;
    }

  FSize = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::Fill( long ASize, Byte *AData )
{
  Clear();
  if ( FStoredInExe )
    if ( ASize > 0 )
      {
      FSize = ASize;
      FData = new Byte [ FSize ];
      memcpy ( FData, AData, FSize );
      }
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::Assign( TPersistent *Source )
{
  if ( ! Source )
    Clear();

  else
    {
    TBMWaveData *WaveData = dynamic_cast<TBMWaveData *> ( Source );
    if ( WaveData )
      {
      FileName = WaveData->FileName;
      FStoredInExe = WaveData->FStoredInExe;
      Fill ( WaveData->FSize, WaveData->FData );
      }

    else
      inherited::Assign(Source);
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::LoadFromFile( String Value )
{
  int iFileHandle;
  long  iFileLength;

  Clear();
  if ( FileExists(Value) )
    {
    FileName = Value;
    iFileHandle = FileOpen( Value, fmOpenRead );
    iFileLength = FileSeek(iFileHandle,0,2);
    if ( iFileLength > 0 )
      {
      FSize = iFileLength;
      FData = new Byte [ FSize ];
      FileSeek(iFileHandle,0,0);
      FileRead( iFileHandle, FData, FSize );
      }

   FileClose(iFileHandle);
   }

}
//---------------------------------------------------------------------------
bool __fastcall TBMWaveData::Empty()
{
  if ( FStoredInExe )
    return ( FSize == 0 );
    
  else
    return FileName.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::Play()
{
  if ( ! Empty() )
    {
    if ( FStoredInExe )
      PlaySound((TCHAR *)FData, Application->Handle, SND_ASYNC | SND_MEMORY | SND_NODEFAULT );

    else
      PlaySound( FileName.c_str(), Application->Handle, SND_ASYNC | SND_FILENAME | SND_NODEFAULT );
    }
}
//---------------------------------------------------------------------------
bool __fastcall TBMWaveData::DoWrite()
{
  return ! Empty();
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::WriteData( Classes::TStream *Stream )
{
  Stream->Write( &FStoredInExe, sizeof ( FStoredInExe ));
  int StrSize = FileName.Length();
  Stream->Write( &StrSize, sizeof ( StrSize ));
  if ( StrSize )
    Stream->Write( FileName.c_str(), StrSize);
     
  Stream->Write( &FSize, sizeof(FSize));
  Stream->Write( FData,FSize );
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::ReadData( Classes::TStream *Stream )
{
  Clear();
  Stream->Read( &FStoredInExe, sizeof ( FStoredInExe ));

  int StrSize;
  Stream->Read( &StrSize,sizeof(StrSize));
  if ( StrSize )
    {
    char *Buffer = new char [ StrSize + 1];
    memset ( Buffer, '\0', StrSize + 1 ); 
    Stream->Read( Buffer, StrSize);
    FileName = Buffer;
//---> Begin ED
//    delete Buffer;
    delete[]Buffer;
// End ED
    }

  Stream->Read( &FSize,sizeof(FSize));
  if ( FSize>0 )
    {
    FData = new Byte [ FSize ];
    Stream->Read( FData, FSize );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveData::DefineProperties(Classes::TFiler* Filer )
{
  inherited::DefineProperties(Filer);
  Filer->DefineBinaryProperty( "Data", ReadData, WriteData, DoWrite() );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
      TComponentClass classes[1] = {__classid(TBMWave)};
      RegisterComponents("BMitov", classes, 0);
    }
}
//---------------------------------------------------------------------------
 