// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsCP.pas' rev: 6.00

#ifndef DsCPHPP
#define DsCPHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dscp
{
//-- type declarations -------------------------------------------------------
typedef char Char256[256];

typedef char *pChar256;

#pragma option push -b-
enum CP_TYPE { Access_General, Access_Greece, Access_Japanese, Access_NordDanish, Access_SwedFinnish, ascii_ANSI, Borland_ANSI_Arabic, Borland_DAN_Latin1, Borland_DEU_Latin1, Borland_ENG_Latin1, Borland_ENU_Latin1, Borland_ESP_Latin1, Borland_FIN_Latin1, Borland_FRA_Latin1, Borland_FRC_Latin1, Borland_ISL_Latin1, Borland_ITA_Latin1, Borland_NLD_Latin1, Borland_NOR_Latin1, Borland_PTG_Latin1, Borland_SVE_Latin1, DB2_SQL_ANSI_DEU, dBASE_BUL_868, dBASE_CHS_cp936, dBASE_CHT_cp950, dBASE_CSY_cp852, dBASE_CSY_cp867, dBASE_DAN_cp865, dBASE_DEU_cp437, dBASE_DEU_cp850, dBASE_ELL_GR437, dBASE_ENG_cp437, dBASE_ENG_cp850, dBASE_ENU_cp437, dBASE_ENU_cp850, dBASE_ESP_cp437, dBASE_ESP_cp850, dBASE_FIN_cp437, dBASE_FRA_cp437, dBASE_FRA_cp850, dBASE_FRC_cp850, dBASE_FRC_cp863, dBASE_HUN_cp852, dBASE_ITA_cp437, dBASE_ITA_cp850, dBASE_JPN_cp932, dBASE_JPN_Dic932, dBASE_KOR_cp949, dBASE_NLD_cp437, dBASE_NLD_cp850, dBASE_NOR_cp865, dBASE_PLK_cp852, dBASE_PTB_cp850, dBASE_PTG_cp860, dBASE_RUS_cp866, dBASE_SLO_cp852
	, dBASE_SVE_cp437, dBASE_SVE_cp850, dBASE_THA_cp874, dBASE_TRK_cp857, FoxPro_Czech_1250, FoxPro_Czech_DOS895, FoxPro_German_1252, FoxPro_German_437, FoxPro_Nordic_1252, FoxPro_Nordic_437, FoxPro_Nordic_850, Hebrew_dBASE, MSSQL_ANSI_Greek, Oracle_SQL_WE850, Paradox_ANSI_HEBREW, Paradox_ascii, Paradox_BUL_868, Paradox_China_936, Paradox_Cyrr_866, Paradox_Czech_852, Paradox_Czech_867, Paradox_ESP_437, Paradox_Greek_GR437, Paradox_hebrew, Paradox_Hun_852_DC, Paradox_intl_850, Paradox_intl, Paradox_ISL_861, Paradox_japan, Paradox_Korea_949, Paradox_nordan, Paradox_nordan40, Paradox_Polish_852, Paradox_Slovene_852, Paradox_swedfin, Paradox_Taiwan_950, Paradox_Thai_874, Paradox_turk, Pdox_ANSI_Bulgaria, Pdox_ANSI_Cyrillic, Pdox_ANSI_Czech, Pdox_ANSI_Greek, Pdox_ANSI_Hun_DC, Pdox_ANSI_Intl, Pdox_ANSI_Intl850, Pdox_ANSI_Nordan4, Pdox_ANSI_Polish, Pdox_ANSI_Slovene, Pdox_ANSI_Spanish, Pdox_ANSI_Swedfin, Pdox_ANSI_Turkish, Pdox_ascii_Japan, pdx_ANSI_Czech_CH, pdx_ANSI_ISO_L_2_CZ, pdx_Czech_852_CH, pdx_Czech_867_CH, pdx_ISO_L_2_Czech
	, Spanish_ANSI, SQL_Link_ROMAN8, Sybase_SQL_Dic437, Sybase_SQL_Dic850, WEurope_ANSI };
#pragma option pop

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char CP_AN_ACCGEN[256];
extern PACKAGE char CP_NA_ACCGEN[256];
extern PACKAGE char CP_AN_ACCGREEK[256];
extern PACKAGE char CP_NA_ACCGREEK[256];
extern PACKAGE char CP_AN_ACCJAPAN[256];
extern PACKAGE char CP_NA_ACCJAPAN[256];
extern PACKAGE char CP_AN_ACCNRDAN[256];
extern PACKAGE char CP_NA_ACCNRDAN[256];
extern PACKAGE char CP_AN_ACCSWFIN[256];
extern PACKAGE char CP_NA_ACCSWFIN[256];
extern PACKAGE char CP_DBWINUS0[256];
extern PACKAGE char CP_AN_BLWINAR0[256];
extern PACKAGE char CP_NA_BLWINAR0[256];
extern PACKAGE char CP_AN_BLLT1DA0[256];
extern PACKAGE char CP_NA_BLLT1DA0[256];
extern PACKAGE char CP_AN_BLLT1DE0[256];
extern PACKAGE char CP_NA_BLLT1DE0[256];
extern PACKAGE char CP_AN_BLLT1UK0[256];
extern PACKAGE char CP_NA_BLLT1UK0[256];
extern PACKAGE char CP_AN_BLLT1US0[256];
extern PACKAGE char CP_NA_BLLT1US0[256];
extern PACKAGE char CP_AN_BLLT1ES0[256];
extern PACKAGE char CP_NA_BLLT1ES0[256];
extern PACKAGE char CP_AN_BLLT1FI0[256];
extern PACKAGE char CP_NA_BLLT1FI0[256];
extern PACKAGE char CP_AN_BLLT1FR0[256];
extern PACKAGE char CP_NA_BLLT1FR0[256];
extern PACKAGE char CP_AN_BLLT1CA0[256];
extern PACKAGE char CP_NA_BLLT1CA0[256];
extern PACKAGE char CP_AN_BLLT1IS0[256];
extern PACKAGE char CP_NA_BLLT1IS0[256];
extern PACKAGE char CP_AN_BLLT1IT0[256];
extern PACKAGE char CP_NA_BLLT1IT0[256];
extern PACKAGE char CP_AN_BLLT1NL0[256];
extern PACKAGE char CP_NA_BLLT1NL0[256];
extern PACKAGE char CP_AN_BLLT1NO0[256];
extern PACKAGE char CP_NA_BLLT1NO0[256];
extern PACKAGE char CP_AN_BLLT1PT0[256];
extern PACKAGE char CP_NA_BLLT1PT0[256];
extern PACKAGE char CP_AN_BLLT1SV0[256];
extern PACKAGE char CP_NA_BLLT1SV0[256];
extern PACKAGE char CP_AN_DB2ANDEU[256];
extern PACKAGE char CP_NA_DB2ANDEU[256];
extern PACKAGE char CP_AN_BGDB868[256];
extern PACKAGE char CP_NA_BGDB868[256];
extern PACKAGE char CP_AN_DB936CN0[256];
extern PACKAGE char CP_NA_DB936CN0[256];
extern PACKAGE char CP_AN_DB950TW0[256];
extern PACKAGE char CP_NA_DB950TW0[256];
extern PACKAGE char CP_AN_DB852CZ0[256];
extern PACKAGE char CP_NA_DB852CZ0[256];
extern PACKAGE char CP_AN_DB867CZ0[256];
extern PACKAGE char CP_NA_DB867CZ0[256];
extern PACKAGE char CP_AN_DB865DA0[256];
extern PACKAGE char CP_NA_DB865DA0[256];
extern PACKAGE char CP_AN_DB437DE0[256];
extern PACKAGE char CP_NA_DB437DE0[256];
extern PACKAGE char CP_AN_DB850DE0[256];
extern PACKAGE char CP_NA_DB850DE0[256];
extern PACKAGE char CP_AN_DB437GR0[256];
extern PACKAGE char CP_NA_DB437GR0[256];
extern PACKAGE char CP_AN_DB437UK0[256];
extern PACKAGE char CP_NA_DB437UK0[256];
extern PACKAGE char CP_AN_DB850UK0[256];
extern PACKAGE char CP_NA_DB850UK0[256];
extern PACKAGE char CP_AN_DB437US0[256];
extern PACKAGE char CP_NA_DB437US0[256];
extern PACKAGE char CP_AN_DB850US0[256];
extern PACKAGE char CP_NA_DB850US0[256];
extern PACKAGE char CP_AN_DB437ES1[256];
extern PACKAGE char CP_NA_DB437ES1[256];
extern PACKAGE char CP_AN_DB850ES0[256];
extern PACKAGE char CP_NA_DB850ES0[256];
extern PACKAGE char CP_AN_DB437FI0[256];
extern PACKAGE char CP_NA_DB437FI0[256];
extern PACKAGE char CP_AN_DB437FR0[256];
extern PACKAGE char CP_NA_DB437FR0[256];
extern PACKAGE char CP_AN_DB850FR0[256];
extern PACKAGE char CP_NA_DB850FR0[256];
extern PACKAGE char CP_AN_DB850CF0[256];
extern PACKAGE char CP_NA_DB850CF0[256];
extern PACKAGE char CP_AN_DB863CF1[256];
extern PACKAGE char CP_NA_DB863CF1[256];
extern PACKAGE char CP_AN_DB852HDC[256];
extern PACKAGE char CP_NA_DB852HDC[256];
extern PACKAGE char CP_AN_DB437IT0[256];
extern PACKAGE char CP_NA_DB437IT0[256];
extern PACKAGE char CP_AN_DB850IT1[256];
extern PACKAGE char CP_NA_DB850IT1[256];
extern PACKAGE char CP_AN_DB932JP0[256];
extern PACKAGE char CP_NA_DB932JP0[256];
extern PACKAGE char CP_AN_DB932JP1[256];
extern PACKAGE char CP_NA_DB932JP1[256];
extern PACKAGE char CP_AN_DB949KO0[256];
extern PACKAGE char CP_NA_DB949KO0[256];
extern PACKAGE char CP_AN_DB437NL0[256];
extern PACKAGE char CP_NA_DB437NL0[256];
extern PACKAGE char CP_AN_DB850NL0[256];
extern PACKAGE char CP_NA_DB850NL0[256];
extern PACKAGE char CP_AN_DB865NO0[256];
extern PACKAGE char CP_NA_DB865NO0[256];
extern PACKAGE char CP_AN_DB852PO0[256];
extern PACKAGE char CP_NA_DB852PO0[256];
extern PACKAGE char CP_AN_DB850PT0[256];
extern PACKAGE char CP_NA_DB850PT0[256];
extern PACKAGE char CP_AN_DB860PT0[256];
extern PACKAGE char CP_NA_DB860PT0[256];
extern PACKAGE char CP_AN_DB866RU0[256];
extern PACKAGE char CP_NA_DB866RU0[256];
extern PACKAGE char CP_AN_DB852SL0[256];
extern PACKAGE char CP_NA_DB852SL0[256];
extern PACKAGE char CP_AN_DB437SV0[256];
extern PACKAGE char CP_NA_DB437SV0[256];
extern PACKAGE char CP_AN_DB850SV1[256];
extern PACKAGE char CP_NA_DB850SV1[256];
extern PACKAGE char CP_AN_DB874TH0[256];
extern PACKAGE char CP_NA_DB874TH0[256];
extern PACKAGE char CP_AN_DB857TR0[256];
extern PACKAGE char CP_NA_DB857TR0[256];
extern PACKAGE char CP_AN_FOXCZWIN[256];
extern PACKAGE char CP_NA_FOXCZWIN[256];
extern PACKAGE char CP_AN_FOXCZ895[256];
extern PACKAGE char CP_NA_FOXCZ895[256];
extern PACKAGE char CP_AN_FOXDEWIN[256];
extern PACKAGE char CP_NA_FOXDEWIN[256];
extern PACKAGE char CP_AN_FOXDE437[256];
extern PACKAGE char CP_NA_FOXDE437[256];
extern PACKAGE char CP_AN_FOXNOWIN[256];
extern PACKAGE char CP_NA_FOXNOWIN[256];
extern PACKAGE char CP_AN_FOXNO437[256];
extern PACKAGE char CP_NA_FOXNO437[256];
extern PACKAGE char CP_AN_FOXNO850[256];
extern PACKAGE char CP_NA_FOXNO850[256];
extern PACKAGE char CP_AN_DBHEBREW[256];
extern PACKAGE char CP_NA_DBHEBREW[256];
extern PACKAGE char CP_AN_MSSGRWIN[256];
extern PACKAGE char CP_NA_MSSGRWIN[256];
extern PACKAGE char CP_AN_ORAWE850[256];
extern PACKAGE char CP_NA_ORAWE850[256];
extern PACKAGE char CP_AN_ANHEBREW[256];
extern PACKAGE char CP_NA_ANHEBREW[256];
extern PACKAGE char CP_AN_ASCII[256];
extern PACKAGE char CP_NA_ASCII[256];
extern PACKAGE char CP_AN_BULGARIA[256];
extern PACKAGE char CP_NA_BULGARIA[256];
extern PACKAGE char CP_AN_CHINA[256];
extern PACKAGE char CP_NA_CHINA[256];
extern PACKAGE char CP_AN_CYRR[256];
extern PACKAGE char CP_NA_CYRR[256];
extern PACKAGE char CP_AN_CZECH[256];
extern PACKAGE char CP_NA_CZECH[256];
extern PACKAGE char CP_AN_CSKAMEN[256];
extern PACKAGE char CP_NA_CSKAMEN[256];
extern PACKAGE char CP_AN_SPANISH[256];
extern PACKAGE char CP_NA_SPANISH[256];
extern PACKAGE char CP_AN_GRCP437[256];
extern PACKAGE char CP_NA_GRCP437[256];
extern PACKAGE char CP_AN_HEBREW[256];
extern PACKAGE char CP_NA_HEBREW[256];
extern PACKAGE char CP_AN_HUN852DC[256];
extern PACKAGE char CP_NA_HUN852DC[256];
extern PACKAGE char CP_AN_INTL850[256];
extern PACKAGE char CP_NA_INTL850[256];
extern PACKAGE char CP_AN_INTL[256];
extern PACKAGE char CP_NA_INTL[256];
extern PACKAGE char CP_AN_ICELAND[256];
extern PACKAGE char CP_NA_ICELAND[256];
extern PACKAGE char CP_AN_JAPAN[256];
extern PACKAGE char CP_NA_JAPAN[256];
extern PACKAGE char CP_AN_KOREA[256];
extern PACKAGE char CP_NA_KOREA[256];
extern PACKAGE char CP_AN_NORDAN[256];
extern PACKAGE char CP_NA_NORDAN[256];
extern PACKAGE char CP_AN_NORDAN40[256];
extern PACKAGE char CP_NA_NORDAN40[256];
extern PACKAGE char CP_AN_POLISH[256];
extern PACKAGE char CP_NA_POLISH[256];
extern PACKAGE char CP_AN_SLOVENE[256];
extern PACKAGE char CP_NA_SLOVENE[256];
extern PACKAGE char CP_AN_SWEDFIN[256];
extern PACKAGE char CP_NA_SWEDFIN[256];
extern PACKAGE char CP_AN_TAIWAN[256];
extern PACKAGE char CP_NA_TAIWAN[256];
extern PACKAGE char CP_AN_THAI[256];
extern PACKAGE char CP_NA_THAI[256];
extern PACKAGE char CP_AN_TURK[256];
extern PACKAGE char CP_NA_TURK[256];
extern PACKAGE char CP_AN_BGPD1251[256];
extern PACKAGE char CP_NA_BGPD1251[256];
extern PACKAGE char CP_AN_ANCYRR[256];
extern PACKAGE char CP_NA_ANCYRR[256];
extern PACKAGE char CP_AN_ANCZECH[256];
extern PACKAGE char CP_NA_ANCZECH[256];
extern PACKAGE char CP_AN_ANGREEK1[256];
extern PACKAGE char CP_NA_ANGREEK1[256];
extern PACKAGE char CP_AN_ANHUNDC[256];
extern PACKAGE char CP_NA_ANHUNDC[256];
extern PACKAGE char CP_AN_ANSIINTL[256];
extern PACKAGE char CP_NA_ANSIINTL[256];
extern PACKAGE char CP_AN_ANSII850[256];
extern PACKAGE char CP_NA_ANSII850[256];
extern PACKAGE char CP_AN_ANSINOR4[256];
extern PACKAGE char CP_NA_ANSINOR4[256];
extern PACKAGE char CP_AN_ANPOLISH[256];
extern PACKAGE char CP_NA_ANPOLISH[256];
extern PACKAGE char CP_AN_ANSISLOV[256];
extern PACKAGE char CP_NA_ANSISLOV[256];
extern PACKAGE char CP_AN_ANSISPAN[256];
extern PACKAGE char CP_NA_ANSISPAN[256];
extern PACKAGE char CP_AN_ANSISWFN[256];
extern PACKAGE char CP_NA_ANSISWFN[256];
extern PACKAGE char CP_AN_ANTURK[256];
extern PACKAGE char CP_NA_ANTURK[256];
extern PACKAGE char CP_AN_JASCII[256];
extern PACKAGE char CP_NA_JASCII[256];
extern PACKAGE char CP_AN_ANCZECHW[256];
extern PACKAGE char CP_NA_ANCZECHW[256];
extern PACKAGE char CP_AN_ANIL2CZW[256];
extern PACKAGE char CP_NA_ANIL2CZW[256];
extern PACKAGE char CP_AN_CZECHW[256];
extern PACKAGE char CP_NA_CZECHW[256];
extern PACKAGE char CP_AN_CSKAMENW[256];
extern PACKAGE char CP_NA_CSKAMENW[256];
extern PACKAGE char CP_AN_IL2CZW[256];
extern PACKAGE char CP_NA_IL2CZW[256];
extern PACKAGE char CP_AN_DBWINES0[256];
extern PACKAGE char CP_NA_DBWINES0[256];
extern PACKAGE char CP_AN_BLROM800[256];
extern PACKAGE char CP_NA_BLROM800[256];
extern PACKAGE char CP_AN_SYDC437[256];
extern PACKAGE char CP_NA_SYDC437[256];
extern PACKAGE char CP_AN_SYDC850[256];
extern PACKAGE char CP_NA_SYDC850[256];
extern PACKAGE char CP_AN_DBWINWE0[256];
extern PACKAGE char CP_NA_DBWINWE0[256];
extern PACKAGE char *CP_AN_TYPE_ARRAY[118];
extern PACKAGE char *CP_NA_TYPE_ARRAY[118];
extern PACKAGE char *CP_TYPE_NAMES[118];
extern PACKAGE char *CP_TYPE_DESCS[118];
extern PACKAGE Byte CP_TYPE_CODES[118];
extern PACKAGE Byte CP_TYPE_CODES_DBASE[118];
extern PACKAGE Byte CP_TYPE_CODES_PARADOX[118];

}	/* namespace Dscp */
using namespace Dscp;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsCP
