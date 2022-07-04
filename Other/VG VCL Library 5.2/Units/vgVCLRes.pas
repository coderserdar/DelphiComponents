{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgVCLRes resource constants unit               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC}
{$D-,L-}

unit vgVCLRes;

interface

const
  MaxVCLStrID                = 51000;

  { Registration }
  SRegFirst                  = MaxVCLStrID;
  SRegSystem                 = SRegFirst - 1;
  SRegTools                  = SRegFirst - 2;
  SRegControls               = SRegFirst - 3;
  SRegExplorer               = SRegFirst - 4;
  SRegReports                = SRegFirst - 5;
  SRegHASP                   = SRegFirst - 6;
  SRegDataAccess             = SRegFirst - 7;
  SRegDataControls           = SRegFirst - 8;
  SRegMultiTier              = SRegFirst - 9;
  SRegMisc                   = SRegFirst - 10;
  SRegVGServer               = SRegFirst - 11;

  SRegIniFile                = SRegFirst - 20;
  SRegIniSection             = SRegIniFile - 1;

  { Library constants }

  MaxVCLResStrID             = MaxVCLStrID - 100;

  { vgUtils, vgSystem, vgComObj }
  MaxVgUtils                 = MaxVCLResStrID - 1;
  SCannotDeleteFile          = MaxVgUtils - 1;
  SCannotRenameFile          = MaxVgUtils - 2;
  STlsCannotAlloc            = MaxVgUtils - 3;
  SUknownCompressorSign      = MaxVgUtils - 4;
  SBadVariantType            = MaxVgUtils - 5;
  SNA                        = MaxVgUtils - 6;
  SNIA                       = MaxVgUtils - 7;
  SNotSupported              = MaxVgUtils - 8;
  SNIY                       = MaxVgUtils - 9;

  { vgTools, vgStndrt }
  MaxVgTools                 = MaxVgUtils - 100;
  SParameterNotFound         = MaxVgTools - 1;
  SParamTooBig               = MaxVgTools - 2;
  SFieldUndefinedType        = MaxVgTools - 3;
  SFieldUnsupportedType      = MaxVgTools - 4;
  STextTrue                  = MaxVgTools - 5;
  STextFalse                 = MaxVgTools - 6;
  SCircularLink              = MaxVgTools - 7;
  SItemNotFound              = MaxVgTools - 8;
  SDuplicateItem             = MaxVgTools - 9;

  { TMoneyString }
  MaxMoneyString             = MaxVgTools - 100;
  SsrCurrency                = MaxMoneyString - 1;
  SsrCurrencySub             = MaxMoneyString - 2;
  SsrMaleOne                 = MaxMoneyString - 3;
  SsrFemaleOne               = MaxMoneyString - 4;
  SsrTen                     = MaxMoneyString - 5;
  SsrFirstTen                = MaxMoneyString - 6;
  SsrHundred                 = MaxMoneyString - 7;
  SsrThousand                = MaxMoneyString - 8;
  SsrMillion                 = MaxMoneyString - 9;
  SsrBillion                 = MaxMoneyString - 10;
  
  SsrZero                    = MaxMoneyString - 20;

  { Dialogs }
  MaxDialogStrID             = MaxVgTools - 200;

  { Login Dialog }
  SLoginDialogCaption        = MaxDialogStrID - 1;
  SLoginCaption              = MaxDialogStrID - 2;
  SLoginPassword             = MaxDialogStrID - 3;

implementation

{$R *.RES}

end.
