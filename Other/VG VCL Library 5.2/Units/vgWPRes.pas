{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgWPRes resource constants unit               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgWPRes;

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxWPStrID           = 49000;

  SBookmarkNotFound     = MaxWPStrID - 1;
  SConnectError         = MaxWPStrID - 2;
  SNotConnected         = MaxWPStrID - 3;
  SInvalidChar          = MaxWPStrID - 4;
  SPrinting             = MaxWPStrID - 5;
  SFileNotFound         = MaxWPStrID - 6;
  SInvalidFileName      = MaxWPStrID - 7;
  SNewDocError          = MaxWPStrID - 8;
  SOpenDocError         = MaxWPStrID - 9;
  SBookmarksNotFound    = MaxWPStrID - 10;
  SGroupsNotFound       = MaxWPStrID - 11;
  SBookmarkSQLNotFound  = MaxWPStrID - 12;
  SInvalidBookmark      = MaxWPStrID - 13;
  SInvalidBookmarkText  = MaxWPStrID - 14;
  SInvalidNesting       = MaxWPStrID - 15;
  SDuplicateSQL         = MaxWPStrID - 16;
  SFieldNotFound        = MaxWPStrID - 17;
  SDataSetClassEmpty    = MaxWPStrID - 18;

implementation

{$R *.RES}

end.
