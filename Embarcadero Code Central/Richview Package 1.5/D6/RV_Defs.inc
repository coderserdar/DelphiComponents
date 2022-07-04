{$B-}
{$H+}

{------------------------------------------------------------------------------}
{                  Determining Delphi/C++Builder version                       }

{$IFNDEF VER80}                         {       not Delphi 1.0           }
  {$IFNDEF VER90}                       {       not Delphi 2.0           }
    {$DEFINE RICHVIEWCBDEF1}            (* C++Builder 1.0+              *)
    {$IFNDEF VER93}                     {       not C++Builder 1.0       }
      {$DEFINE RICHVIEWCBDEF3}          (* C++Builder 3.0+              *)
      {$IFNDEF VER110}                  {       not C++Builder 3.0       }
        {$DEFINE RICHVIEWDEF3}          (* Delphi 3.0+, C++Builder 4.0+ *)
        {$IFNDEF VER100}                {       not Delphi 3.0           }
          {$DEFINE RICHVIEWDEF4}        (* Delphi 4.0+                  *)
          {$IFNDEF VER120}              {       not Delphi 4.0           }
            {$IFNDEF VER125}            {       not C++Builder 4.0       }
              {$DEFINE RICHVIEWDEF5}    (* Delphi 5.0+                  *)
              {$IFNDEF VER130}          {       not Delphi 5.0           }
                 {$IFNDEF VER135}       {       not C++Builder 5.0       }
                   {$DEFINE RICHVIEWDEF6}(* Delphi 6.0+                 *)
                   {$J+}
                 {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{------------------------------------------------------------------------------}
{ CONDITIONAL DEFINES.                                                         }
{ Do not modify this file, use Project|Options, Directories/Conditionals,      }
{ Conditional defines instead.                                                 }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ Defines for shrinking exe size / improving speed and memory usage by         }
{ turning off unused features.                                                 }
{------------------------------------------------------------------------------}

{NOT$DEFINE RVDONOTUSEALLCAPS}
{
   Define it if you do not wish to use rvfsAllCaps option.
   Some spead up of text drawing.
}

{NOT$DEFINE RVDONOTUSEJPEGIMAGE}
{
   Define it if you do not wish to use Delphi jpeg units.
   This will cause saving images to HTML as bitmaps, and turning off special
   support for jpegs in paletted display mode.
}

{NOT$DEFINE RVDONOTUSEHTML}
{
   Define it to remove all HTML-related methods.
}

{NOT$DEFINE RVDONOTUSERTF}
{
   Define it to remove all RTF-export related methods
}

{NOT$DEFINE RVDONOTUSERTFIMPORT}
{
   Define it to remove all RTF-import related methods.
}

{NOT$DEFINE RVDONOTUSERVF}
{
   Define it to remove all RVF-related methods.
   You can do it only if you do not use TRichViewEdit. Editor requires
   RVF methods.
}

{NOT$DEFINE DONOTUSEJUSTIFY}
{
   Define it to remove justify alignment support.
   Was not properly tested.
}

{NOT$DEFINE RVDONOTUSEUNICODE}
{
   Define it to disable Unicode support.
}
{------------------------------------------------------------------------------}
{ Other defines                                                                }
{------------------------------------------------------------------------------}
{NOT$DEFINE RVUSETEXTHOVERCOLORWITHSELECTED}
{
  Define if you wish to return behavior of v1.2-, where text hover color was
  used even for selected text.
}

{NOT$DEFINE DONOTDECVSCROLLUNITS}
{
  Define to forbid RichView to decrease vertical scrolling units automatically
  when needed (if you wish to control scrolling units yourself).
}

{NOT$DEFINE RVDONOTCORRECTWMFSCALE}
{
  There is a problem in Delphi TMetafile class (wrong size) when pasting
  some metafiles from Clipboard. RichView tries to correct it.
  Turn this define on, if you have troubles with metafile sizes when pasting.
}

{NOT$DEFINE RVDONOTUSEINPLACE}

{$DEFINE RVALLOWCPBYCP}
{
  RichView since version 1.0 does not allow to add one checkpoint just after
  another.
  When defined (default), second checkpoint is ignored.
  When not defined, RichView raises exception.
}

{------------------------------------------------------------------------------}
{ Automatical defines:                                                         }
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF1}
{
 IME (Input Method Editor) support is disabled for Delphi 2.
 D2 has required library file - Imm.pas in sources, but has not it in library (?!).
 So I think support for D2 is possible, but requires some IDE configuration...

}

{$IFNDEF RVDONOTUSEUNICODE}

{$DEFINE RVUSEIME} // Using IME

{$ENDIF}
{$ENDIF}

{$IFNDEF RICHVIEWDEF3}
// Turning off Jpeg support for Delphi2, C++Builder 1,3
{$DEFINE RVDONOTUSEJPEGIMAGE}
{$ENDIF}


{$DEFINE RVDEBUG}

