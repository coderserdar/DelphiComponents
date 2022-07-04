(*

  TMPHexEditor v 02-06-2006<br>

  @author((C) markus stephany, vcl[at]mirkes[dot]de, all rights reserved.)
  @abstract(TMPHexEditor displays and edits binary files in hexadecimal notation)
  @lastmod(02-06-2006)

  credits to :<br><br>
  - John Hamm, http://users.snapjax.com/john/<br><br>

  - Christophe Le Corfec for introducing the EBCDIC format and the nice idea about
    half byte insert/delete<br><br>

  - Philippe Chessa for his suggestions about AsText, AsHex and better support for
    the french keyboard layout<br><br>

  - Daniel Jensen for octal offset display and the INS-key recognition stuff<br><br>

  - Shmuel Zeigerman for introducing more flexible offset display formats<br><br>

  - Vaf, http://carradio.al.ru for reporting missing delver.inc and suggesting OnChange<br><br>

  - Eugene Tarasov for reporting that setting the BytesPerColumn value to 4 at design
    time didn't work<br><br>

  - FuseBurner for BytesPerUnit/RulerBytesPerUnit related suggestions<br><br>

  - Motzi for SyncView/ShowPositionIfNotFocused related suggestions<br><br>

  - Martin Hsiao for bcb compatibility and reporting some bugs when moving cursor beyond eof<br><br>

  - Miyu for delphi 7 defines<br><br>

  - Nils Hoyer for bcb testing and his help on creating a BCB6 package<br><br>

  - Skamnitsly S.V for reporting a bug when doubleclicking the ruler bar<br><br>

  - Pete Fraser for reporting problems with array properties under BCB<br><br>

  - Andrew Novikov for bug reports and suggestions<br><br>

  - Al for bug reports<br><br>

  - Dieter Köhler for reporting the delphi vcl related CanFocus bug<br><br>

  - Piotr Likus for reporting a cardinal&lt;-&gt;integer related bug in the Undo method<br><br>

  - Marc Girod for bug reports<br><br>

  - Gerd Schwartz for reporting a bug with printing headers/footers that contain long texts
    (MPHexEditorEx only)<br><br>

  - Bogdan Ureche for reporting an integer overflow when moving the cursor over a large selection<br><br>

  <h3>history:</h3>
  <p><ul>
  <li>v 02-06-2006: february 06, 2006<br><br>
         - changed key handling (VK_INSERT, no action if a control key is pressed)<br>
         - fixed an access violation in CursorOverSelection when moving the cursor over
           a large selection<br>
         - added conditional defines for delphi 8 and delphi 2005 in MPDELVER.INC</br>
         - removed FastPointer property, use the GetFastPointer function instead (it checks boundaries)<br><br></li>

  <li>v 05-23-2005: may 23, 2005<br><br>
         - fixed an access violation in the undo storage code when reallocating
           memory during storing of undo data<br>
         - the secondary focus frame on the hex pane now is painted around the
           whole actual data value (4 digits if unicode, 2 digits otherwise)<br>
         - added procedure @link(CenterCursorPosition)<br>
         - in @link(InsertBuffer) and @link(Replace) now the position parameter
           is checked<br><br></li>

  <li>v 12-29-2004: december 29, 2004<br><br>
         - initialized Result to '' in some string functions/methods to avoid
           non empty Result vars at function startup due to compiler
           optimizations (particularly on d4), e.g. printing did not work
           correctly under d4<br>
         - updated some of the sample projects (fixed the broken bcb6 sample,
           added printing to the hex viewer and the bcb6 editor sample) <br><br></li>

  <li>v 12-28-2004: december 28, 2004<br><br>
         - changed the progress event calling part in @link(Find) and
           @link(FindWithWildcard) to avoid a division by zero error when working
           on files &lt; 500 bytes<br><br></li>

  <li>v 12-21-2004: december 21, 2004<br><br>
         - changed @link(PrepareFindReplaceData) to avoid an exception when
           the string parameter is empty<br><br></li>

  <li>v 11-12-2004: november 12, 2004<br><br>
         - changed mouse selection in insert mode, now it's more text
           editor-like<br>
         - @link(Undo) and @link(Redo) disabled when @link(ReadonlyView)
           is True<br>
         - some small other modifications<br>
         <br><br></li>

  <li>v 10-26-2004: october 26, 2004<br><br>
         - fixed a typecasting bug in the Undo method (integer overflow)<br>
         - added some utility functions for unsigned int64 arithmetics (@link(AddU64), @link(TryAddU64),
           @link(SubtractU64), @link(TrySubtractU64), @link(MultiplyU64), @link(TryMultiplyU64),
           @link(DivideU64), @link(TryDivideU64), @link(ModuloU64), @link(TryModuloU64))
         <br><br></li>

  <li>v 08-29-2004: august 29, 2004<br><br>
         - Added @link(ActiveFieldBackground) color property<br><br></li>

  <li>v 08-14-2004: august 14, 2004<br><br>
         - the caret was not set properly when switching from
           hex to char pane if no data was in the editor <br>
         - Added @link(MaskedChars) property<br><br></li>

  <li>v 06-15-2004: june 15, 2004<br><br>
         - Added @link(DrawDataPosition) and @link(IsDrawDataSelected) properties <br>
         - changes in drawing/invalidating to avoid unnecessary painting <br>
         - OnMouseDown is now called also if offset pane or ruler are clicked <br>
         - if @link(BytesPerUnit) is changed, the selection is reset
           if (SelCount mod BytesPerUnit) &lt;&gt; 0 <br>
         - if @link(CaretKind) is ckAuto, the caret is a bottom line if
           @link(ReadOnlyView) is True<br><br></li>

  <li>v 06-10-2004: june 10, 2004<br><br>
         - added @link(RulerNumberBase) property <br>
         - overwritten CanFocus method due to vcl bug (see
           <a href="http://info.borland.com/devsupport/delphi/fixes/delphi4/vcl.html">
           http://info.borland.com/devsupport/delphi/fixes/delphi4/vcl.html</a>,
           ref 279<br><br></li>

  <li>v 06-07-2004: june 07, 2004<br><br>
         - fixed a crash ("Grid index out of range") when switching from
           unicode <br>
         - @link(SyncView) modified to be able to synchronize the view
           of editors with different data sizes/layouts, also with offset <br>
         - on changing TopRow/LeftCol the caret is repositionned <br>
         - overwritten mouse wheel handling to allow page scrolling <br>
         - manual handling of MaskChar property streaming due to bug reports
           ("Invalid Property Value")<br><br></li>

  <li>v 05-30-2004: may 30, 2004<br><br>
         - fixed broken CanOpenFile routine (files were always marked read-only)<br><br></li>

  <li>v 05-27-2004: may 27, 2004<br><br>
         - added @link(IsMaxOffset) property <br>
         - the control gets focused when the mouse is clicked even when
           the mouse is over the selection<br><br></li>

  <li>v 05-13-2004: may 13, 2004<br><br>
         - @link(OnDrawCell) is now also called for the top left cell<br>
         - setting @link(UnicodeChars) to False now correctly sets
           @link(BytesPerUnit) to 1 <br><br></li>

  <li>v 04-18-2004: april 18, 2004<br><br>
         - parameters aBuffer and bBuffer were interchanged in the
           CopyMemory call in @link(TranslateBufferFromAnsi)<br>
         - @link(GetOffsetString) can now be called in @link(OnGetOffsetText)
           without crashing (infinite recursion = stack overflow) <br><br></li>

  <li>v 01-08-2004: january 08, 2004<br><br>
         - added some explicit pointer typecasts for {$T+} compatibility<br>
         - removed FindTable and FindTableI properties under BCB (doesn't
           compile) <br><br></li>

  <li>v 12-16-2003: december 16, 2003<br><br>
         - Setting the @link(DataSize) property is now undoable<br>
         - Added the public @link(SetDataSizeFillByte) property to be able to control
           what byte is used to enlarge the data<br>
         - Now checking @link(NoSizeChange) before allowing to set @link(DataSize)<br>
         - CreateUndo is no longer a function, but a procedure. Now an
           exception is raised when no undo record can be created <br><br></li>

  <li>v 12-10-2003: december 10, 2003<br><br>
         - Renamed OnLoadSaveProgress to @link(OnProgress)<br>
         - Added property @link(FindProgress)<br>
         - Added custom find methods (@link(OnFind), @link(OnWildcardFind)<br>
         - @link(Find) and @link(FindWithWildcard) speeded up by using
           precompiled character tables<br>
         - @link(Find) and @link(FindWithWildcard) now also fire the @link(OnProgress) event
           if @link(FindProgress) is set to true<br>
         - fixed a bug in mouse handling (weird selection or line offsets when
           doublecklicking ruler bar/offset panel) <br>
         - modified selectioncode to better support double byte selection (unicode) <br><br></li>

  <li>v 09-24-2003: september 24, 2003<br><br>
         - modified the BCB6 package<br><br></li>

  <li>v 09-09-2003: september 09, 2003<br><br>
         - changed some constants, classes and types from MPTH... to MPH...<br>
         - changed MPHCustTransFieldFrom/To to @link(MPHCustomCharConv)<br>
         - @link(BytesPerBlock) and @link(SeparateBlocksInCharField) properties added<br>
         - @link(DataSize) property is writeable now<br>
         - Page down key now also reaches the last row<br>
         - added @link(OnGetOffsetText) property<br>
         - added @link(AddSelectionUndo) procedure<br>
         - added defines for delphi7, renamed delver.inc to mpdelver.inc<br>
         - added wildcard search (@link(FindWithWildcard))<br>
         - added @link(SeekToEOF)<br>
         - changed keyboard handling, so OnKeyDown should work better<br>
         - added @link(GotoBookmark) method to set cursor to a bookmarked position<br>
         - added @link(OnBookmarkChanged) property<br>
         - support for unsigned int64 radix conversions<br>
         - @link(Replace) method added<br><br></li>

  <li>v 07-05-2003: july 05, 2003<br><br>
         - better handling of odd sized files when BytesPerUnit &lt;&gt; 1<br>
         - added support for pasting clipboard data in fixed filesize mode in @link(TMPHexEditorEx)<br>
         - added RegEdit_HexData clipboard support in @link(TMPHexEditorEx)<br><br></li>

  <li>v 05-25-2003-b: may 25, 2003<br><br>
         - fixed a bug (moving the cursor beyond eof)<br><br></li>

  <li>v 05-25-2003: may 25, 2003<br><br>
         - added some kind of ownerdraw (see @link(OnDrawCell))<br><br></li>

  <li>v 05-20-2003: may 20, 2003<br><br>
         - renamed, added and changed some methods, classes and properties<br>
         - fixed some bugs in the ruler display (e.g. when BytesPerRow is
           changed)<br>
         - fixed some bugs when BytesPerUnit &lt;&gt; 1<br>
         - added some unicode support (@link(UnicodeChars) and
           @link(UnicodeBigEndian))<br>
         - fixed some half byte (nibble) related bugs<br><br></li>

  <li>v 05-17-2003: may 17, 2003<br><br>
         - added @link(DisplayStart) and @link(DisplayEnd) functions to retrieve
           the data bounds currently displayed<br>
         - added @link(BytesPerUnit) and @link(RulerBytesPerUnit) properties to
           treat words/dwords/qwords as a unit<br>
         - added @link(SyncView) procedure and @link(OnSelectionChanged)
           property to synchronize position and selection with another
           editor<br>
         - added @link(ShowPositionIfNotFocused) property to show the current
           position if the editor is not focused<br><br></li>

  <li>v 10-25-2002: october 25, 2002<br><br>
         - corrected the BytesPerColumn default value<br><br></li>

  <li>v 08-18-2002: august 18, 2002<br><br>
         - modified painting and selection<br>
  - implemented an additional ruler bar at the top<br>
         - new properties: @link(ShowRuler), @link(DrawGutter3D)<br><br></li>

  <li>v 08-12-2002: august 12, 2002<br><br>
  - modified Changed calls to get correct Modified property in
           OnChange handler<br><br></li>

  <li>v 08-09-2002: august 09, 2002<br><br>
  - included missing include file delver.inc<br>
  - added OnChange event<br><br></li>

  <li>v 07-21-2002: july 21, 2002<br><br>
  too many changes to mention here (completely rewritten, basic and advanced versions
  TMPHexEditor and TMPHexEditorEx), plz read the documentation included with this
  package for more information</li>
  </ul></p>

*)

unit MPHexEditor;
{$R *.res}
{.$DEFINE TINYHEXER}// don't define this!
{$DEFINE FASTACCESS} // if this is defined, direct access to the stream memory is given

(* define this if you want to have the old savetostream behaviour
  (clear target stream before copying data).
  if it is undef'd, do not clear the target stream
  (just copy the editor data to the stream) *)
{.$DEFINE OLD_STREAM_OUT}

{$I MPDELVER.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,

  Grids;

type
  // @exclude
  TGridCoord = Grids.TGridCoord;

  // character conversion type
  TMPHCharConvType = (cctFromAnsi, cctToAnsi);
  // character conversion table
  TMPHCharConvTable = array[0..255] of Char;
  // character conversion data storage
  TMPHCharConv = array[TMPHCharConvType] of TMPHCharConvTable;

const
  // block size in file i/o
  MPH_FILEIO_BLOCKSIZE = $F000;

  // this message is posted to the hex editor when it should update the caret position
  CM_INTUPDATECARET = CM_BASE + $100;

  // this message is posted when an OnSelectionChange event is to be fired
  CM_SELECTIONCHANGED = CM_BASE + $101;

  (* translation tables from/to ms windows ansi (~ MS Latin-1)  *)

  // macintosh..ms ansi conversion
  MPH_CCONV_MAC: TMPHCharConv = (
    //ansi to mac
    (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B,
    #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B,
    #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B,
    #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B,
    #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B,
    #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B,
    #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B,
    #$7C, #$7D, #$7E, #$7F,
    #$C4, #$C5, #$AB, #$C9, #$D1, #$F7, #$DC, #$E1, #$E0, #$E2, #$E4, #$E3,
    #$AC, #$B0, #$AA, #$F8,
    #$D5, #$CE, #$C3, #$CF, #$D3, #$D4, #$D2, #$DB, #$DA, #$DD, #$F6, #$F5,
    #$FA, #$F9, #$FB, #$FC,
    #$A0, #$C1, #$A2, #$A3, #$DF, #$B4, #$B6, #$A4, #$C6, #$A9, #$BB, #$C7,
    #$C2, #$AD, #$A8, #$FF,
    #$A1, #$B1, #$B2, #$B3, #$A5, #$B5, #$A6, #$B7, #$B8, #$B9, #$BC, #$C8,
    #$BA, #$BD, #$CA, #$C0,
    #$CB, #$E7, #$E5, #$CC, #$80, #$81, #$AE, #$82, #$E9, #$83, #$E6, #$E8,
    #$ED, #$EA, #$EB, #$EC,
    #$D0, #$84, #$F1, #$EE, #$EF, #$CD, #$85, #$D7, #$AF, #$F4, #$F2, #$F3,
    #$86, #$D9, #$DE, #$A7,
    #$88, #$87, #$89, #$8B, #$8A, #$8C, #$BE, #$8D, #$8F, #$8E, #$90, #$91,
    #$93, #$92, #$94, #$95,
    #$F0, #$96, #$98, #$97, #$99, #$9B, #$9A, #$D6, #$BF, #$9D, #$9C, #$9E,
    #$9F, #$FD, #$FE, #$D8
    ),
    // mac to ansi
    (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B,
    #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B,
    #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B,
    #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B,
    #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B,
    #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B,
    #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B,
    #$7C, #$7D, #$7E, #$7F,
    #$C4, #$C5, #$C7, #$C9, #$D1, #$D6, #$DC, #$E1, #$E0, #$E2, #$E4, #$E3,
    #$E5, #$E7, #$E9, #$E8,
    #$EA, #$EB, #$ED, #$EC, #$EE, #$EF, #$F1, #$F3, #$F2, #$F4, #$F6, #$F5,
    #$FA, #$F9, #$FB, #$FC,
    #$A0, #$B0, #$A2, #$A3, #$A7, #$B4, #$B6, #$DF, #$AE, #$A9, #$8E, #$82,
    #$8C, #$AD, #$C6, #$D8,
    #$8D, #$B1, #$B2, #$B3, #$A5, #$B5, #$A6, #$B7, #$B8, #$B9, #$BC, #$AA,
    #$BA, #$BD, #$E6, #$F8,
    #$BF, #$A1, #$AC, #$92, #$80, #$81, #$A8, #$AB, #$BB, #$83, #$BE, #$C0,
    #$C3, #$D5, #$91, #$93,
    #$D0, #$84, #$96, #$94, #$95, #$90, #$F7, #$D7, #$FF, #$DD, #$98, #$97,
    #$86, #$99, #$DE, #$A4,
    #$88, #$87, #$89, #$8B, #$8A, #$C2, #$CA, #$C1, #$CB, #$C8, #$CD, #$CE,
    #$CF, #$CC, #$D3, #$D4,
    #$F0, #$D2, #$DA, #$DB, #$D9, #$9B, #$9A, #$85, #$8F, #$9D, #$9C, #$9E,
    #$9F, #$FD, #$FE, #$AF
    )
    );

  // ebcdic cp38..ms ansi conversion
  MPH_CCONV_BCD38: TMPHCharConv = (
    //ansi to bcd (taken from recode 3.5)
    (#$00, #$01, #$02, #$03, #$37, #$2D, #$2E, #$2F, #$16, #$05, #$25, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$3C, #$3D, #$32, #$26, #$18, #$19, #$3F, #$27,
    #$1C, #$1D, #$1E, #$1F,
    #$40, #$4F, #$7F, #$7B, #$5B, #$6C, #$50, #$7D, #$4D, #$5D, #$5C, #$4E,
    #$6B, #$60, #$4B, #$61,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$7A, #$5E,
    #$4C, #$7E, #$6E, #$6F,
    #$7C, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$D1, #$D2,
    #$D3, #$D4, #$D5, #$D6,
    #$D7, #$D8, #$D9, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$4A,
    #$E0, #$5A, #$5F, #$6D,
    #$79, #$81, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$91, #$92,
    #$93, #$94, #$95, #$96,
    #$97, #$98, #$99, #$A2, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$C0,
    #$20, #$D0, #$A1, #$07,
    #$80, #$22, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$8A, #$8B,
    #$8C, #$8D, #$8E, #$8F,
    #$90, #$77, #$2C, #$0A, #$3B, #$3E, #$1A, #$70, #$71, #$72, #$9A, #$9B,
    #$9C, #$9D, #$9E, #$9F,
    #$A0, #$15, #$73, #$74, #$75, #$76, #$6A, #$78, #$09, #$3A, #$AA, #$AB,
    #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB,
    #$BC, #$BD, #$BE, #$BF,
    #$23, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$CA, #$CB,
    #$CC, #$CD, #$CE, #$CF,
    #$1B, #$24, #$06, #$14, #$28, #$2B, #$21, #$17, #$51, #$52, #$DA, #$DB,
    #$DC, #$DD, #$DE, #$DF,
    #$2A, #$E1, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$29, #$EA, #$EB,
    #$EC, #$ED, #$EE, #$EF,
    #$30, #$31, #$08, #$33, #$34, #$35, #$36, #$04, #$38, #$39, #$FA, #$FB,
    #$FC, #$FD, #$FE, #$FF
    ),
    // bcd to ansi (taken from recode 3.5)
    (#$00, #$01, #$02, #$03, #$F7, #$09, #$D2, #$7F, #$F2, #$A8, #$93, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$D3, #$A1, #$08, #$D7, #$18, #$19, #$96, #$D0,
    #$1C, #$1D, #$1E, #$1F,
    #$7C, #$D6, #$81, #$C0, #$D1, #$0A, #$17, #$1B, #$D4, #$E9, #$E0, #$D5,
    #$92, #$05, #$06, #$07,
    #$F0, #$F1, #$16, #$F3, #$F4, #$F5, #$F6, #$04, #$F8, #$F9, #$A9, #$94,
    #$14, #$15, #$95, #$1A,
    #$20, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$5B, #$2E,
    #$3C, #$28, #$2B, #$21,
    #$26, #$D8, #$D9, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$5D, #$24,
    #$2A, #$29, #$3B, #$5E,
    #$2D, #$2F, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$A6, #$2C,
    #$25, #$5F, #$3E, #$3F,
    #$97, #$98, #$99, #$A2, #$A3, #$A4, #$A5, #$91, #$A7, #$60, #$3A, #$23,
    #$40, #$27, #$3D, #$22,
    #$80, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$8A, #$8B,
    #$8C, #$8D, #$8E, #$8F,
    #$90, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F, #$70, #$71, #$72, #$9A, #$9B,
    #$9C, #$9D, #$9E, #$9F,
    #$A0, #$7E, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$AA, #$AB,
    #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB,
    #$BC, #$BD, #$BE, #$BF,
    #$7B, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$CA, #$CB,
    #$CC, #$CD, #$CE, #$CF,
    #$7D, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F, #$50, #$51, #$52, #$DA, #$DB,
    #$DC, #$DD, #$DE, #$DF,
    #$5C, #$E1, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$EA, #$EB,
    #$EC, #$ED, #$EE, #$EF,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$FA, #$FB,
    #$FC, #$FD, #$FE, #$FF
    )
    );

type
  // custom Exception class
  EMPHexEditor = class(Exception);

  (* bookmark record:<br>
     defined by pressing SHIFT+CTRL+[0..9], goto bookmark by pressing CTRL+[0..9]<br><br>

     - mPosition: file position<br>
     - mInCharField: cursor in character pane (True) or hex number pane
  *)
  TMPHBookmark = record
    mPosition: integer;
    mInCharField: boolean;
  end;

  // array of bookmarks, representing keys 0..9
  TMPHBookmarks = array[0..9] of TMPHBookmark;

  (* look of the editor's caret:<br>
     - ckFull: full block<br>
     - ckLeft: left line<br>
     - ckBottom: bottom line<br>
     - ckAuto: left line if @link(InsertMode), full block if overwrite,
               bottom line if ReadOnlyView
  *)
  TMPHCaretKind = (ckFull,
    ckLeft,
    ckBottom,
    ckAuto
    );

  (* how to show a file's content in the character pane of the editor:<br>
    - tkAsIs:    leave as is (current windows code page)<br>
    - tkDos8:    current dos codepage<br>
    - tkASCII:   7 bit ascii<br>
    - tkMac:     macintosh charset (translation always from/to ms cp 1252 (ms latin1)!!<br>
    - tkBCD:     ibm ebcdic codepage 38 (translation always from/to ms cp 1252 (ms latin1)!!<br>
    - tkCustom:  custom codepage stored in @link(MPHCustomCharConv)
  *)
  TMPHTranslationKind = (tkAsIs,
    tkDos8,
    tkASCII,
    tkMac,
    tkBCD

    , tkCustom

    );

  (* action indicator used in @link(OnProgress) event handler:<br>
    - pkLoad:  loading data<br>
    - pkSave:  saving data<br>
    - pkFind:  finding
  *)
  TMPHProgressKind = (pkLoad,
    pkSave, pkFind
    );

  (* progress event handler, used in @link(OnProgress)<br><br>

       - ProgressType: am i loading or saving? (see @link(TMPHProgressKind))<br>
       - aName: name of file to be load from/saved to<br>
       - Percent: current progress (0..100)<br>
       - Cancel: if set to true, the load/save procedure will abort (no meaning in Find* methods) <br>
  *)
  TMPHProgressEvent = procedure(Sender: TObject;
    const ProgressType: TMPHProgressKind;
    const aName: TFileName;
    const Percent: byte;
    var Cancel: boolean) of object;

  (* retrieve the "line number" to display by the application<br><br>

       - Number: the number to convert to text
       - OffsetText: the resulting text output
  *)
  TMPHGetOffsetTextEvent = procedure(Sender: TObject;
    const Number: int64;
    var OffsetText: string) of object;

  (* handler for custom search routines<br><br>

     - Pattern: the data to find
     - PatLength: length of the data to find
     - SearchFrom: first search position
     - SearchUntil: last search position
     - IgnoreCase: case sensitive?
     - Wilcard: Wildcard character (only used by FindWithWildcard)
     - FoundPos: result, set to -1 if data was not found
  *)
  TMPHFindEvent = procedure(Sender: TObject;
    const Pattern: PChar; const PatLength: integer;
    const SearchFrom, SearchUntil: integer;
    const IgnoreCase: boolean;
    const Wildcard: Char;
    var FoundPos: Integer) of object;

  // precompiled converted character table types for faster data search
  PMPHFindTable = ^TMPHFindTable;
  TMPHFindTable = array[#0..#255] of Char;

  //@exclude
  // flags internally used in the undo storage
  TMPHUndoFlag = (
    // kind of undo storage
    ufKindBytesChanged,
    ufKindByteRemoved,
    ufKindInsertBuffer,
    ufKindReplace,
    ufKindAppendBuffer,
    ufKindNibbleInsert,
    ufKindNibbleDelete,
    ufKindConvert,
    ufKindSelection, // store a selection
    ufKindCombined,
    ufKindAllData, // store current data and size for complete undo
    // additional information
    ufFlagByte1Changed,
    ufFlagByte2Changed,
    ufFlagModified,
    ufFlag2ndByteCol,
    ufFlagInCharField,
    ufFlagHasSelection,
    ufFlagInsertMode,
    ufFlagIsUnicode,
    ufFlagIsUnicodeBigEndian,
    ufFlagHasDescription
    );

  //@exclude
  // set of undo flags
  TMPHUndoFlags = set of TMPHUndoFlag;

type
  // persistent color storage (contains the colors in hex editors)
  TMPHColors = class(TPersistent)
  private
    FParent: TControl;
    FOffset: TColor;
    FOddColumn: TColor;
    FEvenColumn: TColor;
    FCursorFrame: TColor;
    FNonFocusCursorFrame: TColor;
    FBackground: TColor;
    FChangedText: TColor;
    FChangedBackground: TColor;
    FCurrentOffsetBackground: TColor;
    FOffsetBackGround: TColor;
    FActiveFieldBackground: TColor;
    FCurrentOffset: TColor;
    FGrid: TColor;

    procedure SetOffsetBackGround(const Value: TColor);
    procedure SetCurrentOffset(const Value: TColor);
    procedure SetParent(const Value: TControl);
    procedure SetGrid(const Value: TColor);
    procedure SetBackground(const Value: TColor);
    procedure SetChangedBackground(const Value: TColor);
    procedure SetChangedText(const Value: TColor);
    procedure SetCursorFrame(const Value: TColor);
    procedure SetEvenColumn(const Value: TColor);
    procedure SetOddColumn(const Value: TColor);
    procedure SetOffset(const Value: TColor);
    procedure SetActiveFieldBackground(const Value: TColor);
    procedure SetCurrentOffsetBackground(const Value: TColor);
    procedure SetNonFocusCursorFrame(const Value: TColor);
  public
    // @exclude(constructor)
    constructor Create(Parent: TControl);
    // @exclude()
    procedure Assign(Source: TPersistent); override;
    // parent hex editor control
    property Parent: TControl read FParent write SetParent;
  published
    // background color
    property Background: TColor read FBackground write SetBackground;
    // background color of modified bytes (in overwrite mode)
    property ChangedBackground: TColor read FChangedBackground write
      SetChangedBackground;
    // foreground color of modified bytes (in overwrite mode)
    property ChangedText: TColor read FChangedText write SetChangedText;
    // color of the cursor and position frame in the second pane
    property CursorFrame: TColor read FCursorFrame write SetCursorFrame;
    // foreground color of the line offsets
    property Offset: TColor read FOffset write SetOffset;
    // foreground color of odd columns
    property OddColumn: TColor read FOddColumn write SetOddColumn;
    // foreground color of even columns
    property EvenColumn: TColor read FEvenColumn write SetEvenColumn;
    // background color of the current line in the offset pane (gutter)
    property CurrentOffsetBackground: TColor read FCurrentOffsetBackground write
      SetCurrentOffsetBackground;
    // background color of the offset pane (gutter)
    property OffsetBackGround: TColor read FOffsetBackGround write
      SetOffsetBackGround;
    // foreground color of the current line in the offset pane (gutter)
    property CurrentOffset: TColor read FCurrentOffset write SetCurrentOffset;
    // pen color of the grid
    property Grid: TColor read FGrid write SetGrid;
    // color of a cursor frame in a non-focused editor
    property NonFocusCursorFrame: TColor read FNonFocusCursorFrame write
      SetNonFocusCursorFrame;
    // background color of the active field (hex/chars)
    property ActiveFieldBackground: TColor read FActiveFieldBackground write
      SetActiveFieldBackground;

  end;

  // @exclude(stream class for internal storage/undo)
  TMPHMemoryStream = class(TMemoryStream)
  private

    procedure CheckBounds(const AMax: Integer);
    function PointerAt(const APosition: Integer): Pointer;
  protected

  public

{$IFDEF FASTACCESS}
    function GetAddress(const Index, Count: integer): PByte;
{$ENDIF}
    procedure ReadBufferAt(var Buffer; const APosition, ACount: Integer);
    procedure WriteBufferAt(const Buffer; const APosition, ACount: Integer);
    procedure Move(const AFromPos, AToPos, ACount: Integer);
    procedure TranslateToAnsi(const FromTranslation: TMPHTranslationKind; const
      APosition, ACount: integer);
    procedure TranslateFromAnsi(const ToTranslation: TMPHTranslationKind; const
      APosition, ACount: integer);
    function GetAsHex(const APosition, ACount: integer; const SwapNibbles:
      Boolean): string;
  end;

  //@exclude
  // undo storage implementation
  TMPHUndoStorage = class;

  //@exclude
  // offset format flags
  TMPHOffsetFormatFlag = (offCalcWidth,
    // calculate minwidth depending on data size (width field = '-')
    offCalcRow,
    // calculate _BytesPerUnit depending on bytes per row (=real line numbers)
    offCalcColumn, // " bytes per column (= column numbers)
    offBytesPerUnit // use BytesPerUnit property
    );

  //@exclude
  // set of the above flags
  TMPHOffsetFormatFlags = set of TMPHOffsetFormatFlag;

  //@exclude
  // offset format record
  TMPHOffsetFormat = record
    Format: string; // format as string
    Prefix,
      Suffix: string; // splitted format
    MinWidth: integer; // min length of value (zero padded on the left)
    Flags: // auto calculation flags
    TMPHOffsetFormatFlags;
    Radix, // radix (base) of display (2..16)
    _BytesPerUnit: byte; // length of one unit (1 Byte...BytesPerRow Bytes)
  end;

  (* owner draw event type. parameters:<br><br>
     - Sender: the hex editor<br>
     - ACanvas: the editor's canvas<br>
     - ACol, ARow: the position to be drawn<br>
     - AWideText: the text to be drawn<br>
     - ARect: the cell rectangle<br>
     - ADefaultDraw: if set to True (default), default drawing isperformed after the event handler returns.
       if set to false, the event handler must do all cell painting.
  *)
  TMPHDrawCellEvent = procedure(Sender: TObject; ACanvas: TCanvas; ACol, ARow:
    Integer; var AWideText: WideString; ARect: TRect; var ADefaultDraw: Boolean)
    of object;

  // protected ancestor of the hex editor components

  TCustomMPHexEditor = class(TCustomGrid)

  private

    FIntLastHexCol: integer;
    FFindTable,
      FFindTableI: TMPHFindTable;
    FIsMaxOffset: boolean;
    FFindProgress: boolean;
    FBlockSize: Integer;
    FSepCharBlocks: boolean;
    FOnGetOffsetText: TMPHGetOffsetTextEvent;
    FFixedFileSize: boolean;
    FCharWidth,
      FCharHeight: integer;
    FBookmarkImageList: TImageList;
    FInsertModeOn: boolean;
    FCaretBitmap: TBitmap;
    FColors: TMPHColors;
    FBytesPerRow: integer;
    FOffSetDisplayWidth: integer;
    FBytesPerRowDup: integer;
    FDataStorage: TMPHMemoryStream;
    FSwapNibbles: integer;
    FFocusFrame: boolean;
    FIsFileReadonly: boolean;
    FBytesPerCol: integer;
    FPosInCharField,
      FLastPosInCharField: boolean;
    FFileName: string;
    FModifiedBytes: TBits;
    FBookmarks: TMPHBookmarks;
    FSelStart,
      FSelPosition,
      FSelEnd: integer;
    FSelBeginPosition: integer;
    FTranslation: TMPHTranslationKind;
    FCaretKind: TMPHCaretKind;
    FReplaceUnprintableCharsBy: char;
    FAllowInsertMode: boolean;
    FWantTabs: boolean;
    FReadOnlyView: boolean;
    FHideSelection: boolean;
    FGraySelOnLostFocus: boolean;
    FOnProgress: TMPHProgressEvent;
    FMouseDownCol,
      FMouseDownRow: integer;
    FShowDrag: boolean;
    FDropCol,
      FDropRow: integer;
    FOnInvalidKey,
      FOnTopLeftChanged: TNotifyEvent;

    FDrawGridLines: boolean;
    FDrawGutter3D: boolean;
    FGutterWidth: integer;
    FOffsetFormat: TMPHOffsetFormat;
    FSelectionPossible: boolean;
    FBookmarkBitmap: TBitmap;
    FCursorList: array of integer;
    FHasCustomBMP: boolean;
    FStreamFileName: string;
    FHasFile: boolean;
    FMaxUndo: integer;
    FHexChars: array[0..15] of char;
    FHexLowerCase: boolean;
    FOnChange: TNotifyEvent;
    FShowRuler: boolean;
    FBytesPerUnit: Integer;
    FRulerBytesPerUnit: Integer;
    FOnSelectionChanged: TNotifyEvent;
    FSelectionChangedCount: Integer;
    FShowPositionIfNotFocused: Boolean;
    FOffsetHandler: Boolean;
    FUsedRulerBytesPerUnit: Integer;
    FIsSelecting: boolean;
    FMouseUpCanResetSel: boolean;
    FUndoStorage: TMPHUndoStorage;
    FUnicodeCharacters: Boolean;
    FUnicodeBigEndian: Boolean;
    FMaskedChars: TSysCharSet;

    FDrawDataPosition: integer;
    FOnDrawCell: TMPHDrawCellEvent;

    FOnBookmarkChanged: TNotifyEvent;

    FIsDrawDataSelected: boolean;

    FOnWildcardFind: TMPHFindEvent;
    FOnFind: TMPHFindEvent;
{$IFDEF FASTACCESS}
    FSetDataSizeFillByte: Byte;
{$ENDIF}
    FRulerNumberBase: byte;
    property Color;

    function IsInsertModePossible: boolean;

    function IsFileSizeFixed: boolean;
    procedure InternalErase(const KeyWasBackspace: boolean; const UndoDesc:
      string = '');
    procedure SetReadOnlyView(const Value: boolean);
    procedure SetCaretKind(const Value: TMPHCaretKind);
    procedure SetFocusFrame(const Value: boolean);
    procedure SetBytesPerColumn(const Value: integer);
    procedure SetSwapNibbles(const Value: boolean);
    function GetSwapNibbles: boolean;
    function GetBytesPerColumn: integer;
    procedure SetOffsetDisplayWidth;
    procedure SetColors(const Value: TMPHColors);
    procedure SetReadOnlyFile(const Value: boolean);
    procedure SetTranslation(const Value: TMPHTranslationKind);
    procedure SetModified(const Value: boolean);
    procedure SetChanged(DataPos: integer; const Value: boolean);
    procedure SetFixedFileSize(const Value: boolean);
    procedure SetAllowInsertMode(const Value: boolean);
    function GetInsertMode: boolean;
    procedure SetWantTabs(const Value: boolean);
    procedure SetHideSelection(const Value: boolean);
    procedure SetGraySelectionIfNotFocused(const Value: boolean);
    function CalcColCount: integer;
    function GetLastCharCol: integer;
    function GetPropColCount: integer;
    function GetPropRowCount: integer;
    function GetMouseOverSelection: boolean;
    function CursorOverSelection(const X, Y: integer): boolean;
    function MouseOverFixed(const X, Y: integer): boolean;
    procedure AdjustBookmarks(const From, Offset: integer);
    procedure IntSetCaretPos(const X, Y, ACol: integer);
    procedure TruncMaxPosition(var DataPos: integer);
    procedure SetSelection(DataPos, StartPos, EndPos: integer);
    function GetCurrentValue: integer;
    procedure SetInsertMode(const Value: boolean);
    function GetModified: boolean;
    //function GetDataPointer: Pointer;
    procedure SetBytesPerRow(const Value: integer);
    procedure SetMaskChar(const Value: char);
    procedure SetAsText(const Value: string);
    procedure SetAsHex(const Value: string);
    function GetAsText: string;
    function GetAsHex: string;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    // show or hide caret depending on row/col in view
    procedure CheckSetCaret;
    // get the row according to the given buffer position
    function GetRow(const DataPos: integer): integer;
    // invalid key pressed (in ebcdic)
    procedure WrongKey;

    // create an inverting caret bitmap
    procedure CreateCaretGlyph;
    // get start of selection
    function GetSelStart: integer;
    // get end of selection
    function GetSelEnd: integer;
    // get selection count
    function GetSelCount: integer;
    // set selection start
    procedure SetSelStart(aValue: integer);
    // set selection end
    procedure SetSelEnd(aValue: integer);
    // position the caret in the given field
    procedure SetInCharField(const Value: boolean);
    // is the caret in the char field ?
    function GetInCharField: boolean;
    // insert a buffer (internal)
    procedure InternalInsertBuffer(Buffer: PChar; const Size, Position:
      integer);
    // append some data (int)
    procedure InternalAppendBuffer(Buffer: PChar; const Size: integer);
    // store the caret properties
    procedure InternalGetCurSel(var StartPos, EndPos, ACol, ARow: integer);
    // delete data
    procedure InternalDelete(StartPos, EndPos, ACol, ARow: integer);
    // delete one half byte
    function InternalDeleteNibble(const Pos: integer;
      const HighNibble: boolean): boolean;
    // insert half byte
    function InternalInsertNibble(const Pos: integer; const HighNibble:
      boolean): boolean;
    // used by nibble functions
    function CreateShift4BitStream(const StartPos: integer; var FName:
      TFileName): TFileStream;
    // convert a given amount of data from ansi to something different and vice versa
    procedure InternalConvertRange(const aFrom, aTo: integer; const aTransFrom,
      aTransTo: TMPHTranslationKind);
    // move data in buffer to a different position
    procedure MoveFileMem(const aFrom, aTo, aCount: integer);
    function GetBookmark(Index: byte): TMPHBookmark;
    procedure SetBookmark(Index: byte; const Value: TMPHBookmark);
    procedure SetBookmarkVals(const Index: byte; const Position: integer; const
      InCharField: boolean);
    procedure SetDrawGridLines(const Value: boolean);
    procedure SetGutterWidth(const Value: integer);
    // images have changed
    procedure BookmarkBitmapChanged(Sender: TObject);
    procedure SetBookmarkBitmap(const Value: TBitmap);

    function GetVersion: string;
    procedure SetVersion(const Value: string);

    // free alloc'd memory of one of the storage streams;
    procedure FreeStorage(FreeUndo: boolean = False);
    function GetCanUndo: boolean;
    function GetCanRedo: boolean;
    function GetUndoDescription: string;
    function GetOffsetFormat: string;
    procedure SetOffsetFormat(const Value: string);
    // generate offset format
    procedure GenerateOffsetFormat(Value: string);
    procedure SetHexLowerCase(const Value: boolean);
    procedure SetDrawGutter3D(const Value: boolean);
    procedure SetShowRuler(const Value: boolean);
    procedure SetBytesPerUnit(const Value: integer);
    procedure SetRulerString;
    procedure CheckSelectUnit(var AStart, AEnd: Integer);
    procedure SetRulerBytesPerUnit(const Value: integer);
    procedure SetShowPositionIfNotFocused(const Value: Boolean);
    function GetDataAt(Index: integer): Byte;
    procedure SetDataAt(Index: integer; const Value: Byte);
    procedure SetUnicodeCharacters(const Value: Boolean);
    procedure SetUnicodeBigEndian(const Value: Boolean);
    function GetPositionAtCursor(const ACol, ARow: integer): integer;
    function GetIsCharFieldCol(const ACol: integer): Boolean;
    procedure SetDataSize(const Value: integer);
    procedure SetBlockSize(const Value: Integer);
    procedure SetSepCharBlocks(const Value: boolean);
    procedure SetFindProgress(const Value: boolean);
    procedure SetRulerNumberBase(const Value: byte);
    procedure SetMaskedChars(const Value: TSysCharSet);
{+}
{.$IFDEF BCB}
  protected
    // bcb seems to need overwritten abstract methods for dynamically created
    // controls (this method is never called in TMPHexEditor/Ex)
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
{.$ENDIF}
{+.}
  protected
    // @exclude()
    FRulerString: string;
    // @exclude()
    FRulerCharString: string;

    // @exclude(used by TMPHexEditorEx for internal drag 'n' drop)
    FFixedFileSizeOverride: boolean;
    // @exclude(used by TMPHexEditorEx for internal undo changing)
    FModified: boolean;
    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(actually used bytes per unit)
    property UsedRulerBytesPerUnit: Integer read FUsedRulerBytesPerUnit;
    // @exclude(True: cells are currently to be selected)
    property IsSelecting: boolean read FIsSelecting;
    // @exclude(True: MouseUp resets selection)
    property MouseUpCanResetSel: boolean read FMouseUpCanResetSel write
      FMouseUpCanResetSel;
    // @exclude(memory stream which contains the undo/redo data)
    property UndoStorage: TMPHUndoStorage read FUndoStorage;
    // @exclude(stream that contains the data)
    property DataStorage: TMPHMemoryStream read FDataStorage;
    // @exclude(fire OnSelectionChange)
    procedure SelectionChanged; virtual;
    // @exclude(set a new selection)
    procedure NewSelection(SelFrom, SelTo: integer);
    // @exclude(get the current mouse position)
    function CheckMouseCoord(var X, Y: integer): TGridCoord;
    // @exclude(assure the value is a multiple of FBytesPerUnit)
    procedure CheckUnit(var AValue: Integer);
    // call changed on every undo creation for OnChange event
    procedure Changed; virtual;
    // returns the drop file position after a drag'n'drop operation
    function DropPosition: integer;
    // copy a stream to a second one and fire the OnProgress handler
    procedure Stream2Stream(strFrom, strTo: TStream; const Operation:
      TMPHProgressKind; const Count: integer = -1);
    (* allows descendants to take special action if contents are to be saved
     to the file from where the data was load *)
    procedure PrepareOverwriteDiskFile; virtual;
    // store the current Cursor and set it to crHourGlass (see also @link(OldCursor))
    procedure WaitCursor;
    // reset the Cursor to the previous value (see also @link(WaitCursor))
    procedure OldCursor;
    // @exclude(override paint)
    procedure Paint; override;
    // @exclude(view changed)
    procedure TopLeftChanged; override;
    // adjust cell widths/heigths depending on font, offset format, bytes per row/column...
    procedure AdjustMetrics;
    // get the size of the contained data
    function GetDataSize: integer;
    // @exclude(calculate the grid sizes)
    procedure CalcSizes;
    // @exclude(select one cell)
    function SelectCell(ACol, ARow: longint): boolean; override;
    // @exclude(get the data position depending on col and row)
    function GetPosAtCursor(const aCol, aRow: integer): integer;
    // @exclude(vice versa)
    function GetCursorAtPos(const aPos: integer; const aChars: boolean):
      TGridCoord;
    // @exclude(get the column of the other field (hex<->char))
    function GetOtherFieldCol(const aCol: integer): integer;
    // @exclude(get the column of the other field (hex<->char))
    function GetOtherFieldColCheck(const aCol: integer): integer;
    // @exclude(can the cell be selected ?)
    function CheckSelectCell(aCol, aRow: integer): boolean;
    // @exclude(char message handler)
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    // @exclude(posted message to update the caret position)
    procedure CMINTUPDATECARET(var Msg: TMessage); message CM_INTUPDATECARET;
    // @exclude(posted message to fire an OnSelectionChanged event)
    procedure CMSelectionChanged(var Msg: TMessage); message
      CM_SELECTIONCHANGED;
    // @exclude(for shortcuts)
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    // @exclude(readjust grid sizes after font has changed)
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    // @exclude(change a byte at the given position)
    procedure IntChangeByte(const aOldByte, aNewByte: byte;
      aPos, aCol, aRow: integer; const UndoDesc: string = '');
    // @exclude(change two bytes at the given position)
    procedure IntChangeWideChar(const aOldChar, aNewChar: WideChar; aPos, aCol,
      aRow: integer; const UndoDesc: string = '');
    // @exclude(keydown handler)
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    // @exclude(keyup handler)
    //procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // @exclude(has this byte been modified ?)
    function HasChanged(aPos: integer): boolean;
    // @exclude(redraw some lines)
    procedure RedrawPos(aFrom, aTo: integer);
    // @exclude(make a selection)
    procedure Select(const aCurCol, aCurRow, aNewCol, aNewRow: integer);
    // @exclude(mouse down handler)
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); override;
    // @exclude(mouse move handler)
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    // @exclude(mouse up handler)
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    // @exclude(is undo record creation possible?)
    function CanCreateUndo(const aKind: TMPHUndoFlag; const aCount, aReplCount:
      integer): Boolean; virtual;
    // @exclude(add an undo to the undo buffer)
    procedure CreateUndo(const aKind: TMPHUndoFlag; const aPos, aCount,
      aReplCount: integer; const sDesc: string = '');
    // @exclude(after loading)
    procedure Loaded; override;
    // @exclude(override CreateWnd)
    procedure CreateWnd; override;
    // @exclude(wm_setfocus handler)
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    // @exclude(wm_killfocus handler)
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    // @exclude(wm_vscroll handler)
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    // @exclude(wm_hscroll handler)
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    // @exclude(resize the control)
    procedure Resize; override;
    // @exclude(store bitmap ? (its set to true, if a custom bitmap has been stored in BookmarkBitmap))
    function HasCustomBookmarkBitmap: boolean;
    // number of bytes to show in each row
    property BytesPerRow: integer read FBytesPerRow write SetBytesPerRow;
    // if set to True, the find* routines also fire OnProgress events (default is False)
    property FindProgress: boolean read FFindProgress write SetFindProgress
      default False;
    // number of bytes to show in each column
    property BytesPerColumn: integer read GetBytesPerColumn write
      SetBytesPerColumn default 2;
    (* translation kind of the data (used to show characters on and to handle key presses in the char pane),
       (see also @link(TMPHTranslationKind))
    *)
    property Translation: TMPHTranslationKind read FTranslation write
      SetTranslation;
    (* offset display ("line numbers") format, in the form<br>
       [r|c|&lt;HEXNUM&gt;%][-|&lt;HEXNUM&gt;!]&lt;HEXNUM&gt;:[Prefix]|[Suffix]<br>
       (&lt;HEXNUM&gt; means a number in hexadecimal format (without prefix/suffix))<br><br>
       - first field (up to the percent sign):<br>
       <ul>
       <li>sets the "bytes per unit field" of the offset display format</li>
       <li>if it's set to 1, each row offset displays the data position in bytes</li>
       <li>if it's set to 2, each row offset displays the data position in words</li>
       <li>if it's set to 4, each row offset displays the data position in dwords</li>
       <li>if it's set to "r", each row offset displays the current row number (1st row=0,
       see also @link(BytesPerRow))</li>
       <li>if it's set to "c", each row offset displays the current column number (1st column=0,
       see also @link(BytesPerColumn))</li>
       <li>if this field is omitted, bytes per unit is set to the value of the
       @link(RulerBytesPerUnit) property</li>
       </ul><br>
       - second field (up to the exclamation mark):<br>
       <ul>
       <li>sets the minimum width of the number part, if the number is shorter, it will be padded
       by '0' chars at the left</li>
       <li>if this field reads -!, the the minimum width is automatically set to the longest number
       that can appear in the editor (the data's size)</li>
       <li>if this field is omitted, the minimum width is set to 1</li>
       </ul><br>
       - third field (up to the colon):<br>
       <ul>
       <li>sets the radix (base) of the offset format in hex notation</li>
       <li>set this to '10' (without quotes) for hexadecimal offset display, set it to '08' for
       octal and to '0a' for decimal offset display</li>
       <li>this field cannot be omitted, but the whole format string my be blank to avoid the display of
       offset identifiers</li>
       </ul></br>
       - fourth field (up to the pipe ('|') char):<br>
       <ul>
       <li>the prefix that is put in front of the "number" string (e.g. '0x' or '$' to show that numbers are in hex format)
       </li><li>this field may be omitted (but not the pipe char!)</li>
       </ul><br>
       - fifth (and last) field:<br>
       <ul>
       <li>the suffix to put after the "number string" (e.g. 'h' to show hex numbers)</li>
       <li>this field may be omitted</li></ul>
    *)
    property OffsetFormat: string read GetOffsetFormat write SetOffsetFormat;

    (* if this handler is assigned, the @link(OffsetFormat) is not used to
       create "line numbers", but the application tells the editor how to format the offset text
    *)
    property OnGetOffsetText: TMPHGetOffsetTextEvent read FOnGetOffsetText write
      FOnGetOffsetText;

    (* how many bytes form one block in a row? blocks are separated by a one character wide blank.
       -1 means no block separation (see also @link(SeparateBlocksInCharField)) *)
    property BytesPerBlock: Integer read FBlockSize write SetBlockSize default
      -1;

    (* if @link(BytesPerBlock) is used, this property tells the editor whether it should
       separate blocks of bytes in the character pane too or not *)
    property SeparateBlocksInCharField: boolean read FSepCharBlocks write
      SetSepCharBlocks default True;

    // look of the editor's caret (see @link(TMPHCaretKind))
    property CaretKind: TMPHCaretKind read FCaretKind write SetCaretKind default
      ckAuto;
    // colors to display (see @link(TMPHColors))
    property Colors: TMPHColors read FColors write SetColors;
    (* if FocusFrame is set to True, the current caret position will be displayed in the
       second field (hex - characters) as a dotted focus frame, if set to False, it will
       be shown as an ordinary rectangle
    *)
    property FocusFrame: boolean read FFocusFrame write SetFocusFrame;
    (* if SwapNibbles is set to True, the hex pane will show all bytes in the order
       lower 4 bits-higher 4 bits (i.e. the value 192 dec = C0 hex will be drawn as
       0C). if set to False, hex values will be displayed in usual order. this
       setting also affects hex data input and hex-string conversions
    *)
    property SwapNibbles: boolean read GetSwapNibbles write SetSwapNibbles
      default False;
    // replace @link(MaskedChars) with the following character in the character pane
    property MaskChar: char read FReplaceUnprintableCharsBy write SetMaskChar
      stored False;
    (* if set to True, the data size is readonly, e.g. no data may be appended, deleted
       or inserted, just overwriting is allowed. this also affects @link(InsertMode).
    *)
    property NoSizeChange: boolean read FFixedFileSize write SetFixedFileSize
      default False;
    (* if set to False, switching between overwrite and insert mode is not allowed
       (see also @link(InsertMode) and @link(NoSizeChange))
    *)
    property AllowInsertMode: boolean read FAllowInsertMode write
      SetAllowInsertMode default True;
    (* if set to True, the Tab key is used to switch the caret between hex and character pane.
       if set to False, the Tab key can be used to switch between controls. then the
       combination CTRL+T is used to switch the panes
    *)
    property WantTabs: boolean read FWantTabs write SetWantTabs default True;
    // if set to True, the data can not be edited, just cursor movement is allowed ("Hex Viewer" mode)
    property ReadOnlyView: boolean read FReadOnlyView write SetReadOnlyView
      default False;
    // hide the current selection when the hex editor looses focus (see also @link(GraySelectionIfNotFocused))
    property HideSelection: boolean read FHideSelection write SetHideSelection
      default False;
    (* if set to True and @link(HideSelection) is False, then the current selection will be
       grayed when the hex editor looses focus (the values from the @link(Colors) property will
       be converted to grayscale colors)
    *)
    property GraySelectionIfNotFocused: boolean read FGraySelOnLostFocus write
      SetGraySelectionIfNotFocused default False;
    (* this event is called in @link(LoadFromFile), @link(SaveToFile), @link(Find) and
       @link(FindWithWildcard) routines, so a progress indicator may be updated
       (see also @link(TMPHProgressEvent), @link(FindProgress))
    *)
    property OnProgress: TMPHProgressEvent read FOnProgress write
      FOnProgress;
    (* this event is fired if an invalid character has been typed (like non-hex characters
       in the hex pane)
    *)
    property OnInvalidKey: TNotifyEvent read FOnInvalidKey write FOnInvalidKey;
    // this event is fired if the first visible row or column have been changed (e.g. on scrolling)
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write
      FOnTopLeftChanged;
    // returns the current selection in hex format ('00010203...') as string, uses @link(SwapNibbles)
    function GetSelectionAsHex: string;
    (* replace the current selection by a string containing data in hex format ('00 01 02 03' or similar),
      uses @link(SwapNibbles)
    *)
    procedure SetSelectionAsHex(const s: string);
    // returns a string containing the currently selected data
    function GetSelectionAsText: string;
    // replaces the currently selected data with the string's contents
    procedure SetSelectionAsText(const s: string);
    // if set to True, a grid is drawn
    property DrawGridLines: boolean read FDrawGridLines write SetDrawGridLines;
    // width of the offset display gutter, if set to -1, automatically adjust the gutter's width
    property GutterWidth: integer read FGutterWidth write SetGutterWidth default
      -1;
    (* bitmap containing 20 10x10 pixels pictures for bokkmarks (they are displayed in the offset
      gutter), the first ten pictures represent the bookmarks 0(10)..9, if they are set in the
      hexpane, the last 10 pics are shown if bookmarks are set in the character pane (see also
      @link(TMPHBookMark))
    *)
    property BookmarkBitmap: TBitmap read FBookmarkBitmap write SetBookmarkBitmap
      stored HasCustomBookmarkBitmap;

    // current version of the hex editor component (returns the build data), readonly
    property Version: string read GetVersion write SetVersion stored True;

    // maximum memory that is used for undo storage (in bytes, approximately)
    property MaxUndo: integer read FMaxUndo write FMaxUndo default 1024 * 1024;
    (* insert mode (typed characters are inserted at the current position) or
       overwrite mode (typed characters replace values at the current position), see also
       @link(AllowInsertMode), @link(NoSizeChange) and @link(ReadOnlyView)
    *)
    property InsertMode: boolean read GetInsertMode write SetInsertMode default
      False;
    // if set to True, hex data and hex offsets are displayed in lower case
    property HexLowerCase: boolean read FHexLowerCase write SetHexLowerCase
      default False;
    // this event is called on every data change (load/empty/undo/redo)
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // if set to True, a 3d line is drawn at the right of the offset gutter
    property DrawGutter3D: boolean read FDrawGutter3D write SetDrawGutter3D
      default True;
    // if set to True, a ruler is shown above the first row
    property ShowRuler: boolean read FShowRuler write SetShowRuler default
      False;
    (* number base (i.e. radix) for the ruler display (2-16), tells the component
       which number format to use when drawing the ruler
    *)
    property RulerNumberBase: byte read FRulerNumberBase write SetRulerNumberBase
      default 16;
    (* setting this property changes the way how mouse/keyboard selection
       works:<br>
       e.g. if set to two, two bytes will be treated as a unit, that means you
       cannot select a single byte, only two, four, six... bytes can be selected.
       also drag/drop and clipboard pasting is affected (data size
       is always a multiple of BytesPerUnit). See also @link(RulerBytesPerUnit)
    *)
    property BytesPerUnit: integer read FBytesPerUnit write SetBytesPerUnit
      default 1;
    (* setting this property affects the offset/ruler drawing:<br>
       e.g. if set to two, two bytes will be treated as a unit, that means the
       offset and ruler values will step by one each two bytes.
       if this property is set to -1, it will use the value of the
       @link(BytesPerUnit) property
    *)
    property RulerBytesPerUnit: integer read FRulerBytesPerUnit write
      SetRulerBytesPerUnit default -1;
    // mark the current position even if the editor is not focused
    property ShowPositionIfNotFocused: Boolean read FShowPositionIfNotFocused
      write SetShowPositionIfNotFocused default False;
    (* if set to True, the character pane displays unicode characters
       and the @link(BytesPerUnit) property is set to 2. @link(Translation) is
       set to tkAsIs. @link(BytesPerRow) and @link(BytesPerColumn) must be a
       multiple of two to be able to use the unicode mode.
       see also @link(UnicodeBigEndian)
    *)
    property UnicodeChars: Boolean read FUnicodeCharacters write
      SetUnicodeCharacters default False;
    (* if set to True, big endian unicode mode is used if @link(UnicodeChars) is
       enabled
    *)
    property UnicodeBigEndian: Boolean read FUnicodeBigEndian write
      SetUnicodeBigEndian default False;
    // this event is fired when the selection/caret position has changed
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write
      FOnSelectionChanged;

    // use this event to implement owner drawing. see also @link(TMPHDrawCellEvent)
    property OnDrawCell: TMPHDrawCellEvent read FOnDrawCell write FOnDrawCell;

    // fire OnBookmarkChanged
    procedure BookmarkChanged; virtual;

    procedure DoSetCellWidth(const Index: integer; Value: integer);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadMaskChar(Reader: TReader);
    procedure ReadMaskChar_I(Reader: TReader);
    procedure WriteMaskChar_I(Writer: TWriter);
  public
    { Public-Deklarationen }

{$IFDEF FASTACCESS}
    // return the memory address at the given stream position after checking bounaries
    function GetFastPointer(const Index, Count: integer): PByte;
{$ENDIF}
    //@exclude()
    constructor Create(aOwner: TComponent); override;
    //@exclude()
    destructor Destroy; override;
    // these characters are masked in the character pane using @link(MaskChar)
    property MaskedChars: TSysCharSet read FMaskedChars write SetMaskedChars;
    (* during OnDrawCell event handlers, this property tells the data position currently
       being drawn (-1, if offset or ruler are drawn)
    *)
    property DrawDataPosition: integer read FDrawDataPosition;

    (* during OnDrawCell event handlers, this property tells whether the cell is
       to be drawn in selected style (only valid if DrawDataPosition <> -1)
    *)
    property IsDrawDataSelected: boolean read FIsDrawDataSelected;

    // @exclude(use TMPHexEditor.ReadBuffer!)
    function GetMemory(const Index: Integer): char;
    (* @exclude(see http://info.borland.com/devsupport/delphi/fixes/delphi4/vcl.html,
      ref 279)
    *)

    function CanFocus: Boolean;
{$IFDEF DELPHi5UP} override;
{$ENDIF}
    // @exclude(use TMPHexEditor.WriteBuffer!)
    procedure SetMemory(const Index: integer; const Value: char);

    (* this property is valid only in the @link(OnGetOffsetText) event. if True,
       the component asks for the string of the highest possible offset, if False,
       a row's offset text is queried
    *)
    property IsMaxOffset: boolean read FIsMaxOffset;
    // seek behind the last position if @link(InsertMode) = True, goto last position otherwise
    procedure SeekToEOF;
    (* synchronize another TCustomMPHexEditor view (top, left, selection),
       the optional SyncOffset parameter may be used for a different viewpoint
    *)
    procedure SyncView(Source: TCustomMPHexEditor; SyncOffset: integer = 0);
    // return the offset of the first displayed data
    function DisplayStart: integer;
    // return the offset of the last displayed data
    function DisplayEnd: integer;
    // is the given position part of the selection?
    function IsSelected(const APosition: integer): boolean;
    // calculate a data position from a col/row pair
    property PositionAtCursor[const ACol, ARow: integer]: integer read
    GetPositionAtCursor;
    // is the given col in the hex or the character pane?
    property IsCharFieldCol[const ACol: integer]: Boolean read
    GetIsCharFieldCol;
{$IFDEF FASTACCESS}
    // this byte value is used to fill the data when setting @link(DataSize)
    // enlarges the stream
    property SetDataSizeFillByte: Byte read FSetDataSizeFillByte write
      FSetDataSizeFillByte;
{$ENDIF}
    // has data been load from/saved to a file (or is the filename valid)
    property HasFile: boolean read FHasFile write FHasFile;
    (* each call to UndoBeginUpdate increments an internal counter that prevents using
       undo storage and also disables undo functionality (see also @link(UndoEndUpdate))
    *)
    function UndoBeginUpdate: integer; virtual;
    (* each call to UndoEndUpdate decrements an internal counter that prevents using
       undo storage and also disables undo functionality. the return value is the value
       of this counter. if the counter is reset to zero, undo creation is permitted again
       (see also @link(UndoBeginUpdate))
    *)
    function UndoEndUpdate: integer; virtual;
    // remove selection state from all data
    procedure ResetSelection(const aDraw: boolean);
    // see @link(GetSelectionAsHex) and @link(SetSelectionAsHex)
    property SelectionAsHex: string read GetSelectionAsHex write
      SetSelectionAsHex;
    // see @link(GetSelectionAsText) and @link(SetSelectionAsText)
    property SelectionAsText: string read GetSelectionAsText write
      SetSelectionAsText;
{$IFNDEF BCB}
    (* precompiled character comparison table for custom find routines, see also
     @link(FindTableI), @link(OnFind), @link(OnWildcardFind), case sensitive, not
     public under BCB!
    *)
    property FindTable: TMPHFindTable read FFindTable;
    (* precompiled character comparison table for custom find routines, see also
     @link(FindTable), @link(OnFind), @link(OnWildcardFind), case insensitive, not
     public under BCB!
    *)
    property FindTableI: TMPHFindTable read FFindTableI;
{$ENDIF}

    // implement your custom @link(Find) routine by assigning a method to this handler,
    // see also @link(OnWildcardFind)
    property OnFind: TMPHFindEvent read FOnFind write FOnFind;
    // implement your custom @link(FindWithWildcard) routine by assigning a method
    // to this handler, see also @link(OnFind)
    property OnWildcardFind: TMPHFindEvent read FOnWildcardFind
      write FOnWildcardFind;
    (* returns the given position as it would be drawn in the offset gutter,
      see also @link(OffsetFormat)
    *)
    function GetOffsetString(const Position: cardinal): string; virtual;
    (* returns the given position as it would be drawn in the offset gutter, exception:
      if @link(OffsetFormat) is set to an empty string, returns the hexadecimal representation
      of the Position value (see also @link(GetOffsetString))
    *)
    function GetAnyOffsetString(const Position: integer): string; virtual;
    // returns the height of one row in pixels
    function RowHeight: integer;
    // free the undo storage (discard all possible undo steps)
    procedure ResetUndo;
    // set the current position (like TStream.Seek)
    function Seek(const aOffset, aOrigin: integer): integer;
    (* searches for text or data in the data buffer, returns the find position (-1, if data have not been found):<br><br>
       - aBuffer: data to search for<br>
       - aCount: size of data in aBuffer<br>
       - aStart: start search at this position<br>
       - aEnd: searches up to this position<br>
       - IgnoreCase: if True, lowercase and uppercase characters are treated as if they were equal<br>
       - SearchText: if True, the current @link(Translation) is taken into account when searching textual data<br><br>
       NOTE: call @link(PrepareFindReplaceData) before the first Find call
    *)
    function Find(aBuffer: PChar; aCount: integer; const aStart, aEnd: integer;
      const IgnoreCase: boolean): integer;
    (* searches for text or data in the data buffer using a wildcard character
       returns the find position (-1, if data have not been found):<br><br>
       - aBuffer: data to search for<br>
       - aCount: size of data in aBuffer<br>
       - aStart: start search at this position<br>
       - aEnd: searches up to this position<br>
       - IgnoreCase: if True, lowercase and uppercase characters are treated as if they were equal<br>
       - SearchText: if True, the current @link(Translation) is taken into account when searching textual data<br>
       - Wildcard: this character is a placeholder for any character<br><br>
       NOTE: call @link(PrepareFindReplaceData) before the first FindWithWildcard call
    *)
    function FindWithWildcard(aBuffer: PChar; aCount: integer; const aStart,
      aEnd: integer;
      const IgnoreCase: boolean; const Wildcard: char): integer;
    (* convert a buffer for @link(Find)/@link(FindWithWildcard)/replace operation depending on
       unicode mode. sets the string to lower case if IgnoreCase is True. if in unicode mode,
       creates a unicode string.
    *)
    (*
      store a selection as undo record, so you can restore the selection start and end by using
      @link(Undo). this can be useful e.g. to show position of replaced data
    *)
    procedure AddSelectionUndo(const AStart, ACount: integer);
    function PrepareFindReplaceData(StrData: string; const IgnoreCase, IsText:
      boolean): string;
    // read data into a buffer
    procedure ReadBuffer(var Buffer; const Index, Count: Integer);
    // write a buffer to the file data
    procedure WriteBuffer(const Buffer; const Index, Count: Integer); virtual;
    // delete the currently selected data
    procedure DeleteSelection(const UndoDesc: string = '');
    // load the contents of a stream into the data buffer
    procedure LoadFromStream(Strm: TStream);
    // load the contents of a file into the data buffer
    procedure LoadFromFile(const Filename: string);
    // save the contents of the data buffer into a stream
    procedure SaveToStream(Strm: TStream);
    // save the contents of the data buffer to a file
    procedure SaveToFile(const Filename: string; const aUnModify: boolean =
      True);
    // save a range of bytes to a stream
    procedure SaveRangeToStream(Strm: TStream; const APosition, ACount:
      integer);
    // undo the last modification, multiple undos are possible
    function Undo: boolean;
    // discard the last undo action (only one single redo is possible)
    function Redo: boolean;
    // empty the data buffer and set the filename (e.g. "Untitled")
    procedure CreateEmptyFile(const TempName: string);
    (* returns a buffer containing parts of the data buffer's contents. the buffer is allocated
       in this routine and must be freed by the caller
    *)
    function BufferFromFile(const aPos: integer; var aCount: integer): PChar;
    // insert some data at the specified position into the data buffer
    procedure InsertBuffer(aBuffer: PChar; const aSize, aPos: integer; const
      UndoDesc: string = ''; const MoveCursor: Boolean = True);
    // append some data at the end of the data buffer
    procedure AppendBuffer(aBuffer: PChar; const aSize: integer; const UndoDesc:
      string = ''; const MoveCursor: Boolean = True);
    // replace the currently selected data with some other data
    procedure ReplaceSelection(aBuffer: PChar; aSize: integer; const UndoDesc:
      string = ''; const MoveCursor: Boolean = True);
    // replace some amount of data
    function Replace(aBuffer: PChar; aPosition, aOldCount, aNewCount: integer;
      const UndoDesc:
      string = ''; const MoveCursor: Boolean = False): integer;
    // get the current data position (depending on the cursor/caret)
    function GetCursorPos: integer;
    // delete 4 bits (=half byte = nibble) from the data buffer (see also @link(InsertNibble))
    function DeleteNibble(const aPos: integer; const HighNibble: boolean; const
      UndoDesc: string = ''): boolean;
    // insert 4 bits (0000) into the data buffer (see also @link(DeleteNibble))
    function InsertNibble(const aPos: integer; const HighNibble: boolean; const
      UndoDesc: string = ''): boolean;
    // convert a part of the data buffer's content from one character table to a different one
    procedure ConvertRange(const aFrom, aTo: integer; const aTransFrom,
      aTransTo: TMPHTranslationKind; const UndoDesc: string = '');
    (* returns the data position of the top left cell and also whether the caret is in the
       character pane, see also @link(SetTopLeftPosition)
    *)
    function GetTopLeftPosition(var oInCharField: boolean): integer;
    (* set top left cell to the given data position and also whether the caret is in the
       character pane (see also @link(GetTopLeftPosition))
    *)
    procedure SetTopLeftPosition(const aPosition: integer; const aInCharField:
      boolean);
    (* show a drop position marker on the cell at the given mouse cursor position
      (see also @link(HideDragCell))
    *)
    function ShowDragCell(const X, Y: integer): integer;
    // hide the drop position marker (see also @link(ShowDragCell))
    procedure HideDragCell;
    // combine two or more changes, so @link(Undo) will discard the at once
    procedure CombineUndo(const aCount: integer; const sDesc: string = '');
    (* translate a byte from the current @link(Translation) to the Windows Codepage
      (see also @link(TranslateFromAnsiChar))
    *)
    function TranslateToAnsiChar(const aByte: byte): char;
    (* translate a byte from Windows Codepage to the current @link(Translation)
      (see also @link(TranslateToAnsiChar))
    *)
    function TranslateFromAnsiChar(const aByte: byte): char;
    // retrieve or set the selection start
    property SelStart: integer read GetSelStart write SetSelStart;
    // retrieve or set the selection end
    property SelEnd: integer read GetSelEnd write SetSelEnd;
    // retrieve the size of the selected data
    property SelCount: integer read GetSelCount;
    // is @link(Undo) possible?
    property CanUndo: boolean read GetCanUndo;
    // is @link(Redo) possible?
    property CanRedo: boolean read GetCanRedo;
    // is the caret in the character or the hex pane ?
    property InCharField: boolean read GetInCharField write SetInCharField;
    // description of the next @link(Undo) action
    property UndoDescription: string read GetUndoDescription;
    // if True, the currently loaded file cannot be overwritten
    property ReadOnlyFile: boolean read FIsFileReadonly write SetReadOnlyFile;
    // if True, changes have been made to the data buffer content
    property Modified: boolean read GetModified write SetModified;
    // retrieves or stores the amount of data in the data buffer
    // when enlarging the data stream, the @link(SetDataSizeFillByte) property
    // tells which value to use to fill the new data
    property DataSize: integer read GetDataSize write SetDataSize;
    // array to the data buffer's content
    property Data[Index: integer]: Byte read GetDataAt write SetDataAt;
    // retrieve or set the data as string
    property AsText: string read GetAsText write SetAsText;
    // retrieve or set the data as hex formatted string (00 01 02 03...)
    property AsHex: string read GetAsHex write SetAsHex;
    // name of the file that has been loaded into the data buffer
    property Filename: string read FFileName;
    // retrieve or set bookmarks programmatically (see also @link(TMPHBookmark))
    property Bookmark[Index: byte]: TMPHBookmark read GetBookmark write
    SetBookmark;
    // has the byte at the given position been modified ? (only in overwrite mode)
    property ByteChanged[index: integer]: boolean read HasChanged write
    SetChanged;
    // retrieves the number of columns (grid columns)
    property ColCountRO: integer read GetPropColCount;
    // retrieves the number of rows (grid rows)
    property RowCountRO: integer read GetPropRowCount;
    // returns True if the mouse cursor is positionned over selected data
    property MouseOverSelection: boolean read GetMouseOverSelection;
    // get the data value at the current caret position, returns -1 if an error occured
    property CurrentValue: integer read GetCurrentValue;
    // pointer to the whole data buffer's contents
    //property DataPointer: Pointer read GetDataPointer;
    // select all data
    procedure SelectAll;
    // retrieves the number of visible columns
    property VisibleColCount;
    // retrieves the number of visible rows
    property VisibleRowCount;
    // the control's canvas
    property Canvas;
    // current column (grid column)
    property Col;
    // first visible column
    property LeftCol;
    // current row (grid row)
    property Row;
    // first visible row (grid row)
    property TopRow;
    // this event is fired when a bookmark is added/modifed/removed
    property OnBookmarkChanged: TNotifyEvent read FOnBookmarkChanged write
      FOnBookmarkChanged;
    // call this procedure to navigate to a bookmarked position
    function GotoBookmark(const Index: integer): boolean;
    // call this function if the external offset formatting changed (see @link(OnGetOffsetText))
    procedure UpdateGetOffsetText;
    // center the current position vertically
    procedure CenterCursorPosition;
  end;

  // published hex editor component
  TMPHexEditor = class(TCustomMPHexEditor)
  published
    // @exclude(inherited)
    property Align;
    // @exclude(inherited)
    property Anchors;
    // @exclude(inherited)
    property BiDiMode;
    // @exclude(inherited)
    property BorderStyle;
    // @exclude(inherited)
    property Constraints;
    // @exclude(inherited)
    property Ctl3D;
    // @exclude(inherited)
    property DragCursor;
    // @exclude(inherited)
    property DragKind;
    // @exclude(inherited)
    property DragMode;
    // @exclude(inherited)
    property Enabled;
    // @exclude(inherited)
    property Font;
    // @exclude(inherited)
    property ImeMode;
    // @exclude(inherited)
    property ImeName;
    // @exclude(inherited)
    property OnClick;
    // @exclude(inherited)
    property OnDblClick;
    // @exclude(inherited)
    property OnDragDrop;
    // @exclude(inherited)
    property OnDragOver;
    // @exclude(inherited)
    property OnEndDock;
    // @exclude(inherited)
    property OnEndDrag;
    // @exclude(inherited)
    property OnEnter;
    // @exclude(inherited)
    property OnExit;
    // @exclude(inherited)
    property OnKeyDown;
    // @exclude(inherited)
    property OnKeyPress;
    // @exclude(inherited)
    property OnKeyUp;
    // @exclude(inherited)
    property OnMouseDown;
    // @exclude(inherited)
    property OnMouseMove;
    // @exclude(inherited)
    property OnMouseUp;
    // @exclude(inherited)
    property OnMouseWheel;
    // @exclude(inherited)
    property OnMouseWheelDown;
    // @exclude(inherited)
    property OnMouseWheelUp;
    // @exclude(inherited)
    property OnStartDock;
    // @exclude(inherited)
    property OnStartDrag;
    // @exclude(inherited)
    property ParentBiDiMode;
    // @exclude(inherited)
    property ParentCtl3D;
    // @exclude(inherited)
    property ParentFont;
    // @exclude(inherited)
    property ParentShowHint;
    // @exclude(inherited)
    property PopupMenu;
    // @exclude(inherited)
    property ScrollBars;
    // @exclude(inherited)
    property ShowHint;
    // @exclude(inherited)
    property TabOrder;
    // @exclude(inherited)
    property TabStop;
    // @exclude(inherited)
    property Visible;
    // see inherited @inherited
    property BytesPerRow;
    // see inherited @inherited
    property BytesPerColumn;
    // see inherited @inherited
    property Translation;
    // see inherited @inherited
    property OffsetFormat;
    // see inherited @inherited
    property CaretKind;
    // see inherited @inherited
    property Colors;
    // see inherited @inherited
    property FocusFrame;
    // see inherited @inherited
    property SwapNibbles;
    // see inherited @inherited
    property MaskChar;
    // see inherited @inherited
    property NoSizeChange;
    // see inherited @inherited
    property AllowInsertMode;
    // see inherited @inherited
    property DrawGridLines;
    // see inherited @inherited
    property WantTabs;
    // see inherited @inherited
    property ReadOnlyView;
    // see inherited @inherited
    property HideSelection;
    // see inherited @inherited
    property GraySelectionIfNotFocused;
    // see inherited @inherited
    property GutterWidth;
    // see inherited @inherited
    property BookmarkBitmap;

    // see inherited @inherited
    property Version;

    // see inherited @inherited
    property MaxUndo;
    // see inherited @inherited
    property InsertMode;
    // see inherited @inherited
    property HexLowerCase;
    // see inherited @inherited
    property OnProgress;
    // see inherited @inherited
    property OnInvalidKey;
    // see inherited @inherited
    property OnTopLeftChanged;
    // see inherited @inherited
    property OnChange;
    // see inherited @inherited
    property DrawGutter3D;
    // see inherited @inherited
    property ShowRuler;
    // see inherited @inherited
    property BytesPerUnit;
    // see inherited @inherited
    property RulerBytesPerUnit;
    // see inherited @inherited
    property ShowPositionIfNotFocused;
    // see inherited @inherited
    property OnSelectionChanged;
    // see inherited @inherited
    property UnicodeChars;
    // see inherited @inherited
    property UnicodeBigEndian;

    // see inherited @inherited
    property OnDrawCell;

    // see inherited @inherited
    property OnBookmarkChanged;
    // see inherited @inherited
    property OnGetOffsetText;
    // see inherited @inherited
    property BytesPerBlock;
    // see inherited @inherited
    property SeparateBlocksInCharField;
    // see inherited @inherited
    property FindProgress;
    // see inherited @inherited
    property RulerNumberBase;
  end;

  // @exclude(undo storage record)
  PMPHUndoRec = ^TMPHUndoRec;
  // @exclude(undo storage record)
  TMPHUndoRec = packed record
    DataLen: integer;
    Flags: TMPHUndoFlags;
    CurPos: integer;
    Pos, Count, ReplCount: cardinal;
    CurTranslation: TMPHTranslationKind;
    CurBPU: Integer;
    Buffer: byte;
  end;

  // @exclude(implements undo/redo)
  TMPHUndoStorage = class(TMemoryStream)
  private
    FCount,
      FUpdateCount: integer;
    FEditor: TCustomMPHexEditor;
    FDescription: string;
    FRedoPointer,
      FLastUndo: PMPHUndoRec;
    FLastUndoSize: integer;
    FLastUndoDesc: string;
    procedure SetCount(const Value: integer);
    procedure ResetRedo;
    procedure CreateRedo(const Rec: TMPHUndoRec);
    function GetUndoKind(const Flags: TMPHUndoFlags): TMPHUndoFlag;
    procedure AddSelection(const APos, ACount: integer);
    function ReadUndoRecord(var aUR: TMPHUndoRec; var SDescription: string):
      TMPHUndoFlag;
    function GetLastUndoKind: TMPHUndoFlag;
  public
    constructor Create(AEditor: TCustomMPHexEditor);
    destructor Destroy; override;
    procedure SetSize(NewSize: longint); override;
    procedure CreateUndo(aKind: TMPHUndoFlag; APosition, ACount, AReplaceCount:
      integer; const SDescription: string = '');
    function CanUndo: boolean;
    function CanRedo: boolean;
    function Redo: boolean;
    function Undo: boolean;
    function BeginUpdate: integer;
    function EndUpdate: integer;
    procedure Reset(AResetRedo: boolean = True);
    procedure RemoveLastUndo;
    property Count: integer read FCount write SetCount;
    property UpdateCount: integer read FUpdateCount;
    property Description: string read FDescription;
    property UndoKind: TMPHUndoFlag read GetLastUndoKind;
  end;

resourcestring

  // long descriptive names of character translations
  // tkAsIs
  MPH_TK_ASIS = 'Windows';
  // tkDos8
  MPH_TK_DOS8 = 'Dos 8 bits';
  // tkASCII
  MPH_TK_ASCII7 = 'ASCII 7 bits';
  // tkMac
  MPH_TK_MAC = 'Macintosh';
  // tkBCD
  MPH_TK_BCD38 = 'EBCDIC codepage 38';

  // unicode
  MPH_UC = 'Unicode little endian';
  // unicode be
  MPH_UC_BE = 'Unicode big endian';

  // short names (e.g. for status bars) of character translations
  // tkAsIs
  MPH_TK_ASIS_S = 'WIN';
  // tkDos8
  MPH_TK_DOS8_S = 'DOS';
  // tkASCII
  MPH_TK_ASCII7_S = 'ASC';
  // tkMac
  MPH_TK_MAC_S = 'MAC';
  // tkBCD
  MPH_TK_BCD38_S = 'BCD';

  // tkCustom
  MPH_TK_CUSTOM_S = 'Cust';
  // tkCustom
  MPH_TK_CUSTOM = 'Custom translation';

  // unicode
  MPH_UC_S = 'UCLE';
  // unicode be
  MPH_UC_BE_S = 'UCBE';

const
  // long descriptions of the different translations (e.g. for menues)
  MPHTranslationDesc: array[TMPHTranslationKind] of string = (MPH_TK_ASIS,
    MPH_TK_DOS8, MPH_TK_ASCII7, MPH_TK_MAC,
    MPH_TK_BCD38,
    MPH_TK_CUSTOM);

  // short descriptions of the different translations (e.g. for status bars)
  MPHTranslationDescShort: array[TMPHTranslationKind] of string =
  (MPH_TK_ASIS_S, MPH_TK_DOS8_S, MPH_TK_ASCII7_S, MPH_TK_MAC_S,
    MPH_TK_BCD38_S, MPH_TK_CUSTOM_S);

// public utility functions

(* translate a hexadecimal data representation ("a000 cc45 d3 42"...) to binary data
 (see @link(SwapNibbles) for the meaning of the SwapNibbles value)
*)
function ConvertHexToBin(aFrom, aTo: PChar; const aCount: integer; const
  SwapNibbles: boolean; var BytesTranslated: integer): PChar;

(* translate binary data to its hex representation (see @link(ConvertHexToBin)),
   (see @link(SwapNibbles) for the meaning of the SwapNibbles value)
*)
function ConvertBinToHex(aFrom, aTo: PChar; const aCount: integer; const
  SwapNibbles: boolean): PChar;

// convert X and Y into a TGridCoord record
function GridCoord(aX, aY: longint): TGridCoord;
// check whether the given key (VK_...) is currently down
function IsKeyDown(aKey: integer): boolean;

// get a unique filename in the temporary directory
function GetTempName: string;

(* translate an integer to a radix (base) coded string, e.g.<br>
  - IntToRadix(100,16) converts into a hexadecimal (number) string<br>
  - IntToRadix(100,2) converts into a string consisting only of 0 and 1<br>
  - IntToRadix(100,8) means IntToOctal<br>
  <br>
  hint: Radix must be in the range of 2..16*)
function IntToRadix(Value: integer; Radix: byte): string;
function IntToRadix64(Value: int64; Radix: byte): string;
// translate an integer to a radix coded string and left fill with 0 (see also @link(IntToRadix))
function IntToRadixLen(Value: integer; Radix, Len: byte): string;
function IntToRadixLen64(Value: int64; Radix, Len: byte): string;
// translate an integer to an octal string (see also @link(IntToRadix))
function IntToOctal(const Value: integer): string;

(* translate a radix coded number string into an integer, e.g.<br>
  - RadixToInt('0f', 16) => 15<br>
  - RadixToInt('755', 8) => 493
*)
function RadixToInt(Value: string; Radix: byte): integer;
function RadixToInt64(Value: string; Radix: byte): int64;

(* 64 bit unsigned integer arithmetics *)

// division of two unsigned int64 values, may raise an exception on error
function DivideU64(const Dividend, Divisor: int64): int64;
// division of two unsigned int64 values, returns false if an error occurred
function TryDivideU64(const Dividend, Divisor: int64;
  var Val: int64): boolean;
// modulo of two unsigned int64 values, may raise an exception on error
function ModuloU64(const Dividend, Divisor: int64): int64;
// modulo of two unsigned int64 values, returns false if an error occurred
function TryModuloU64(const Dividend, Divisor: int64;
  var Val: int64): boolean;
// multiplication of two unsigned int64 values, may raise an exception on error
function MultiplyU64(const Multiplier, Multiplicator: int64): int64;
// multiplication of two unsigned int64 values, returns false if an error occurred
function TryMultiplyU64(const Multiplier, Multiplicator: int64;
  var Val: int64): boolean;
// addition of two unsigned int64 values, may raise an exception on error
function AddU64(const Addend1, Addend2: int64): int64;
// addition of two unsigned int64 values, returns false if an error occurred
function TryAddU64(const Addend1, Addend2: int64;
  var Val: int64): boolean;
// subtraction of two unsigned int64 values, may raise an exception on error
function SubtractU64(const Minuend, Subtrahend: int64): int64;
// subtraction of two unsigned int64 values, returns false if an error occurred
function TrySubtractU64(const Minuend, Subtrahend: int64;
  var Val: int64): boolean;

(* try to find the correct radix (based on prefix/suffix) and return the number, known
   prefixes/suffixes are:<br>
   0x&lt;number&gt;, 0X&lt;number&gt;, $&lt;number&gt;, &lt;number&gt;h, &lt;number&gt;H: radix 16<br>
   o&lt;number&gt;, O&lt;number&gt;, 0&lt;number&gt;, &lt;number&gt;o, &lt;number&gt;O: radix 8<br>
   %&lt;number&gt;, &lt;number&gt;%: radix 2<br>
   otherwise: radix 10
*)
function CheckRadixToInt(Value: string): integer;
function CheckRadixToInt64(Value: string): int64;

// translate an number string built on radix 8 into an integer (see also @link(RadixToInt))
function OctalToInt(const Value: string): integer;

// swap lo and high byte of a widechar
procedure SwapWideChar(var WChar: WideChar);

// @exclude(fade a color to a gray value)
function FadeToGray(aColor: TColor): TColor;

(* translate data from Ansi to a different character set (see also @link(TMPHTranslationKind))<br>
  - TType: translate to this character set<br>
  - aBuffer: pointer to source data<br>
  - bBuffer: pointer to target data, must be allocated (may equal to aBuffer)<br>
  - aCount: number of bytes to translate
*)
procedure TranslateBufferFromAnsi(const TType: TMPHTranslationKind; aBuffer,
  bBuffer: PChar; const aCount: integer);
// translate data from a different character set to Ansi (see also @link(TranslateBufferFromAnsi))
procedure TranslateBufferToAnsi(const TType: TMPHTranslationKind; aBuffer,
  bBuffer: PChar; const aCount: integer);

// compatibility
{$IFNDEF DELPHI6UP}
procedure RaiseLastOSError;
{$ENDIF}

// returns the lower of the two numbers
function Min(a1, a2: integer): integer;
// returns the higer of the two numbers
function Max(a1, a2: integer): integer;

var
  (* translation tables for tkCustom *)

  // this character conversion is used in translations from tkAsIs to tkCustom (see @link(TMPHTranslationKind))
  MPHCustomCharConv: TMPHCharConv;

const
  (* standard offset formats *)

  // standard offset format: hex, auto min width, prefixed by 0x
  MPHOffsetHex = '-!10:0x|';
  // standard offset format: decimal
  MPHOffsetDec = 'a:|';
  // standard offset format: octal, suffixed by a small "o"
  MPHOffsetOct = '0!8:o|';

implementation

uses
  Consts, {$IFDEF DELPHI6UP}RTLConsts, {$ENDIF}ImgList, StdCtrls, SysConst;

const
  MPH_VERSION = 'February 06, 2006; © markus stephany, vcl[at]mirkes[dot]de';

resourcestring

  // undo descriptions
  UNDO_BYTESCHANGED = 'Change byte(s)';
  UNDO_REMOVED = 'Remove data';
  UNDO_INSERT = 'Insert buffer';
  UNDO_REPLACE = 'Replace';
  UNDO_APPEND = 'Append buffer';
  UNDO_INSNIBBLE = 'Insert nibble';
  UNDO_DELNIBBLE = 'Delete nibble';
  UNDO_CONVERT = 'Convert';
  UNDO_SELECTION = 'Cursor movement';
  UNDO_COMBINED = 'Multiple modification';
  UNDO_ALLDATA = 'All data saved';
  UNDO_NOUNDO = 'No undo';

  // error messages
  ERR_FILE_OPEN_FAILED = 'Cannot open %s.'#13#10'(%s.)';
  ERR_FILE_READONLY = 'Cannot save readonly file %s.';
  ERR_INVALID_BOOKMARK = 'Invalid bookmark index';
  ERR_INVALID_SELSTART = 'Invalid selection start';
  ERR_INVALID_SELEND = 'Invalid selection end';
  ERR_INVALID_BYTESPERLINE = 'Invalid bytes per line argument';
  ERR_INVALID_BUFFERFROMFILE = 'Invalid buffer from file argument';
  ERR_INVALID_BYTESPERCOL = 'Invalid bytes per column argument';
  ERR_INVALID_BOOKMARKBMP = 'Invalid bookmark bitmap (must be 10 x 200 px)';
  ERR_CANCELLED = 'Operation cancelled';
  ERR_MISSING_FORMATCHAR = 'Missing char in offset format: %s';
  ERR_INVALID_FORMATRADIX =
    'Invalid radix in offset format (%xh), allowed: 02h..10h';
  ERR_INVALID_RADIXCHAR =
    'Invalid character %s, cannot convert using radix %xh';
  ERR_INVALID_BPU = 'Invalid bytes per unit value %d, allowed: 1,2,4,8';
  ERR_INVALID_BPU_U = 'BytesPerUnit must be set to 2 in unicode mode';
  ERR_INVALID_RBPU =
    'Invalid ruler bytes per unit value %d, allowed: -1,1,2,4,8';
  ERR_DATA_BOUNDS = 'Data position/length out of data bounds';
  ERR_NO_TRANSLATION_IN_UNICODE_MODE =
    'Translations cannot be used in unicode mode';
  ERR_ODD_FILESIZE_UNICODE = 'Cannot use unicode mode with odd-sized files';

  ERR_FIXED_FILESIZE = 'Cannot change fixed filesize';
  ERR_NOUNDO = 'Cannot update undo storage';

  // new, empty file
  UNNAMED_FILE = 'Untitled';

const
  // fixed cols/rows
  GRID_FIXED = 2;

  // available undo descriptions
  STRS_UNDODESC: array[ufKindBytesChanged..ufKindAllData] of string =
  (UNDO_BYTESCHANGED, UNDO_REMOVED, UNDO_INSERT, UNDO_REPLACE, UNDO_APPEND,
    UNDO_INSNIBBLE, UNDO_DELNIBBLE, UNDO_CONVERT, UNDO_SELECTION, UNDO_COMBINED,
    UNDO_ALLDATA);

  // valid hex characters
  HEX_LOWER = '0123456789abcdef';
  HEX_UPPER = '0123456789ABCDEF';
  HEX_ALLCHARS = HEX_LOWER + HEX_UPPER;

{$IFNDEF DELPHI6UP}

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}

// invert the given color

function Invert(Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $00FFFFFF;
end;

// translate the buffer from ANSI to the given translation mode

procedure TranslateBufferFromAnsi(const TType: TMPHTranslationKind; aBuffer,
  bBuffer: PChar; const aCount: integer);
var
  LIntLoop: integer;
begin
  case TType of
    // changed 04/18/04: bBuffer and aBuffer were interchanged!
    tkAsIs: Move(aBuffer^, bBuffer^, aCount);
    tkDOS8,
      tkASCII: CharToOEMBuff(aBuffer, bBuffer, aCount);
    tkMAC: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            MPH_CCONV_MAC[cctFromAnsi][Ord(aBuffer[LIntLoop])];
    tkBCD: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            MPH_CCONV_BCD38[cctFromAnsi][Ord(aBuffer[LIntLoop])];

    tkCustom: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            MPHCustomCharConv[cctFromAnsi][Ord(aBuffer[LIntLoop])];

  end;
end;

// translate the buffer to ANSI from the given translation mode

procedure TranslateBufferToAnsi(const TType: TMPHTranslationKind; aBuffer,
  bBuffer: PChar; const aCount: integer);
var
  LIntLoop: integer;
begin
  case TType of
    tkAsIs: Move(aBuffer^, bBuffer^, aCount);
    tkDOS8,
      tkASCII: OEMToCharBuff(aBuffer, bBuffer, aCount);
    tkMAC: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] := MPH_CCONV_MAC[cctToAnsi][Ord(aBuffer[LIntLoop])];
    tkBCD: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            MPH_CCONV_BCD38[cctToAnsi][Ord(aBuffer[LIntLoop])];

    tkCustom: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            MPHCustomCharConv[cctToAnsi][Ord(aBuffer[LIntLoop])];

  end;
end;

// ansi to oem

function OEM2Char(aByte: byte): char;
var
  LszBuf: array[0..1] of char;
begin
  LszBuf[0] := char(aByte);
  LszBuf[1] := #0;
  OEMToChar(LSzBuf, LSzBuf);
  Result := LSzBuf[0];
end;

// oem to ansi

function Char2OEM(aByte: byte): char;
var
  LszBuf: array[0..1] of char;
begin
  LszBuf[0] := char(aByte);
  LszBuf[1] := #0;
  CharToOEM(LSzBuf, LSzBuf);
  Result := LSzBuf[0];
end;

(* helper functions *)

// get a temporary file name

function GetTempName: string;

var
  LStrTemp: string;
begin
  SetLength(LStrTemp, MAX_PATH + 1);
  SetLength(LStrTemp, GetTempPath(MAX_PATH, @LStrTemp[1]));
  LStrTemp := Trim(LStrTemp);
{$IFDEF DELPHI6UP}
  LstrTemp := IncludeTrailingPathDelimiter(LstrTemp);
{$ELSE}
  if LStrTemp[Length(LStrTemp)] <> '\' then
    LStrTemp := LStrTemp + '\';
{$ENDIF}
  repeat
    Result := LStrTemp + IntToHex(GetTickCount, 8) + '.MPHT';
  until GetFileAttributes(PChar(Result)) = $FFFFFFFF;

end;

// can the file be opened for reading (possibly read only) ?

function CanOpenFile(const aName: TFileName; var ReadOnly: boolean): boolean;
var
  LHdlFile: THandle;
begin
  Result := False;
  ReadOnly := True;
  LHdlFile := FileOpen(aName, fmOpenRead or fmShareDenyNone);
  if LHdlFile <> INVALID_HANDLE_VALUE then
  begin
    FileClose(LHdlFile);
    Result := True;
    try
      LHdlFile := FileOpen(aName, fmOpenReadWrite);
      if LHdlFile <> INVALID_HANDLE_VALUE then
      begin
        FileClose(LHdlFile);
        ReadOnly := False;
      end;
    except
      Result := True;
      ReadOnly := True;
    end;
  end;
end;

// is that key pressed ?

function IsKeyDown(aKey: integer): boolean;
begin
  Result := (GetKeyState(aKey) and (not 1)) <> 0;
end;

// return the lesser value

function Min(a1, a2: integer): integer;
begin
  if a1 < a2 then
    Result := a1
  else
    Result := a2;
end;

// return the bigger value

function Max(a1, a2: integer): integer;
begin
  if a1 > a2 then
    Result := a1
  else
    Result := a2;
end;

// cast x,y to grid coord

function GridCoord(aX, aY: longint): TGridCoord;
begin
  Result.x := aX;
  Result.y := aY;
end;

// convert '00 01 02...' to binary data

function ConvertHexToBin(aFrom, aTo: PChar; const aCount: integer;
  const SwapNibbles: boolean; var BytesTranslated: integer): PChar;
var
  LBoolHi: boolean;
  LIntLoop: integer;
  LBytCurrent: byte;
  LChrCurrent: char;
begin
  Result := aTo;
  BytesTranslated := 0;
  LBoolHi := True;
  LBytCurrent := 0;
  for LIntLoop := 0 to Pred(aCount) do
    if Pos(aFrom[LIntLoop], HEX_ALLCHARS) <> 0 then
    begin
      LChrCurrent := UpCase(aFrom[LIntLoop]);
      if LBoolHi then
        LBytCurrent := ((Pos(LChrCurrent, HEX_UPPER) - 1) * 16)
      else
        LBytCurrent := LBytCurrent or ((Pos(LChrCurrent, HEX_UPPER) - 1));

      LBoolHi := not LBoolHi;
      if LBoolHi then
      begin
        if SwapNibbles then
          aTo[BytesTranslated] := char(((LBytCurrent and 15) * 16) or
            ((LBytCurrent and $F0) shr 4))
        else
          aTo[BytesTranslated] := char(LBytCurrent);

        Inc(BytesTranslated);
      end;
    end;
end;

// convert binary data to '00 01 02...'

function ConvertBinToHex(aFrom, aTo: PChar; const aCount: integer;
  const SwapNibbles: boolean): PChar;
var
  LIntLoop: integer;
  LByteCurrent: byte;
  LIntLoop2: integer;
begin
  Result := aTo;
  LIntLoop2 := 0;
  for LIntLoop := 0 to Pred(aCount) do
  begin
    LByteCurrent := Ord(aFrom[LIntLoop]);
    if SwapNibbles then
    begin
      aTo[LIntLoop2] := UpCase(HEX_UPPER[(LByteCurrent and 15) + 1]);
      aTo[LIntLoop2 + 1] := UpCase(HEX_UPPER[(LByteCurrent shr 4) + 1])
    end
    else
    begin
      aTo[LIntLoop2 + 1] := UpCase(HEX_UPPER[(LByteCurrent and 15) + 1]);
      aTo[LIntLoop2] := UpCase(HEX_UPPER[(LByteCurrent shr 4) + 1])
    end;

    Inc(LIntLoop2, 2);
  end;
  aTO[LIntLoop2] := #0;
end;

// translate an integer to a radix coded string

function IntToRadix(Value: integer; Radix: byte): string;
begin
  Result := IntToRadixLen(Value, Radix, 0);
end;

function IntToRadix64(Value: int64; Radix: byte): string;
begin
  Result := IntToRadixLen64(Value, Radix, 0);
end;

// translate an integer to a radix coded string and left fill with 0

function IntToRadixLen(Value: integer; Radix, Len: byte): string;
var
  LCrdTemp: cardinal absolute Value;
begin
  Result := '';
  repeat
    Result := HEX_UPPER[(LCrdTemp mod Radix) + 1] + Result;
    LCrdTemp := LCrdTemp div Radix;
  until LCrdTemp = 0;
  while Length(Result) < Len do
    Result := '0' + Result;
end;

// unsigned 64 bit integer routines (division and modulo)
// this code is derived from assembler code written by
// Norbert Juffa, found on "the assembly gems page"
// (http://www.df.lth.se/~john_e/)

procedure _UModDiv64;
begin
  asm
    // divisor > 2^32-1 ?
    test ecx, ecx

    // yes, divisor > 32^32-1
    jnz @big_divisor

    // only one division needed ? (ecx = 0)
    cmp edx, ebx

    // yes, one division sufficient
    jb @one_div

    // save dividend-lo in ecx
    mov ecx, eax

    // get dividend-hi
    mov eax, edx

    // zero extend it into edx:eax
    xor edx, edx

    // quotient-hi in eax
    div ebx

    // ecx = quotient-hi, eax =dividend-lo
    xchg eax, ecx

@one_div:

    // eax = quotient-lo
    div ebx

    //ebx = remainder-lo
    mov ebx, edx

    //edx = quotient-hi(quotient in edx:eax)
    mov edx, ecx

    // ecx = remainder-hi (rem. in ecx:ebx)
    xor ecx, ecx
    jmp @cleanup;

@big_divisor:

    //  save dividend
    push edx
    push eax

    // divisor now in edi:ebx and ecx:esi
    mov esi, ebx
    mov edi, ecx

    // shift both divisor and and dividend right by 1 bit
    shr edx, 1
    rcr eax, 1
    ror edi, 1
    rcr ebx, 1

    // ecx = number of remaining shifts
    bsr ecx, ecx

    // scale down divisor and dividend such that divisor less than 2^32 (i.e. fits in ebx)
    shrd ebx, edi, CL
    shrd eax, edx, CL
    shr edx, CL

    //  restore original divisor (edi:esi)
    rol edi, 1

    // compute quotient
    div ebx

    // get dividend lo-word
    pop ebx

    // save quotient
    mov ecx, eax

    // quotient * divisor hi-word (low only)
    imul edi, eax

    // quotient * divisor lo-word
    mul esi

    // edx:eax = quotient * divisor
    add edx, edi

    // dividend-lo - (quot.*divisor)-lo
    sub ebx, eax

    // get quotient
    mov eax, ecx

    // restore dividend hi-word
    pop ecx

    // subtract divisor * quot. from dividend
    sbb ecx, edx

    // 0 if remainder > 0, else FFFFFFFFh
    sbb edx, edx

    // nothing to add
    and esi, edx

    // back if remainder positive
    and edi, edx

    // correct remaider and quotient if necessary
    add ebx, esi
    adc ecx, edi
    add eax, edx

    // clear hi-word of quot (eax<=FFFFFFFFh)
    xor edx, edx

@cleanup:
  end;
end;

{$WARNINGS OFF}

function UDiv64(I1, I2: Int64): int64;
begin
  asm
    // save registers
    push ebp
    push ebx
    push esi
    push edi

    // load I2 into ebx/ecx
    mov ebx, [ebp+$08];
    mov ecx, [ebp+$0c];

    // load I1 into eax/edx
    mov eax, [ebp+$10];
    mov edx, [ebp+$14];

    call _UModDiv64

    // store result (division result is in eax:edx)
    mov [ebp-$08], eax;
    mov [ebp-$04], edx;

    // restore registers
    pop edi
    pop esi
    pop ebx
    pop ebp
  end;
end;

function UMod64(I1, I2: Int64): int64;
begin
  asm
    // save registers
    push ebp
    push ebx
    push esi
    push edi

    // load I2 into ebx/ecx
    mov ebx, [ebp+$08];
    mov ecx, [ebp+$0c];

    // load I1 into eax/edx
    mov eax, [ebp+$10];
    mov edx, [ebp+$14];

    call _UModDiv64

    // store result (division remainder is in ebx:ecx)
    mov [ebp-$08], ebx;
    mov [ebp-$04], ecx;

    // restore registers
    pop edi
    pop esi
    pop ebx
    pop ebp
  end;
end;
{$WARNINGS ON}

(* 64 bit unsigned integer arithmetics *)

function DivideU64(const Dividend, Divisor: int64): int64;
begin
  Result := UDiv64(Dividend, Divisor);
end;

function TryDivideU64(const Dividend, Divisor: int64;
  var Val: int64): boolean;
begin
  Result := True;
  try
    Val := UDiv64(Dividend, Divisor);
  except
    Result := False;
  end;
end;

function ModuloU64(const Dividend, Divisor: int64): int64;
begin
  Result := UMod64(Dividend, Divisor);
end;

function TryModuloU64(const Dividend, Divisor: int64;
  var Val: int64): boolean;
begin
  Result := True;
  try
    Val := UMod64(Dividend, Divisor);
  except
    Result := False;
  end;
end;

// unsigned 64 bit integer routines (multiplication, addition, substraction)
// this code is derived from assembler code found in the online book
// "Art of Assembly Programming" maintained by Randall Hyde
// (http://webster.cs.ucr.edu/)

function TryMultiplyU64(const Multiplier, Multiplicator: int64;
  var Val: int64): boolean;
asm
  // save registers
  push ebx
  push esi

  mov byte ptr result, 1

  // store val pointer
  mov esi, eax

  // multiply lo dword of multiplier * lo dword of multiplicator
  mov eax, dword ptr Multiplier
  mul dword ptr Multiplicator

  // save lo dword
  mov dword [esi], eax

  // save hi dword of partial product
  mov ecx, edx

  // multiply lo dword of multiplier * hi dword of multiplicator
  mov eax, dword ptr Multiplier
  mul dword ptr Multiplicator+4

  // add to the partial product (including carry)
  add eax, ecx
  adc edx, 0

  // save partial product
  mov ebx, eax
  mov ecx, edx

  // multiply hi dword of multiplier * lo dword of multiplicator
  mov eax, dword ptr Multiplier+4
  mul dword ptr Multiplicator

  // add the partial product
  add eax, ebx

  // save the partial product
  mov dword ptr [esi+4], eax

  // add in the carry flag
  adc ecx, edx

  // save carry
  pushfd

  // multiply hi dword of multiplier * hi dword of multiplicator
  mov eax, dword ptr Multiplier+4
  mul dword ptr Multiplicator+4

  // load carry
  popfd

  // add partial product + carry
  adc eax, ecx
  adc edx, 0

  // check overflow
  test eax, eax
  jnz @over
  test edx, edx
  jz @finish

@over:
  // overflow
  mov byte ptr result, 0

@finish:
  // restore register
  pop esi
  pop ebx
end;

function MultiplyU64(const Multiplier, Multiplicator: int64): int64;
begin
  if not TryMultiplyU64(Multiplier, Multiplicator, Result) then
    raise EIntOverflow.Create(SIntOverflow);
end;

function TryAddU64(const Addend1, Addend2: int64;
  var Val: int64): boolean;
asm
  mov byte ptr result, 1

  // store val pointer
  mov edx, eax

  // add lo dwords
  mov eax, dword ptr Addend1
  add eax, dword ptr Addend2

  // store lo dword
  mov dword ptr [edx], eax

  // add hi dwords + carry
  mov eax, dword ptr Addend1+4
  adc eax, dword ptr Addend2+4

  // store hi dword
  mov dword ptr [edx+4], eax

  // check carry
  jnc @finish
  mov byte ptr result, 0
@finish:
end;

function AddU64(const Addend1, Addend2: int64): int64;
begin
  if not TryAddU64(Addend1, Addend2, Result) then
    raise EIntOverflow.Create(SIntOverflow);
end;

function TrySubtractU64(const Minuend, Subtrahend: int64;
  var Val: int64): boolean;
asm
  mov byte ptr result, 1

  // store val pointer
  mov edx, eax

  // subtract lo dwords
  mov eax, dword ptr Minuend
  sub eax, dword ptr Subtrahend

  // store lo dword
  mov dword ptr [edx], eax

  // subtract hi dwords - carry
  mov eax, dword ptr Minuend+4
  sbb eax, dword ptr Subtrahend+4

  // store hi dword
  mov dword ptr [edx+4], eax

  // check carry
  jnc @finish
  mov byte ptr result, 0
@finish:
end;

function SubtractU64(const Minuend, Subtrahend: int64): int64;
begin
  if not TrySubtractU64(Minuend, Subtrahend, Result) then
    raise EIntOverflow.Create(SIntOverflow);
end;

function IntToRadixLen64(Value: int64; Radix, Len: byte): string;
begin
  Result := '';
  repeat
    Result := HEX_UPPER[UMod64(Value, Radix) + 1] + Result;
    Value := UDiv64(Value, Radix);
  until Value = 0;
  while Length(Result) < Len do
    Result := '0' + Result;
end;

// translate an integer value to an octal string

function IntToOctal(const Value: integer): string;
begin
  Result := IntToRadix(Value, 8);
end;

// translate a radix coded string into an integer

function RadixToInt(Value: string; Radix: byte): integer;
var
  LCrdTemp: cardinal absolute Result;
begin
  LCrdTemp := 0;
  Value := UpperCase(Value);
  while Value <> '' do
  begin
    if not (Pos(Value[1], HEX_UPPER) in [1..Radix]) then
      raise EMPHexEditor.CreateFmt(ERR_INVALID_RADIXCHAR, [Value[1], Radix]);
    LCrdTemp := LCrdTemp * Radix + cardinal(Pos(Value[1], HEX_UPPER) - 1);
    Delete(Value, 1, 1);
  end;
end;

function RadixToInt64(Value: string; Radix: byte): int64;
begin
  Result := 0;
  Value := UpperCase(Value);
  while Value <> '' do
  begin
    if not (Pos(Value[1], HEX_UPPER) in [1..Radix]) then
      raise EMPHexEditor.CreateFmt(ERR_INVALID_RADIXCHAR, [Value[1], Radix]);
    Result := Result * Radix + cardinal(Pos(Value[1], HEX_UPPER) - 1);
    Delete(Value, 1, 1);
  end;
end;

(* try to find the correct radix (based on prefix/suffix) and return the number, known
   prefixes/suffixes are:<br>
   0x<number>, 0X<number>, $<number>, <number>h, <number>H: radix 16<br>
   o<number>, O<number>, <number>o, <number>O: radix 8<br>
   %<number>, <number>%: radix 2<br>
   otherwise: radix 10
*)

function CheckRadixToInt(Value: string): integer;
begin
  // hex
  if UpperCase(Copy(Value, 1, 2)) = '0X' then
    Result := RadixToInt(Copy(Value, 3, MaxInt), 16)
  else if Copy(Value, 1, 1) = '$' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 16)
  else if UpperCase(Copy(Value, Length(Value), 1)) = 'H' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 16)
  else {// octal} if UpperCase(Copy(Value, Length(Value), 1)) = 'O' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 8)
  else if UpperCase(Copy(Value, 1, 1)) = 'O' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 8)
      (* removed, is ambigous else if (Copy(Value, 1, 1) = '0') and (AllCharsIn(['0'..'7'])) then
  Result := RadixToInt(Value, 8)*)
  else {// binary} if UpperCase(Copy(Value, Length(Value), 1)) = '%' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 2)
  else if UpperCase(Copy(Value, 1, 1)) = '%' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 2)
  else
    // decimal
    Result := StrToInt(Value);
end;

function CheckRadixToInt64(Value: string): int64;
begin
  // hex
  if UpperCase(Copy(Value, 1, 2)) = '0X' then
    Result := RadixToInt64(Copy(Value, 3, MaxInt), 16)
  else if Copy(Value, 1, 1) = '$' then
    Result := RadixToInt64(Copy(Value, 2, MaxInt), 16)
  else if UpperCase(Copy(Value, Length(Value), 1)) = 'H' then
    Result := RadixToInt64(Copy(Value, 1, Length(Value) - 1), 16)
  else {// octal} if UpperCase(Copy(Value, Length(Value), 1)) = 'O' then
    Result := RadixToInt64(Copy(Value, 1, Length(Value) - 1), 8)
  else if UpperCase(Copy(Value, 1, 1)) = 'O' then
    Result := RadixToInt64(Copy(Value, 2, MaxInt), 8)
      (* removed, is ambigous else if (Copy(Value, 1, 1) = '0') and (AllCharsIn(['0'..'7'])) then
  Result := RadixToInt(Value, 8)*)
  else {// binary} if UpperCase(Copy(Value, Length(Value), 1)) = '%' then
    Result := RadixToInt64(Copy(Value, 1, Length(Value) - 1), 2)
  else if UpperCase(Copy(Value, 1, 1)) = '%' then
    Result := RadixToInt64(Copy(Value, 2, MaxInt), 2)
  else
    // decimal
    Result := StrToInt64(Value)
end;

// translate an octal to an integer

function OctalToInt(const Value: string): integer;
begin
  Result := RadixToInt(Value, 8);
end;

// swap lo and high byte of a widechar

procedure SwapWideChar(var WChar: WideChar);
var
  LWrdChar: word absolute WChar;
begin
  LWrdChar := Swap(LWrdChar);
end;

// fade a color to a gray value

function FadeToGray(aColor: TColor): TColor;
var
  LBytGray: byte;
begin
  aColor := ColorToRGB(aColor);
  LBytGray := HiByte(GetRValue(aColor) * 74 + GetGValue(aColor) * 146 +
    GetBValue(aColor) * 36);
  Result := RGB(LBytGray, LBytGray, LBytGray);
end;

(* TCustomMPHexEditor *)

constructor TCustomMPHexEditor.Create(aOwner: TComponent);
var
  LIntLoop: integer;
begin
  inherited Create(aOwner);
{$IFDEF FASTACCESS}
  FSetDataSizeFillByte := 0;
{$ENDIF}
  FMaskedChars := [#0..#31];
  FRulerNumberBase := 16;
  FOffsetHandler := False;
  FOnFind := nil;
  FOnWildcardFind := nil;
  FFindProgress := False;
  FBlockSize := -1;
  FSepCharBlocks := True;
  FUnicodeCharacters := False;
  FUnicodeBigEndian := False;
  FSelectionChangedCount := 0;
  FBytesPerUnit := 1;
  FRulerBytesPerUnit := -1;
  FUsedRulerBytesPerUnit := 1;
  FShowPositionIfNotFocused := False;
  FShowRuler := False;
  FDrawGutter3D := True;
  FHexLowerCase := True;
  SetHexLowerCase(False);
  DoubleBuffered := True;
  FBookmarkBitmap := TBitmap.Create;
  FCursorList := nil;
  FHasCustomBMP := False;
  FStreamFileName := '';
  FHasFile := False;
  FMaxUndo := 1024 * 1024;
  FPosInCharField := False;
  FLastPosInCharField := True;

  FGutterWidth := -1;
  GenerateOffsetFormat(MPHOffsetHex);
  FSelectionPossible := True;
  FBookmarkImageList := TImageList.Create(self);
  FBookmarkImageList.DrawingStyle := dsTransparent;
  FBookmarkImageList.BkColor := clBlack;
  FBookmarkImageList.Width := 10;
  FBookmarkImageList.Height := 10;

  Options := [goThumbTracking];
  DesignOptionsBoost := [];
  DefaultDrawing := False;
  FSaveCellExtents := False;

  FColors := TMPHColors.Create(Self);
  FDrawGridLines := False;

  ParentColor := False;
  FDataStorage := TMPHMemoryStream.Create;
  FUndoStorage := TMPHUndoStorage.Create(self);

  Color := FColors.Background;

  FCharWidth := -1;
  FOffSetDisplayWidth := -1;
  FBytesPerRow := 16;
  FCaretKind := ckAuto;
  FFocusFrame := True;
  FSwapNibbles := 0;
  FFileName := '---';

  Font.Name := 'Courier New';
  Font.Size := 11;
  BorderStyle := bsSingle;
  FBytesPerCol := 4;
  CTL3D := False;
  Cursor := crIBeam;
  FModifiedBytes := TBits.Create;
  for LIntLoop := Low(FBookmarks) to High(FBookmarks) do
    FBookmarks[LIntLoop].mPosition := -1;
  SetSelection(-1, -1, -1);
  FIsSelecting := False;
  ResetUndo;
  DefaultColWidth := 0;
  DefaultRowHeight := 0;
  RowHeights[0] := 0;
  RowHeights[1] := 0;
  ColCount := CalcColCount;
  RowCount := GRID_FIXED + 1;
  FTranslation := tkAsIs;
  FModified := False;
  FIsFileReadonly := True;
  FBytesPerRowDup := 2 * FBytesPerRow;
  FIntLastHexCol := (GRID_FIXED + FBytesPerRowDup - 1);
  FReplaceUnprintableCharsBy := '.';
  FCaretBitmap := TBitmap.Create;
  FFixedFileSize := False;
  FFixedFileSizeOverride := False;
  FAllowInsertMode := True;
  FInsertModeOn := False;
  FWantTabs := True;
  FReadOnlyView := False;
  FHideSelection := False;
  FGraySelOnLostFocus := False;
  FOnProgress := nil;
  FShowDrag := False;
  FSelBeginPosition := -1;
  FBookmarkBitmap.OnChange := BookmarkBitmapChanged;
  FBookmarkBitmap.LoadFromResourceName(HINSTANCE, 'BOOKMARKICONS');
  SetRulerString;
{$IFDEF DELPHI7UP}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
{$ENDIF}
end;

destructor TCustomMPHexEditor.Destroy;
begin

  FCursorList := nil;
  FBookmarkBitmap.OnChange := nil;
  FreeStorage;
  FreeStorage(True);
  FUndoStorage.Free;
  FDataStorage.Free;
  FModifiedBytes.Free;
  FColors.Free;
  FCaretBitmap.Free;
  FBookmarkImageList.Free;
  FBookmarkBitmap.Free;
  inherited Destroy;
end;

procedure TCustomMPHexEditor.AdjustMetrics;
var
  LIntLoop: integer;
  LIntChWidth: integer;
begin
  Canvas.Font.Assign(Font);
  FCharWidth := Canvas.TextWidth('w');

  SetOffsetDisplayWidth;
  DoSetCellWidth(1, 6);

  for LIntLoop := 0 to FBytesPerRowDup do
  begin
    if LIntLoop = Pred(FBytesPerRowDup) then
      LIntChWidth := FCharWidth * 2
    else
    begin
      LIntChWidth := FCharWidth;
      if (((LIntLoop + GRID_FIXED) mod FBytesPerCol) = 1) then
        Inc(LIntChWidth, FCharWidth);
      if (FBlockSize > 1) and (((LIntLoop + GRID_FIXED) mod (FBlockSize * 2)) =
        1) then
        Inc(LIntChWidth, FCharWidth);
    end;
    DoSetCellWidth(LIntLoop + GRID_FIXED, LIntChWidth);
  end;

  if FUnicodeCharacters then
    LIntLoop := Pred(FBytesPerRow div 2)
  else
    LIntLoop := Pred(FBytesPerRow);
  for LIntLoop := 0 to LIntLoop do
    //FBytesPerRowDup + 1 to (FBytesPerRow * 3) - 1 do
  begin
    if (FUsedRulerBytesPerUnit > 1) and ((LIntLoop mod FUsedRulerBytesPerUnit)
      = Pred(FUsedRulerBytesPerUnit)) and (not FUnicodeCharacters) then
      LIntChWidth := (FCharWidth * 3 div 2) + 1
    else
      LIntChWidth := FCharWidth + 1;
    if not FUnicodeCharacters then
    begin
      if (FBlockSize > 1) and FSepCharBlocks and ((LIntLoop mod FBlockSize) =
        Pred(FBlockSize)) then
        Inc(LIntChWidth, FCharWidth);
    end
    else
    begin
      if (FBlockSize > 1) and FSepCharBlocks and ((LIntLoop mod (FBlockSize div
        2)) = Pred(FBlockSize div 2)) then
        Inc(LIntChWidth, FCharWidth);
    end;
    DoSetCellWidth(LIntLoop + GRID_FIXED + FBytesPerRowDup + 1, LIntChWidth);
  end;

  DoSetCellWidth(GetLastCharCol, (FCharWidth * 2) + 1);

  FCharHeight := Canvas.TextHeight('yY') + 2;
  DefaultRowHeight := FCharHeight;
  RowHeights[1] := 0;
  if FShowRuler then
    RowHeights[0] := DefaultRowHeight + 3
  else
    RowHeights[0] := 0;
  CheckSetCaret;
end;

function TCustomMPHexEditor.GetDataSize: integer;
begin
  Result := FDataStorage.Size;
end;

procedure TCustomMPHexEditor.CreateEmptyFile;
begin
  FreeStorage;
  if TempName = '' then
    FFileName := UNNAMED_FILE
  else
    FFileName := TempName;
  ResetUndo;
  ResetSelection(False);
  FModifiedBytes.Size := 0;
  CalcSizes;
  FModified := False;
  FIsFileReadonly := True;
  FHasFile := False;
  MoveColRow(GRID_FIXED, GRID_FIXED, True, True);
  Changed;
end;

procedure TCustomMPHexEditor.SaveToStream(Strm: TStream);
begin
  WaitCursor;
  try
    FDataStorage.Position := 0;

    Stream2Stream(FDataStorage, Strm, pkSave);
  finally
    Invalidate;
    OldCursor;
  end;
end;

procedure TCustomMPHexEditor.SaveRangeToStream(Strm: TStream; const APosition,
  ACount: integer);
begin
  WaitCursor;
  try
    FDataStorage.Position := APosition;
    Stream2Stream(FDataStorage, Strm, pkSave, ACount);
  finally
    Invalidate;
    OldCursor;
  end;
end;

procedure TCustomMPHexEditor.SaveToFile(const Filename: string;
  const aUnModify: boolean = True);
var
  LfstFile: TFileStream;
begin
  if (FFileName = FileName) then
    PrepareOverwriteDiskFile;

  LfstFile := TFileStream.Create(FileName, fmCreate);
  try
    FStreamFileName := FileName;
    SaveToStream(LfstFile);
    FHasFile := True;
    if aUnModify then
    begin
      FModifiedBytes.Size := 0;
      FModified := False;
      FIsFileReadonly := False;
      FFileName := Filename;
      FDataStorage.Position := 0;
      ResetUndo;
    end;
  finally
    FStreamFileName := '';
    LfstFile.Free;
  end;
end;

procedure TCustomMPHexEditor.LoadFromStream(Strm: TStream);
begin
  try
    FreeStorage;
    CalcSizes;
    WaitCursor;
    try
      try
        Strm.Position := 0;
        FDataStorage.Size := Strm.Size;
        FDataStorage.Position := 0;

        Stream2Stream(Strm, FDataStorage, pkLoad);
        //FDataStorage.CopyFrom(Strm, Strm.Size - Strm.Position);

        FDataStorage.Position := 0;
      finally
        with FUndoStorage do
          if UpdateCount < 1 then
            Reset;
        FModifiedBytes.Size := 0;
        CalcSizes;
        FModified := False;
        FIsSelecting := False;
        MoveColRow(GRID_FIXED, GRID_FIXED, True, True);
        Changed;
      end;
    finally
      OldCursor;
    end;
  except
    FreeStorage;
    FreeStorage(True);
    FHasFile := False;
    raise;
  end;
end;

procedure TCustomMPHexEditor.LoadFromFile(const Filename: string);
var
  LfstFile: TFileStream;
begin
  if CanOpenFile(FileName, FIsFileReadonly) then
  begin
    LfstFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FStreamFileName := FileName;
      try
        LoadFromStream(LfstFile);
      except
        FHasFile := False;
        raise;
      end;
      FFileName := FileName;
      FHasFile := True;
    finally
      FStreamFileName := '';
      LfstFile.Free;
    end;
  end
  else
    raise EFOpenError.CreateFmt(ERR_FILE_OPEN_FAILED, [FileName,
      SysErrorMessage(GetLastError)]);
end;

procedure TCustomMPHexEditor.CalcSizes;
var
  LIntRows: integer;
begin
  if FModifiedBytes.Size > DataSize then
    FModifiedBytes.Size := DataSize;

  if DataSize < 1 then
  begin
    RowCount := GRID_FIXED + 1;
    ColCount := CalcColCount;
    FixedCols := GRID_FIXED;
  end
  else
  begin
    LIntRows := (DataSize + (FBytesPerRow - 1)) div FBytesPerRow;
    if ((DataSize mod FBytesPerRow) = 0) and InsertMode then
      INC(LIntRows);
    RowCount := LIntRows + GRID_FIXED;

    ColCount := CalcColCount;
    FixedCols := GRID_FIXED;
  end;
  FixedRows := GRID_FIXED;
  AdjustMetrics;
end;

function TCustomMPHexEditor.TranslateFromAnsiChar(const aByte: byte): char;
begin
  case FTranslation of
    tkAsIs: Result := char(aByte);
    tkDos8,
      tkASCII:
      begin
        if ((FTranslation = tkDos8) or (aByte < 128)) and (aByte > 31) then
          Result := Char2Oem(aByte)
        else
          Result := #0;
      end;
    tkMac: Result := MPH_CCONV_MAC[cctFromAnsi][aByte];
    tkBCD: Result := MPH_CCONV_BCD38[cctFromAnsi][aByte];

    tkCustom: Result := MPHCustomCharConv[cctFromAnsi][aByte];

  else
    Result := #0;
  end;
  if Result in FMaskedChars then
    Result := #0;
end;

function TCustomMPHexEditor.TranslateToAnsiChar(const aByte: byte): char;
begin
  case FTranslation of
    tkAsIs: Result := char(aByte);
    tkDos8,
      tkASCII:
      begin
        Result := Oem2Char(aByte);
        if ((FTranslation = tkASCII) and (aByte > 127)) then
          Result := FReplaceUnprintableCharsBy;
      end;
    tkMac: Result := MPH_CCONV_MAC[cctToAnsi][aByte];
    tkBCD: Result := MPH_CCONV_BCD38[cctToAnsi][aByte];

    tkCustom: Result := MPHCustomCharConv[cctToAnsi][aByte];

  else
    Result := FReplaceUnprintableCharsBy;
  end;

  if (FReplaceUnprintableCharsBy <> #0) and (Result in FMaskedChars) then
    Result := FReplaceUnprintableCharsBy;
end;

// get the position of the drag marker

function TCustomMPHexEditor.DropPosition: integer;
var
  LBoolInCharField: boolean;
begin
  Result := -1;
  LBoolInCharField := FPosInCharField;
  try
    if FShowDrag then
    begin
      Result := GetPosAtCursor(FDropCol, FDropRow);
      CheckUnit(Result);
    end;
  finally
    FPosInCharField := LBoolInCharField;
  end;
end;

procedure TCustomMPHexEditor.Stream2Stream(strFrom, strTo: TStream;
  const Operation: TMPHProgressKind; const Count: integer = -1);
var
  LBytProgress, LBytLastProgress: byte;
  LIntRemain, LIntRead, LIntCount: integer;
  LBoolCancel: boolean;
  LStrFile: string;

  LBytBuffer: array[0..MPH_FILEIO_BLOCKSIZE - 1] of byte;
begin
  LIntCount := Count;
  if LIntCount = -1 then
    LIntCount := strFrom.Size - strFrom.Position;

  LIntRemain := LIntCount;
  LBoolCancel := False;
  LBytLastProgress := 255;
  LStrFile := FStreamFileName;
  if LStrFile = '' then
    LStrFile := FFileName;

  while LIntRemain > 0 do
  begin
    LBytProgress := Round(((LIntCount - LIntRemain) / LIntCount) * 100);
    if (LBytProgress <> LBytLastProgress) or (LIntRemain <=
      MPH_FILEIO_BLOCKSIZE) then
    begin
      if LIntRemain <= MPH_FILEIO_BLOCKSIZE then
        LBytLastProgress := 100
      else
        LBytLastProgress := LBytProgress;
      if Assigned(FOnProgress) then
      begin
        FOnProgress(self, Operation, LStrFile, LBytLastProgress,
          LBoolCancel);
        if LBoolCancel then
          raise EMPHexEditor.Create(ERR_CANCELLED);
      end
    end;

    LIntRead := Min(LIntRemain, MPH_FILEIO_BLOCKSIZE);
    strFrom.ReadBuffer(LBytBuffer, LIntRead);
    strTo.WriteBuffer(LBytBuffer, LIntRead);
    Dec(LIntRemain, LIntRead);
  end;
end;

function TCustomMPHexEditor.SelectCell(ACol, ARow: longint): boolean;
var
  LIntCurRow: integer;
  LRctCellRect: TRect;
  LIntOtherFieldCol: integer;
  LIntNewPosition, LIntPrevPosition: integer;
begin
  LIntCurRow := Row;
  if DataSize > 0 then
    Result := CheckSelectCell(aCol, aRow)
  else
  begin
    if not ((aCol = GRID_FIXED) or (aCol = Max(GetOtherFieldColCheck(GRID_FIXED)
      , GRID_FIXED)) and (aRow = GRID_FIXED)) then
      Result := False
    else
    begin
      LRctCellRect := CellRect(aCol, aRow);
      if LRctCellRect.Left + LRctCellRect.Bottom = 0 then
        IntSetCaretPos(-50, -50, -1)
      else
        IntSetCaretPos(LRctCellRect.Left, LRctCellRect.Top, aCol);
      Result := True;
      Exit;
    end;
  end;

  if Result then
  begin
    //cursor an alter stelle löschen
    if (aCol <> Col) or (aRow <> Row) then
    begin
      LIntOtherFieldCol := GetOtherFieldColCheck(Col);
      if not FPosInCharField then
        LRctCellRect := CellRect(LIntOtherFieldCol, LIntCurRow)
      else
      begin
        if FUnicodeCharacters then
          LRctCellRect := BoxRect(LIntOtherFieldCol, LIntCurRow,
            LIntOtherFieldCol+3, LIntCurRow)
        else
          LRctCellRect := BoxRect(LIntOtherFieldCol, LIntCurRow,
            LIntOtherFieldCol+1, LIntCurRow)
      end;
      InvalidateRect(Handle, @LRctCellRect, False);
      if FShowRuler and (aCol <> Col) then
      begin
        LRctCellRect := CellRect(LIntOtherFieldCol, 0);
        InvalidateRect(Handle, @LRctCellRect, False);
        LRctCellRect := CellRect(Col, 0);
        InvalidateRect(Handle, @LRctCellRect, False);
      end;

      // cursor an neuer stelle setzen
      LIntOtherFieldCol := GetOtherFieldColCheck(aCol);
      if not FPosInCharField then
        LRctCellRect := CellRect(LIntOtherFieldCol, aRow)
      else
      begin
        if FUnicodeCharacters then
          LRctCellRect := BoxRect(LIntOtherFieldCol, aRow,
            LIntOtherFieldCol+3, aRow)
        else
          LRctCellRect := BoxRect(LIntOtherFieldCol, aRow,
            LIntOtherFieldCol+1, aRow)
      end;
      InvalidateRect(Handle, @LRctCellRect, False);
      if FShowRuler and (aCol <> Col) then
      begin
        LRctCellRect := CellRect(LIntOtherFieldCol, 0);
        InvalidateRect(Handle, @LRctCellRect, False);
        LRctCellRect := CellRect(aCol, 0);
        InvalidateRect(Handle, @LRctCellRect, False);
      end;

      if LIntCurRow <> aRow then
      begin
        LRctCellRect := CellRect(0, LIntCurRow);
        InvalidateRect(Handle, @LRctCellRect, False);
        LRctCellRect := CellRect(0, aRow);
        InvalidateRect(Handle, @LRctCellRect, False);
      end;
    end;

    if FIsSelecting then
    begin
      LIntNewPosition := GetPosAtCursor(aCol, aRow);
      LIntPrevPosition := GetPosAtCursor(Col, Row);
      if FSelBeginPosition = -1 then
        FSelBeginPosition := LIntPrevPosition;
      if not InsertMode then
      begin
        CheckSelectUnit(FSelBeginPosition, LIntNewPosition);
        NewSelection(FSelBeginPosition, LIntNewPosition);
      end
      else
      begin
        if FSelBeginPosition > LIntNewPosition then
        begin
          CheckUnit(FSelBeginPosition);
          CheckUnit(LIntNewPosition);
          if FSelBeginPosition = LIntNewPosition then
          begin
            ResetSelection(True);
            FSelBeginPosition := LIntNewPosition;
            FIsSelecting := True;
          end
          else
          begin
            NewSelection(FSelBeginPosition - FBytesPerUnit, LIntNewPosition);
          end;
        end
        else if FSelBeginPosition < LIntNewPosition then
        begin
          CheckUnit(FSelBeginPosition);
          CheckUnit(LIntNewPosition);
          if FSelBeginPosition = LIntNewPosition then
          begin
            ResetSelection(True);
            FSelBeginPosition := LIntNewPosition;
            FIsSelecting := True;
          end
          else
          begin
            NewSelection(FSelBeginPosition, LIntNewPosition - FBytesPerUnit);
          end;
        end
        else
        begin
          ResetSelection(True);
          FSelBeginPosition := LIntNewPosition;
          FIsSelecting := True;
        end
      end;
    end
    else
      ResetSelection(True);

    // caret neu setzen
    //CheckSetCaret;
    LRctCellRect := CellRect(aCol, aRow);
    if LRctCellRect.Left + LRctCellRect.Bottom = 0 then
      IntSetCaretPos(-50, -50, -1)
    else
      IntSetCaretPos(LRctCellRect.Left, LRctCellRect.Top, aCol);
    SelectionChanged;
  end;
end;

// Obtient la position dans le fichier à partir de la position du curseur

function TCustomMPHexEditor.GetPosAtCursor(const aCol, aRow: integer): integer;
begin
  FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
  if FPosInCharField then
  begin
    Result := aCol - ((GRID_FIXED + 1) + FBytesPerRowDup);
    if FUnicodeCharacters then
      Result := Result * 2;
  end
  else
    Result := (aCol - GRID_FIXED) div 2;

  Result := Result + ((aRow - GRID_FIXED) * FBytesPerRow);
  if Result < 0 then
    Result := 0;
end;

function TCustomMPHexEditor.GetRow(const DataPos: integer): integer;
begin
  Result := (DataPos div FBytesPerRow) + GRID_FIXED;
end;

function TCustomMPHexEditor.GetCursorAtPos(const aPos: integer;
  const aChars: boolean): TGridCoord;
var
  LIntCol: integer;
begin
  if aPos < 0 then
  begin
    Result.y := GRID_FIXED;
    Result.x := GRID_FIXED;
    Exit;
  end;

  Result.y := GetRow(aPos);
  LIntCol := aPos mod FBytesPerRow;

  if aChars then
  begin
    if FUnicodeCharacters then
      Result.x := (LIntCol div 2) + (GRID_FIXED + 1) + FBytesPerRowDup
    else
      Result.x := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup)
  end
  else
    Result.x := (LIntCol * 2) + GRID_FIXED;
end;

function TCustomMPHexEditor.GetOtherFieldCol(const aCol: integer): integer;
var
  LIntCol: integer;
begin
  FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
  if FPosInCharField then
  begin
    LIntCol := (aCol - (GRID_FIXED + 1 + FBytesPerRowDup));
    if FUnicodeCharacters then
      Result := (LIntCol * 4) + GRID_FIXED
    else
      Result := (LIntCol * 2) + GRID_FIXED;
  end
  else
  begin
    if FUnicodeCharacters then
      LIntCol := ((aCol - GRID_FIXED) div 4)
    else
      LIntCol := ((aCol - GRID_FIXED) div 2);
    Result := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup);
  end;
end;

function TCustomMPHexEditor.GetOtherFieldColCheck(const aCol: integer): integer;
var
  LIntCol: integer;
begin
  if aCol > (GRID_FIXED + FBytesPerRowDup) then
  begin
    LIntCol := (aCol - (GRID_FIXED + 1 + FBytesPerRowDup));
    if FUnicodeCharacters then
      Result := (LIntCol * 4) + GRID_FIXED
    else
      Result := (LIntCol * 2) + GRID_FIXED;
  end
  else
  begin
    if FUnicodeCharacters then
      LIntCol := ((aCol - GRID_FIXED) div 4)
    else
      LIntCol := ((aCol - GRID_FIXED) div 2);
    Result := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup);
  end;
end;

function TCustomMPHexEditor.CheckSelectCell(aCol, aRow: integer): boolean;
var
  LgrcEndCoords: TGridCoord;
  LIntPos: integer;
begin
  Result := inherited SelectCell(aCol, aRow);

  if not FSelectionPossible then
    Exit;

  try
    FSelectionPossible := False;

    if Result then
    begin
      // überprüfen, ob linke maustaste oder shift gedrückt, sonst selection zurücksetzen
      if not (IsKeyDown(VK_SHIFT) or IsKeyDown(VK_LBUTTON)) then
        ResetSelection(True);

      // überprüfen, ob außerhalb der DateiGröße
      LIntPos := GetPosAtCursor(aCol, aRow);
      if (LIntPos >= DataSize) and not (InsertMode and (LIntPos = DataSize) and
        (FPosInCharField or ((aCol mod 2) = 0))) then
      begin
        if (not InsertMode) then
          LgrcEndCoords := GetCursorAtPos(DataSize - 1, InCharField)
        else
          LgrcEndCoords := GetCursorAtPos(DataSize, InCharField);

        MoveColRow(LgrcEndCoords.x, LgrcEndCoords.y, True, True);
        Result := False;
      end
      else if aCol = (GRID_FIXED + FBytesPerRowDup) then
      begin
        Result := False;
        if IsKeyDown(VK_LBUTTON) then
        begin
          aCol := aCol - 1;
          aCol := Max(GRID_FIXED, aCol);
          MoveColRow(aCol, aRow, True, True);
          Exit;
        end;
      end;
    end;

  finally
    FSelectionPossible := True;
  end;
end;

procedure TCustomMPHexEditor.WMChar(var Msg: TWMChar);
var
  LIntPos: integer;
  LChrChar: char;
  LBytOldData, LBytNewData: byte;
  LArrNewData: packed array[0..7] of byte;
  LWChrNewData: WideChar absolute LArrNewData;
  LgrcPosition: TGridCoord;
  LWChrOldData: WideChar;
  LWrdKey: Word;
begin
  LChrChar := char(Msg.CharCode);

  if Assigned(OnKeyPress) then
    OnKeyPress(Self, LChrChar);

  if FReadOnlyView or (LChrChar in FMaskedChars) then
    Exit;

  LIntPos := GetPosAtCursor(Col, Row);
  if (LIntPos >= DataSize) and not InsertMode then
    Exit;

  if not FPosInCharField then
  begin
    // hex-eingabe, nur 0..9 , a..f erlaubt
    if Pos(LChrChar, HEX_ALLCHARS) <> 0 then
    begin
      LChrChar := UpCase(LChrChar);

      if not InsertMode then
        ResetSelection(True);

      LgrcPosition := GetCursorAtPos(LIntPos, FPosInCharField);
      // Obtient la valeur du byte dans le fichier (OldByte)
      if DataSize > LIntPos then
        LBytOldData := Data[LIntPos]
      else
        LBytOldData := 0;

      if (LgrcPosition.x = (Col - FSwapNibbles)) or (SelCount <> 0) then
        LBytNewData := LBytOldData and 15 + ((Pos(LChrChar, HEX_UPPER) - 1) * 16)
      else
        LBytNewData := (LBytOldData and $F0) + (Pos(LChrChar, HEX_UPPER) - 1);

      FillChar(LArrNewData, sizeof(LArrNewData), #0);
      if InsertMode and ((((Col - GRID_FIXED) mod (FBytesPerUnit * 2)) = 0) or
        (SelCount > 0)) then
      begin
        if FSwapNibbles = 0 then
          LBytNewData := LBytNewData and $F0
        else
          LBytNewData := LBytNewData and $0F;
        LArrNewData[0] := LBytNewData;

        if DataSize = 0 then
          AppendBuffer(PChar(@LArrNewData), FBytesPerUnit, '', False)
        else if SelCount = 0 then
        begin
          InsertBuffer(PChar(@LArrNewData), FBytesPerUnit, LIntPos, '', False);
        end
        else
          ReplaceSelection(PChar(@LArrNewData), FBytesPerUnit, '', False);
      end
      else
      begin
        if LIntPos >= DataSize then
          Exit;
        IntChangeByte(LBytOldData, LBytNewData, LIntPos, Col, Row);
      end;
      FIsSelecting := False;

      LWrdKey := VK_RIGHT;
      KeyDown(LWrdKey, []);
    end
    else
      WrongKey
  end
  else
  begin
    // zeichen-eingabe, alle zeichen erlaubt
    LChrChar := TranslateFromAnsiChar(Ord(LChrChar));

    if (LChrChar in FMaskedChars) then
    begin
      WrongKey;
      Exit;
    end;

    if not InsertMode then
      ResetSelection(True);

    LgrcPosition := GetCursorAtPos(LIntPos, FPosInCharField);

    FillChar(LArrNewData, sizeof(LArrNewData), #0);
    if not FUnicodeCharacters then
      LArrNewData[0] := Ord(LChrChar)
    else
    begin
      LWChrNewData := StringToWideChar(LChrChar, @LWChrNewData, 2)^;
      if FUnicodeBigEndian then
        SwapWideChar(LWChrNewData);
    end;
    if (DataSize = 0) or (DataSize = LIntPos) then
      LBytOldData := 0
    else
      LBytOldData := Data[LIntPos];
    if FUnicodeCharacters then
    begin
      if (DataSize = 0) or (DataSize = LIntPos) or (DataSize = (LIntPos + 1))
        then
        LWChrOldData := #0
      else
        ReadBuffer(LWChrOldData, LIntPos, 2);
    end;

    if InsertMode then
    begin
      if SelCount > 0 then
        ReplaceSelection(PChar(@LArrNewData), FBytesPerUnit, '', False)
      else
      begin
        if LIntPos = DataSize then
          AppendBuffer(PChar(@LArrNewData), FBytesPerUnit)
        else
        begin
          if (LIntPos mod FBytesPerUnit) = 0 then
            InsertBuffer(PChar(@LArrNewData), FBytesPerUnit, LIntPos, '', False)
          else
            IntChangeByte(LBytOldData, LArrNewData[0], LIntPos, Col, Row)
        end;
        FIsSelecting := False;
      end;
    end
    else
    begin
      if FUnicodeCharacters then
        IntChangeWideChar(LWChrOldData, LWChrNewData, LIntPos, Col, Row)
      else
        IntChangeByte(LBytOldData, Ord(LChrChar), LIntPos, Col, Row);
    end;

    LWrdKey := VK_RIGHT;
    KeyDown(LWrdKey, []);
  end;
end;

{-------------------------------------------------------------------------------}
// *** procedure TCustomMPHexEditor.IntChangeByte***
// Change la valeur du byte
// Renseigne la structure Undo
{-------------------------------------------------------------------------------}

procedure TCustomMPHexEditor.IntChangeByte(const aOldByte, aNewByte: byte; aPos,
  aCol, aRow: integer; const UndoDesc: string = '');
var
  LRctBoxRect: TRect;
  LIntOtherFieldCol: integer;
begin
  if aOldByte = aNewByte then
    Exit;

  CreateUndo(ufKindBytesChanged, aPos, 1, 0, UndoDesc);

  // Ecrit dans le fichier
  Data[aPos] := aNewByte;

  if not InsertMode then
    FModifiedBytes.Bits[aPos] := True;

  aCol := GetCursorAtPos(aPos, False).X;
  LIntOtherFieldCol := GetOtherFieldColCheck(aCol);
  LRctBoxRect := BoxRect(aCol, aRow, aCol + 1, aRow);
  InvalidateRect(Handle, @LRctBoxRect, False);
  LRctBoxRect := BoxRect(LIntOtherFieldCol, aRow, LIntOtherFieldCol, aRow);
  InvalidateRect(Handle, @LRctBoxRect, False);
  Changed;
end;

procedure TCustomMPHexEditor.IntChangeWideChar(const aOldChar, aNewChar:
  WideChar; aPos, aCol, aRow: integer; const UndoDesc: string);
var
  LRctBoxRect: TRect;
  LIntOtherFieldCol: integer;
  LBArrOld: packed array[0..1] of Byte absolute aOldChar;
  LBArrNew: packed array[0..1] of Byte absolute aNewChar;
begin
  if aOldChar = aNewChar then
    Exit;

  CreateUndo(ufKindBytesChanged, aPos, 2, 0, UndoDesc);

  // Ecrit dans le fichier
  WriteBuffer(aNewChar, aPos, 2);

  if not InsertMode then
  begin
    FModifiedBytes.Bits[aPos] := LBArrOld[0] <> LBArrNew[0];
    FModifiedBytes.Bits[aPos + 1] := LBArrOld[1] <> LBArrNew[1];
  end;

  aCol := GetCursorAtPos(aPos, False).X;
  LIntOtherFieldCol := GetOtherFieldColCheck(aCol);
  LRctBoxRect := BoxRect(aCol, aRow, aCol + 3, aRow);
  InvalidateRect(Handle, @LRctBoxRect, False);
  LRctBoxRect := BoxRect(LIntOtherFieldCol, aRow, LIntOtherFieldCol, aRow);
  InvalidateRect(Handle, @LRctBoxRect, False);
  Changed;
end;

procedure TCustomMPHexEditor.KeyDown(var Key: word; Shift: TShiftState);
var
  LIntCol: integer;
  LgrcPosition: TGridCoord;
  LIntRow: integer;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(self, Key, Shift);

  // reset selection if no shift key is pressed (except of SHIFT-Key)
  if not ((Shift <> []) or (KEY = VK_SHIFT)) then
    if not InsertMode then
      ResetSelection(True);

  case Key of

    VK_PRIOR:
      begin
        if ssCtrl in Shift then
        begin
          // go to the first visible line
          LIntRow := TopRow;
          LIntCol := Col;
          if LIntRow > -1 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end
        else
        begin
          // scroll up one page
          LIntRow := Max(GRID_FIXED, Row - VisibleRowCount + 1);
          TopRow := Max(GRID_FIXED, TopRow - VisibleRowCount + 1);
          LIntCol := Col;
          if LIntRow > -1 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end;
      end;

    VK_NEXT:
      begin
        if ssCtrl in Shift then
        begin
          // go to the Last visible line
          LIntRow := Min(RowCount - 1, TopRow + VisibleRowCount - 1);
          LIntCol := Col;
          if LIntRow > 0 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end
        else
        begin
          // scroll down one page
          LIntRow := Min(RowCount - 1, Row + VisibleRowCount - 1);
          TopRow := Min(Max(GRID_FIXED, RowCount - VisibleRowCount),
            TopRow + VisibleRowCount - 1);
          LIntCol := Col;
          if LIntRow > 0 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end;
      end;

    VK_HOME:
      begin
        InCharField;
        if (ssCtrl in Shift) then
        begin // strg+pos1
          if not FPosInCharField then
            MoveColRow(GRID_FIXED, GRID_FIXED, True, True)
          else
            MoveColRow(Max(GRID_FIXED, GetOtherFieldCol(GRID_FIXED)),
              GRID_FIXED, True, True);
        end
        else
        begin // normaler zeilenstart
          if not FPosInCharField then
            MoveColRow(GRID_FIXED, Row, True, True)
          else
            MoveColRow(Max(GRID_FIXED, GetOtherFieldCol(GRID_FIXED)),
              Row, True, True);
        end;
      end;

    VK_END:
      begin
        InCharField;
        if (ssCtrl in Shift) then
        begin // strg+end
          if (not InsertMode) then
            LgrcPosition := GetCursorAtPos(DataSize - 1, FPosInCharField)
          else
            LgrcPosition := GetCursorAtPos(DataSize, FPosInCharField);
          MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True)
        end
        else
        begin // normales zeilenende
          if not FPosInCharField then
          begin
            LIntCol := GetPosAtCursor(GRID_FIXED, Row + 1) - 1;
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x + 1, LgrcPosition.y, True, True)
          end
          else
          begin
            LIntCol := GetPosAtCursor(GRID_FIXED, Row + 1) - 1;
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, True);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
        end;
      end;

    VK_LEFT, VK_BACK:
      if (InsertMode and (not FReadOnlyView)) and (Key = VK_BACK) then
      begin
        if SelCount > 0 then
          DeleteSelection
        else
          InternalErase(True);
      end
      else if (not (ssCTRL in Shift)) then
      begin
        if FIsSelecting or (FUnicodeCharacters and FPosInCharField) then
          LIntCol := GetPosAtCursor(Col, Row) - FBytesPerUnit
        else
          LIntCol := GetPosAtCursor(Col, Row) - 1;
        if FPosInCharField then
        begin
          if LIntCol < 0 then
            LIntCol := 0;
          LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
          MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
        end
        else
        begin
          if FIsSelecting then
          begin
            CheckUnit(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
          else
          begin
            LIntCol := LIntCol + 1;
            LgrcPosition := GetCursorAtPos(LIntCol, False);
            if LgrcPosition.x < Col then
              MoveColRow(Col - 1, Row, True, True)
            else
            begin
              LIntCol := LIntCol - 1;
              if LIntCol >= 0 then
              begin
                LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
                MoveColRow(LgrcPosition.x + 1, LgrcPosition.y, True, True);
              end;
            end
          end;
        end;
      end
      else
      begin
        if Key = VK_LEFT then
        begin
          LIntCol := GRID_FIXED;
          MoveColRow(LIntCol, Row, True, True);
        end;
      end;

    VK_RIGHT:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          if FIsSelecting or (FUnicodeCharacters and FPosInCharField) then
            LIntCol := GetPosAtCursor(Col, Row) + FBytesPerUnit
          else
            LIntCol := GetPosAtCursor(Col, Row) + 1;
          if FPosInCharField then
          begin
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
          else
          begin
            if FIsSelecting then
            begin
              CheckUnit(LIntCol);
              TruncMaxPosition(LIntCol);
              LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
              MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
            end
            else
            begin
              LIntCol := LIntCol - 1;
              LgrcPosition := GetCursorAtPos(LIntCol, False);
              if (LgrcPosition.x = Col) and not (LIntCol = DataSize) then
                MoveColRow(Col + 1, Row, True, True)
              else
              begin
                LIntCol := LIntCol + 1;
                if (LIntCol < DataSize) or ((LIntCol = DataSize) and InsertMode)
                  then
                begin
                  LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
                  MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
                end;
              end
            end;
          end;
        end
        else
        begin
          LIntCol := GetLastCharCol;
          MoveColRow(LIntCol, Row, True, True);
        end;
      end;

    VK_DOWN:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          LIntRow := Row + 1;

          LIntCol := Col;
          if LIntRow < RowCount then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end
          end;
      end;

    VK_UP:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          LIntRow := Row - 1;
          LIntCol := Col;
          if LIntRow >= GRID_FIXED then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end
          end;
      end;

    Word('T'): if (ssCtrl in Shift) then
      begin
        Col := Max(GRID_FIXED, GetOtherFieldCol(Col));
      end;

    VK_TAB: if ((Shift = []) or (Shift = [ssShift])) then
      begin // tab-taste
        Col := Max(GRID_FIXED, GetOtherFieldCol(Col));
      end;

    Word('0')..Word('9'): if ssCtrl in Shift then
      begin
        if ssShift in Shift then
        begin
          LIntRow := GetPosAtCursor(Col, Row);
          SetBookmarkVals(Key - Ord('0'), LIntRow, FPosInCharField);
        end
        else
        begin
          GotoBookmark(Key - Ord('0'));
        end;
      end;

    VK_SHIFT: if (Shift = [ssShift]) or (Shift = [ssShift, ssCtrl]) then
      begin // selektion starten
        FIsSelecting := True;
      end;

    VK_DELETE: if (not FReadOnlyView) then
      begin
        if (SelCount > 0) and (InsertMode or (Shift = [ssCtrl])) then
          DeleteSelection
        else if InsertMode or (Shift = [ssCtrl]) then
          InternalErase(False)
      end;

    VK_INSERT: if (Shift = []) then InsertMode := not InsertMode;
  end;
end;

function TCustomMPHexEditor.HasChanged(aPos: integer): boolean;
begin
  Result := False;
  if InsertMode then
    Exit;

  if FModifiedBytes.Size > aPos then
    Result := FModifiedBytes.Bits[aPos];
end;

function TCustomMPHexEditor.IsSelected(const APosition: integer): boolean;
begin
  Result := False;
  if (FSelPosition <> -1) and (APosition >= FSelStart) and (APosition <= FSelEnd)
    then
  begin
    Result := True
  end;
end;

procedure TCustomMPHexEditor.NewSelection(SelFrom, SelTo: integer);
var
  LIntSelStart, LIntSelEnd, LIntSelPos: integer;
  LIntOldStart, LIntNewStart, LIntOldEnd, LIntNewEnd: integer;
begin
  CheckSelectUnit(SelFrom, SelTo);
  LIntSelEnd := FSelEnd;
  LIntSelStart := FSelStart;
  LIntSelPos := FSelPosition;

  SetSelection(SelFrom, Min(SelFrom, SelTo), Max(SelFrom, SelTo));

  if (LIntSelPos = -1) then
    RedrawPos(Min(FSelStart, FSelEnd), Max(FSelStart, FSelEnd))
  else
  begin
    // den bereich neu zeichnen, der neu selektiert ist, sowie den, der nicht mehr selektiert ist
    // hinzugekommene selektion berechnen
    LIntNewStart := Min(SelFrom, SelTo);
    LIntOldStart := Min(LIntSelEnd, LIntSelStart);
    LIntNewEnd := Max(SelFrom, SelTo);
    LIntOldEnd := Max(LIntSelEnd, LIntSelStart);
    RedrawPos(Min(LIntNewStart, LIntOldStart), Max(LIntNewStart, LIntOldStart));
    RedrawPos(Min(LIntOldEnd, LIntNewEnd), Max(LIntOldEnd, LIntNewEnd));
  end;
  SelectionChanged;
end;

function TCustomMPHexEditor.GetOffsetFormat: string;
begin
  Result := FOffsetFormat.Format;
end;

procedure TCustomMPHexEditor.SetOffsetFormat(const Value: string);
begin
  if Value <> FOffsetFormat.Format then
  try
    GenerateOffsetFormat(Value);
    SetOffsetDisplayWidth;
    Invalidate;
  except
    GenerateOffsetFormat(FOffsetFormat.Format);
    raise;
  end;
end;

procedure TCustomMPHexEditor.SetHexLowerCase(const Value: boolean);
begin
  if FHexLowerCase <> Value then
  begin
    FHexLowerCase := Value;
    if Value then
      Move(HEX_LOWER[1], FHexChars, sizeof(FHexChars))
    else
      Move(HEX_UPPER[1], FHexChars, sizeof(FHexChars));
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.GenerateOffsetFormat(Value: string);
var
  LIntTemp: integer;
  LStrTemp: string;
begin
  with FOffsetFormat do
  begin
    Flags := [];
    LStrTemp := Value;
    // aufbau: [r|c|<HEXNUM>%][-|<HEXNUM>!]<HEXNUM>:[Prefix]|[Suffix]
    if LStrTemp <> '' then
    begin
      // bytes per unit
      if UpperCase(Copy(LStrTemp, 1, 2)) = 'R%' then
      begin
        Flags := Flags + [offCalcRow];
        Delete(LStrTemp, 1, 2);
        _BytesPerUnit := BytesPerRow;
      end
      else if UpperCase(Copy(LStrTemp, 1, 2)) = 'C%' then
      begin
        Flags := Flags + [offCalcColumn];
        Delete(LStrTemp, 1, 2);
        _BytesPerUnit := BytesPerColumn;
      end
      else
      begin
        LIntTemp := 1;
        while (LIntTemp <= Length(LStrTemp)) and
          (LStrTemp[LIntTemp] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(LIntTemp);
        if Copy(LStrTemp, LIntTemp, 1) = '%' then
        begin
          // width field
          if LIntTemp = 1 then
          begin
            Flags := Flags + [offBytesPerUnit];
            _BytesPerUnit := FUsedRulerBytesPerUnit;
            Delete(LStrTemp, 1, 1)
          end
          else
          begin
            _BytesPerUnit := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
            //  StrToInt('$'+Copy(LStrTemp, 1, LIntTemp-1));
            Delete(LStrTemp, 1, LIntTemp);
          end;
        end
        else
        begin
          Flags := Flags + [offBytesPerUnit];
          _BytesPerUnit := FUsedRulerBytesPerUnit;
        end;
      end;
      if not (_BytesPerUnit in [1, 2, 4, 8]) then
        raise EMPHexEditor.CreateFmt(ERR_INVALID_BPU, [_BytesPerUnit]);
      // auto calc width
      if Copy(LStrTemp, 1, 2) = '-!' then
      begin
        Flags := Flags + [offCalcWidth];
        Delete(LStrTemp, 1, 2);
        MinWidth := 1;
      end
      else
      begin
        // width ?
        LIntTemp := 1;
        while (LIntTemp <= Length(LStrTemp)) and
          (LStrTemp[LIntTemp] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(LIntTemp);
        if Copy(LStrTemp, LIntTemp, 1) = '!' then
        begin
          // width field
          if LIntTemp = 1 then
          begin
            MinWidth := 1;
            Delete(LStrTemp, 1, 1)
          end
          else
          begin
            MinWidth := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
            //  StrToInt('$'+Copy(LStrTemp, 1, LIntTemp-1));
            Delete(LStrTemp, 1, LIntTemp);
          end;
        end
        else
          MinWidth := 1;
      end;

      // radix
      LIntTemp := 1;
      while (LIntTemp <= Length(LStrTemp)) and (LStrTemp[LIntTemp] in ['0'..'9',
        'A'..'F', 'a'..'f']) do
        Inc(LIntTemp);

      if LIntTemp = 1 then
        raise EMPHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, ['number radix']);

      if Copy(LStrTemp, LIntTemp, 1) <> ':' then
        raise EMPHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, [':']);

      Radix := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
      if not (Radix in [2..16]) then
        raise EMPHexEditor.CreateFmt(ERR_INVALID_FORMATRADIX, [Radix]);

      Delete(LStrTemp, 1, LIntTemp);

      // prefix, suffix
      LIntTemp := Pos('|', LStrTemp);
      if LIntTemp = 0 then
        raise EMPHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, ['|']);

      Prefix := Copy(LStrTemp, 1, LIntTemp - 1);
      Suffix := Copy(LStrTemp, LIntTemp + 1, MaxInt);
    end;
    Format := Value;
  end;
end;

procedure TCustomMPHexEditor.Select(const aCurCol, aCurRow, aNewCol, aNewRow:
  integer);
var
  LIntOldStart,
    //LIntOldEnd,
  LIntNewStart,
    LIntNewEnd: integer;
begin
  //LIntOldEnd := FSelEnd;
  //LIntOldStart := FSelStart;
  LIntNewStart := GetPosAtCursor(aNewCol, aNewRow);
  if FSelPosition = -1 then
  begin
    LIntOldStart := LIntNewStart;
    //LIntOldEnd := LIntNewStart;
    LIntNewEnd := GetPosAtCursor(aCurCol, aCurRow);
    NewSelection(LIntNewEnd, LIntOldStart); // abcd
    //SetSelection(LIntNewEnd, Min(LIntOldStart, LIntNewEnd), Max(LIntNewEnd,
      //LIntOldEnd));
    //RedrawPos(FSelStart, FSelEnd)
  end
  else
    //begin
    NewSelection(FSelPosition, LIntNewStart); // abcd
  (*// testen, ob neue selection  /\ liegt als fSelPO
  // wenn ja, dann start = sel, ende = selpo
  if LIntNewStart < FSelPosition then
  begin
    NewSelection(FSelPosition, LIntNewStart);// abcd
    //SetSelection(FSelPosition, LIntNewStart, FSelPosition);
    //RedrawPos(Min(FSelStart, LIntOldStart), Max(FSelStart, LIntOldStart));
    //RedrawPos(Min(FSelEnd, LIntOldEnd), Max(FSelEnd, LIntOldEnd));
  end
  else
  begin
    NewSelection(FSelPosition, LIntNewStart); //abcd
    //SetSelection(FSelPosition, FSelPosition, LIntNewStart);
    //RedrawPos(Min(FSelStart, LIntOldStart), Max(FSelStart, LIntOldStart));
    //RedrawPos(Min(FSelEnd, LIntOldEnd), Max(FSelEnd, LIntOldEnd));
  end;
end;*)
end;

procedure TCustomMPHexEditor.RedrawPos(aFrom, aTo: integer);
var
  LRctBox: TRect;
begin
  aFrom := GetRow(aFrom);
  aTo := GetRow(aTo);
  LRctBox := BoxRect(GRID_FIXED, aFrom, GetLastCharCol, aTo);
  InvalidateRect(Handle, @LRctBox, False);
end;

procedure TCustomMPHexEditor.ResetSelection(const aDraw: boolean);
var
  LIntOldStart,
    LIntOldEnd: integer;
begin
  FIsSelecting := False;
  LIntOldStart := FSelStart;
  LIntOldEnd := FSelEnd;
  SetSelection(-1, -1, -1);
  FSelBeginPosition := -1;

  if aDraw and ((LIntOldStart > -1) or (LIntOldStart > -1)) then
    RedrawPos(LIntOldStart, LIntOldEnd);
end;

procedure TCustomMPHexEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  LgrcDummy: TGridCoord;
  lboolInherited: boolean;
begin
  FIsSelecting := False;
  FMouseUpCanResetSel := False;

  if Button = mbLeft then
    LgrcDummy := CheckMouseCoord(X, Y);

  // do not change selection when clicking ruler or offset panel.
  if (not MouseOverSelection) and (not MouseOverFixed(x, y)) then
  begin
    lBoolInherited := True;
    inherited MouseDown(Button, Shift, x, y);
  end
  else
  begin
    lboolInherited := False;
    // but set focus if possible (05/27/2004)
    if not (csDesigning in ComponentState) and
      (CanFocus or (GetParentForm(Self) = nil)) then
      SetFocus;
  end;

  if (GetParentForm(self) <> nil) then
    if (GetParentForm(self).ActiveControl = self) then
      if GetParentForm(self) <> Screen.ActiveForm then
        if HandleAllocated then
          Windows.SetFocus(self.Handle);

  if (Button = mbLeft) and (not MouseOverSelection) and
    (LgrcDummy.X >= GRID_FIXED) and (LgrcDummy.Y >= GRID_FIXED) then
  begin
    ResetSelection(True);
    if not (ssDouble in Shift) then
      FIsSelecting := True;
  end;

  if (Button = mbLeft) and MouseOverSelection then
  begin
    FMouseDownCol := x;
    FMouseDownRow := y;
    FMouseUpCanResetSel := True;
  end;

  if (not lBoolInherited) and (Assigned(OnMouseDown)) and Focused then
    OnMouseDown(self, Button, Shift, X, Y);
end;

procedure TCustomMPHexEditor.InternalGetCurSel(var StartPos, EndPos, ACol, ARow:
  integer);
begin
  if FSelPosition = -1 then
  begin
    StartPos := GetPosAtCursor(Col, Row);
    EndPos := StartPos + 1;
    aCol := Col;
    aRow := Row;
  end
  else
  begin
    StartPos := FSelStart;
    EndPos := FSelEnd + 1;
    with GetCursorAtPos(FSelStart, InCharField) do
    begin
      aCOL := X;
      aROW := Y;
    end;
  end;

  if FModifiedBytes.Size > StartPos then
    FModifiedBytes.Size := StartPos;
end;

function TCustomMPHexEditor.CreateShift4BitStream(const StartPos: integer; var
  FName: TFileName): TFileStream;
var
  LByt1,
    LByt2: byte;
  LBytBuffer: array[0..511] of byte;
  LIntLoop,
    LIntRead: integer;
begin
  Result := nil;
  if StartPos >= DataSize then
    Exit;

  FName := GetTempName;
  Result := TFileStream.Create(FName, fmCreate);
  Result.Position := 0;
  FDataStorage.Position := StartPos;
  LByt1 := 0;
  while FDataStorage.Position < DataSize do
  begin
    FillChar(LBytBuffer[0], 512, 0);
    LIntRead := FDataStorage.Read(LBytBuffer[0], 512);
    for LIntLoop := 0 to Pred(LIntRead) do
    begin
      LByt2 := LBytBuffer[LIntLoop] and 15;
      LBytBuffer[LIntLoop] := (LBytBuffer[LIntLoop] shr 4) or (LByt1 shl 4);
      LByt1 := LByt2;
    end;
    Result.WriteBuffer(LBytBuffer[0], LIntRead);
  end;
  Result.Position := 0;
end;

function TCustomMPHexEditor.InternalInsertNibble(const Pos: integer; const
  HighNibble: boolean): boolean;
var
  LfstNibbleStream: TFileStream;
  LStrFName: TFileName;
  LIntOldSize: integer;
  LByteFirst,
    LByteLast: byte;
begin
  Result := False;

  if DataSize = 0 then
    Exit;

  LIntOldSize := FDataStorage.Size;

  WaitCursor;
  try
    // nun zuerst alle restlichen bits verschieben
    LByteFirst := Data[Pos];
    LByteLast := Data[Pred(DataSize)];

    LfstNibbleStream := CreateShift4BitStream(Pos, LStrFName);
    with LfstNibbleStream do
    try
      FDataStorage.Position := Pos;
      FDataStorage.CopyFrom(LfstNibbleStream, LfstNibbleStream.Size);
    finally
      Free;
      DeleteFile(LStrFName);
    end;

    if HighNibble then
      LByteFirst := LByteFirst shr 4
    else
      LByteFirst := LByteFirst and 240;
    Data[Pos] := LByteFirst;
    FDataStorage.Size := LIntOldSize + 1;
    Data[Pred(DataSize)] := LByteLast shl 4;
    Result := True;
  finally
    OldCursor;
  end;
end;

function TCustomMPHexEditor.InsertNibble(const aPos: integer; const HighNibble:
  boolean; const UndoDesc: string = ''): boolean;
const
  L_BytAppend: byte = 0;
begin
  Result := False;

  if DataSize < 1 then
  begin
    ResetSelection(False);
    AppendBuffer(PChar(@L_BytAppend), 1);
    Result := True;
    Exit;
  end;

  if (aPos >= DataSize) or (aPos < 0) then
    Exit;

  CreateUndo(ufKindNibbleInsert, aPos, 0, 0, UndoDesc);

  ResetSelection(False);
  Result := InternalInsertNibble(aPos, HighNibble);

  if Result and (FModifiedBytes.Size >= (aPos)) then
    FModifiedBytes.Size := aPos;

  CalcSizes;
  Changed;
end;

function TCustomMPHexEditor.InternalDeleteNibble(const Pos: integer; const
  HighNibble: boolean): boolean;
var
  LfstNibbleStream: TFileStream;
  LStrFName: TFileName;
  LIntOldSize: integer;
  LByt1: byte;
begin
  Result := False;
  if DataSize = 0 then
    Exit;

  LIntOldSize := FDataStorage.Size;
  WaitCursor;
  try
    // nun zuerst alle restlichen bits verschieben
    LByt1 := Data[Pos];

    LfstNibbleStream := CreateShift4BitStream(Pos, LStrFName);
    with LfstNibbleStream do
    try
      FDataStorage.Position := Pos;
      Position := 1;
      FDataStorage.CopyFrom(LfstNibbleStream, LfstNibbleStream.Size - 1);
    finally
      Free;
      DeleteFile(LStrFName);
    end;

    if not HighNibble then
      Data[Pos] := (LByt1 and 240) or (Data[Pos] and 15);

    Result := True;
    FDataStorage.Size := LIntOldSize;
    Data[Pred(DataSize)] := Data[Pred(DataSize)] shl 4;
  finally
    OldCursor;
  end;
end;

function TCustomMPHexEditor.DeleteNibble(const aPos: integer; const HighNibble:
  boolean; const UndoDesc: string = ''): boolean;
begin
  Result := False;

  if (aPos >= DataSize) or (aPos < 0) then
    Exit;

  CreateUndo(ufKindNibbleDelete, aPos, 0, 0, UndoDesc);

  ResetSelection(False);
  Result := InternalDeleteNibble(aPos, HighNibble);

  if Result and (FModifiedBytes.Size >= (aPos)) then
    FModifiedBytes.Size := aPos;

  CalcSizes;
  Changed;
end;

procedure TCustomMPHexEditor.InternalConvertRange(const aFrom, aTo: integer;
  const aTransFrom, aTransTo: TMPHTranslationKind);
var
  LIntSize: integer;
begin
  LIntSize := (aTo - aFrom) + 1;
  WaitCursor;
  try
    FDataStorage.TranslateToAnsi(aTransFrom, aFrom, LIntSize);
    FDataStorage.TranslateFromAnsi(aTransTo, aFrom, LIntSize);
  finally
    OldCursor;
  end;
end;

procedure TCustomMPHexEditor.ConvertRange(const aFrom, aTo: integer; const
  aTransFrom, aTransTo: TMPHTranslationKind; const UndoDesc: string = '');
begin
  if aFrom > aTo then
    Exit;

  if aTransFrom = aTransTo then
    Exit;

  if (aTo >= DataSize) or (aFrom < 0) then
    Exit;

  CreateUndo(ufKindConvert, aFrom, (aTo - aFrom) + 1, 0, UndoDesc);

  InternalConvertRange(aFrom, aTo, aTransFrom, aTransTo);

  Invalidate;
  Changed;
end;

procedure TCustomMPHexEditor.InternalDelete(StartPos, EndPos, ACol, ARow:
  integer);
var
  LgrdEndPos: TGridCoord;
  LIntNewCol: integer;
begin
  if EndPos <= (DataSize - 1) then
    MoveFileMem(EndPos, StartPos, DataSize - EndPos);

  FDataStorage.Size := DataSize - (EndPos - StartPos);
  EndPos := GetPosAtCursor(aCol, aRow);

  if DataSize < 1 then
  begin
    LIntNewCol := GRID_FIXED;
    if FPosInCharField then
      LIntNewCol := Max(GRID_FIXED, GetOtherFieldColCheck(LIntNewCol));
    MoveColRow(LIntNewCol, GRID_FIXED, True, True)
  end
  else if EndPos >= DataSize then
  begin
    if InsertMode then
      LgrdEndPos := GetCursorAtPos(DataSize, FPosInCharField)
    else
      LgrdEndPos := GetCursorAtPos(DataSize - 1, FPosInCharField);
    MoveColRow(LgrdEndPos.x, LgrdEndPos.y, True, True);
  end
  else if ACol > -1 then
    MoveColRow(aCol, aRow, True, True);

  CalcSizes;
  ResetSelection(False);

  Invalidate;
end;

procedure TCustomMPHexEditor.DeleteSelection(const UndoDesc: string = '');
var
  LIntSelStart,
    LIntSelEnd,
    LIntCol,
    LIntRow: integer;
begin
  InternalGetCurSel(LIntSelStart, LIntSelEnd, LIntCol, LIntRow);
  CreateUndo(ufKindByteRemoved, LIntSelStart, LIntSelEnd - LIntSelStart,
    0, UndoDesc);

  InternalDelete(LIntSelStart, LIntSelEnd, LIntCol, LIntRow);
  Changed;
end;

procedure TCustomMPHexEditor.CreateUndo(const aKind: TMPHUndoFlag; const aPos,
  aCount, aReplCount: integer; const sDesc: string = '');
begin

  if CanCreateUndo(aKind, aCount, aReplCount) then
  begin
    if FUndoStorage.UpdateCount = 0 then
      FUndoStorage.CreateUndo(aKind, aPos, aCount, aReplCount, sDesc);
    FModified := True;
    //Changed;
  end
  else
    raise EMPHexEditor.Create(ERR_NOUNDO);
end;

procedure TCustomMPHexEditor.ResetUndo;
begin
  FUndoStorage.Reset;
end;

function TCustomMPHexEditor.GetCanUndo: boolean;
begin
  Result := (not FReadOnlyView) and FUndoStorage.CanUndo;
end;

function TCustomMPHexEditor.GetCanRedo: boolean;
begin
  Result := (not FReadOnlyView) and FUndoStorage.CanRedo;
end;

function TCustomMPHexEditor.GetUndoDescription: string;
begin
  if not (csDestroying in ComponentState) then
  begin
    with FUndoStorage do
      if CanUndo then
        Result := Description
      else
        Result := UNDO_NOUNDO;
  end
  else
    Result := UNDO_NOUNDO;
end;

function TCustomMPHexEditor.GetSelStart: integer;
begin
  if FSelPosition = -1 then
  begin
    Result := GetPosAtCursor(Col, Row);
  end
  else
    Result := FSelPosition;
end;

function TCustomMPHexEditor.GetSelEnd: integer;
begin
  if FSelPosition = -1 then
    Result := GetPosAtCursor(Col, Row)
  else
  begin
    Result := FSelEnd;
    if FSelPosition = FSelEnd then
      Result := FSelStart;
  end;
end;

procedure TCustomMPHexEditor.SetSelStart(aValue: integer);
begin
  if (aValue < 0) or (aValue >= DataSize) then
    raise EMPHexEditor.Create(ERR_INVALID_SELSTART)
  else
  begin
    ResetSelection(True);
    with GetCursorAtPos(aValue, InCharField) do
      MoveColRow(X, Y, True, True);
  end;
end;

procedure TCustomMPHexEditor.SetSelEnd(aValue: integer);
begin
  if (aValue < -1) or (aValue >= DataSize) then
    raise EMPHexEditor.Create(ERR_INVALID_SELEND)
  else
  begin
    ResetSelection(True);
    if aValue > -1 then
    begin
      with GetCursorAtPos(aValue, InCharField) do
        Select(Col, Row, X, Y);
      SelectionChanged;
    end;
  end;
end;

procedure TCustomMPHexEditor.SetInCharField(const Value: boolean);
begin
  if (DataSize < 1) then
    Exit;

  if InCharField <> Value then
    MoveColRow(GetOtherFieldCol(Col), Row, True, True);
end;

function TCustomMPHexEditor.GetInCharField: boolean;
begin
  Result := False;
  if DataSize < 1 then
    Exit;

  GetPosAtCursor(Col, Row);
  Result := FPosInCharField;
end;

procedure TCustomMPHexEditor.Loaded;
begin
  inherited;
  CreateEmptyFile(UNNAMED_FILE);
end;

procedure TCustomMPHexEditor.CreateWnd;
begin
  inherited;
  if (csDesigning in ComponentState) or (FFileName = '---') then
    CreateEmptyFile(UNNAMED_FILE);
end;

procedure TCustomMPHexEditor.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  CreateCaretGlyph;
  CheckSetCaret;
  Invalidate;
end;

procedure TCustomMPHexEditor.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  HideCaret(Handle);
  DestroyCaret();
  FIsSelecting := False;
  Invalidate;
end;

procedure TCustomMPHexEditor.CMINTUPDATECARET(var Msg: TMessage);
begin
  if Msg.WParam = 7 then
  begin
    CheckSetCaret;
  end;
end;

procedure TCustomMPHexEditor.SetTranslation(const Value: TMPHTranslationKind);
begin
  if FTranslation <> Value then
  begin
    if (Value <> tkAsIs) and FUnicodeCharacters then
      raise EMPHexEditor.Create(ERR_NO_TRANSLATION_IN_UNICODE_MODE);
    FTranslation := Value;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetModified(const Value: boolean);
begin
  FModified := Value;
  if not Value then
  begin
    ResetUndo;
    FModifiedBytes.Size := 0;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetBytesPerRow(const Value: integer);
var
  LIntPos,
    LIntSelPos,
    LIntSelStart,
    LIntSelEnd: integer;
  LBoolInCharField,
    LBool2ndCol: boolean;
begin
  if ((Value < 1) or (Value > 256)) or
    (FUnicodeCharacters and ((Value mod 2) <> 0)) then
    raise EMPHexEditor.Create(ERR_INVALID_BYTESPERLINE)
  else if FBytesPerRow <> Value then
  begin
    with FOffsetFormat do
      if offCalcRow in Flags then
        _BytesPerUnit := Value;
    LIntSelPos := FSelPosition;
    LIntSelStart := FSelStart;
    LIntSelEnd := FSelEnd;
    LIntPos := GetPosAtCursor(Col, Row);
    LBoolInCharField := FPosInCharField;
    LBool2ndCol := GetCursorAtPos(LIntPos, LBoolInCharField).x <> Col;
    FBytesPerRow := Value;
    FBytesPerRowDup := Value * 2;
    FIntLastHexCol := (GRID_FIXED + FBytesPerRowDup - 1);
    SetRulerString;
    CalcSizes;
    if (LIntPos >= DataSize) or (InsertMode and (LIntPos > DataSize)) then
      LIntPos := DataSize - 1;

    with GetCursorAtPos(LIntPos, LBoolInCharField) do
    begin
      if LBool2ndCol then
        Inc(x);

      MoveColRow(x, y, True, True);
    end;

    SetSelection(LIntSelPos, LIntSelStart, LIntSelEnd);
  end;
end;

procedure TCustomMPHexEditor.InternalAppendBuffer(Buffer: PChar; const Size:
  integer);
var
  LIntSize: integer;
begin
  if DataSize = 0 then
  begin
    FDataStorage.Position := 0;
    FModifiedBytes.Size := 0;
  end;

  LIntSize := DataSize;
  FDataStorage.Size := LIntSize + Size;
  WriteBuffer(Buffer^, LIntSize, Size);
  CalcSizes;
end;

procedure TCustomMPHexEditor.InternalInsertBuffer(Buffer: PChar; const Size,
  Position: integer);
var
  LIntSize: integer;
begin
  if DataSize = 0 then
  begin
    FDataStorage.Position := 0;
    FModifiedBytes.Size := 0;
  end;

  LIntSize := DataSize;
  FDataStorage.Size := LIntSize + Size;
  if Position < LIntSize then
    // nur, wenn nicht hinter streamende, dann platz schaffen
    MoveFileMem(Position, Position + Size, DataSize - Position - Size); //+ 1);

  if Buffer <> nil then
    WriteBuffer(Buffer^, Position, Size);
  CalcSizes;
end;

procedure TCustomMPHexEditor.InsertBuffer(aBuffer: PChar; const aSize, aPos:
  integer; const UndoDesc: string = ''; const MoveCursor: Boolean = True);
begin
  FDataStorage.CheckBounds(aPos);
  CreateUndo(ufKindInsertBuffer, aPos, aSize, 0, UndoDesc);

  InternalInsertBuffer(aBuffer, aSize, aPos);

  if FModifiedBytes.Size >= (aPos) then
    FModifiedBytes.Size := aPos;

  if Enabled then
  begin
    SetSelection(aPos, aPos, aPos + aSize - 1);
    if MoveCursor then
    begin
      with GetCursorAtPos(FSelEnd, InCharField) do
        MoveColRow(x, y, True, True);
      SetSelection(aPos, aPos, aPos + aSize - 1);
    end;
    Invalidate;
  end;
  Changed;
end;

procedure TCustomMPHexEditor.AppendBuffer(aBuffer: PChar; const aSize: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = True);
var
  LIntSize: integer;
begin
  if (not Assigned(aBuffer)) or (aSize = 0) then
    Exit;

  CreateUndo(ufKindAppendBuffer, DataSize, aSize, 0, UndoDesc);

  if FModifiedBytes.Size >= (DataSize) then
    FModifiedBytes.Size := DataSize;

  LIntSize := DataSize;
  InternalAppendBuffer(aBuffer, aSize);

  if MoveCursor then
    with GetCursorAtPos(LIntSize, InCharField) do
      MoveColRow(x, y, True, True);
  SetSelection(LIntSize, LIntSize, LIntSize + aSize - 1);
  Invalidate;
  Changed;
end;

procedure TCustomMPHexEditor.ReplaceSelection(aBuffer: PChar; aSize: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = True);
var
  LIntStart,
    LIntEnd,
    LIntCol,
    LIntRow: integer;
  LBoolInCharField: boolean;
begin
  // auswahl berechnen
  LBoolInCharField := GetInCharField;
  if FSelPosition = -1 then
    InsertBuffer(aBuffer, aSize, SelStart, UndoDesc, MoveCursor)
  else
  begin
    if IsFileSizeFixed then
    begin
      if aSize > SelCount then
        aSize := SelCount
      else if SelCount > aSize then
      begin
        SelStart := Min(SelStart, SelEnd);
        SelEnd := SelStart + aSize - 1;
      end;
    end;

    CreateUndo(ufKindReplace, FSelStart, aSize, SelCount, UndoDesc);

    // zuerst aktuelle auswahl löschen
    InternalGetCurSel(LIntStart, LIntEnd, LIntCol, LIntRow);
    InternalDelete(LIntStart, LIntEnd, LIntCol, LIntRow);
    InternalInsertBuffer(aBuffer, aSize, LIntStart);
    if FModifiedBytes.Size >= LIntStart then
      FModifiedBytes.Size := Max(0, LIntStart);

    if MoveCursor then
    begin
      with GetCursorAtPos(LIntStart + aSize - 1, LBoolInCharField) do
        MoveColRow(x, y, True, True);
      SetSelection(LIntStart + aSize - 1, LIntStart, LIntStart + aSize - 1);
    end;
    Invalidate;
    Changed;
  end;
end;

procedure TCustomMPHexEditor.SetChanged(DataPos: integer; const Value: boolean);
begin
  if InsertMode then
    FModifiedBytes.Size := 0;

  if not Value then
    if FModifiedBytes.Size <= DataPos then
      Exit;

  FModifiedBytes[DataPos] := Value;
end;

procedure TCustomMPHexEditor.MoveFileMem(const aFrom, aTo, aCount: integer);
begin
  FDataStorage.Move(aFrom, aTo, aCount);
end;

function TCustomMPHexEditor.GetCursorPos: integer;
begin
  Result := GetPosAtCursor(Col, Row);
  if Result < 0 then
    Result := 0;

  if Result > Max(0, DataSize - 1) then
    Result := Max(0, DataSize - 1)
end;

function TCustomMPHexEditor.GetSelCount: integer;
begin
  if FSelPosition = -1 then
    Result := 0
  else
    Result := Max(FSelStart, FSelEnd) - Min(FSelStart, FSelEnd) + 1;
end;

procedure TCustomMPHexEditor.SetReadOnlyFile(const Value: boolean);
begin
  if Value and (not FIsFileReadonly) then
  begin
    FIsFileReadonly := True;
  end;
end;

function TCustomMPHexEditor.BufferFromFile(const aPos: integer; var aCount:
  integer): PChar;
begin
  if (aPos < 0) or (aPos >= DataSize) then
    raise EMPHexEditor.Create(ERR_INVALID_BUFFERFROMFILE)
  else
  begin
    if (aPos + aCount) > DataSize then
      aCount := (DataSize - aPos) + 1;

    GetMem(Result, aCount);
    try
      FDataStorage.ReadBufferAt(Result^, aPos, aCount);
    except
      try
        FreeMem(Result);
      except
      end;
      Result := nil;
      aCount := 0;
    end;
  end;
end;

procedure TCustomMPHexEditor.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  CheckSetCaret;
end;

procedure TCustomMPHexEditor.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  CheckSetCaret;
end;

procedure TCustomMPHexEditor.CreateCaretGlyph;
begin
  DestroyCaret();
  FCaretBitmap.Width := FCharWidth;
  FCaretBitmap.Height := FCharHeight - 2;
  FCaretBitmap.Canvas.Brush.Color := clBlack;
  FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight - 2));
  FCaretBitmap.Canvas.Brush.Color := clWhite;
  case FCaretKind of
    ckFull: FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight -
        2));
    ckLeft: FCaretBitmap.Canvas.FillRect(Rect(0, 0, 2, FCharHeight - 2));
    ckBottom: FCaretBitmap.Canvas.FillRect(Rect(0, FCharHeight - 4, FCharWidth,
        FCharHeight - 2));
    ckAuto:
      begin
        if FReadOnlyView then
          FCaretBitmap.Canvas.FillRect(Rect(0, FCharHeight - 4, FCharWidth,
            FCharHeight - 2))
        else
        begin
          if FInsertModeOn then
            FCaretBitmap.Canvas.FillRect(Rect(0, 0, 2, FCharHeight - 2))
          else
            FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight -
              2));
        end;
      end;
  end;
  CreateCaret(Handle, FCaretBitmap.Handle, 0, 0);
  ShowCaret(Handle);
end;

procedure TCustomMPHexEditor.SetBytesPerColumn(const Value: integer);
begin
  if ((Value < 1) or (Value > 256)) or
    (FUnicodeCharacters and ((Value mod 2) <> 0)) then
    raise EMPHexEditor.Create(ERR_INVALID_BYTESPERCOL)
  else if FBytesPerCol <> (Value * 2) then
  begin
    with FOffsetFormat do
      if offCalcColumn in Flags then
        _BytesPerUnit := Value;
    FBytesPerCol := Value * 2;
    AdjustMetrics;
    SetRulerString;
    Invalidate;
  end;
end;

function TCustomMPHexEditor.GetBytesPerColumn: integer;
begin
  Result := FBytesPerCol div 2;
end;

function TCustomMPHexEditor.PrepareFindReplaceData(StrData: string; const
  IgnoreCase, IsText: boolean): string;
var
  LWStrTemp: WideString;
  LIntLoop: Integer;
  lChrTbl: Char;
begin
  if Length(StrData) = 0 then
    Result := ''
  else
  begin
    if IgnoreCase then
      StrData := AnsiLowerCase(StrData);
    if IsText and (FTranslation <> tkAsIs) then
    begin
      UniqueString(StrData);
      TranslateBufferFromAnsi(FTranslation, @StrData[1], @StrData[1],
        Length(StrData));
    end;
    if (not IsText) or (not FUnicodeCharacters) then
      Result := StrData
    else
    begin
      // create a unicode string
      LWStrTemp := StrData;
      if FUnicodeBigEndian then
        for LIntLoop := 1 to Length(LWStrTemp) do
          SwapWideChar(LWStrTemp[LIntLoop]);
      SetLength(Result, Length(LWStrTemp) * 2);
      Move(LWStrTemp[1], Result[1], Length(Result));
    end;

    // create compare tables
    for LChrTbl := #0 to #255 do
    begin
      FFindTable[LChrTbl] := LChrTbl;

      FFindTableI[LChrTbl] := LChrTbl;
      if FTranslation <> tkAsIs then
        TranslateBufferToAnsi(FTranslation, @FFindTableI[LChrTbl],
          @FFindTableI[LChrTbl], 1);
      CharLowerBuff(@FFindTableI[LChrTbl], 1);
      if FTranslation <> tkAsIs then
        TranslateBufferFromAnsi(FTranslation, @FFindTableI[LChrTbl],
          @FFindTableI[LChrTbl], 1);
    end;
  end;
end;

function TCustomMPHexEditor.Find(aBuffer: PChar; aCount: integer; const aStart,
  aEnd: integer; const IgnoreCase: boolean): integer;
var
  LBoolDummy: Boolean;
  LChrCurrent: char;
  LIntCurPos,
    LIntLoop,
    LIntFound,
    LIntEnd: integer;
  cLoop,
    cInc: Cardinal;
  LPTblFind: PMPHFindTable;
begin
  if Assigned(FOnFind) then
    FOnFind(self, aBuffer, aCount, aStart, aEnd, IgnoreCase, #0, Result)
  else
  begin
    Result := -1;
    LIntEnd := aEnd;
    cLoop := 0;
    if LIntEnd >= DataSize then
      LIntEnd := DataSize - 1;

    if aCount < 1 then
      Exit;

    if aStart + aCount > (LIntEnd + 1) then
      Exit; // will never be found, if search-part is smaller than searched data

    if IgnoreCase then
      LPTblFind := @FFindTableI
    else
      LPTblFind := @FFindTable;

    cInc := DataSize div 500;

    WaitCursor;
    try

      LIntCurPos := aStart;
      LIntLoop := 0;
      LIntFound := LIntCurPos + 1;

      repeat
        if FFindProgress and Assigned(FOnProgress) then
        begin
          Inc(cLoop);
          // changed in 12-28-2004 to avoid edivbyzero
          if (cInc = 0) or ((cLoop mod cInc) = 0) then
            FOnProgress(self, pkFind, FFileName, Round((LIntCurpos / DataSize) *
              100), LBoolDummy);
        end;

        if LIntCurPos > LIntEnd then
          Exit;

        LChrCurrent := LPTblFind^[char(Data[LIntCurPos])];

        if (LChrCurrent = aBuffer[LIntLoop]) then
        begin
          if LIntLoop = (aCount - 1) then
          begin
            Result := LIntCurPos - aCount + 1;
            Exit;
          end
          else
          begin
            if LIntLoop = 0 then
              LIntFound := LIntCurPos + 1;
            Inc(LIntCurPos);
            Inc(LIntLoop);
          end;
        end
        else
        begin
          LIntCurPos := LIntFound;
          LIntLoop := 0;
          LIntFound := LIntCurPos + 1;
        end;
      until False;

    finally
      OldCursor;
    end;
  end;
end;

procedure TCustomMPHexEditor.AddSelectionUndo(const AStart,
  ACount: integer);
begin
  CreateUndo(ufKindSelection, AStart, aCount, 0, '');
end;

function TCustomMPHexEditor.FindWithWildcard(aBuffer: PChar;
  aCount: integer; const aStart, aEnd: integer; const IgnoreCase: boolean;
  const Wildcard: char): integer;
var
  LBoolDummy: boolean;
  LChrCurrent: char;
  LIntCurPos,
    LIntLoop,
    LIntFound,
    LIntEnd: integer;
  bFound: boolean;
  cLoop,
    cInc: cardinal;
  LPTblFind: PMPHFindTable;
begin
  if Assigned(FOnWildcardFind) then
    FOnWildcardFind(self, aBuffer, aCount, aStart, aEnd, IgnoreCase, Wildcard,
      Result)
  else
  begin
    Result := -1;
    LIntEnd := aEnd;
    cLoop := 0;
    if LIntEnd >= DataSize then
      LIntEnd := DataSize - 1;

    if aCount < 1 then
      Exit;

    if aStart + aCount > (LIntEnd + 1) then
      Exit; // will never be found, if search-part is smaller than searched data

    if IgnoreCase then
      LPTblFind := @FFindTableI
    else
      LPTblFind := @FFindTable;

    cInc := DataSize div 500;

    WaitCursor;
    try
      LIntCurPos := aStart;
      LIntLoop := 0;
      LIntFound := LIntCurPos + 1;

      repeat
        if FFindProgress and Assigned(FOnProgress) then
        begin
          Inc(cLoop);
          // changed in 12-28-2004 to avoid edivbyzero
          if (cInc = 0) or ((cLoop mod cInc) = 0) then
            FOnProgress(self, pkFind, FFileName, Round((LIntCurpos / DataSize) *
              100), LBoolDummy);
        end;

        if LIntCurPos > LIntEnd then
          Exit;

        bFound := aBuffer[LIntLoop] = WildCard;
        if not bFound then
        begin
          LChrCurrent := LPTblFind^[char(Data[LIntCurPos])];
          bFound := (LChrCurrent = aBuffer[LIntLoop]);
        end;

        if bFound then
        begin
          if LIntLoop = (aCount - 1) then
          begin
            Result := LIntCurPos - aCount + 1;
            Exit;
          end
          else
          begin
            if LIntLoop = 0 then
              LIntFound := LIntCurPos + 1;
            Inc(LIntCurPos);
            Inc(LIntLoop);
          end;
        end
        else
        begin
          LIntCurPos := LIntFound;
          LIntLoop := 0;
          LIntFound := LIntCurPos + 1;
        end;
      until False;

    finally
      OldCursor;
    end;
  end;
end;

procedure TCustomMPHexEditor.SetOffsetDisplayWidth;
var
  s: string;
begin
  if Assigned(FOnGetOffsetText) and (not FOffsetHandler) then
  begin
    FOffsetHandler := True;
    try
      FIsMaxOffset := True;
      FOnGetOffsetText(self, (RowCount - 3) * FBytesPerRow, s);
    finally
      FOffsetHandler := False;
    end;
    FOffsetDisplayWidth := Length(s) + 1;
  end
  else
  begin
    with FOffsetFormat do
      if offCalcWidth in Flags then
        MinWidth := Length(IntToRadix(((RowCount - 3) * FBytesPerRow) div
          _BytesPerUnit, Radix));

    FOffSetDisplayWidth := Length(GetOffsetString((RowCount - 3) * FBytesPerRow))
      + 1;
  end;
  if FGutterWidth = -1 then
    DoSetCellWidth(0, FOffSetDisplayWidth * FCharWidth + 20 + 1)
  else
    DoSetCellWidth(0, FGutterWidth);
end;

function TCustomMPHexEditor.Seek(const aOffset, aOrigin: integer): integer;
var
  LIntPos: integer;
begin
  Result := -1;
  LIntPos := GetCursorPos;
  case aOrigin of
    soFromBeginning: LIntPos := aOffset;
    soFromCurrent: LIntPos := GetCursorPos + aOffset;
    soFromEnd: LIntPos := DataSize + aOffset - 1;
  end;

  if DataSize < 1 then
    Exit;

  LIntPos := Min(Max(0, LIntPos), DataSize - 1);

  SelStart := LIntPos;
  Result := LIntPos;
end;

procedure TCustomMPHexEditor.SetSwapNibbles(const Value: boolean);
begin
  if integer(Value) <> FSwapNibbles then
  begin
    FSwapNibbles := integer(Value);
    Invalidate;
  end;
end;

function TCustomMPHexEditor.GetSwapNibbles: boolean;
begin
  Result := boolean(FSwapNibbles);
end;

procedure TCustomMPHexEditor.SetColors(const Value: TMPHColors);
begin
  FColors.Assign(Value);
end;

procedure TCustomMPHexEditor.SetCaretKind(const Value: TMPHCaretKind);
begin
  if FCaretKind <> Value then
  begin
    FCaretKind := Value;
    if Focused then
    begin
      CreateCaretGlyph;
      IntSetCaretPos(-50, -50, -1);
      Invalidate;
    end;
  end;
end;

procedure TCustomMPHexEditor.SetFocusFrame(const Value: boolean);
begin
  if FFocusFrame <> Value then
  begin
    FFocusFrame := Value;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetMaskChar(const Value: char);
begin
  if FReplaceUnprintableCharsBy <> Value then
  begin
    FReplaceUnprintableCharsBy := Value;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetAsText(const Value: string);
var
  LpszBuffer: PChar;
begin
  if DataSize > 0 then
  begin
    // alles selektieren
    SelStart := 0;
    SelEnd := DataSize - 1;
  end;
  // do translation (thanks to philippe chessa)  dec 17 98
  GetMem(LpszBuffer, Length(Value));
  try
    Move(Value[1], LpszBuffer^, Length(Value));
    TranslateBufferFromANSI(FTranslation, @Value[1], LpszBuffer, Length(Value));
    ReplaceSelection(LpszBuffer, Length(Value));
  finally
    FreeMem(LpszBuffer);
  end;
end;

procedure TCustomMPHexEditor.SetAsHex(const Value: string);
var
  LpszBuffer: PChar;
  LIntAmount: integer;
begin
  if DataSize > 0 then
  begin
    // alles selektieren
    SelStart := 0;
    SelEnd := DataSize - 1;
  end;

  GetMem(LpszBuffer, Length(Value));
  try
    ConvertHexToBin(@Value[1], LpszBuffer, Length(Value), SwapNibbles,
      LIntAmount);
    ReplaceSelection(LpszBuffer, LIntAmount);
  finally
    FreeMem(LpszBuffer);
  end;
end;

function TCustomMPHexEditor.GetAsText: string;
begin
  if DataSize < 1 then
    Result := ''
  else
  begin
    SetLength(Result, DataSize);
    ReadBuffer(Result[1], 0, DataSize);
  end;
end;

function TCustomMPHexEditor.GetAsHex: string;
begin
  Result := FDataStorage.GetAsHex(0, DataSize, SwapNibbles)
end;

function TCustomMPHexEditor.GetSelectionAsHex: string;
begin
  if (DataSize < 1) or (SelCount < 1) then
    Result := ''
  else
    Result := FDataStorage.GetAsHex(Min(SelStart, SelEnd), SelCount,
      SwapNibbles);
end;

function TCustomMPHexEditor.GetInsertMode: boolean;
begin
  Result := FInsertModeOn and IsInsertModePossible;
end;

procedure TCustomMPHexEditor.SetAllowInsertMode(const Value: boolean);
begin
  if not Value then
  begin
    if FInsertModeOn then
      InsertMode := False;
  end;
  FAllowInsertMode := Value;
end;

procedure TCustomMPHexEditor.SetFixedFileSize(const Value: boolean);
begin
  if Value <> FFixedFileSize then
  begin
    if Value then
      InsertMode := False;
    FFixedFileSize := Value;
  end;
end;

procedure TCustomMPHexEditor.InternalErase(const KeyWasBackspace: boolean; const
  UndoDesc: string = '');
var
  LIntPos: integer;
  LIntSavePos: integer;
  LIntCount: integer;
begin
  LIntPos := GetCursorPos div FBytesPerUnit * FBytesPerUnit;
  LIntCount := FBytesPerUnit;
  LIntSavePos := LIntPos;
  if KeyWasBackspace then
  begin // Delete previous byte(s)
    if InsertMode and (SelCount = 0) then
    begin
      LIntPos := GetPosAtCursor(Col, Row);
      if (LIntPos = DataSize) and ((DataSize mod FBytesPerUnit) <> 0) then
        LIntCount := 1
      else
      begin
        LIntPos := LIntPos div FBytesPerUnit * FBytesPerUnit;
        LIntCount := FBytesPerUnit;
      end;
    end;

    if LIntPos = 0 then
      Exit; // Can't delete at offset -1

    CreateUndo(ufKindByteRemoved, LIntPos - LIntCount, LIntCount,
      0, UndoDesc);

    InternalDelete(LIntPos - LIntCount, LIntPos, Col, Row);
    if LIntSavePos = LIntPos then
      Seek(LIntPos - LIntCount, soFromBeginning) // Move caret
    else
    begin
      if (Col + 1) <= GetLastCharCol then
        Col := Col + 1;
    end;
    Changed;
  end
  else
  begin // Delete next byte
    if LIntPos >= DataSize then
      Exit; // Cant delete at EOF
    while (LIntPos + LIntCount) > DataSize do
      Dec(LIntCount);
    CreateUndo(ufKindByteRemoved, LIntPos, LIntCount, 0, UndoDesc);
    InternalDelete(LIntPos, LIntPos + LIntCount, Col, Row);
    Changed;
  end;
end;

procedure TCustomMPHexEditor.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS or
    DLGC_WANTALLKEYS;
  if FWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB
  else
    Msg.Result := Msg.Result and not DLGC_WANTTAB;
end;

procedure TCustomMPHexEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    AdjustMetrics;
    if Focused then
    begin
      CreateCaretGlyph;
    end;
  end;
end;

procedure TCustomMPHexEditor.SetWantTabs(const Value: boolean);
begin
  FWantTabs := Value;
end;

procedure TCustomMPHexEditor.SetReadOnlyView(const Value: boolean);
begin
  FReadOnlyView := Value;

  if (FCaretKind = ckAuto) and Focused then
    CreateCaretGlyph;
end;

procedure TCustomMPHexEditor.SetHideSelection(const Value: boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    if (not Focused) and (GetSelCount > 0) then
      Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetGraySelectionIfNotFocused(const Value: boolean);
begin
  if FGraySelOnLostFocus <> Value then
  begin
    FGraySelOnLostFocus := Value;
    if (not Focused) and (GetSelCount > 0) and (not FHideSelection) then
      Invalidate;
  end;
end;

function TCustomMPHexEditor.CalcColCount: integer;
begin
  if FUnicodeCharacters then
    Result := (FBytesPerRow * 2) + (FBytesPerRow div 2) + 1 + GRID_FIXED
  else
    Result := FBytesPerRow * 3 + 1 + GRID_FIXED;
end;

function TCustomMPHexEditor.GetLastCharCol: integer;
begin
  Result := ColCount - 1;
end;

function TCustomMPHexEditor.GetTopLeftPosition(var oInCharField: boolean):
  integer;
begin
  Result := GetPosAtCursor(Max(LeftCol, GRID_FIXED), TopRow);
  oInCharField := InCharField;
end;

procedure TCustomMPHexEditor.SetTopLeftPosition(const aPosition: integer; const
  aInCharField: boolean);
begin
  with GetCursorAtPos(aPosition, aInCharField) do
  begin
    TopRow := y;
    LeftCol := x;
  end;
end;

function TCustomMPHexEditor.GetPropColCount: integer;
begin
  Result := inherited ColCount;
end;

function TCustomMPHexEditor.GetPropRowCount: integer;
begin
  Result := inherited RowCount;
end;

function TCustomMPHexEditor.ShowDragCell(const X, Y: integer): integer;
var
  LRctCell: TRect;
  LIntDragPos,
    LIntMouseX,
    LIntMouseY: integer;
begin
  with MouseCoord(X, Y) do
  begin
    LIntMouseX := X;
    LIntMouseY := Y;
    if X < GRID_FIXED then
      X := GRID_FIXED;
    if Y >= RowCount then
      Y := RowCount - 1;
    if Y < GRID_FIXED then
      Y := GRID_FIXED;
    LIntDragPos := GetPosAtCursor(X, Y)
  end;

  if LIntDragPos < 0 then
    LIntDragPos := 0;
  if LIntDragPos > DataSize then
    LIntDragPos := DataSize;
  if IsSelected(LIntDragPos) then
    LIntDragPos := Min(SelStart, SelEnd);
  CheckUnit(LIntDragPos);
  Result := LIntDragPos;
  FShowDrag := True;

  if (LIntMouseY <= TopRow) and (LIntMouseY > GRID_FIXED) then
  begin
    // nach oben scrollen
    TopRow := TopRow - 1;
  end
  else if (LIntMouseY >= (TopRow + VisibleRowCount - 1)) and (LIntMouseY <
    Pred(RowCount)) then
  begin
    // nach unten scrollen
    TopRow := TopRow + 1;
  end;

  if (LIntMouseX <= LeftCol) and (LIntMouseX > GRID_FIXED) then
  begin
    // nach links scrollen
    LeftCol := LeftCol - 1;
  end
  else if (LIntMouseX >= (LeftCol + VisibleColCount - 1)) and
    (LIntMouseX < GetLastCharCol) then
  begin
    // nach unten scrollen
    LeftCol := LeftCol + 1;
  end;

  with GetCursorAtPos(LIntDragPos, FPosInCharField) do
  begin
    if (x = FDropCol) and (y = FDropRow) then
      Exit;
    LRctCell := CellRect(FDropCol, FDropRow);
    FDropCol := x;
    FDropRow := y;
    InvalidateRect(Handle, @LRctCell, True);
    LRctCell := CellRect(X, Y);
    InvalidateRect(Handle, @LRctCell, True);
  end;
end;

procedure TCustomMPHexEditor.HideDragCell;
begin
  FShowDrag := False;
  Invalidate;
end;

procedure TCustomMPHexEditor.CombineUndo(const aCount: integer; const sDesc:
  string = '');
begin
  CreateUndo(ufKindCombined, 0, aCount, 0, sDesc);
end;

function TCustomMPHexEditor.GetMouseOverSelection: boolean;
var
  LPntMouse: TPoint;
begin
  Windows.GetCursorPos(LPntMouse);
  LPntMouse := ScreenToClient(LPntMouse);
  Result := CursorOverSelection(LPntMouse.x, LPntMouse.y);
end;

function TCustomMPHexEditor.CursorOverSelection(const X, Y: integer): boolean;
var
  LIntPos: integer;
  LBoolInCharField: boolean;
begin
  Result := False;
  if (SelCount = 0) or (DataSize = 0) then
    Exit;

  LBoolInCharField := FPosInCharField;
  with MouseCoord(x, y) do
  begin
    if (x < GRID_FIXED) or (y < GRID_FIXED) then
      Exit;

    LIntPos := GetPosAtCursor(X, Y);
    FPosInCharField := (LBoolInCharField);
    if (LIntPos < 0) or (LIntPos >= DataSize) then
      Exit;
  end;

  Result := IsSelected(LIntPos);
end;

function TCustomMPHexEditor.MouseOverFixed(const X, Y: integer): boolean;
begin
  with MouseCoord(x, y) do
    Result := (x < GRID_FIXED) or (y < GRID_FIXED);
end;

procedure TCustomMPHexEditor.MouseMove(Shift: TShiftState; X, Y: integer);
var
  LgrcCoords: TGridCoord;
begin
  if Shift = [ssLeft] then
    LgrcCoords := CheckMouseCoord(X, Y);

  inherited MouseMove(Shift, x, y);

  if FMouseUpCanResetSel then
  begin
    FMouseUpCanResetSel := (LgrcCoords.x = FMouseDownCol) and
      (LgrcCoords.y = FMouseDownRow);
  end;

  if (Shift = []) and (CursorOverSelection(X, Y) or MouseOverFixed(X, Y)) then
    Cursor := crArrow
  else
    Cursor := crIBeam;
end;

procedure TCustomMPHexEditor.WMTimer(var Msg: TWMTimer);
var
  LPtMouse: TPoint;
  LgrcCoord: TGridCoord;
begin
  if FGridState <> gsSelecting then
    Exit;
  Windows.GetCursorPos(LPtMouse);
  LPtMouse := ScreenToClient(LPtMouse);
  LgrcCoord := CheckMouseCoord(LPtMouse.X, LPtMouse.Y);
  if (LGrcCoord.X <> -1) and (LGrcCoord.Y <> -1) then
    inherited;
end;

function TCustomMPHexEditor.CheckMouseCoord(var X, Y: integer): TGridCoord;
var
  LRctCell: TRect;
begin
  Result := MouseCoord(X, Y);
  if FInsertModeOn then
  begin
    // use the following cell if the mouse is over the second half of the cell
    LRctCell := CellRect(Result.X, Result.Y);
    if (LRctCell.Left + (FCharWidth div 2)) <= X then
    begin
      if not (Result.X in [GetLastCharCol, FBytesPerRowDup + GRID_FIXED - 1])
        then
      begin
        X := LRctCell.Right + 1;
        Inc(Result.X);
        LRctCell := CellRect(Result.X, Result.Y);
      end;
    end;
    if (Result.X = GetLastCharCol) then
    begin
      if (X - LRctCell.Left) > (FCharWidth div 2) then
      begin
        Y := Y + RowHeight;
        Result.Y := Result.Y + 1;
        Result.X := FBytesPerRowDup + 1 + GRID_FIXED;
        X := CellRect(Result.X, Result.Y - 1).Left;
        //Dec(X, FCharWidth * FBytesPerRow);
      end;
    end
    else if Result.X = (FBytesPerRowDup + GRID_FIXED - 1) then
    begin
      if (X - LRctCell.Left) > (FCharWidth div 2) then
      begin
        Y := Y + RowHeight;
        Result.Y := Result.Y + 1;
        Result.X := GRID_FIXED;
        X := CellRect(Result.X, Result.Y - 1).Left;
        //Dec(X, FCharWidth * FBytesPerRow);
      end;
    end;
  end;
end;

procedure TCustomMPHexEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  CheckMouseCoord(X, Y);
  inherited;
  if FMouseUpCanResetSel then
  begin
    FMouseUpCanResetSel := False;
    ResetSelection(True);
    with MouseCoord(x, y) do
      MoveColRow(x, y, True, True);
  end;
  if FShowDrag then
    HideDragCell;
end;

procedure TCustomMPHexEditor.AdjustBookmarks(const From, Offset: integer);
var
  LIntLoop: integer;
  LBoolChanged: boolean;
begin
  LBoolChanged := False;
  if From >= 0 then
    for LIntLoop := 0 to 9 do
      with FBookmarks[LIntLoop] do
        if mPosition >= From then
        begin
          LBoolChanged := True;
          Inc(mPosition, Offset);
          if mPosition > DataSize then
            mPosition := -1;
        end;
  if LBoolChanged then
    BookMarkChanged;
end;

procedure TCustomMPHexEditor.IntSetCaretPos(const X, Y, aCol: integer);
begin
  if Focused then
  begin
    if aCol <> -1 then
    begin
      FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
      if FLastPosInCharField <> FPosInCharField then
      begin
        FLastPosInCharField := FPosInCharField;
        Invalidate;
      end;
    end;
    SetCaretPos(X, Y);
  end;
end;

procedure TCustomMPHexEditor.TruncMaxPosition(var DataPos: integer);
begin
  if DataPos >= DataSize then
  begin
    DataPos := DataSize - 1;
    if InsertMode then
      DataPos := DataSize;
  end;
end;

function TCustomMPHexEditor.GetCurrentValue: integer;
var
  LIntPos: integer;
begin
  Result := -1;
  LIntPos := GetPosAtCursor(Col, Row);
  if (LIntPos >= DataSize) or (LIntPos < 0) then
    Exit;
  Result := Data[LIntPos]
end;

procedure TCustomMPHexEditor.SetInsertMode(const Value: boolean);
var
  LIntPos: integer;
begin
  if Value = FInsertModeOn then
    Exit;
  if IsInsertModePossible then
  begin
    FInsertModeOn := Value;
    if (FCaretKind = ckAuto) and Focused then
      CreateCaretGlyph;
    if DataSize < 1 then
      Exit;
    if not FInsertModeOn then
    begin
      if ((DataSize mod FBytesPerRow) = 0) and (DataSize > 0) then
        RowCount := RowCount - 1;
      LIntPos := GetPosAtCursor(Col, Row);
      if LIntPos = DataSize then
        SelStart := DataSize - 1;
    end
    else
    begin
      if ((DataSize mod FBytesPerRow) = 0) and (DataSize > 0) then
        RowCount := RowCount + 1;
    end;
    FModifiedBytes.Size := 0;
    Invalidate;
  end;
end;

function TCustomMPHexEditor.GetModified: boolean;
begin
  Result := FModified and ((DataSize > 0) or FileExists(FileName));
end;

procedure TCustomMPHexEditor.SetSelection(DataPos, StartPos, EndPos:
  integer);
begin
  //CheckSelectUnit(StartPos, EndPos);
  FSelEnd := Max(-1, Min(EndPos, DataSize - 1));
  FSelPosition := Max(-1, Min(DataPos, DataSize - 1));
  FSelStart := Max(-1, Min(StartPos, DataSize - 1));
end;

procedure TCustomMPHexEditor.Resize;
begin
  PostMessage(Handle, CM_INTUPDATECARET, 7, 7);
  inherited;
end;

procedure TCustomMPHexEditor.WrongKey;
begin
  if Assigned(FOnInvalidKey) then
    FOnInvalidKey(self);
end;

procedure TCustomMPHexEditor.TopLeftChanged;
begin
  CheckSetCaret;
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(self);
end;

function TCustomMPHexEditor.GetOffsetString(const Position: cardinal): string;
begin
  Result := '';
  if Assigned(FOnGetOffsetText) and (not FOffsetHandler) then
  begin
    FOffsetHandler := True;
    try
      FIsMaxOffset := False;
      FOnGetOffsetText(self, Position, Result)
    finally
      FOffsetHandler := False;
    end;
  end
  else
  begin
    with FOffsetFormat do
    begin
      if Format <> '' then
      begin
        if (MinWidth <> 0) or (Position <> 0) then
        begin
          if FHexLowercase then
            Result := LowerCase(IntToRadixLen(Position div _BytesPerUnit, Radix,
              MinWidth))
          else
            Result := Uppercase(IntToRadixLen(Position div _BytesPerUnit, Radix,
              MinWidth));
        end;
        Result := Prefix + Result + Suffix;
      end;
    end;
  end;
end;

function TCustomMPHexEditor.GetAnyOffsetString(const Position: integer): string;
begin
  if FOffsetFormat.Format = '' then
    Result := IntToRadix(Position, 16)
  else
    Result := GetOffsetString(Position);
end;

function TCustomMPHexEditor.RowHeight: integer;
begin
  Result := DefaultRowHeight;
end;

function TCustomMPHexEditor.GetBookmark(Index: byte): TMPHBookmark;
begin
  if Index > 9 then
    raise EMPHexEditor.Create(ERR_INVALID_BOOKMARK);

  Result := FBookmarks[Index];
end;

procedure TCustomMPHexEditor.SetBookmark(Index: byte; const Value:
  TMPHBookmark);
begin
  SetBookmarkVals(Index, Value.mPosition, Value.mInCharField);
end;

procedure TCustomMPHexEditor.SetBookmarkVals(const Index: byte; const Position:
  integer; const InCharField: boolean);
begin
  if Index > 9 then
    raise EMPHexEditor.Create(ERR_INVALID_BOOKMARK);

  if (FBookmarks[Index].mPosition <> Position) or
    (FBookmarks[Index].mInCharField <> InCharField) then
  begin
    FBookmarks[Index].mPosition := Position;
    FBookmarks[Index].mInCharField := InCharField;
    Invalidate;
  end
  else
  begin
    FBookmarks[Index].mPosition := -1;
    FBookmarks[Index].mInCharField := InCharField;
    Invalidate;
  end;
  BookmarkChanged;
end;

{.$DEFINE TESTCOLOR}// check for unneeded drawings

type
  TestColor = TColor;

procedure TCustomMPHexEditor.Paint;
type
  TKindOfCell = (kocData, kocRuler, kocOffset, kocEmpty);
var
  DrawInfo: TGridDrawInfo;
  LIntCurCol, LIntCurRow: longint;
  LRctWhere: TRect;
  LBoolOddCol: boolean;
  LBoolChanged: boolean;
  LIntDataPos, LIntDataSize: integer;
  LWStrOutput: WideString;
  LColTextColor, LColTextBackColor, LColBackColor: TColor;
  LIntPenWidthSave: integer;
  LrecSize: TSize;

  LBoolDraw: Boolean;

  LBoolFocused: boolean;
  LRect2: TRect;
  LIntLastCol: integer;

  // get the width of a wide text
  function GetTextWidthW: Integer;
  begin
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(LWStrOutput),
      Length(LWStrOutput), LrecSize);
    Result := LRecSize.cx;
  end;

  // render an offset/ruler/fixed cell
  procedure _TextOut;
  begin
    with Canvas, LRctWhere do
    begin
      Brush.Color := TestColor(LColBackColor);
      Font.Color := LColTextColor;
      SetBKColor(Handle, ColorToRGB(TestColor(LColTextBackColor)));
      LRect2 := LRctWhere; //Rect(Left, Top, Left + FCharWidth, Bottom);
      LRect2.Right := Left + FCharWidth;
      //SetTextColor(Handle, ColorToRGB(LColTextColor));

      LBoolDraw := True;
      if Assigned(FOnDrawCell) then
      begin
        if LIntCurCol = 0 then
          FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput,
            LRctWhere, LBoolDraw)
        else
          FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput, LRect2,
            LBoolDraw)
      end;
      if LBoolDraw then
      begin

        FillRect(LRctWhere);
        if LIntCurCol = 0 then
          ExtTextOutW(Handle, Right - GetTextWidthW - 4, Top,
            ETO_CLIPPED or ETO_OPAQUE, @LRctWhere, PWideChar(LWStrOutput),
            Length(LWStrOutput), nil)
        else
          ExtTextOutW(Handle, Left, Top,
            ETO_CLIPPED or ETO_OPAQUE, @LRect2, PWideChar(LWStrOutput),
            Length(LWStrOutput), nil);

      end
      else
        LBoolDraw := True;

    end;
  end;

  // render a data cell
  procedure _TextOutData;
  begin
    with Canvas, LRctWhere do
    begin
      Brush.Color := TestColor(LColBackColor);
      Font.Color := LColTextColor;
      SetBKColor(Handle, ColorToRGB(TestColor(LColTextBackColor)));
      LRect2 := LRctWhere; //Rect(Left, Top, Left + FCharWidth, Bottom);
      LRect2.Right := Left + FCharWidth;
      //SetTextColor(Handle, ColorToRGB(LColTextColor));

      LBoolDraw := True;
      if Assigned(FOnDrawCell) then
      begin
        FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput, LRect2,
          LBoolDraw)
      end;
      if LBoolDraw then
      begin

        FillRect(LRctWhere);
        ExtTextOutW(Handle, Left, Top,
          ETO_CLIPPED or ETO_OPAQUE, @LRect2, PWideChar(LWStrOutput),
          Length(LWStrOutput), nil);

      end
      else
        LBoolDraw := True;

      if FShowDrag and (LIntCurCol = FDropCol) and (LIntCurRow = FDropRow) then
      begin
        LIntPenWidthSave := Pen.Width;
        try
          Pen.Width := 2;
          Pen.Color := LColTextColor;
          MoveTo(Left + 1, Top + 1);
          LineTo(Left + 1, Bottom - 1)
        finally
          Pen.Width := LIntPenWidthSave;
        end;
      end
    end;
  end;

  // draw an offset cell
  procedure DrawOffsetCell;
  var
    LIntLoop: integer;
  begin
    if (LIntCurRow = Row) then
    begin
      LColBackColor := FColors.CurrentOffsetBackground;
      LColTextColor := FColors.CurrentOffset;
    end
    else
    begin
      LColBackColor := FColors.OffsetBackground;
      LColTextColor := Colors.Offset;
    end;
    LColTextBackColor := LColBackColor;

    (* text ausgeben *)
    LWStrOutput := GetOffsetString((LIntCurRow - GRID_FIXED) * FBytesPerRow);
    _TextOut;

    (* auf bookmark prüfen *)
    for LIntLoop := 0 to 9 do
      with FBookmarks[lIntLoop] do
        if (mPosition > -1) and ((mPosition div FBytesPerRow) = (LIntCurRow -
          GRID_FIXED)) then
          with LRctWhere do
            FBookmarkImageList.Draw(Canvas, Left + 3, ((Bottom - Top - 10) div 2)
              + Top, lIntLoop + (10 * integer(mInCharField)));
  end;

  // draw a ruler cell
  procedure DrawRulerCell;
  begin
    if LIntCurCol <> (GRID_FIXED + FBytesPerRowDup) then
    begin
      if LIntCurCol > (GRID_FIXED + FBytesPerRowDup) then
      begin
        LIntDataPos := (LIntCurCol - (GRID_FIXED + FBytesPerRowDup + 1));
        LWStrOutput := FRulerCharString[LIntDataPos + 1];
      end
      else
        LWStrOutput := FRulerString[LIntCurCol - GRID_FIXED + 1];
    end
    else
      LWStrOutput := '  ';
    LColBackColor := FColors.OffsetBackGround;
    if Col = LIntCurCol then
    begin
      LColTextBackColor := FColors.CurrentOffsetBackGround;
      LColTextColor := FColors.CurrentOffset;
    end
    else
    begin
      LColTextBackColor := FColors.OffsetBackGround;
      LColTextColor := FColors.Offset;
    end;
    _TextOut;
  end;

  // draw a hex/char cell
  procedure DrawDataCell(const bIsCharCell, bIsCurrentField: boolean);
  begin
    (*// caret setzen
    if (LIntCurRow = Row) and (LIntCurCol = Col) then
      IntSetCaretPos(LRctWhere.Left, LRctWhere.Top);*)

    LIntDataPos := GetPosAtCursor(LIntCurCol, LIntCurRow);
    FDrawDataPosition := LIntDataPos;
    if bIsCurrentField and (LIntCurCol < LIntLastCol) and
      (LIntCurCol <> FIntLastHexCol) then
      LColBackColor := FColors.FActiveFieldBackground
    else
      LColBackColor := FColors.FBackground;

    // nicht zeichnen, falls keine daten
    if (LIntDataPos < LIntDataSize) then
    begin
      if not bIsCharCell then
      begin // partie hexadecimale
        if ((LIntCurCol - GRID_FIXED) mod 2) = FSwapNibbles then
          LWStrOutput := FHexChars[Data[LIntDataPos] shr 4]
        else
          LWStrOutput := FHexChars[Data[LIntDataPos] and 15]
      end
      else
      begin
        if FUnicodeCharacters then
        begin
          SetLength(LWStrOutput, 1);
          LWStrOutput[1] := #0;
          ReadBuffer(LWStrOutput[1], LIntDataPos, Min(2, LIntDataSize -
            LIntDataPos));
          if FUnicodeBigEndian then
            SwapWideChar(LWStrOutput[1]);
          if (LWStrOutput[1] < #256) and (Char(LWStrOutput[1]) in FMaskedChars)
            then
            LWStrOutput[1] := WideChar(FReplaceUnprintableCharsBy);
        end
        else
          LWStrOutput := TranslateToAnsiChar(Data[LIntDataPos]);
      end;

      // testen ob byte geändert
      LBoolChanged := (HasChanged(LIntDataPos)) or ((FUnicodeCharacters and
        bIsCharCell) and HasChanged(LIntDataPos + 1));
      LBoolOddCol := (((LIntCurCol - GRID_FIXED) div FBytesPerCol) mod 2) = 0;

      if LBoolChanged then
      begin
        LColTextColor := FColors.FChangedText;
        LColTextBackColor := FColors.FChangedBackground;
      end
      else
      begin
        if bIsCurrentField then
          LColTextBackColor := FColors.FActiveFieldBackground
        else
          LColTextBackColor := FColors.FBackground;

        if not FPosInCharField then
        begin
          if LBoolOddCol then
            LColTextColor := Colors.FOddColumn
          else
            LColTextColor := Colors.FEvenColumn;
        end
        else
          LColTextColor := Font.Color;
      end;

      if (FSelPosition <> -1) and IsSelected(LIntDataPos) then
      begin

        FIsDrawDataSelected := True;

        if (not FHideSelection) or LBoolFocused then
        begin
          if (LIntCurCol < LIntLastCol) and (LIntCurCol <> FIntLastHexCol)
            and (LIntDataPos <> Max(FSelStart, FSelEnd)) then
            LColBackColor := Invert(LColBackColor);
          LColTextBackColor := Invert(LColTextBackColor);
          LColTextColor := Invert(LColTextColor);

          if FGraySelOnLostFocus and (not LBoolFocused) then
          begin
            LColTextBackColor := FadeToGray(LColTextBackColor);
            LColTextColor := FadeToGray(LColTextColor);
          end;
        end;
      end

      else
        FIsDrawDataSelected := False
;

      _TextOutData
    end;

    // focus frame auf der anderen seite
    if LBoolFocused then
    begin
      if not FPosInCharField then
      begin
        if (LIntCurRow = Row) then
        begin
          if not FUnicodeCharacters then
          begin
            if GetOtherFieldColCheck(Col) = (LIntCurCol - 1) then
              with LRctWhere do
                if FFocusFrame then
                  Canvas.DrawFocusRect(Rect(
                    CellRect(LIntCurCol-1, LIntCurRow).Left,
                    Top, Left + FCharWidth, Bottom - 1))
                else
                begin
                  Canvas.Pen.Color := FColors.CursorFrame;
                  Canvas.Brush.Style := bsClear;
                  Canvas.Rectangle(CellRect(LIntCurCol-3, LIntCurRow).Left, Top,
                    Left + FCharWidth, Bottom - 1);
                end;
          end
          else if GetOtherFieldColCheck(Col) = (LIntCurCol - 3) then
            with LRctWhere do
              if FFocusFrame then
                Canvas.DrawFocusRect(Rect(
                  CellRect(LIntCurCol-3, LIntCurRow).Left, Top,
                  Left + FCharWidth, Bottom - 1))
              else
              begin
                Canvas.Pen.Color := FColors.CursorFrame;
                Canvas.Brush.Style := bsClear;
                Canvas.Rectangle(CellRect(LIntCurCol-3, LIntCurRow).Left, Top,
                  Left + FCharWidth, Bottom - 1);
              end;
        end;
      end
      else
      begin
        if (LIntCurRow = Row) and (GetOtherFieldColCheck(Col) = LIntCurCol) then
        begin
          with LRctWhere do
            if FFocusFrame then
              Canvas.DrawFocusRect(Rect(Left, Top, Left + FCharWidth, Bottom -
                1))
            else
            begin
              Canvas.Pen.Color := FColors.CursorFrame;
              Canvas.Brush.Style := bsClear;
              Canvas.Rectangle(Left, Top, Left + FCharWidth, Bottom - 1);
            end;
        end;
      end;
    end
    else
    begin
      // possibly draw a mark at the current position when not focused
      if FShowPositionIfNotFocused and (LIntCurRow = Row) and (Col = LIntCurCol)
        then
      begin
        with LRctWhere do
          if FFocusFrame then
            Canvas.DrawFocusRect(Rect(Left, Top, Left + FCharWidth, Bottom - 1))
          else
          begin
            Canvas.Pen.Color := FColors.NonFocusCursorFrame;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle(Left, Top, Left + FCharWidth, Bottom - 1);
          end;
      end;
    end;

    if FDrawGridLines and (LIntCurCol = LIntLastCol) then
      with Canvas, LRctWhere do
      begin
        Pen.Color := FColors.FGrid;
        MoveTo(Right - 1, Top);
        LineTo(Right - 1, Bottom - 1);
      end;

  end;

  // draw
  procedure DrawCells(ACol, ARow: longint; StartX, StartY, StopX, StopY:
    integer;
    Kind: TKindOfCell);
  begin
    LIntCurRow := ARow;
    LRctWhere.Top := StartY;
    while (LRctWhere.Top < StopY) and (LIntCurRow < RowCount) do
    begin
      LIntCurCol := ACol;
      LRctWhere.Left := StartX;
      LRctWhere.Bottom := LRctWhere.Top + RowHeights[LIntCurRow];
      while (LRctWhere.Left < StopX) and (LIntCurCol <= LIntLastCol) do
      begin
        LRctWhere.Right := LRctWhere.Left + ColWidths[LIntCurCol];
        if (LRctWhere.Right > LRctWhere.Left) (*and RectVisible(Canvas.Handle,
        LRctWhere) slows down, removed *)then
        begin
          case Kind of
            kocData:
              begin
                if LIntCurCol < (GRID_FIXED + FBytesPerRowDup) then
                  DrawDataCell(False, not FLastPosInCharField)
                else if LIntCurCol > (GRID_FIXED + FBytesPerRowDup) then
                  DrawDataCell(True, FLastPosInCharField)
                else if FDrawGridLines then
                  with Canvas do
                  begin
                    Pen.Color := FColors.FGrid;
                    MoveTo(LRctWhere.Left, LRctWhere.Top);
                    LineTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                  end;

                if FDrawGridLines then
                  with Canvas do
                  begin
                    Pen.Color := FColors.FGrid;
                    MoveTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                    LineTo(LRctWhere.Right, LRctWhere.Bottom - 1);
                  end;
              end;
            kocEmpty:
              begin
                FDrawDataPosition := -1;
                LColTextBackColor := FColors.OffsetBackGround;
                LColTextColor := FColors.Offset;
                LWStrOutput := '';
                _TextOut;
              end;
            kocRuler:
              begin
                FDrawDataPosition := -1;
                DrawRulerCell;
              end;
            kocOffset:
              begin
                FDrawDataPosition := -1;
                if LIntCurCol = 1 then
                begin
                  if FDrawGridLines then
                    with Canvas do
                    begin
                      Pen.Color := FColors.FGrid;
                      MoveTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                      LineTo(LRctWhere.Right, LRctWhere.Bottom - 1);
                    end;
                end
                else
                  DrawOffsetCell;
              end;
          end;
        end;
        LRctWhere.Left := LRctWhere.Right;
        Inc(LIntCurCol);
      end;
      LRctWhere.Top := LRctWhere.Bottom;
      Inc(LIntCurRow);
    end;
  end;
var
  LIntTop: integer;
begin

{$IFDEF DELPHI6UP}
  if UseRightToLeftAlignment then
    ChangeGridOrientation(True);
{$ENDIF}

  CalcDrawInfo(DrawInfo);
  LBoolFocused := Focused;
  LIntDataSize := DataSize;
  LIntLastCol := GetLastCharCol;
  with DrawInfo do
  begin
    if FShowRuler then
    begin
      // oben links, fixed
      DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, kocEmpty);
      // oben, fixed
      DrawCells(LeftCol, 0, Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary, kocRuler);
    end;
    // links, fixed
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, kocOffset);
    // daten
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary, Vert.FixedBoundary,
      Horz.GridBoundary, Vert.GridBoundary, kocData);

    // paint unoccupied space on the right
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := TestColor(Color);
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent,
        Vert.GridBoundary));

      // fixed (ruler)
      Canvas.Brush.Color := TestColor(FColors.OffsetBackGround);
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, RowHeights[0]
        + RowHeights[1]));
    end;

    // paint unoccupied space on bottom
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      // hex + chars
      Canvas.Brush.Color := TestColor(Color);
      Canvas.FillRect(Rect(ColWidths[0] + 1, Vert.GridBoundary, Horz.GridExtent,
        Vert.GridExtent));

      // fixed (position gutter)
      Canvas.Brush.Color := TestColor(FColors.OffsetBackGround);
      Canvas.FillRect(Rect(0, Vert.GridBoundary, ColWidths[0],
        Vert.GridExtent));
    end;

    LIntTop := RowHeights[0] + RowHeights[1];

    // draw bevel on the right of the offset gutter
    if (ColWidths[0] <> 0) then
    begin
      if FDrawGutter3D then
      begin
        Canvas.MoveTo(ColWidths[0], LIntTop);
        Canvas.Pen.Color := TestColor(clBtnShadow);
        Canvas.LineTo(ColWidths[0], Vert.GridExtent);
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop);
        Canvas.Pen.Color := TestColor(clBtnHighlight);
        Canvas.LineTo(ColWidths[0] - 1, Vert.GridExtent);
      end
      else if FDrawGridLines then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop);
        Canvas.Pen.Color := TestColor(FColors.Grid);
        Canvas.LineTo(ColWidths[0] - 1, Vert.GridExtent);
      end;
    end;

    if (FShowRuler) then
    begin
      if FDrawGutter3D then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 1);
        Canvas.Pen.Color := TestColor(clBtnShadow);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 1);
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 2);
        Canvas.Pen.Color := TestColor(clBtnHighlight);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 2);
      end
      else if FDrawGridLines then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 1);
        Canvas.Pen.Color := TestColor(FColors.Grid);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 1);
      end;
    end;
  end;

{$IFDEF DELPHI6UP}
  if UseRightToLeftAlignment then
    ChangeGridOrientation(False);
{$ENDIF}
end;

procedure TCustomMPHexEditor.SetSelectionAsHex(const s: string);
var
  LStrData: string;
  LIntAmount: integer;
begin
  if s <> '' then
  begin
    SetLength(LStrData, Length(s));
    ConvertHexToBin(@s[1], @LStrData[1], Length(s), SwapNibbles, LIntAmount);
    SetLength(LStrData, LIntAmount);
    SetSelectionAsText(LStrData);
  end;
end;

function TCustomMPHexEditor.GetSelectionAsText: string;
begin
  if (DataSize < 1) or (SelCount < 1) then
    Result := ''
  else
  begin
    SetLength(Result, SelCount);
    FDataStorage.ReadBufferAt(Result[1], Min(SelStart, SelEnd), SelCount);
  end;
end;

procedure TCustomMPHexEditor.SetSelectionAsText(const s: string);
begin
  if s <> '' then
    ReplaceSelection(@s[1], Length(s));
end;

procedure TCustomMPHexEditor.SetDrawGridLines(const Value: boolean);
begin
  if Value <> FDrawGridLines then
  begin
    FDrawGridLines := Value;
    Invalidate;
  end;
end;

function TCustomMPHexEditor.UndoBeginUpdate: integer;
begin
  Result := FUndoStorage.BeginUpdate;
end;

function TCustomMPHexEditor.UndoEndUpdate: integer;
begin
  Result := FUndoStorage.EndUpdate;
end;

function TCustomMPHexEditor.Undo: boolean;
begin
  Result := FUndoStorage.Undo;
end;

function TCustomMPHexEditor.Redo: boolean;
begin
  Result := FUndoStorage.Redo;
end;

procedure TCustomMPHexEditor.SetGutterWidth(const Value: integer);
begin
  if FGutterWidth <> Value then
  begin
    FGutterWidth := Value;
    SetOffsetDisplayWidth;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.BookmarkBitmapChanged(Sender: TObject);
var
  LRctBox: TRect;
begin
  // spalte 1 invalidieren
  FBookmarkImageList.Clear;
  FBookmarkImageList.AddMasked(FBookmarkBitmap, FBookmarkBitmap.Canvas.Pixels[0,
    0]);
  if HandleAllocated then
  begin
    LRctBox := BoxRect(0, TopRow, 0, TopRow + VisibleRowCount);
    InvalidateRect(Handle, @LRctBox, False);
  end;
end;

procedure TCustomMPHexEditor.SetBookmarkBitmap(const Value: TBitmap);
begin
  if Value = nil then
    FBookmarkBitmap.LoadFromResourceName(HINSTANCE, 'BOOKMARKICONS')
  else
  begin
    if (Value.Width <> 200) or (Value.Height <> 10) then
      raise EMPHexEditor.Create(ERR_INVALID_BOOKMARKBMP);
    FBookmarkBitmap.Assign(Value);
  end;
  FHasCustomBMP := Value <> nil;
end;

procedure TCustomMPHexEditor.SelectAll;
var
  LgrcPosition: TGridCoord;
begin
  if DataSize > 0 then
  begin
    // position auf ende stzen
    if (not InsertMode) then
      LgrcPosition := GetCursorAtPos(DataSize - 1, InCharField)
    else
      LgrcPosition := GetCursorAtPos(DataSize, InCharField);
    MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);

    // alles wählen
    NewSelection(0, Pred(DataSize));
  end;
end;

function TCustomMPHexEditor.GetVersion: string;
begin
  Result := MPH_VERSION;
end;

procedure TCustomMPHexEditor.SetVersion(const Value: string);
begin
  // readonly property
end;

procedure TCustomMPHexEditor.FreeStorage(FreeUndo: boolean = False);
begin
  if not FreeUndo then
    FDataStorage.Size := 0
  else
    FUndoStorage.Size := 0;
end;

procedure TCustomMPHexEditor.OldCursor;
begin
  if Length(FCursorList) > 0 then
  begin
    Cursor := FCursorList[Pred(Length(FCursorList))];
    SetLength(FCursorList, PRed(Length(FCursorList)));
  end;
end;

procedure TCustomMPHexEditor.WaitCursor;
begin
  SetLength(FCursorList, Succ(Length(FCursorList)));
  FCursorList[Pred(Length(FCursorList))] := Cursor;
  Cursor := crHourGlass;
end;

function TCustomMPHexEditor.HasCustomBookmarkBitmap: boolean;
begin
  Result := FHasCustomBMP;
end;

procedure TCustomMPHexEditor.PrepareOverwriteDiskFile;
begin
  if FIsFileReadonly then
    raise EFOpenError.CreateFmt(ERR_FILE_READONLY, [FileName]);
end;

procedure TCustomMPHexEditor.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
  SelectionChanged;
end;

procedure TCustomMPHexEditor.SetDrawGutter3D(const Value: boolean);
begin
  if FDrawGutter3D <> Value then
  begin
    FDrawGutter3D := Value;
    Repaint;
  end;
end;

procedure TCustomMPHexEditor.SetShowRuler(const Value: boolean);
begin
  if (FShowRuler <> Value) or (csLoading in ComponentState) then
  begin
    FShowRuler := Value;
    AdjustMetrics;
  end;
end;

function TCustomMPHexEditor.DisplayEnd: integer;
begin
  if DataSize < 1 then
    Result := -1
  else
    Result := Min((DataSize - 1), (DisplayStart - 1) + (VisibleRowCount *
      BytesPerRow));
end;

function TCustomMPHexEditor.DisplayStart: integer;
begin
  if DataSize < 1 then
    Result := -1
  else
    Result := GetPosAtCursor(GRID_FIXED, TopRow);
end;

procedure TCustomMPHexEditor.SetBytesPerUnit(const Value: integer);
begin
  if FBytesPerUnit <> Value then
  begin
    if FUnicodeCharacters and (Value <> 2) then
      raise EMPHexEditor.Create(ERR_INVALID_BPU_U);
    if not (Value in [1, 2, 4, 8]) then
      raise EMPHexEditor.CreateFmt(ERR_INVALID_BPU, [Value]);
    FBytesPerUnit := Value;
    if FRulerBytesPerUnit = -1 then
      FUsedRulerBytesPerUnit := Value;
    with FOffsetFormat do
      if offBytesPerUnit in Flags then
        _BytesPerUnit := FUsedRulerBytesPerUnit;
    AdjustMetrics;
    SetRulerString;
    if (SelCount mod FBytesPerUnit) <> 0 then
      ResetSelection(False);
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetRulerString;
var
  intLoop, intLen: Integer;
  sLoop: string;
begin
  FRulerString := '';
  intLen := 2 * FUsedRulerBytesPerUnit;
  for intLoop := 0 to Pred(FBytesPerRow div FUsedRulerBytesPerUnit) do
  begin
    sLoop := IntToRadixLen(intLoop, FRulerNumberBase, intLen);
    if Length(sLoop) > intLen then
      Delete(sLoop, 1, Length(sLoop) - intLen);
    FRulerString := FRulerString + sLoop;
  end;
  if FHexLowerCase then
    FRulerString := LowerCase(FRulerString)
  else
    FRulerString := UpperCase(FRulerString);
  FRulerCharString := '';
  if FUnicodeCharacters then
    intLen := FUsedRulerBytesPerUnit div 2
  else
    intLen := FUsedRulerBytesPerUnit;
  for intLoop := 0 to Pred(FBytesPerRow div FUsedRulerBytesPerUnit) do
  begin
    sLoop := IntToRadix(intLoop, FRulerNumberBase);
    if Length(sLoop) > intLen then
      Delete(sLoop, 1, Length(sLoop) - intLen)
    else
      while Length(sLoop) < intLen do
        sLoop := ' ' + sLoop;
    FRulerCharString := FRulerCharString + sLoop;
  end;
  if FHexLowerCase then
    FRulerCharString := LowerCase(FRulerCharString)
  else
    FRulerCharString := UpperCase(FRulerCharString);
end;

procedure TCustomMPHexEditor.CheckSelectUnit(var AStart, AEnd: Integer);
begin
  // assure that the selection covers a whole unit
  if AStart <= AEnd then
  begin
    CheckUnit(AStart);
    CheckUnit(AEnd);
    Inc(AEnd, FBytesPerUnit - 1);
    if (AEnd >= DataSize) then
      AEnd := Pred(DataSize);
  end
  else
  begin
    CheckUnit(AEnd);
    CheckUnit(AStart);
    Inc(AStart, FBytesPerUnit - 1);
    if (AStart >= DataSize) then
      AStart := Pred(DataSize);
  end;
end;

// make sure the value is a multiple of FBytesPerUnit

procedure TCustomMPHexEditor.CheckUnit(var AValue: Integer);
begin
  AValue := AValue div FBytesPerUnit * FBytesPerUnit;
end;

procedure TCustomMPHexEditor.SelectionChanged;
begin
  if FSelectionChangedCount = 0 then
    PostMessage(Handle, CM_SELECTIONCHANGED, 0, 0);
  Inc(FSelectionChangedCount);
end;

procedure TCustomMPHexEditor.SyncView(Source: TCustomMPHexEditor;
  SyncOffset: integer = 0);
var
  curPos, SelS, SelE: integer;
  coord: TGridCoord;
begin
  if (Source.BytesPerRow = BytesPerRow) and (Source.BytesPerColumn =
    BytesPerColumn) and (Source.BytesPerUnit = BytesPerUnit) and (Source.DataSize
    = DataSize) and (SyncOffset = 0) then
  begin
    TopRow := Source.TopRow;
    LeftCol := Source.LeftCol;
    MoveColRow(Source.Col, Source.Row, True, False);
  end
  else
  begin
    // get the current view
    curPos := Source.GetCursorPos;
    coord := Source.GetCursorAtPos(curPos, Source.InCharField);
    with Source.CellRect(coord.X, coord.Y) do
      if Left + Bottom = 0 then
      begin
        curPos := Source.GetPositionAtCursor(Source.LeftCol, Source.TopRow) +
          SyncOffset;
        if curPos >= DataSize then
          curPos := Pred(DataSize);
        if curPos < 0 then
          curPos := 0;
        coord := GetCursorAtPos(curPos, Source.InCharField);
        LeftCol := coord.X;
        TopRow := coord.Y;
      end
      else
      begin
        // use this value if visible, left/top otherwise (when wheeling or scrolling)
        curPos := curPos + SyncOffset;
        if curPos >= DataSize then
          curPos := Pred(DataSize);
        if curPos < 0 then
          curPos := 0;
        coord := GetCursorAtPos(curPos, Source.InCharField);
        MoveColRow(coord.X, coord.Y, True, True);
      end;
  end;
  if (Source.SelCount = 0) then
  begin
    if (SelCount <> 0) then
      ResetSelection(True)
  end
  else
  begin
    SelS := Source.FSelStart + SyncOffset;
    SelE := Source.FSelEnd + SyncOffset;
    if SelE >= DataSize then
      SelE := DataSize - 1;
    if SelS >= DataSize then
      SelS := DataSize - 1;
    if SelE < 0 then
      SelE := 0;
    if SelS < 0 then
      SelS := 0;
    NewSelection(SelS, SelE);
  end;
end;

procedure TCustomMPHexEditor.CMSelectionChanged(var Msg: TMessage);
begin
  if (FSelectionChangedCount <> 0) and Assigned(FOnSelectionChanged) then
  try
    FOnSelectionChanged(self);
  finally
    FSelectionChangedCount := 0;
  end;
end;

procedure TCustomMPHexEditor.SetRulerBytesPerUnit(const Value: integer);
begin
  if FRulerBytesPerUnit <> Value then
  begin
    if (not (Value in [1, 2, 4, 8])) and (Value <> -1) then
      raise EMPHexEditor.CreateFmt(ERR_INVALID_RBPU, [Value]);
    FRulerBytesPerUnit := Value;
    if Value = -1 then
      FUsedRulerBytesPerUnit := FBytesPerUnit
    else
      FUsedRulerBytesPerUnit := Value;
    with FOffsetFormat do
      if offBytesPerUnit in Flags then
        _BytesPerUnit := FUsedRulerBytesPerUnit;
    AdjustMetrics;
    SetRulerString;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetShowPositionIfNotFocused(const Value: Boolean);
begin
  if FShowPositionIfNotFocused <> Value then
  begin
    FShowPositionIfNotFocused := Value;
    Invalidate;
  end;
end;

function TCustomMPHexEditor.GetDataAt(Index: integer): Byte;
begin
{$IFDEF FASTACCESS}
{$R-}
  Result := GetFastPointer(Index,1)^;
{$ELSE}
  ReadBuffer(Result, Index, sizeof(Result));
{$ENDIF}
end;

procedure TCustomMPHexEditor.SetDataAt(Index: integer; const Value: Byte);
begin
{$IFDEF FASTACCESS}
  GetFastPointer(Index, 1)^ := Value;
{$ELSE}
  WriteBuffer(Value, Index, sizeof(Value));
{$ENDIF}
end;

procedure TCustomMPHexEditor.ReadBuffer(var Buffer; const Index, Count:
  Integer);
begin
{$IFDEF FASTACCESS}
  Move(GetFastPointer(Index, Count)^, Buffer, Count);
{$ELSE}
  FDataStorage.ReadBufferAt(Buffer, Index, Count);
{$ENDIF}
end;

procedure TCustomMPHexEditor.WriteBuffer(const Buffer; const Index, Count:
  Integer);
begin
{$IFDEF FASTACCESS}
  Move(Buffer, GetFastPointer(Index,Count)^, Count);
{$ELSE}
  FDataStorage.WriteBufferAt(Buffer, Index, Count);
{$ENDIF}
end;

// fire OnBookmarkChanged

procedure TCustomMPHexEditor.BookmarkChanged;
begin
  if Assigned(FOnBookmarkChanged) then
    FOnBookmarkChanged(self);
end;

procedure TCustomMPHexEditor.DoSetCellWidth(const Index: integer;
  Value: integer);
begin
  ColWidths[Index] := Value;
end;

// legacy, do not use

function TCustomMPHexEditor.GetMemory(const Index: Integer): char;
begin
  Result := Char(Data[Index])
end;

// legacy, do not use

procedure TCustomMPHexEditor.SetMemory(const Index: integer; const Value: char);
begin
  Data[Index] := Ord(Value);
end;

procedure TCustomMPHexEditor.SetUnicodeCharacters(const Value: Boolean);
begin
  if FUnicodeCharacters <> Value then
  begin
    if Value then
    begin
      if (BytesPerRow mod 2) <> 0 then
        raise EMPHexEditor.Create(ERR_INVALID_BYTESPERLINE);
      if (BytesPerColumn mod 2) <> 0 then
        raise EMPHexEditor.Create(ERR_INVALID_BYTESPERCOL);
      if (DataSize mod 2) <> 0 then
        raise EMPHexEditor.Create(ERR_ODD_FILESIZE_UNICODE);
      FTranslation := tkAsIs;
    end;
    FUnicodeCharacters := Value;
    ColCount := CalcColCount;
    if Value then
      BytesPerUnit := 2
    else
      BytesPerUnit := 1;

    CalcSizes;
    SetRulerString;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetUnicodeBigEndian(const Value: Boolean);
begin
  if FUnicodeBigEndian <> Value then
  begin
    FUnicodeBigEndian := Value;
    if FUnicodeCharacters then
      Invalidate;
  end;
end;

function TCustomMPHexEditor.GetPositionAtCursor(const ACol,
  ARow: integer): integer;
var
  LBoolInCharField: Boolean;
begin
  LBoolInCharField := FPosInCharField;
  try
    Result := GetPosAtCursor(ACol, ARow);
  finally
    FPosInCharField := (LBoolInCharField);
  end;
end;

function TCustomMPHexEditor.GetIsCharFieldCol(
  const ACol: integer): Boolean;
begin
  Result := ACol > (GRID_FIXED + FBytesPerRowDup);
end;

function TCustomMPHexEditor.IsFileSizeFixed: boolean;
begin
  if FFixedFileSizeOverride then
    Result := False
  else
    Result := FFixedFileSize;
end;

function TCustomMPHexEditor.IsInsertModePossible: boolean;
begin
  Result := (not IsFileSizeFixed) and FAllowInsertMode and (not FReadOnlyView)
end;

function TCustomMPHexEditor.Replace(aBuffer: PChar; aPosition, aOldCount,
  aNewCount: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = False): integer;
var
  LBoolInCharField: boolean;
  LIntSize: integer;
begin
  FDataStorage.CheckBounds((Abs(aPosition) + Abs(aOldCount)) - 1);
  LIntSize := DataSize;
  // auswahl berechnen
  LBoolInCharField := GetInCharField;
  if LIntSize - APosition < aOldCount then
  begin
    if aNewCount = aOldCount then
      aNewCount := LIntSize - APosition;
    aOldCount := LIntSize - APosition;
  end;
  if IsFileSizeFixed then
  begin
    if aOldCount < aNewCount then
      aNewCount := aOldCount
    else
      aOldCount := aNewCount;
  end;

  CreateUndo(ufKindReplace, APosition, aNewCount, aOldCount, UndoDesc);

  if (not MoveCursor) and (FUndoStorage.FUpdateCount = 0) then
    FUndoStorage.AddSelection(APosition, aOldCount);

  if aOldCount = aNewCount then
    WriteBuffer(aBuffer^, APosition, aOldCount)
  else
    if aOldCount > aNewCount then
    begin
      InternalDelete(APosition, APosition + (aOldCount - aNewCount), Col, Row);
      WriteBuffer(aBuffer^, APosition, aNewCount)
    end
    else
    begin
      InternalInsertBuffer(nil, aNewCount-aOldCount, APosition);
      WriteBuffer(aBuffer^, APosition, aNewCount)
    end;
  Result := aNewCount;
  if FModifiedBytes.Size >= APosition then
    FModifiedBytes.Size := Max(0, APosition);

  if MoveCursor then
  begin
    with GetCursorAtPos(APosition, LBoolInCharField) do
      MoveColRow(x, y, True, True);
  end;
  Invalidate;
  Changed;
end;

function TCustomMPHexEditor.GotoBookmark(const Index: integer): boolean;
var
  LIntRow: integer;
  LgrcPosition: TGridCoord;
begin
  Result := False;
  if FBookmarks[Index].mPosition > -1 then
  begin
    ResetSelection(True);
    LIntRow := FBookmarks[Index].mPosition;
    if (LIntRow < DataSize) or ((LIntRow = DataSize) and InsertMode) then
    begin
      LgrcPosition := GetCursorAtPos(LIntRow, FBookmarks[Index].mInCharField);
      MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
      Result := True;
    end
    else
      SetBookmarkVals(Index, -1, False);
  end;
end;

procedure TCustomMPHexEditor.UpdateGetOffsetText;
begin
  SetOffsetDisplayWidth;
  Invalidate;
  CheckSetCaret;
end;

{$IFDEF FASTACCESS}

function TCustomMPHexEditor.GetFastPointer(const Index, Count: integer): PByte;
begin
  Result := FDataStorage.GetAddress(Index, Count);
end;
{$ENDIF}

procedure TCustomMPHexEditor.SeekToEOF;
var
  LgrcPosition: TGridCoord;
begin
  InCharField;
  if (not InsertMode) then
    LgrcPosition := GetCursorAtPos(DataSize - 1, FPosInCharField)
  else
    LgrcPosition := GetCursorAtPos(DataSize, FPosInCharField);
  MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True)
end;

function TCustomMPHexEditor.CanCreateUndo(const aKind: TMPHUndoFlag; const
  aCount,
  aReplCount: integer): Boolean;
begin
  Result := False;
  if DataSize > 0 then
    Result := True;

  if not Result then
    if aKind in [ufKindInsertBuffer, ufKindAppendBuffer, ufKindAllData] then
      Result := True;

  // check for NoSizeChange
  if IsFileSizeFixed and Result then
    if (aKind in [ufKindByteRemoved, ufKindInsertBuffer, ufKindAppendBuffer,
      ufKindNibbleInsert,
        ufKindNibbleDelete]) or
      ((aKind = ufKindReplace) and (aCount <> aReplCount)) then
      Result := False;

  if (not Result) and ((aKind = ufKindCombined) and (FUndoStorage.Count >=
    aCount)) then
    Result := True;

end;

procedure TCustomMPHexEditor.SetDataSize(const Value: integer);
var
  iPos: Integer;
  iSize: integer;
begin
  iSize := DataSize;
  if Value <> iSize then
  begin
    iPos := GetCursorPos;

    // new in 12-16-2003: don't allow change of datasize if nosizechange
    // and (new datasize <> 0 and old datasize <> 0)
    if (Value <> 0) and (iSize <> 0) and IsFileSizeFixed then
      raise EMPHexEditor.Create(ERR_FIXED_FILESIZE);

    FFixedFileSizeOverride := True;
    try
      // new in 12-16-2003: generate undo
      if Value < iSize then
        // create a 'bytes deleted' undo
        CreateUndo(ufKindByteRemoved, Value, DataSize - Value, 0)
      else
        // create a 'append buffer' undo
        CreateUndo(ufKindAppendBuffer, DataSize, Value - DataSize, 0);
      FDataStorage.Size := Value;
{$IFDEF FASTACCESS}
      if Value > iSize then
        // fill the new data block
        FillChar(GetFastPointer(iSize, Value-iSize)^, Value - iSize, FSetDataSizeFillByte);
{$ENDIF}
      FModified := True;
      CalcSizes;
      if iPos > DataSize then
      begin
        ResetSelection(True);
        if (DataSize = 0) and (not InsertMode) then
        begin
          with GetCursorAtPos(0, InCharField) do
            MoveColRow(X, Y, True, True);
        end
        else
          SeekToEOF;
      end;
    finally
      FFixedFileSizeOverride := False;
    end;
  end;
end;

procedure TCustomMPHexEditor.SetBlockSize(const Value: Integer);
begin
  if FBlockSize <> Value then
  begin
    FBlockSize := Value;
    AdjustMetrics;
  end;
end;

procedure TCustomMPHexEditor.SetSepCharBlocks(const Value: boolean);
begin
  if FSepCharBlocks <> Value then
  begin
    FSepCharBlocks := Value;
    if Value and (FBlockSize > 1) then
      AdjustMetrics;
  end;
end;

procedure TCustomMPHexEditor.SetFindProgress(const Value: boolean);
begin
  FFindProgress := Value;
end;

procedure TCustomMPHexEditor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MaskChar', ReadMaskChar, nil, False);
  Filer.DefineProperty('MaskChar_AsInteger', ReadMaskChar_I, WriteMaskChar_I,
    FReplaceUnprintableCharsBy <> '.');
end;

procedure TCustomMPHexEditor.ReadMaskChar(Reader: TReader);
var
  s: string;
begin
  s := Reader.ReadString;
  if Length(s) <> 1 then
    FReplaceUnprintableCharsBy := '.'
  else
  try
    FReplaceUnprintableCharsBy := s[1];
  except
    FReplaceUnprintableCharsBy := '.';
  end;
end;

procedure TCustomMPHexEditor.ReadMaskChar_I(Reader: TReader);
begin
  try
    Byte(FReplaceUnprintableCharsBy) := Reader.ReadInteger;
  except
    FReplaceUnprintableCharsBy := '.';
  end;
end;

procedure TCustomMPHexEditor.WriteMaskChar_I(Writer: TWriter);
begin
  Writer.WriteInteger(Byte(FReplaceUnprintableCharsBy));
end;

function TCustomMPHexEditor.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if Shift <> [] then
    Result := inherited DoMouseWheelDown(Shift, MousePos)
  else
  begin
    // scroll down one page
    TopRow := Min(Max(GRID_FIXED, RowCount - VisibleRowCount),
      TopRow + VisibleRowCount - 1);
    CheckSetCaret;
    Result := True;
  end;
end;

function TCustomMPHexEditor.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if Shift <> [] then
    Result := inherited DoMouseWheelUp(Shift, MousePos)
  else
  begin
    // scroll up one page
    TopRow := Max(GRID_FIXED, TopRow - VisibleRowCount + 1);
    CheckSetCaret;
    Result := True;
  end;
end;

procedure TCustomMPHexEditor.CheckSetCaret;
begin
  with CellRect(Col, Row) do
  begin
    if Left + Bottom = 0 then
      IntSetCaretPos(-50, -50, -1)
    else
      IntSetCaretPos(Left, Top, Col);
  end;
end;

function TCustomMPHexEditor.CanFocus: Boolean;
var
  Form: TCustomForm;
begin
  Result := {$IFDEF DELPHI5UP}inherited CanFocus{$ELSE}True{$ENDIF};
  if Result and not (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self);
    Result := (not Assigned(Form)) or (Form.Enabled and Form.Visible);
  end;
end;

procedure TCustomMPHexEditor.SetRulerNumberBase(const Value: byte);
begin
  if FRulerNumberBase <> Value then
  begin
    // force number that can be represented using '0'-'9','A'-'F'
    if not (Value in [2..16]) then
      FRulerNumberBase := 16
    else
      FRulerNumberBase := Value;
    SetRulerString;
    if FShowRuler then
      Invalidate;
  end;
end;

procedure TCustomMPHexEditor.SetMaskedChars(const Value: TSysCharSet);
begin
  if FMaskedChars <> Value then
  begin
    FMaskedChars := Value;
    Invalidate;
  end;
end;

procedure TCustomMPHexEditor.CenterCursorPosition;
var
  iPos: integer;
begin
  iPos := GetCursorPos;
  iPos := (iPos div FBytesPerRow) + GRID_FIXED;
  TopRow := Max(GRID_FIXED, Min(iPos - (VisibleRowCount div 2), RowCount-VisibleRowCount));
end;

{+}
{.$IFDEF BCB}
procedure TCustomMPHexEditor.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
begin
 { empty }
end;
{.$ENDIF}
{+.}

{ TMPHColors }

procedure TMPHColors.Assign(Source: TPersistent);
begin
  if Source is TMPHColors then
  begin
    Background := TMPHColors(Source).Background;
    ChangedText := TMPHColors(Source).ChangedText;
    CursorFrame := TMPHColors(Source).CursorFrame;
    NonFocusCursorFrame := TMPHColors(Source).NonFocusCursorFrame;
    Offset := TMPHColors(Source).Offset;
    OddColumn := TMPHColors(Source).OddColumn;
    EvenColumn := TMPHColors(Source).EvenColumn;
    ChangedBackground := TMPHColors(Source).ChangedBackground;
    CurrentOffsetBackground := TMPHColors(Source).CurrentOffsetBackground;
    CurrentOffset := TMPHColors(Source).CurrentOffset;
    OffsetBackground := TMPHColors(Source).OffsetBackground;
    ActiveFieldBackground := TMPHColors(Source).ActiveFieldBackground;
    Grid := TMPHColors(Source).Grid;
  end;
end;

constructor TMPHColors.Create(Parent: TControl);
begin
  inherited Create;
  FBackground := clWindow;
  FActiveFieldBackground := clWindow;
  FChangedText := clMaroon;
  FCursorFrame := clNavy;
  FNonFocusCursorFrame := clAqua;
  FOffset := clBlack;
  FOddColumn := clBlue;
  FEvenColumn := clNavy;
  FChangedBackground := $00A8FFFF;
  FCurrentOffsetBackground := clBtnShadow;
  FCurrentOffset := clBtnHighLight;
  FOffsetBackground := clBtnFace;
  FGrid := clBtnFace;
  FParent := Parent;

end;

procedure TMPHColors.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    if Assigned(fParent) then
    begin
      TCustomMPHexEditor(FParent).Color := Value;
      fParent.Invalidate;
    end;
  end;
end;

procedure TMPHColors.SetChangedBackground(const Value: TColor);
begin
  if FChangedBackground <> Value then
  begin
    FChangedBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetCurrentOffsetBackground(const Value: TColor);
begin
  if FCurrentOffsetBackground <> Value then
  begin
    FCurrentOffsetBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetNonFocusCursorFrame(const Value: TColor);
begin
  if FNonFocusCursorFrame <> Value then
  begin
    FNonFocusCursorFrame := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetChangedText(const Value: TColor);
begin
  if FChangedText <> Value then
  begin
    FChangedText := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetCursorFrame(const Value: TColor);
begin
  if FCursorFrame <> Value then
  begin
    FCursorFrame := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetEvenColumn(const Value: TColor);
begin
  if FEvenColumn <> Value then
  begin
    FEvenColumn := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetOddColumn(const Value: TColor);
begin
  if FOddColumn <> Value then
  begin
    FOddColumn := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetOffset(const Value: TColor);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetOffsetBackGround(const Value: TColor);
begin
  if FOffsetBackGround <> Value then
  begin
    FOffsetBackGround := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetCurrentOffset(const Value: TColor);
begin
  if FCurrentOffset <> Value then
  begin
    FCurrentOffset := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetParent(const Value: TControl);
begin
  FParent := Value;
  Assign(self);
end;

procedure TMPHColors.SetGrid(const Value: TColor);
begin
  if FGrid <> Value then
  begin
    FGrid := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TMPHColors.SetActiveFieldBackground(const Value: TColor);
begin
  if FActiveFieldBackground <> Value then
  begin
    FActiveFieldBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

{ TMPHUndoStorage }

type

  // undo storage

  PUndoSelRec = ^TUndoSelRec;
  TUndoSelRec = packed record
    SelStart,
      SelEnd,
      SelPos: integer;
  end;

constructor TMPHUndoStorage.Create(AEditor: TCustomMPHexEditor);
begin
  inherited Create;
  FEditor := AEditor;
  FRedoPointer := nil;
  FLastUndo := nil;
  FLastUndoSize := 0;
  Reset;
end;

destructor TMPHUndoStorage.Destroy;
begin
  Reset;
  inherited;
end;

function TMPHUndoStorage.BeginUpdate: integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

function TMPHUndoStorage.CanUndo: boolean;
begin
  Result := (FCount > 0) and (FUpdateCount < 1) and (Size > 0);
end;

procedure TMPHUndoStorage.CreateUndo(aKind: TMPHUndoFlag; APosition, ACount,
  AReplaceCount: integer; const SDescription: string);
var
  urPos: integer;

  function PUndoRec: PMPHUndoRec;
  begin
    Result := PMPHUndoRec(@(PChar(Memory)[urPos]))
  end;
  //LPurUndoRec: PMPHUndoRec;

  procedure NewFillBuffer(ASize: integer);
  var
    i: integer;
  begin
    i := Position;
    urPos := i;
    (*if FEditor.FSelPosition > -1 then
      ASize := ASize+sizeof(TUndoSelRec);*)

    Size := Size + sizeof(TMPHUndoRec) + ASize;

    FillChar(PUndoRec^, SizeOf(TMPHUndoRec) + ASize, 0);
    with PUndoRec^ do
    begin
      Flags := [aKind];
      CurPos := FEditor.GetPosAtCursor(FEditor.Col, FEditor.Row);
      if not FEditor.FPosInCharField then
        with FEditor.GetCursorAtPos(CurPos, FEditor.FPosInCharField) do
          if (FEditor.Col - x) <> 0 then
            Include(Flags, ufFlag2ndByteCol);
      if FEditor.FPosInCharField then
        Include(Flags, ufFlagInCharField);
      if FEditor.FInsertModeOn then
        Include(Flags, ufFlagInsertMode);
      Pos := aPosition;
      Count := aCount;
      ReplCount := aReplaceCount;
      CurTranslation := FEditor.FTranslation;
      if FEditor.UnicodeChars then
        Include(Flags, ufFlagIsUnicode);
      if FEditor.UnicodeBigEndian then
        Include(Flags, ufFlagIsUnicodeBigEndian);
      CurBPU := FEditor.BytesPerUnit;
      if FEditor.FModified then
        Include(Flags, ufFlagModified);
      if FEditor.FSelPosition > -1 then
        Include(Flags, ufFlagHasSelection);
      if SDescription <> '' then
        Include(Flags, ufFlagHasDescription);
    end;
  end;

  procedure DeleteOldestUndoRec;
  var
    LintRecSize: integer;
  begin
    begin
      if Size < 4 then
      begin
        Size := 0;
        FCount := 0;
      end
      else
      begin
        Seek(0, soFromBeginning);
        Read(LIntRecSize, sizeof(integer));
        if LIntRecSize < sizeof(TMPHUndoRec) then
        begin
          Size := 0;
          FCount := 0;
        end
        else
        begin
          Move(PChar(Memory)[LIntRecSize], Memory^, Size - LIntRecSize);
          Size := Size - LIntRecSize;
          Dec(FCount);
        end;
      end;
    end;
  end;

  procedure UpdateUndoRecord(Length: integer = 0);
  var
    LRecSelection: TUndoSelRec;
    i: integer;
  begin
    PUndoRec^.DataLen := SizeOf(TMPHUndoRec) + Length + 4;
    if ufFlagHasSelection in PUndoRec^.Flags then
      Inc(PUndoRec^.DataLen, sizeof(TUndoSelRec));
    if ufFlagHasDescription in PUndoRec^.Flags then
      Inc(PUndoRec^.DataLen, system.Length(SDescription) + sizeof(i));

    Position := Size;
    if ufFlagHasDescription in PUndoRec^.Flags then
    begin
      write(Sdescription[1], system.Length(SDescription));
      i := system.Length(sDescription);
      write(i, sizeof(i));
      Length := Length + i + sizeof(i);
    end;

    if ufFlagHasSelection in PUndoRec^.Flags then
    begin
      with LRecSelection do
      begin
        SelStart := FEditor.FSelStart;
        SelEnd := FEditor.FSelEnd;
        SelPos := FEditor.FSelPosition;
      end;
      Write(LRecSelection, sizeof(LRecSelection));
      Length := Length + sizeof(LRecSelection);
    end;

    Length := SizeOf(TMPHUndoRec) + 4 + Length;
    Write(Length, 4);
  end;

var
  LPtrBytes: PByteArray;
  LSStDesc: shortstring;
begin
  if FUpdateCount < 1 then
  begin
    ResetRedo;

    if sDescription <> '' then
      FDescription := sDescription
    else
      FDescription := STRS_UNDODESC[aKind];

    while (FEditor.FMaxUndo > 0) and (FCount > 0) and (Size > FEditor.FMaxUndo)
      do
      DeleteOldestUndoRec;

    Position := Size;

    Inc(FCount);

    case aKind of
      ufKindBytesChanged:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          if (aCount = 2) and FEditor.HasChanged(aPosition + 1) then
            Include(PUndoRec.Flags, ufFlagByte2Changed);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindByteRemoved:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          FEditor.AdjustBookmarks(aPosition + aCount, -aCount);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindInsertBuffer:
        begin
          NewFillBuffer(0);
          FEditor.AdjustBookmarks(aPosition, aCount);
          UpdateUndoRecord;
        end;
      ufKindReplace:
        begin
          NewFillBuffer(aReplaceCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aReplaceCount);
          FEditor.AdjustBookmarks(aPosition + aCount, aCount - aReplaceCount);
          UpdateUndoRecord(aReplaceCount - 1);
        end;
      ufKindAppendBuffer:
        begin
          NewFillBuffer(0);
          UpdateUndoRecord;
        end;
      ufKindNibbleInsert:
        begin
          NewFillBuffer(0);
          PUndoRec.Buffer := FEditor.Data[aPosition];
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          UpdateUndoRecord;
        end;
      ufKindNibbleDelete:
        begin
          NewFillBuffer(0);
          PUndoRec.Buffer := FEditor.Data[aPosition];
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          UpdateUndoRecord;
        end;
      ufKindConvert:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindSelection:
        begin
          NewFillBuffer(0);
          PUndoRec^.CurPos := APosition;
          UpdateUndoRecord;
          AddSelection(APosition, ACount);
        end;
      ufKindAllData:
        begin
          aCount := FEditor.DataSize;
          if aCount = 0 then
            NewFillBuffer(0)
          else
            NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          if aCount > 0 then
            FEditor.ReadBuffer(LPtrBytes^, 0, aCount);
          if aCount = 0 then
            UpdateUndoRecord
          else
            UpdateUndoRecord(aCount - 1);
        end;
      ufKindCombined:
        begin
          LSStDesc := sDescription;
          NewFillBuffer(Length(LSStDesc));
          PUndoRec.Buffer := aCount;
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          Move(LSStDesc[0], PUndoRec^.Buffer, Length(LSStDesc) + 1);
          UpdateUndoRecord(Length(LSStDesc));
        end;
    end;
  end;
end;

function TMPHUndoStorage.EndUpdate: integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Result := FUpdateCount;
end;

function TMPHUndoStorage.Undo: boolean;

  procedure PopulateUndo(const aBuffer: TMPHUndoRec);
  var
    LRecSel: TUndoSelRec;
  begin
    with FEditor.GetCursorAtPos(aBuffer.CurPos, ufFlagInCharField in
      aBuffer.Flags) do
    begin
      if not (ufFlagInCharField in aBuffer.Flags) then
        if FEditor.DataSize > 0 then
          if ufFlag2ndByteCol in aBuffer.Flags then
            x := x + 1;

      FEditor.MoveColRow(x, y, True, True);
    end;
    FEditor.FModified := ufFlagModified in aBuffer.Flags;
    FEditor.InsertMode := (ufFlagInsertMode in aBuffer.Flags);
    if ufFlagHasSelection in aBuffer.Flags then
    begin
      Position := Size - 4 - sizeof(LRecSel);
      Read(LRecSel, sizeof(LRecSel));
      with LRecSel do
      begin
        if SelEnd = -1 then
          FEditor.Seek(SelStart, FILE_BEGIN)
        else
          FEditor.SetSelection(SelPos, SelStart, SelEnd);
      end;
    end;
    FEditor.UnicodeChars := (ufFlagIsUnicode in aBuffer.Flags);
    FEditor.UnicodeBigEndian := (ufFlagIsUnicodeBigEndian in aBuffer.Flags);
    if not FEditor.UnicodeChars then
      FEditor.Translation := aBuffer.CurTranslation
    else
      FEditor.FTranslation := aBuffer.CurTranslation;
    FEditor.BytesPerUnit := aBuffer.CurBPU;
    FEditor.Invalidate;
    FEditor.Changed;
  end;

var
  LEnumUndo: TMPHUndoFlag;
  LRecUndo: TMPHUndoRec;
  LIntLoop: integer;
  s: string;
begin
  Result := False;
  if not CanUndo then
  begin
    Reset(False);
    Exit;
  end;

  if Size >= sizeof(TMPHUndoRec) then
  begin
    // letzten eintrag lesen
    LEnumUndo := ReadUndoRecord(LRecUndo, s);
    // redo erstellen
    CreateRedo(LRecUndo);
    case LEnumUndo of
      ufKindBytesChanged:
        begin
          FEditor.WriteBuffer(PChar(Memory)[Position - 1], LRecUndo.Pos,
            LRecUndo.Count);
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          if LRecUndo.Count = 2 then
            FEditor.SetChanged(LRecUndo.Pos + 1, ufFlagByte2Changed in
              LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          FEditor.RedrawPos(LRecUndo.Pos, LRecUndo.Pos + LRecUndo.Count - 1);
          RemoveLastUndo;
        end;
      ufKindByteRemoved:
        begin
          FEditor.InternalInsertBuffer(Pointer(integer(Memory) + Position - 1),
            LRecUndo.Count, LRecUndo.Pos);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos - LRecUndo.Count,
            LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindInsertBuffer:
        begin
          FEditor.InternalDelete(LRecUndo.Pos, LRecUndo.Pos + LRecUndo.Count,
            -1, 0);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos, -LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindSelection:
        begin
          PopulateUndo(LRecUndo);
          RemoveLastUndo;
        end;
      ufKindAllData:
        begin
          FEditor.FDataStorage.Size := LRecUndo.Count;
          FEditor.FDataStorage.WriteBufferAt(Pointer(integer(Memory) + Position
            - 1)^, 0,
            LRecUndo.Count);
          FEditor.CalcSizes;
          PopulateUndo(LRecUndo);
          RemoveLastUndo;
        end;
      ufKindReplace:
        begin
          FEditor.InternalDelete(LRecUndo.Pos, LRecUndo.Pos + LRecUndo.Count,
            -1, 0);
          FEditor.InternalInsertBuffer(Pointer(integer(Memory) + Position - 1),
            LRecUndo.ReplCount, LRecUndo.Pos);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos + LRecUndo.ReplCount,
            LRecUndo.ReplCount - LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            // was:
            // FEditor.FModifiedBytes.Size := Max(0, LRecUndo.Pos - 1);
            // line above might lead to an integer overflow
          begin
            if LRecUndo.Pos > 0 then
              FEditor.FModifiedBytes.Size := LRecUndo.Pos - 1
            else
              FEditor.FModifiedBytes.Size := 0;
          end;

          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindAppendBuffer:
        begin
          FEditor.Col := GRID_FIXED;
          FEditor.FDataStorage.Size := LRecUndo.Pos;
          FEditor.CalcSizes;
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          PopulateUndo(LRecUndo);
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindNibbleInsert:
        begin
          FEditor.InternalDeleteNibble(LRecUndo.Pos, False);
          FEditor.Data[LRecUndo.Pos] := LRecUndo.Buffer;
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.FDataStorage.Size := FEditor.FDataStorage.Size - 1;
          FEditor.CalcSizes;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindNibbleDelete:
        begin
          FEditor.InternalInsertNibble(LRecUndo.Pos, False);
          FEditor.Data[LRecUndo.Pos] := LRecUndo.Buffer;
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.FDataStorage.Size := FEditor.FDataStorage.Size - 1;
          FEditor.CalcSizes;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindConvert:
        begin
          FEditor.WriteBuffer(PChar(Memory)[Position - 1], LRecUndo.Pos,
            LRecUndo.Count);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindCombined:
        begin
          LIntLoop := LRecUndo.Count;
          RemoveLastUndo;
          for LIntLoop := 1 to LIntLoop do
            Undo;
          ResetRedo;
        end;
    end;
  end
  else
    Reset;
end;

procedure TMPHUndoStorage.RemoveLastUndo;
var
  LRecUndo: TMPHUndoRec;
  LSStDesc: shortstring;
  LIntRecOffs: integer;
begin
  if Size < sizeof(TMPHUndoRec) then
    Reset(False)
  else
  begin
    Position := Size - 4;
    Read(LIntRecOffs, 4);
    // restore record in case of a redo
    Seek(-LIntRecOffs, soFromCurrent);
    ReAllocMem(FLastUndo, LIntRecOffs);
    Read(FLastUndo^, LIntRecOffs);
    FLastUndoSize := LIntRecOffs;
    FLastUndoDesc := FDescription;

    // delete last undo record
    SetSize(Max(0, Size - LIntRecOffs));
    Dec(FCount);
    if Size < sizeof(TMPHUndoRec) then
    begin
      Reset(False);
    end
    else
    begin
      if ReadUndoRecord(LRecUndo, FDescription) <> ufKindCombined then
      begin
        if FDescription = '' then
          FDescription := STRS_UNDODESC[GetUndoKind(LRecUndo.Flags)]
      end
      else
      begin
        if LRecUndo.Buffer = 0 then
          LSStDesc := ''
        else
        begin
          Read(LSStDesc[1], LRecUndo.Buffer);
          LSStDesc[0] := char(LRecUndo.Buffer);
        end;
        if LSStDesc = '' then
          FDescription := STRS_UNDODESC[GetUndoKind(LRecUndo.Flags)]
        else
          FDescription := LSStDesc;
      end;
    end;
  end;
end;

procedure TMPHUndoStorage.SetSize(NewSize: integer);
begin
  inherited;
  if NewSize < sizeof(TMPHUndoRec) then
    FCount := 0;
end;

procedure TMPHUndoStorage.Reset(AResetRedo: boolean = True);
begin
  Size := 0;
  FCount := 0;
  FUpdateCount := 0;
  FDescription := '';
  if AResetRedo then
    ResetRedo;
end;

procedure TMPHUndoStorage.SetCount(const Value: integer);
begin
  FCount := Value;
  if FCount < 1 then
    Reset(False);
end;

function TMPHUndoStorage.CanRedo: boolean;
begin
  Result := Assigned(FRedoPointer);
end;

function TMPHUndoStorage.Redo: boolean;

  procedure SetEditorStateFromRedoRec(const _2Bytes: Boolean = False);
  begin
    with FRedoPointer^ do
    begin
      Move(PChar(FRedoPointer)[FRedoPointer^.DataLen], FEditor.FBookmarks,
        sizeof(TMPHBookmarks));

      with FEditor.GetCursorAtPos(CurPos, ufFlagInCharField in Flags) do
      begin
        if not (ufFlagInCharField in Flags) then
          if FEditor.DataSize > 0 then
            if ufFlag2ndByteCol in Flags then
              x := x + 1;

        FEditor.MoveColRow(x, y, True, True);
      end;
      FEditor.FModified := ufFlagModified in Flags;
      FEditor.InsertMode := (ufFlagInsertMode in Flags);

      with PUndoSelRec(@(PChar(FRedoPointer)[FRedoPointer^.DataLen +
        sizeof(TMPHBookmarks)]))^ do
        FEditor.SetSelection(SelPos, SelStart, SelEnd);

      FEditor.Translation := CurTranslation;
      FEditor.FTranslation := CurTranslation;
      FEditor.UnicodeChars := (ufFlagIsUnicode in Flags);
      FEditor.UnicodeBigEndian := (ufFlagIsUnicodeBigEndian in Flags);
      FEditor.BytesPerUnit := CurBPU;

      FEditor.InCharField := ufFlagInCharField in Flags;

      FEditor.SetChanged(Pos, ufFlagByte1Changed in Flags);
      if _2Bytes then
        FEditor.SetChanged(Pos + 1, ufFlagByte2Changed in Flags);

      // restore last undo record
      if Assigned(FLastUndo) then
      begin
        Seek(0, soFromEnd);
        Write(FLastUndo^, FLastUndoSize);
        Inc(FCount);
        FreeMem(FLastUndo);
        FLastUndo := nil;
        FLastUndoSize := 0;
      end;
      FDescription := FLastUndoDesc;

      FEditor.Invalidate;
      FEditor.BookmarkChanged;
    end;
  end;
begin
  Result := CanRedo;
  if Result then
  begin
    case GetUndoKind(FRedoPointer^.Flags) of
      ufKindBytesChanged:
        begin
          FEditor.WriteBuffer(FRedoPointer^.Buffer,
            FRedoPointer^.Pos, FRedoPointer^.Count);
          SetEditorStateFromRedoRec(FRedoPointer^.Count = 2);
        end;
      ufKindByteRemoved:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.Count, -1, 0);
          SetEditorStateFromRedoRec;
        end;
      ufKindInsertBuffer:
        begin
          FEditor.InternalInsertBuffer(PChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindSelection:
        begin
          SetEditorStateFromRedoRec;
        end;
      ufKindAllData:
        begin
          FEditor.FDataStorage.Size := FRedoPointer^.Count;
          FEditor.FDataStorage.WriteBufferAt(FRedoPointer^.Buffer, 0,
            FRedoPointer^.Count);
          FEditor.CalcSizes;
          SetEditorStateFromRedoRec;
        end;
      ufKindReplace:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.ReplCount, -1, 0);
          FEditor.InternalInsertBuffer(PChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindConvert:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.Count, -1, 0);
          FEditor.InternalInsertBuffer(PChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindAppendBuffer:
        begin
          FEditor.InternalAppendBuffer(PChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count);
          SetEditorStateFromRedoRec;
        end;
      ufKindNibbleInsert,
        ufKindNibbleDelete:
        begin
          FEditor.FDataStorage.Size := FRedoPointer^.Count;
          FEditor.FDataStorage.WriteBufferAt(FRedoPointer^.Buffer, 0,
            FRedoPointer^.Count);
          FEditor.CalcSizes;
          SetEditorStateFromRedoRec;
        end;
    end;
    ResetRedo;
    FEditor.Changed;
  end;
end;

procedure TMPHUndoStorage.ResetRedo;
begin
  if Assigned(FRedoPointer) then
    FreeMem(FRedoPointer);
  FRedoPointer := nil;
  if Assigned(FLastUndo) then
    FreeMem(FLastUndo);
  FLastUndo := nil;
  FLastUndoSize := 0;
  FLastUndoDesc := '';
end;

procedure TMPHUndoStorage.CreateRedo(const Rec: TMPHUndoRec);
var
  LIntDataSize: integer;

  procedure AllocRedoPointer;
  begin
    GetMem(FRedoPointer, sizeof(TMPHUndoRec) + sizeof(TMPHBookMarks) +
      sizeof(TUndoSelRec) + LIntDataSize);
    FRedoPointer^.Flags := [GetUndoKind(Rec.Flags)];
    FRedoPointer^.DataLen := sizeof(TMPHUndoRec) + LIntDataSize;
  end;

  procedure FinishRedoPointer;
  begin
    with FRedoPointer^ do
    begin
      CurPos := FEditor.GetPosAtCursor(FEditor.Col, FEditor.Row);
      if not FEditor.FPosInCharField then
        with FEditor.GetCursorAtPos(CurPos, FEditor.FPosInCharField) do
          if (FEditor.Col - x) <> 0 then
            Include(Flags, ufFlag2ndByteCol);
      if FEditor.FPosInCharField then
        Include(Flags, ufFlagInCharField);
      if FEditor.FInsertModeOn then
        Include(Flags, ufFlagInsertMode);
      Pos := Rec.pos;
      Count := Rec.Count;
      ReplCount := Rec.ReplCount;
      CurTranslation := FEditor.FTranslation;
      if FEditor.UnicodeChars then
        Include(Flags, ufFlagIsUnicode);
      if FEditor.UnicodeBigEndian then
        Include(Flags, ufFlagIsUnicodeBigEndian);
      CurBPU := FEditor.BytesPerUnit;
      if FEditor.FModified then
        Include(Flags, ufFlagModified);
    end;
    Move(FEditor.FBookmarks, PChar(FRedoPointer)[FRedoPointer^.DataLen],
      sizeof(TMPHBookmarks));
    with PUndoSelRec(@(PChar(FRedoPointer)[FRedoPointer^.DataLen +
      sizeof(TMPHBookmarks)]))^ do
    begin
      SelStart := FEditor.FSelStart;
      SelPos := FEditor.FSelPosition;
      SelEnd := FEditor.FSelEnd;
    end;
  end;
begin
  ResetRedo;
  // simple redo, store bookmarks, selection, insertmode, col, row, charfield...
  // and bytes to save

  case GetUndoKind(Rec.Flags) of
    ufKindBytesChanged:
      begin
        LIntDataSize := Rec.Count - 1;
        AllocRedoPointer;
        if FEditor.HasChanged(Rec.Pos) then
          Include(FRedoPointer^.Flags, ufFlagByte1Changed);
        if Rec.Count = 2 then
          if FEditor.HasChanged(Rec.Pos + 1) then
            Include(FRedoPointer^.Flags, ufFlagByte2Changed);
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, Rec.Count);
        FinishRedoPointer;
      end;
    ufKindByteRemoved:
      begin
        LIntDataSize := 0;
        AllocRedoPointer;
        FinishRedoPointer;
      end;
    ufKindInsertBuffer,
      ufKindReplace,
      ufKindConvert:
      begin
        LIntDataSize := Rec.Count;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, Rec.Count);
        FinishRedoPointer;
      end;
    ufKindSelection:
      begin
        LIntDataSize := 0;
        AllocRedoPointer;
        FinishRedoPointer;
      end;
    ufKindAllData:
      begin
        LIntDataSize := FEditor.DataSize;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, 0, FEditor.DataSize);
        FinishRedoPointer;
        FRedoPointer^.Count := FEditor.DataSize;
      end;
    ufKindAppendBuffer:
      begin
        LIntDataSize := FEditor.DataSize - integer(Rec.Pos);
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, FEditor.DataSize -
          integer(Rec.Pos));
        FinishRedoPointer;
      end;
    ufKindNibbleInsert,
      ufKindNibbleDelete:
      begin
        LIntDataSize := FEditor.DataSize;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, 0, FEditor.DataSize);
        FinishRedoPointer;
        FRedoPointer^.Count := LIntDataSize;
      end;
  end;
  //FEditor.Changed;
end;

function TMPHUndoStorage.GetUndoKind(const Flags: TMPHUndoFlags): TMPHUndoFlag;
begin
  for Result := ufKindBytesChanged to ufKindAllData do
    if Result in Flags then
      Break;
end;

procedure TMPHUndoStorage.AddSelection(const APos, ACount: integer);
var
  P: PMPHUndoRec;
  PSel: PUndoSelRec;
  LIntRecOffset: integer;
begin
  if CanUndo then
  begin
    Position := Size - 4;
    Read(LIntRecOffset, 4);
    Seek(-LIntRecOffset, soFromCurrent);
    P := Pointer(Integer(Memory) + Position);
    if not (ufFlagHasSelection in P^.Flags) then
    begin
      Size := Size + SizeOf(TUndoSelRec);
      P := Pointer(Integer(Memory) + Position);
      Include(P^.Flags, ufFlagHasSelection);
      Inc(P^.DataLen, sizeof(TUndoSelRec));
      Inc(LIntRecOffset, sizeof(TUndoSelRec));
      Seek(-4, soFromEnd);
      WriteBuffer(LIntRecOffset, 4);
    end;
    P^.CurPos := APos;
    PSel := Pointer(Integer(Memory) + size - 4 - sizeof(TUndoSelRec));
    PSel^.SelStart := APos;
    if aCount = 0 then
      PSel^.SelEnd := -1
    else
      PSel^.SelEnd := APos + Acount - 1;
    PSel^.SelPos := PSel^.SelStart;
  end;
end;

function TMPHUndoStorage.ReadUndoRecord(
  var aUR: TMPHUndoRec; var SDescription: string): TMPHUndoFlag;
var
  LIntRecOffs: integer;
  LIntPos: integer;
begin
  Position := Size - 4;
  Read(LIntRecOffs, 4);
  Seek(-LIntRecOffs, soFromCurrent);
  Read(aUR, SizeOf(TMPHUndoRec));
  Result := GetUndoKind(aUr.Flags);
  if ufFlagHasDescription in aUr.Flags then
  begin
    LIntPos := Position;
    try
      Position := size - 4 - sizeof(integer);
      if ufFlagHasSelection in aUr.Flags then
        Seek(-sizeof(TUndoSelRec), soFromCurrent);
      Read(LIntRecOffs, sizeof(integer));
      Seek(-(LIntRecOffs + sizeof(integer)), soFromCurrent);
      SetLength(SDescription, LIntRecOffs);
      Read(SDescription[1], LIntRecOffs);
    finally
      Position := LIntPos;
    end;
  end
  else
    SDescription := '';
end;

function TMPHUndoStorage.GetLastUndoKind: TMPHUndoFlag;
var
  recUndo: TMPHUndoRec;
  s: string;
begin
  Result := ReadUndoRecord(recUndo, s);
end;

// initialize tkCustom translation tables

procedure InitializeCustomTables;
var
  LBytLoop: byte;
begin
  for LBytLoop := 0 to 255 do
  begin
    MPHCustomCharConv[cctFromAnsi][LBytLoop] := char(LBytLoop);
    MPHCustomCharConv[cctToAnsi][LBytLoop] := char(LBytLoop);
  end;
end;

{ TMPHMemoryStream }

const
  MAX_PER_BLOCK = $F000;

procedure TMPHMemoryStream.CheckBounds(const AMax: Integer);
begin
  if (AMax < 0) or (AMax > Size) then
    raise EMPHexEditor.Create(ERR_DATA_BOUNDS);
end;

{$IFDEF FASTACCESS}
function TMPHMemoryStream.GetAddress(const Index, Count: integer): PByte;
begin
  if (Index < 0) or ((Index+Count) > Size) then
    raise EMPHexEditor.Create(ERR_DATA_BOUNDS);
  Result := Pointer(Integer(Memory)+Index);
end;
{$ENDIF}

function TMPHMemoryStream.GetAsHex(const APosition, ACount: integer;
  const SwapNibbles: Boolean): string;
begin
  CheckBounds(APosition + ACount);
  SetLength(Result, ACount * 2);
  if ACount > 0 then
    ConvertBinToHex(PointerAt(APosition), @Result[1], ACount, SwapNibbles);
end;

procedure TMPHMemoryStream.Move(const AFromPos, AToPos, ACount: Integer);
begin
  MoveMemory(PointerAt(AToPos), PointerAt(AFromPos), ACount);
end;

function TMPHMemoryStream.PointerAt(const APosition: Integer): Pointer;
begin
  Result := Pointer(LongInt(Memory) + APosition);
end;

procedure TMPHMemoryStream.ReadBufferAt(var Buffer; const APosition,
  ACount: Integer);
var
  LIntPos: Integer;
begin
  CheckBounds(APosition + ACount);
  LIntPos := Position;
  try
    Position := APosition;
    ReadBuffer(Buffer, ACount);
  finally
    Position := LIntPos;
  end;
end;

procedure TMPHMemoryStream.TranslateFromAnsi(const ToTranslation:
  TMPHTranslationKind; const APosition, ACount: integer);
begin
  if ToTranslation = tkAsIs then
    Exit; // no translation needed
  CheckBounds(APosition + ACount);
  if ACount > 0 then
    TranslateBufferFromAnsi(ToTranslation, PointerAt(APosition),
      PointerAt(APosition), ACount);
end;

procedure TMPHMemoryStream.TranslateToAnsi(const FromTranslation:
  TMPHTranslationKind; const APosition, ACount: integer);
begin
  if FromTranslation = tkAsIs then
    Exit; // no translation needed
  CheckBounds(APosition + ACount);
  if ACount > 0 then
    TranslateBufferToAnsi(FromTranslation, PointerAt(APosition),
      PointerAt(APosition), ACount);
end;

procedure TMPHMemoryStream.WriteBufferAt(const Buffer; const APosition,
  ACount: Integer);
var
  LIntPos: Integer;
begin
  CheckBounds(APosition + ACount);
  LIntPos := Position;
  try
    Position := APosition;
    WriteBuffer(Buffer, ACount);
  finally
    Position := LIntPos;
  end;
end;

initialization

  // initialize custom tables

  InitializeCustomTables;

end.
