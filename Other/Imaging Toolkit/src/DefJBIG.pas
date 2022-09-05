// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  30061: DefJBIG.pas 
//
//    Rev 1.0    03-01-2009 17:09:42  mcm
// Initial JBIG implementation
unit DefJBIG;

interface

uses mcmImageTypeDef;

const

  JBIG_MX_MAX          = 127; // Maximal supported mx offset for adaptive template in the encoder

  JBIG_TPB2CX          = $0195;  // Contexts for TP special pixels
  JBIG_TPB3CX          = $00e5;
  JBIG_TPDCX           = $0c3f;

  // Marker codes
  JBIG_MARKER_STUFF    = $000;
  JBIG_MARKER_RESERVE  = $001;
  JBIG_MARKER_SDNORM   = $002;
  JBIG_MARKER_SDRST    = $003;
  JBIG_MARKER_ABORT    = $004;
  JBIG_MARKER_NEWLEN   = $005;
  JBIG_MARKER_ATMOVE   = $006;
  JBIG_MARKER_COMMENT  = $007;
  JBIG_MARKER_ESC      = $0FF;

  // Loop array indices
  JBIG_STRIPE          = 0;
  JBIG_LAYER           = 1;
  JBIG_PLANE           = 2;
(*
  // Special jbg_buf pointers (instead of NULL)
  JBIG_SDE_DONE ((struct jbg_buf * ) -1)
  JBIG_SDE_TODO ((struct jbg_buf * ) 0)

  // Object code version id
  const char jbg_version[] =
  " JBIG-KIT " JBG_VERSION " -- Markus Kuhn -- "
"$Id: jbig.c,v 1.22 2004-06-11 15:17:06+01 mgk25 Exp $ ";

/*
 * the following array specifies for each combination of the 3
 * ordering bits, which ii[] variable represents which dimension
 * of s->sde.
 */
static const int iindex[8][3] = {
  { 2, 1, 0 },    /* no ordering bit set */
  { -1, -1, -1},  /* SMID -> illegal combination */
  { 2, 0, 1 },    /* ILEAVE */
  { 1, 0, 2 },    /* SMID + ILEAVE */
  { 0, 2, 1 },    /* SEQ */
  { 1, 2, 0 },    /* SEQ + SMID */
  { 0, 1, 2 },    /* SEQ + ILEAVE */
  { -1, -1, -1 }  /* SEQ + SMID + ILEAVE -> illegal combination */
};

*)

type

//------------------------------------------------------------------------------
// JBIG Header
//------------------------------------------------------------------------------

  TJBIGHeader = record
                  DL        : byte;     // Initial resolution layer
                  D         : byte;     // Final resolution layer described in this BIE
                  BitPlanes : byte;     // Number of bit-planes. 1 for bi-level images
                  NotUsed   : byte;     // MUST be zero.
                  Width     : longword; // Horizontal dimension at highest resolution
                  Height    : longword; // Vertical dimension at highest resolution
                  l0        : longword; // Number of lines per stripe at lowest resolution.
                  mx        : byte;     // Maximum horizontal offsets allowed
                  my        : byte;     // Maximum vertical offsets allowed
                  Order     : byte;     // Carry the binary parameters HITOLO, SEQ, ILEAVE and SMID
                  Options   : byte;     //
                end;
implementation

end.
