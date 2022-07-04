// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zlib.pas' rev: 3.00

#ifndef ZlibHPP
#define ZlibHPP
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zlib
{
//-- type declarations -------------------------------------------------------
struct inflate_huft;
typedef inflate_huft *pInflate_huft;

struct inflate_huft
{
	Byte Exop;
	Byte bits;
	Cardinal base;
} ;

typedef inflate_huft huft_field[268435455];

typedef huft_field *huft_ptr;

typedef pInflate_huft *ppInflate_huft;

enum inflate_codes_mode { START, LEN, LENEXT, DIST, DISTEXT, COPY, LIT, WASH, ZEND, BADCODE };

struct inflate_codes_state;
typedef inflate_codes_state *pInflate_codes_state;

struct Zlib__2
{
	inflate_huft *tree;
	Cardinal need;
} ;

struct Zlib__3
{
	Cardinal get;
	Cardinal dist;
} ;

struct Zlib__1
{
	
	union
	{
		Zlib__3 copy;
		Cardinal lit;
		Zlib__2 code;
		
	};
} ;

struct inflate_codes_state
{
	inflate_codes_mode mode;
	Cardinal len;
	Zlib__1 sub;
	Byte lbits;
	Byte dbits;
	inflate_huft *ltree;
	inflate_huft *dtree;
} ;

typedef int __fastcall (*check_func)(int check, Zutil::pBytef buf, Cardinal len);

enum inflate_block_mode { ZTYPE, LENS, STORED, TABLE, BTREE, DTREE, CODES, DRY, BLKDONE, BLKBAD };

struct inflate_blocks_state;
typedef inflate_blocks_state *pInflate_blocks_state;

struct Zlib__5
{
	Cardinal table;
	Cardinal index;
	Zutil::zuIntArray *blens;
	Cardinal bb;
	inflate_huft *tb;
} ;

struct Zlib__6
{
	inflate_huft *tl;
	inflate_huft *td;
	inflate_codes_state *codes;
} ;

struct Zlib__4
{
	
	union
	{
		Zlib__6 decode;
		Zlib__5 trees;
		Cardinal left;
		
	};
} ;

struct inflate_blocks_state
{
	inflate_block_mode mode;
	Zlib__4 sub;
	bool last;
	Cardinal bitk;
	int bitb;
	huft_field *hufts;
	Byte *window;
	Byte *zend;
	Byte *read;
	Byte *write;
	check_func checkfn;
	int check;
} ;

enum inflate_mode { METHOD, FLAG, DICT4, DICT3, DICT2, DICT1, DICT0, BLOCKS, CHECK4, CHECK3, CHECK2, 
	CHECK1, DONE, BAD };

struct internal_state;
typedef internal_state *pInternal_state;

struct Zlib__8
{
	int was;
	int need;
} ;

struct Zlib__7
{
	
	union
	{
		Cardinal marker;
		Zlib__8 check;
		Cardinal method;
		
	};
} ;

struct internal_state
{
	inflate_mode mode;
	Zlib__7 sub;
	bool nowrap;
	Cardinal wbits;
	inflate_blocks_state *blocks;
} ;

typedef void * __fastcall (*alloc_func)(void * opaque, Cardinal items, Cardinal size);

typedef void __fastcall (*free_func)(void * opaque, void * address);

struct z_stream;
typedef z_stream *z_streamp;

struct z_stream
{
	Byte *next_in;
	Cardinal avail_in;
	int total_in;
	Byte *next_out;
	Cardinal avail_out;
	int total_out;
	System::AnsiString msg;
	internal_state *state;
	alloc_func zalloc;
	free_func zfree;
	void *opaque;
	int data_type;
	int adler;
	int reserved;
} ;

//-- var, const, procedure ---------------------------------------------------
#define MAX_MEM_LEVEL (Byte)(8)
#define DEF_MEM_LEVEL (Byte)(8)
#define MAX_WBITS (Byte)(15)
#define DEF_WBITS (Byte)(15)
#define Z_NO_FLUSH (Byte)(0)
#define Z_PARTIAL_FLUSH (Byte)(1)
#define Z_SYNC_FLUSH (Byte)(2)
#define Z_FULL_FLUSH (Byte)(3)
#define Z_FINISH (Byte)(4)
#define Z_OK (Byte)(0)
#define Z_STREAM_END (Byte)(1)
#define Z_NEED_DICT (Byte)(2)
#define Z_ERRNO (Shortint)(-1)
#define Z_STREAM_ERROR (Shortint)(-2)
#define Z_DATA_ERROR (Shortint)(-3)
#define Z_MEM_ERROR (Shortint)(-4)
#define Z_BUF_ERROR (Shortint)(-5)
#define Z_VERSION_ERROR (Shortint)(-6)
#define Z_NO_COMPRESSION (Byte)(0)
#define Z_BEST_SPEED (Byte)(1)
#define Z_BEST_COMPRESSION (Byte)(9)
#define Z_DEFAULT_COMPRESSION (Shortint)(-1)
#define Z_FILTERED (Byte)(1)
#define Z_HUFFMAN_ONLY (Byte)(2)
#define Z_DEFAULT_STRATEGY (Byte)(0)
#define Z_BINARY (Byte)(0)
#define Z_ASCII (Byte)(1)
#define Z_UNKNOWN (Byte)(2)
#define Z_DEFLATED (Byte)(8)
#define Z_NULL (void *)(0x0)
#define STORED_BLOCK (Byte)(0)
#define STATIC_TREES (Byte)(1)
#define DYN_TREES (Byte)(2)
#define MIN_MATCH (Byte)(3)
#define MAX_MATCH (Word)(258)
#define PRESET_DICT (Byte)(32)
extern PACKAGE System::SmallString<10>  ZLIB_VERSION;
#define z_errbase (Byte)(2)
extern PACKAGE System::SmallString<21>  z_errmsg[10];
extern PACKAGE int z_verbose;
extern PACKAGE System::AnsiString __fastcall zError(int err);
extern PACKAGE System::AnsiString __fastcall zlibVersion();
extern PACKAGE void __fastcall Trace(System::AnsiString x);
extern PACKAGE void __fastcall Tracev(System::AnsiString x);
extern PACKAGE void __fastcall Tracevv(System::AnsiString x);
extern PACKAGE void __fastcall Tracevvv(System::AnsiString x);
extern PACKAGE void __fastcall Tracec(bool c, System::AnsiString x);
extern PACKAGE void __fastcall Tracecv(bool c, System::AnsiString x);
extern PACKAGE void * __fastcall ZALLOC(z_stream &strm, Cardinal items, Cardinal size);
extern PACKAGE void __fastcall ZFREE(z_stream &strm, void * ptr);
extern PACKAGE void __fastcall TRY_FREE(z_stream &strm, void * ptr);

}	/* namespace Zlib */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zlib;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Zlib
