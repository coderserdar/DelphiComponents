// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'trees.pas' rev: 3.00

#ifndef treesHPP
#define treesHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Trees
{
//-- type declarations -------------------------------------------------------
struct ct_data;
typedef ct_data *ct_data_ptr;

struct trees__1
{
	
	union
	{
		Word code;
		Word freq;
		
	};
} ;

struct trees__2
{
	
	union
	{
		Word len;
		Word dad;
		
	};
} ;

struct ct_data
{
	trees__1 fc;
	trees__2 dl;
} ;

typedef ct_data ltree_type[573];

typedef ct_data dtree_type[61];

typedef ct_data htree_type[39];

typedef ct_data tree_type[536870911];

typedef tree_type *tree_ptr;

typedef ltree_type *ltree_ptr;

typedef dtree_type *dtree_ptr;

typedef htree_type *htree_ptr;

struct static_tree_desc;
typedef static_tree_desc *static_tree_desc_ptr;

struct static_tree_desc
{
	tree_type *static_tree;
	Zutil::zIntfArray *extra_bits;
	int extra_base;
	int elems;
	int max_length;
} ;

struct tree_desc;
typedef tree_desc *tree_desc_ptr;

struct tree_desc
{
	tree_type *dyn_tree;
	int max_code;
	static_tree_desc *stat_desc;
} ;

typedef Word Pos;

typedef Word Posf;

typedef Cardinal IPos;

typedef Word *pPosf;

typedef Word zPosfArray[1073741823];

typedef zPosfArray *pzPosfArray;

struct deflate_state;
typedef deflate_state *deflate_state_ptr;

struct deflate_state
{
	Bzlib::z_stream *strm;
	int status;
	Zutil::zByteArray *pending_buf;
	int pending_buf_size;
	Byte *pending_out;
	int pending;
	int noheader;
	Byte data_type;
	Byte method;
	int last_flush;
	Cardinal w_size;
	Cardinal w_bits;
	Cardinal w_mask;
	Zutil::zByteArray *window;
	int window_size;
	zPosfArray *prev;
	zPosfArray *head;
	Cardinal ins_h;
	Cardinal hash_size;
	Cardinal hash_bits;
	Cardinal hash_mask;
	Cardinal hash_shift;
	int block_start;
	Cardinal match_length;
	Cardinal prev_match;
	bool match_available;
	Cardinal strstart;
	Cardinal match_start;
	Cardinal lookahead;
	Cardinal prev_length;
	Cardinal max_chain_length;
	int level;
	int strategy;
	Cardinal good_match;
	int nice_match;
	ct_data dyn_ltree[573];
	ct_data dyn_dtree[61];
	ct_data bl_tree[39];
	tree_desc l_desc;
	tree_desc d_desc;
	tree_desc bl_desc;
	Word bl_count[16];
	int heap[573];
	int heap_len;
	int heap_max;
	Byte depth[573];
	Zutil::zByteArray *l_buf;
	Cardinal lit_bufsize;
	Cardinal last_lit;
	Zutil::zushfArray *d_buf;
	int opt_len;
	int static_len;
	int compressed_len;
	Cardinal matches;
	int last_eob_len;
	Word bi_buf;
	int bi_valid;
	union
	{
		Cardinal max_insert_length;
		Cardinal max_lazy_match;
		
	};
} ;

//-- var, const, procedure ---------------------------------------------------
#define LENGTH_CODES (Byte)(29)
#define LITERALS (Word)(256)
#define L_CODES (Word)(286)
#define D_CODES (Byte)(30)
#define BL_CODES (Byte)(19)
#define HEAP_SIZE (Word)(573)
#define MAX_BITS (Byte)(15)
#define INIT_STATE (Byte)(42)
#define BUSY_STATE (Byte)(113)
#define FINISH_STATE (Word)(666)
extern PACKAGE void __fastcall _tr_init(deflate_state &s);
extern PACKAGE void __fastcall _tr_stored_block(deflate_state &s, Zutil::pcharf buf, int stored_len, 
	bool eof);
extern PACKAGE void __fastcall _tr_align(deflate_state &s);
extern PACKAGE int __fastcall _tr_flush_block(deflate_state &s, Zutil::pcharf buf, int stored_len, bool 
	eof);
extern PACKAGE bool __fastcall _tr_tally(deflate_state &s, Cardinal dist, Cardinal lc);

}	/* namespace Trees */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Trees;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// trees
