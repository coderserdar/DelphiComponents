
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareOpenGL;

interface

{$I BARE.INC}

uses
	Windows;

type
  HGLRC = THandle;

type
  GLenum = Cardinal;
  GLboolean = BYTEBOOL;
  GLbitfield = Cardinal;
  GLbyte = Shortint;
  GLshort = SmallInt;
  GLint = Integer;
  GLsizei = Integer;
  GLubyte = Byte;
  GLushort = Word;
  GLuint = Cardinal;
  GLfloat = Single;
  GLclampf = Single;
  GLdouble = Double;
  GLclampd = Double;
  PGLenum = ^GLenum;
  PGLboolean = ^GLboolean;
  PGLbyte = ^GLbyte;
  PGLshort = ^GLshort;
  PGLint = ^GLint;
  PGLsizei = ^GLsizei;
  PGLubyte = ^GLubyte;
  PGLushort = ^GLushort;
  PGLuint = ^GLuint;
  PGLclampf = ^GLclampf;
  PGLfloat =  ^GLfloat;
  PGLdouble = ^GLdouble;
  PGLclampd = ^GLclampd;

{ AttribMask }

const
  GL_CURRENT_BIT                      = $00000001;
  GL_POINT_BIT                        = $00000002;
  GL_LINE_BIT                         = $00000004;
  GL_POLYGON_BIT                      = $00000008;
  GL_POLYGON_STIPPLE_BIT              = $00000010;
  GL_PIXEL_MODE_BIT                   = $00000020;
  GL_LIGHTING_BIT                     = $00000040;
  GL_FOG_BIT                          = $00000080;
  GL_DEPTH_BUFFER_BIT                 = $00000100;
  GL_ACCUM_BUFFER_BIT                 = $00000200;
  GL_STENCIL_BUFFER_BIT               = $00000400;
  GL_VIEWPORT_BIT                     = $00000800;
  GL_TRANSFORM_BIT                    = $00001000;
  GL_ENABLE_BIT                       = $00002000;
  GL_COLOR_BUFFER_BIT                 = $00004000;
  GL_HINT_BIT                         = $00008000;
  GL_EVAL_BIT                         = $00010000;
  GL_LIST_BIT                         = $00020000;
  GL_TEXTURE_BIT                      = $00040000;
  GL_SCISSOR_BIT                      = $00080000;
  GL_ALL_ATTRIB_BITS                  = $000fffff;

{ ClearBufferMask }

{ Boolean }
  GL_FALSE                            = Boolean(0);
  GL_TRUE                             = Boolean(1);

{ BeginMode }
  GL_POINTS                           = $0000;
  GL_LINES                            = $0001;
  GL_LINE_LOOP                        = $0002;
  GL_LINE_STRIP                       = $0003;
  GL_TRIANGLES                        = $0004;
  GL_TRIANGLE_STRIP                   = $0005;
  GL_TRIANGLE_FAN                     = $0006;
  GL_QUADS                            = $0007;
  GL_QUAD_STRIP                       = $0008;
  GL_POLYGON                          = $0009;

{ AccumOp }
  GL_ACCUM                            = $0100;
  GL_LOAD                             = $0101;
  GL_RETURN                           = $0102;
  GL_MULT                             = $0103;
  GL_ADD                              = $0104;

{ AlphaFunction }
  GL_NEVER                            = $0200;
  GL_LESS                             = $0201;
  GL_EQUAL                            = $0202;
  GL_LEQUAL                           = $0203;
  GL_GREATER                          = $0204;
  GL_NOTEQUAL                         = $0205;
  GL_GEQUAL                           = $0206;
  GL_ALWAYS                           = $0207;

{ BlendingFactorDest }
  GL_ZERO                             = 0;
  GL_ONE                              = 1;
  GL_SRC_COLOR                        = $0300;
  GL_ONE_MINUS_SRC_COLOR              = $0301;
  GL_SRC_ALPHA                        = $0302;
  GL_ONE_MINUS_SRC_ALPHA              = $0303;
  GL_DST_ALPHA                        = $0304;
  GL_ONE_MINUS_DST_ALPHA              = $0305;

{ BlendingFactorSrc }
  GL_DST_COLOR                        = $0306;
  GL_ONE_MINUS_DST_COLOR              = $0307;
  GL_SRC_ALPHA_SATURATE               = $0308;

{ BlendingMode }

{ ColorMaterialFace }

{ ColorMaterialParameter }

{ CullFaceMode }

{ DepthFunction }

{ DrawBufferMode }
  GL_NONE                             = 0;
  GL_FRONT_LEFT                       = $0400;
  GL_FRONT_RIGHT                      = $0401;
  GL_BACK_LEFT                        = $0402;
  GL_BACK_RIGHT                       = $0403;
  GL_FRONT                            = $0404;
  GL_BACK                             = $0405;
  GL_LEFT                             = $0406;
  GL_RIGHT                            = $0407;
  GL_FRONT_AND_BACK                   = $0408;
  GL_AUX0                             = $0409;
  GL_AUX1                             = $040A;
  GL_AUX2                             = $040B;
  GL_AUX3                             = $040C;

{ ErrorCode }
  GL_NO_ERROR                         = 0;
  GL_INVALID_ENUM                     = $0500;
  GL_INVALID_VALUE                    = $0501;
  GL_INVALID_OPERATION                = $0502;
  GL_STACK_OVERFLOW                   = $0503;
  GL_STACK_UNDERFLOW                  = $0504;
  GL_OUT_OF_MEMORY                    = $0505;

{ FeedBackMode }
  GL_2D                               = $0600;
  GL_3D                               = $0601;
  GL_3D_COLOR                         = $0602;
  GL_3D_COLOR_TEXTURE                 = $0603;
  GL_4D_COLOR_TEXTURE                 = $0604;

{ FeedBackToken }
  GL_PASS_THROUGH_TOKEN               = $0700;
  GL_POINT_TOKEN                      = $0701;
  GL_LINE_TOKEN                       = $0702;
  GL_POLYGON_TOKEN                    = $0703;
  GL_BITMAP_TOKEN                     = $0704;
  GL_DRAW_PIXEL_TOKEN                 = $0705;
  GL_COPY_PIXEL_TOKEN                 = $0706;
  GL_LINE_RESET_TOKEN                 = $0707;

{ FogMode }
  GL_EXP                              = $0800;
  GL_EXP2                             = $0801;

{ FogParameter }

{ FrontFaceDirection }
  GL_CW                               = $0900;
  GL_CCW                              = $0901;

{ GetMapTarget }
  GL_COEFF                            = $0A00;
  GL_ORDER                            = $0A01;
  GL_DOMAIN                           = $0A02;

{ GetPixelMap }
  GL_PIXEL_MAP_I_TO_I                 = $0C70;
  GL_PIXEL_MAP_S_TO_S                 = $0C71;
  GL_PIXEL_MAP_I_TO_R                 = $0C72;
  GL_PIXEL_MAP_I_TO_G                 = $0C73;
  GL_PIXEL_MAP_I_TO_B                 = $0C74;
  GL_PIXEL_MAP_I_TO_A                 = $0C75;
  GL_PIXEL_MAP_R_TO_R                 = $0C76;
  GL_PIXEL_MAP_G_TO_G                 = $0C77;
  GL_PIXEL_MAP_B_TO_B                 = $0C78;
  GL_PIXEL_MAP_A_TO_A                 = $0C79;

{ GetTarget }
  GL_CURRENT_COLOR                    = $0B00;
  GL_CURRENT_INDEX                    = $0B01;
  GL_CURRENT_NORMAL                   = $0B02;
  GL_CURRENT_TEXTURE_COORDS           = $0B03;
  GL_CURRENT_RASTER_COLOR             = $0B04;
  GL_CURRENT_RASTER_INDEX             = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS    = $0B06;
  GL_CURRENT_RASTER_POSITION          = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID    = $0B08;
  GL_CURRENT_RASTER_DISTANCE          = $0B09;
  GL_POINT_SMOOTH                     = $0B10;
  GL_POINT_SIZE                       = $0B11;
  GL_POINT_SIZE_RANGE                 = $0B12;
  GL_POINT_SIZE_GRANULARITY           = $0B13;
  GL_LINE_SMOOTH                      = $0B20;
  GL_LINE_WIDTH                       = $0B21;
  GL_LINE_WIDTH_RANGE                 = $0B22;
  GL_LINE_WIDTH_GRANULARITY           = $0B23;
  GL_LINE_STIPPLE                     = $0B24;
  GL_LINE_STIPPLE_PATTERN             = $0B25;
  GL_LINE_STIPPLE_REPEAT              = $0B26;
  GL_LIST_MODE                        = $0B30;
  GL_MAX_LIST_NESTING                 = $0B31;
  GL_LIST_BASE                        = $0B32;
  GL_LIST_INDEX                       = $0B33;
  GL_POLYGON_MODE                     = $0B40;
  GL_POLYGON_SMOOTH                   = $0B41;
  GL_POLYGON_STIPPLE                  = $0B42;
  GL_EDGE_FLAG                        = $0B43;
  GL_CULL_FACE                        = $0B44;
  GL_CULL_FACE_MODE                   = $0B45;
  GL_FRONT_FACE                       = $0B46;
  GL_LIGHTING                         = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER         = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE             = $0B52;
  GL_LIGHT_MODEL_AMBIENT              = $0B53;
  GL_SHADE_MODEL                      = $0B54;
  GL_COLOR_MATERIAL_FACE              = $0B55;
  GL_COLOR_MATERIAL_PARAMETER         = $0B56;
  GL_COLOR_MATERIAL                   = $0B57;
  GL_FOG                              = $0B60;
  GL_FOG_INDEX                        = $0B61;
  GL_FOG_DENSITY                      = $0B62;
  GL_FOG_START                        = $0B63;
  GL_FOG_END                          = $0B64;
  GL_FOG_MODE                         = $0B65;
  GL_FOG_COLOR                        = $0B66;
  GL_DEPTH_RANGE                      = $0B70;
  GL_DEPTH_TEST                       = $0B71;
  GL_DEPTH_WRITEMASK                  = $0B72;
  GL_DEPTH_CLEAR_VALUE                = $0B73;
  GL_DEPTH_FUNC                       = $0B74;
  GL_ACCUM_CLEAR_VALUE                = $0B80;
  GL_STENCIL_TEST                     = $0B90;
  GL_STENCIL_CLEAR_VALUE              = $0B91;
  GL_STENCIL_FUNC                     = $0B92;
  GL_STENCIL_VALUE_MASK               = $0B93;
  GL_STENCIL_FAIL                     = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL          = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS          = $0B96;
  GL_STENCIL_REF                      = $0B97;
  GL_STENCIL_WRITEMASK                = $0B98;
  GL_MATRIX_MODE                      = $0BA0;
  GL_NORMALIZE                        = $0BA1;
  GL_VIEWPORT                         = $0BA2;
  GL_MODELVIEW_STACK_DEPTH            = $0BA3;
  GL_PROJECTION_STACK_DEPTH           = $0BA4;
  GL_TEXTURE_STACK_DEPTH              = $0BA5;
  GL_MODELVIEW_MATRIX                 = $0BA6;
  GL_PROJECTION_MATRIX                = $0BA7;
  GL_TEXTURE_MATRIX                   = $0BA8;
  GL_ATTRIB_STACK_DEPTH               = $0BB0;
  GL_ALPHA_TEST                       = $0BC0;
  GL_ALPHA_TEST_FUNC                  = $0BC1;
  GL_ALPHA_TEST_REF                   = $0BC2;
  GL_DITHER                           = $0BD0;
  GL_BLEND_DST                        = $0BE0;
  GL_BLEND_SRC                        = $0BE1;
  GL_BLEND                            = $0BE2;
  GL_LOGIC_OP_MODE                    = $0BF0;
  GL_LOGIC_OP                         = $0BF1;
  GL_AUX_BUFFERS                      = $0C00;
  GL_DRAW_BUFFER                      = $0C01;
  GL_READ_BUFFER                      = $0C02;
  GL_SCISSOR_BOX                      = $0C10;
  GL_SCISSOR_TEST                     = $0C11;
  GL_INDEX_CLEAR_VALUE                = $0C20;
  GL_INDEX_WRITEMASK                  = $0C21;
  GL_COLOR_CLEAR_VALUE                = $0C22;
  GL_COLOR_WRITEMASK                  = $0C23;
  GL_INDEX_MODE                       = $0C30;
  GL_RGBA_MODE                        = $0C31;
  GL_DOUBLEBUFFER                     = $0C32;
  GL_STEREO                           = $0C33;
  GL_RENDER_MODE                      = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT      = $0C50;
  GL_POINT_SMOOTH_HINT                = $0C51;
  GL_LINE_SMOOTH_HINT                 = $0C52;
  GL_POLYGON_SMOOTH_HINT              = $0C53;
  GL_FOG_HINT                         = $0C54;
  GL_TEXTURE_GEN_S                    = $0C60;
  GL_TEXTURE_GEN_T                    = $0C61;
  GL_TEXTURE_GEN_R                    = $0C62;
  GL_TEXTURE_GEN_Q                    = $0C63;
  GL_PIXEL_MAP_I_TO_I_SIZE            = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE            = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE            = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE            = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE            = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE            = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE            = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE            = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE            = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE            = $0CB9;
  GL_UNPACK_SWAP_BYTES                = $0CF0;
  GL_UNPACK_LSB_FIRST                 = $0CF1;
  GL_UNPACK_ROW_LENGTH                = $0CF2;
  GL_UNPACK_SKIP_ROWS                 = $0CF3;
  GL_UNPACK_SKIP_PIXELS               = $0CF4;
  GL_UNPACK_ALIGNMENT                 = $0CF5;
  GL_PACK_SWAP_BYTES                  = $0D00;
  GL_PACK_LSB_FIRST                   = $0D01;
  GL_PACK_ROW_LENGTH                  = $0D02;
  GL_PACK_SKIP_ROWS                   = $0D03;
  GL_PACK_SKIP_PIXELS                 = $0D04;
  GL_PACK_ALIGNMENT                   = $0D05;
  GL_MAP_COLOR                        = $0D10;
  GL_MAP_STENCIL                      = $0D11;
  GL_INDEX_SHIFT                      = $0D12;
  GL_INDEX_OFFSET                     = $0D13;
  GL_RED_SCALE                        = $0D14;
  GL_RED_BIAS                         = $0D15;
  GL_ZOOM_X                           = $0D16;
  GL_ZOOM_Y                           = $0D17;
  GL_GREEN_SCALE                      = $0D18;
  GL_GREEN_BIAS                       = $0D19;
  GL_BLUE_SCALE                       = $0D1A;
  GL_BLUE_BIAS                        = $0D1B;
  GL_ALPHA_SCALE                      = $0D1C;
  GL_ALPHA_BIAS                       = $0D1D;
  GL_DEPTH_SCALE                      = $0D1E;
  GL_DEPTH_BIAS                       = $0D1F;
  GL_MAX_EVAL_ORDER                   = $0D30;
  GL_MAX_LIGHTS                       = $0D31;
  GL_MAX_CLIP_PLANES                  = $0D32;
  GL_MAX_TEXTURE_SIZE                 = $0D33;
  GL_MAX_PIXEL_MAP_TABLE              = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH           = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH        = $0D36;
  GL_MAX_NAME_STACK_DEPTH             = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH       = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH          = $0D39;
  GL_MAX_VIEWPORT_DIMS                = $0D3A;
  GL_SUBPIXEL_BITS                    = $0D50;
  GL_INDEX_BITS                       = $0D51;
  GL_RED_BITS                         = $0D52;
  GL_GREEN_BITS                       = $0D53;
  GL_BLUE_BITS                        = $0D54;
  GL_ALPHA_BITS                       = $0D55;
  GL_DEPTH_BITS                       = $0D56;
  GL_STENCIL_BITS                     = $0D57;
  GL_ACCUM_RED_BITS                   = $0D58;
  GL_ACCUM_GREEN_BITS                 = $0D59;
  GL_ACCUM_BLUE_BITS                  = $0D5A;
  GL_ACCUM_ALPHA_BITS                 = $0D5B;
  GL_NAME_STACK_DEPTH                 = $0D70;
  GL_AUTO_NORMAL                      = $0D80;
  GL_MAP1_COLOR_4                     = $0D90;
  GL_MAP1_INDEX                       = $0D91;
  GL_MAP1_NORMAL                      = $0D92;
  GL_MAP1_TEXTURE_COORD_1             = $0D93;
  GL_MAP1_TEXTURE_COORD_2             = $0D94;
  GL_MAP1_TEXTURE_COORD_3             = $0D95;
  GL_MAP1_TEXTURE_COORD_4             = $0D96;
  GL_MAP1_VERTEX_3                    = $0D97;
  GL_MAP1_VERTEX_4                    = $0D98;
  GL_MAP2_COLOR_4                     = $0DB0;
  GL_MAP2_INDEX                       = $0DB1;
  GL_MAP2_NORMAL                      = $0DB2;
  GL_MAP2_TEXTURE_COORD_1             = $0DB3;
  GL_MAP2_TEXTURE_COORD_2             = $0DB4;
  GL_MAP2_TEXTURE_COORD_3             = $0DB5;
  GL_MAP2_TEXTURE_COORD_4             = $0DB6;
  GL_MAP2_VERTEX_3                    = $0DB7;
  GL_MAP2_VERTEX_4                    = $0DB8;
  GL_MAP1_GRID_DOMAIN                 = $0DD0;
  GL_MAP1_GRID_SEGMENTS               = $0DD1;
  GL_MAP2_GRID_DOMAIN                 = $0DD2;
  GL_MAP2_GRID_SEGMENTS               = $0DD3;
  GL_TEXTURE_1D                       = $0DE0;
  GL_TEXTURE_2D                       = $0DE1;

  GL_POLYGON_OFFSET_UNITS             = $2A00;
  GL_POLYGON_OFFSET_POINT             = $2A01;
  GL_POLYGON_OFFSET_LINE              = $2A02;
  GL_POLYGON_OFFSET_FILL              = $8037;
  GL_POLYGON_OFFSET_FACTOR            = $8038;

{ GetTextureParameter }
  GL_TEXTURE_WIDTH                    = $1000;
  GL_TEXTURE_HEIGHT                   = $1001;
  GL_TEXTURE_COMPONENTS               = $1003;
  GL_TEXTURE_BORDER_COLOR             = $1004;
  GL_TEXTURE_BORDER                   = $1005;

{ HintMode }
  GL_DONT_CARE                        = $1100;
  GL_FASTEST                          = $1101;
  GL_NICEST                           = $1102;

{ HintTarget }

{ LightModelParameter }

{ LightParameter }
  GL_AMBIENT                          = $1200;
  GL_DIFFUSE                          = $1201;
  GL_SPECULAR                         = $1202;
  GL_POSITION                         = $1203;
  GL_SPOT_DIRECTION                   = $1204;
  GL_SPOT_EXPONENT                    = $1205;
  GL_SPOT_CUTOFF                      = $1206;
  GL_CONSTANT_ATTENUATION             = $1207;
  GL_LINEAR_ATTENUATION               = $1208;
  GL_QUADRATIC_ATTENUATION            = $1209;

{ ListMode }
  GL_COMPILE                          = $1300;
  GL_COMPILE_AND_EXECUTE              = $1301;

{ ListNameType }
  GL_BYTE                             = $1400;
  GL_UNSIGNED_BYTE                    = $1401;
  GL_SHORT                            = $1402;
  GL_UNSIGNED_SHORT                   = $1403;
  GL_INT                              = $1404;
  GL_UNSIGNED_INT                     = $1405;
  GL_FLOAT                            = $1406;
  GL_2_BYTES                          = $1407;
  GL_3_BYTES                          = $1408;
  GL_4_BYTES                          = $1409;

{ LogicOp }
  GL_CLEAR                            = $1500;
  GL_AND                              = $1501;
  GL_AND_REVERSE                      = $1502;
  GL_COPY                             = $1503;
  GL_AND_INVERTED                     = $1504;
  GL_NOOP                             = $1505;
  GL_XOR                              = $1506;
  GL_OR                               = $1507;
  GL_NOR                              = $1508;
  GL_EQUIV                            = $1509;
  GL_INVERT                           = $150A;
  GL_OR_REVERSE                       = $150B;
  GL_COPY_INVERTED                    = $150C;
  GL_OR_INVERTED                      = $150D;
  GL_NAND                             = $150E;
  GL_SET                              = $150F;

{ MapTarget }

{ MaterialFace }

{ MaterialParameter }
  GL_EMISSION                         = $1600;
  GL_SHININESS                        = $1601;
  GL_AMBIENT_AND_DIFFUSE              = $1602;
  GL_COLOR_INDEXES                    = $1603;

{ MatrixMode }
  GL_MODELVIEW                        = $1700;
  GL_PROJECTION                       = $1701;
  GL_TEXTURE                          = $1702;

{ MeshMode1 }

{ MeshMode2 }

{ PixelCopyType }
  GL_COLOR                            = $1800;
  GL_DEPTH                            = $1801;
  GL_STENCIL                          = $1802;

{ PixelFormat }
  GL_COLOR_INDEX                      = $1900;
  GL_STENCIL_INDEX                    = $1901;
  GL_DEPTH_COMPONENT                  = $1902;
  GL_RED                              = $1903;
  GL_GREEN                            = $1904;
  GL_BLUE                             = $1905;
  GL_ALPHA                            = $1906;
  GL_RGB                              = $1907;
  GL_RGBA                             = $1908;
  GL_LUMINANCE                        = $1909;
  GL_LUMINANCE_ALPHA                  = $190A;

{ PixelMap }

{ PixelStore }

{ PixelTransfer }

{ PixelType }
  GL_BITMAP                           = $1A00;

{ PolygonMode }
  GL_POINT                            = $1B00;
  GL_LINE                             = $1B01;
  GL_FILL                             = $1B02;

{ ReadBufferMode }

{ RenderingMode }
  GL_RENDER                           = $1C00;
  GL_FEEDBACK                         = $1C01;
  GL_SELECT                           = $1C02;

{ ShadingModel }
  GL_FLAT                             = $1D00;
  GL_SMOOTH                           = $1D01;

{ StencilFunction }

{ StencilOp }
  GL_KEEP                             = $1E00;
  GL_REPLACE                          = $1E01;
  GL_INCR                             = $1E02;
  GL_DECR                             = $1E03;

{ StringName }
  GL_VENDOR                           = $1F00;
  GL_RENDERER                         = $1F01;
  GL_VERSION                          = $1F02;
  GL_EXTENSIONS                       = $1F03;

{ TextureCoordName }
  GL_S                                = $2000;
  GL_T                                = $2001;
  GL_R                                = $2002;
  GL_Q                                = $2003;

{ TextureEnvMode }
  GL_MODULATE                         = $2100;
  GL_DECAL                            = $2101;

{ TextureEnvParameter }
  GL_TEXTURE_ENV_MODE                 = $2200;
  GL_TEXTURE_ENV_COLOR                = $2201;

{ TextureEnvTarget }
  GL_TEXTURE_ENV                      = $2300;

{ TextureGenMode }
  GL_EYE_LINEAR                       = $2400;
  GL_OBJECT_LINEAR                    = $2401;
  GL_SPHERE_MAP                       = $2402;

{ TextureGenParameter }
  GL_TEXTURE_GEN_MODE                 = $2500;
  GL_OBJECT_PLANE                     = $2501;
  GL_EYE_PLANE                        = $2502;

{ TextureMagFilter }
  GL_NEAREST                          = $2600;
  GL_LINEAR                           = $2601;

{ TextureMinFilter }
  GL_NEAREST_MIPMAP_NEAREST           = $2700;
  GL_LINEAR_MIPMAP_NEAREST            = $2701;
  GL_NEAREST_MIPMAP_LINEAR            = $2702;
  GL_LINEAR_MIPMAP_LINEAR             = $2703;

{ TextureParameterName }
  GL_TEXTURE_MAG_FILTER               = $2800;
  GL_TEXTURE_MIN_FILTER               = $2801;
  GL_TEXTURE_WRAP_S                   = $2802;
  GL_TEXTURE_WRAP_T                   = $2803;
  GL_CLAMP_TO_EDGE                    = $812F; // GL 1.2
  GL_TEXTURE_MIN_LOD                  = $813A; // GL 1.2
  GL_TEXTURE_MAX_LOD                  = $813B; // GL 1.2
  GL_TEXTURE_BASE_LEVEL               = $813C; // GL 1.2
  GL_TEXTURE_MAX_LEVEL                = $813D; // GL 1.2
  GL_TEXTURE_DEPTH                    = $8071; // GL 1.2

{ TextureTarget }

{ TextureWrapMode }
  GL_CLAMP                            = $2900;
  GL_REPEAT                           = $2901;

{ ClipPlaneName }
  GL_CLIP_PLANE0                      = $3000;
  GL_CLIP_PLANE1                      = $3001;
  GL_CLIP_PLANE2                      = $3002;
  GL_CLIP_PLANE3                      = $3003;
  GL_CLIP_PLANE4                      = $3004;
  GL_CLIP_PLANE5                      = $3005;

{ LightName }
  GL_LIGHT0                           = $4000;
  GL_LIGHT1                           = $4001;
  GL_LIGHT2                           = $4002;
  GL_LIGHT3                           = $4003;
  GL_LIGHT4                           = $4004;
  GL_LIGHT5                           = $4005;
  GL_LIGHT6                           = $4006;
  GL_LIGHT7                           = $4007;

  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;

// Extensions
  GL_EXT_vertex_array                 = 1;
  GL_WIN_swap_hint                    = 1;

// vertex_array
  GL_VERTEX_ARRAY               = $8074;
  GL_NORMAL_ARRAY               = $8075;
  GL_COLOR_ARRAY                = $8076;
  GL_INDEX_ARRAY                = $8077;
  GL_TEXTURE_COORD_ARRAY        = $8078;
  GL_EDGE_FLAG_ARRAY            = $8079;
  GL_VERTEX_ARRAY_SIZE          = $807A;
  GL_VERTEX_ARRAY_TYPE          = $807B;
  GL_VERTEX_ARRAY_STRIDE        = $807C;
  GL_VERTEX_ARRAY_COUNT         = $807D;
  GL_NORMAL_ARRAY_TYPE          = $807E;
  GL_NORMAL_ARRAY_STRIDE        = $807F;
  GL_NORMAL_ARRAY_COUNT         = $8080;
  GL_COLOR_ARRAY_SIZE           = $8081;
  GL_COLOR_ARRAY_TYPE           = $8082;
  GL_COLOR_ARRAY_STRIDE         = $8083;
  GL_COLOR_ARRAY_COUNT          = $8084;
  GL_INDEX_ARRAY_TYPE           = $8085;
  GL_INDEX_ARRAY_STRIDE         = $8086;
  GL_INDEX_ARRAY_COUNT          = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT      = $808D;
  GL_VERTEX_ARRAY_POINTER       = $808E;
  GL_NORMAL_ARRAY_POINTER       = $808F;
  GL_COLOR_ARRAY_POINTER        = $8090;
  GL_INDEX_ARRAY_POINTER        = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER    = $8093;

type
  PPointFloat = ^TPointFloat;
  _POINTFLOAT = record
    X,Y: Single;
  end;
  TPointFloat = _POINTFLOAT;
  POINTFLOAT = _POINTFLOAT;

  PGlyphMetricsFloat = ^TGlyphMetricsFloat;
  _GLYPHMETRICSFLOAT = record
    gmfBlackBoxX: Single;
    gmfBlackBoxY: Single;
    gmfptGlyphOrigin: TPointFloat;
    gmfCellIncX: Single;
    gmfCellIncY: Single;
  end;
  TGlyphMetricsFloat = _GLYPHMETRICSFLOAT;
  GLYPHMETRICSFLOAT = _GLYPHMETRICSFLOAT;

const
  WGL_FONT_LINES      = 0;
  WGL_FONT_POLYGONS   = 1;

{***********************************************************}

procedure glAccum(op: GLenum; value: GLfloat); stdcall;
procedure glAlphaFunc(func: GLenum; ref: GLclampf); stdcall;
procedure glBegin(mode: GLenum); stdcall;
procedure glBitmap(width, height: GLsizei; xorig, yorig: GLfloat;
                    xmove, ymove: GLfloat; bitmap: Pointer); stdcall;
procedure glBlendFunc(sfactor, dfactor: GLenum); stdcall;
procedure glCallList(list: GLuint); stdcall;
procedure glCallLists(n: GLsizei; cltype: GLenum; lists: Pointer); stdcall;
procedure glClear(mask: GLbitfield); stdcall;
procedure glClearAccum(red, green, blue, alpha: GLfloat); stdcall;
procedure glClearColor(red, green, blue, alpha: GLclampf); stdcall;
procedure glClearDepth(depth: GLclampd); stdcall;
procedure glClearIndex(c: GLfloat); stdcall;
procedure glClearStencil(s: GLint); stdcall;
procedure glClipPlane(plane: GLenum; equation: PGLDouble); stdcall;

procedure glColor3b(red, green, blue: GLbyte); stdcall;
procedure glColor3bv(v: PGLByte); stdcall;
procedure glColor3d(red, green, blue: GLdouble); stdcall;
procedure glColor3dv(v: PGLdouble); stdcall;
procedure glColor3f(red, green, blue: GLfloat); stdcall;
procedure glColor3fv(v: PGLfloat); stdcall;
procedure glColor3i(red, green, blue: GLint); stdcall;
procedure glColor3iv(v: PGLint); stdcall;
procedure glColor3s(red, green, blue: GLshort); stdcall;
procedure glColor3sv(v: PGLshort); stdcall;
procedure glColor3ub(red, green, blue: GLubyte); stdcall;
procedure glColor3ubv(v: PGLubyte); stdcall;
procedure glColor3ui(red, green, blue: GLuint); stdcall;
procedure glColor3uiv(v: PGLuint); stdcall;
procedure glColor3us(red, green, blue: GLushort); stdcall;
procedure glColor3usv(v: PGLushort); stdcall;
procedure glColor4b(red, green, blue, alpha: GLbyte); stdcall;
procedure glColor4bv(v: PGLbyte); stdcall;
procedure glColor4d(red, green, blue, alpha: GLdouble); stdcall;
procedure glColor4dv(v: PGLdouble); stdcall;
procedure glColor4f(red, green, blue, alpha: GLfloat); stdcall;
procedure glColor4fv(v: PGLfloat); stdcall;
procedure glColor4i(red, green, blue, alpha: GLint); stdcall;
procedure glColor4iv(v: PGLint); stdcall;
procedure glColor4s(red, green, blue, alpha: GLshort); stdcall;
procedure glColor4sv(v: PGLshort); stdcall;
procedure glColor4ub(red, green, blue, alpha: GLubyte); stdcall;
procedure glColor4ubv(v: PGLubyte); stdcall;
procedure glColor4ui(red, green, blue, alpha: GLuint); stdcall;
procedure glColor4uiv(v: PGLuint); stdcall;
procedure glColor4us(red, green, blue, alpha: GLushort); stdcall;
procedure glColor4usv(v: PGLushort); stdcall;
procedure glColor(red, green, blue: GLbyte); stdcall; overload;
procedure glColor(red, green, blue: GLdouble); stdcall; overload;
procedure glColor(red, green, blue: GLfloat); stdcall; overload;
procedure glColor(red, green, blue: GLint); stdcall; overload;
procedure glColor(red, green, blue: GLshort); stdcall; overload;
procedure glColor(red, green, blue: GLubyte); stdcall; overload;
procedure glColor(red, green, blue: GLuint); stdcall; overload;
procedure glColor(red, green, blue: GLushort); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLbyte); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLdouble); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLfloat); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLint); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLshort); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLubyte); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLuint); stdcall; overload;
procedure glColor(red, green, blue, alpha: GLushort); stdcall; overload;
procedure glColor3(v: PGLbyte); stdcall; overload;
procedure glColor3(v: PGLdouble); stdcall; overload;
procedure glColor3(v: PGLfloat); stdcall; overload;
procedure glColor3(v: PGLint); stdcall; overload;
procedure glColor3(v: PGLshort); stdcall; overload;
procedure glColor3(v: PGLubyte); stdcall; overload;
procedure glColor3(v: PGLuint); stdcall; overload;
procedure glColor3(v: PGLushort); stdcall; overload;
procedure glColor4(v: PGLbyte); stdcall; overload;
procedure glColor4(v: PGLdouble); stdcall; overload;
procedure glColor4(v: PGLfloat); stdcall; overload;
procedure glColor4(v: PGLint); stdcall; overload;
procedure glColor4(v: PGLshort); stdcall; overload;
procedure glColor4(v: PGLubyte); stdcall; overload;
procedure glColor4(v: PGLuint); stdcall; overload;
procedure glColor4(v: PGLushort); stdcall; overload;

procedure glColorMask(red, green, blue, alpha: GLboolean); stdcall;
procedure glColorMaterial(face, mode: GLenum); stdcall;
procedure glCopyPixels(x,y: GLint; width, height: GLsizei; pixeltype: GLenum); stdcall;
procedure glCullFace(mode: GLenum); stdcall;
procedure glDeleteLists(list: GLuint; range: GLsizei); stdcall;
procedure glDepthFunc(func: GLenum); stdcall;
procedure glDepthMask(flag: GLboolean); stdcall;
procedure glDepthRange(zNear, zFar: GLclampd); stdcall;
procedure glDisable(cap: GLenum); stdcall;
procedure glDisableClientState(cap: GLenum); stdcall;
procedure glEnableClientState(cap: GLenum); stdcall;
procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); stdcall;
procedure  glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum;
  const indices: Pointer); stdcall;
procedure glDrawBuffer(mode: GLenum); stdcall;
procedure glDrawPixels(width, height: GLsizei; format, pixeltype: GLenum;
 pixels: Pointer); stdcall;
procedure glEdgeFlag(flag: GLboolean); stdcall;
procedure glEdgeFlagv(flag: PGLboolean); stdcall;
procedure glEnable(cap: GLenum); stdcall;
procedure glEnd; stdcall;
procedure glEndList; stdcall;

procedure glEvalCoord1d(u: GLdouble); stdcall;
procedure glEvalCoord1dv(u: PGLdouble); stdcall;
procedure glEvalCoord1f(u: GLfloat); stdcall;
procedure glEvalCoord1fv(u: PGLfloat); stdcall;
procedure glEvalCoord2d(u,v: GLdouble); stdcall;
procedure glEvalCoord2dv(u: PGLdouble); stdcall;
procedure glEvalCoord2f(u,v: GLfloat); stdcall;
procedure glEvalCoord2fv(u: PGLfloat); stdcall;
procedure glEvalCoord(u: GLdouble); stdcall; overload;
procedure glEvalCoord(u: GLfloat); stdcall; overload;
procedure glEvalCoord(u,v: GLdouble); stdcall; overload;
procedure glEvalCoord(u,v: GLfloat); stdcall; overload;
procedure glEvalCoord1(v: PGLdouble); stdcall; overload;
procedure glEvalCoord1(v: PGLfloat); stdcall; overload;
procedure glEvalCoord2(v: PGLdouble); stdcall; overload;
procedure glEvalCoord2(v: PGLfloat); stdcall; overload;

procedure glEvalMesh1(mode: GLenum; i1, i2: GLint); stdcall;
procedure glEvalMesh2(mode: GLenum; i1, i2, j1, j2: GLint); stdcall;
procedure glEvalMesh(mode: GLenum; i1, i2: GLint); stdcall; overload;
procedure glEvalMesh(mode: GLenum; i1, i2, j1, j2: GLint); stdcall; overload;

procedure glEvalPoint1(i: GLint); stdcall;
procedure glEvalPoint2(i,j: GLint); stdcall;
procedure glEvalPoint(i: GLint); stdcall; overload;
procedure glEvalPoint(i,j: GLint); stdcall; overload;

procedure glFeedbackBuffer(size: GLsizei; buftype: GLenum; buffer: PGLFloat); stdcall;
procedure glFinish; stdcall;
procedure glFlush; stdcall;

procedure glFogf(pname: GLenum; param: GLfloat); stdcall;
procedure glFogfv(pname: GLenum; params: PGLfloat); stdcall;
procedure glFogi(pname: GLenum; param: GLint); stdcall;
procedure glFogiv(pname: GLenum; params: PGLint); stdcall;
procedure glFog(pname: GLenum; param: GLfloat); stdcall; overload;
procedure glFog(pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glFog(pname: GLenum; param: GLint); stdcall; overload;
procedure glFog(pname: GLenum; params: PGLint); stdcall; overload;

procedure glFrontFace(mode: GLenum); stdcall;
procedure glFrustum(left, right, bottom, top, zNear, zFar: GLdouble); stdcall;
function  glGenLists(range: GLsizei): GLuint; stdcall;
procedure glGetBooleanv(pname: GLenum; params: PGLboolean); stdcall;
procedure glGetClipPlane(plane: GLenum; equation: PGLdouble); stdcall;
procedure glGetDoublev(pname: GLenum; params: PGLdouble); stdcall;
function  glGetError: GLenum; stdcall;
procedure glGetFloatv(pname: GLenum; params: PGLfloat); stdcall;
procedure glGetIntegerv(pname: GLenum; params: PGLint); stdcall;

procedure glGetLightfv(light: GLenum; pname: GLenum; params: PGLfloat); stdcall;
procedure glGetLightiv(light: GLenum; pname: GLenum; params: PGLint); stdcall;
procedure glGetLight(light: GLenum; pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetLight(light: GLenum; pname: GLenum; params: PGLint); stdcall; overload;

procedure glGetMapdv(target: GLenum; query: GLenum; v: PGLdouble); stdcall;
procedure glGetMapfv(target: GLenum; query: GLenum; v: PGLfloat); stdcall;
procedure glGetMapiv(target: GLenum; query: GLenum; v: PGLint); stdcall;
procedure glGetMap(target: GLenum; query: GLenum; v: PGLdouble); stdcall; overload;
procedure glGetMap(target: GLenum; query: GLenum; v: PGLfloat); stdcall; overload;
procedure glGetMap(target: GLenum; query: GLenum; v: PGLint); stdcall; overload;

procedure glGetMaterialfv(face: GLenum; pname: GLenum; params: PGLfloat); stdcall;
procedure glGetMaterialiv(face: GLenum; pname: GLenum; params: PGLint); stdcall;
procedure glGetMaterial(face: GLenum; pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetMaterial(face: GLenum; pname: GLenum; params: PGLint); stdcall; overload;

procedure glGetPixelMapfv(map: GLenum; values: PGLfloat); stdcall;
procedure glGetPixelMapuiv(map: GLenum; values: PGLuint); stdcall;
procedure glGetPixelMapusv(map: GLenum; values: PGLushort); stdcall;
procedure glGetPixelMap(map: GLenum; values: PGLfloat); stdcall; overload;
procedure glGetPixelMap(map: GLenum; values: PGLuint); stdcall; overload;
procedure glGetPixelMap(map: GLenum; values: PGLushort); stdcall; overload;

procedure glGetPolygonStipple(var mask: GLubyte); stdcall;
function  glGetString(name: GLenum): PChar; stdcall;

procedure glGetTexEnvfv(target: GLenum; pname: GLenum; params: PGLfloat); stdcall;
procedure glGetTexEnviv(target: GLenum; pname: GLenum; params: PGLint); stdcall;
procedure glGetTexEnv(target: GLenum; pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetTexEnv(target: GLenum; pname: GLenum; params: PGLint); stdcall; overload;

procedure glGetTexGendv(coord: GLenum; pname: GLenum; params: PGLdouble); stdcall;
procedure glGetTexGenfv(coord: GLenum; pname: GLenum; params: PGLfloat); stdcall;
procedure glGetTexGeniv(coord: GLenum; pname: GLenum; params: PGLint); stdcall;
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLdouble); stdcall; overload;
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLint); stdcall; overload;

procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; _type: GLenum; pixels: pointer); stdcall;

procedure glGetTexLevelParameterfv(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); stdcall;
procedure glGetTexLevelParameteriv(target: GLenum; level: GLint; pname: GLenum; params: PGLint); stdcall;
procedure glGetTexLevelParameter(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetTexLevelParameter(target: GLenum; level: GLint; pname: GLenum; params: PGLint); stdcall; overload;

procedure glGetTexParameterfv(target, pname: GLenum; params: PGLfloat); stdcall;
procedure glGetTexParameteriv(target, pname: GLenum; params: PGLint); stdcall;
procedure glGetTexParameter(target, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glGetTexParameter(target, pname: GLenum; params: PGLint); stdcall; overload;

procedure glHint(target, mode: GLenum); stdcall;
procedure glIndexMask(mask: GLuint); stdcall;

procedure glIndexd(c: GLdouble); stdcall;
procedure glIndexdv(c: PGLdouble); stdcall;
procedure glIndexf(c: GLfloat); stdcall;
procedure glIndexfv(c: PGLfloat); stdcall;
procedure glIndexi(c: GLint); stdcall;
procedure glIndexiv(c: PGLint); stdcall;
procedure glIndexs(c: GLshort); stdcall;
procedure glIndexsv(c: PGLshort); stdcall;
procedure glIndex(c: GLdouble); stdcall; overload;
procedure glIndex(c: PGLdouble); stdcall; overload;
procedure glIndex(c: GLfloat); stdcall;  overload;
procedure glIndex(c: PGLfloat); stdcall; overload;
procedure glIndex(c: GLint); stdcall; overload;
procedure glIndex(c: PGLint); stdcall; overload;
procedure glIndex(c: GLshort); stdcall; overload;
procedure glIndex(c: PGLshort); stdcall; overload;

procedure glInitNames; stdcall;
function  glIsEnabled(cap: GLenum): GLBoolean; stdcall;
function  glIsList(list: GLuint): GLBoolean;   stdcall;

procedure glLightModelf(pname: GLenum; param: GLfloat); stdcall;
procedure glLightModelfv(pname: GLenum; params: PGLfloat); stdcall;
procedure glLightModeli(pname: GLenum; param: GLint); stdcall;
procedure glLightModeliv(pname: GLenum; params: PGLint); stdcall;
procedure glLightModel(pname: GLenum; param: GLfloat); stdcall; overload;
procedure glLightModel(pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glLightModel(pname: GLenum; param: GLint); stdcall; overload;
procedure glLightModel(pname: GLenum; params: PGLint); stdcall; overload;

procedure glLightf(light, pname: GLenum; param: GLfloat); stdcall;
procedure glLightfv(light, pname: GLenum; params: PGLfloat); stdcall;
procedure glLighti(light, pname: GLenum; param: GLint); stdcall;
procedure glLightiv(light, pname: GLenum; params: PGLint); stdcall;
procedure glLight(light, pname: GLenum; param: GLfloat); stdcall; overload;
procedure glLight(light, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glLight(light, pname: GLenum; param: GLint); stdcall; overload;
procedure glLight(light, pname: GLenum; params: PGLint); stdcall; overload;

procedure glLineStipple(factor: GLint; pattern: GLushort); stdcall;
procedure glLineWidth(width: GLfloat); stdcall;
procedure glListBase(base: GLuint); stdcall;
procedure glLoadIdentity; stdcall;

procedure glLoadMatrixd(m: PGLdouble); stdcall;
procedure glLoadMatrixf(m: PGLfloat); stdcall;
procedure glLoadMatrix(m: PGLdouble); stdcall; overload;
procedure glLoadMatrix(m: PGLfloat); stdcall; overload;

procedure glLoadName(name: GLuint); stdcall;
procedure glLogicOp(opcode: GLenum); stdcall;

procedure glMap1d(target: GLenum; u1,u2: GLdouble; stride, order: GLint;
  Points: PGLdouble); stdcall;
procedure glMap1f(target: GLenum; u1,u2: GLfloat; stride, order: GLint;
  Points: PGLfloat); stdcall;
procedure glMap2d(target: GLenum;
  u1,u2: GLdouble; ustride, uorder: GLint;
  v1,v2: GLdouble; vstride, vorder: GLint; Points: PGLdouble); stdcall;
procedure glMap2f(target: GLenum;
  u1,u2: GLfloat; ustride, uorder: GLint;
  v1,v2: GLfloat; vstride, vorder: GLint; Points: PGLfloat); stdcall;
procedure glMap(target: GLenum; u1,u2: GLdouble; stride, order: GLint;
  Points: PGLdouble); stdcall; overload;
procedure glMap(target: GLenum; u1,u2: GLfloat; stride, order: GLint;
  Points: PGLfloat); stdcall; overload;
procedure glMap(target: GLenum;
  u1,u2: GLdouble; ustride, uorder: GLint;
  v1,v2: GLdouble; vstride, vorder: GLint; Points: PGLdouble); stdcall; overload;
procedure glMap(target: GLenum;
  u1,u2: GLfloat; ustride, uorder: GLint;
  v1,v2: GLfloat; vstride, vorder: GLint; Points: PGLfloat); stdcall; overload;

procedure glMapGrid1d(un: GLint; u1, u2: GLdouble); stdcall;
procedure glMapGrid1f(un: GLint; u1, u2: GLfloat); stdcall;
procedure glMapGrid2d(un: GLint; u1, u2: GLdouble;
                       vn: GLint; v1, v2: GLdouble); stdcall;
procedure glMapGrid2f(un: GLint; u1, u2: GLfloat;
                       vn: GLint; v1, v2: GLfloat); stdcall;
procedure glMapGrid(un: GLint; u1, u2: GLdouble); stdcall; overload;
procedure glMapGrid(un: GLint; u1, u2: GLfloat); stdcall;  overload;
procedure glMapGrid(un: GLint; u1, u2: GLdouble;
                    vn: GLint; v1, v2: GLdouble); stdcall; overload;
procedure glMapGrid(un: GLint; u1, u2: GLfloat;
                    vn: GLint; v1, v2: GLfloat); stdcall; overload;

procedure glMaterialf(face, pname: GLenum; param: GLfloat); stdcall;
procedure glMaterialfv(face, pname: GLenum; params: PGLfloat); stdcall;
procedure glMateriali(face, pname: GLenum; param: GLint); stdcall;
procedure glMaterialiv(face, pname: GLenum; params: PGLint); stdcall;
procedure glMaterial(face, pname: GLenum; param: GLfloat); stdcall; overload;
procedure glMaterial(face, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glMaterial(face, pname: GLenum; param: GLint); stdcall; overload;
procedure glMaterial(face, pname: GLenum; params: PGLint); stdcall; overload;

procedure glMatrixMode(mode: GLenum); stdcall;

procedure glMultMatrixd(m: PGLdouble); stdcall;
procedure glMultMatrixf(m: PGLfloat); stdcall;
procedure glMultMatrix(m: PGLdouble); stdcall; overload;
procedure glMultMatrix(m: PGLfloat); stdcall; overload;

procedure glNewList(ListIndex: GLuint; mode: GLenum); stdcall;

procedure glNormal3b(nx, ny, nz: GLbyte); stdcall;
procedure glNormal3bv(v: PGLbyte); stdcall;
procedure glNormal3d(nx, ny, nz: GLdouble); stdcall;
procedure glNormal3dv(v: PGLdouble); stdcall;
procedure glNormal3f(nx, ny, nz: GLFloat); stdcall;
procedure glNormal3fv(v: PGLfloat); stdcall;
procedure glNormal3i(nx, ny, nz: GLint); stdcall;
procedure glNormal3iv(v: PGLint); stdcall;
procedure glNormal3s(nx, ny, nz: GLshort); stdcall;
procedure glNormal3sv(v: PGLshort); stdcall;
procedure glNormal(nx, ny, nz: GLbyte); stdcall; overload;
procedure glNormal3(v: PGLbyte); stdcall; overload;
procedure glNormal(nx, ny, nz: GLdouble); stdcall; overload;
procedure glNormal3(v: PGLdouble); stdcall; overload;
procedure glNormal(nx, ny, nz: GLFloat); stdcall; overload;
procedure glNormal3(v: PGLfloat); stdcall; overload;
procedure glNormal(nx, ny, nz: GLint); stdcall; overload;
procedure glNormal3(v: PGLint); stdcall; overload;
procedure glNormal(nx, ny, nz: GLshort); stdcall; overload;
procedure glNormal3(v: PGLshort); stdcall; overload;

procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble); stdcall;
procedure glPassThrough(token: GLfloat); stdcall;

procedure glPixelMapfv(map: GLenum; mapsize: GLint; values: PGLfloat); stdcall;
procedure glPixelMapuiv(map: GLenum; mapsize: GLint; values: PGLuint); stdcall;
procedure glPixelMapusv(map: GLenum; mapsize: GLint; values: PGLushort); stdcall;
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLfloat); stdcall; overload;
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLuint); stdcall;  overload;
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLushort); stdcall; overload;

procedure glPixelStoref(pname: GLenum; param: GLfloat); stdcall;
procedure glPixelStorei(pname: GLenum; param: GLint); stdcall;
procedure glPixelStore(pname: GLenum; param: GLfloat); stdcall; overload;
procedure glPixelStore(pname: GLenum; param: GLint); stdcall; overload;

procedure glPixelTransferf(pname: GLenum; param: GLfloat); stdcall;
procedure glPixelTransferi(pname: GLenum; param: GLint); stdcall;
procedure glPixelTransfer(pname: GLenum; param: GLfloat); stdcall; overload;
procedure glPixelTransfer(pname: GLenum; param: GLint); stdcall; overload;

procedure glPixelZoom(xfactor, yfactor: GLfloat); stdcall;
procedure glPointSize(size: GLfloat); stdcall;
procedure glPolygonMode(face, mode: GLenum); stdcall;
procedure glPolygonOffset(factor, units: GLfloat); stdcall;
procedure glPolygonStipple(mask: PGLubyte); stdcall;
procedure glPopAttrib; stdcall;
procedure glPopMatrix; stdcall;
procedure glPopName; stdcall;
procedure glPushAttrib(mask: GLbitfield); stdcall;
procedure glPushMatrix; stdcall;
procedure glPushName(name: GLuint); stdcall;

procedure glRasterPos2d(x,y: GLdouble); stdcall;
procedure glRasterPos2dv(v: PGLdouble); stdcall;
procedure glRasterPos2f(x,y: GLfloat); stdcall;
procedure glRasterPos2fv(v: PGLfloat); stdcall;
procedure glRasterPos2i(x,y: GLint); stdcall;
procedure glRasterPos2iv(v: PGLint); stdcall;
procedure glRasterPos2s(x,y: GLshort); stdcall;
procedure glRasterPos2sv(v: PGLshort); stdcall;
procedure glRasterPos3d(x,y,z: GLdouble); stdcall;
procedure glRasterPos3dv(v: PGLdouble); stdcall;
procedure glRasterPos3f(x,y,z: GLfloat); stdcall;
procedure glRasterPos3fv(v: PGLfloat); stdcall;
procedure glRasterPos3i(x,y,z: GLint); stdcall;
procedure glRasterPos3iv(v: PGLint); stdcall;
procedure glRasterPos3s(x,y,z: GLshort); stdcall;
procedure glRasterPos3sv(v: PGLshort); stdcall;
procedure glRasterPos4d(x,y,z,w: GLdouble); stdcall;
procedure glRasterPos4dv(v: PGLdouble); stdcall;
procedure glRasterPos4f(x,y,z,w: GLfloat); stdcall;
procedure glRasterPos4fv(v: PGLfloat); stdcall;
procedure glRasterPos4i(x,y,z,w: GLint); stdcall;
procedure glRasterPos4iv(v: PGLint); stdcall;
procedure glRasterPos4s(x,y,z,w: GLshort); stdcall;
procedure glRasterPos4sv(v: PGLshort); stdcall;
procedure glRasterPos(x,y: GLdouble); stdcall; overload;
procedure glRasterPos2(v: PGLdouble); stdcall; overload;
procedure glRasterPos(x,y: GLfloat); stdcall; overload;
procedure glRasterPos2(v: PGLfloat); stdcall; overload;
procedure glRasterPos(x,y: GLint); stdcall; overload;
procedure glRasterPos2(v: PGLint); stdcall; overload;
procedure glRasterPos(x,y: GLshort); stdcall; overload;
procedure glRasterPos2(v: PGLshort); stdcall; overload;
procedure glRasterPos(x,y,z: GLdouble); stdcall; overload;
procedure glRasterPos3(v: PGLdouble); stdcall; overload;
procedure glRasterPos(x,y,z: GLfloat); stdcall; overload;
procedure glRasterPos3(v: PGLfloat); stdcall; overload;
procedure glRasterPos(x,y,z: GLint); stdcall; overload;
procedure glRasterPos3(v: PGLint); stdcall; overload;
procedure glRasterPos(x,y,z: GLshort); stdcall; overload;
procedure glRasterPos3(v: PGLshort); stdcall; overload;
procedure glRasterPos(x,y,z,w: GLdouble); stdcall; overload;
procedure glRasterPos4(v: PGLdouble); stdcall; overload;
procedure glRasterPos(x,y,z,w: GLfloat); stdcall; overload;
procedure glRasterPos4(v: PGLfloat); stdcall; overload;
procedure glRasterPos(x,y,z,w: GLint); stdcall; overload;
procedure glRasterPos4(v: PGLint); stdcall; overload;
procedure glRasterPos(x,y,z,w: GLshort); stdcall; overload;
procedure glRasterPos4(v: PGLshort); stdcall; overload;

procedure glReadBuffer(mode: GLenum); stdcall;
procedure glReadPixels(x,y: GLint; width, height: GLsizei;
  format, _type: GLenum; pixels: Pointer); stdcall;

procedure glRectd(x1, y1, x2, y2: GLdouble); stdcall;
procedure glRectdv(v1, v2: PGLdouble); stdcall;
procedure glRectf(x1, y1, x2, y2: GLfloat); stdcall;
procedure glRectfv(v1, v2: PGLfloat); stdcall;
procedure glRecti(x1, y1, x2, y2: GLint); stdcall;
procedure glRectiv(v1, v2: PGLint); stdcall;
procedure glRects(x1, y1, x2, y2: GLshort); stdcall;
procedure glRectsv(v1, v2: PGLshort); stdcall;
procedure glRect(x1, y1, x2, y2: GLdouble); stdcall; overload;
procedure glRect(v1, v2: PGLdouble); stdcall; overload;
procedure glRect(x1, y1, x2, y2: GLfloat); stdcall; overload;
procedure glRect(v1, v2: PGLfloat); stdcall; overload;
procedure glRect(x1, y1, x2, y2: GLint); stdcall; overload;
procedure glRect(v1, v2: PGLint); stdcall; overload;
procedure glRect(x1, y1, x2, y2: GLshort); stdcall; overload;
procedure glRect(v1, v2: PGLshort); stdcall; overload;

function  glRenderMode(mode: GLenum): GLint; stdcall;

procedure glRotated(angle, x,y,z: GLdouble); stdcall;
procedure glRotatef(angle, x,y,z: GLfloat); stdcall;
procedure glRotate(angle, x,y,z: GLdouble); stdcall; overload;
procedure glRotate(angle, x,y,z: GLfloat); stdcall; overload;

procedure glScaled(x,y,z: GLdouble); stdcall;
procedure glScalef(x,y,z: GLfloat); stdcall;
procedure glScale(x,y,z: GLdouble); stdcall; overload;
procedure glScale(x,y,z: GLfloat); stdcall; overload;

procedure glScissor(x,y: GLint; width, height: GLsizei); stdcall;
procedure glSelectBuffer(size: GLsizei; buffer: PGLuint); stdcall;
procedure glShadeModel(mode: GLenum); stdcall;
procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint); stdcall;
procedure glStencilMask(mask: GLuint); stdcall;
procedure glStencilOp(fail, zfail, zpass: GLenum); stdcall;

procedure glTexCoord1d(s: GLdouble); stdcall;
procedure glTexCoord1dv(v: PGLdouble); stdcall;
procedure glTexCoord1f(s: GLfloat); stdcall;
procedure glTexCoord1fv(v: PGLfloat); stdcall;
procedure glTexCoord1i(s: GLint); stdcall;
procedure glTexCoord1iv(v: PGLint); stdcall;
procedure glTexCoord1s(s: GLshort); stdcall;
procedure glTexCoord1sv(v: PGLshort); stdcall;
procedure glTexCoord2d(s,t: GLdouble); stdcall;
procedure glTexCoord2dv(v: PGLdouble); stdcall;
procedure glTexCoord2f(s,t: GLfloat); stdcall;
procedure glTexCoord2fv(v: PGLfloat); stdcall;
procedure glTexCoord2i(s,t: GLint); stdcall;
procedure glTexCoord2iv(v: PGLint); stdcall;
procedure glTexCoord2s(s,t: GLshort); stdcall;
procedure glTexCoord2sv(v: PGLshort); stdcall;
procedure glTexCoord3d(s,t,r: GLdouble); stdcall;
procedure glTexCoord3dv(v: PGLdouble); stdcall;
procedure glTexCoord3f(s,t,r: GLfloat); stdcall;
procedure glTexCoord3fv(v: PGLfloat); stdcall;
procedure glTexCoord3i(s,t,r: GLint); stdcall;
procedure glTexCoord3iv(v: PGLint); stdcall;
procedure glTexCoord3s(s,t,r: GLshort); stdcall;
procedure glTexCoord3sv(v: PGLshort); stdcall;
procedure glTexCoord4d(s,t,r,q: GLdouble); stdcall;
procedure glTexCoord4dv(v: PGLdouble); stdcall;
procedure glTexCoord4f(s,t,r,q: GLfloat); stdcall;
procedure glTexCoord4fv(v: PGLfloat); stdcall;
procedure glTexCoord4i(s,t,r,q: GLint); stdcall;
procedure glTexCoord4iv(v: PGLint); stdcall;
procedure glTexCoord4s(s,t,r,q: GLshort); stdcall;
procedure glTexCoord4sv(v: PGLshort); stdcall;
procedure glTexCoord(s: GLdouble); stdcall; overload;
procedure glTexCoord1(v: PGLdouble); stdcall; overload;
procedure glTexCoord(s: GLfloat); stdcall; overload;
procedure glTexCoord1(v: PGLfloat); stdcall; overload;
procedure glTexCoord(s: GLint); stdcall; overload;
procedure glTexCoord1(v: PGLint); stdcall; overload;
procedure glTexCoord(s: GLshort); stdcall; overload;
procedure glTexCoord1(v: PGLshort); stdcall; overload;
procedure glTexCoord(s,t: GLdouble); stdcall; overload;
procedure glTexCoord2(v: PGLdouble); stdcall; overload;
procedure glTexCoord(s,t: GLfloat); stdcall; overload;
procedure glTexCoord2(v: PGLfloat); stdcall; overload;
procedure glTexCoord(s,t: GLint); stdcall; overload;
procedure glTexCoord2(v: PGLint); stdcall; overload;
procedure glTexCoord(s,t: GLshort); stdcall; overload;
procedure glTexCoord2(v: PGLshort); stdcall; overload;
procedure glTexCoord(s,t,r: GLdouble); stdcall; overload;
procedure glTexCoord3(v: PGLdouble); stdcall; overload;
procedure glTexCoord(s,t,r: GLfloat); stdcall; overload;
procedure glTexCoord3(v: PGLfloat); stdcall; overload;
procedure glTexCoord(s,t,r: GLint); stdcall; overload;
procedure glTexCoord3(v: PGLint); stdcall; overload;
procedure glTexCoord(s,t,r: GLshort); stdcall; overload;
procedure glTexCoord3(v: PGLshort); stdcall; overload;
procedure glTexCoord(s,t,r,q: GLdouble); stdcall; overload;
procedure glTexCoord4(v: PGLdouble); stdcall; overload;
procedure glTexCoord(s,t,r,q: GLfloat); stdcall; overload;
procedure glTexCoord4(v: PGLfloat); stdcall; overload;
procedure glTexCoord(s,t,r,q: GLint); stdcall; overload;
procedure glTexCoord4(v: PGLint); stdcall; overload;
procedure glTexCoord(s,t,r,q: GLshort); stdcall; overload;
procedure glTexCoord4(v: PGLshort); stdcall; overload;

procedure glTexEnvf(target, pname: GLenum; param: GLfloat); stdcall;
procedure glTexEnvfv(target, pname: GLenum; params: PGLfloat); stdcall;
procedure glTexEnvi(target, pname: GLenum; param: GLint); stdcall;
procedure glTexEnviv(target, pname: GLenum; params: PGLint); stdcall;
procedure glTexEnv(target, pname: GLenum; param: GLfloat); stdcall; overload;
procedure glTexEnv(target, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glTexEnv(target, pname: GLenum; param: GLint); stdcall; overload;
procedure glTexEnv(target, pname: GLenum; params: PGLint); stdcall; overload;

procedure glTexGend(coord, pname: GLenum; param: GLdouble); stdcall;
procedure glTexGendv(coord, pname: GLenum; params: PGLdouble); stdcall;
procedure glTexGenf(coord, pname: GLenum; param: GLfloat); stdcall;
procedure glTexGenfv(coord, pname: GLenum; params: PGLfloat); stdcall;
procedure glTexGeni(coord, pname: GLenum; param: GLint); stdcall;
procedure glTexGeniv(coord, pname: GLenum; params: PGLint); stdcall;
procedure glTexGen(coord, pname: GLenum; param: GLdouble); stdcall; overload;
procedure glTexGen(coord, pname: GLenum; params: PGLdouble); stdcall; overload;
procedure glTexGen(coord, pname: GLenum; param: GLfloat); stdcall; overload;
procedure glTexGen(coord, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glTexGen(coord, pname: GLenum; param: GLint); stdcall; overload;
procedure glTexGen(coord, pname: GLenum; params: PGLint); stdcall; overload;

procedure glTexImage1D(target: GLenum; level, components: GLint;
  width: GLsizei; border: GLint; format, _type: GLenum; pixels: Pointer); stdcall;
procedure glTexImage2D(target: GLenum; level, components: GLint;
  width, height: GLsizei; border: GLint; format, _type: GLenum; pixels: Pointer); stdcall;
procedure glCopyTexImage1D(target: GLenum; level: GLint; internalFormat: GLenum;
  x: GLint; y: GLint; width: GLsizei; border: GLint); stdcall;
procedure glCopyTexImage2D(target: GLenum; level: GLint; internalFormat: GLenum;
  x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); stdcall;

procedure glTexParameterf(target, pname: GLenum; param: GLfloat); stdcall;
procedure glTexParameterfv(target, pname: GLenum; params: PGLfloat); stdcall;
procedure glTexParameteri(target, pname: GLenum; param: GLint); stdcall;
procedure glTexParameteriv(target, pname: GLenum; params: PGLint); stdcall;
procedure glTexParameter(target, pname: GLenum; param: GLfloat); stdcall; overload;
procedure glTexParameter(target, pname: GLenum; params: PGLfloat); stdcall; overload;
procedure glTexParameter(target, pname: GLenum; param: GLint); stdcall; overload;
procedure glTexParameter(target, pname: GLenum; params: PGLint); stdcall; overload;

procedure glTranslated(x,y,z: GLdouble); stdcall;
procedure glTranslatef(x,y,z: GLfloat); stdcall;
procedure glTranslate(x,y,z: GLdouble); stdcall; overload;
procedure glTranslate(x,y,z: GLfloat); stdcall; overload;

procedure glVertex2d(x,y: GLdouble); stdcall;
procedure glVertex2dv(v: PGLdouble); stdcall;
procedure glVertex2f(x,y: GLfloat); stdcall;
procedure glVertex2fv(v: PGLfloat); stdcall;
procedure glVertex2i(x,y: GLint); stdcall;
procedure glVertex2iv(v: PGLint); stdcall;
procedure glVertex2s(x,y: GLshort); stdcall;
procedure glVertex2sv(v: PGLshort); stdcall;
procedure glVertex3d(x,y,z: GLdouble); stdcall;
procedure glVertex3dv(v: PGLdouble); stdcall;
procedure glVertex3f(x,y,z: GLfloat); stdcall;
procedure glVertex3fv(v: PGLfloat); stdcall;
procedure glVertex3i(x,y,z: GLint); stdcall;
procedure glVertex3iv(v: PGLint); stdcall;
procedure glVertex3s(x,y,z: GLshort); stdcall;
procedure glVertex3sv(v: PGLshort); stdcall;
procedure glVertex4d(x,y,z,w: GLdouble); stdcall;
procedure glVertex4dv(v: PGLdouble); stdcall;
procedure glVertex4f(x,y,z,w: GLfloat); stdcall;
procedure glVertex4fv(v: PGLfloat); stdcall;
procedure glVertex4i(x,y,z,w: GLint); stdcall;
procedure glVertex4iv(v: PGLint); stdcall;
procedure glVertex4s(x,y,z,w: GLshort); stdcall;
procedure glVertex4sv(v: PGLshort); stdcall;
procedure glVertex(x,y: GLdouble); stdcall; overload;
procedure glVertex2(v: PGLdouble); stdcall; overload;
procedure glVertex(x,y: GLfloat); stdcall; overload;
procedure glVertex2(v: PGLfloat); stdcall; overload;
procedure glVertex(x,y: GLint); stdcall; overload;
procedure glVertex2(v: PGLint); stdcall; overload;
procedure glVertex(x,y: GLshort); stdcall; overload;
procedure glVertex2(v: PGLshort); stdcall; overload;
procedure glVertex(x,y,z: GLdouble); stdcall; overload;
procedure glVertex3(v: PGLdouble); stdcall; overload;
procedure glVertex(x,y,z: GLfloat); stdcall; overload;
procedure glVertex3(v: PGLfloat); stdcall; overload;
procedure glVertex(x,y,z: GLint); stdcall; overload;
procedure glVertex3(v: PGLint); stdcall; overload;
procedure glVertex(x,y,z: GLshort); stdcall; overload;
procedure glVertex3(v: PGLshort); stdcall; overload;
procedure glVertex(x,y,z,w: GLdouble); stdcall; overload;
procedure glVertex4(v: PGLdouble); stdcall; overload;
procedure glVertex(x,y,z,w: GLfloat); stdcall; overload;
procedure glVertex4(v: PGLfloat); stdcall; overload;
procedure glVertex(x,y,z,w: GLint); stdcall; overload;
procedure glVertex4(v: PGLint); stdcall; overload;
procedure glVertex(x,y,z,w: GLshort); stdcall; overload;
procedure glVertex4(v: PGLshort); stdcall; overload;


procedure glViewport(x,y: GLint; width, height: GLsizei); stdcall;
procedure glBindTexture(target: GLEnum; texture: GLuint); stdcall;
procedure glGenTextures(n: GLsizei; textures: PGLuint); stdcall;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall;

procedure glVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); stdcall;
procedure glNormalPointer(atype: GLenum; stride: GLsizei; data: Pointer); stdcall;
procedure glColorPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); stdcall;
procedure glTexCoordPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); stdcall;

function gluErrorString(errCode: GLenum): PChar; stdcall;
function gluErrorUnicodeStringEXT(errCode: GLenum): PWChar; stdcall;
function gluGetString(name: GLenum): PChar; stdcall;

procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz,
  upx, upy, upz: GLdouble); stdcall;
procedure gluOrtho2D(left, right, bottom, top: GLdouble); stdcall;
procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble); stdcall;
procedure gluPickMatrix(x, y, width, height: GLdouble; viewport: PGLint); stdcall;
function  gluProject(objx, objy, obyz: GLdouble;
                      modelMatrix: PGLdouble;
                      projMatrix: PGLdouble;
                      viewport: PGLint;
                      var winx, winy, winz: GLDouble): Integer; stdcall;
function  gluUnProject(winx, winy, winz: GLdouble;
                      modelMatrix: PGLdouble;
                      projMatrix: PGLdouble;
                      viewport: PGLint;
                      var objx, objy, objz: GLdouble): Integer; stdcall;
function  gluScaleImage(format: GLenum;
   widthin, heightin: GLint; typein: GLenum; datain: Pointer;
   widthout, heightout: GLint; typeout: GLenum; dataout: Pointer): Integer; stdcall;

function  gluBuild1DMipmaps(target: GLenum; components, width: GLint;
                             format, atype: GLenum; data: Pointer): Integer; stdcall;
function  gluBuild2DMipmaps(target: GLenum; components, width: GLint;
                             format, atype: GLenum; data: Pointer): Integer; stdcall;

type
  _GLUquadricObj = record end;
  GLUquadricObj = ^_GLUquadricObj;

  GLUquadricErrorProc = procedure(error: GLenum) stdcall;
  TGLUquadricErrorProc = GLUquadricErrorProc;

function  gluNewQuadric: GLUquadricObj; stdcall;
procedure gluDeleteQuadric(state: GLUquadricObj); stdcall;
procedure gluQuadricNormals(quadObject: GLUquadricObj; normals: GLenum); stdcall;
procedure gluQuadricTexture(quadObject: GLUquadricObj; textureCoords: GLboolean); stdcall;
procedure gluQuadricOrientation(quadObject: GLUquadricObj; orientation: GLenum); stdcall;
procedure gluQuadricDrawStyle(quadObject: GLUquadricObj; drawStyle: GLenum); stdcall;
procedure gluCylinder(quadObject: GLUquadricObj;
  baseRadius, topRadius, height: GLdouble; slices, stacks: GLint); stdcall;
procedure gluDisk(quadObject: GLUquadricObj;
  innerRadius, outerRadius: GLdouble; slices, loops: GLint); stdcall;
procedure gluPartialDisk(quadObject: GLUquadricObj;
  innerRadius, outerRadius: GLdouble; slices, loops: GLint;
  startAngle, sweepAngle: GLdouble); stdcall;
procedure gluSphere(quadObject: GLUquadricObj; radius: GLdouble; slices, loops: GLint); stdcall;
procedure gluQuadricCallback(quadObject: GLUquadricObj; which: GLenum;
  callback: Pointer); stdcall;

type
  _GLUtesselator = record end;
  GLUtesselator = ^_GLUtesselator;

  // tesselator callback procedure types
  GLUtessBeginProc = procedure(a: GLenum) stdcall;
  TGLUtessBeginProc = GLUtessBeginProc;
  GLUtessEdgeFlagProc = procedure(flag: GLboolean) stdcall;
  TGLUtessEdgeFlagProc = GLUtessEdgeFlagProc;
  GLUtessVertexProc = procedure(p: Pointer) stdcall;
  TGLUtessVertexProc = GLUtessVertexProc;
  GLUtessEndProc = procedure stdcall;
  TGLUtessEndProc = GLUtessEndProc;
  GLUtessErrorProc = TGLUquadricErrorProc;
  GLUtessCombineProc = procedure(a: PGLdouble; b: Pointer;
                                   c: PGLfloat; var d: Pointer) stdcall;
  TGLUtessCombineProc = GLUtessCombineProc;

function gluNewTess: GLUtesselator; stdcall;
procedure gluDeleteTess(tess: GLUtesselator); stdcall;
procedure gluTessBeginPolygon(tess: GLUtesselator); stdcall;
procedure gluTessBeginContour(tess: GLUtesselator); stdcall;
procedure gluTessVertex(tess: GLUtesselator; coords: PGLdouble; data: Pointer); stdcall;
procedure gluTessEndContour(tess: GLUtesselator); stdcall;
procedure gluTessEndPolygon(tess: GLUtesselator); stdcall;
procedure gluTessProperty(tess: GLUtesselator; which: GLenum; value: GLdouble); stdcall;
procedure gluTessNormal(tess: GLUtesselator; x,y,z: GLdouble); stdcall;
procedure gluTessCallback(tess: GLUtesselator; which: GLenum; callback: pointer); stdcall;

type
  TGLUnurbsObj = record end;
  GLUnurbsObj = ^TGLUnurbsObj;

  GLUnurbsErrorProc = GLUquadricErrorProc;
  TGLUnurbsErrorProc = GLUnurbsErrorProc;

function gluNewNurbsRenderer: GLUnurbsObj; stdcall;
procedure gluDeleteNurbsRenderer(nobj: GLUnurbsObj); stdcall;
procedure gluBeginSurface(nobj: GLUnurbsObj); stdcall;
procedure gluBeginCurve(nobj: GLUnurbsObj); stdcall;
procedure gluEndCurve(nobj: GLUnurbsObj); stdcall;
procedure gluEndSurface(nobj: GLUnurbsObj); stdcall;
procedure gluBeginTrim(nobj: GLUnurbsObj); stdcall;
procedure gluEndTrim(nobj: GLUnurbsObj); stdcall;
procedure gluPwlCurve(nobj: GLUnurbsObj; count: GLint; points: PGLfloat;
  stride: GLint; _type: GLenum); stdcall;
procedure gluNurbsCurve(nobj: GLUnurbsObj; nknots: GLint; knot: PGLfloat;
  stride: GLint; ctlpts: PGLfloat; order: GLint; _type: GLenum); stdcall;
procedure gluNurbsSurface(nobj: GLUnurbsObj;
  sknot_count: GLint; sknot: PGLfloat;
  tknot_count: GLint; tknot: PGLfloat;
  s_stride, t_stride: GLint;
  ctlpts: PGLfloat; sorder, torder: GLint; _type: GLenum); stdcall;
procedure gluLoadSamplingMatrices(nobj: GLUnurbsObj;
  modelMatrix: PGLdouble; projMatrix: PGLdouble; viewport: PGLint); stdcall;
procedure gluNurbsProperty(nobj: GLUnurbsObj; prop: GLenum; value: GLfloat); stdcall;
procedure gluGetNurbsProperty(nobj: GLUnurbsObj; prop: GLenum; var value: GLfloat); stdcall;
procedure gluNurbsCallback(nobj: GLUnurbsObj; which: GLenum; callback: pointer); stdcall;

{****           Generic constants               ****}
const
  GLU_VERSION_1_1  =               1;

{ Errors:(return value 0 = no error) }
  GLU_INVALID_ENUM       = 100900;
  GLU_INVALID_VALUE      = 100901;
  GLU_OUT_OF_MEMORY      = 100902;
  GLU_INCOMPATIBLE_GL_VERSION  =   100903;

{ gets }
  GLU_VERSION            = 100800;
  GLU_EXTENSIONS         = 100801;

{ For laughs: }
  GLU_TRUE               = GL_TRUE;
  GLU_FALSE              = GL_FALSE;

{***           Quadric constants               ***}

{ Types of normals: }
  GLU_SMOOTH             = 100000;
  GLU_FLAT               = 100001;
  GLU_NONE               = 100002;

{ DrawStyle types: }
  GLU_POINT              = 100010;
  GLU_LINE               = 100011;
  GLU_FILL               = 100012;
  GLU_SILHOUETTE         = 100013;

{ Orientation types: }
  GLU_OUTSIDE            = 100020;
  GLU_INSIDE             = 100021;

{ Callback types: }


{***           Tesselation constants           ***}

  GLU_TESS_MAX_COORD     =         1.0e150;

{ Property types: }
  GLU_TESS_WINDING_RULE  =         100110;
  GLU_TESS_BOUNDARY_ONLY =         100111;
  GLU_TESS_TOLERANCE     =         100112;

{ Possible winding rules: }
  GLU_TESS_WINDING_ODD          =  100130;
  GLU_TESS_WINDING_NONZERO      =  100131;
  GLU_TESS_WINDING_POSITIVE     =  100132;
  GLU_TESS_WINDING_NEGATIVE     =  100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO  =  100134;

{ Callback types: }
  GLU_TESS_BEGIN     = 100100 ;     { void(*)(GLenum    type)         }
  GLU_TESS_VERTEX    = 100101 ;     { void(*)(void      *data)        }
  GLU_TESS_END       = 100102 ;     { void(*)(void)                   }
  GLU_TESS_ERROR     = 100103 ;     { void(*)(GLenum    errno)        }
  GLU_TESS_EDGE_FLAG = 100104 ;     { void(*)(GLboolean boundaryEdge) }
  GLU_TESS_COMBINE   = 100105 ;     { void(*)(GLdouble  coords[3],;
                                                    void      *data[4],;
                                                    GLfloat   weight[4],;
                                                    void      **dataOut)    }

{ Errors: }
  GLU_TESS_ERROR1    = 100151;
  GLU_TESS_ERROR2    = 100152;
  GLU_TESS_ERROR3    = 100153;
  GLU_TESS_ERROR4    = 100154;
  GLU_TESS_ERROR5    = 100155;
  GLU_TESS_ERROR6    = 100156;
  GLU_TESS_ERROR7    = 100157;
  GLU_TESS_ERROR8    = 100158;

  GLU_TESS_MISSING_BEGIN_POLYGON  = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR  = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON    = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR    = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE        = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK  = GLU_TESS_ERROR6;

{***           NURBS constants                 ***}

{ Properties: }
  GLU_AUTO_LOAD_MATRIX          =  100200;
  GLU_CULLING                   =  100201;
  GLU_SAMPLING_TOLERANCE        =  100203;
  GLU_DISPLAY_MODE              =  100204;
  GLU_PARAMETRIC_TOLERANCE      =  100202;
  GLU_SAMPLING_METHOD           =  100205;
  GLU_U_STEP                    =  100206;
  GLU_V_STEP                    =  100207;

{ Sampling methods: }
  GLU_PATH_LENGTH               =  100215;
  GLU_PARAMETRIC_ERROR          =  100216;
  GLU_DOMAIN_DISTANCE           =  100217;

{ Trimming curve types }
  GLU_MAP1_TRIM_2       =  100210;
  GLU_MAP1_TRIM_3       =  100211;

{ Display modes: }
  GLU_OUTLINE_POLYGON    = 100240;
  GLU_OUTLINE_PATCH      = 100241;

{ Callbacks: }
{      GLU_ERROR               100103 }

{ Errors: }
  GLU_NURBS_ERROR1       = 100251;
  GLU_NURBS_ERROR2       = 100252;
  GLU_NURBS_ERROR3       = 100253;
  GLU_NURBS_ERROR4       = 100254;
  GLU_NURBS_ERROR5       = 100255;
  GLU_NURBS_ERROR6       = 100256;
  GLU_NURBS_ERROR7       = 100257;
  GLU_NURBS_ERROR8       = 100258;
  GLU_NURBS_ERROR9       = 100259;
  GLU_NURBS_ERROR10      = 100260;
  GLU_NURBS_ERROR11      = 100261;
  GLU_NURBS_ERROR12      = 100262;
  GLU_NURBS_ERROR13      = 100263;
  GLU_NURBS_ERROR14      = 100264;
  GLU_NURBS_ERROR15      = 100265;
  GLU_NURBS_ERROR16      = 100266;
  GLU_NURBS_ERROR17      = 100267;
  GLU_NURBS_ERROR18      = 100268;
  GLU_NURBS_ERROR19      = 100269;
  GLU_NURBS_ERROR20      = 100270;
  GLU_NURBS_ERROR21      = 100271;
  GLU_NURBS_ERROR22      = 100272;
  GLU_NURBS_ERROR23      = 100273;
  GLU_NURBS_ERROR24      = 100274;
  GLU_NURBS_ERROR25      = 100275;
  GLU_NURBS_ERROR26      = 100276;
  GLU_NURBS_ERROR27      = 100277;
  GLU_NURBS_ERROR28      = 100278;
  GLU_NURBS_ERROR29      = 100279;
  GLU_NURBS_ERROR30      = 100280;
  GLU_NURBS_ERROR31      = 100281;
  GLU_NURBS_ERROR32      = 100282;
  GLU_NURBS_ERROR33      = 100283;
  GLU_NURBS_ERROR34      = 100284;
  GLU_NURBS_ERROR35      = 100285;
  GLU_NURBS_ERROR36      = 100286;
  GLU_NURBS_ERROR37      = 100287;

function wglGetProcAddress(ProcName: PChar): Pointer;  stdcall;

const
  glu32 = 'glu32.dll';

implementation

procedure glAccum; external opengl32;
procedure glAlphaFunc; external opengl32;
procedure glBegin; external opengl32;
procedure glBitmap; external opengl32;
procedure glBlendFunc; external opengl32;
procedure glCallList; external opengl32;
procedure glCallLists; external opengl32;
procedure glClear; external opengl32;
procedure glClearAccum; external opengl32;
procedure glClearColor; external opengl32;
procedure glClearDepth; external opengl32;
procedure glClearIndex; external opengl32;
procedure glClearStencil; external opengl32;
procedure glClipPlane; external opengl32;
procedure glColor3b; external opengl32;
procedure glColor3bv; external opengl32;
procedure glColor3d; external opengl32;
procedure glColor3dv; external opengl32;
procedure glColor3f; external opengl32;
procedure glColor3fv; external opengl32;
procedure glColor3i; external opengl32;
procedure glColor3iv; external opengl32;
procedure glColor3s; external opengl32;
procedure glColor3sv; external opengl32;
procedure glColor3ub; external opengl32;
procedure glColor3ubv; external opengl32;
procedure glColor3ui; external opengl32;
procedure glColor3uiv; external opengl32;
procedure glColor3us; external opengl32;
procedure glColor3usv; external opengl32;
procedure glColor4b; external opengl32;
procedure glColor4bv; external opengl32;
procedure glColor4d; external opengl32;
procedure glColor4dv; external opengl32;
procedure glColor4f; external opengl32;
procedure glColor4fv; external opengl32;
procedure glColor4i; external opengl32;
procedure glColor4iv; external opengl32;
procedure glColor4s; external opengl32;
procedure glColor4sv; external opengl32;
procedure glColor4ub; external opengl32;
procedure glColor4ubv; external opengl32;
procedure glColor4ui; external opengl32;
procedure glColor4uiv; external opengl32;
procedure glColor4us; external opengl32;
procedure glColor4usv; external opengl32;
procedure glColor(red, green, blue: GLbyte); external opengl32 name 'glColor3b';
procedure glColor(red, green, blue: GLdouble); external opengl32 name 'glColor3d';
procedure glColor(red, green, blue: GLfloat); external opengl32 name 'glColor3f';
procedure glColor(red, green, blue: GLint); external opengl32 name 'glColor3i';
procedure glColor(red, green, blue: GLshort); external opengl32 name 'glColor3s';
procedure glColor(red, green, blue: GLubyte); external opengl32 name 'glColor3ub';
procedure glColor(red, green, blue: GLuint); external opengl32 name 'glColor3ui';
procedure glColor(red, green, blue: GLushort); external opengl32 name 'glColor3us';
procedure glColor(red, green, blue, alpha: GLbyte); external opengl32 name 'glColor4b';
procedure glColor(red, green, blue, alpha: GLdouble); external opengl32 name 'glColor4d';
procedure glColor(red, green, blue, alpha: GLfloat); external opengl32 name 'glColor4f';
procedure glColor(red, green, blue, alpha: GLint); external opengl32 name 'glColor4i';
procedure glColor(red, green, blue, alpha: GLshort); external opengl32 name 'glColor4s';
procedure glColor(red, green, blue, alpha: GLubyte); external opengl32 name 'glColor4ub';
procedure glColor(red, green, blue, alpha: GLuint); external opengl32 name 'glColor4ui';
procedure glColor(red, green, blue, alpha: GLushort); external opengl32 name 'glColor4us';
procedure glColor3(v: PGLbyte); external opengl32 name 'glColor3bv';
procedure glColor3(v: PGLdouble); external opengl32 name 'glColor3dv';
procedure glColor3(v: PGLfloat); external opengl32 name 'glColor3fv';
procedure glColor3(v: PGLint); external opengl32 name 'glColor3iv';
procedure glColor3(v: PGLshort); external opengl32 name 'glColor3sv';
procedure glColor3(v: PGLubyte); external opengl32 name 'glColor3ubv';
procedure glColor3(v: PGLuint); external opengl32 name 'glColor3uiv';
procedure glColor3(v: PGLushort); external opengl32 name 'glColor3usv';
procedure glColor4(v: PGLbyte); external opengl32 name 'glColor4bv';
procedure glColor4(v: PGLdouble); external opengl32 name 'glColor4dv';
procedure glColor4(v: PGLfloat); external opengl32 name 'glColor4fv';
procedure glColor4(v: PGLint); external opengl32 name 'glColor4iv';
procedure glColor4(v: PGLshort); external opengl32 name 'glColor4sv';
procedure glColor4(v: PGLubyte); external opengl32 name 'glColor4ubv';
procedure glColor4(v: PGLuint); external opengl32 name 'glColor4uiv';
procedure glColor4(v: PGLushort); external opengl32 name 'glColor4usv';
procedure glColorMask; external opengl32;
procedure glColorMaterial; external opengl32;
procedure glCopyPixels; external opengl32;
procedure glCullFace; external opengl32;
procedure glDeleteLists; external opengl32;
procedure glDepthFunc; external opengl32;
procedure glDepthMask; external opengl32;
procedure glDepthRange; external opengl32;
procedure glDisable; external opengl32;
procedure glDisableClientState; external opengl32;
procedure glEnableClientState; external opengl32;
procedure glDrawArrays; external opengl32;
procedure glDrawElements; external opengl32;
procedure glDrawBuffer; external opengl32;
procedure glDrawPixels; external opengl32;
procedure glEdgeFlag; external opengl32;
procedure glEdgeFlagv; external opengl32;
procedure glEnable; external opengl32;
procedure glEnd; external opengl32;
procedure glEndList; external opengl32;
procedure glEvalCoord1d; external opengl32;
procedure glEvalCoord1dv; external opengl32;
procedure glEvalCoord1f; external opengl32;
procedure glEvalCoord1fv; external opengl32;
procedure glEvalCoord2d; external opengl32;
procedure glEvalCoord2dv; external opengl32;
procedure glEvalCoord2f; external opengl32;
procedure glEvalCoord2fv; external opengl32;
procedure glEvalCoord(u: GLdouble); external opengl32 name 'glEvalCoord1d';
procedure glEvalCoord(u: GLfloat); external opengl32 name 'glEvalCoord1f';
procedure glEvalCoord(u,v: GLdouble); external opengl32 name 'glEvalCoord2d';
procedure glEvalCoord(u,v: GLfloat); external opengl32 name 'glEvalCoord2f';
procedure glEvalCoord1(v: PGLdouble); external opengl32 name 'glEvalCoord1dv';
procedure glEvalCoord1(v: PGLfloat); external opengl32 name 'glEvalCoord1fv';
procedure glEvalCoord2(v: PGLdouble); external opengl32 name 'glEvalCoord2dv';
procedure glEvalCoord2(v: PGLfloat); external opengl32 name 'glEvalCoord2fv';
procedure glEvalMesh1; external opengl32;
procedure glEvalMesh2; external opengl32;
procedure glEvalMesh(mode: GLenum; i1, i2: GLint); external opengl32 name 'glEvalMesh1';
procedure glEvalMesh(mode: GLenum; i1, i2, j1, j2: GLint); external opengl32 name 'glEvalMesh2';
procedure glEvalPoint1; external opengl32;
procedure glEvalPoint2; external opengl32;
procedure glEvalPoint(i: GLint); external opengl32 name 'glEvalPoint1';
procedure glEvalPoint(i,j: GLint); external opengl32 name 'glEvalPoint2';
procedure glFeedbackBuffer; external opengl32;
procedure glFinish; external opengl32;
procedure glFlush; external opengl32;
procedure glFogf; external opengl32;
procedure glFogfv; external opengl32;
procedure glFogi; external opengl32;
procedure glFogiv; external opengl32;
procedure glFog(pname: GLenum; param: GLfloat); external opengl32 name 'glFogf';
procedure glFog(pname: GLenum; params: PGLfloat); external opengl32 name 'glFogfv';
procedure glFog(pname: GLenum; param: GLint); external opengl32 name 'glFogi';
procedure glFog(pname: GLenum; params: PGLint); external opengl32 name 'glFogiv';
procedure glFrontFace; external opengl32;
procedure glFrustum; external opengl32;
function  glGenLists; external opengl32;
procedure glGetBooleanv; external opengl32;
procedure glGetClipPlane; external opengl32;
procedure glGetDoublev; external opengl32;
function  glGetError: GLenum; external opengl32;
procedure glGetFloatv; external opengl32;
procedure glGetIntegerv; external opengl32;
procedure glGetLightfv; external opengl32;
procedure glGetLightiv; external opengl32;
procedure glGetLight(light: GLenum; pname: GLenum; params: PGLfloat); external opengl32 name 'glGetLightfv';
procedure glGetLight(light: GLenum; pname: GLenum; params: PGLint); external opengl32 name 'glGetLightiv';
procedure glGetMapdv; external opengl32;
procedure glGetMapfv; external opengl32;
procedure glGetMapiv; external opengl32;
procedure glGetMap(target: GLenum; query: GLenum; v: PGLdouble); external opengl32 name 'glGetMapdv';
procedure glGetMap(target: GLenum; query: GLenum; v: PGLfloat); external opengl32 name 'glGetMapfv';
procedure glGetMap(target: GLenum; query: GLenum; v: PGLint); external opengl32 name 'glGetMapiv';
procedure glGetMaterialfv; external opengl32;
procedure glGetMaterialiv; external opengl32;
procedure glGetMaterial(face: GLenum; pname: GLenum; params: PGLfloat); external opengl32 name 'glGetMaterialfv';
procedure glGetMaterial(face: GLenum; pname: GLenum; params: PGLint); external opengl32 name 'glGetMaterialiv';
procedure glGetPixelMapfv; external opengl32;
procedure glGetPixelMapuiv; external opengl32;
procedure glGetPixelMapusv; external opengl32;
procedure glGetPixelMap(map: GLenum; values: PGLfloat); external opengl32 name 'glGetPixelMapfv';
procedure glGetPixelMap(map: GLenum; values: PGLuint); external opengl32 name 'glGetPixelMapuiv';
procedure glGetPixelMap(map: GLenum; values: PGLushort); external opengl32 name 'glGetPixelMapusv';
procedure glGetPolygonStipple; external opengl32;
function  glGetString; external opengl32;
procedure glGetTexEnvfv; external opengl32;
procedure glGetTexEnviv; external opengl32;
procedure glGetTexEnv(target: GLenum; pname: GLenum; params: PGLfloat); external opengl32 name 'glGetTexEnvfv';
procedure glGetTexEnv(target: GLenum; pname: GLenum; params: PGLint); external opengl32 name 'glGetTexEnviv';
procedure glGetTexGendv; external opengl32;
procedure glGetTexGenfv; external opengl32;
procedure glGetTexGeniv; external opengl32;
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLdouble); external opengl32 name 'glGetTexGendv';
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLfloat); external opengl32 name 'glGetTexGenfv';
procedure glGetTexGen(coord: GLenum; pname: GLenum; params: PGLint); external opengl32 name 'glGetTexGeniv';
procedure glGetTexImage; external opengl32;
procedure glGetTexLevelParameterfv; external opengl32;
procedure glGetTexLevelParameteriv; external opengl32;
procedure glGetTexLevelParameter(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); external opengl32 name 'glGetTexLevelParameterfv';
procedure glGetTexLevelParameter(target: GLenum; level: GLint; pname: GLenum; params: PGLint); external opengl32 name 'glGetTexLevelParameteriv';
procedure glGetTexParameterfv; external opengl32;
procedure glGetTexParameteriv; external opengl32;
procedure glGetTexParameter(target, pname: GLenum; params: PGLfloat); external opengl32 name 'glGetTexParameterfv';
procedure glGetTexParameter(target, pname: GLenum; params: PGLint); external opengl32 name 'glGetTexParameteriv';
procedure glHint; external opengl32;
procedure glIndexMask; external opengl32;
procedure glIndexd; external opengl32;
procedure glIndexdv; external opengl32;
procedure glIndexf; external opengl32;
procedure glIndexfv; external opengl32;
procedure glIndexi; external opengl32;
procedure glIndexiv; external opengl32;
procedure glIndexs; external opengl32;
procedure glIndexsv; external opengl32;
procedure glIndex(c: GLdouble); external opengl32 name 'glIndexd';
procedure glIndex(c: PGLdouble); external opengl32 name 'glIndexdv';
procedure glIndex(c: GLfloat); external opengl32 name 'glIndexf';
procedure glIndex(c: PGLfloat); external opengl32 name 'glIndexfv';
procedure glIndex(c: GLint); external opengl32 name 'glIndexi';
procedure glIndex(c: PGLint); external opengl32 name 'glIndexiv';
procedure glIndex(c: GLshort); external opengl32 name 'glIndexs';
procedure glIndex(c: PGLshort); external opengl32 name 'glIndexsv';
procedure glInitNames; external opengl32;
function  glIsEnabled; external opengl32;
function  glIsList; external opengl32;
procedure glLightModelf; external opengl32;
procedure glLightModelfv; external opengl32;
procedure glLightModeli; external opengl32;
procedure glLightModeliv; external opengl32;
procedure glLightModel(pname: GLenum; param: GLfloat); external opengl32 name 'glLightModelf';
procedure glLightModel(pname: GLenum; params: PGLfloat); external opengl32 name 'glLightModelfv';
procedure glLightModel(pname: GLenum; param: GLint); external opengl32 name 'glLightModeli';
procedure glLightModel(pname: GLenum; params: PGLint); external opengl32 name 'glLightModeliv';
procedure glLightf; external opengl32;
procedure glLightfv; external opengl32;
procedure glLighti; external opengl32;
procedure glLightiv; external opengl32;
procedure glLight(light, pname: GLenum; param: GLfloat); external opengl32 name 'glLightf';
procedure glLight(light, pname: GLenum; params: PGLfloat); external opengl32 name 'glLightfv';
procedure glLight(light, pname: GLenum; param: GLint); external opengl32 name 'glLighti';
procedure glLight(light, pname: GLenum; params: PGLint); external opengl32 name 'glLightiv';
procedure glLineStipple; external opengl32;
procedure glLineWidth; external opengl32;
procedure glListBase; external opengl32;
procedure glLoadIdentity; external opengl32;
procedure glLoadMatrixd; external opengl32;
procedure glLoadMatrixf; external opengl32;
procedure glLoadMatrix(m: PGLdouble); external opengl32 name 'glLoadMatrixd';
procedure glLoadMatrix(m: PGLfloat); external opengl32 name 'glLoadMatrixf';
procedure glLoadName; external opengl32;
procedure glLogicOp; external opengl32;
procedure glMap1d; external opengl32;
procedure glMap1f; external opengl32;
procedure glMap2d; external opengl32;
procedure glMap2f; external opengl32;
procedure glMap(target: GLenum; u1,u2: GLdouble; stride, order: GLint;
  Points: PGLdouble); external opengl32 name 'glMap1d';
procedure glMap(target: GLenum; u1,u2: GLfloat; stride, order: GLint;
  Points: PGLfloat); external opengl32 name 'glMap1f';
procedure glMap(target: GLenum;
  u1,u2: GLdouble; ustride, uorder: GLint;
  v1,v2: GLdouble; vstride, vorder: GLint; Points: PGLdouble); external opengl32 name 'glMap2d';
procedure glMap(target: GLenum;
  u1,u2: GLfloat; ustride, uorder: GLint;
  v1,v2: GLfloat; vstride, vorder: GLint; Points: PGLfloat); external opengl32 name 'glMap2f';
procedure glMapGrid1d; external opengl32;
procedure glMapGrid1f; external opengl32;
procedure glMapGrid2d; external opengl32;
procedure glMapGrid2f; external opengl32;
procedure glMapGrid(un: GLint; u1, u2: GLdouble); external opengl32 name 'glMapGrid1d';
procedure glMapGrid(un: GLint; u1, u2: GLfloat); external opengl32 name 'glMapGrid1f';
procedure glMapGrid(un: GLint; u1, u2: GLdouble;
                    vn: GLint; v1, v2: GLdouble); external opengl32 name 'glMapGrid2d';
procedure glMapGrid(un: GLint; u1, u2: GLfloat;
                    vn: GLint; v1, v2: GLfloat); external opengl32 name 'glMapGrid2f';
procedure glMaterialf; external opengl32;
procedure glMaterialfv; external opengl32;
procedure glMateriali; external opengl32;
procedure glMaterialiv; external opengl32;
procedure glMaterial(face, pname: GLenum; param: GLfloat); external opengl32 name 'glMaterialf';
procedure glMaterial(face, pname: GLenum; params: PGLfloat); external opengl32 name 'glMaterialfv';
procedure glMaterial(face, pname: GLenum; param: GLint); external opengl32 name 'glMateriali';
procedure glMaterial(face, pname: GLenum; params: PGLint); external opengl32 name 'glMaterialiv';
procedure glMatrixMode; external opengl32;
procedure glMultMatrixd; external opengl32;
procedure glMultMatrixf; external opengl32;
procedure glMultMatrix(m: PGLdouble); external opengl32 name 'glMultMatrixd';
procedure glMultMatrix(m: PGLfloat); external opengl32 name 'glMultMatrixf';
procedure glNewList; external opengl32;
procedure glNormal3b; external opengl32;
procedure glNormal3bv; external opengl32;
procedure glNormal3d; external opengl32;
procedure glNormal3dv; external opengl32;
procedure glNormal3f; external opengl32;
procedure glNormal3fv; external opengl32;
procedure glNormal3i; external opengl32;
procedure glNormal3iv; external opengl32;
procedure glNormal3s; external opengl32;
procedure glNormal3sv; external opengl32;
procedure glNormal(nx, ny, nz: GLbyte); external opengl32 name 'glNormal3b';
procedure glNormal3(v: PGLbyte); external opengl32 name 'glNormal3bv';
procedure glNormal(nx, ny, nz: GLdouble); external opengl32 name 'glNormal3d';
procedure glNormal3(v: PGLdouble); external opengl32 name 'glNormal3dv';
procedure glNormal(nx, ny, nz: GLFloat); external opengl32 name 'glNormal3f';
procedure glNormal3(v: PGLfloat); external opengl32 name 'glNormal3fv';
procedure glNormal(nx, ny, nz: GLint); external opengl32 name 'glNormal3i';
procedure glNormal3(v: PGLint); external opengl32 name 'glNormal3iv';
procedure glNormal(nx, ny, nz: GLshort); external opengl32 name 'glNormal3s';
procedure glNormal3(v: PGLshort); external opengl32 name 'glNormal3sv';
procedure glOrtho; external opengl32;
procedure glPassThrough; external opengl32;
procedure glPixelMapfv; external opengl32;
procedure glPixelMapuiv; external opengl32;
procedure glPixelMapusv; external opengl32;
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLfloat); external opengl32 name 'glPixelMapfv';
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLuint); external opengl32 name 'glPixelMapuiv';
procedure glPixelMap(map: GLenum; mapsize: GLint; values: PGLushort); external opengl32 name 'glPixelMapusv';
procedure glPixelStoref; external opengl32;
procedure glPixelStorei; external opengl32;
procedure glPixelStore(pname: GLenum; param: GLfloat); external opengl32 name 'glPixelStoref';
procedure glPixelStore(pname: GLenum; param: GLint); external opengl32 name 'glPixelStorei';
procedure glPixelTransferf; external opengl32;
procedure glPixelTransferi; external opengl32;
procedure glPixelTransfer(pname: GLenum; param: GLfloat); external opengl32 name 'glPixelTransferf';
procedure glPixelTransfer(pname: GLenum; param: GLint); external opengl32 name 'glPixelTransferi';
procedure glPixelZoom; external opengl32;
procedure glPointSize; external opengl32;
procedure glPolygonMode; external opengl32;
procedure glPolygonOffset; external opengl32;
procedure glPolygonStipple; external opengl32;
procedure glPopAttrib; external opengl32;
procedure glPopMatrix; external opengl32;
procedure glPopName; external opengl32;
procedure glPushAttrib; external opengl32;
procedure glPushMatrix; external opengl32;
procedure glPushName; external opengl32;
procedure glRasterPos2d; external opengl32;
procedure glRasterPos2dv; external opengl32;
procedure glRasterPos2f; external opengl32;
procedure glRasterPos2fv; external opengl32;
procedure glRasterPos2i; external opengl32;
procedure glRasterPos2iv; external opengl32;
procedure glRasterPos2s; external opengl32;
procedure glRasterPos2sv; external opengl32;
procedure glRasterPos3d; external opengl32;
procedure glRasterPos3dv; external opengl32;
procedure glRasterPos3f; external opengl32;
procedure glRasterPos3fv; external opengl32;
procedure glRasterPos3i; external opengl32;
procedure glRasterPos3iv; external opengl32;
procedure glRasterPos3s; external opengl32;
procedure glRasterPos3sv; external opengl32;
procedure glRasterPos4d; external opengl32;
procedure glRasterPos4dv; external opengl32;
procedure glRasterPos4f; external opengl32;
procedure glRasterPos4fv; external opengl32;
procedure glRasterPos4i; external opengl32;
procedure glRasterPos4iv; external opengl32;
procedure glRasterPos4s; external opengl32;
procedure glRasterPos4sv; external opengl32;
procedure glRasterPos(x,y: GLdouble); external opengl32 name 'glRasterPos2d';
procedure glRasterPos2(v: PGLdouble); external opengl32 name 'glRasterPos2dv';
procedure glRasterPos(x,y: GLfloat); external opengl32 name 'glRasterPos2f';
procedure glRasterPos2(v: PGLfloat); external opengl32 name 'glRasterPos2fv';
procedure glRasterPos(x,y: GLint); external opengl32 name 'glRasterPos2i';
procedure glRasterPos2(v: PGLint); external opengl32 name 'glRasterPos2iv';
procedure glRasterPos(x,y: GLshort); external opengl32 name 'glRasterPos2s';
procedure glRasterPos2(v: PGLshort); external opengl32 name 'glRasterPos2sv';
procedure glRasterPos(x,y,z: GLdouble); external opengl32 name 'glRasterPos3d';
procedure glRasterPos3(v: PGLdouble); external opengl32 name 'glRasterPos3dv';
procedure glRasterPos(x,y,z: GLfloat); external opengl32 name 'glRasterPos3f';
procedure glRasterPos3(v: PGLfloat); external opengl32 name 'glRasterPos3fv';
procedure glRasterPos(x,y,z: GLint); external opengl32 name 'glRasterPos3i';
procedure glRasterPos3(v: PGLint); external opengl32 name 'glRasterPos3iv';
procedure glRasterPos(x,y,z: GLshort); external opengl32 name 'glRasterPos3s';
procedure glRasterPos3(v: PGLshort); external opengl32 name 'glRasterPos3sv';
procedure glRasterPos(x,y,z,w: GLdouble); external opengl32 name 'glRasterPos4d';
procedure glRasterPos4(v: PGLdouble); external opengl32 name 'glRasterPos4dv';
procedure glRasterPos(x,y,z,w: GLfloat); external opengl32 name 'glRasterPos4f';
procedure glRasterPos4(v: PGLfloat); external opengl32 name 'glRasterPos4fv';
procedure glRasterPos(x,y,z,w: GLint); external opengl32 name 'glRasterPos4i';
procedure glRasterPos4(v: PGLint); external opengl32 name 'glRasterPos4iv';
procedure glRasterPos(x,y,z,w: GLshort); external opengl32 name 'glRasterPos4s';
procedure glRasterPos4(v: PGLshort); external opengl32 name 'glRasterPos4sv';
procedure glReadBuffer; external opengl32;
procedure glReadPixels; external opengl32;
procedure glRectd; external opengl32;
procedure glRectdv; external opengl32;
procedure glRectf; external opengl32;
procedure glRectfv; external opengl32;
procedure glRecti; external opengl32;
procedure glRectiv; external opengl32;
procedure glRects; external opengl32;
procedure glRectsv; external opengl32;
procedure glRect(x1, y1, x2, y2: GLdouble); external opengl32 name 'glRectd';
procedure glRect(v1, v2: PGLdouble); external opengl32 name 'glRectdv';
procedure glRect(x1, y1, x2, y2: GLfloat); external opengl32 name 'glRectf';
procedure glRect(v1, v2: PGLfloat); external opengl32 name 'glRectfv';
procedure glRect(x1, y1, x2, y2: GLint); external opengl32 name 'glRecti';
procedure glRect(v1, v2: PGLint); external opengl32 name 'glRectiv';
procedure glRect(x1, y1, x2, y2: GLshort); external opengl32 name 'glRects';
procedure glRect(v1, v2: PGLshort); external opengl32 name 'glRectsv';
function  glRenderMode; external opengl32;
procedure glRotated; external opengl32;
procedure glRotatef; external opengl32;
procedure glRotate(angle, x,y,z: GLdouble); external opengl32 name 'glRotated';
procedure glRotate(angle, x,y,z: GLfloat); external opengl32 name 'glRotatef';
procedure glScaled; external opengl32;
procedure glScalef; external opengl32;
procedure glScale(x,y,z: GLdouble); external opengl32 name 'glScaled';
procedure glScale(x,y,z: GLfloat); external opengl32 name 'glScalef';
procedure glScissor; external opengl32;
procedure glSelectBuffer; external opengl32;
procedure glShadeModel; external opengl32;
procedure glStencilFunc; external opengl32;
procedure glStencilMask; external opengl32;
procedure glStencilOp; external opengl32;
procedure glTexCoord1d; external opengl32;
procedure glTexCoord1dv; external opengl32;
procedure glTexCoord1f; external opengl32;
procedure glTexCoord1fv; external opengl32;
procedure glTexCoord1i; external opengl32;
procedure glTexCoord1iv; external opengl32;
procedure glTexCoord1s; external opengl32;
procedure glTexCoord1sv; external opengl32;
procedure glTexCoord2d; external opengl32;
procedure glTexCoord2dv; external opengl32;
procedure glTexCoord2f; external opengl32;
procedure glTexCoord2fv; external opengl32;
procedure glTexCoord2i; external opengl32;
procedure glTexCoord2iv; external opengl32;
procedure glTexCoord2s; external opengl32;
procedure glTexCoord2sv; external opengl32;
procedure glTexCoord3d; external opengl32;
procedure glTexCoord3dv; external opengl32;
procedure glTexCoord3f; external opengl32;
procedure glTexCoord3fv; external opengl32;
procedure glTexCoord3i; external opengl32;
procedure glTexCoord3iv; external opengl32;
procedure glTexCoord3s; external opengl32;
procedure glTexCoord3sv; external opengl32;
procedure glTexCoord4d; external opengl32;
procedure glTexCoord4dv; external opengl32;
procedure glTexCoord4f; external opengl32;
procedure glTexCoord4fv; external opengl32;
procedure glTexCoord4i; external opengl32;
procedure glTexCoord4iv; external opengl32;
procedure glTexCoord4s; external opengl32;
procedure glTexCoord4sv; external opengl32;
procedure glTexCoord(s: GLdouble); external opengl32 name 'glTexCoord1d';
procedure glTexCoord1(v: PGLdouble); external opengl32 name 'glTexCoord1dv';
procedure glTexCoord(s: GLfloat); external opengl32 name 'glTexCoord1f';
procedure glTexCoord1(v: PGLfloat); external opengl32 name 'glTexCoord1fv';
procedure glTexCoord(s: GLint); external opengl32 name 'glTexCoord1i';
procedure glTexCoord1(v: PGLint); external opengl32 name 'glTexCoord1iv';
procedure glTexCoord(s: GLshort); external opengl32 name 'glTexCoord1s';
procedure glTexCoord1(v: PGLshort); external opengl32 name 'glTexCoord1sv';
procedure glTexCoord(s,t: GLdouble); external opengl32 name 'glTexCoord2d';
procedure glTexCoord2(v: PGLdouble); external opengl32 name 'glTexCoord2dv';
procedure glTexCoord(s,t: GLfloat); external opengl32 name 'glTexCoord2f';
procedure glTexCoord2(v: PGLfloat); external opengl32 name 'glTexCoord2fv';
procedure glTexCoord(s,t: GLint); external opengl32 name 'glTexCoord2i';
procedure glTexCoord2(v: PGLint); external opengl32 name 'glTexCoord2iv';
procedure glTexCoord(s,t: GLshort); external opengl32 name 'glTexCoord2s';
procedure glTexCoord2(v: PGLshort); external opengl32 name 'glTexCoord2sv';
procedure glTexCoord(s,t,r: GLdouble); external opengl32 name 'glTexCoord3d';
procedure glTexCoord3(v: PGLdouble); external opengl32 name 'glTexCoord3dv';
procedure glTexCoord(s,t,r: GLfloat); external opengl32 name 'glTexCoord3f';
procedure glTexCoord3(v: PGLfloat); external opengl32 name 'glTexCoord3fv';
procedure glTexCoord(s,t,r: GLint); external opengl32 name 'glTexCoord3i';
procedure glTexCoord3(v: PGLint); external opengl32 name 'glTexCoord3iv';
procedure glTexCoord(s,t,r: GLshort); external opengl32 name 'glTexCoord3s';
procedure glTexCoord3(v: PGLshort); external opengl32 name 'glTexCoord3sv';
procedure glTexCoord(s,t,r,q: GLdouble); external opengl32 name 'glTexCoord4d';
procedure glTexCoord4(v: PGLdouble); external opengl32 name 'glTexCoord4dv';
procedure glTexCoord(s,t,r,q: GLfloat); external opengl32 name 'glTexCoord4f';
procedure glTexCoord4(v: PGLfloat); external opengl32 name 'glTexCoord4fv';
procedure glTexCoord(s,t,r,q: GLint); external opengl32 name 'glTexCoord4i';
procedure glTexCoord4(v: PGLint); external opengl32 name 'glTexCoord4iv';
procedure glTexCoord(s,t,r,q: GLshort); external opengl32 name 'glTexCoord4s';
procedure glTexCoord4(v: PGLshort); external opengl32 name 'glTexCoord4sv';
procedure glTexEnvf; external opengl32;
procedure glTexEnvfv; external opengl32;
procedure glTexEnvi; external opengl32;
procedure glTexEnviv; external opengl32;
procedure glTexEnv(target, pname: GLenum; param: GLfloat); external opengl32 name 'glTexEnvf';
procedure glTexEnv(target, pname: GLenum; params: PGLfloat); external opengl32 name 'glTexEnvfv';
procedure glTexEnv(target, pname: GLenum; param: GLint); external opengl32 name 'glTexEnvi';
procedure glTexEnv(target, pname: GLenum; params: PGLint); external opengl32 name 'glTexEnviv';
procedure glTexGend; external opengl32;
procedure glTexGendv; external opengl32;
procedure glTexGenf; external opengl32;
procedure glTexGenfv; external opengl32;
procedure glTexGeni; external opengl32;
procedure glTexGeniv; external opengl32;
procedure glTexGen(coord, pname: GLenum; param: GLdouble); external opengl32 name 'glTexGend';
procedure glTexGen(coord, pname: GLenum; params: PGLdouble); external opengl32 name 'glTexGendv';
procedure glTexGen(coord, pname: GLenum; param: GLfloat); external opengl32 name 'glTexGenf';
procedure glTexGen(coord, pname: GLenum; params: PGLfloat); external opengl32 name 'glTexGenfv';
procedure glTexGen(coord, pname: GLenum; param: GLint); external opengl32 name 'glTexGeni';
procedure glTexGen(coord, pname: GLenum; params: PGLint); external opengl32 name 'glTexGeniv';
procedure glTexImage1D; external opengl32;
procedure glTexImage2D; external opengl32;
procedure glCopyTexImage1D; external opengl32;
procedure glCopyTexImage2D; external opengl32;
procedure glTexParameterf; external opengl32;
procedure glTexParameterfv; external opengl32;
procedure glTexParameteri; external opengl32;
procedure glTexParameteriv; external opengl32;
procedure glTexParameter(target, pname: GLenum; param: GLfloat); external opengl32 name 'glTexParameterf';
procedure glTexParameter(target, pname: GLenum; params: PGLfloat); external opengl32 name 'glTexParameterfv';
procedure glTexParameter(target, pname: GLenum; param: GLint); external opengl32 name 'glTexParameteri';
procedure glTexParameter(target, pname: GLenum; params: PGLint); external opengl32 name 'glTexParameteriv';
procedure glTranslated; external opengl32;
procedure glTranslatef; external opengl32;
procedure glTranslate(x,y,z: GLdouble); external opengl32 name 'glTranslated';
procedure glTranslate(x,y,z: GLfloat); external opengl32 name 'glTranslatef';
procedure glVertex2d; external opengl32;
procedure glVertex2dv; external opengl32;
procedure glVertex2f; external opengl32;
procedure glVertex2fv; external opengl32;
procedure glVertex2i; external opengl32;
procedure glVertex2iv; external opengl32;
procedure glVertex2s; external opengl32;
procedure glVertex2sv; external opengl32;
procedure glVertex3d; external opengl32;
procedure glVertex3dv; external opengl32;
procedure glVertex3f; external opengl32;
procedure glVertex3fv; external opengl32;
procedure glVertex3i; external opengl32;
procedure glVertex3iv; external opengl32;
procedure glVertex3s; external opengl32;
procedure glVertex3sv; external opengl32;
procedure glVertex4d; external opengl32;
procedure glVertex4dv; external opengl32;
procedure glVertex4f; external opengl32;
procedure glVertex4fv; external opengl32;
procedure glVertex4i; external opengl32;
procedure glVertex4iv; external opengl32;
procedure glVertex4s; external opengl32;
procedure glVertex4sv; external opengl32;
procedure glVertex(x,y: GLdouble); external opengl32 name 'glVertex2d';
procedure glVertex2(v: PGLdouble); external opengl32 name 'glVertex2dv';
procedure glVertex(x,y: GLfloat); external opengl32 name 'glVertex2f';
procedure glVertex2(v: PGLfloat); external opengl32 name 'glVertex2fv';
procedure glVertex(x,y: GLint); external opengl32 name 'glVertex2i';
procedure glVertex2(v: PGLint); external opengl32 name 'glVertex2iv';
procedure glVertex(x,y: GLshort); external opengl32 name 'glVertex2s';
procedure glVertex2(v: PGLshort); external opengl32 name 'glVertex2sv';
procedure glVertex(x,y,z: GLdouble); external opengl32 name 'glVertex3d';
procedure glVertex3(v: PGLdouble); external opengl32 name 'glVertex3dv';
procedure glVertex(x,y,z: GLfloat); external opengl32 name 'glVertex3f';
procedure glVertex3(v: PGLfloat); external opengl32 name 'glVertex3fv';
procedure glVertex(x,y,z: GLint); external opengl32 name 'glVertex3i';
procedure glVertex3(v: PGLint); external opengl32 name 'glVertex3iv';
procedure glVertex(x,y,z: GLshort); external opengl32 name 'glVertex3s';
procedure glVertex3(v: PGLshort); external opengl32 name 'glVertex3sv';
procedure glVertex(x,y,z,w: GLdouble); external opengl32 name 'glVertex4d';
procedure glVertex4(v: PGLdouble); external opengl32 name 'glVertex4dv';
procedure glVertex(x,y,z,w: GLfloat); external opengl32 name 'glVertex4f';
procedure glVertex4(v: PGLfloat); external opengl32 name 'glVertex4fv';
procedure glVertex(x,y,z,w: GLint); external opengl32 name 'glVertex4i';
procedure glVertex4(v: PGLint); external opengl32 name 'glVertex4iv';
procedure glVertex(x,y,z,w: GLshort); external opengl32 name 'glVertex4s';
procedure glVertex4(v: PGLshort); external opengl32 name 'glVertex4sv';
procedure glViewport; external opengl32;
procedure glBindTexture; external opengl32;
procedure glGenTextures; external opengl32;
procedure glDeleteTextures; external opengl32;

procedure glVertexPointer; external opengl32 name 'glVertexPointer';
procedure glNormalPointer; external opengl32 name 'glNormalPointer';
procedure glColorPointer; external opengl32 name 'glColorPointer';
procedure glTexCoordPointer; external opengl32 name 'glTexCoordPointer';

function wglGetProcAddress; external opengl32;

function gluErrorString; external glu32;
function gluErrorUnicodeStringEXT; external glu32;
function gluGetString; external glu32;
procedure gluLookAt; external glu32;
procedure gluOrtho2D; external glu32;
procedure gluPerspective; external glu32;
procedure gluPickMatrix; external glu32;
function  gluProject; external glu32;
function  gluUnProject; external glu32;
function  gluScaleImage; external glu32;
function  gluBuild1DMipmaps; external glu32;
function  gluBuild2DMipmaps; external glu32;
function  gluNewQuadric; external glu32;
procedure gluDeleteQuadric; external glu32;
procedure gluQuadricNormals; external glu32;
procedure gluQuadricTexture; external glu32;
procedure gluQuadricOrientation; external glu32;
procedure gluQuadricDrawStyle; external glu32;
procedure gluCylinder; external glu32;
procedure gluDisk; external glu32;
procedure gluPartialDisk; external glu32;
procedure gluSphere; external glu32;
procedure gluQuadricCallback; external glu32;

function gluNewTess; external glu32;
procedure gluDeleteTess; external glu32;
procedure gluTessBeginPolygon; external glu32;
procedure gluTessBeginContour; external glu32;
procedure gluTessVertex; external glu32;
procedure gluTessEndContour; external glu32;
procedure gluTessEndPolygon; external glu32;
procedure gluTessProperty; external glu32;
procedure gluTessNormal; external glu32;
procedure gluTessCallback; external glu32;

function gluNewNurbsRenderer; external glu32;
procedure gluDeleteNurbsRenderer; external glu32;
procedure gluBeginSurface; external glu32;
procedure gluBeginCurve; external glu32;
procedure gluEndCurve; external glu32;
procedure gluEndSurface; external glu32;
procedure gluBeginTrim; external glu32;
procedure gluEndTrim; external glu32;
procedure gluPwlCurve; external glu32;
procedure gluNurbsCurve; external glu32;
procedure gluNurbsSurface; external glu32;
procedure gluLoadSamplingMatrices; external glu32;
procedure gluNurbsProperty; external glu32;
procedure gluGetNurbsProperty; external glu32;
procedure gluNurbsCallback; external glu32;

end.
