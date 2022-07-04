
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareOpenGLExt;

interface

{$I BARE.INC}

uses
  BareOpenGL, Windows, {$IFDEF BARE}BareUtils{$ELSE}SysUtils, Classes,
  StrTools, FileTools{$ENDIF};

type
  GLChar = Char;
  PGLChar = PChar;
  GLhandle = Cardinal;
  PGLhandle = ^GLhandle;

{ OpenGL 1.2 }

var
  glBlendColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); stdcall = nil;
  glBlendEquation: procedure(mode: GLenum); stdcall = nil;
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: Pointer); stdcall = nil;
  glColorTable: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: Pointer); stdcall = nil;
  glColorTableParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); stdcall = nil;
  glColorTableParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); stdcall = nil;
  glCopyColorTable: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); stdcall = nil;
  glGetColorTable: procedure(target: GLenum; format: GLenum; _type: GLenum; table: Pointer); stdcall = nil;
  glGetColorTableParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); stdcall = nil;
  glGetColorTableParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); stdcall = nil;
  glColorSubTable: procedure(target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; _type: GLenum; const data: Pointer); stdcall = nil;
  glCopyColorSubTable: procedure(target: GLenum; start: GLsizei; x: GLint; y: GLint; width: GLsizei); stdcall = nil;
  glConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const image: Pointer); stdcall = nil;
  glConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const image: Pointer); stdcall = nil;
  glConvolutionParameterf: procedure(target: GLenum; pname: GLenum; params: GLfloat); stdcall = nil;
  glConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); stdcall = nil;
  glConvolutionParameteri: procedure(target: GLenum; pname: GLenum; params: GLint); stdcall = nil;
  glConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); stdcall = nil;
  glCopyConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); stdcall = nil;
  glCopyConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); stdcall = nil;
  glGetConvolutionFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; image: Pointer); stdcall = nil;
  glGetConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); stdcall = nil;
  glGetConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); stdcall = nil;
  glGetSeparableFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; row: Pointer; column: Pointer; span: Pointer); stdcall = nil;
  glSeparableFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const row: Pointer; const column: Pointer); stdcall = nil;
  glGetHistogram: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: Pointer); stdcall = nil;
  glGetHistogramParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); stdcall = nil;
  glGetHistogramParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); stdcall = nil;
  glGetMinmax: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: Pointer); stdcall = nil;
  glGetMinmaxParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); stdcall = nil;
  glGetMinmaxParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); stdcall = nil;
  glHistogram: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); stdcall = nil;
  glMinmax: procedure(target: GLenum; internalformat: GLenum; sink: GLboolean); stdcall = nil;
  glResetHistogram: procedure(target: GLenum); stdcall = nil;
  glResetMinmax: procedure(target: GLenum); stdcall = nil;
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: Pointer); stdcall = nil;
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: Pointer); stdcall = nil;
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); stdcall = nil;

const
  GL_UNSIGNED_BYTE_3_3_2                                       = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4                                    = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1                                    = $8034;
  GL_UNSIGNED_INT_8_8_8_8                                      = $8035;
  GL_UNSIGNED_INT_10_10_10_2                                   = $8036;
  GL_RESCALE_NORMAL                                            = $803A;
  GL_UNSIGNED_BYTE_2_3_3_REV                                   = $8362;
  GL_UNSIGNED_SHORT_5_6_5                                      = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV                                  = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV                                = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV                                = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV                                  = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV                               = $8368;
  GL_BGR                                                       = $80E0;
  GL_BGRA                                                      = $80E1;
  GL_MAX_ELEMENTS_VERTICES                                     = $80E8;
  GL_MAX_ELEMENTS_INDICES                                      = $80E9;
  GL_CLAMP_TO_EDGE                                             = $812F;
  GL_TEXTURE_MIN_LOD                                           = $813A;
  GL_TEXTURE_MAX_LOD                                           = $813B;
  GL_TEXTURE_BASE_LEVEL                                        = $813C;
  GL_TEXTURE_MAX_LEVEL                                         = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL                                 = $81F8;
  GL_SINGLE_COLOR                                              = $81F9;
  GL_SEPARATE_SPECULAR_COLOR                                   = $81FA;
  GL_SMOOTH_POINT_SIZE_RANGE                                   = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY                             = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE                                   = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY                             = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE                                  = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE                                  = $846E;
  GL_PACK_SKIP_IMAGES                                          = $806B;
  GL_PACK_IMAGE_HEIGHT                                         = $806C;
  GL_UNPACK_SKIP_IMAGES                                        = $806D;
  GL_UNPACK_IMAGE_HEIGHT                                       = $806E;
  GL_TEXTURE_3D                                                = $806F;
  GL_PROXY_TEXTURE_3D                                          = $8070;
  GL_TEXTURE_DEPTH                                             = $8071;
  GL_TEXTURE_WRAP_R                                            = $8072;
  GL_MAX_3D_TEXTURE_SIZE                                       = $8073;

{ OpenGL 1.3 }

var
  glActiveTexture: procedure(texture: GLenum); stdcall = nil;
  glClientActiveTexture: procedure(texture: GLenum); stdcall = nil;
  glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); stdcall = nil;
  glMultiTexCoord1dv: procedure(target: GLenum; const v: PGLdouble); stdcall = nil;
  glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); stdcall = nil;
  glMultiTexCoord1fv: procedure(target: GLenum; const v: PGLfloat); stdcall = nil;
  glMultiTexCoord1i: procedure(target: GLenum; s: GLint); stdcall = nil;
  glMultiTexCoord1iv: procedure(target: GLenum; const v: PGLint); stdcall = nil;
  glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); stdcall = nil;
  glMultiTexCoord1sv: procedure(target: GLenum; const v: PGLshort); stdcall = nil;
  glMultiTexCoord2d: procedure(target: GLenum; s: GLdouble; t: GLdouble); stdcall = nil;
  glMultiTexCoord2dv: procedure(target: GLenum; const v: PGLdouble); stdcall = nil;
  glMultiTexCoord2f: procedure(target: GLenum; s: GLfloat; t: GLfloat); stdcall = nil;
  glMultiTexCoord2fv: procedure(target: GLenum; const v: PGLfloat); stdcall = nil;
  glMultiTexCoord2i: procedure(target: GLenum; s: GLint; t: GLint); stdcall = nil;
  glMultiTexCoord2iv: procedure(target: GLenum; const v: PGLint); stdcall = nil;
  glMultiTexCoord2s: procedure(target: GLenum; s: GLshort; t: GLshort); stdcall = nil;
  glMultiTexCoord2sv: procedure(target: GLenum; const v: PGLshort); stdcall = nil;
  glMultiTexCoord3d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); stdcall = nil;
  glMultiTexCoord3dv: procedure(target: GLenum; const v: PGLdouble); stdcall = nil;
  glMultiTexCoord3f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); stdcall = nil;
  glMultiTexCoord3fv: procedure(target: GLenum; const v: PGLfloat); stdcall = nil;
  glMultiTexCoord3i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); stdcall = nil;
  glMultiTexCoord3iv: procedure(target: GLenum; const v: PGLint); stdcall = nil;
  glMultiTexCoord3s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); stdcall = nil;
  glMultiTexCoord3sv: procedure(target: GLenum; const v: PGLshort); stdcall = nil;
  glMultiTexCoord4d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); stdcall = nil;
  glMultiTexCoord4dv: procedure(target: GLenum; const v: PGLdouble); stdcall = nil;
  glMultiTexCoord4f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); stdcall = nil;
  glMultiTexCoord4fv: procedure(target: GLenum; const v: PGLfloat); stdcall = nil;
  glMultiTexCoord4i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); stdcall = nil;
  glMultiTexCoord4iv: procedure(target: GLenum; const v: PGLint); stdcall = nil;
  glMultiTexCoord4s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); stdcall = nil;
  glMultiTexCoord4sv: procedure(target: GLenum; const v: PGLshort); stdcall = nil;
  glLoadTransposeMatrixf: procedure(const m: PGLfloat); stdcall = nil;
  glLoadTransposeMatrixd: procedure(const m: PGLdouble); stdcall = nil;
  glMultTransposeMatrixf: procedure(const m: PGLfloat); stdcall = nil;
  glMultTransposeMatrixd: procedure(const m: PGLdouble); stdcall = nil;
  glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); stdcall = nil;
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glCompressedTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: Pointer); stdcall = nil;
  glGetCompressedTexImage: procedure(target: GLenum; level: GLint; img: Pointer); stdcall = nil;

const
  GL_TEXTURE0                                = $84C0;
  GL_TEXTURE1                                = $84C1;
  GL_TEXTURE2                                = $84C2;
  GL_TEXTURE3                                = $84C3;
  GL_TEXTURE4                                = $84C4;
  GL_TEXTURE5                                = $84C5;
  GL_TEXTURE6                                = $84C6;
  GL_TEXTURE7                                = $84C7;
  GL_TEXTURE8                                = $84C8;
  GL_TEXTURE9                                = $84C9;
  GL_TEXTURE10                               = $84CA;
  GL_TEXTURE11                               = $84CB;
  GL_TEXTURE12                               = $84CC;
  GL_TEXTURE13                               = $84CD;
  GL_TEXTURE14                               = $84CE;
  GL_TEXTURE15                               = $84CF;
  GL_TEXTURE16                               = $84D0;
  GL_TEXTURE17                               = $84D1;
  GL_TEXTURE18                               = $84D2;
  GL_TEXTURE19                               = $84D3;
  GL_TEXTURE20                               = $84D4;
  GL_TEXTURE21                               = $84D5;
  GL_TEXTURE22                               = $84D6;
  GL_TEXTURE23                               = $84D7;
  GL_TEXTURE24                               = $84D8;
  GL_TEXTURE25                               = $84D9;
  GL_TEXTURE26                               = $84DA;
  GL_TEXTURE27                               = $84DB;
  GL_TEXTURE28                               = $84DC;
  GL_TEXTURE29                               = $84DD;
  GL_TEXTURE30                               = $84DE;
  GL_TEXTURE31                               = $84DF;
  GL_ACTIVE_TEXTURE                          = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE                   = $84E1;
  GL_MAX_TEXTURE_UNITS                       = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX              = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX             = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX                = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX                  = $84E6;
  GL_MULTISAMPLE                             = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE                = $809E;
  GL_SAMPLE_ALPHA_TO_ONE                     = $809F;
  GL_SAMPLE_COVERAGE                         = $80A0;
  GL_SAMPLE_BUFFERS                          = $80A8;
  GL_SAMPLES                                 = $80A9;
  GL_SAMPLE_COVERAGE_VALUE                   = $80AA;
  GL_SAMPLE_COVERAGE_INVERT                  = $80AB;
  GL_MULTISAMPLE_BIT                         = $20000000;
  GL_NORMAL_MAP                              = $8511;
  GL_REFLECTION_MAP                          = $8512;
  GL_TEXTURE_CUBE_MAP                        = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP                = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X             = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X             = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y             = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y             = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z             = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z             = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP                  = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE               = $851C;
  GL_COMPRESSED_ALPHA                        = $84E9;
  GL_COMPRESSED_LUMINANCE                    = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA              = $84EB;
  GL_COMPRESSED_INTENSITY                    = $84EC;
  GL_COMPRESSED_RGB                          = $84ED;
  GL_COMPRESSED_RGBA                         = $84EE;
  GL_TEXTURE_COMPRESSION_HINT                = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE           = $86A0;
  GL_TEXTURE_COMPRESSED                      = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS          = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS              = $86A3;
  GL_CLAMP_TO_BORDER                         = $812D;
  GL_CLAMP_TO_BORDER_SGIS                    = $812D;
  GL_COMBINE                                 = $8570;
  GL_COMBINE_RGB                             = $8571;
  GL_COMBINE_ALPHA                           = $8572;
  GL_SOURCE0_RGB                             = $8580;
  GL_SOURCE1_RGB                             = $8581;
  GL_SOURCE2_RGB                             = $8582;
  GL_SOURCE0_ALPHA                           = $8588;
  GL_SOURCE1_ALPHA                           = $8589;
  GL_SOURCE2_ALPHA                           = $858A;
  GL_OPERAND0_RGB                            = $8590;
  GL_OPERAND1_RGB                            = $8591;
  GL_OPERAND2_RGB                            = $8592;
  GL_OPERAND0_ALPHA                          = $8598;
  GL_OPERAND1_ALPHA                          = $8599;
  GL_OPERAND2_ALPHA                          = $859A;
  GL_RGB_SCALE                               = $8573;
  GL_ADD_SIGNED                              = $8574;
  GL_INTERPOLATE                             = $8575;
  GL_SUBTRACT                                = $84E7;
  GL_CONSTANT                                = $8576;
  GL_PRIMARY_COLOR                           = $8577;
  GL_PREVIOUS                                = $8578;
  GL_DOT3_RGB                                = $86AE;
  GL_DOT3_RGBA                               = $86AF;

{ GL_SHADER_OBJECTS }

var
  glDeleteObject: procedure(obj: GLhandle); stdcall = nil;
  glGetHandle: function (pname: GLenum): GLhandle; stdcall = nil;
  glDetachObject: procedure(containerObj: GLhandle; attachedObj: GLhandle); stdcall = nil;
  glCreateShaderObject: function(shaderType: GLenum): GLhandle; stdcall = nil;
  glShaderSource: procedure(shaderObj: GLhandle; count: GLsizei; source: PGLChar; length: PGLint); stdcall = nil;
  glCompileShader: procedure(shaderObj: GLhandle); stdcall = nil;
  glCreateProgramObject: function: GLhandle; stdcall = nil;
  glAttachObject: procedure(aprogram: GLhandle; shader: GLhandle); stdcall = nil;
  glLinkProgram: procedure(aprogram: GLhandle); stdcall = nil;
  glUseProgramObject: procedure(aprogram: GLhandle); stdcall = nil;
  glValidateProgram: procedure(aprogram: GLhandle); stdcall = nil;
  glUniform1f: procedure(location: GLint; v0: GLfloat); stdcall = nil;
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); stdcall = nil;
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); stdcall = nil;
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); stdcall = nil;
  glUniform1i: procedure(location: GLint; v0: GLint); stdcall = nil;
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); stdcall = nil;
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); stdcall = nil;
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); stdcall = nil;
  glUniform1fv: procedure(location: GLint; count: GLint; value: PGLfloat); stdcall = nil;
  glUniform2fv: procedure(location: GLint; count: GLint; value: PGLfloat); stdcall = nil;
  glUniform3fv: procedure(location: GLint; count: GLint; value: PGLfloat); stdcall = nil;
  glUniform4fv: procedure(location: GLint; count: GLint; value: PGLfloat); stdcall = nil;
  glUniform1iv: procedure(location: GLint; count: GLint; value: PGLint); stdcall = nil;
  glUniform2iv: procedure(location: GLint; count: GLint; value: PGLint); stdcall = nil;
  glUniform3iv: procedure(location: GLint; count: GLint; value: PGLint); stdcall = nil;
  glUniform4iv: procedure(location: GLint; count: GLint; value: PGLint); stdcall = nil;
  glUniformMatrix2fv: procedure(location: GLint; count: GLint; transpose: boolean; value: PGLfloat); stdcall = nil;
  glUniformMatrix3fv: procedure(location: GLint; count: GLint; transpose: boolean; value: PGLfloat); stdcall = nil;
  glUniformMatrix4fv: procedure(location: GLint; count: GLint; transpose: boolean; value: PGLfloat); stdcall = nil;
  glGetObjectParameterfv: procedure(obj: GLhandle; pname: GLenum; var param: GLfloat); stdcall = nil;
  glGetObjectParameteriv: procedure(obj: GLhandle; pname: GLenum; var param: GLint); stdcall = nil;
  glGetInfoLog: procedure(obj: GLhandle; maxLength: GLint; length: PGLint; infoLog: PGLChar); stdcall = nil;
  glGetAttachedObjects: procedure(containerObj: GLhandle; maxCount: GLint; count: PGLint; obj: PGLhandle); stdcall = nil;
  glGetUniformLocation: function(aprogram: GLhandle; name: PGLChar): GLint; stdcall = nil;
  glGetActiveUniform: procedure(aprogram: GLhandle; index: GLuint; maxLength: GLint; length: PGLint; var size: GLint; var atype: GLenum; name: PGLChar); stdcall = nil;
  glGetUniformfv: procedure(aprogram: GLhandle; location: GLint; params: PGLfloat); stdcall = nil;
  glGetUniformiv: procedure(aprogram: GLhandle; location: GLint; params: PGLint); stdcall = nil;
  glGetShaderSource: procedure(obj: GLhandle; maxLength: GLint; length: PGLint; source: PGLChar); stdcall = nil;

const
  { Accepted by the <pname> argument of glGetHandle: }

  GL_PROGRAM_OBJECT				                   = $8B40;

  { Accepted by the <pname> parameter of glGetObjectParameter: }

  GL_OBJECT_TYPE					                   = $8B4E;
  GL_OBJECT_SUBTYPE				                   = $8B4F;
  GL_OBJECT_DELETE_STATUS			               = $8B80;
  GL_OBJECT_COMPILE_STATUS			             = $8B81;
  GL_OBJECT_LINK_STATUS				               = $8B82;
  GL_OBJECT_VALIDATE_STATUS			             = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH			             = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS			           = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS			             = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH        = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH		         = $8B88;

  { Returned by the <params> parameter of glGetObjectParameter }

  GL_SHADER_OBJECT				                   = $8B48;

  { Returned by the <type> parameter of glGetActiveUniform: }

  GL_FLOAT						                       = $1406;
  GL_FLOAT_VEC2					                     = $8B50;
  GL_FLOAT_VEC3					                     = $8B51;
  GL_FLOAT_VEC4					                     = $8B52;
  GL_INT						                         = $1404;
  GL_INT_VEC2					                       = $8B53;
  GL_INT_VEC3					                       = $8B54;
  GL_INT_VEC4					                       = $8B55;
  GL_BOOL					                           = $8B56;
  GL_BOOL_VEC2					                     = $8B57;
  GL_BOOL_VEC3					                     = $8B58;
  GL_BOOL_VEC4					                     = $8B59;
  GL_FLOAT_MAT2					                     = $8B5A;
  GL_FLOAT_MAT3					                     = $8B5B;
  GL_FLOAT_MAT4					                     = $8B5C;

{ GL_ARB_VERTEX_PROGRAM }

var
  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); stdcall = nil;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); stdcall = nil;
  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); stdcall = nil;
  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); stdcall = nil;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); stdcall = nil;
  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); stdcall = nil;
  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); stdcall = nil;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); stdcall = nil;
  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); stdcall = nil;
  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); stdcall = nil;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); stdcall = nil;
  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); stdcall = nil;
  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); stdcall = nil;
  glVertexAttrib1sv: procedure(index: GLuint; v: PGLshort); stdcall = nil;
  glVertexAttrib1fv: procedure(index: GLuint; v: PGLfloat); stdcall = nil;
  glVertexAttrib1dv: procedure(index: GLuint; v: PGLdouble); stdcall = nil;
  glVertexAttrib2sv: procedure(index: GLuint; v: PGLshort); stdcall = nil;
  glVertexAttrib2fv: procedure(index: GLuint; v: PGLfloat); stdcall = nil;
  glVertexAttrib2dv: procedure(index: GLuint; v: PGLdouble); stdcall = nil;
  glVertexAttrib3sv: procedure(index: GLuint; v: PGLshort); stdcall = nil;
  glVertexAttrib3fv: procedure(index: GLuint; v: PGLfloat); stdcall = nil;
  glVertexAttrib3dv: procedure(index: GLuint; v: PGLdouble); stdcall = nil;
  glVertexAttrib4bv: procedure(index: GLuint; v: PGLbyte); stdcall = nil;
  glVertexAttrib4sv: procedure(index: GLuint; v: PGLshort); stdcall = nil;
  glVertexAttrib4iv: procedure(index: GLuint; v: PGLint); stdcall = nil;
  glVertexAttrib4ubv: procedure(index: GLuint; v: PGLubyte); stdcall = nil;
  glVertexAttrib4usv: procedure(index: GLuint; v: PGLushort); stdcall = nil;
  glVertexAttrib4uiv: procedure(index: GLuint; v: PGLuint); stdcall = nil;
  glVertexAttrib4fv: procedure(index: GLuint; v: PGLfloat); stdcall = nil;
  glVertexAttrib4dv: procedure(index: GLuint; v: PGLdouble); stdcall = nil;
  glVertexAttrib4Nbv: procedure(index: GLuint; v: PGLbyte); stdcall = nil;
  glVertexAttrib4Nsv: procedure(index: GLuint; v: PGLshort); stdcall = nil;
  glVertexAttrib4Niv: procedure(index: GLuint; v: PGLint); stdcall = nil;
  glVertexAttrib4Nubv: procedure(index: GLuint; v: PGLubyte); stdcall = nil;
  glVertexAttrib4Nusv: procedure(index: GLuint; v: PGLushort); stdcall = nil;
  glVertexAttrib4Nuiv: procedure(index: GLuint; v: PGLuint); stdcall = nil;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; atype: GLenum; normalized: GLboolean; stride: GLsizei; data: Pointer); stdcall = nil;
  glEnableVertexAttribArray: procedure(index: GLuint); stdcall = nil;
  glDisableVertexAttribArray: procedure(index: GLuint); stdcall = nil;
  glProgramString: procedure(target: GLenum; format: GLenum; len: GLsizei; astring: Pointer); stdcall = nil;
  glBindProgram: procedure(target: GLenum; aprogram: GLuint); stdcall = nil;
  glDeletePrograms: procedure(n: GLsizei; programs: PGLuint); stdcall = nil;
  glGenPrograms: procedure(n: GLsizei; programs: PGLuint); stdcall = nil;
  glProgramEnvParameter4d: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); stdcall = nil;
  glProgramEnvParameter4dv: procedure(target: GLenum; index: GLuint; params: GLdouble); stdcall = nil;
  glProgramEnvParameter4f: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); stdcall = nil;
  glProgramEnvParameter4fv: procedure(target: GLenum; index: GLuint; params: GLfloat); stdcall = nil;
  glProgramLocalParameter4d: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); stdcall = nil;
  glProgramLocalParameter4dv: procedure(target: GLenum; index: GLuint; params: GLdouble); stdcall = nil;
  glProgramLocalParameter4f: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); stdcall = nil;
  glProgramLocalParameter4fv: procedure(target: GLenum; index: GLuint; params: GLfloat); stdcall = nil;
  glGetProgramEnvParameterdv: procedure(target: GLenum; index: GLuint; params: GLdouble); stdcall = nil;
  glGetProgramEnvParameterfv: procedure(target: GLenum; index: GLuint; params: GLfloat); stdcall = nil;
  glGetProgramLocalParameterdv: procedure(target: GLenum; index: GLuint; params: GLdouble); stdcall = nil;
  glGetProgramLocalParameterfv: procedure(target: GLenum; index: GLuint; params: GLfloat); stdcall = nil;
  glGetProgramiv: procedure(target: GLenum; pname: GLenum; params: GLint); stdcall = nil;
  glGetProgramString: procedure(target: GLenum; pname: GLenum; astring: Pointer); stdcall = nil;
  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: GLdouble); stdcall = nil;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: GLfloat); stdcall = nil;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: GLint); stdcall = nil;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; var data: Pointer); stdcall = nil;
  glIsProgram: function(aprogram: GLuint): GLboolean; stdcall = nil;

  { Accepted by the <cap> parameter of Disable, Enable, and IsEnabled, by the
  <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and GetDoublev,
  and by the <target> parameter of ProgramStringARB, BindProgramARB,
  ProgramEnvParameter4[df][v]ARB, ProgramLocalParameter4[df][v]ARB,
  GetProgramEnvParameter[df]vARB, GetProgramLocalParameter[df]vARB,
  GetProgramivARB, and GetProgramStringARB. }

const
  GL_VERTEX_PROGRAM                          = $8620;

  { Accepted by the <cap> parameter of Disable, Enable, and IsEnabled, and by
  the <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and
  GetDoublev: }

  GL_VERTEX_PROGRAM_POINT_SIZE               = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE                 = $8643;
  GL_COLOR_SUM                               = $8458;

  { Accepted by the <format> parameter of ProgramString: }

  GL_PROGRAM_FORMAT_ASCII                    = $8875;

  { Accepted by the <pname> parameter of GetVertexAttrib[dfi]v: }

  GL_VERTEX_ATTRIB_ARRAY_ENABLED             = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE                = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE              = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE                = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED          = $886A;
  GL_CURRENT_VERTEX_ATTRIB                   = $8626;

  { Accepted by the <pname> parameter of GetVertexAttribPointerv: }

  GL_VERTEX_ATTRIB_ARRAY_POINTER             = $8645;

  { Accepted by the <pname> parameter of GetProgramiv: }

  GL_PROGRAM_LENGTH                          = $8627;
  GL_PROGRAM_FORMAT                          = $8876;
  GL_PROGRAM_BINDING                         = $8677;
  GL_PROGRAM_INSTRUCTIONS                    = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS                = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS             = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS         = $88A3;
  GL_PROGRAM_TEMPORARIES                     = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES                 = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES              = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES          = $88A7;
  GL_PROGRAM_PARAMETERS                      = $88A8;
  GL_MAX_PROGRAM_PARAMETERS                  = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS               = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS           = $88AB;
  GL_PROGRAM_ATTRIBS                         = $88AC;
  GL_MAX_PROGRAM_ATTRIBS                     = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS                  = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS              = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS               = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS           = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS        = $88B2;
  MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB   = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS            = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS              = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS             = $88B6;

  { Accepted by the <pname> parameter of GetProgramString: }

  GL_PROGRAM_STRING                          = $8628;

  { Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
  GetFloatv, and GetDoublev: }

  GL_PROGRAM_ERROR_POSITION                  = $864B;
  GL_CURRENT_MATRIX                          = $8641;
  GL_TRANSPOSE_CURRENT_MATRIX                = $88B7;
  GL_CURRENT_MATRIX_STACK_DEPTH              = $8640;
  GL_MAX_VERTEX_ATTRIBS                      = $8869;
  GL_MAX_PROGRAM_MATRICES                    = $862F;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH          = $862E;

 { Accepted by the <name> parameter of GetString: }

  GL_PROGRAM_ERROR_STRING                    = $8874;

  { Accepted by the <mode> parameter of MatrixMode: }

  GL_MATRIX0                                 = $88C0;
  GL_MATRIX1                                 = $88C1;
  GL_MATRIX2                                 = $88C2;
  GL_MATRIX3                                 = $88C3;
  GL_MATRIX4                                 = $88C4;
  GL_MATRIX5                                 = $88C5;
  GL_MATRIX6                                 = $88C6;
  GL_MATRIX7                                 = $88C7;
  GL_MATRIX8                                 = $88C8;
  GL_MATRIX9                                 = $88C9;
  GL_MATRIX10                                = $88CA;
  GL_MATRIX11                                = $88CB;
  GL_MATRIX12                                = $88CC;
  GL_MATRIX13                                = $88CD;
  GL_MATRIX14                                = $88CE;
  GL_MATRIX15                                = $88CF;
  GL_MATRIX16                                = $88D0;
  GL_MATRIX17                                = $88D1;
  GL_MATRIX18                                = $88D2;
  GL_MATRIX19                                = $88D3;
  GL_MATRIX20                                = $88D4;
  GL_MATRIX21                                = $88D5;
  GL_MATRIX22                                = $88D6;
  GL_MATRIX23                                = $88D7;
  GL_MATRIX24                                = $88D8;
  GL_MATRIX25                                = $88D9;
  GL_MATRIX26                                = $88DA;
  GL_MATRIX27                                = $88DB;
  GL_MATRIX28                                = $88DC;
  GL_MATRIX29                                = $88DD;
  GL_MATRIX30                                = $88DE;
  GL_MATRIX31                                = $88DF;

{ GL_ARB_VERTEX_SHADER }

var
  glBindAttribLocation: procedure(aprogram: GLhandle; index: GLuint; const name: PGLchar); stdcall = nil;
  glGetActiveAttrib: procedure(aprogram: GLhandle; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; atype: PGLenum; name: PGLchar); stdcall = nil;
  glGetAttribLocation: function(aprogram: GLhandle; const name: PGLchar): GLint; stdcall = nil;

const
  GL_VERTEX_SHADER                           = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS           = $8B4A;
  GL_MAX_VARYING_FLOATS                      = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS          = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS        = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES                = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH      = $8B8A;

{ GL_FRAGMENT_SHADER }

  { Accepted by the <shaderType> argument of CreateShaderObject: }

const
	GL_FRAGMENT_SHADER				                 = $8B30;

  { Accepted by the <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and
    GetDoublev: }

	GL_MAX_FRAGMENT_UNIFORM_COMPONENTS		     = $8B49;
	GL_MAX_TEXTURE_COORDS				               = $8871;
	GL_MAX_TEXTURE_IMAGE_UNITS			           = $8872;

{ GL_VERTEX_BUFFER_OBJECT }

var
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); stdcall = nil;
  glDeleteBuffers: procedure(n: GLsizei; buffers: PGLuint); stdcall = nil;
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); stdcall = nil;
  glIsBuffer: function(buffer: GLuint): GLboolean; stdcall = nil;
  glBufferData: procedure(target: GLenum; size: GLint; data: Pointer; usage: GLenum); stdcall = nil;
  glBufferSubData: procedure(target: GLenum; offser: GLint; size: GLint; data: Pointer); stdcall = nil;
  glGetBufferSubData: procedure(target: GLenum; offset: GLint; size: GLint; data: Pointer); stdcall = nil;
  glMapBuffer: function(target: GLenum; access: GLenum): Pointer;
  glUnmapBuffer: function(target: GLenum): GLboolean; stdcall = nil;
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint);
  glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; var params: Pointer);

  { Accepted by the <target> parameters of BindBufferARB, BufferDataARB,
  BufferSubDataARB, MapBufferARB, UnmapBufferARB, GetBufferSubDataARB,
  GetBufferParameterivARB, and GetBufferPointerv: }

const
  GL_ARRAY_BUFFER                            = $8892;
  GL_ELEMENT_ARRAY_BUFFER                    = $8893;

  { Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
  GetFloatv, and GetDoublev: }

  GL_ARRAY_BUFFER_BINDING                    = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING            = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING             = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING             = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING              = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING              = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING      = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING          = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING    = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING     = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING             = $889E;

  { Accepted by the <pname> parameter of GetVertexAttribiv: }

  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING      = $889F;

  { Accepted by the <usage> parameter of BufferData: }

  GL_STREAM_DRAW                             = $88E0;
  GL_STREAM_READ                             = $88E1;
  GL_STREAM_COPY                             = $88E2;
  GL_STATIC_DRAW                             = $88E4;
  GL_STATIC_READ                             = $88E5;
  GL_STATIC_COPY                             = $88E6;
  GL_DYNAMIC_DRAW                            = $88E8;
  GL_DYNAMIC_READ                            = $88E9;
  GL_DYNAMIC_COPY                            = $88EA;

  { Accepted by the <access> parameter of MapBuffer: }

  GL_READ_ONLY                               = $88B8;
  GL_WRITE_ONLY                              = $88B9;
  GL_READ_WRITE                              = $88BA;

  { Accepted by the <pname> parameter of GetBufferParameteriv: }

  GL_BUFFER_SIZE                             = $8764;
  GL_BUFFER_USAGE                            = $8765;
  GL_BUFFER_ACCESS                           = $88BB;
  GL_BUFFER_MAPPED                           = $88BC;

  { Accepted by the <pname> parameter of GetBufferPointerv: }

  GL_BUFFER_MAP_POINTER                      = $88BD;

{ ARB extension names }

var
  GL_ARB_DEPTH_TEXTURE: Boolean = False;
  GL_ARB_FRAGMENT_PROGRAM: Boolean = False;
  GL_ARB_FRAGMENT_SHADER: Boolean = False;
  GL_ARB_MULTISAMPLE: Boolean = False;
  GL_ARB_MULTITEXTURE: Boolean = False;
  GL_ARB_OCCLUSION_QUERY: Boolean = False;
  GL_ARB_POINT_PARAMETERS: Boolean = False;
  GL_ARB_POINT_SPRITE: Boolean = False;
  GL_ARB_SHADER_OBJECTS: Boolean = False;
  GL_ARB_SHADING_LANGUAGE_100: Boolean = False;
  GL_ARB_SHADOW: Boolean = False;
  GL_ARB_SHADOW_AMBIENT: Boolean = False;
  GL_ARB_TEXTURE_BORDER_CLAMP: Boolean = False;
  GL_ARB_TEXTURE_COMPRESSION: Boolean = False;
  GL_ARB_TEXTURE_CUBE_MAP: Boolean = False;
  GL_ARB_TEXTURE_ENV_ADD: Boolean = False;
  GL_ARB_TEXTURE_ENV_COMBINE: Boolean = False;
  GL_ARB_TEXTURE_ENV_CROSSBAR: Boolean = False;
  GL_ARB_TEXTURE_ENV_DOT3: Boolean = False;
  GL_ARB_TEXTURE_MIRRORED_REPEAT: Boolean = False;
  GL_ARB_TRANSPOSE_MATRIX: Boolean = False;
  GL_ARB_VERTEX_BLEND: Boolean = False;
  GL_ARB_VERTEX_BUFFER_OBJECT: Boolean = False;
  GL_ARB_VERTEX_PROGRAM: Boolean = False;
  GL_ARB_VERTEX_SHADER: Boolean = False;
  GL_ARB_WINDOW_POS: Boolean = False;

{ glx loading and querying routines }

procedure glxLoadExtensions;
procedure glxGetExtensions(Strings: TStrings); overload;
function glxGetExtensions: string; overload;
function glxFindExtension(const Name: string): Boolean;

{ glx extensions }

procedure glxAlphaMask(alpha: GLfloat);
procedure glxOrthoOrientation(Angle: GLfloat);
procedure glxBeginOrtho(left, right, bottom, top: GLfloat); overload;
procedure glxBeginOrtho(width, height: GLfloat); overload;
procedure glxEndOrtho;
procedure glxBeginStencil;
procedure glxEndStencil;
procedure glxFlipStencil;
procedure glxClearErrors;
procedure glxColor3f(red, green, blue: GLfloat);
procedure glxColor4f(red, green, blue, alpha: GLfloat);
procedure glxColorMask(red, green, blue: GLfloat);

{ glx routines that cache parameters }

procedure glxDeleteBuffers(n: GLsizei; buffers: PGLuint);
procedure glxGenBuffers(n: GLsizei; buffers: PGLuint);
procedure glxBindBuffer(target: GLenum; buffer: GLuint);
procedure glxVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);
procedure glxNormalPointer(atype: GLenum; stride: GLsizei; data: Pointer);
procedure glxColorPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);
procedure glxTexCoordPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);

{ GL Utility Toolkit routines }

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
procedure UnLoadGlut(Module: HMODULE);

var
  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCube: procedure(size: GLdouble); stdcall;
  glutSolidCube: procedure(size: GLdouble); stdcall;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutWireDodecahedron: procedure; stdcall;
  glutSolidDodecahedron: procedure; stdcall;
  glutWireTeapot: procedure(size: GLdouble); stdcall;
  glutSolidTeapot: procedure(size: GLdouble); stdcall;
  glutWireOctahedron: procedure; stdcall;
  glutSolidOctahedron: procedure; stdcall;
  glutWireTetrahedron: procedure; stdcall;
  glutSolidTetrahedron: procedure; stdcall;
  glutWireIcosahedron: procedure; stdcall;
  glutSolidIcosahedron: procedure; stdcall;

function ReadString(const FileName: string): string;

type
  EGraphicsError = class(Exception)
  public
    ErrorCode: GLuint;
    class procedure Clear;
    class procedure Check;
  end;

implementation

var
  ExtensionStrings: TStrings;

procedure InitializeExtensions;
begin
  if ExtensionStrings = nil then
  begin
    ExtensionStrings := TStringList.Create;
    ExtensionStrings.Text := StringReplace(UpperCase(glGetString(GL_EXTENSIONS)),
      #32, #13#10, [rfReplaceAll, rfIgnoreCase]);
    TStringList(ExtensionStrings).Sorted := True;
  end;
end;

procedure glxLoadExtensions;
begin
  InitializeExtensions;
  { OpenGL 1.2 }
  glBlendColor := wglGetProcAddress('glBlendColor');
  glBlendEquation := wglGetProcAddress('glBlendEquation');
  glDrawRangeElements := wglGetProcAddress('glDrawRangeElements');
  glColorTable := wglGetProcAddress('glColorTable');
  glColorTableParameterfv := wglGetProcAddress('glColorTableParameterfv');
  glColorTableParameteriv := wglGetProcAddress('glColorTableParameteriv');
  glCopyColorTable := wglGetProcAddress('glCopyColorTable');
  glGetColorTable := wglGetProcAddress('glGetColorTable');
  glGetColorTableParameterfv := wglGetProcAddress('glGetColorTableParameterfv');
  glGetColorTableParameteriv := wglGetProcAddress('glGetColorTableParameteriv');
  glColorSubTable := wglGetProcAddress('glColorSubTable');
  glCopyColorSubTable := wglGetProcAddress('glCopyColorSubTable');
  glConvolutionFilter1D := wglGetProcAddress('glConvolutionFilter1D');
  glConvolutionFilter2D := wglGetProcAddress('glConvolutionFilter2D');
  glConvolutionParameterf := wglGetProcAddress('glConvolutionParameterf');
  glConvolutionParameterfv := wglGetProcAddress('glConvolutionParameterfv');
  glConvolutionParameteri := wglGetProcAddress('glConvolutionParameteri');
  glConvolutionParameteriv := wglGetProcAddress('glConvolutionParameteriv');
  glCopyConvolutionFilter1D := wglGetProcAddress('glCopyConvolutionFilter1D');
  glCopyConvolutionFilter2D := wglGetProcAddress('glCopyConvolutionFilter2D');
  glGetConvolutionFilter := wglGetProcAddress('glGetConvolutionFilter');
  glGetConvolutionParameterfv := wglGetProcAddress('glGetConvolutionParameterfv');
  glGetConvolutionParameteriv := wglGetProcAddress('glGetConvolutionParameteriv');
  glGetSeparableFilter := wglGetProcAddress('glGetSeparableFilter');
  glSeparableFilter2D := wglGetProcAddress('glSeparableFilter2D');
  glGetHistogram := wglGetProcAddress('glGetHistogram');
  glGetHistogramParameterfv := wglGetProcAddress('glGetHistogramParameterfv');
  glGetHistogramParameteriv := wglGetProcAddress('glGetHistogramParameteriv');
  glGetMinmax := wglGetProcAddress('glGetMinmax');
  glGetMinmaxParameterfv := wglGetProcAddress('glGetMinmaxParameterfv');
  glGetMinmaxParameteriv := wglGetProcAddress('glGetMinmaxParameteriv');
  glHistogram := wglGetProcAddress('glHistogram');
  glMinmax := wglGetProcAddress('glMinmax');
  glResetHistogram := wglGetProcAddress('glResetHistogram');
  glResetMinmax := wglGetProcAddress('glResetMinmax');
  glTexImage3D := wglGetProcAddress('glTexImage3D');
  glTexSubImage3D := wglGetProcAddress('glTexSubImage3D');
  glCopyTexSubImage3D := wglGetProcAddress('glCopyTexSubImage3D');
  { OpenGL 1.3 }
  glActiveTexture := wglGetProcAddress('glActiveTexture');
  glClientActiveTexture := wglGetProcAddress('glClientActiveTexture');
  glMultiTexCoord1d := wglGetProcAddress('glMultiTexCoord1d');
  glMultiTexCoord1dv := wglGetProcAddress('glMultiTexCoord1dv');
  glMultiTexCoord1f := wglGetProcAddress('glMultiTexCoord1f');
  glMultiTexCoord1fv := wglGetProcAddress('glMultiTexCoord1fv');
  glMultiTexCoord1i := wglGetProcAddress('glMultiTexCoord1i');
  glMultiTexCoord1iv := wglGetProcAddress('glMultiTexCoord1iv');
  glMultiTexCoord1s := wglGetProcAddress('glMultiTexCoord1s');
  glMultiTexCoord1sv := wglGetProcAddress('glMultiTexCoord1sv');
  glMultiTexCoord2d := wglGetProcAddress('glMultiTexCoord2d');
  glMultiTexCoord2dv := wglGetProcAddress('glMultiTexCoord2dv');
  glMultiTexCoord2f := wglGetProcAddress('glMultiTexCoord2f');
  glMultiTexCoord2fv := wglGetProcAddress('glMultiTexCoord2fv');
  glMultiTexCoord2i := wglGetProcAddress('glMultiTexCoord2i');
  glMultiTexCoord2iv := wglGetProcAddress('glMultiTexCoord2iv');
  glMultiTexCoord2s := wglGetProcAddress('glMultiTexCoord2s');
  glMultiTexCoord2sv := wglGetProcAddress('glMultiTexCoord2sv');
  glMultiTexCoord3d := wglGetProcAddress('glMultiTexCoord3d');
  glMultiTexCoord3dv := wglGetProcAddress('glMultiTexCoord3dv');
  glMultiTexCoord3f := wglGetProcAddress('glMultiTexCoord3f');
  glMultiTexCoord3fv := wglGetProcAddress('glMultiTexCoord3fv');
  glMultiTexCoord3i := wglGetProcAddress('glMultiTexCoord3i');
  glMultiTexCoord3iv := wglGetProcAddress('glMultiTexCoord3iv');
  glMultiTexCoord3s := wglGetProcAddress('glMultiTexCoord3s');
  glMultiTexCoord3sv := wglGetProcAddress('glMultiTexCoord3sv');
  glMultiTexCoord4d := wglGetProcAddress('glMultiTexCoord4d');
  glMultiTexCoord4dv := wglGetProcAddress('glMultiTexCoord4dv');
  glMultiTexCoord4f := wglGetProcAddress('glMultiTexCoord4f');
  glMultiTexCoord4fv := wglGetProcAddress('glMultiTexCoord4fv');
  glMultiTexCoord4i := wglGetProcAddress('glMultiTexCoord4i');
  glMultiTexCoord4iv := wglGetProcAddress('glMultiTexCoord4iv');
  glMultiTexCoord4s := wglGetProcAddress('glMultiTexCoord4s');
  glMultiTexCoord4sv := wglGetProcAddress('glMultiTexCoord4sv');
  glLoadTransposeMatrixf := wglGetProcAddress('glLoadTransposeMatrixf');
  glLoadTransposeMatrixd := wglGetProcAddress('glLoadTransposeMatrixd');
  glMultTransposeMatrixf := wglGetProcAddress('glMultTransposeMatrixf');
  glMultTransposeMatrixd := wglGetProcAddress('glMultTransposeMatrixd');
  glSampleCoverage := wglGetProcAddress('glSampleCoverage');
  glCompressedTexImage3D := wglGetProcAddress('glCompressedTexImage3D');
  glCompressedTexImage2D := wglGetProcAddress('glCompressedTexImage2D');
  glCompressedTexImage1D := wglGetProcAddress('glCompressedTexImage1D');
  glCompressedTexSubImage3D := wglGetProcAddress('glCompressedTexSubImage3D');
  glCompressedTexSubImage2D := wglGetProcAddress('glCompressedTexSubImage2D');
  glCompressedTexSubImage1D := wglGetProcAddress('glCompressedTexSubImage1D');
  glGetCompressedTexImage := wglGetProcAddress('glGetCompressedTexImage');
  { New extensions}
  GL_ARB_DEPTH_TEXTURE := ExtensionStrings.IndexOf('GL_ARB_DEPTH_TEXTURE') > -1;
  GL_ARB_FRAGMENT_PROGRAM := ExtensionStrings.IndexOf('GL_ARB_FRAGMENT_PROGRAM') > -1;
  GL_ARB_FRAGMENT_SHADER := ExtensionStrings.IndexOf('GL_ARB_FRAGMENT_SHADER') > -1;
  GL_ARB_MULTISAMPLE := ExtensionStrings.IndexOf('GL_ARB_MULTISAMPLE') > -1;
  GL_ARB_MULTITEXTURE := ExtensionStrings.IndexOf('GL_ARB_MULTITEXTURE') > -1;
  GL_ARB_OCCLUSION_QUERY := ExtensionStrings.IndexOf('GL_ARB_OCCLUSION_QUERY') > -1;
  GL_ARB_POINT_PARAMETERS := ExtensionStrings.IndexOf('GL_ARB_POINT_PARAMETERS') > -1;
  GL_ARB_POINT_SPRITE := ExtensionStrings.IndexOf('GL_ARB_POINT_SPRITE') > -1;
  GL_ARB_SHADER_OBJECTS := ExtensionStrings.IndexOf('GL_ARB_SHADER_OBJECTS') > -1;
  GL_ARB_SHADING_LANGUAGE_100 := ExtensionStrings.IndexOf('GL_ARB_SHADING_LANGUAGE_100') > -1;
  GL_ARB_SHADOW := ExtensionStrings.IndexOf('GL_ARB_SHADOW') > -1;
  GL_ARB_SHADOW_AMBIENT := ExtensionStrings.IndexOf('GL_ARB_SHADOW_AMBIENT') > -1;
  GL_ARB_TEXTURE_BORDER_CLAMP := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_BORDER_CLAMP') > -1;
  GL_ARB_TEXTURE_COMPRESSION := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_COMPRESSION') > -1;
  GL_ARB_TEXTURE_CUBE_MAP := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_CUBE_MAP') > -1;
  GL_ARB_TEXTURE_ENV_ADD := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_ENV_ADD') > -1;
  GL_ARB_TEXTURE_ENV_COMBINE := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_ENV_COMBINE') > -1;
  GL_ARB_TEXTURE_ENV_CROSSBAR := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_ENV_CROSSBAR') > -1;
  GL_ARB_TEXTURE_ENV_DOT3 := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_ENV_DOT3') > -1;
  GL_ARB_TEXTURE_MIRRORED_REPEAT := ExtensionStrings.IndexOf('GL_ARB_TEXTURE_MIRRORED_REPEAT') > -1;
  GL_ARB_TRANSPOSE_MATRIX := ExtensionStrings.IndexOf('GL_ARB_TRANSPOSE_MATRIX') > -1;
  GL_ARB_VERTEX_BLEND := ExtensionStrings.IndexOf('GL_ARB_VERTEX_BLEND') > -1;
  GL_ARB_VERTEX_BUFFER_OBJECT := ExtensionStrings.IndexOf('GL_ARB_VERTEX_BUFFER_OBJECT') > -1;
  GL_ARB_VERTEX_PROGRAM := ExtensionStrings.IndexOf('GL_ARB_VERTEX_PROGRAM') > -1;
  GL_ARB_VERTEX_SHADER := ExtensionStrings.IndexOf('GL_ARB_VERTEX_SHADER') > -1;
  GL_ARB_WINDOW_POS := ExtensionStrings.IndexOf('GL_ARB_WINDOW_POS') > -1;
  if GL_ARB_SHADER_OBJECTS then
  begin
    glDeleteObject := wglGetProcAddress('glDeleteObjectARB');
    glGetHandle := wglGetProcAddress('glGetHandleARB');
    glDetachObject := wglGetProcAddress('glDetachObjectARB');
    glCreateShaderObject := wglGetProcAddress('glCreateShaderObjectARB');
    glShaderSource := wglGetProcAddress('glShaderSourceARB');
    glCompileShader := wglGetProcAddress('glCompileShaderARB');
    glCreateProgramObject := wglGetProcAddress('glCreateProgramObjectARB');
    glAttachObject := wglGetProcAddress('glAttachObjectARB');
    glLinkProgram := wglGetProcAddress('glLinkProgramARB');
    glUseProgramObject := wglGetProcAddress('glUseProgramObjectARB');
    glValidateProgram := wglGetProcAddress('glValidateProgramARB');
    glUniform1f := wglGetProcAddress('glUniform1fARB');
    glUniform2f := wglGetProcAddress('glUniform2fARB');
    glUniform3f := wglGetProcAddress('glUniform3fARB');
    glUniform4f := wglGetProcAddress('glUniform4fARB');
    glUniform1i := wglGetProcAddress('glUniform1iARB');
    glUniform2i := wglGetProcAddress('glUniform2iARB');
    glUniform3i := wglGetProcAddress('glUniform3iARB');
    glUniform4i := wglGetProcAddress('glUniform4iARB');
    glUniform1fv := wglGetProcAddress('glUniform1fvARB');
    glUniform2fv := wglGetProcAddress('glUniform2fvARB');
    glUniform3fv := wglGetProcAddress('glUniform3fvARB');
    glUniform4fv := wglGetProcAddress('glUniform4fvARB');
    glUniform1iv := wglGetProcAddress('glUniform1ivARB');
    glUniform2iv := wglGetProcAddress('glUniform2ivARB');
    glUniform3iv := wglGetProcAddress('glUniform3ivARB');
    glUniform4iv := wglGetProcAddress('glUniform4ivARB');
    glUniformMatrix2fv := wglGetProcAddress('glUniformMatrix2fvARB');
    glUniformMatrix3fv := wglGetProcAddress('glUniformMatrix3fvARB');
    glUniformMatrix4fv := wglGetProcAddress('glUniformMatrix4fvARB');
    glGetObjectParameterfv := wglGetProcAddress('glGetObjectParameterfvARB');
    glGetObjectParameteriv := wglGetProcAddress('glGetObjectParameterivARB');
    glGetInfoLog := wglGetProcAddress('glGetInfoLogARB');
    glGetAttachedObjects := wglGetProcAddress('glGetAttachedObjectsARB');
    glGetUniformLocation := wglGetProcAddress('glGetUniformLocationARB');
    glGetActiveUniform := wglGetProcAddress('glGetActiveUniformARB');
    glGetUniformfv := wglGetProcAddress('glGetUniformfvARB');
    glGetUniformiv := wglGetProcAddress('glGetUniformivARB');
    glGetShaderSource := wglGetProcAddress('glGetShaderSourceARB');
  end;
  if GL_ARB_VERTEX_PROGRAM then
  begin
    glVertexAttrib1s := wglGetProcAddress('glVertexAttrib1sARB');
    glVertexAttrib1f := wglGetProcAddress('glVertexAttrib1fARB');
    glVertexAttrib1d := wglGetProcAddress('glVertexAttrib1dARB');
    glVertexAttrib2s := wglGetProcAddress('glVertexAttrib2sARB');
    glVertexAttrib2f := wglGetProcAddress('glVertexAttrib2fARB');
    glVertexAttrib2d := wglGetProcAddress('glVertexAttrib2dARB');
    glVertexAttrib3s := wglGetProcAddress('glVertexAttrib3sARB');
    glVertexAttrib3f := wglGetProcAddress('glVertexAttrib3fARB');
    glVertexAttrib3d := wglGetProcAddress('glVertexAttrib3dARB');
    glVertexAttrib4s := wglGetProcAddress('glVertexAttrib4sARB');
    glVertexAttrib4f := wglGetProcAddress('glVertexAttrib4fARB');
    glVertexAttrib4d := wglGetProcAddress('glVertexAttrib4dARB');
    glVertexAttrib4Nub := wglGetProcAddress('glVertexAttrib4NubARB');
    glVertexAttrib1sv := wglGetProcAddress('glVertexAttrib1svARB');
    glVertexAttrib1fv := wglGetProcAddress('glVertexAttrib1fvARB');
    glVertexAttrib1dv := wglGetProcAddress('glVertexAttrib1dvARB');
    glVertexAttrib2sv := wglGetProcAddress('glVertexAttrib2svARB');
    glVertexAttrib2fv := wglGetProcAddress('glVertexAttrib2fvARB');
    glVertexAttrib2dv := wglGetProcAddress('glVertexAttrib2dvARB');
    glVertexAttrib3sv := wglGetProcAddress('glVertexAttrib3svARB');
    glVertexAttrib3fv := wglGetProcAddress('glVertexAttrib3fvARB');
    glVertexAttrib3dv := wglGetProcAddress('glVertexAttrib3dvARB');
    glVertexAttrib4bv := wglGetProcAddress('glVertexAttrib4bvARB');
    glVertexAttrib4sv := wglGetProcAddress('glVertexAttrib4svARB');
    glVertexAttrib4iv := wglGetProcAddress('glVertexAttrib4ivARB');
    glVertexAttrib4ubv := wglGetProcAddress('glVertexAttrib4ubvARB');
    glVertexAttrib4usv := wglGetProcAddress('glVertexAttrib4usvARB');
    glVertexAttrib4uiv := wglGetProcAddress('glVertexAttrib4uivARB');
    glVertexAttrib4fv := wglGetProcAddress('glVertexAttrib4fvARB');
    glVertexAttrib4dv := wglGetProcAddress('glVertexAttrib4dvARB');
    glVertexAttrib4Nbv := wglGetProcAddress('glVertexAttrib4NbvARB');
    glVertexAttrib4Nsv := wglGetProcAddress('glVertexAttrib4NsvARB');
    glVertexAttrib4Niv := wglGetProcAddress('glVertexAttrib4NivARB');
    glVertexAttrib4Nubv := wglGetProcAddress('glVertexAttrib4NubvARB');
    glVertexAttrib4Nusv := wglGetProcAddress('glVertexAttrib4NusvARB');
    glVertexAttrib4Nuiv := wglGetProcAddress('glVertexAttrib4NuivARB');
    glVertexAttribPointer := wglGetProcAddress('glVertexAttribPointerARB');
    glEnableVertexAttribArray := wglGetProcAddress('glEnableVertexAttribArrayARB');
    glDisableVertexAttribArray := wglGetProcAddress('glDisableVertexAttribArrayARB');
    glProgramString := wglGetProcAddress('glProgramStringARB');
    glBindProgram := wglGetProcAddress('glBindProgramARB');
    glDeletePrograms := wglGetProcAddress('glDeleteProgramsARB');
    glGenPrograms := wglGetProcAddress('glGenProgramsARB');
    glProgramEnvParameter4d := wglGetProcAddress('glProgramEnvParameter4dARB');
    glProgramEnvParameter4dv := wglGetProcAddress('glProgramEnvParameter4dvARB');
    glProgramEnvParameter4f := wglGetProcAddress('glProgramEnvParameter4fARB');
    glProgramEnvParameter4fv := wglGetProcAddress('glProgramEnvParameter4fvARB');
    glProgramLocalParameter4d := wglGetProcAddress('glProgramLocalParameter4dARB');
    glProgramLocalParameter4dv := wglGetProcAddress('glProgramLocalParameter4dvARB');
    glProgramLocalParameter4f := wglGetProcAddress('glProgramLocalParameter4fARB');
    glProgramLocalParameter4fv := wglGetProcAddress('glProgramLocalParameter4fvARB');
    glGetProgramEnvParameterdv := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    glGetProgramEnvParameterfv := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    glGetProgramLocalParameterdv := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    glGetProgramLocalParameterfv := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    glGetProgramiv := wglGetProcAddress('glGetProgramivARB');
    glGetProgramString := wglGetProcAddress('glGetProgramStringARB');
    glGetVertexAttribdv := wglGetProcAddress('glGetVertexAttribdvARB');
    glGetVertexAttribfv := wglGetProcAddress('glGetVertexAttribfvARB');
    glGetVertexAttribiv := wglGetProcAddress('glGetVertexAttribivARB');
    glGetVertexAttribPointerv := wglGetProcAddress('glGetVertexAttribPointervARB');
    glIsProgram := wglGetProcAddress('glIsProgramARB');
  end;
  if GL_ARB_VERTEX_SHADER then
  begin
    glBindAttribLocation := wglGetProcAddress('glBindAttribLocationARB');
    glGetActiveAttrib := wglGetProcAddress('glGetActiveAttribARB');
    glGetAttribLocation := wglGetProcAddress('glGetAttribLocationARB');
  end;
  if GL_ARB_VERTEX_BUFFER_OBJECT then
  begin
    glBindBuffer := wglGetProcAddress('glBindBufferARB');
    glDeleteBuffers := wglGetProcAddress('glDeleteBuffersARB');
    glGenBuffers := wglGetProcAddress('glGenBuffersARB');
    glIsBuffer := wglGetProcAddress('glIsBufferARB');
    glBufferData := wglGetProcAddress('glBufferDataARB');
    glBufferSubData := wglGetProcAddress('glBufferSubDataARB');
    glGetBufferSubData := wglGetProcAddress('glGetBufferSubDataARB');
    glMapBuffer := wglGetProcAddress('glMapBufferARB');
    glUnmapBuffer := wglGetProcAddress('glUnmapBufferARB');
    glBufferData := wglGetProcAddress('glBufferDataARB');
  end;
end;

procedure glxGetExtensions(Strings: TStrings);
begin
  InitializeExtensions;
  Strings.Assign(ExtensionStrings);
end;

function glxGetExtensions: string;
begin
  InitializeExtensions;
  Result := ExtensionStrings.Text;
end;

function glxFindExtension(const Name: string): Boolean;
begin
  InitializeExtensions;
  Result := ExtensionStrings.IndexOf(Name) > -1;
end;

{ glx extension state variables }

var
  glvRedMask: GLfloat;
  glvGreenMask: GLfloat;
  glvBlueMask: GLfloat;
  glvAlphaMask: GLfloat;
  glvOrthoRef: GLuint;
  glvOrthoOrientation: GLfloat;
  glvOrthoMode: GLuint;
  glvOrthoDepth: GLboolean;

procedure glxClearErrors;
begin
  Set8087CW($133F);
end;

procedure glxColorMask(red, green, blue: GLfloat);
begin
  glvRedMask := red;
  glvGreenMask := green;
  glvBlueMask := blue;
end;

procedure glxAlphaMask(alpha: GLfloat);
begin
  glvAlphaMask := alpha;
  if glvAlphaMask < 0 then glvAlphaMask := 0
  else if glvAlphaMask > 1 then glvAlphaMask := 1;
end;

procedure glxColor3f(red, green, blue: GLfloat);
begin
  glColor4f(red * glvRedMask, green * glvGreenMask, blue * glvBlueMask,
    glvAlphaMask);
end;

procedure glxColor4f(red, green, blue, alpha: GLfloat);
begin
  glColor4f(red * glvRedMask, green * glvGreenMask, blue * glvBlueMask,
    alpha * glvAlphaMask);
end;

procedure glxOrthoOrientation(angle: GLfloat);

  function Remainder(const Quotient, Divisor: Single): Single;
  begin
    if Divisor = 0 then
      Result := 0
    else
      Result := Quotient - (Trunc(Quotient) div Trunc(Divisor)) * Divisor;
  end;

begin
  glvOrthoOrientation := Remainder(Angle, 360);
end;

procedure glxBeginOrtho(left, right, bottom, top: GLfloat);
begin
  Inc(glvOrthoRef);
  if glvOrthoRef > 1 then Exit;
  glGetIntegerv(GL_MATRIX_MODE, @glvOrthoMode);
  if glvOrthoMode = GL_MODELVIEW then
  begin
    glPushMatrix;
    glLoadIdentity;
  end;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glGetBooleanv(GL_DEPTH_TEST, @glvOrthoDepth);
  if glvOrthoDepth then
    glDisable(GL_DEPTH_TEST);
  if glvOrthoOrientation <> 0 then
    glRotate(glvOrthoOrientation, 0, 0, 1);
  gluOrtho2d(Left, Right, Bottom, Top);
  glMatrixMode(GL_MODELVIEW);
end;

procedure glxBeginOrtho(width, height: GLfloat);
begin
  glxBeginOrtho(0, width, height, 0);
end;

procedure glxEndOrtho;
begin
  Dec(glvOrthoRef);
  if glvOrthoRef > 0 then Exit;
  glMatrixMode(GL_PROJECTION);
  if glvOrthoDepth then
    glEnable(GL_DEPTH_TEST);
  glPopMatrix;
  glMatrixMode(glvOrthoMode);
  if glvOrthoMode = GL_MODELVIEW then
    glPopMatrix;
end;

procedure glxBeginStencil;
begin
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
end;

procedure glxEndStencil;
begin
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDepthMask(GL_TRUE);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glStencilFunc(GL_EQUAL, 1, 1);
end;

procedure glxFlipStencil;
var
  Func: GLenum;
begin
  glGetIntegerv(GL_STENCIL_FUNC, @Func);
  if Func = GL_EQUAL then
    glStencilFunc(GL_NOTEQUAL, 1, 1)
  else
    glStencilFunc(GL_EQUAL, 1, 1);
end;

procedure glxDeleteBuffers(n: GLsizei; buffers: PGLuint);
begin
  if GL_ARB_VERTEX_BUFFER_OBJECT then
    glDeleteBuffers(n, buffers);
end;

procedure glxGenBuffers(n: GLsizei; buffers: PGLuint);
begin
  if GL_ARB_VERTEX_BUFFER_OBJECT then
    glGenBuffers(n, buffers);
end;

procedure glxBindBuffer(target: GLenum; buffer: GLuint);
const
  ArrayBuffer: GLuint = 0;
  IndexBuffer: GLuint = 0;
begin
  if GL_ARB_VERTEX_BUFFER_OBJECT then
    if (target = GL_ARRAY_BUFFER) and (buffer <> ArrayBuffer) then
    begin
      glBindBuffer(target, buffer);
      ArrayBuffer := buffer;
    end
    else if (target = GL_ELEMENT_ARRAY_BUFFER) and (buffer <> IndexBuffer) then
    begin
      glBindBuffer(target, buffer);
      IndexBuffer := buffer;
    end
end;

procedure glxVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);
const
  PriorSize: GLint = 0;
  PriorAtype: GLenum = 0;
  PriorStride: GLsizei = 0;
  PriorData: Pointer = nil;
begin
  if (PriorSize <> size) or (PriorAtype <> atype) or (PriorStride <> stride) or
    (PriorData<> data) then
  begin
    glVertexPointer(size, atype, stride, data);
    PriorSize := size;
    PriorAtype := atype;
    PriorStride := stride;
    PriorData := data;
  end;
end;

procedure glxNormalPointer(atype: GLenum; stride: GLsizei; data: Pointer);
const
  PriorAtype: GLenum = 0;
  PriorStride: GLsizei = 0;
  PriorData: Pointer = nil;
begin
  if (PriorAtype <> atype) or (PriorStride <> stride) or (PriorData<> data) then
  begin
    glNormalPointer(atype, stride, data);
    PriorAtype := atype;
    PriorStride := stride;
    PriorData := data;
  end;
end;

procedure glxColorPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);
const
  PriorSize: GLint = 0;
  PriorAtype: GLenum = 0;
  PriorStride: GLsizei = 0;
  PriorData: Pointer = nil;
begin
  if (PriorSize <> size) or (PriorAtype <> atype) or (PriorStride <> stride) or
    (PriorData<> data) then
  begin
    glColorPointer(size, atype, stride, data);
    PriorSize := size;
    PriorAtype := atype;
    PriorStride := stride;
    PriorData := data;
  end;
end;

procedure glxTexCoordPointer(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer);
const
  PriorSize: GLint = 0;
  PriorAtype: GLenum = 0;
  PriorStride: GLsizei = 0;
  PriorData: Pointer = nil;
begin
  if (PriorSize <> size) or (PriorAtype <> atype) or (PriorStride <> stride) or
    (PriorData<> data) then
  begin
    glTexCoordPointer(size, atype, stride, data);
    PriorSize := size;
    PriorAtype := atype;
    PriorStride := stride;
    PriorData := data;
  end;
end;
{ Glut support }

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
begin
  Result := LoadLibrary(PChar(ModuleName));
  if Result <> 0 then
  begin
    @glutWireSphere := GetProcAddress(Result, 'glutWireSphere');
    @glutSolidSphere := GetProcAddress(Result, 'glutSolidSphere');
    @glutWireCone := GetProcAddress(Result, 'glutWireCone');
    @glutSolidCone := GetProcAddress(Result, 'glutSolidCone');
    @glutWireCube := GetProcAddress(Result, 'glutWireCube');
    @glutSolidCube := GetProcAddress(Result, 'glutSolidCube');
    @glutWireTorus := GetProcAddress(Result, 'glutWireTorus');
    @glutSolidTorus := GetProcAddress(Result, 'glutSolidTorus');
    @glutWireDodecahedron := GetProcAddress(Result, 'glutWireDodecahedron');
    @glutSolidDodecahedron := GetProcAddress(Result, 'glutSolidDodecahedron');
    @glutWireTeapot := GetProcAddress(Result, 'glutWireTeapot');
    @glutSolidTeapot := GetProcAddress(Result, 'glutSolidTeapot');
    @glutWireOctahedron := GetProcAddress(Result, 'glutWireOctahedron');
    @glutSolidOctahedron := GetProcAddress(Result, 'glutSolidOctahedron');
    @glutWireTetrahedron := GetProcAddress(Result, 'glutWireTetrahedron');
    @glutSolidTetrahedron := GetProcAddress(Result, 'glutSolidTetrahedron');
    @glutWireIcosahedron := GetProcAddress(Result, 'glutWireIcosahedron');
    @glutSolidIcosahedron := GetProcAddress(Result, 'glutSolidIcosahedron');
  end;
end;

procedure UnLoadGlut(Module: HMODULE);
begin
  @glutWireSphere := nil;
  @glutSolidSphere := nil;
  @glutWireCone := nil;
  @glutSolidCone := nil;
  @glutWireCube := nil;
  @glutSolidCube := nil;
  @glutWireTorus := nil;
  @glutSolidTorus := nil;
  @glutWireDodecahedron := nil;
  @glutSolidDodecahedron := nil;
  @glutWireTeapot := nil;
  @glutSolidTeapot := nil;
  @glutWireOctahedron := nil;
  @glutSolidOctahedron := nil;
  @glutWireTetrahedron := nil;
  @glutSolidTetrahedron := nil;
  @glutWireIcosahedron := nil;
  @glutSolidIcosahedron := nil;
  if Module <> 0 then
    FreeLibrary(Module);
end;

function ReadString(const FileName: string): string;
var
  F: THandle;
  Bytes: Cardinal;
begin
  Result := '';
  F := CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    Bytes := GetFileSize(F, nil);
    if Bytes > 0 then
    begin
      SetLength(Result, Bytes);
      ReadFile(F, PChar(Result)^, Bytes, Bytes, nil);
    end;
  finally
    CloseHandle(F);
  end;
end;

class procedure EGraphicsError.Clear;
begin
  glGetError;
end;

class procedure EGraphicsError.Check;
var
  Code: GLuint;
  E: EGraphicsError;
begin
  Code := glGetError;
  if Code <> GL_NO_ERROR then
  begin
    E := EGraphicsError.Create('');
    E.ErrorCode := Code;
    raise E;
  end;
end;

initialization
  ExtensionStrings := nil;
  glvRedMask := 1;
  glvGreenMask := 1;
  glvBlueMask := 1;
  glvAlphaMask := 1;
finalization
  ExtensionStrings.Free;
end.





