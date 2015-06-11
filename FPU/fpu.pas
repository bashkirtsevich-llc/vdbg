unit fpu;

interface

{$define USE_WITH_CPU_SIM}
{$define PECULIAR_486}

uses
  math, Config, Gui32, cpu, Service, SysUtils;

type
  pu8     = ^u8;
  u8      = Bit8u;
  s8      = Bit8s;
  pu16    = ^u16;
  u16     = Bit16u;
  ps16    = ^s16;
  s16     = Bit32s;
  pu32    = ^u32;
  u32     = Bit32u;
  ps32    = ^s32;
  s32     = Bit32s;
  pu64    = ^u64;
  u64     = Bit64u;
  ps64    = ^s64;
  s64     = Bit64s;
  pu_char = ^u_char;
  u_char  = byte;
  u_short = byte;
  u_int   = word;
  u_long  = longword;
  intg    = integer;
  long    = longint;

  pfloat = ^float;
  float  = single;

  bx_ptr_equiv_t = Bit32u;

  pinfo = ^info;

  info = record
    donotindexme: byte;
  end;

  overrides = record
    address_size, operand_size, segment: u_char;
  end;

  fpu_addr_modes = record
    override_:    overrides;
    default_mode: u_char;
  end;

  paddress = ^address;

  address = record
    offset:   u32;
{$ifdef EMU_BIG_ENDIAN}
  empty:u32;
  opcode:u32;
  selector:u32;
{$else}
    selector: u32;
    opcode:   u32;
    empty:    u32;
{$endif}
  end;

  Pfpu__reg = ^fpu__reg;

  fpu__reg = record
{$ifdef EMU_BIG_ENDIAN}
  sigh:u32;
  sigl:u32;
  exp:s16;   { Signed quantity used in internal arithmetic. }
{$else}
    sigl: u32;
    sigh: u32;
    exp:  s16;   { Signed quantity used in internal arithmetic. }
{$endif}
  end;
  PFPU_REG = ^FPU_REG;
  FPU_REG  = fpu__reg;

  parray64    = ^array64;
  array64     = array[0..16] of u64;
  parray64_10 = ^array64_10;
  array64_10  = array[0..10] of u64;
  array_64_1  = array[0..1] of u64;

  parr64 = ^arr64;
  arr64  = array of u64;

  i387_t = record
    soft: record
      cwd:       s32;
      swd:       s32;
      twd:       s32;
      fip:       s32;
      fcs:       s32;
      foo:       s32;
      fos:       s32;
      fill0:     u32;        // to make sure the following aligns on an 8byte boundary
      st_space:  array64;    // 8*16 bytes per FP-reg (aligned) = 128 bytes
      ftop:      byte;
      no_update: byte;
      rm:        byte;
      alimit:    byte;
    end;
  end;

  PXsig = ^Xsig;

  Xsig = record

    lsw:  u32;
    midw: u32;
    msw:  u32;
  end;

  FUNC     = procedure;
  FUNC_ST0 = procedure (st0_ptr: PFPU_REG; st0tag: u_char);

  TException_Names = record
    type_: Intg;
    Name:  string;
  end;

const
  _NONE_  = 0;   { Take no special action }
  _REG0_  = 1;   { Need to check for not empty st(0) }
  _REGI_  = 2;   { Need to check for not empty st(0) and st(rm) }
  _REGi2_ = 0;   { Uses st(rm) }
  _PUSH_  = 3;   { Need to check for space to push onto stack }
  _null_  = 4;   { Function illegal or not implemented }
  _REGIi  = 5;   { Uses st(0) and st(rm), result to st(rm) }
  _REGIp  = 6;   { Uses st(0) and st(rm), result to st(rm) then pop }
  _REGIc  = 0;   { Compare st(0) and st(rm) }
  _REGIn  = 0;   { Uses st(0) and st(rm), but handle checks later }

  type_table_2: array[0..63] of u_char = (
    _REGI_, _NONE_, _null_, _null_, _REGIi, _REGi_, _REGIp, _REGi_,
    _REGI_, _REGIn, _null_, _null_, _REGIi, _REGI_, _REGIp, _REGI_,
    _REGIc, _NONE_, _null_, _null_, _REGIc, _REG0_, _REGIc, _REG0_,
    _REGIc, _REG0_, _null_, _null_, _REGIc, _REG0_, _REGIc, _REG0_,
    _REGI_, _NONE_, _null_, _NONE_, _REGIi, _REGIc, _REGIp, _NONE_,
    _REGI_, _NONE_, _REGIc, _null_, _REGIi, _REGIc, _REGIp, _null_,
    _REGI_, _NONE_, _null_, _null_, _REGIi, _null_, _REGIp, _null_,
    _REGI_, _NONE_, _null_, _null_, _REGIi, _null_, _REGIp, _null_
    );

  SW_Backward    	= $8000;	{ backward compatibility }
  SW_C3		        = $4000;	{ condition bit 3 }
  SW_Top	      	= $3800;	{ top of stack }
  SW_Top_Shift  	= 11   ;	{ shift for top of stack bits }
  SW_C2		        = $0400;	{ condition bit 2 }
  SW_C1	        	= $0200;	{ condition bit 1 }
  SW_C0	        	= $0100;	{ condition bit 0 }
  SW_Summary     	= $0080;	{ exception summary }
  SW_Stack_Fault	= $0040;	{ stack fault }
  SW_Precision   	= $0020;	{ loss of precision }
  SW_Underflow   	= $0010;	{ underflow }
  SW_Overflow    	= $0008;	{ overflow }
  SW_Zero_Div    	= $0004;	{ divide by zero }
  SW_Denorm_Op   	= $0002;	{ denormalized operand }
  SW_Invalid     	= $0001;	{ invalid operation }

  EXP_BIAS	=0;
  EXP_OVER	=$4000;    { smallest invalid large exponent }
  EXP_UNDER	=-$3fff;   { largest invalid small exponent }
  EXP_WAY_UNDER   =-$6000;   { Below the smallest denormal, but still a 16 bit nr. }
  EXP_Infinity    = EXP_OVER;
  EXP_NaN         = EXP_OVER;

  EXTENDED_Ebias =$3fff;
  EXTENDED_Emin =-$3ffe;  { smallest valid exponent }

  SIGN_POS	=0;
  SIGN_NEG	=$80;

  SIGN_Positive	=0;
  SIGN_Negative	=$8000;

{ Keep the order TAG_Valid, TAG_Zero, TW_Denormal }
{ The following fold to 2 (Special; in the Tag Word }
  TW_Denormal     =4;        { De-normal }
  TW_Infinity	=5;	{ + or - infinity }
  TW_NaN		=6;	{ Not a Number }
  TW_Unsupported	=7;	{ Not supported by an 80486 }

  TAG_Valid	=0;	{ valid }
  TAG_Zero	=1;	{ zero }
  TAG_Special	=2;	{ De-normal, + or - infinity,
  		   or Not a Number }
  TAG_Empty	=3;	{ empty }

  LOADED_DATA	=10101;	{ Special st(; number to identify
					   loaded data (not on stack;. }

{ A few flags (must be >= $10;. }
  REV            = $10;
  DEST_RM        = $20;
  LOADED         = $40;

  FPU_Exception_c   =$8000;   { Added to tag returns. }

  CW_RC		= $0C00;	{ rounding control }
  CW_PC		= $0300;	{ precision control }

  CW_Precision	= $0020;	{ loss of precision mask }
  CW_Underflow	= $0010;	{ underflow mask }
  CW_Overflow	= $0008;	{ overflow mask }
  CW_ZeroDiv	= $0004;	{ divide by zero mask }
  CW_Denormal	= $0002;	{ denormalized operand mask }
  CW_Invalid	= $0001;	{ invalid operation mask }

  CW_Exceptions  	= $003f;	 // all masks

  RC_RND		= $0000;
  RC_DOWN		= $0400;
  RC_UP		= $0800;
  RC_CHOP		= $0C00;

{ p 15-5: Precision control bits affect only the following:
   ADD, SUB(R;, MUL, DIV(R;, and SQRT }
  PR_24_BITS        = $000;
  PR_53_BITS        = $200;
  PR_64_BITS        = $300;
  PR_RESERVED_BITS  = $100;
{ FULL_PRECISION simulates all exceptions masked }
  FULL_PRECISION = (PR_64_BITS or RC_RND or $3f);

  SW_Exc_Mask     = $27f;  { Status word exception bit mask }
  EX_INTERNAL	= $8000;	// Internal error in wm-FPU-emu

  FPU_BUSY        =$8000;   { FPU busy bit (8087 compatibility; }
  EX_ErrorSummary =$0080;   { Error summary status }
{ Special exceptions: }
  EX_StackOver	=$0041 or SW_C1;	{ stack overflow }
  EX_StackUnder	=$0041;	{ stack underflow }
{ Exception flags: }
  EX_Precision	=$0020;	{ loss of precision }
  EX_Underflow	=$0010;	{ underflow }
  EX_Overflow	=$0008;	{ overflow }
  EX_ZeroDiv	=$0004;	{ divide by zero }
  EX_Denormal	=$0002;	{ denormalized operand }
  EX_Invalid	=$0001;	{ invalid operation }

  PRECISION_LOST_UP   = (EX_Precision or SW_C1);
  PRECISION_LOST_DOWN = EX_Precision;

  PROTECTED_ = 4;

  VERIFY_READ = 0;

  VERIFY_WRITE = 1;

  COMP_A_gt_B	= 1;

  COMP_A_eq_B	= 2;
  COMP_A_lt_B	= 3;
  COMP_No_Comp	= 4;
  COMP_Denormal	= $20;
  COMP_NaN 	=	$40;
  COMP_SNaN	 = $80;

  CONST_1: FPU_REG = (sigl: $00000000; sigh: $80000000; exp: (($3fff + (0))));
  CONST_2: FPU_REG = (sigl: $00000000; sigh: $80000000; exp: ($3fff + (1)));
  CONST_HALF: FPU_REG = (sigl: $00000000; sigh: $80000000; exp: ($3fff + (-1)));
  CONST_L2T: FPU_REG = (sigl: $cd1b8afe; sigh: $d49a784b; exp: ($3fff + (1)));
  CONST_L2E: FPU_REG = (sigl: $5c17f0bc; sigh: $b8aa3b29; exp: ($3fff + (0)));
  CONST_PI: FPU_REG = (sigl: $2168c235; sigh: $c90fdaa2; exp: ($3fff + (1)));
  CONST_PI2: FPU_REG = (sigl: $c90fdaa2; sigh: $2168c235; exp: ($3fff + (0)));
  CONST_PI4: FPU_REG = (sigl: $c90fdaa2; sigh: $2168c235; exp: ($3fff + (-1)));
  CONST_LG2: FPU_REG = (sigl: $9a209a84; sigh: $fbcff799; exp: ($3fff + (-2)));
  CONST_LN2: FPU_REG = (sigl: $b17217f7; sigh: $d1cf79ac; exp: ($3fff + (-1)));
  CONST_PI2extra: FPU_REG = (sigl: $ece675d1; sigh: $fc8f8cbb; exp: ($3fff + (-66)));
  CONST_Z: FPU_REG = (sigl: $0; sigh: $0; exp: (($3fff + (-$3fff))));
  CONST_QNaN: FPU_REG = (sigl: $c0000000; sigh: $00000000; exp: ($3fff + ($4000)));
  CONST_INF: FPU_REG = (sigl: $8000; sigh: $00000000; exp: (($3fff + ($4000))));

// Flags for FPU_bits_lost
  LOST_DOWN = 1;
  LOST_UP = 2;
// Flags for FPU_denormal
  DENORMAL = 1;
  UNMASKED_UNDERFLOW = 2;

  ERR_MARGIN = $20;

 { Keep the order TAG_Valid, TAG_Zero, TW_Denormal }
 { The following fold to 2 (Special; in the Tag Word }

  FWAIT_OPCODE     = $9b;
  OP_SIZE_PREFIX   = $66;
  ADDR_SIZE_PREFIX = $67;
  PREFIX_CS        = $2e;
  PREFIX_DS        = $3e;
  PREFIX_ES        = $26;
  PREFIX_SS        = $36;
  PREFIX_FS        = $64;
  PREFIX_GS        = $65;
  PREFIX_REPE      = $f3;
  PREFIX_REPNE     = $f2;
  PREFIX_LOCK      = $f0;
  PREFIX_CS_       = 1;
  PREFIX_DS_       = 2;
  PREFIX_ES_       = 3;
  PREFIX_FS_       = 4;
  PREFIX_GS_       = 5;
  PREFIX_SS_       = 6;
  PREFIX_DEFAULT   = 7;

//  PROTECTED_ = 4;
  SIXTEEN = 1;         // We rely upon this being 1 (true)
  VM86 = SIXTEEN;
  PM16 = (SIXTEEN or PROTECTED_);
  SEG32 = PROTECTED_;

  HIPOWER = 11;

  lterms: array64_10 = (
    ($0000000000000000),
    ($f5fdeffc162c7543),
    ($1c6b08d704a0bfa6),
    ($0276556df749cc21),
    ($002bb0ffcf14f6b8),
    ($0002861225ef751c),
    ($00001ffcbfcd5422),
    ($00000162c005d5f1),
    ($0000000da96ccb1b),
    ($0000000078d1b897),
    ($000000000422b029));

  hiterm: Xsig = (lsw: $c8a39194; midw: $d1cf79ab; msw: $b17217f7);

  shiftterm0: Xsig = (lsw: 0; midw: 0; msw: 0);

  shiftterm1: Xsig = (lsw: $9837f051; midw: $8db8a96f; msw: $46ad2318);

  shiftterm2: Xsig = (lsw: $b504f333; midw: $f9de6484; msw: $597d89b3);

  shiftterm3: Xsig = (lsw: $d744fcca; midw: $d69d6af4; msw: $39a68bb9);

  shiftterm: array[0..3] of PXsig =
    (@shiftterm0, @shiftterm1, @shiftterm2, @shiftterm3);

  HiPOWERop1 = 3;

  oddplterm: array [0..HiPOWERop1 - 1] of u64 = (
    $0000000000000000,
    $0051a1cf08fca228,
    $0000000071284ff7);

  HiPOWERon1 = 2;  { odd poly, negative terms }

  oddnegterm: array[0..HiPOWERon1 - 1] of u64 = (
    $1291a9a184244e80,
    $0000583245819c21);

  HiPOWERep = 2;  { even poly, positive terms }

  evenplterm: array[0..HiPOWERep - 1] of u64 = (
    $0e848884b539e888,
    $00003c7f18b887da);

  HiPOWERen = 2;  { even poly, negative terms }

  evennegterm: array[0..HiPOWERen - 1] of u64 = (
    $f1f0200fd51569cc,
    $003afb46105c4432);

  twothirds: u64 = $aaaaaaaaaaaaaaab;

  N_COEFF_P = 4 - 1;

  N_COEFF_N = 4 - 1;

  N_COEFF_PH = 4 - 1;

  N_COEFF_NH = 4 - 1;

  pos_terms_l: array[0..N_COEFF_P] of u64 =
    (
    $aaaaaaaaaaaaaaab,
    $00d00d00d00cf906,
    $000006b99159a8bb,
    $000000000d7392e6);

  neg_terms_l: array[0..N_COEFF_N] of u64 =
    (
    $2222222222222167,
    $0002e3bc74aab624,
    $0000000b09229062,
    $00000000000c7973);

  pos_terms_h: array[0..N_COEFF_PH] of u64 =
    (
    $0000000000000000,
    $05b05b05b05b0406,
    $000049f93edd91a9,
    $00000000c9c9ed62);

  neg_terms_h: array[0..N_COEFF_NH] of u64 =
    (
    $aaaaaaaaaaaaaa98,
    $001a01a01a019064,
    $0000008f76c68a77,
    $0000000000d58f5e);

  DOUBLE_Emax = 1023;         (* largest valid exponent *)
  DOUBLE_Ebias = 1023;
  DOUBLE_Emin = (-1022);      (* smallest valid exponent *)

  SINGLE_Emax = 127;          (* largest valid exponent *)
  SINGLE_Ebias = 127;
  SINGLE_Emin = (-126);       (* smallest valid exponent *)

//const
//  _NONE_ = 0;   (* st0_ptr etc not needed *)
//
//const
//  _REG0_ = 1;   (* Will be storing st(0) *)
//
//const
//  _PUSH_ = 3;   (* Need to check for space to push onto stack *)
//
//const
//  _null_ = 4;   (* Function illegal or not implemented *)

  type_table: array[0..31] of u_char = (
    _PUSH_, _PUSH_, _PUSH_, _PUSH_,
    _null_, _null_, _null_, _null_,
    _REG0_, _REG0_, _REG0_, _REG0_,
    _REG0_, _REG0_, _REG0_, _REG0_,
    _NONE_, _null_, _NONE_, _PUSH_,
    _NONE_, _PUSH_, _null_, _PUSH_,
    _NONE_, _null_, _NONE_, _REG0_,
    _NONE_, _REG0_, _NONE_, _REG0_
    );

  data_sizes_16: array[0..31] of u_char = (
    4, 4, 8, 2, 0, 0, 0, 0,
    4, 4, 8, 2, 4, 4, 8, 2,
    14, 0, 94, 10, 2, 10, 0, 8,
    14, 0, 94, 10, 2, 10, 2, 8);

  data_sizes_32: array[0..31] of u_char = (
    4, 4, 8, 2, 0, 0, 0, 0,
    4, 4, 8, 2, 4, 4, 8, 2,
    28, 0, 108, 10, 2, 10, 0, 8,
    28, 0, 108, 10, 2, 10, 2, 8);

{$define BETTER_THAN_486}

  FCOS_ = 4;

  FPTAN_ = 8;

  HIPOWERon2 = 6;  // odd poly, negative terms

  oddnegterms: array[0..HIPOWERon2 - 1] of u64 =
    (
    $0000000000000000,  //Dummy (not for - 1.0)
    $015328437f756467,
    $0005dda27b73dec6,
    $0000226bf2bfb91a,
    $000000ccc439c5f7,
    $0000000355438407);

  HIPOWERop = 6;  // odd poly, positive terms

  oddplterms: array[0..HIPOWERop - 1] of u64 = (
    //  $aaaaaaaaaaaaaaab,  transferred to fixedpterm[] */
    $0db55a71875c9ac2,
    $0029fce2d67880b0,
    $0000dfd3908b4596,
    $00000550fd61dab4,
    $0000001c9422b3f9,
    $000000003e3301e1);

  denomterm: u64 = $ebd9b842c5c53a0e;

  fixedpterm: Xsig = (lsw: $aaaaaaaa; midw: $aaaaaaaa; msw: $aaaaaaaa);

  pi_signif: Xsig = (lsw: $c90fdaa2; midw: $2168c234; msw: $c4c6628b);

  exception_names: array[0..9] of TException_Names = (
    (type_: EX_StackOver; Name: 'stack overflow'),
    (type_: EX_StackUnder; Name: 'stack underflow'),
    (type_: EX_Precision; Name: 'loss of precision'),
    (type_: EX_Underflow; Name: 'underflow'),
    (type_: EX_Overflow; Name: 'overflow'),
    (type_: EX_ZeroDiv; Name: 'divide by zero'),
    (type_: EX_Denormal; Name: 'denormalized operand'),
    (type_: EX_Invalid; Name: 'invalid operation'),
    (type_: EX_INTERNAL; Name: 'INTERNAL BUG in FPU_VERSION'),
    (type_: 0; Name: ''));

var
  vardata_sizes_16:  array[0..32] of u_char;
  varI387:           i387_t;
  varfpu_cpu_ptr:    PCPU;
  varfpu_iptr:       PInstruction_tag;
  varCONST_1:        FPU_REG;
  varCONST_2:        FPU_REG;
  varCONST_HALF:     FPU_REG;
  varCONST_L2T:      FPU_REG;
  varCONST_L2E:      FPU_REG;
  varCONST_PI:       FPU_REG;
  varCONST_PI2:      FPU_REG;
  varCONST_PI2extra: FPU_REG;
  varCONST_PI4:      FPU_REG;
  varCONST_LG2:      FPU_REG;
  varCONST_LN2:      FPU_REG;
  varCONST_Z:        FPU_REG;
  varCONST_PINF:     FPU_REG;
  varCONST_INF:      FPU_REG;
  varCONST_MINF:     FPU_REG;
  varCONST_QNaN:     FPU_REG;

  fp_fpu: integer;

//procedure Un_impl;
//procedure FPU_illegal;
//procedure FPU_printall;
procedure FPU_exception (n: intg);
function real_1op_NaN (a: PFPU_REG): intg;
function real_2op_NaN (b: PFPU_REG; tagb: u_char; deststnr: intg;
  defaultNaN: PFPU_REG): intg;
function arith_invalid (deststnr: intg): intg;
function FPU_divide_by_zero (deststnr: intg; sign: u_char): intg;
function set_precision_flag (flags: intg): intg;
procedure set_precision_flag_up;
procedure set_precision_flag_down;
function denormal_operand: intg;
function arith_overflow (dest: PFPU_REG): intg;
function arith_round_overflow (dest: PFPU_REG; sign: u8): intg;
function arith_underflow (dest: PFPU_REG): intg;
procedure FPU_stack_overflow;
procedure FPU_stack_underflow;
procedure FPU_stack_underflow_i (i: intg);
procedure FPU_stack_underflow_pop (i: intg);
function NOT_EMPTY (stnr: intg): intg;

{ fpu_arith.c }
procedure fadd__;
procedure fadd_i;
procedure faddp_;
procedure fmul__;
procedure fmul_i;
procedure fmulp_;
procedure fsub__;
procedure fsubri;
procedure fsubrp;
procedure fsubr_;
procedure fsub_i;
procedure fsubp_;
procedure fdiv__;
procedure fdivri;
procedure fdivrp;
procedure fdivr_;
procedure fdiv_i;
procedure fdivp_;
(*
{ fpu_aux.c }
procedure fclex;
procedure finit;
procedure finit_;
procedure fstsw_;
procedure fp_nop;
procedure fld_i_;
procedure fxch_i;
procedure ffree_;
procedure ffreep;
procedure fst_i_;
procedure fstp_i;
{ fpu_entry.c }
procedure math_emulate(arg:long);
procedure math_abort(info:pinfo; signal:word);
{ fpu_etc.c }
procedure FPU_etc;
{ fpu_tags.c }*)
function  FPU_gettagi (stnr: intg): intg;
function  FPU_gettag (regnr: intg): intg;
(*procedure FPU_settag0(tag:intg);*)
procedure FPU_settagi (stnr: intg; tag: intg);
(*procedure FPU_settag(regnr:intg;tag:intg);*)
function  FPU_Special (ptr: PFPU_REG): intg;
(*function isNaN(ptr:PFPU_REG):intg;*)
procedure FPU_pop;
function  FPU_empty_i (stnr: intg): intg;
(*function FPU_stackoverflow(st_new_ptr:PFPU_REG):intg;
procedure FPU_sync_tags;  *)
procedure FPU_copy_to_regi (r: PFPU_REG; tag: u_char; stnr: intg);
(*procedure FPU_copy_to_reg1(r:PFPU_REG; tag:u_char);*)
procedure FPU_copy_to_reg0 (r: PFPU_REG; tag: u_char);
{ fpu_trig.c }
(*procedure FPU_triga;
procedure FPU_trigb;
{ get_address.c }
function FPU_get_address(FPU_modrm:u_char; fpu_eip:pu32; addr:paddress; addr_modes:fpu_addr_modes):pointer;
function FPU_get_address_16(FPU_modrm:u_char; fpu_eip:pu32; addr:paddress; addr_modes:fpu_addr_modes):pointer;
{ load_store.c }
function FPU_load_store(type_:u_char; addr_modes:fpu_addr_modes; data_address:pointer):intg;
{ poly_2xm1.c }
function poly_2xm1(sign:u_char; arg:PFPU_REG; result:PFPU_REG):intg;
{ poly_atan.c }
procedure poly_atan(st0_ptr:PFPU_REG; st0_tag:u_char; st1_ptr:PFPU_REG; st1_tag:u_char);
{ poly_l2.c }
procedure poly_l2(st0_ptr:PFPU_REG; st1_ptr:PFPU_REG; st1_sign:u_char);
function poly_l2p1(s0:u_char; s1:u_char; r0:PFPU_REG; r1:PFPU_REG; d:PFPU_REG):intg;
{ poly_sin.c }
procedure poly_sine(st0_ptr:PFPU_REG);
procedure poly_cos(st0_ptr:PFPU_REG);
{ poly_tan.c }
procedure poly_tan(st0_ptr:PFPU_REG; flag:intg);
{ reg_add_sub.c }
function FPU_add(b:PFPU_REG; tagb:u_char; destrnr:intg; control_w:u16):intg;
function FPU_sub(flags:intg; rm:PFPU_REG; control_w:u16):intg;   // bbd: changed arg2 from intg to FPU_REG*
{ reg_compare.c }
function FPU_compare_st_data(loaded_data:PFPU_REG; loaded_tag:u_char):intg;
procedure fcom_st;
procedure fcompst;
procedure fcompp;
procedure fucom_;
procedure fucomp;
procedure fucompp;
{ reg_constant.c }
procedure fconst;
{ reg_ld_str.c }
function FPU_load_extended(s:pextended; stnr:intg):intg;
function FPU_load_double(dfloat:pdouble; loaded_data:PFPU_REG):intg;
function FPU_load_single(single:psingle; loaded_data:PFPU_REG):intg;
function FPU_load_int64(_s:ps64):intg;
function FPU_load_int32(_s:ps32; loaded_data:PFPU_REG):intg;
function FPU_load_int16(_s:ps16; loaded_data:PFPU_REG):intg;
function FPU_load_bcd(s:pu_char):intg;
function FPU_store_extended(st0_ptr:PFPU_REG; st0_tag:u_char; d:pextended):intg;
function FPU_store_double(st0_ptr:PFPU_REG; st0_tag:u_char; dfloat:pdouble):intg;
function FPU_store_single(st0_ptr:PFPU_REG; st0_tag:u_char; single:psingle):intg;
function FPU_store_int64(st0_ptr:PFPU_REG; st0_tag:u_char; d:ps64):intg;
function FPU_store_int32(st0_ptr:PFPU_REG; st0_tag:u_char; d:ps32):intg;
function FPU_store_int16(st0_ptr:PFPU_REG; st0_tag:u_char; d:ps16):intg;
function FPU_store_bcd(st0_ptr:PFPU_REG; st0_tag:u_char; d:pu_char):intg;
function FPU_round_to_int(r:PFPU_REG; tag:u_char):intg;
function fldenv(addr_modes:fpu_addr_modes; s:pu_char):pu_char;
procedure frstor(addr_modes:fpu_addr_modes; data_address:pu_char);
function  fstenv(addr_modes:fpu_addr_modes; d:pu_char):pu_char;
procedure fsave(addr_modes:fpu_addr_modes; data_address:pu_char);
function FPU_tagof(ptr:PFPU_REG):intg;
{ reg_mul.c }
function FPU_mul(b:PFPU_REG; tagb:u_char; deststnr:intg; control_w:intg):intg;

function FPU_div(flags:intg; regrm:PFPU_REG; control_w:intg):intg; // bbd: changed arg2 from intg to FPU_REG*
{ reg_convert.c }
function FPU_to_exp16(a:PFPU_REG; x:PFPU_REG):intg;*)

procedure printk (fmt: string);

function  FPU_div (flags: intg; rm: PFPU_REG; control_w: intg): intg;
procedure math_emulate2 (addr_modes: fpu_addr_modes; FPU_modrm: u_char;
  byte1: u_char; data_address: longword; data_sel_off: address; entry_sel_off: address);
function  FPU_gettag0: intg;
procedure FPU_settag (regnr: intg; tag: intg);
function  isNaN (ptr: PFPU_REG): intg;
procedure FPU_settag0 (tag: intg);
function  FPU_stackoverflow (var st_new_ptr: PFPU_REG): intg;
procedure FPU_copy_to_reg1 (const r: PFPU_REG; tag: u_char);
procedure fconst;
function  significand (x: Pfpu_reg): pu64;
function  exponent16 (x: Pfpu_reg): s16;
function  addexponent (x: Pfpu_reg; y: s32): u32; // !!! controllare secondo parametro
procedure stdexp (x: Pfpu_reg);
function  st (x: integer): PFPU_REG;

function  signbyte (a: PFPU_REG): pu_char;
function  getsign (a: PFPU_REG): u_char;
procedure setsign (a: PFPU_REG; b: u_char);
procedure copysign (a: PFPU_REG; b: PFPU_REG);
procedure changesign (a: PFPU_REG);
procedure setpositive (a: PFPU_REG);
procedure setnegative (a: PFPU_REG);
function  signpositive (a: PFPU_REG): u_char;
function  signnegative (a: PFPU_REG): u_char;

function  fpu_rm: pu_char;
procedure clear_C1;
procedure setexponent16 (x: PFPU_REG; y: s32);
function  exponent (x: PFPU_REG): u32;
procedure setexponentpos (x: PFPU_REG; y: integer);

function  FPU_round (x: PFPU_REG; extent: u32; dummy: intg; control_w: u16;
  sign: u8): intg;
function  round_up_64 (x: PFPU_REG; extent: u32): intg;
function  truncate_64 (x: PFPU_REG; extent: u32): intg;
function  round_up_53 (x: PFPU_REG; extent: u32): intg;

procedure add_two_Xsig (dest: PXsig; const x2: PXsig; exp: ps32);
procedure negate_Xsig (x: PXsig);
function  XSIG_LL (x: Xsig): pu64;
function  LL_MSW (x: Pointer): pu64;
procedure add_Xsig_Xsig (dest: PXsig; const x2: PXsig);
function  mul_32_32 (const arg1: u32; const arg2: u32): u32;
function  access_limit: pu_char;

//function  FPU_normalize_nuo (x: PFPU_REG; bias: integer): intg;
function  FPU_u_sub (arg1: PFPU_REG; arg2: PFPU_REG; dest: PFPU_REG;
  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;
function FPU_u_mul (a: PFPU_REG; b: PFPU_REG; c: PFPU_REG; cw: u16;
  sign: u_char; expon: s32): intg;
function  FPU_u_div (a: PFPU_REG; b: PFPU_REG; dest: PFPU_REG;
  control_w: u16; sign: u_char): intg;
//function  FPU_u_add (arg1: PFPU_REG; arg2: PFPU_REG; answ: PFPU_REG;
//  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;
function  wm_sqrt (n: PFPU_REG; dummy1: intg; dummy2: intg; control_w: u16;
  sign: u_char): intg;
function  FPU_shrx (arg1: pointer; arg2: u32): intg;
function  FPU_shrxs (arg1: pointer; arg2: u32): intg;
function  FPU_div_small (x: pu64; y: u32): intg;

function  no_ip_update: u_char;

//function  exponent (x: Pfpu_reg): longword;
function  top: u32;
function  register_base: pu_char;
procedure poppop;
procedure FPU_illegal;
procedure pop_0 ();
function  status_word: intg;
procedure push;

procedure reg_copy (x: PFPU_REG; y: PFPU_REG);
function  fpu_register (index: integer): PFPU_REG;
procedure setcc (cc: u32);

function  FPU_add (b: PFPU_REG; tagb: u_char; deststnr: intg; control_w: word): intg;
function  FPU_sub (flags: intg; rm: PFPU_REG; control_w: u16): intg;
function  add_sub_specials (a: PFPU_REG; taga: u_char; signa: u_char;
  b: PFPU_REG; tagb: u_char; signb: u_char; dest: PFPU_REG; deststnr: intg;
  control_w: u16): intg;

function  FPU_u_add (arg1: PFPU_REG; arg2: PFPU_REG; answ: PFPU_REG;
  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;

function  FPU_to_exp16 (a: PFPU_REG; x: PFPU_REG): intg;

function  FPU_normalize_nuo (x: PFPU_REG; bias: intg): intg;

function  poly_2xm1 (sign: u_char; arg: PFPU_REG; res: PFPU_REG): intg;

procedure polynomial_Xsig (accum: PXsig; const x: pu64; const terms: parr64;
  const n: intg);

procedure mul32_Xsig (x: PXsig; const ba: u32);
procedure mul64_Xsig (x: PXsig; const b: pu64);
procedure mul_Xsig_Xsig (x: PXsig; const b: PXsig);

procedure shr_Xsig (arg: PXsig; nr: intg);

function  FPU_mul (b: PFPU_REG; tagb: u_char; deststnr: intg; control_w: intg): intg;

procedure math_abort (info: pinfo; signal: word);
procedure fpu_verify_area (what: unsigned; ptr: pointer; n: unsigned);
function  fpu_get_user (ptr: pointer; len: integer): longword;
function  fpu_put_user (val: longword; ptr: pointer; len: unsigned): longword;
function  fpu_get_ds: word;

procedure poly_tan (st0_ptr: PFPU_REG; invert: intg);

procedure poly_sine (st0_ptr: PFPU_REG);
procedure poly_cos (st0_ptr: PFPU_REG);

procedure fnop;
procedure fclex;
procedure finit ();

function  compare (b: PFPU_REG; tagb: intg): intg;
function  FPU_compare_st_data (loaded_data: PFPU_REG; loaded_tag: u_char): intg;
procedure fcom_st ();
procedure fcompst ();
procedure fcompp ();
procedure fucom_ ();
procedure fucompp ();
procedure fucomp ();

function  FPU_load_single(single:pfloat; loaded_data:PFPU_REG):intg;
function  FPU_round_to_int(r:PFPU_REG; tag:u_char):intg;
function  FPU_load_int32(_s:ps32; loaded_data:PFPU_REG):intg;
function  FPU_load_double(dfloat:pdouble; loaded_data:PFPU_REG):intg;
function  FPU_load_int16(_s:ps16; loaded_data:PFPU_REG):intg;
function  FPU_store_single(st0_ptr:PFPU_REG; st0_tag:u_char; single:psingle):intg;
function  FPU_store_int32(st0_ptr:PFPU_REG; st0_tag:u_char; d:ps32):intg;
function  FPU_store_double(st0_ptr:PFPU_REG; st0_tag:u_char; dfloat:pdouble):intg;
function  FPU_store_int16(st0_ptr:PFPU_REG; st0_tag:u_char; d:ps16):intg;
function  fldenv(addr_modes:fpu_addr_modes; s:pu_char):pu_char;
procedure frstor(addr_modes:fpu_addr_modes; data_address:pu_char);
function  FPU_load_bcd(s:pu_char):intg;
function  FPU_load_extended(s:extended; stnr:intg):intg;
function  FPU_load_int64(_s:ps64):intg;
function  fstenv(addr_modes:fpu_addr_modes; d:pu_char):pu_char;
procedure fsave(addr_modes:fpu_addr_modes; data_address:pu_char);
function  FPU_store_bcd(st0_ptr:PFPU_REG; st0_tag:u_char; d:u_char):intg;
function  FPU_store_extended(st0_ptr:PFPU_REG; st0_tag:u_char; d:pextended):intg;
function  FPU_store_int64(st0_ptr:PFPU_REG; st0_tag:u_char; d:s64):intg;

function FPU_load_store (type_: u_char; addr_modes: fpu_addr_modes;
  data_address: pointer): intg;

//procedure fclex;
//procedure finit;
procedure fld_i_;
procedure ffree_;
procedure fxch_i;
procedure fp_nop;
procedure fst_i_ ();
procedure fstp_i ();
procedure finit_;
procedure fstsw_ ();

procedure FPU_etc_;

procedure rem_kernel (st0: u64; y: pu64; st1: u64; q: u64; n: intg);
procedure fcos (st0_ptr: PFPU_REG; st0_tag: u_char);
function fsin (st0_ptr: PFPU_REG; tag: u_char): intg;
procedure fscale (st0_ptr: PFPU_REG; st0_tag: u_char);
procedure FPU_triga;
procedure FPU_trigb;



(* Used only by fptan, fsin, fcos, and fsincos. *)
(* This routine produces very accurate results, similar to
   using a value of pi with more than 128 bits precision. *)
(* Limited measurements show no results worse than 64 bit precision
   except for the results for arguments close to 2^63, where the
   precision of the result sometimes degrades to about 63.9 bits *)

procedure div_Xsig (const aa: PXsig; const b: PXsig; dest: PXsig);

function round_Xsig (x: PXsig): intg;
function norm_Xsig (x: PXsig): intg;

procedure log2_kernel(const arg:PFPU_REG; argsign:u_char; accum_result:PXsig;
			expon:ps32);
procedure	poly_l2(st0_ptr:PFPU_REG; st1_ptr:PFPU_REG; st1_sign:u_char);

function poly_l2p1(sign0:u_char; sign1:u_char;
		  st0_ptr:PFPU_REG; st1_ptr:PFPU_REG; dest:PFPU_REG):intg;

procedure poly_atan (st0_ptr: PFPU_REG; st0_tag: u_char; st1_ptr: PFPU_REG;
  st1_tag: u_char);

const
  st_instr_table: array[0..63] of procedure = (
    fadd__, fld_i_, FPU_illegal, FPU_illegal, fadd_i, ffree_, faddp_, FPU_illegal,
    fmul__, fxch_i, FPU_illegal, FPU_illegal, fmul_i, FPU_illegal,
    fmulp_, FPU_illegal,
    fcom_st, fp_nop, FPU_illegal, FPU_illegal, FPU_illegal, fst_i_,
    FPU_illegal, FPU_illegal,
    fcompst, FPU_illegal, FPU_illegal, FPU_illegal, FPU_illegal,
    fstp_i, fcompp, FPU_illegal,
    fsub__, FPU_etc_, FPU_illegal, finit_, fsubri, fucom_, fsubrp, fstsw_,
    fsubr_, fconst, fucompp, FPU_illegal, fsub_i, fucomp, fsubp_, FPU_illegal,
    fdiv__, FPU_triga, FPU_illegal, FPU_illegal, fdivri, FPU_illegal,
    fdivrp, FPU_illegal,
    fdivr_, FPU_trigb, FPU_illegal, FPU_illegal, fdiv_i, FPU_illegal,
    fdivp_, FPU_illegal);

implementation

(*
  Divide one register by another and put the result into a third register.
bbd: arg2 used to be an int, see comments on FPU_sub.
  *)

var
  AlreadyGone: boolean = False;

function FPU_div (flags: intg; rm: PFPU_REG; control_w: intg): intg;
var
  x, y: FPU_REG;
  a, b, st0_ptr, st_ptr: PFPU_REG;
  dest: PFPU_REG;
  taga, tagb, signa, signb, sign{, saved_sign}: u_char;
  tag, deststnr: intg;
  rmint: intg;
  P: PFPU_REG;
begin
  rmint := bx_ptr_equiv_t(rm);

//  if (flags and DEST_RM) <> 0
//    then deststnr := rmint
//    else deststnr := 0;
  deststnr := rmint * integer((flags and DEST_RM) <> 0);

  if (flags and REV) <> 0 then
  begin
    b := st(0);
    st0_ptr := b;
    tagb := FPU_gettag0();
    if (flags and LOADED) <> 0 then
    begin
      a := rm;
      taga := flags and $0f;
    end else
    begin
      a := st(rmint);
//        st_ptr := a; [hint]
      taga := FPU_gettagi(rmint);
    end;
  end else
  begin
    a := st(0);
    st0_ptr := a;
    taga := FPU_gettag0();
    if (flags and LOADED) <> 0 then
    begin
      b := rm;
      tagb := flags and $0f;
    end else
    begin
      b := st(rmint);
//        st_ptr := b; [hint]
      tagb := FPU_gettagi(rmint);
    end;
  end;

  signa := getsign(a);
  signb := getsign(b);

  sign := signa xor signb;

  dest := st(deststnr);
  {saved_sign := }getsign(dest); // [hint]

  if (taga or tagb) = 0 then
  begin
    (* Both regs Valid, this should be the most common case. *)
    reg_copy(a, @x);
    reg_copy(b, @y);
    setpositive(@x);
    setpositive(@y);
    tag := FPU_u_div(@x, @y, dest, control_w, sign);

    if (tag < 0) then
      Exit(tag);

    FPU_settagi(deststnr, tag);
    Exit(tag);
  end;

  if (taga = TAG_Special) then
    taga := FPU_Special(a);
  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);

  if (((taga = TAG_Valid) and (tagb = TW_Denormal)) or
      ((taga = TW_Denormal) and (tagb = TAG_Valid)) or
      ((taga = TW_Denormal) and (tagb = TW_Denormal))) then
  begin
    if denormal_operand() < 0 then
      Exit(FPU_Exception_c);

    FPU_to_exp16(a, @x);
    FPU_to_exp16(b, @y);
    tag := FPU_u_div(@x, @y, dest, control_w, sign);
    if tag < 0 then
      Exit(tag);

    FPU_settagi(deststnr, tag);
    Exit(tag);
  end else
  if (taga <= TW_Denormal) and (tagb <= TW_Denormal) then
  begin
    if tagb <> TAG_Zero then
    begin
      (* Want to find Zero/Valid *)
      if (tagb = TW_Denormal) and (denormal_operand() < 0) then
        Exit(FPU_Exception_c);

      (* The result is zero. *)
      FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);
      setsign(dest, sign);
      Exit(TAG_Zero);
    end;
    (* We have an exception condition, either 0/0 or Valid/Zero. *)
    if taga = TAG_Zero then
    begin
      (* 0/0 *)
      Result := arith_invalid(deststnr);
      Exit;
    end;
    (* Valid/Zero *)
    exit(FPU_divide_by_zero(deststnr, sign));
  end
    (* Must have infinities, NaNs, etc *)
  else
  if (taga = TW_NaN) or (tagb = TW_NaN) then
  begin
    if (flags and LOADED) <> 0 then
      Exit(real_2op_NaN(PFPU_REG(rm), flags and $0f, 0, st0_ptr));

    if (flags and DEST_RM) <> 0 then
    begin
      tag := FPU_gettag0();
      if (tag = TAG_Special) then
        tag := FPU_Special(st0_ptr);

      if (flags and REV) <> 0
        then p := st0_ptr
        else p := st(rmint);

      Exit(real_2op_NaN(st0_ptr, tag, rmint, p));
    end else
    begin
      tag := FPU_gettagi(rmint);
      if (tag = TAG_Special) then
        tag := FPU_Special(st(rmint));

      if (flags and REV) <> 0
        then p := st0_ptr
        else p := st(rmint);

      exit(real_2op_NaN(st(rmint), tag, 0, p));
    end;
  end else
  if (taga = TW_Infinity) then
  begin
    if tagb = TW_Infinity then
          (* infinity/infinity *)
      Exit(arith_invalid(deststnr))
    else
    begin
      (* tagb must be Valid or Zero *)
      if ((tagb = TW_Denormal) and (denormal_operand() < 0)) then
        Exit(FPU_Exception_c);

//      Exit; // íàäî ëè?

(* Infinity divided by Zero or Valid does
not raise and exception, but returns Infinity *)
      FPU_copy_to_regi(a, TAG_Special, deststnr);
      setsign(dest, sign);
      exit(taga);
    end;
  end else
  if tagb = TW_Infinity then
  begin
    if ((taga = TW_Denormal) and (denormal_operand() < 0)) then
      Exit(FPU_Exception_c);

    (* The result is zero. *)
    FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);
    setsign(dest, sign);
    Exit(TAG_Zero);
  end;
//{$ifdef PARANOID}
//else
//  begin
//    EXCEPTION(EX_INTERNAL|$102);
//    return FPU_Exception;
//  end;
//{$endif}(* PARANOID *)
end;

procedure math_emulate2 (addr_modes: fpu_addr_modes; FPU_modrm: u_char;
  byte1: u_char; data_address: longword; data_sel_off: address; entry_sel_off: address);
var
  code: u16;
  unmasked: intg;
  loaded_data: FPU_REG;
  st0_ptr: PFPU_REG;
  loaded_tag, st0_tag: u_char;
  status1: u16;
  instr_index: u_char;
//    MyAddr: longword; [hint]
label
  do_the_FPU_interrupt,
  FPU_fwait_done,
  reg_mem_instr_done,
  FPU_instruction_done;
begin
  // assuming byte is $d8..$df or $db=FWAIT

  // lock is not a valid prefix for FPU instructions, +++
  // let the cpu handle it to generate a SIGILL.
  varI387.soft.no_update := 0;

  if byte1 = FWAIT_OPCODE then
  begin
    if (varI387.soft.swd and SW_Summary) <> 0 then
      goto do_the_FPU_interrupt
    else
      goto FPU_fwait_done;
  end;

  if (varI387.soft.swd and SW_Summary) <> 0 then
  begin
  (* Ignore the error for now if the current instruction is a no-wait
     control instruction *)
  (* The 80486 manual contradicts itself on this topic,
     but a real 80486 uses the following instructions:
     fninit, fnstenv, fnsave, fnstsw, fnstenv, fnclex.
     *)
    code := (FPU_modrm shl 8) or byte1;
    if (((((code and $f803) = $e003) or  (* fnclex, fninit, fnstsw *)
       (((code and $3003) = $3001) and   (* fnsave, fnstcw, fnstenv, fnstsw *)
      ((code and $c000) <> $c000)))) = False) then
    begin
    (*
     *  We need to simulate the action of the kernel to FPU
     *  interrupts here.
     *)
      do_the_FPU_interrupt:

      math_abort(nil, 0);
      exit;
    end;
  end;

  entry_sel_off.opcode := ((byte1 shl 8) or FPU_modrm) and $7FF;

  pu_char(@varI387.soft.rm)^ := FPU_modrm and 7;

  if FPU_modrm < 192 then
  begin
    (* All of these instructions use the mod_/rm byte to get a data address *)

    if (byte1 and 1) = 0 then
    begin
      status1 := varI387.soft.swd;

      st0_ptr := st(0);
      st0_tag := FPU_gettag0();

      (* Stack underflow has priority *)
      if (st0_tag xor TAG_Empty) <> 0 then
      begin
        if (addr_modes.default_mode and PROTECTED_) <> 0 then
        begin
          (* This table works for 16 and 32 bit protected mod_e *)
          if (varI387.soft.alimit < data_sizes_16[(byte1 shr 1) and 3]) then
          begin
            math_abort(nil, 0);
            exit;
          end;
        end;

        unmasked := 0;  (* Do this here to stop compiler warnings. *)
        case ((byte1 shr 1) and 3) of
          0:
          begin
            unmasked := FPU_load_single(pfloat(Data_Address), @loaded_data);
            loaded_tag := unmasked and $ff;
            unmasked := unmasked and not $ff;
          end;

          1:
          begin
            loaded_tag := FPU_load_int32(ps32(Data_Address), @loaded_data);
            // bbd: was (u32 *)
          end;

          2:
          begin
            unmasked := FPU_load_double(pdouble(Data_Address), @loaded_data);
            loaded_tag := unmasked and $ff;
            unmasked := unmasked and not $ff;
          end;

          3:
          begin
            loaded_tag := FPU_load_int16(ps16(Data_Address), @loaded_data);
          end;
        else  (* Used here to suppress gcc warnings. *)
          begin
            loaded_tag := FPU_load_int16(ps16(Data_Address), @loaded_data);
          end;
        end;

            (* No more access to user memory, it is safe
               to use static data now *)

        (* NaN operands have the next priority. *)
            (* We have to delay looking at st(0) until after
               loading the data, because that data might contain an SNaN *)
        if ((st0_tag = TAG_Special) and (isNaN(st0_ptr) <> 0)) or
           ((loaded_tag = TAG_Special) and (isNaN(@loaded_data) <> 0)) then
        begin
                (* Restore the status word; we might have loaded a
                   denormal. *)
          varI387.soft.swd := status1;
          if ((FPU_modrm and $30) = $10) then
          begin
            (* fcom or fcomp *)
            fpu_EXCEPTION(EX_Invalid);
            setcc(SW_C3 or SW_C2 or SW_C0);
            if ((FPU_modrm and $08) <> 0 and
               (varI387.soft.cwd and CW_Invalid)) then
              FPU_pop();             (* fcomp, masked, so we pop. *)
          end else
          begin
            if (loaded_tag = TAG_Special) then
              loaded_tag := FPU_Special(@loaded_data);
{$ifdef PECULIAR_486}
                    (* This is not really needed, but gives behaviour
                       identical to an 80486 *)
            if ( (FPU_modrm  and $28) = $20 ) then
              (* fdiv or fsub *)
              real_2op_NaN(@loaded_data, loaded_tag, 0, @loaded_data)
            else
{$endif}      (* PECULIAR_486 *)
            (* fadd, fdivr, fmul, or fsubr *)
              real_2op_NaN(@loaded_data, loaded_tag, 0, st0_ptr);
          end;
          goto reg_mem_instr_done;
        end;

        if ((unmasked <> 0) and (((FPU_modrm and $30) = $10) = False)) then
        begin
          (* Is not a comparison instruction. *)
          if ((FPU_modrm and $38) = $38) then
          begin
            (* fdivr *)
            if ((st0_tag = TAG_Zero) and
               ((loaded_tag = TAG_Valid) or ((loaded_tag = TAG_Special) and
               (exponent(@loaded_data) = EXP_BIAS + EXP_UNDER)))) then
            begin
              if (FPU_divide_by_zero(0, getsign(@loaded_data)) < 0) then
              begin
                            (* We use the fact here that the unmasked
                               exception in the loaded data was for a
                               denormal operand *)
                (* Restore the state of the denormal op bit *)
                varI387.soft.swd := varI387.soft.swd and not SW_Denorm_Op;
                varI387.soft.swd := varI387.soft.swd or status1 and SW_Denorm_Op;
              end else
                setsign(st0_ptr, getsign(@loaded_data));
            end;
          end;
          goto reg_mem_instr_done;
        end;

        case ((FPU_modrm shr 3) and 7) of
          0:         (* fadd *)
          begin
            clear_C1();
            FPU_add(@loaded_data, loaded_tag, 0, varI387.soft.cwd);
          end;
          1:         (* fmul *)
          begin
            clear_C1();
            FPU_mul(@loaded_data, loaded_tag, 0, varI387.soft.cwd);
          end;
          2:         (* fcom *)
          begin
            FPU_compare_st_data(@loaded_data, loaded_tag);
          end;
          3:         (* fcomp *)
          begin
            // bbd: used to typecase to int first, but this corrupted the
            // pointer on 64 bit machines.
            if ((FPU_compare_st_data(@loaded_data, loaded_tag) = 0) and
               (unmasked = 0)) then
              FPU_pop();
          end;
          4:         (* fsub *)
          begin
            clear_C1();
            FPU_sub(LOADED or loaded_tag, @loaded_data, varI387.soft.cwd);
          end;
          5:         (* fsubr *)
          begin
            clear_C1();
            FPU_sub(REV or LOADED or loaded_tag, @loaded_data, varI387.soft.cwd);
          end;
          6:         (* fdiv *)
          begin
            clear_C1();
            FPU_div(LOADED or loaded_tag, @loaded_data, varI387.soft.cwd);
          end;
          7:         (* fdivr *)
          begin
            clear_C1();
            if (st0_tag = TAG_Zero) then
              varI387.soft.swd := status1;  (* Undo any denorm tag,
                                                zero-divide has priority. *)
            FPU_div(REV or LOADED or loaded_tag, @loaded_data, varI387.soft.cwd);
          end;
        end;
      end else
      begin
        if ((FPU_modrm and $30) = $10) then
        begin
          (* The instruction is fcom or fcomp *)
          fpu_EXCEPTION(EX_StackUnder);
          setcc(SW_C3 or SW_C2 or SW_C0);
          if (((FPU_modrm and $08) <> 0) and
            ((varI387.soft.cwd and CW_Invalid) <> 0)) then
            FPU_pop();             (* fcomp *)
        end else
          FPU_stack_underflow();
      end;
      reg_mem_instr_done:
        paddress(@varI387.soft.foo)^ := data_sel_off;
    end else
    begin
      varI387.soft.no_update :=
        FPU_load_store(((FPU_modrm and $38) or (byte1 and 6)) shr
              1, addr_modes, Pointer(Data_Address));

      if varI387.soft.no_update = 0 then
        paddress(@varI387.soft.foo)^ := data_sel_off;
    end;
  end else
  begin
    (* None of these instructions access user memory *)
    instr_index := (FPU_modrm and $38) or (byte1 and 7);

{$ifdef PECULIAR_486}
    (* This is supposed to be undefined, but a real 80486 seems
       to do this: *)
    paddress(@varI387.soft.foo)^.offset := 0;
    paddress(@varI387.soft.foo)^.selector := fpu_get_ds;
{$endif}(* PECULIAR_486 *)

//      st0_ptr := st(0); [hint]
    st0_tag := FPU_gettag0();
    case (type_table_2[instr_index]) of
      _NONE_:   (* also _REGIc: _REGIn *)
      begin
      end;
      _REG0_:
      begin
        if (st0_tag xor TAG_Empty) = 0 then
        begin
          FPU_stack_underflow();
          goto FPU_instruction_done;
        end;
      end;
      _REGIi:
      begin
        if (((st0_tag xor TAG_Empty) = 0) or (FPU_empty_i(FPU_rm^) <> 0)) then
        begin
          FPU_stack_underflow_i(FPU_rm^);
          goto FPU_instruction_done;
        end;
      end;
      _REGIp:
      begin
        if (((st0_tag xor TAG_Empty) = 0) or (FPU_empty_i(FPU_rm^) <> 0)) then
        begin
          FPU_stack_underflow_pop(FPU_rm^);
          goto FPU_instruction_done;
        end;
      end;
      _REGI_:
      begin
        if (((st0_tag xor TAG_Empty) = 0) or (FPU_empty_i(FPU_rm^) <> 0)) then
        begin
          FPU_stack_underflow();
          goto FPU_instruction_done;
        end;
      end;
      _PUSH_:     (* Only used by the fld st(i) instruction *)
      begin
      end;
      _null_:
      begin
        FPU_illegal();
        goto FPU_instruction_done;
      end;
    else
      begin
        fpu_EXCEPTION(EX_INTERNAL or $111);
        goto FPU_instruction_done;
      end;
    end;
    st_instr_table[instr_index];

    FPU_instruction_done: ;
  end;

  if (no_ip_update) = 0 then
    varI387.soft.fip := integer(@entry_sel_off);

  FPU_fwait_done: ;
end;

function FPU_gettag0: intg;
begin
  Result := (varI387.soft.twd shr ((top and 7) * 2)) and 3;
end;

procedure FPU_settag (regnr: intg; tag: intg);
begin
  regnr := regnr and 7;
  varI387.soft.twd := varI387.soft.twd and not (3 shl (regnr * 2));
  varI387.soft.twd := varI387.soft.twd or (tag and 3) shl (regnr * 2);
end;

procedure FPU_settag0 (tag: intg);
var
  regnr: intg;
begin
  regnr := top;
  regnr := regnr and 7;
  varI387.soft.twd := varI387.soft.twd and not (3 shl (regnr * 2));
  varI387.soft.twd := varI387.soft.twd or (tag and 3) shl (regnr * 2);
end;

function isNaN (ptr: PFPU_REG): intg;
begin
  Result := word((exponent(ptr) = EXP_BIAS + EXP_OVER) and
    ((ptr^.sigh = $8000) and (ptr^.sigl = 0)) = False);
end;

function FPU_stackoverflow (var st_new_ptr: PFPU_REG): intg;
begin
  st_new_ptr := st(-1);

  Result := word(((varI387.soft.twd shr (((varI387.soft.ftop - 1) and 7) * 2)) and 3) <>
    TAG_Empty);
end;

procedure FPU_copy_to_reg1 (const r: PFPU_REG; tag: u_char);
begin
  reg_copy(r, st(1));
  FPU_settagi(1, tag);
end;

procedure fld_const (c: PFPU_REG; adj: intg; tag: u_char);
var
  st_new_ptr: PFPU_REG;
begin

  if (FPU_stackoverflow(st_new_ptr)) <> 0 then
  begin
    FPU_stack_overflow();
    exit;
  end;
  Dec(varI387.soft.ftop);
  reg_copy(c, st_new_ptr); // zzzzzzzzz
  st_new_ptr^.sigl := st_new_ptr^.sigl + adj;  (* For all our fldxxx constants, we don't need to
           borrow or carry. *)
  FPU_settag0(tag);
  clear_C1();
end;

(* A fast way to find out whether x is one of RC_DOWN or RC_CHOP
   (and not one of RC_RND or RC_UP).
   *)
procedure fld1 (rc: intg);
begin
  fld_const(@CONST_1, 0, TAG_Valid);
end;

procedure fldl2t (rc: intg);
begin
  fld_const(@CONST_L2T, ifthen(rc = RC_UP, 1, 0), TAG_Valid);
end;

procedure fldl2e (rc: intg);
begin
  fld_const(@CONST_L2E, ifthen(rc and RC_DOWN <> 0, -1, 0), TAG_Valid);
end;

procedure fldpi (rc: intg);
begin
  fld_const(@CONST_PI, ifthen(rc and RC_DOWN <> 0, -1, 0), TAG_Valid);
end;

procedure fldlg2 (rc: intg);
begin
  fld_const(@CONST_LG2, ifthen(rc and RC_DOWN <> 0, -1, 0), TAG_Valid);
end;

procedure fldln2 (rc: intg);
begin
  fld_const(@CONST_LN2, ifthen(rc and RC_DOWN <> 0, -1, 0), TAG_Valid);
end;

procedure fldz (rc: intg);
begin
  fld_const(@CONST_Z, 0, TAG_Zero);
end;

procedure FPU_illegalRC (i: intg);
begin
end;

type
  FUNC_RC = procedure (i: intg);

const
  constants_table: array[0..7] of FUNC_RC = (
    fld1, fldl2t, fldl2e, fldpi, fldlg2, fldln2, fldz, FPU_illegalRC);

procedure fconst;
var
  P: FUNC_RC;
begin
  P := constants_table[FPU_rm^];
  P(varI387.soft.cwd and CW_RC);
end;

function significand (x: Pfpu_reg): pu64;
begin
{$ifdef EMU_BIG_ENDIAN}
  Result:=@x^.sigh;
{$else}
  Result := @x^.sigl;
{$endif}
end;

function exponent16 (x: Pfpu_reg): s16;
  begin
    Result := x^.Exp;
  end;

function addexponent (x: Pfpu_reg; y: s32): u32;
begin
  x^.exp := x^.exp + y;
end;

procedure stdexp (x: Pfpu_reg);
begin
  x^.exp := x^.exp + EXTENDED_Ebias;
end;

function registers: pu_char;
begin
end;

function st (x: integer): PFPU_REG;
begin
  Result := PFPU_REG(longint(register_base) + sizeof(FPU_REG) *
    ((varI387.soft.ftop + x) and 7));
end;

function signbyte (a: PFPU_REG): pu_char;
var
  p: PChar;
begin
  p := PChar(longint(a) + 9);
  Result := pu_char(p);
end;

function getsign (a: PFPU_REG): u_char;
begin
  Result := signbyte(a)^ and $80;
end;

procedure setsign (a: PFPU_REG; b: u_char);
begin
  if b <> 0 then
    pu_char(longint(a) + 8)^ := pu_char(longint(a) + 8)^ or $80
  else
    pu_char(longint(a) + 8)^ := pu_char(longint(a) + 8)^ and $7f;
end;

procedure copysign (a: PFPU_REG; b: PFPU_REG);
begin
  if getsign(a) <> 0 then
    signbyte(b)^ := signbyte(b)^ or $80
  else
    signbyte(b)^ := $7f;
end;

procedure changesign (a: PFPU_REG);
begin
  signbyte(a)^ := signbyte(a)^ xor $80;
end;

procedure setpositive (a: PFPU_REG);
begin
  signbyte(a)^ := signbyte(a)^ and $7f;
end;

procedure setnegative (a: PFPU_REG);
begin
  signbyte(a)^ := signbyte(a)^ or $80;
end;

function signpositive (a: PFPU_REG): u_char;
begin
  Result := word((signbyte(a)^ and $80) = 0);
end;

function signnegative (a: PFPU_REG): u_char;
begin
  Result := word((signbyte(a)^ and $80));
end;

function fpu_rm: pu_char;
begin
  Result := pu_char(@varI387.soft.rm);
end;

procedure clear_C1;
begin
  varI387.soft.swd := varI387.soft.swd and not SW_C1;
end;

procedure setexponent16 (x: PFPU_REG; y: s32);
begin
  x^.exp := y;
end;

function exponent (x: PFPU_REG): u32;
begin
  Result := ((x^.exp and $7fff) - EXTENDED_Ebias);
end;

procedure setexponentpos (x: PFPU_REG; y: integer);
begin
  x^.exp := ((y) + EXTENDED_Ebias) and $7fff;
end;

function round_up_64 (x: PFPU_REG; extent: u32): intg;
begin
  Inc(x^.sigl);
  if (x^.sigl = 0) then
  begin
    Inc(x^.sigl);
    if x^.sigh = 0 then
    begin
      x^.sigh := $8000;
      Inc(x^.exp);
    end;
  end;
  Result := LOST_UP;
end;

function truncate_64 (x: PFPU_REG; extent: u32): intg;
begin
  Result := LOST_DOWN;
end;

function round_up_53 (x: PFPU_REG; extent: u32): intg;
begin
  x^.sigl := x^.sigl and $fffff800;
  Inc(x^.sigl, $800);
  if x^.sigl = 0 then
  begin
    Inc(x^.sigh);
    if (x^.sigh = 0) then
    begin
      x^.sigh := $8000;
      Inc(x^.exp);
    end;
  end;
  Result := LOST_UP;
end;

function truncate_53 (x: PFPU_REG; extent: u32): intg;
begin
  x^.sigl := x^.sigl and $fffff800;
  Result  := LOST_DOWN;
end;

function round_up_24 (x: PFPU_REG; extent: u32): intg;
begin
  x^.sigl := 0;
  x^.sigh := x^.sigh and $ffffff00;
  Inc(x^.sigh, $100);
  if x^.sigh = 0 then
  begin
    x^.sigh := $8000;
    Inc(x^.exp);
  end;
  Result := LOST_UP;
end;

function truncate_24 (x: PFPU_REG; extent: u32): intg;
begin
  x^.sigl := 0;
  x^.sigh := x^.sigh and $ffffff00;
  Result  := LOST_DOWN;
end;

function FPU_round (x: PFPU_REG; extent: u32; dummy: intg; control_w: u16;
  sign: u8): intg;
var
  work:  u64;
  leading: u32;
  expon: s16;
  FPU_bits_lost, FPU_denormal, shift, tag: intg;
begin
  expon := x^.exp;
  FPU_bits_lost := 0;
  if expon <= EXP_UNDER then
  begin
    (* A denormal or zero *)
    if (control_w and CW_Underflow) <> 0 then
    begin
      (* Underflow is masked. *)
      FPU_denormal := DENORMAL;
      shift := EXP_UNDER + 1 - expon;
      if shift >= 64 then
      begin
        if shift = 64 then
        begin
          Inc(x^.exp, 64);
          if (extent or x^.sigl) <> 0 then
            extent := x^.sigh or 1
          else
            extent := x^.sigh;
        end else
        begin
          x^.exp := EXP_UNDER + 1;
          extent := 1;
        end;
        pu64(x)^ := 0;
      end else
      begin
        Inc(x^.exp, shift);
        if shift >= 32 then
        begin
          Dec(shift, 32);
          if (shift) <> 0 then
          begin
            extent := extent or x^.sigl;
            work := x^.sigh shr shift;
            if (extent) <> 0 then
              extent := work or 1
            else
              extent := work;
            x^.sigh := x^.sigh shr shift;
            x^.sigl := x^.sigh;
          end else
          begin
            if (extent) <> 0 then
              extent := x^.sigl or 1
            else
              extent := x^.sigl;
            x^.sigl := x^.sigh;
          end;
          x^.sigh := 0;
        end else
        begin
          (* Shift by 1 to 32 places. *)
          work := x^.sigl;
          work := work shl 32;
          work := work or extent;
          work := work shr shift;
          if (extent) <> 0 then
            extent := 1;
          extent := extent or work;
          pu64(x^.sigh)^ := pu64(x^.sigh)^ shr shift;
        end;
      end;
    end else
    begin
      (* Unmasked underflow. *)
      FPU_denormal := UNMASKED_UNDERFLOW;
    end;
  end else
    FPU_denormal := 0;

  case (control_w and CW_PC) of
    01:
    begin
      {$ifndef PECULIAR_486}
    (* With the precision control bits set to 01 '(reserved)', a real 80486
 behaves as if the precision control bits were set to 11 '64 bits' *)
      {$ifdef PARANOID}
      Exception(EX_INTERNAL|$236);
      return - 1;
      {$endif}
      {$endif}
    end;
    (* Fall through to the 64 bit case. *)
    PR_64_BITS:
    begin
      if (extent) <> 0 then
      begin
        case (control_w and CW_RC) of
          RC_RND:
          begin(* Nearest or even *)
            (* See if there is exactly half a ulp. *)
            if (extent = $80000000) then
            begin
              (* Round to even. *)
              if (x^.sigl and $1) <> 0 then
                (* Odd *)
                FPU_bits_lost := round_up_64(x, extent)
              else
                (* Even *)
                FPU_bits_lost := truncate_64(x, extent);
            end else
              if (extent > $80000000) then
                (* Greater than half *)
                FPU_bits_lost := round_up_64(x, extent)
              else
                (* Less than half *)
                FPU_bits_lost := truncate_64(x, extent);
          end;

          RC_CHOP:    (* Truncate *)
          begin
            FPU_bits_lost := truncate_64(x, extent);
          end;

          RC_UP:    (* Towards +infinity *)
          begin
            if sign = SIGN_POS then
              FPU_bits_lost := round_up_64(x, extent)
            else
              FPU_bits_lost := truncate_64(x, extent);
          end;

          RC_DOWN:    (* Towards -infinity *)
          begin
            if sign <> SIGN_POS then
              FPU_bits_lost := round_up_64(x, extent)
            else
              FPU_bits_lost := truncate_64(x, extent);
          end;

          else
          begin
            //EXCEPTION(EX_INTERNAL|$231); MANCA!!!
            exit(-1);
          end;
        end;
      end;
    end;

    PR_53_BITS:
    begin
      leading := x^.sigl and $7ff;
      if (extent <> 0) or (leading <> 0) then
      begin
        case (control_w and CW_RC) of
          RC_RND:    (* Nearest or even *)
          begin
            (* See if there is exactly half a ulp. *)
            if leading = $400 then
            begin
              if extent = 0 then
              begin
                (* Round to even. *)
                if (x^.sigl and $800) <> 0 then
                  FPU_bits_lost := round_up_53(x, extent)
                else
                  FPU_bits_lost := truncate_53(x, extent);
              end else
                (* Greater than half *)
                FPU_bits_lost := round_up_53(x, extent);
            end else
              if leading > $400 then
                (* Greater than half *)
                FPU_bits_lost := round_up_53(x, extent)
              else
                (* Less than half *)
                FPU_bits_lost := truncate_53(x, extent);
          end;

          RC_CHOP:    (* Truncate *)
          begin
            FPU_bits_lost := truncate_53(x, extent);
          end;

          RC_UP:    (* Towards +infinity *)
          begin
            if sign = SIGN_POS then
              FPU_bits_lost := round_up_53(x, extent)
            else
              FPU_bits_lost := truncate_53(x, extent);
          end;

          RC_DOWN:    (* Towards -infinity *)
          begin
            if sign <> SIGN_POS then
              FPU_bits_lost := round_up_53(x, extent)
            else
              FPU_bits_lost := truncate_53(x, extent);
          end;

          else
          begin
            //EXCEPTION(EX_INTERNAL|$231); manca
            Exit(-1);
          end;
        end;
      end;
    end;

    PR_24_BITS:
    begin
      leading := x^.sigh and $ff;
      if (leading <> 0) or (x^.sigl <> 0) or (extent <> 0) then
      begin
        case (control_w and CW_RC) of
          RC_RND:    (* Nearest or even *)
          begin
            (* See if there is exactly half a ulp. *)
            if (leading = $80) then
            begin
              if ((x^.sigl = 0) and (extent = 0)) then
              begin
                (* Round to even. *)
                if (x^.sigh and $100) <> 0 then
                  (* Odd *)
                  FPU_bits_lost := round_up_24(x, extent)
                else
                  (* Even *)
                  FPU_bits_lost := truncate_24(x, extent);
              end else
                (* Greater than half *)
                FPU_bits_lost := round_up_24(x, extent);
            end else
              if (leading > $80) then
                (* Greater than half *)
                FPU_bits_lost := round_up_24(x, extent)
              else
                (* Less than half *)
                FPU_bits_lost := truncate_24(x, extent);
          end;

          RC_CHOP:    (* Truncate *)
          begin
            FPU_bits_lost := truncate_24(x, extent);
          end;

          RC_UP:    (* Towards +infinity *)
          begin
            if (sign = SIGN_POS) then
              FPU_bits_lost := round_up_24(x, extent)
            else
              FPU_bits_lost := truncate_24(x, extent);
          end;

          RC_DOWN:    (* Towards -infinity *)
          begin
            if (sign <> SIGN_POS) then
              FPU_bits_lost := round_up_24(x, extent)
            else
              FPU_bits_lost := truncate_24(x, extent);
          end;

          else
          begin
            //Exception(EX_INTERNAL|$231); manca!!!
            Exit(-1);
          end;
        end;
      end;
    end;

  end;

  tag := TAG_Valid;

  if FPU_denormal <> 0 then
  begin
    (* Undo the de-normalisation. *)
    if FPU_denormal = UNMASKED_UNDERFLOW then
    begin
      (* Increase the exponent by the magic number *)
      if x^.exp <= EXP_UNDER then
        Inc(x^.exp, 3 * (1 shl 13));
        //Exception(EX_Underflow); manca
    end else
    begin
//      if x^.exp <> EXP_UNDER + 1 then
//      begin
//        //Exception(EX_INTERNAL|$234); manca
//      end;

      if (x^.sigh = 0) and (x^.sigl = 0) then
      begin
        (* Underflow to zero *)
        // set_precision_flag_down(); manca, vedi in errors.c
        //Exception(EX_Underflow);
        x^.exp := EXP_UNDER;
        tag := TAG_Zero;
        FPU_bits_lost := 0;  (* Stop another call to
           set_precision_flag_down() *)
      end else
      begin
        if (x^.sigh and $8000) <> 0 then
        begin
              {$ifdef PECULIAR_486}
(*
 * This implements a special feature of 80486 behaviour.
 * Underflow will be signalled even if the number is
 * not a denormal after rounding.
 * This difference occurs only for masked underflow, and not
 * in the unmasked case.
 * Actual 80486 behaviour differs from this in some circumstances.
 *)
              (* Will be masked underflow *)
              {$else}
          (* No longer a denormal *)
              {$endif}
        end else
            {$ifndef PECULIAR_486}
        begin
              {$endif}
          Dec(x^.exp);

//          if (FPU_bits_lost) <> 0 then
//          begin
//            (* There must be a masked underflow *)
//            //Exception(EX_Underflow); manca
//          end;

          tag := TAG_Special;
              {$ifndef PECULIAR_486}
        end;
          {$endif}
      end;
    end;
  end;

  { !!! manca perchè non ci sono ancora le funzioni di errors.c}
  if FPU_bits_lost = LOST_UP then
    set_precision_flag_up()
  else
    if FPU_bits_lost = LOST_DOWN then
      set_precision_flag_down();

  if x^.exp >= EXP_OVER then
  begin
    Inc(x^.exp, EXTENDED_Ebias);
    tag := arith_round_overflow(x, sign);
  end else
  begin
    Inc(x^.exp, EXTENDED_Ebias);
    x^.exp := x^.exp and $7fff;
  end;

  if (sign <> SIGN_POS) then
    x^.exp := x^.exp or $8000;

  Result := tag;
end;

procedure reg_copy (x: PFPU_REG; y: PFPU_REG);
var
//    src, dest: PChar;
  I: integer;
begin
  y^.exp := x^.exp;
{src:=pchar(x);
dest:=pchar(y);
if Cardinal(Dest) > Cardinal(Src) then
  for I := sizeof(int64)-1 downto 0 do
    dest[I] := src[I]
else
  for I := 0 to sizeof(int64)-1 do
    dest[I] := src[I];}
  significand(y)^ := significand(x)^;
  //PU64(@y^.SIGH)^:=PU64(@X^.SIGH)^;
end;

function FPU_normalize_nuo (x: PFPU_REG; bias: integer): intg;
begin
  if (x^.sigh and $8000) = 0 then
  begin
    if x^.sigh = 0 then
    begin
      if x^.sigl = 0 then
      begin
        x^.exp := EXP_UNDER;
        Exit(TAG_Zero);
      end;
      x^.sigh := x^.sigl;
      x^.sigl := 0;
      x^.exp  := x^.exp - 32;
    end;

    while (x^.sigh and $8000) = 0 do
    begin
      x^.sigh := x^.sigh shl 1;
      if (x^.sigl and $8000) <> 0 then
        x^.sigh := x^.sigh or 1;
      x^.sigl := x^.sigl shl 1;
      Dec(x^.exp);
    end;
  end;
  x^.exp := x^.exp + bias;
  Result := TAG_Valid;
end;

function FPU_u_sub (arg1: PFPU_REG; arg2: PFPU_REG; dest: PFPU_REG;
  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;
var
  shifted, answ: FPU_REG;
  extent: u32;
  ediff, ed2, borrow: intg;
begin
  ediff := expa - expb;
//{$ifdef PARANOID}
//  if ( ediff < 0 )
//    begin
//      EXCEPTION(EX_INTERNAL|$206);
//      return -1;
//    end;
//{$endif}
  answ.exp := expa;
//{$ifdef PARANOID}
//  if ( !(arg1^.sigh  and $8000) or !(arg2^.sigh  and $8000) )
//    begin
//      EXCEPTION(EX_INTERNAL|$209);
//      return -1;
//    end;
//{$endif}

  if ediff = 0 then
  begin
    shifted.sigl := arg2^.sigl;
    shifted.sigh := arg2^.sigh;
    extent := 0;
  end else
  if ediff < 32 then
  begin
    ed2 := 32 - ediff;
    extent := arg2^.sigl shl ed2;
    shifted.sigl := arg2^.sigl shr ediff;
    shifted.sigl := shifted.sigl or (arg2^.sigh shl ed2);
    shifted.sigh := arg2^.sigh shr ediff;
  end else
  if (ediff < 64) then
  begin
    Dec(ediff, 32);
    if ediff = 0 then
    begin
      extent := arg2^.sigl;
      shifted.sigl := arg2^.sigh;
      shifted.sigh := 0;
    end else
    begin
      ed2 := 32 - ediff;
      extent := arg2^.sigl shr ediff;
      extent := extent or (arg2^.sigh shl ed2);
      if (arg2^.sigl shl ed2) <> 0 then
        extent := extent or 1;
      shifted.sigl := arg2^.sigh shr ediff;
      shifted.sigh := 0;
    end;
  end else
  begin
    Dec(ediff, 64);
    if ediff = 0 then
    begin
      extent := arg2^.sigh;
      if (arg2^.sigl) <> 0 then
        extent := extent or 1;
      shifted.sigl := 0;
      shifted.sigh := 0;
    end else
    begin
      if ediff < 32 then
      begin
        extent := arg2^.sigh shr ediff;
        if ((arg2^.sigl <> 0) or ((arg2^.sigh shl (32 - ediff)) <> 0)) then
          extent := extent or 1;
      end else
        extent := 1;
      shifted.sigl := 0;
      shifted.sigh := 0;
    end;
  end;

  extent := -extent;
  borrow := extent;
  answ.sigl := arg1^.sigl - shifted.sigl;

  if (answ.sigl > arg1^.sigl) then
  begin
    if (borrow) <> 0 then
      Dec(answ.sigl);
    borrow := 1;
  end else
    if (borrow) <> 0 then
    begin
      Dec(answ.sigl);
      if (answ.sigl <> $ffffffff) then
        borrow := 0;
    end;

  answ.sigh := arg1^.sigh - shifted.sigh;

  if answ.sigh > arg1^.sigh then
  begin
    if (borrow) <> 0 then
      Dec(answ.sigh);
    borrow := 1;
  end else
    if (borrow) <> 0 then
    begin
      Dec(answ.sigh);
      if (answ.sigh <> $ffffffff) then
        borrow := 0;
    end;

//{$ifdef PARANOID}
//  if ( borrow )
//    begin
//      (* This can only occur if the code is bugged *)
//      EXCEPTION(EX_INTERNAL|$212);
//      return -1;
//    end;
//{$endif}

  if (answ.sigh and $80000000) <> 0 then
  begin
    (*
The simpler '*dest := answ' is broken in gcc
    *)
    dest^.exp := answ.exp;
    dest^.sigh := answ.sigh;
    dest^.sigl := answ.sigl;
    Result := FPU_round(dest, extent, 0, control_w, sign);
    exit;
  end;

  if answ.sigh = 0 then
  begin
    if answ.sigl <> 0 then
    begin
      answ.sigh := answ.sigl;
      answ.sigl := extent;
      extent := 0;
      Dec(answ.exp, 32);
    end else
    if extent <> 0 then
    begin
(*
*   A rare case, the only one which is non-zero if we got here
*         is:           1000000 .... 0000
*                      -0111111 .... 1111 1
*                       --------------------
*                       0000000 .... 0000 1
*)
      (* This can only occur if the code is bugged *)
      //EXCEPTION(EX_INTERNAL|$210); manca
      if extent <> $8000 then
        exit(-1);

      dest^.sigh := extent;
      extent := 0;
      dest^.sigl := extent;
      Dec(dest^.exp, 64);
      Result := FPU_round(dest, extent, 0, control_w, sign);
    end else
    begin
      dest^.exp := 0;
      dest^.sigl := 0;
      dest^.sigh := dest^.sigl;
      Result := TAG_Zero;
      Exit;
    end;
  end;

  while ((answ.sigh and $80000000) = 0) do
  begin
    answ.sigh := answ.sigh shl 1;
    if (answ.sigl and $80000000) <> 0 then
      answ.sigh := answ.sigh or 1;
    answ.sigl := answ.sigl shl 1;
    if (extent and $80000000) <> 0 then
      //!!! dacci una occhiata al codice con l'originale
      answ.sigl := answ.sigl or 1;
    extent := extent shl 1;
    Dec(answ.exp);
  end;

  dest^.exp  := answ.exp;
  dest^.sigh := answ.sigh;
  dest^.sigl := answ.sigl;

  Result := FPU_round(dest, extent, 0, control_w, sign);
end;

function FPU_u_mul (a: PFPU_REG; b: PFPU_REG; c: PFPU_REG; cw: u16;
  sign: u_char; expon: s32): intg;
var
  mu, ml, mi: u64;
  lh, ll, th, tl: u32;
begin

//{$ifdef PARANOID}
//  if ( ! (a^.sigh  and $8000) or ! (b^.sigh  and $8000) )
//    begin
//      EXCEPTION(EX_INTERNAL|$205);
//    end;
//{$endif}

  ml := a^.sigl;
  ml := ml * b^.sigl;
  ll := ml;
  lh := ml shr 32;

  mu := a^.sigh;
  mu := mu * b^.sigh;

  mi := a^.sigh;
  mi := mi * b^.sigl;
  tl := mi;
  th := mi shr 32;
  lh := lh + tl;
  if (tl > lh) then
    Inc(mu);
  Inc(mu, th);

  mi := a^.sigl;
  mi := mi * b^.sigh;
  tl := mi;
  th := mi shr 32;
  lh := lh + tl;
  if (tl > lh) then
    Inc(mu);
  mu := mu + th;

  ml := lh;
  ml := ml shl 32;
  ml := ml + ll;

  Dec(expon, EXP_BIAS - 1);
  if (expon <= EXP_WAY_UNDER) then
    expon := EXP_WAY_UNDER;

  c^.exp := expon;

  if ((mu and $800000000000) = 0) then
  begin
    mu := mu shl 1;
    if (ml and $800000000000) <> 0 then
      mu := mu or 1;
    ml := ml shl 1;
    Dec(c^.exp);
  end;

  ll := ml;
  lh := ml shr 32;

  if (ll) <> 0 then
    lh := lh or 1;

  c^.sigl := mu;
  c^.sigh := mu shr 32;

  Result := FPU_round(c, lh, 0, cw, sign);
end;

function FPU_u_div (a: PFPU_REG; b: PFPU_REG; dest: PFPU_REG;
  control_w: u16; sign: u_char): intg;
var
  exp:  s32;
  divr32, rem, rat1, rat2, work32, accum3, prodh: u32;
  work64, divr64, prod64, accum64: u64;
  ovfl: u8;
begin
  exp := s32(a^.exp) - s32(b^.exp);

  if exp < EXP_WAY_UNDER then
    exp := EXP_WAY_UNDER;

  dest^.exp := exp;
//{$ifdef PARANOID}
//  if ( !(b^.sigh  and $8000) )
//    begin
//      EXCEPTION(EX_INTERNAL|$202);
//    end;
//{$endif}

  work64 := significand(a)^;

(* We can save a lot of time if the divisor has all its lowest
   32 bits equal to zero. *)
  if b^.sigl = 0 then
  begin
    divr32 := b^.sigh;
    ovfl := u8(a^.sigh >= divr32);
    rat1 := work64 div divr32;
    rem  := work64 mod divr32;
    work64 := rem;
    work64 := work64 shl 32;
    rat2 := work64 div divr32;
    rem  := work64 mod divr32;

    work64 := rem;
    work64 := work64 shl 32;
    rem := work64 div divr32;

    if ovfl <> 0 then
    begin
      rem := rem shr 1;
      if (rat2 and 1) <> 0 then
        rem := rem or $8000;
      rat2 := rat2 shr 1;
      if (rat1 and 1) <> 0 then
        rat2 := rat2 or $8000;
      rat1 := rat1 shr 1;
      rat1 := rat1 or $8000;
      Inc(dest^.exp);
    end;
    dest^.sigh := rat1;
    dest^.sigl := rat2;

    Dec(dest^.exp);
    Result := FPU_round(dest, rem, 0, control_w, sign);
    Exit;
  end;

  (* This may take a little time... *)

  accum64 := work64;
  divr64 := significand(Pfpu_reg(b))^;
  ovfl := u8(accum64 >= divr64);
  if ovfl <> 0 then
    Dec(accum64, divr64);
  divr32 := b^.sigh + 1;

  if divr32 <> 0 then
    rat1 := accum64 div divr32
  else
    rat1 := accum64 shr 32;

  prod64 := rat1 * u64(b^.sigh);

  Dec(accum64, prod64);
  prod64 := rat1 * u64(b^.sigl);
  accum3 := prod64;
  if (accum3) <> 0 then
    begin
    accum3 := -accum3;
    Dec(accum64);
    end;
  prodh := prod64 shr 32;
  Dec(accum64, prodh);

  work32 := accum64 shr 32;
  if work32 <> 0 then
  begin
//{$ifdef PARANOID}
//      if ( work32 !:= 1 )
//	begin
//	  EXCEPTION(EX_INTERNAL|$203);
//	end;
//{$endif}

    (* Need to subtract the divisor once more. *)
    work32 := accum3;
    accum3 := work32 - b^.sigl;
    if (accum3 > work32) then
      Dec(accum64);
    Inc(rat1);
    Dec(accum64, b^.sigh);

//{$ifdef PARANOID}
//      if ( (accum64 shr 32) )
//	begin
//	  EXCEPTION(EX_INTERNAL|$203);
//	end;
//{$endif}
  end;

(* Now we essentially repeat what we have just done, but shifted
   32 bits. *)

  accum64 := accum64 shl 32;
  accum64 := accum64 or accum3;

  if accum64 >= divr64 then
  begin
    Dec(accum64, divr64);
    Inc(rat1);
  end;

  if divr32 <> 0 then
    rat2 := accum64 div divr32
  else
    rat2 := accum64 shr 32;
  prod64 := rat2 * u64(b^.sigh);

  Dec(accum64, prod64);
  prod64 := rat2 * u64(b^.sigl);
  accum3 := prod64;
  if (accum3) <> 0 then
  begin
    accum3 := -accum3;
    Dec(accum64);
  end;
  prodh := prod64 shr 32;
  Dec(accum64, prodh);

  work32 := accum64 shr 32;
  if (work32) <> 0 then
  begin
//{$ifdef PARANOID}
//      if ( work32 !:= 1 )
//  begin
//    EXCEPTION(EX_INTERNAL|$203);
//  end;
//{$endif}

    (* Need to subtract the divisor once more. *)
    work32 := accum3;
    accum3 := work32 - b^.sigl;
    if (accum3 > work32) then
      Dec(accum64);
    Inc(rat2);
    if (rat2 = 0) then
      Inc(rat1);
    Dec(accum64, b^.sigh);

//{$ifdef PARANOID}
//      if ( (accum64 shr 32) )
//	begin
//	  EXCEPTION(EX_INTERNAL|$203);
//	end;
//{$endif}
  end;

  (* Tidy up the remainder *)

  accum64 := accum64 shl 32;
  accum64 := accum64 or accum3;
  if accum64 >= divr64 then
  begin
    Dec(accum64, divr64);
    Inc(rat2);
    if rat2 = 0 then
    begin
      Inc(rat1);
//{$ifdef PARANOID}
//	  (* No overflow should be possible here *)
//	  if ( rat1 = 0 )
//	    begin
//	      EXCEPTION(EX_INTERNAL|$203);
//	    end;
//{$endif}
    end;

  end;

(* The basic division is done, now we must be careful with the
   remainder. *)

  if (ovfl) <> 0 then
  begin
    if (rat2 and 1) <> 0 then
      rem := $8000
    else
      rem := 0;
    rat2 := rat2 shr 1;
    if (rat1 and 1) <> 0 then
      rat2 := rat2 or $8000;
    rat1 := rat1 shr 1;
    rat1 := rat1 or $8000;

    if (accum64) <> 0 then
      rem := rem or $ff0000;

    Inc(dest^.exp);
  end else
  begin
    (* Now we just need to know how large the remainder is
 relative to half the divisor. *)
    if (accum64 = 0) then
      rem := 0
    else
    begin
      accum3 := accum64 shr 32;
      if (accum3 and $8000) <> 0 then
        (* The remainder is definitely larger than 1/2 divisor. *)
        rem := $ff000000
      else
      begin
        accum64 := accum64 shl 1;
        if accum64 >= divr64 then
        begin
          Dec(accum64, divr64);
          if (accum64 = 0) then
            rem := $8000
          else
            rem := $ff000000;
        end else
          rem := $7f000000;
      end;
    end;
  end;

  dest^.sigh := rat1;
  dest^.sigl := rat2;

  Dec(dest^.exp);
  Result := FPU_round(dest, rem, 0, control_w, sign);
end;

//function FPU_u_add (arg1: PFPU_REG; arg2: PFPU_REG; answ: PFPU_REG;
//  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;
//  var
//    rtmp: PFPU_REG;
//    shifted: FPU_REG;
//    extent: u32;
//    ediff, ed2, eflag, ovfl, carry: intg;
//  begin
//    ediff  := expa - expb;
//    extent := 0;
//    if (ediff < 0) then
//      begin
//      ediff := -ediff;
//      rtmp  := arg1;
//      arg1  := arg2;
//      arg2  := rtmp;
//      expa  := expb;
//      end;
//
//    (* Now we have exponent of arg1 >= exponent of arg2 *)
//
//    answ^.exp := expa;
//
////{$ifdef PARANOID}
////  if ( !(arg1^.sigh  and $8000) or !(arg2^.sigh  and $8000) )
////    begin
////      EXCEPTION(EX_INTERNAL|$201);
////      return -1;
////    end;
////{$endif}
//
//    if (ediff = 0) then
//      begin
//      extent := 0;
//      shifted.sigl := arg2^.sigl;
//      shifted.sigh := arg2^.sigh;
//      end
//    else
//      if (ediff < 32) then
//        begin
//        ed2 := 32 - ediff;
//        extent := arg2^.sigl shl ed2;
//        shifted.sigl := arg2^.sigl shr ediff;
//        shifted.sigl := shifted.sigl or (arg2^.sigh shl ed2);
//        shifted.sigh := arg2^.sigh shr ediff;
//        end
//      else
//        if (ediff < 64) then
//          begin
//          Dec(ediff, 32);
//          if (ediff) = 0 then
//            begin
//            eflag  := 0;
//            extent := arg2^.sigl;
//            shifted.sigl := arg2^.sigh;
//            end
//          else
//            begin
//            ed2 := 32 - ediff;
//            eflag := arg2^.sigl;
//            if (eflag) <> 0 then
//              extent := extent or 1;
//            extent := arg2^.sigl shr ediff;
//            extent := extent or (arg2^.sigh shl ed2);
//            shifted.sigl := arg2^.sigh shr ediff;
//            end;
//          shifted.sigh := 0;
//          end
//        else
//          begin
//          Dec(ediff, 64);
//          if (ediff) = 0 then
//            begin
//            eflag  := arg2^.sigl;
//            extent := arg2^.sigh;
//            end
//          else
//            begin
//            ed2 := 64 - ediff;
//            eflag := arg2^.sigl or arg2^.sigh;
//            extent := arg2^.sigh shr ediff;
//            end;
//          shifted.sigl := 0;
//          shifted.sigh := 0;
//          if (eflag) <> 0 then
//            extent := extent or 1;
//          end;
//
//    answ^.sigh := arg1^.sigh + shifted.sigh;
//    ovfl := u8(shifted.sigh > answ^.sigh);
//    answ^.sigl := arg1^.sigl + shifted.sigl;
//    if (shifted.sigl > answ^.sigl) then
//      begin
//      Inc(answ^.sigh);
//      if (answ^.sigh = 0) then
//        ovfl := 1;
//      end;
//    if (ovfl) <> 0 then
//      begin
//      carry  := extent and 1;
//      extent := extent shr 1;
//      extent := extent or carry;
//      if (answ^.sigl and 1) <> 0 then
//        extent := extent or $8000;
//      answ^.sigl := answ^.sigl shr 1;
//      if (answ^.sigh and 1) <> 0 then
//        answ^.sigl := answ^.sigl or $8000;
//      answ^.sigh := answ^.sigh shr 1;
//      answ^.sigh := answ^.sigh or $8000;
//      Inc(answ^.exp);
//      end;
//
//    Result := FPU_round(answ, extent, 0, control_w, sign);
//  end;

function wm_sqrt (n: PFPU_REG; dummy1: intg; dummy2: intg; control_w: u16;
  sign: u_char): intg;
var
  nn, guess, halfn, lowr, mid, upr, diff, uwork: u64;
  work: s64;
  ne, guess32, work32, diff32, mid32: u32;
  shifted: intg;
begin
  nn := significand(Pfpu_reg(n))^;
  ne := 0;
  if (exponent16(Pfpu_reg(n)) = EXP_BIAS) then
  begin
    (* Shift the argument right one position. *)
    if (nn and 1) <> 0 then
      ne := $8000;
    nn := nn shr 1;
    guess := n^.sigh shr 2;
    shifted := 1;
  end else
  begin
    guess := n^.sigh shr 1;
    shifted := 0;
  end;

  Inc(guess, $40000000);
  guess := guess * $aaaaaaaa;
  guess := guess shl 1;
  guess32 := guess shr 32;
  if ((guess32 and $8000)) = 0 then
    guess32 := $8000;
  halfn := nn shr 1;

  guess32 := halfn div guess32 + (guess32 shr 1);
  guess32 := halfn div guess32 + (guess32 shr 1);
  guess32 := halfn div guess32 + (guess32 shr 1);

(*
* Now that an estimate accurate to about 30 bits has been obtained,
* we improve it to 60 bits or so.
*
* The strategy from now on is to compute new estimates from
*      guess ::= guess + (n - guess^2) / (2 * guess)
*)

  work := guess32;
  work := nn - work * guess32;
  work := work shl 28;       (* 29 - 1 *)
  work := work div guess32;
  work := work shl 3;        (* 29 + 3 := 32 *)
  Inc(work, u64(guess32) shl 32);

  if (work = 0) then  (* This happens in one or two special cases *)
    work := ($ffffffffffffffff);

  guess := work;

  (* guess is now accurate to about 60 bits *)


  if (work > 0) then
  begin
//{$ifdef PARANOID}
//      if ( (n^.sigh !:= $ffffffff) @ and (n^.sigl !:= $ffffffff) )
//	begin
//	  EXCEPTION(EX_INTERNAL|$213);
//	end;
//{$endif}
    (* We know the answer here. *)
    Result := FPU_round(n, $7fffffff, 0, control_w, sign);
    Exit;
  end;

  (* Refine the guess to significantly more than 64 bits. *)

  (* First, square the current guess. *)

  guess32 := guess shr 32;
  work32  := guess;

  (* lower 32 times lower 32 *)
  lowr := work32;
  lowr := lowr * work32;

  (* lower 32 times upper 32 *)
  mid := guess32;
  mid := mid * work32;

  (* upper 32 times upper 32 *)
  upr := guess32;
  upr := upr * guess32;

  (* upper 32 bits of the middle product times 2 *)
  Inc(Upr, mid shr (32 - 1));

  (* lower 32 bits of the middle product times 2 *)
  work32 := mid shl 1;

  (* upper 32 bits of the lower product *)
  mid32 := lowr shr 32;
  mid32 := mid32 + work32;
  if (mid32 < work32) then
    Inc(upr);

  (* We now have the first 96 bits (truncated) of the square of the guess *)

  diff := upr - nn;
  diff32 := mid32 - ne;
  if (diff32 > mid32) then
    Dec(diff);

  if (s64(diff) < 0) then
  begin
    (* The difference is negative, negate it. *)
    diff32 := -s32(diff32);
    diff := not diff;
    if (diff32 = 0) then
      Inc(diff);
//{$ifdef PARANOID}
//      if ( (diff shr 32) !:= 0 )
//	begin
//	  EXCEPTION(EX_INTERNAL|$207);
//	end;
//{$endif}

    diff := diff shl 32;
    diff := diff or diff32;
    work32 := diff div guess32;
    work := work32;
    work := work shl 32;

    diff := diff mod guess32;
    diff := diff shl 32;
    work32 := diff div guess32;

    work := work or work32;

    work := work shr 1;
    work32 := work shr 32;

    Inc(guess, work32);
    guess32 := work;        (* The next 32 bits *)
    (* The guess should now be good to about 90 bits *)
  end else
  begin
    (* The difference is positive. *)
    diff := diff shl 32;
    diff := diff or diff32;

    work32 := diff div guess32;
    work := work32;
    work := work shl 32;

    diff := diff mod guess32;
    diff := diff shl 32;
    work32 := diff div guess32;

    work := work or work32;

    work := work shr 1;
    work32 := work shr 32;

    guess32 := work;        (* The last 32 bits (of 96) *)
    guess32 := -guess32;
    if (guess32) <> 0 then
      Dec(guess);
    Dec(guess, work32);       (* The first 64 bits *)
    (* The guess should now be good to about 90 bits *)
  end;


  n^.exp := 0;

  if (guess32 >= u32(-ERR_MARGIN)) then
    (* Nearly exact, we round the 64 bit result upward. *)
    Inc(guess)
  else
  if ((guess32 > ERR_MARGIN) and ((guess32 < $8000 - ERR_MARGIN) or
    (guess32 > $8000 + ERR_MARGIN))) then
  begin
    (* We have enough accuracy to decide rounding *)
    n^.sigh := guess; // significand(Pfpu_reg(n)) := guess;
    Result  := FPU_round(n, guess32, 0, control_w, sign);
    Exit;
  end;

  if ((guess32 <= ERR_MARGIN) or (guess32 >= u32(-ERR_MARGIN))) then
  begin
    (*
     * This is an easy case because x^1/2 is monotonic.
     * We need just find the square of our estimate, compare it
     * with the argument, and deduce whether our estimate is
     * above, below, or exact. We use the fact that the estimate
     * is known to be accurate to about 90 bits.
     *)


    (* We compute the lower 64 bits of the 128 bit product *)
    work32 := guess;
    lowr := work32;
    lowr := lowr * work32;

    uwork  := guess shr 32;
    work32 := guess;
    uwork  := uwork * work32;
    uwork  := uwork shl 33;   (* 33 := 32+1 (for two times the product) *)

    Inc(lowr, uwork);

    (* We need only look at bits 65..96 of the square of guess. *)
    if (shifted) <> 0 then
      work32 := lowr shr 31
    else
      work32 := lowr shr 32;

//{$ifdef PARANOID}
//      if ( ((s32)work32 > 3*ERR_MARGIN) or ((s32)work32 < -3*ERR_MARGIN) )
//	begin
//	  EXCEPTION(EX_INTERNAL|$214);
//	end;
//{$endif}

    n^.sigh := guess;
    if (s32(work32) > 0) then
    begin
      (* guess is too large *)
      Dec(n^.sigh);
      Result := FPU_round(n, $ffffff00, 0, control_w, sign);
      Exit;
    end else
    if (s32(work32) < 0) then
    begin
      (* guess is a little too small *)
      Result := FPU_round(n, $000000ff, 0, control_w, sign);
      Exit;
    end else
    if (u32(lowr) <> 0) then
    begin

      (* guess is too large *)
      Dec(n^.sigh);
      Result := FPU_round(n, $ffffff00, 0, control_w, sign);
      Exit;
    end;

    (* Our guess is precise. *)
    Result := FPU_round(n, 0, 0, control_w, sign);
    Exit;
  end;

(* Very similar to the case above, but the last bit is near 0.5.
   We handle this just like the case above but we shift everything
   by one bit. *)


  uwork := guess;
  uwork := uwork shl 1;
  uwork := uwork or 1;      (* add the half bit *)

  (* We compute the lower 64 bits of the 128 bit product *)
  work32 := uwork;
  lowr := work32;
  lowr := lowr * work32;

  work32 := uwork shr 32;
  uwork  := uwork and $ffffffff;
  uwork  := uwork * work32;
  uwork  := uwork shl 33;   (* 33 := 32+1 (for two times the product) *)

  Inc(lowr, uwork);

(* We now have the 64 bits. The lowest 32 bits of lowr
                  are not all zero (the lsb is 1). *)

  (* We need only look at bits 65..96 of the square of guess. *)
  if (shifted) <> 0 then
    work32 := lowr shr 31
  else
    work32 := lowr shr 32;

//{$ifdef PARANOID}
//  if ( ((s32)work32 > 4*3*ERR_MARGIN) or ((s32)work32 < -4*3*ERR_MARGIN) )
//    begin
//      EXCEPTION(EX_INTERNAL|$215);
//    end;
//{$endif}

  n^.sigh := guess;
  if (s32(work32) < 0) then
  begin
    (* guess plus half bit is a little too small *)
    Result := FPU_round(n, $800000ff, 0, control_w, sign);
    Exit;
  end else
  (* Note that the lower 64 bits of the product are not all zero *)
  begin
    (* guess plus half bit is too large *)
    Result := FPU_round(n, $7fffff00, 0, control_w, sign);
    Exit;
  end;
(*
  Note that the result of a square root cannot have precisely a half bit
  of a least significant place (it is left as an exercise for the reader
  to prove this! (hint: 65 bit*65 bit :=> n bits)).
*)
end;

function FPU_shrx (arg1: pointer; arg2: u32): intg;
var
  x: u32;
begin
  if arg2 >= 64 then
  begin
    if arg2 >= 96 then
    begin
      pu64(arg1)^ := 0;
      Exit(0);
    end;
    Dec(arg2, 64);
    x := pu64(arg1)^ shr 32;
    pu64(arg1)^ := 0;

    if arg2 <> 0 then
      exit(x shr arg2)
    else
      Exit(x);
  end;

  if (arg2 < 32) then
  begin
    if (arg2 = 0) then
      Exit(0);

    x := pu64(arg1)^ shl (32 - arg2);
  end else
    if (arg2 > 32) then
      x := pu64(arg1)^ shr (arg2 - 32)
    else
      (* arg2 = 32 *)
      x := pu64(arg1)^;

  pu64(arg1)^ := pu64(arg1)^ shr arg2;

  Result := x;
end;

function FPU_shrxs (arg1: pointer; arg2: u32): intg;
var
  x, bits: u32;
  lost: u64;
  v: int64;
begin
  if (arg2 >= 64) then
  begin
    if (arg2 >= 96) then
    begin
      bits := u32(pu64(arg1)^ <> 0);
      pu64(arg1)^ := 0;
      Exit(ifthen(bits <> 0, 1, 0));
    end;
    Dec(arg2, 64);
    lost := pu64(arg1)^ shl (32 - arg2);
    x := pu64(arg1)^ shr 32;
    pu64(arg1)^ := 0;

    if (arg2) <> 0 then
      x := x shr arg2;

    if (lost) <> 0 then
      x := x or 1;

    Result := x;
  end;

  if (arg2 < 32) then
  begin
    if (arg2 = 0) then
      (* No bits are lost *)
      Exit(0);

    (* No bits are lost *)
    x := pu64(arg1)^ shl (32 - arg2);
  end else
  if (arg2 > 32) then
  begin
    bits := pu64(arg1)^;
    bits := bits shl (64 - arg2);
    x := pu64(arg1)^ shr (arg2 - 32);
    if (bits) <> 0 then
      x := x or 1;
  end else
  begin
    (* arg2 = 32 *)
    (* No bits are lost *)
    x := pu64(arg1)^;
  end;

  v := pu64(arg1)^;
  v := v shr arg2;
  pu64(arg1)^ := (v * -1) + 2;

  if (x and $7fffffff) <> 0 then
    x := x or 1;

  Result := x;
end;

function FPU_div_small (x: pu64; y: u32): intg;
var
  retval: u32;
begin
  retval := x^ mod y;
  x^ := x^ div y;
  Result := retval;
end;

//function exponent (x: Pfpu_reg): longword;
//  begin
//    Result := (x^.exp and $7fff) - EXTENDED_Ebias;
//  end;

function top: u32;
begin
  Result := varI387.soft.ftop;
end;

function no_ip_update: u_char;
begin
  Result := varI387.soft.no_update;
end;

function register_base: pu_char;
begin
  Result := pu_char(parray64(@varI387.soft.st_space));
end;

function fpu_register (index: integer): PFPU_REG;
begin
  Result := PFPU_REG(longint(register_base) + (sizeof(FPU_REG) * index));
end;

procedure setcc (cc: u32);
begin
  varI387.soft.swd := varI387.soft.swd and not (SW_C0 or SW_C1 or SW_C2 or SW_C3);
  varI387.soft.swd := varI387.soft.swd or cc and (SW_C0 or SW_C1 or SW_C2 or SW_C3);
end;

procedure poppop;
begin
  FPU_pop;
  FPU_pop;
end;

procedure FPU_illegal;
begin
  math_abort(nil, 0);
end;

procedure pop_0 ();
begin
  FPU_settag0(TAG_Empty);
  Inc(varI387.soft.ftop);
end;

function status_word: intg;
begin
  Result := ((varI387.soft.swd and not SW_Top and $ffff) or
    ((top shl SW_Top_Shift) and SW_Top));
end;

procedure push;
begin
  Inc(varI387.soft.ftop);
end;

procedure add_two_Xsig (dest: PXsig; const x2: PXsig; exp: ps32);
var
  ovfl: intg;
begin
  ovfl := 0;

  dest^.lsw := dest^.lsw + x2^.lsw;
  if (dest^.lsw < x2^.lsw) then
  begin
    Inc(dest^.midw);
    if (dest^.midw = 0) then
    begin
      Inc(dest^.msw);
      if (dest^.msw = 0) then
        ovfl := 1;
    end;
  end;
  dest^.midw := dest^.midw + x2^.midw;
  if (dest^.midw < x2^.midw) then
  begin
    Inc(dest^.msw);
    if (dest^.msw = 0) then
      ovfl := 1;
  end;
  dest^.msw := dest^.msw + x2^.msw;
  if (dest^.msw < x2^.msw) then
    ovfl := 1;
  if (ovfl) <> 0 then
  begin
    Inc(exp^);
    dest^.lsw := dest^.lsw shr 1;
    if (dest^.midw and 1) <> 0 then
      dest^.lsw := dest^.lsw or $8000;
    dest^.midw := dest^.midw shr 1;
    if (dest^.msw and 1) <> 0 then
      dest^.midw := dest^.midw or $8000;
    dest^.msw := dest^.msw shr 1;
    dest^.msw := dest^.msw or $8000;
  end;
end;

procedure negate_Xsig (x: PXsig);
begin
  x^.lsw  := not x^.lsw;
  x^.midw := not x^.midw;
  x^.msw  := not x^.msw;
  Inc(x^.lsw);
  if (x^.lsw = 0) then
  begin
    Inc(x^.midw);
    if (x^.midw = 0) then
      Inc(x^.msw);
  end;
end;

function XSIG_LL (x: Xsig): pu64;
begin
  Result := pu64(x.msw);
end;

function LL_MSW (x: Pointer): pu64;
begin
  Result := pu64(x);
end;

procedure add_Xsig_Xsig (dest: PXsig; const x2: PXsig);
begin
  dest^.lsw := dest^.lsw + x2^.lsw;
  if (dest^.lsw < x2^.lsw) then
  begin
    Inc(dest^.midw);
    if (dest^.midw = 0) then
      Inc(dest^.msw);
  end;
  dest^.midw := dest^.midw + x2^.midw;
  if (dest^.midw < x2^.midw) then
    Inc(dest^.msw);
  dest^.msw := dest^.msw + x2^.msw;
end;

function mul_32_32 (const arg1: u32; const arg2: u32): u32;
begin
  Result := (u64(arg1) * arg2) shr 32;
end;

function access_limit: pu_char;
begin
  Result := pu_char(@varI387.soft.alimit);
end;

function FPU_add (b: PFPU_REG; tagb: u_char; deststnr: intg; control_w: word): intg;
var
  a: PFPU_REG;
  dest: PFPU_REG;
  signb: u_char;
  taga: u_char;
  signa: u_char;
  saved_sign: u_char;
  diff, tag, expa, expb: intg;
  x, y: FPU_REG;

label
  valid_add;
begin
  a := st(0);
  dest := st(deststnr);
  signb := getsign(b);
  taga := FPU_gettag0();
  signa := getsign(a);
  saved_sign := getsign(dest);

  if ((taga or tagb) = 0) then
  begin
    expa := exponent(a);
    expb := exponent(b);

    valid_add:
      (* Both registers are valid *)
      if ((signa or signb) = 0) then
      begin
        (* signs are the same *)
        tag := FPU_u_add(a, b, dest, control_w, signa, expa, expb);
      end else
      begin
        (* The signs are different, so do a subtraction *)
        diff := expa - expb;
        if (diff) = 0 then
        begin
          diff := a^.sigh - b^.sigh;  (* This works only if the ms bits
            are identical. *)
          if (diff) = 0 then
          begin
            diff := word(a^.sigl > b^.sigl);
            if (diff) = 0 then
              diff := -word(a^.sigl < b^.sigl);
          end;
        end;

        if (diff > 0) then
          tag := FPU_u_sub(a, b, dest, control_w, signa, expa, expb)
        else
        if (diff < 0) then
          tag := FPU_u_sub(b, a, dest, control_w, signb, expb, expa)
        else
        begin
          FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);
          (* sign depends upon rounding mod_e *)
          setsign(dest, ifthen((control_w and CW_RC) <> RC_DOWN,
                  SIGN_POS, SIGN_NEG));
          Exit(TAG_Zero);
        end;
      end;

    if (tag < 0) then
    begin
      setsign(dest, saved_sign);
      Exit(tag);
    end;
    FPU_settagi(deststnr, tag);
    Exit(tag);
  end;

  if (taga = TAG_Special) then
    taga := FPU_Special(a);

  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);

  if (taga = TAG_Valid) and (tagb = TW_Denormal) or (taga = TW_Denormal) and
    (tagb = TAG_Valid) or (taga = TW_Denormal) and (tagb = TW_Denormal) then
  begin
    if (denormal_operand() < 0) then
      Exit(FPU_Exception_c);

    FPU_to_exp16(a, @x);
    FPU_to_exp16(b, @y);
    a := @x;
    b := @y;
    expa := exponent16(a);
    expb := exponent16(b);
    goto valid_add;
  end;

  if ((taga = TW_NaN) or (tagb = TW_NaN)) then
    if (deststnr = 0) then
      Exit(real_2op_NaN(b, tagb, deststnr, a))
    else
      Exit(real_2op_NaN(a, taga, deststnr, a));

  Result := add_sub_specials(a, taga, signa, b, tagb, signb, dest,
    deststnr, control_w);
end;
(* Subtract b from a.  (a-b) ^. dest
 bbd: arg2 used to be int type, but sometimes pointers were forced
 in with typecasts.  On Alphas pointers are 64 bits and ints are 32,
 so when rm was cast back to a pointer...SEGFAULT.  Pass the pointers
 around instead, since they are always larger precision than the
 register numbers. *)
function FPU_sub (flags: intg; rm: PFPU_REG; control_w: u16): intg;
var
  a, b:  PFPU_REG;
  dest:  PFPU_REG;
  taga, tagb, signa, signb, saved_sign, sign: u_char;
  diff, tag, expa, expb, deststnr: intg;
  x, y:  FPU_REG;
  d1, d2: FPU_REG;
  rmint: intg;

label
  valid_subtract;
begin
  a := st(0);
  taga := FPU_gettag0();

  deststnr := 0;
  if (flags and LOADED) <> 0 then
  begin
    b := rm;
    tagb := flags and $0f;
  end else
  begin
    rmint := bx_ptr_equiv_t(rm);
    b := st(rmint);
    tagb := FPU_gettagi(rmint);

    if (flags and DEST_RM) <> 0 then
      deststnr := rmint;
  end;

  signa := getsign(a);
  signb := getsign(b);

  if (flags and REV) <> 0 then
  begin
    signa := SIGN_NEG;
    signb := SIGN_NEG;
  end;

  dest := st(deststnr);
  saved_sign := getsign(dest);

  if ((taga or tagb)) = 0 then
  begin
    expa := exponent(a);
    expb := exponent(b);

    valid_subtract:
      (* Both registers are valid *)

    diff := expa - expb;

    if (diff) = 0 then
    begin
      diff := a^.sigh - b^.sigh;  (* Works only if ms bits are identical *)
      if (diff) = 0 then
      begin
        diff := word(a^.sigl > b^.sigl);
        if (diff) = 0 then
          diff := -word(a^.sigl < b^.sigl);
      end;
    end;

    case Trunc(((signa) * 2 + signb) / SIGN_NEG) of
      0, (* P - P *)
      3: (* N - N *)
      begin
        if (diff > 0) then
        begin
          (* |a| > |b| *)
          tag := FPU_u_sub(a, b, dest, control_w, signa, expa, expb);
        end else
        if (diff = 0) then
        begin
          FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);

          (* sign depends upon rounding mod_e *)
          setsign(dest, ifthen((control_w and CW_RC) <> RC_DOWN,
            SIGN_POS, SIGN_NEG));
          Exit(TAG_Zero);
        end else
        begin
          sign := signa or SIGN_NEG;
          tag  := FPU_u_sub(b, a, dest, control_w, sign, expb, expa);
        end;
      end;
      1: (* P - N *)
        tag := FPU_u_add(a, b, dest, control_w, SIGN_POS, expa, expb);
      2: (* N - P *)
        tag := FPU_u_add(a, b, dest, control_w, SIGN_NEG, expa, expb);
//{$ifdef PARANOID}
//default:
//  EXCEPTION(EX_INTERNAL|$111);
//  return -1;
//{$endif}
    end;
    if (tag < 0) then
    begin
      setsign(dest, saved_sign);
      Exit(tag);
    end;
    FPU_settagi(deststnr, tag);
    Exit(tag);
  end;

  if (taga = TAG_Special) then
    taga := FPU_Special(a);
  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);

  if (((taga = TAG_Valid) and (tagb = TW_Denormal)) or
    ((taga = TW_Denormal) and (tagb = TAG_Valid)) or
    ((taga = TW_Denormal) and (tagb = TW_Denormal))) then
  begin
    if (denormal_operand() < 0) then
      Exit(FPU_Exception_c);

    FPU_to_exp16(a, @x);
    FPU_to_exp16(b, @y);
    a := @x;
    b := @y;
    expa := exponent16(a);
    expb := exponent16(b);

    goto valid_subtract;
  end;

  if ((taga = TW_NaN) or (tagb = TW_NaN)) then
  begin
    if (flags and REV) <> 0 then
    begin
      d1 := b^;
      d2 := a^;
    end else
    begin
      d1 := a^;
      d2 := b^;
    end;
    if (flags and LOADED) <> 0 then
    begin
      Result := real_2op_NaN(b, tagb, deststnr, @d1);
      Exit;
    end;
    if (flags and DEST_RM) <> 0 then
    begin
      Result := real_2op_NaN(a, taga, deststnr, @d2);
      Exit;
    end else
    begin
      Result := real_2op_NaN(b, tagb, deststnr, @d2);
      Exit;
    end;
  end;

  Result := add_sub_specials(a, taga, signa, b, tagb, signb or
    SIGN_NEG, dest, deststnr, control_w);
end;

function add_sub_specials (a: PFPU_REG; taga: u_char; signa: u_char;
  b: PFPU_REG; tagb: u_char; signb: u_char; dest: PFPU_REG; deststnr: intg;
  control_w: u16): intg;
var
  different_signs: u_char;
begin
  if (((taga = TW_Denormal) or (tagb = TW_Denormal)) and (denormal_operand() < 0)) then
    Exit(FPU_Exception_c);

  if (taga = TAG_Zero) then
  begin
    if (tagb = TAG_Zero) then
    begin
      (* Both are zero, result will be zero. *)
      different_signs := signa or signb;

      FPU_copy_to_regi(a, TAG_Zero, deststnr);
      if (different_signs) <> 0 then
      begin
        (* Signs are different. *)
        (* Sign of answer depends upon rounding mod_e. *)
        setsign(dest, ifthen((control_w and CW_RC) <> RC_DOWN, SIGN_POS, SIGN_NEG));
      end else
        setsign(dest, signa);  (* signa may differ from the sign of a. *)

      Exit(TAG_Zero);
    end else
    begin
      reg_copy(b, dest);
      if ((tagb = TW_Denormal) and ((b^.sigh and $80000000) <> 0)) then
      begin
        (* A pseudoDenormal, convert it. *)
        addexponent(dest, 1);
        tagb := TAG_Valid;
      end else
        if (tagb > TAG_Empty) then
          tagb := TAG_Special;

      setsign(dest, signb);  (* signb may differ from the sign of b. *)
      FPU_settagi(deststnr, tagb);
      Exit(tagb);
    end;
  end else
  if (tagb = TAG_Zero) then
  begin
    reg_copy(a, dest);
    if ((taga = TW_Denormal) and ((a^.sigh and $80000000) <> 0)) then
    begin
      (* A pseudoDenormal *)
      addexponent(dest, 1);
      taga := TAG_Valid;
    end else
      if (taga > TAG_Empty) then
        taga := TAG_Special;

    setsign(dest, signa);  (* signa may differ from the sign of a. *)
    FPU_settagi(deststnr, taga);
    Exit(taga);
  end else
  if (taga = TW_Infinity) then
  begin
    if ((tagb <> TW_Infinity) or (signa = signb)) then
    begin
      FPU_copy_to_regi(a, TAG_Special, deststnr);
      setsign(dest, signa);  (* signa may differ from the sign of a. *)
      Exit(taga);
    end;
    (* Infinity-Infinity is undefined. *)
    Exit(arith_invalid(deststnr));
  end else
  if (tagb = TW_Infinity) then
  begin
    FPU_copy_to_regi(b, TAG_Special, deststnr);
    setsign(dest, signb);  (* signb may differ from the sign of b. *)
    Exit(tagb); // really needed?
  end;

//{$ifdef PARANOID}
//EXCEPTION(EX_INTERNAL|$101);
//{$endif}

  Result := FPU_Exception_c;
end;

function FPU_u_add (arg1: PFPU_REG; arg2: PFPU_REG; answ: PFPU_REG;
  control_w: u16; sign: u_char; expa: s32; expb: s32): intg;
var
  rtmp: PFPU_REG;
  shifted: FPU_REG;
  extent: u32;
  ediff, ed2, eflag, ovfl, carry: intg;
begin
//    extent := 0;
  ediff  := expa - expb;

  if (ediff < 0) then
  begin
    ediff := -ediff;
    rtmp  := arg1;
    arg1  := arg2;
    arg2  := rtmp;
    expa  := expb;
  end;

  (* Now we have exponent of arg1 >= exponent of arg2 *)

  answ^.exp := expa;

//{$ifdef PARANOID}
//  if ( !(arg1^.sigh  and $80000000) or !(arg2^.sigh  and $80000000) )
//    begin
//      EXCEPTION(EX_INTERNAL|$201);
//      return -1;
//    end;
//{$endif}

  if (ediff = 0) then
  begin
    extent := 0;
    shifted.sigl := arg2^.sigl;
    shifted.sigh := arg2^.sigh;
  end else

  if (ediff < 32) then
  begin
    ed2 := 32 - ediff;
    extent := arg2^.sigl shl ed2;
    shifted.sigl := arg2^.sigl shr ediff;
    shifted.sigl := shifted.sigl or (arg2^.sigh shl ed2);
    shifted.sigh := arg2^.sigh shr ediff;
  end else

  if (ediff < 64) then
  begin
    ediff := ediff - 32;
    if (ediff) = 0 then
    begin
//            eflag  := 0;
      extent := arg2^.sigl;
      shifted.sigl := arg2^.sigh;
    end else
    begin
      ed2 := 32 - ediff;
//      eflag := arg2^.sigl;
//            if (eflag) <> 0 then
//              extent := extent or 1;
      extent := arg2^.sigl shr ediff;
      extent := extent or (arg2^.sigh shl ed2);
      shifted.sigl := arg2^.sigh shr ediff;
    end;
    shifted.sigh := 0;
  end else
  begin
    ediff := ediff - 64;
    if (ediff) = 0 then
    begin
      eflag  := arg2^.sigl;
      extent := arg2^.sigh;
    end else
    begin
//            ed2 := 64 - ediff;
      eflag := arg2^.sigl or arg2^.sigh;
      extent := arg2^.sigh shr ediff;
    end;
    shifted.sigl := 0;
    shifted.sigh := 0;

    if (eflag) <> 0 then
      extent := extent or 1;
  end;

  answ^.sigh := arg1^.sigh + shifted.sigh;
  ovfl := word(shifted.sigh > answ^.sigh);
  answ^.sigl := arg1^.sigl + shifted.sigl;
  if (shifted.sigl > answ^.sigl) then
  begin
    Inc(answ^.sigh);
    if (answ^.sigh = 0) then
      ovfl := 1;
  end;

  if (ovfl) <> 0 then
  begin
    carry  := extent and 1;
    extent := extent shr 1;
    extent := extent or carry;
    if (answ^.sigl and 1) <> 0 then
      extent := extent or $80000000;
    answ^.sigl := answ^.sigl shr 1;
    if (answ^.sigh and 1) <> 0 then
      answ^.sigl := answ^.sigl or $80000000;
    answ^.sigh := answ^.sigh shr 1;
    answ^.sigh := answ^.sigh or $80000000;
    Inc(answ^.exp);
  end;
  Result := FPU_round(answ, extent, 0, control_w, sign);
end;

function FPU_to_exp16 (a: PFPU_REG; x: PFPU_REG): intg;
var
  sign: intg;
begin
  sign := getsign(a);

{$ifndef EMU_BIG_ENDIAN}
  ps64(@x^.sigl)^ := ps64(@a^.sigl)^;
{$else}
*(s64 *)@(x^.sigh) := *(const s64 *)@(a^.sigh);
{$endif}

  (* Set up the exponent as a 16 bit quantity. *)
  setexponent16(x, exponent(a));

  if (exponent16(x) = EXP_UNDER) then
  begin
    (* The number is a de-normal or pseudodenormal. *)
    (* We only deal with the significand and exponent. *)

    if (x^.sigh and $80000000) <> 0 then
    begin
      (* Is a pseudodenormal. *)
  (* This is non-80486 behaviour because the number
     loses its 'denormal' identity. *)
      addexponent(x, 1);
    end else
    begin
      (* Is a denormal. *)
      addexponent(x, 1);
      FPU_normalize_nuo(x, 0);
    end;
  end;

  if ((x^.sigh and $80000000) = 0) then
    fpu_EXCEPTION(EX_INTERNAL or $180);

  Result := sign;
end;

//function FPU_normalize_nuo (x: PFPU_REG; bias: intg): intg;
//  begin
//    if ((x^.sigh and $8000) = 0) then
//      begin
//      if (x^.sigh = 0) then
//        begin
//        if (x^.sigl = 0) then
//          begin
//          x^.exp := EXP_UNDER;
//          Result := TAG_Zero;
//          exit;
//          end;
//        x^.sigh := x^.sigl;
//        x^.sigl := 0;
//        x^.exp  := x^.exp - 32;
//        end;
//      while ((x^.sigh and $8000) = 0) do
//        begin
//        x^.sigh := x^.sigh shl 1;
//        if (x^.sigl and $8000) <> 0 then
//          x^.sigh := x^.sigh or 1;
//        x^.sigl := x^.sigl shl 1;
//        Dec(x^.exp);
//        end;
//      end;
//
//    x^.exp := x^.exp + bias;
//
//    Result := TAG_Valid;
//  end;

function poly_2xm1 (sign: u_char; arg: PFPU_REG; res: PFPU_REG): intg;
var
  exponent, shift: s32;
  Xll: u64;
  accumulator, Denom, argSignif: Xsig;
  tag: u_char;
begin
  exponent := exponent16(arg);

//{$ifdef PARANOID}
//  if ( exponent >= 0 )    	(* Don't want a |number| >= 1.0 *)
//    begin
//      (* Number negative, too large, or not Valid. *)
//      EXCEPTION(EX_INTERNAL|$127);
//      return 1;
//    end;
//{$endif}(* PARANOID *)

  argSignif.lsw := 0;
  Xll := significand(arg)^;
  pu64(@argSignif.msw)^ := Xll;

  if (exponent = -1) then
  begin
    shift := ifthen(argSignif.msw and $40000000 <> 0, 3, 2);
    (* subtract 0.5 or 0.75 *)
    exponent := exponent - 2;
    pu64(@argSignif.msw)^ := pu64(@argSignif.msw)^ shl 2;
    Xll := Xll shl 2;
  end else
    if (exponent = -2) then
    begin
      shift := 1;
      (* subtract 0.25 *)
      Dec(exponent);
      pu64(@argSignif.msw)^ := pu64(@argSignif.msw)^ shl 1;
      Xll := Xll shl 1;
    end else
      shift := 0;

  if (exponent < -2) then
  begin
    (* Shift the argument right by the required places. *)
    if (FPU_shrx(@Xll, -2 - exponent) >= $8000) then
      Inc(Xll);  (* round up *)
  end;

  accumulator.msw  := 0;
  accumulator.midw := 0;
  accumulator.lsw  := 0;
  polynomial_Xsig(@accumulator, @Xll, @lterms, HIPOWER - 1);
  mul_Xsig_Xsig(@accumulator, @argSignif);
  shr_Xsig(@accumulator, 3);

  mul_Xsig_Xsig(@argSignif, @hiterm);   (* The leading term *)
  add_two_Xsig(@accumulator, @argSignif, @exponent);

  if (shift) <> 0 then
  begin
    (* The argument is large, use the identity:
 f(x+a) := f(a) * (f(x) + 1) - 1;
 *)
    shr_Xsig(@accumulator, -exponent);
    accumulator.msw := accumulator.msw or $8000;      (* add 1.0 *)
    mul_Xsig_Xsig(@accumulator, shiftterm[shift]);
    accumulator.msw := accumulator.msw and $3fffffff;      (* subtract 1.0 *)
    exponent := 1;
  end;

  if (sign <> SIGN_POS) then
  begin
    (* The argument is negative, use the identity:
     f(-x) := -f(x) / (1 + f(x))
 *)
    Denom.lsw := accumulator.lsw;
    pu64(@Denom.msw)^ := pu64(@accumulator.msw)^;
    if (exponent < 0) then
      shr_Xsig(@Denom, -exponent)
    else
      if (exponent > 0) then
      begin
        (* exponent must be 1 here *)
        pu64(@Denom.msw)^ := pu64(@Denom.msw)^ shl 1;
        if (Denom.lsw and $8000) <> 0 then
          pu64(@Denom.msw)^ := pu64(@Denom.msw)^ or 1;
        Denom.lsw := (Denom.lsw) shl 1;
      end;
    Denom.msw := Denom.msw or $8000;      (* add 1.0 *)
    div_Xsig(@accumulator, @Denom, @accumulator);
  end;

  (* Convert to 64 bit signed-compatible *)
  exponent := exponent + round_Xsig(@accumulator);

  Result := longint(st(0));
  significand(st(0))^ := pu64(@accumulator)^;
  setexponent16(PFPU_REG(Result), exponent);

  tag := FPU_round(PFPU_REG(Result), 1, 0, FULL_PRECISION, sign);

  setsign(PFPU_REG(Result), sign);
  FPU_settag0(tag);

  Result := 0;
end;

procedure polynomial_Xsig (accum: PXsig; const x: pu64; const terms: parr64;
  const n: intg);
var
  i: intg;
  acc, Xprod: Xsig;
  lprod: u32;
  xlwr, xupr, prod: u64;
  overflowed: byte;
begin
  xlwr := u32(x^);
  xupr := u32(x^ shr 32);

  acc.msw  := terms^[n] shr 32;
  acc.midw := terms^[n];
  acc.lsw  := 0;
  overflowed := 0;

  i := n - 1;
  while i >= 0 do
  begin
    (* Split the product into five parts to get a 16 byte result *)

    (* first word by first word *)
    prod := acc.msw * xupr;
    Xprod.midw := prod;
    Xprod.msw := prod shr 32;

    (* first word by second word *)
    prod  := acc.msw * xlwr;
    Xprod.lsw := prod;
    lprod := prod shr 32;
    Xprod.midw := Xprod.midw + lprod;
    if (lprod > Xprod.midw) then
      Inc(Xprod.msw);

    (* second word by first word *)
    prod := acc.midw * xupr;
    Xprod.lsw := Xprod.lsw + prod;
    if (u32(prod) > Xprod.lsw) then
    begin
      Inc(Xprod.midw);
      if (Xprod.midw = 0) then
        Inc(Xprod.msw);
    end;
    lprod := prod shr 32;
    Xprod.midw := Xprod.midw + lprod;
    if (lprod > Xprod.midw) then
      Inc(Xprod.msw);

    (* second word by second word *)
    prod  := acc.midw * xlwr;
    lprod := prod shr 32;
    Xprod.lsw := Xprod.lsw + lprod;
    if (lprod > Xprod.lsw) then
    begin
      Inc(Xprod.midw);
      if (Xprod.midw = 0) then
        Inc(Xprod.msw);
    end;

    (* third word by first word *)
    prod  := acc.lsw * xupr;
    lprod := prod shr 32;
    Xprod.lsw := Xprod.lsw + lprod;
    if (lprod > Xprod.lsw) then
    begin
      Inc(Xprod.midw);
      if (Xprod.midw = 0) then
        Inc(Xprod.msw);
    end;

    if (overflowed) <> 0 then
    begin
      Xprod.midw := Xprod.midw + xlwr;
      if (u32(xlwr) > Xprod.midw) then
        Inc(Xprod.msw);
      Xprod.msw  := Xprod.msw + xupr;
      overflowed := 0;    (* We don't check this addition for overflow *)
      Dec(i);
    end;

    acc.lsw  := Xprod.lsw;
    acc.midw := u32(terms^[i]) + Xprod.midw;
    acc.msw  := (terms^[i] shr 32) + Xprod.msw;
    if (Xprod.msw > acc.msw) then
      overflowed := 1;
    if (u32(terms^[i]) > acc.midw) then
    begin
      Inc(acc.msw);
      if (acc.msw = 0) then
        overflowed := 1;
    end;
  end;

  (* We don't check the addition to accum for overflow *)
  accum^.lsw := accum^.lsw + acc.lsw;
  if (acc.lsw > accum^.lsw) then
  begin
    Inc(accum^.midw);
    if (accum^.midw = 0) then
      Inc(accum^.msw);
  end;
  accum^.midw := accum^.midw + acc.midw;
  if (acc.midw > accum^.midw) then
    Inc(accum^.msw);
  accum^.msw := accum^.msw + acc.msw;
end;

procedure mul32_Xsig (x: PXsig; const ba: u32);
var
  y:  Xsig;
  zl: u32;
  b, z: u64;
begin
  b := ba;

  z := b * x^.lsw;
  y.lsw := z shr 32;

  z  := b * x^.midw;
  y.midw := z shr 32;
  zl := z;
  y.lsw := y.lsw + zl;
  if (zl > y.lsw) then
    Inc(y.midw);

  z  := b * x^.msw;
  y.msw := z shr 32;
  zl := z;
  y.midw := y.midw + zl;
  if (zl > y.midw) then
    Inc(y.msw);

  x^ := y;
end;

procedure mul64_Xsig (x: PXsig; const b: pu64);
var
  yh, yl: Xsig;
begin
  yh := x^;
  yl := x^;
  mul32_Xsig(@yh, b^ shr 32);
  mul32_Xsig(@yl, b^);

  x^.msw  := yh.msw;
  x^.midw := yh.midw + yl.msw;
  if (yh.midw > x^.midw) then
    Inc(x^.msw);
  x^.lsw := yh.lsw + yl.midw;
  if (yh.lsw > x^.lsw) then
  begin
    Inc(x^.midw);
    if (x^.midw = 0) then
      Inc(x^.msw);
  end;
end;

procedure mul_Xsig_Xsig (x: PXsig; const b: PXsig);
var
  yh: u32;
  y, z: u64;
begin
  y  := b^.lsw;
  y  := y * x^.msw;
  yh := y shr 32;

  z := b^.msw;
  z := z shl 32;
  z := z + b^.midw;
  mul64_Xsig(x, @z);

  x^.lsw := x^.lsw + yh;
  if (yh > x^.lsw) then
  begin
    Inc(x^.midw);
    if (x^.midw = 0) then
      Inc(x^.msw);
  end;
end;

procedure shr_Xsig (arg: PXsig; nr: intg);
var
  n: intg;
begin
  n := nr;

  while n >= 32 do
  begin
    arg^.lsw := arg^.midw;
    arg^.midw := arg^.msw;
    arg^.msw := 0;
    Dec(n, 32);
  end;

  if (n <= 0) then
    exit;

  arg^.lsw  := (arg^.lsw shr n) or (arg^.midw shl (32 - n));
  arg^.midw := (arg^.midw shr n) or (arg^.msw shl (32 - n));
  arg^.msw  := arg^.msw shr n;
end;

function FPU_mul (b: PFPU_REG; tagb: u_char; deststnr: intg; control_w: intg): intg;
var
  a: PFPU_REG;
  dest: PFPU_REG;
  taga: u_char;
  saved_sign: u_char;
  sign: u_char;
  tag: intg;
  x, y: FPU_REG;
begin
  a := st(deststnr);
  dest := a;
  taga := FPU_gettagi(deststnr);
  saved_sign := getsign(dest);
  sign := (getsign(a) or getsign(b));

  if ((taga or tagb) = 0) then
  begin
    (* Both regs Valid, this should be the most common case. *)
    tag := FPU_u_mul(a, b, dest, control_w, sign, exponent(a) + exponent(b));
    if (tag < 0) then
    begin
      setsign(dest, saved_sign);
      exit(tag);
    end;
    FPU_settagi(deststnr, tag);
    exit(tag);
  end;

  if (taga = TAG_Special) then
    taga := FPU_Special(a);
  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);

  if (((taga = TAG_Valid) and (tagb = TW_Denormal)) or
    ((taga = TW_Denormal) and (tagb = TAG_Valid)) or
    ((taga = TW_Denormal) and (tagb = TW_Denormal))) then
  begin
    if (denormal_operand() < 0) then
      Exit(FPU_Exception_c);

    FPU_to_exp16(a, @x);
    FPU_to_exp16(b, @y);
    tag := FPU_u_mul(@x, @y, dest, control_w, sign, exponent16(@x) +
      exponent16(@y));
    if (tag < 0) then
    begin
      setsign(dest, saved_sign);
      Exit(tag);
    end;
    FPU_settagi(deststnr, tag);
    Exit(tag);
  end else
    if ((taga <= TW_Denormal) and (tagb <= TW_Denormal)) then
    begin
      if (((tagb = TW_Denormal) or (taga = TW_Denormal)) and
        (denormal_operand() < 0)) then
        Exit(FPU_Exception_c);

    (* Must have either both arguments = zero, or
       one valid and the other zero.
       The result is therefore zero. *)
      FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);
    (* The 80486 book says that the answer is +0, but a real
       80486 behaves this way.
       IEEE-754 apparently says it should be this way. *)
      setsign(dest, sign);
      Result := TAG_Zero;
      Exit;
    end else
    (* Must have infinities, NaNs, etc *)
      if ((taga = TW_NaN) or (tagb = TW_NaN)) then
      begin
        Result := real_2op_NaN(b, tagb, deststnr, st(0));
        Exit;
      end else
        if (((taga = TW_Infinity) and (tagb = TAG_Zero)) or
          ((tagb = TW_Infinity) and (taga = TAG_Zero))) then
        begin
          Result := arith_invalid(deststnr);  (* Zero*Infinity is invalid *)
          Exit;
        end else
          if (((taga = TW_Denormal) or (tagb = TW_Denormal)) and
            (denormal_operand() < 0)) then
          begin
            Result := FPU_Exception_c;
            Exit;
          end else
            if (taga = TW_Infinity) then
            begin
              FPU_copy_to_regi(a, TAG_Special, deststnr);
              setsign(dest, sign);
              Result := TAG_Special;
              Exit;
            end else
              if (tagb = TW_Infinity) then
              begin
                FPU_copy_to_regi(b, TAG_Special, deststnr);
                setsign(dest, sign);
                Result := TAG_Special;
                Exit;
              end;

//{$ifdef PARANOID}
//else
//  begin
//    EXCEPTION(EX_INTERNAL|$102);
//    return FPU_Exception;
//  end;
//{$endif}(* PARANOID *)

end;


procedure math_abort (info: pinfo; signal: word);
begin
  bx_cpu.Exception(16, 0, 0);
end;

procedure fpu_verify_area (what: unsigned; ptr: pointer; n: unsigned);
var
  seg: Psegment_reg_t;
begin
  seg := @varfpu_cpu_ptr^.sregs[varfpu_iptr^.seg];

  if (what = VERIFY_READ) then
//      begin
    varfpu_cpu_ptr^.read_virtual_checks(seg, Bit32u(ptr), n);
//      end
  {else;   // VERIFY_WRITE
  varfpu_cpu_ptr->write_virtual_checks(seg, PTR2INT(ptr), n);
  }
  //BX_DEBUG(( "verify_area: 0x%x", PTR2INT(ptr)));
end;

function fpu_get_user (ptr: pointer; len: integer): longword;
var
  val32: Bit32u;
  val16: Bit16u;
  val8:  Bit8u;
begin
  case (len) of
    1:
    begin
      varfpu_cpu_ptr^.read_virtual_byte(varfpu_iptr^.seg, Bit32u(ptr), @val8);
      val32 := val8;
    end;
    2:
    begin
      varfpu_cpu_ptr^.read_virtual_word(varfpu_iptr^.seg, Bit32u(ptr), @val16);
      val32 := val16;
    end;
    4:
      varfpu_cpu_ptr^.read_virtual_dword(varfpu_iptr^.seg, Bit32u(ptr), @val32);
  else
    LogPanic(Format('fpu_get_user: len:=%u', [len]));
  end;
  Result := (val32);
end;

function fpu_put_user (val: longword; ptr: pointer; len: unsigned): longword;
var
  val32: Bit32u;
  val16: Bit16u;
  val8:  Bit8u;
begin
  case (len) of
    1:
    begin
      val8 := val;
      varfpu_cpu_ptr^.write_virtual_byte(varfpu_iptr^.seg, Bit32u(ptr), @val8);
    end;
    2:
    begin
      val16 := val;
      varfpu_cpu_ptr^.write_virtual_word(varfpu_iptr^.seg, Bit32u(ptr), @val16);
    end;
    4:
    begin
      val32 := val;
      varfpu_cpu_ptr^.write_virtual_dword(varfpu_iptr^.seg, Bit32u(ptr), @val32);
    end;
  else
    LogPanic(Format('fpu_put_user: len:=%u', [len]));
  end;
end;

function fpu_get_ds: word;
begin
  Result := (varfpu_cpu_ptr^.sregs[SEG_REG_DS].selector.Value);
end;

procedure poly_tan (st0_ptr: PFPU_REG; invert: intg);
var
  exponent_: s32;
  argSq, argSqSq, accumulatoro, accumulatore, accum, argSignif: Xsig;
begin
  exponent_ := exponent(st0_ptr);
//{$ifdef PARANOID}
//  if ( signnegative(st0_ptr) )<>0 then	(* Can't hack a number < 0.0 *)
//    begin arith_invalid(0); exit; end;  (* Need a positive number *)
//{$endif}(* PARANOID *)

  if ((exponent_ >= 0) or ((exponent_ = -1) and (st0_ptr^.sigh > $c90fdaa2))) then
    fpu_EXCEPTION($250)
  else
  begin
    argSignif.lsw := 0;
    pu64(accum.msw)^ := significand(st0_ptr)^;
    pu64(accum.msw)^ := pu64(accum.msw)^;

    if (exponent_ < -1) then
      (* shift the argument right by the required places *)
      if (FPU_shrx(@pu64(accum.msw)^, -1 - exponent_) >= $8000) then
        Inc(pu64(accum.msw)^);  (* round up *)
  end;

  pu64(argSq.msw)^ := pu64(accum.msw)^;
  argSq.lsw := accum.lsw;
  mul_Xsig_Xsig(@argSq, @argSq);
  pu64(argSqSq.msw)^ := pu64(argSq.msw)^;
  argSqSq.lsw := argSq.lsw;
  mul_Xsig_Xsig(@argSqSq, @argSqSq);

  (* Compute the negative terms for the numerator polynomial *)
  accumulatoro.msw  := 0;
  accumulatoro.midw := 0;
  accumulatoro.lsw  := 0;
  polynomial_Xsig(@accumulatoro, @pu64(argSqSq.msw)^, @oddnegterm, HiPOWERon1 - 1);
  mul_Xsig_Xsig(@accumulatoro, @argSq);
  negate_Xsig(@accumulatoro);
  (* Add the positive terms *)
  polynomial_Xsig(@accumulatoro, @pu64(argSqSq.msw)^, @oddplterm, HiPOWERop1 - 1);


  (* Compute the positive terms for the denominator polynomial *)
  accumulatore.msw  := 0;
  accumulatore.midw := 0;
  accumulatore.lsw  := 0;
  polynomial_Xsig(@accumulatore, @pu64(argSqSq.msw)^, @evenplterm, HiPOWERep - 1);
  mul_Xsig_Xsig(@accumulatore, @argSq);
  negate_Xsig(@accumulatore);
  (* Add the negative terms *)
  polynomial_Xsig(@accumulatore, @pu64(argSqSq.msw)^, @evennegterm, HiPOWERen - 1);
  (* Multiply by arg^2 *)
  mul64_Xsig(@accumulatore, @pu64(argSignif.msw)^);
  mul64_Xsig(@accumulatore, @pu64(argSignif.msw)^);
  (* de-normalize and divide by 2 *)
  shr_Xsig(@accumulatore, -2 * (1 + exponent_) + 1);
  negate_Xsig(@accumulatore);      (* This does 1 - accumulator *)

  (* Now find the ratio. *)
  if (accumulatore.msw = 0) then
    begin
    (* accumulatoro must contain 1.0 here, (actually, 0) but it
 really doesn't matter what value we use because it will
 have negligible effect in later calculations
 *)
    pu64(accum.msw)^ := $800000000000;
    accum.lsw := 0;
    end
  else
    begin
    div_Xsig(@accumulatoro, @accumulatore, @accum);
    end;

  (* Multiply by 1/3 * arg^3 *)
  mul64_Xsig(@accum, @pu64(argSignif.msw)^);
  mul64_Xsig(@accum, @pu64(argSignif.msw)^);
  mul64_Xsig(@accum, @pu64(argSignif.msw)^);
  mul64_Xsig(@accum, @twothirds);
  shr_Xsig(@accum, -2 * (exponent_ + 1));


  (* tan(arg) := arg + accum *)
  add_two_Xsig(@accum, @argSignif, @exponent);

  if (invert) <> 0 then
    begin
    (* accum now contains tan(pi/2 - arg).
 Use tan(arg) := 1.0 / tan(pi/2 - arg)
 *)
    accumulatoro.lsw  := 0;
    accumulatoro.midw := 0;
    accumulatoro.msw  := $8000;
    div_Xsig(@accumulatoro, @accum, @accum);
    exponent_ := -exponent_;
    end;


  (* Transfer the result *)
  exponent_ := exponent_ + pu64(accum.msw)^;
  FPU_settag0(TAG_Valid);
  significand(st0_ptr)^ := pu64(accum.msw)^;
  setexponent16(st0_ptr, exponent_ + EXTENDED_Ebias);  (* Result is positive. *)

end;

procedure poly_sine (st0_ptr: PFPU_REG);
var
  exponent_, echange: intg;
  accumulator, argSqrd, argTo4: Xsig;
  fix_up, adj: s32;
  fixed_arg: u64;
  res: FPU_REG;
begin
  exponent_ := exponent(st0_ptr);

  accumulator.lsw  := 0;
  accumulator.midw := 0;
  accumulator.msw  := 0;

  (* Split into two ranges, for arguments below and above 1.0 *)
  (* The boundary between upper and lower is approx 0.88309101259 *)
  if ((exponent_ < -1) or ((exponent_ = -1) and (st0_ptr^.sigh <= $e21240aa))) then
  begin
    (* The argument is <= 0.883091012ù59 *)

    argSqrd.msw  := st0_ptr^.sigh;
    argSqrd.midw := st0_ptr^.sigl;
    argSqrd.lsw  := 0;
    mul64_Xsig(@argSqrd, significand(st0_ptr));
    shr_Xsig(@argSqrd, 2 * (-1 - exponent_));
    argTo4.msw  := argSqrd.msw;
    argTo4.midw := argSqrd.midw;
    argTo4.lsw  := argSqrd.lsw;
    mul_Xsig_Xsig(@argTo4, @argTo4);

    polynomial_Xsig(@accumulator, @pu64(argTo4.msw)^, @neg_terms_l,
      N_COEFF_N - 1);
    mul_Xsig_Xsig(@accumulator, @argSqrd);
    negate_Xsig(@accumulator);

    polynomial_Xsig(@accumulator, @pu64(argTo4.msw)^, @pos_terms_l,
      N_COEFF_P - 1);

    shr_Xsig(@accumulator, 2);    (* Divide by four *)
    accumulator.msw := accumulator.msw or $8000;  (* Add 1.0 *)

    mul64_Xsig(@accumulator, significand(st0_ptr));
    mul64_Xsig(@accumulator, significand(st0_ptr));
    mul64_Xsig(@accumulator, significand(st0_ptr));

    (* Divide by four, FPU_REG compatible, etc *)
    exponent_ := 3 * exponent_;

    (* The minimum exponent_ difference is 3 *)
    shr_Xsig(@accumulator, exponent(st0_ptr) - exponent_);

    negate_Xsig(@accumulator);
    pu64(accumulator.msw)^ := pu64(accumulator.msw)^ + significand(st0_ptr)^;

    echange := round_Xsig(@accumulator);

    setexponentpos(@res, exponent(st0_ptr) + echange);
  end else
  begin
    (* The argument is > 0.88309101259 *)
    (* We use sin(st(0)) := cos(pi/2-st(0)) *)

    fixed_arg := significand(st0_ptr)^;

    if (exponent_ = 0) then
      begin
      (* The argument is >= 1.0 *)

      (* Put the binary point at the left. *)
      fixed_arg := fixed_arg shl 1;
      end;
    (* pi/2 in hex is: 1.921fb54442d18469 898CC51701B839A2 52049C1 *)
    fixed_arg := $921fb54442d18469 - fixed_arg;
    (* There is a special case which arises due to rounding, to fix here. *)
    if (fixed_arg = $ffffffffffffffff) then
      fixed_arg := 0;

    XSIG_LL(argSqrd)^ := fixed_arg;
    argSqrd.lsw := 0;
    mul64_Xsig(@argSqrd, @fixed_arg);

    XSIG_LL(argTo4)^ := XSIG_LL(argSqrd)^;
    argTo4.lsw := argSqrd.lsw;
    mul_Xsig_Xsig(@argTo4, @argTo4);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @neg_terms_h, N_COEFF_NH - 1);
    mul_Xsig_Xsig(@accumulator, @argSqrd);
    negate_Xsig(@accumulator);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @pos_terms_h, N_COEFF_PH - 1);
    negate_Xsig(@accumulator);

    mul64_Xsig(@accumulator, @fixed_arg);
    mul64_Xsig(@accumulator, @fixed_arg);

    shr_Xsig(@accumulator, 3);
    negate_Xsig(@accumulator);

    add_Xsig_Xsig(@accumulator, @argSqrd);

    shr_Xsig(@accumulator, 1);

    accumulator.lsw := accumulator.lsw or 1;
    (* A zero accumulator here would cause problems *)
    negate_Xsig(@accumulator);

    (* The basic computation is complete. Now fix the answer to
       compensate for the error due to the approximation used for
       pi/2 *)

    (* This has an exponent_ of -65 *)
    fix_up := $898cc517;
    (* The fix-up needs to be improved for larger args *)
    if (argSqrd.msw and $ffc00000) <> 0 then
      (* Get about 32 bit precision in these: *)
      fix_up := fix_up - Trunc(mul_32_32($898cc517, argSqrd.msw) / 6);

    fix_up := mul_32_32(fix_up, LL_MSW(@fixed_arg)^);

    adj := accumulator.lsw;    (* temp save *)
    accumulator.lsw := accumulator.lsw - fix_up;
    if (accumulator.lsw > adj) then
      Dec(XSIG_LL(accumulator)^);

    echange := round_Xsig(@accumulator);

    setexponentpos(@res, echange - 1);
  end;

  significand(@res)^ := XSIG_LL(accumulator)^;
  setsign(@res, getsign(st0_ptr));
  FPU_copy_to_reg0(@res, TAG_Valid);

//{$ifdef PARANOID}
//  if ( (exponent_(@res) >= 0)
//      @ and (significand(@res) > BX_CONST64($800000000000)) )
//    begin
//      EXCEPTION(EX_INTERNAL|$150);
//    end;
//{$endif}(* PARANOID *)

end;

(*--- poly_cos() ------------------------------------------------------------+
or                                                                          |
 +---------------------------------------------------------------------------*)
procedure poly_cos (st0_ptr: PFPU_REG);
var
  res: FPU_REG;
  exponent_, exp2, echange: s32;
  accumulator, argSqrd, fix_up, argTo4: Xsig;
  fixed_arg: u64;
begin
//{$ifdef PARANOID}
//  if ( (exponent_(st0_ptr) > 0)
//      or ((exponent_(st0_ptr) = 0)
//	  @ and (significand(st0_ptr) > BX_CONST64($c90fdaa22168c234))) )
//    begin
//      EXCEPTION(EX_Invalid);
//      FPU_copy_to_reg0(@CONST_QNaN, TAG_Special);
//      exit;
//    end;
//{$endif}(* PARANOID *)

  exponent_ := exponent(st0_ptr);

  accumulator.lsw  := 0;
  accumulator.midw := 0;
  accumulator.msw  := 0;

  if ((exponent_ < -1) or ((exponent_ = -1) and (st0_ptr^.sigh <= $b00d6f54))) then
  begin
    (* arg is < 0.687705 *)

    argSqrd.msw  := st0_ptr^.sigh;
    argSqrd.midw := st0_ptr^.sigl;
    argSqrd.lsw  := 0;
    mul64_Xsig(@argSqrd, significand(st0_ptr));

    (* shift the argument right by the required places *)
    if (exponent_ < -1) then
      shr_Xsig(@argSqrd, 2 * (-1 - exponent_));

    argTo4.msw  := argSqrd.msw;
    argTo4.midw := argSqrd.midw;
    argTo4.lsw  := argSqrd.lsw;
    mul_Xsig_Xsig(@argTo4, @argTo4);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @neg_terms_h, N_COEFF_NH - 1);
    mul_Xsig_Xsig(@accumulator, @argSqrd);
    negate_Xsig(@accumulator);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @pos_terms_h, N_COEFF_PH - 1);
    negate_Xsig(@accumulator);

    mul64_Xsig(@accumulator, significand(st0_ptr));
    mul64_Xsig(@accumulator, significand(st0_ptr));
    shr_Xsig(@accumulator, -2 * (1 + exponent_));

    shr_Xsig(@accumulator, 3);
    negate_Xsig(@accumulator);

    add_Xsig_Xsig(@accumulator, @argSqrd);

    shr_Xsig(@accumulator, 1);

    (* It doesn't matter if accumulator is all zero here, the
     following code will work ok *)
    negate_Xsig(@accumulator);

    if (accumulator.lsw and $8000) <> 0 then
      Inc(XSIG_LL(accumulator)^);
    if (accumulator.msw = 0) then
    begin
      (* The res is 1.0 *)
      FPU_copy_to_reg0(@CONST_1, TAG_Valid);
      exit;
    end else
    begin
      significand(@res)^ := XSIG_LL(accumulator)^;
      (* will be a valid positive nr with expon := -1 *)
      setexponentpos(@res, -1);
    end;
  end else
  begin
    fixed_arg := significand(st0_ptr)^;

    (* The argument is >= 1.0 *)
    (* Put the binary point at the left. *)
    if (exponent_ = 0) then
      fixed_arg := fixed_arg shl 1;

    (* pi/2 in hex is: 1.921fb54442d18469 898CC51701B839A2 52049C1 *)
    fixed_arg := $921fb54442d18469 - fixed_arg;
    (* There is a special case which arises due to rounding, to fix here. *)
    if (fixed_arg = $ffffffffffffffff) then
      fixed_arg := 0;

    exponent_ := -1;
    exp2 := -1;

    (* A shift is needed here only for a narrow range of arguments,
     i.e. for fixed_arg approx 2^-32, but we pick up more... *)
    if ((LL_MSW(@fixed_arg)^ and $ffff0000) = 0) then
    begin
      fixed_arg := fixed_arg shl 16;
      exponent_ := exponent_ - 16;
      exp2 := exp2 - 16;
    end;

    XSIG_LL(argSqrd)^ := fixed_arg;
    argSqrd.lsw := 0;
    mul64_Xsig(@argSqrd, @fixed_arg);

    if (exponent_ < -1) then
      shr_Xsig(@argSqrd, 2 * (-1 - exponent_)); (* shift the argument right by the required places *)

    argTo4.msw  := argSqrd.msw;
    argTo4.midw := argSqrd.midw;
    argTo4.lsw  := argSqrd.lsw;
    mul_Xsig_Xsig(@argTo4, @argTo4);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @neg_terms_l,
      N_COEFF_N - 1);
    mul_Xsig_Xsig(@accumulator, @argSqrd);
    negate_Xsig(@accumulator);

    polynomial_Xsig(@accumulator, XSIG_LL(argTo4), @pos_terms_l,
      N_COEFF_P - 1);

    shr_Xsig(@accumulator, 2);    (* Divide by four *)
    accumulator.msw := accumulator.msw or $8000;  (* Add 1.0 *)

    mul64_Xsig(@accumulator, @fixed_arg);
    mul64_Xsig(@accumulator, @fixed_arg);
    mul64_Xsig(@accumulator, @fixed_arg);

    (* Divide by four, FPU_REG compatible, etc *)
    exponent_ := 3 * exponent_;

    (* The minimum exponent_ difference is 3 *)
    shr_Xsig(@accumulator, exp2 - exponent_);

    negate_Xsig(@accumulator);
    XSIG_LL(accumulator)^ := XSIG_LL(accumulator)^ + fixed_arg;

    (* The basic computation is complete. Now fix the answer to
 compensate for the error due to the approximation used for
 pi/2
 *)

    (* This has an exponent_ of -65 *)
    XSIG_LL(fix_up)^ := $898cc51701b839a2;
    fix_up.lsw := 0;

    (* The fix-up needs to be improved for larger args *)
    if (argSqrd.msw and $ffc00000) <> 0 then
    begin
      (* Get about 32 bit precision in these: *)
      fix_up.msw := Trunc(fix_up.msw - mul_32_32($898cc517, argSqrd.msw) / 2);
      fix_up.msw := Trunc(fix_up.msw + mul_32_32($898cc517, argTo4.msw) / 24);
    end;

    exp2 := exp2 + norm_Xsig(@accumulator);
    shr_Xsig(@accumulator, 1); (* Prevent overflow *)
    Inc(exp2);
    shr_Xsig(@fix_up, 65 + exp2);

    add_Xsig_Xsig(@accumulator, @fix_up);

    echange := round_Xsig(@accumulator);

    setexponentpos(@res, exp2 + echange);
    significand(@res)^ := XSIG_LL(accumulator)^;
  end;

  FPU_copy_to_reg0(@res, TAG_Valid);

//{$ifdef PARANOID}
//  if ( (exponent_(@res) >= 0)
//      @ and (significand(@res) > BX_CONST64($800000000000)) )
//    begin
//      EXCEPTION(EX_INTERNAL|$151);
//    end;
//{$endif}(* PARANOID *)
end;

procedure fnop;
begin
end;

procedure fclex;
begin
  varI387.soft.swd := varI387.soft.swd and not (SW_Backward or SW_Summary or
      SW_Stack_Fault or SW_Precision or SW_Underflow or SW_Overflow or
      SW_Zero_Div or SW_Denorm_Op or SW_Invalid);
  varI387.soft.no_update := 1;
end;

//procedure finit ();
//  begin
//    varI387.soft.cwd  := $037f;
//    varI387.soft.swd  := 0;
//    varI387.soft.ftop := 0;            (* We don't keep top in the status word internally. *)
//    varI387.soft.twd  := $ffff;
//  (* The behaviour is different from that detailed in
//     Section 15.1.6 of the Intel manual *)
//    paddress(@varI387.soft.foo)^.offset := 0;
//    paddress(@varI387.soft.foo)^.selector := 0;
//    paddress(@varI387.soft.fip)^.offset := 0;
//    paddress(@varI387.soft.fip)^.selector := 0;
//    paddress(@varI387.soft.fip)^.opcode := 0;
//    varI387.soft.no_update := 1;
//  end;

(*---------------------------------------------------------------------------+
or reg_compare.c                                                            |
or $Id: reg_compare.c,v 1.3 2001/10/06 03:53:46 bdenney Exp $
or                                                                          |
orCompare two floating point registers                                      |
or                                                                          |
orCopyright (C) 1992,1993,1994,1997                                         |
or                 W. Metzenthen, 22 Parker St, Ormond, Vic 3163, Australia |
or                 E-mail   billm@suburbia.net                              |
or                                                                          |
or                                                                          |
 +---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------+
orcompare() is the core FPU_REG comparison function                         |
 +---------------------------------------------------------------------------*)

function compare (b: PFPU_REG; tagb: intg): intg;
var
  diff, exp0, expb: intg;
  st0_tag: u_char;
  st0_ptr: PFPU_REG;
  x, y: FPU_REG;
  st0_sign, signb: u_char;
  signalling, unsupported: intg;
begin
  signb := getsign(b);

  st0_ptr  := st(0);
  st0_tag  := FPU_gettag0();
  st0_sign := getsign(st0_ptr);

  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);
  if (st0_tag = TAG_Special) then
    st0_tag := FPU_Special(st0_ptr);

  if (((st0_tag <> TAG_Valid) and (st0_tag <> TW_Denormal)) or
    ((tagb <> TAG_Valid) and (tagb <> TW_Denormal))) then
  begin
    if (st0_tag = TAG_Zero) then
    begin
      if (tagb = TAG_Zero) then
      begin
        Result := COMP_A_eq_B;
        Exit;
      end;
      if (tagb = TAG_Valid) then
      begin
        Result := ifthen((signb = SIGN_POS), COMP_A_lt_B, COMP_A_gt_B);
        exit;
      end;
      if (tagb = TW_Denormal) then
      begin
        Result := ifthen((signb = SIGN_POS), COMP_A_lt_B, COMP_A_gt_B) or
          COMP_Denormal;
        exit;
      end;
    end else
      if (tagb = TAG_Zero) then
      begin
        if (st0_tag = TAG_Valid) then
        begin
          Result := ifthen((st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B);
          exit;
        end;
        if (st0_tag = TW_Denormal) then
        begin
          Result := ifthen((st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B) or
            COMP_Denormal;
          exit;
        end;
      end;

    if (st0_tag = TW_Infinity) then
    begin
      if ((tagb = TAG_Valid) or (tagb = TAG_Zero)) then
      begin
        Result := ifthen((st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B);
        exit;
      end else
        if (tagb = TW_Denormal) then
        begin
          Result := ifthen((st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B) or
            COMP_Denormal;
          exit;
        end else
          if (tagb = TW_Infinity) then
          begin
            (* The 80486 book says that infinities can be equal! *)
            Result := ifthen(st0_sign = signb, COMP_A_eq_B, ifthen(
              (st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B));
            exit;
          end;
      (* Fall through to the NaN code *)
    end else
      if (tagb = TW_Infinity) then
      begin
        if ((st0_tag = TAG_Valid) or (st0_tag = TAG_Zero)) then
        begin
          Result := ifthen((signb = SIGN_POS), COMP_A_lt_B, COMP_A_gt_B);
          exit;
        end;
        if (st0_tag = TW_Denormal) then
        begin
          Result := ifthen(signb = SIGN_POS, COMP_A_lt_B, COMP_A_gt_B) or
            COMP_Denormal;
          exit;
        end;
        (* Fall through to the NaN code *)
      end;

    (* The only possibility now should be that one of the arguments
 is a NaN *)
    if ((st0_tag = TW_NaN) or (tagb = TW_NaN)) then
    begin
      signalling  := 0;
      unsupported := 0;
      if (st0_tag = TW_NaN) then
      begin
        signalling  := word((st0_ptr^.sigh and $c0000000) = $8000);
        unsupported := word(((exponent(st0_ptr) = EXP_OVER) and
          ((st0_ptr^.sigh and $8000) <> 0)) = False);
      end;
      if (tagb = TW_NaN) then
      begin
        signalling  := signalling or word((b^.sigh and $c0000000) = $8000);
        unsupported := unsupported or word(
          ((exponent(b) = EXP_OVER) and ((b^.sigh and $8000) <> 0)) = False);
      end;
      if (signalling or unsupported) <> 0 then
      begin
        Result := COMP_No_Comp or COMP_SNaN or COMP_NaN;
        exit;
      end else
      begin (* Neither is a signaling NaN *)
        Result := COMP_No_Comp or COMP_NaN;
        exit;
      end;
    end;

    fpu_EXCEPTION(EX_Invalid);
  end;

  if (st0_sign <> signb) then
  begin
    Result := ifthen(st0_sign = SIGN_POS, COMP_A_gt_B, COMP_A_lt_B) or
      (ifthen((st0_tag = TW_Denormal) or (tagb = TW_Denormal), COMP_Denormal, 0));
    Exit;
  end;

  if ((st0_tag = TW_Denormal) or (tagb = TW_Denormal)) then
  begin
    FPU_to_exp16(st0_ptr, @x);
    FPU_to_exp16(b, @y);
    st0_ptr := @x;
    b := @y;
    exp0 := exponent16(st0_ptr);
    expb := exponent16(b);
  end else
  begin
    exp0 := exponent(st0_ptr);
    expb := exponent(b);
  end;

//{$ifdef PARANOID}
//  if (!(st0_ptr^.sigh  and $8000)) EXCEPTION(EX_Invalid);
//  if (!(b^.sigh  and $8000)) EXCEPTION(EX_Invalid);
//{$endif}(* PARANOID *)

  diff := exp0 - expb;
  if (diff = 0) then
  begin
    diff := st0_ptr^.sigh - b^.sigh;  (* Works only if ms bits are
              identical *)
    if (diff = 0) then
    begin
      diff := word(st0_ptr^.sigl > b^.sigl);
      if (diff = 0) then
        diff := -word(st0_ptr^.sigl < b^.sigl);
    end;
  end;

  if (diff > 0) then
  begin
    Result := ifthen((st0_sign = SIGN_POS), COMP_A_gt_B, COMP_A_lt_B) or
      ifthen(((st0_tag = TW_Denormal) or (tagb = TW_Denormal)), COMP_Denormal, 0);
    exit;
  end;
  if (diff < 0) then
  begin
    Result := ifthen((st0_sign = SIGN_POS), COMP_A_lt_B, COMP_A_gt_B) or
      ifthen(((st0_tag = TW_Denormal) or (tagb = TW_Denormal)), COMP_Denormal, 0);
    exit;
  end;

  Result := COMP_A_eq_B or ifthen(
    ((st0_tag = TW_Denormal) or (tagb = TW_Denormal)), COMP_Denormal, 0);
end;


(* This function requires that st(0) is not empty *)
function FPU_compare_st_data (loaded_data: PFPU_REG; loaded_tag: u_char): intg;
var
  f, c: intg;
begin
  c := compare(loaded_data, loaded_tag);
  if (c and COMP_NaN) <> 0 then
  begin
    fpu_EXCEPTION(EX_Invalid);
    f := SW_C3 or SW_C2 or SW_C0;
  end else
  case (c and 7) of
    COMP_A_lt_B:
      f := SW_C0;
    COMP_A_eq_B:
      f := SW_C3;
    COMP_A_gt_B:
      f := 0;
    COMP_No_Comp:
      f := SW_C3 or SW_C2 or SW_C0;
//{$ifdef PARANOID}
//      default:
//	EXCEPTION(EX_INTERNAL|$121);
//	f := SW_C3orSW_C2orSW_C0;
//	break;
//{$endif}(* PARANOID *)
  end;
  setcc(f);
  if (c and COMP_Denormal) <> 0 then
    exit(word(denormal_operand() < 0));
  Result := 0;
end;

function compare_st_st (nr: intg): intg;
var
  f, c: intg;
  st_ptr: PFPU_REG;
begin
  if ((NOT_EMPTY(0) = 0) or (NOT_EMPTY(nr) = 0)) then
  begin
    setcc(SW_C3 or SW_C2 or SW_C0);
    (* Stack fault *)
    fpu_EXCEPTION(EX_StackUnder);
    Result := word((varI387.soft.cwd and CW_Invalid) = 0);
  end;

  st_ptr := st(nr);
  c := compare(st_ptr, FPU_gettagi(nr));
  if (c and COMP_NaN) <> 0 then
  begin
    setcc(SW_C3 or SW_C2 or SW_C0);
    fpu_EXCEPTION(EX_Invalid);
    Result := word((varI387.soft.cwd and CW_Invalid) = 0);
  end else
  case (c and 7) of
    COMP_A_lt_B:
      f := SW_C0;
    COMP_A_eq_B:
      f := SW_C3;
    COMP_A_gt_B:
      f := 0;
    COMP_No_Comp:
      f := SW_C3 or SW_C2 or SW_C0;
//{$ifdef PARANOID}
//      default:
//	EXCEPTION(EX_INTERNAL|$122);
//	f := SW_C3orSW_C2orSW_C0;
//	break;
//{$endif}(* PARANOID *)
  end;
  setcc(f);
  if (c and COMP_Denormal) <> 0 then
    Exit(word(denormal_operand() < 0));
  Result := 0;
end;


function compare_u_st_st (nr: intg): intg;
var
  f, c: intg;
  st_ptr: PFPU_REG;
begin
  if ((FPU_empty_i(0) = 0) or (FPU_empty_i(nr) = 0)) then
  begin
    setcc(SW_C3 or SW_C2 or SW_C0);
    (* Stack fault *)
    fpu_EXCEPTION(EX_StackUnder);
    Result := word(((varI387.soft.cwd <> 0) and (CW_Invalid <> 0)) = False);
    exit;
  end;

  st_ptr := st(nr);
  c := compare(st_ptr, FPU_gettagi(nr));
  if (c and COMP_NaN) <> 0 then
  begin
    setcc(SW_C3 or SW_C2 or SW_C0);
    if (c and COMP_SNaN) <> 0 then      (* This is the only difference between
        un-ordered and ordinary comparisons *)
    begin
      fpu_EXCEPTION(EX_Invalid);
      Result := word(((varI387.soft.cwd <> 0) and (CW_Invalid <> 0)) = False);
      exit;
    end;
    exit(0);
  end else
  case (c and 7) of
    COMP_A_lt_B:
      f := SW_C0;
    COMP_A_eq_B:
      f := SW_C3;
    COMP_A_gt_B:
      f := 0;
    COMP_No_Comp:
      f := SW_C3 or SW_C2 or SW_C0;
//{$ifdef PARANOID}
//      default:
//	EXCEPTION(EX_INTERNAL|$123);
//	f := SW_C3orSW_C2orSW_C0;
//	break;
//{$endif}(* PARANOID *)
  end;
  setcc(f);
  if (c and COMP_Denormal) <> 0 then
    exit(word(denormal_operand() < 0));
  Result := 0;
end;

(*---------------------------------------------------------------------------*)

procedure fcom_st ();
begin
  (* fcom st(i) *)
  compare_st_st(FPU_rm^);
end;

procedure fcompst ();
begin
  (* fcomp st(i) *)
  if compare_st_st(FPU_rm^) = 0 then
    FPU_pop();
end;

procedure fcompp ();
begin
  (* fcompp *)
  if FPU_rm^ <> 1 then
    //math_abort(FPU_info,SIGILL); !!! manca
    exit;
  if compare_st_st(1) = 0 then
    poppop();
end;

procedure fucom_ ();
begin
  (* fucom st(i) *)
  compare_u_st_st(FPU_rm^);
end;

procedure fucomp ();
begin
  (* fucomp st(i) *)
  if compare_u_st_st(FPU_rm^) = 0 then
    FPU_pop();
end;

procedure fucompp ();
begin
  (* fucompp *)
  if FPU_rm^ = 1 then
  begin
    if compare_u_st_st(1) = 0 then
      poppop();
  end else
    FPU_illegal();
end;

function normalize_no_excep(r: PFPU_REG; exp: intg; sign: intg): u_char;
begin
  setexponent16(r, exp);
  Result := FPU_normalize_nuo(r, 0);
  stdexp(r);
  if sign <> 0 then
    setnegative(r);
end;


function FPU_tagof(ptr: PFPU_REG): intg;
var
  exp: intg;
begin
  exp := exponent16(ptr)  and $7fff;
  if exp = 0 then
  begin
    if ( (ptr^.sigh or ptr^.sigl) )=0 then
      Exit(TAG_Zero);
    (* The number is a de-normal or pseudodenormal. *)
    Exit(TAG_Special);
  end;

  if ( exp = $7fff ) then
    (* Is an Infinity, a NaN, or an unsupported data type. *)
    Exit(TAG_Special);

  if ( (ptr^.sigh  and $80000000)=0 ) then
    Exit(TAG_Special);
    (* Unsupported data type. *)
    (* Valid numbers have the ms bit set to 1. *)
    (* Unnormal. *)

  Result := TAG_Valid;
end;


(* Get a long double from user memory *)
function FPU_load_extended(s:extended; stnr:intg):intg;
var
   sti_ptr:PFPU_REG;
begin
  sti_ptr := st(stnr);

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, @s, 10);
{$ifndef USE_WITH_CPU_SIM}
  __copy_from_user(sti_ptr, s, 10);
{$else}
  //FPU_get_user(sti_ptr^.sigl, (u32*)(((u8*)s)+0)); !!!
  //FPU_get_user(sti_ptr^.sigh, (u32*)(((u8*)s)+4)); !!!
  //FPU_get_user(sti_ptr^.exp,  (u16*)(((u8*)s)+8)); !!!
{$endif}
  //RE_ENTRANT_CHECK_ON;

  Result:= FPU_tagof(sti_ptr);
end;


(* Get a double from user memory *)
function FPU_load_double(dfloat:pdouble; loaded_data:PFPU_REG):intg;
var
  exp, tag, negative:intg;
  m64, l64:u32;
begin

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, dfloat, 8);
  m64:=FPU_get_user(Pointer(Sizeof(Single) + LongWord(dfloat)),Sizeof(Single));
  l64:=FPU_get_user(dfloat,Sizeof(Single));
  //RE_ENTRANT_CHECK_ON;

  negative := ifthen((m64 and $80000000)<>0,SIGN_Negative,SIGN_Positive);
  exp := ((m64 and $7ff00000) shr 20) - DOUBLE_Ebias + EXTENDED_Ebias;
  m64 := m64 and $fffff;
  if ( exp > DOUBLE_Emax + EXTENDED_Ebias ) then
  begin
      (* Infinity or NaN *)
    if ((m64 = 0) and (l64 = 0)) then
    begin
      (* +- infinity *)
      loaded_data^.sigh := $80000000;
      loaded_data^.sigl := $00000000;
      exp := EXP_Infinity + EXTENDED_Ebias;
      tag := TAG_Special;
    end else
    begin
      (* Must be a signaling or quiet NaN *)
      exp := EXP_NaN + EXTENDED_Ebias;
      loaded_data^.sigh := (m64 shl 11)or$80000000;
      loaded_data^.sigh :=	  loaded_data^.sigh or l64 shr 21;
      loaded_data^.sigl := l64 shl 11;
      tag := TAG_Special;    (* The calling function must look for NaNs *)
    end;
  end else
  if ( exp < DOUBLE_Emin + EXTENDED_Ebias ) then
  begin
      (* Zero or de-normal *)
    if ((m64 = 0) and (l64 = 0)) then
    begin
      (* Zero *)
      reg_copy(@CONST_Z, loaded_data);
      exp := 0;
      tag := TAG_Zero;
    end else
    begin
      (* De-normal *)
      loaded_data^.sigh := m64 shl 11;
      loaded_data^.sigh :=	  loaded_data^.sigh or l64 shr 21;
      loaded_data^.sigl := l64 shl 11;

      Result:= normalize_no_excep(loaded_data, DOUBLE_Emin, negative)
       or (ifthen(denormal_operand() < 0 , FPU_Exception_c , 0));
    end;
  end else
  begin
    loaded_data^.sigh := (m64 shl 11)or$80000000;
    loaded_data^.sigh :=      loaded_data^.sigh or l64 shr 21;
    loaded_data^.sigl := l64 shl 11;

    tag := TAG_Valid;
  end;

  setexponent16(loaded_data, exp or negative);

  Result:= tag;
end;

(* Get a float from user memory *)
function FPU_load_single(single:pfloat; loaded_data:PFPU_REG):intg;
var
  m32:u32;
  exp, tag, negative:intg;
begin
  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, @single, 4);
  m32:=FPU_get_user(single, sizeof(single^));
  //RE_ENTRANT_CHECK_ON;

  negative := ifthen((m32  and $80000000)<>0,SIGN_Negative, SIGN_Positive);

  if ((m32 and $7fffffff)=0) then
  begin
    (* Zero *)
    reg_copy(@CONST_Z, loaded_data);
    addexponent(loaded_data, negative);
    Result := TAG_Zero;
    Exit;
  end;
  exp := ((m32  and $7f800000) shr 23) - SINGLE_Ebias + EXTENDED_Ebias;
  m32 := (m32  and $7fffff) shl 8;
  if ( exp < SINGLE_Emin + EXTENDED_Ebias ) then
  begin
    (* De-normals *)
    loaded_data^.sigh := m32;
    loaded_data^.sigl := 0;

    Result:= normalize_no_excep(loaded_data, SINGLE_Emin, negative) or
      ifthen(denormal_operand() < 0 , FPU_Exception_c , 0);
  end else
  if ( exp > SINGLE_Emax + EXTENDED_Ebias ) then
  begin
  (* Infinity or NaN *)
    if ( m32 = 0 ) then
    begin
      (* +- infinity *)
      loaded_data^.sigh := $80000000;
      loaded_data^.sigl := $00000000;
      exp := EXP_Infinity + EXTENDED_Ebias;
      tag := TAG_Special;
    end else
    begin
      (* Must be a signaling or quiet NaN *)
      exp := EXP_NaN + EXTENDED_Ebias;
      loaded_data^.sigh := m32 or $80000000;
      loaded_data^.sigl := 0;
      tag := TAG_Special;  (* The calling function must look for NaNs *)
    end;
  end else
  begin
    loaded_data^.sigh := m32 or $80000000;
    loaded_data^.sigl := 0;
    tag := TAG_Valid;
  end;
  setexponent16(loaded_data, exp or negative);  (* Set the sign. *)
  Result:= tag;
end;


(* Get a 64bit quantity from user memory *)
function FPU_load_int64(_s:ps64):intg;
var
  s:s64;
  sign:intg;
  st0_ptr:PFPU_REG;
  chunk0, chunk1:u32;
begin
  st0_ptr:= st(0);
  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, @_s, 8);
{$ifndef USE_WITH_CPU_SIM}
  copy_from_user(@s,_s,8);
{$else}
  begin
  chunk0:=FPU_get_user(PLongWord(PByte(_s)),1);
  chunk1:=FPU_get_user((PByte(LongWord(_s)+4)),1);
  s := chunk0;
  s :=  s or ((u64(chunk1) shl 32));
  end;
{$endif}
  //RE_ENTRANT_CHECK_ON;

  if (s = 0) then
    begin
      reg_copy(@CONST_Z, st0_ptr);
      Result := TAG_Zero;
      Exit;
    end;

  if (s > 0) then
    sign := SIGN_Positive
  else
  begin
    s := -s;
    sign := SIGN_Negative;
  end;

  st0_ptr^.sigh := s;

  Result:= normalize_no_excep(st0_ptr, 63, sign);
end;


(* Get a long from user memory *)
function FPU_load_int32(_s:ps32; loaded_data:PFPU_REG):intg;
var
  s:s32;
  negative:intg;
begin

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, _s, 4);
  s:=FPU_get_user(_s, sizeof(_s^));
  //RE_ENTRANT_CHECK_ON;

  if (s = 0) then
  begin
    reg_copy(@CONST_Z, loaded_data);
    Exit(TAG_Zero);
  end;

  if (s > 0) then
    negative := SIGN_Positive
  else
  begin
    s := -s;
    negative := SIGN_Negative;
  end;

  loaded_data^.sigh := s;
  loaded_data^.sigl := 0;

  Result := normalize_no_excep(loaded_data, 31, negative);
end;

(* Get a short from user memory *)
function FPU_load_int16(_s:ps16; loaded_data: PFPU_REG):intg;
var
  s, negative: intg;
begin
  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, _s, 2);
  (* Cast as short to get the sign extended. *)
  FPU_get_user(@s, _s^);
  //RE_ENTRANT_CHECK_ON;

  if (s = 0) then
  begin
    reg_copy(@CONST_Z, loaded_data);
    Exit(TAG_Zero);
  end;

  if (s > 0) then
    negative := SIGN_Positive
  else
  begin
    s := -s;
    negative := SIGN_Negative;
  end;

  loaded_data^.sigh := s shl 16;
  loaded_data^.sigl := 0;

  Result:= normalize_no_excep(loaded_data, 15, negative);
end;

(* Get a packed bcd array from user memory *)
function FPU_load_bcd(s: pu_char): intg;
var
  st0_ptr: PFPU_REG;
  pos: intg;
  bcd: u_char;
  l: s64;
  sign: intg;
begin
  st0_ptr := st(0);
  l := 0;

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ, s, 10);
  //RE_ENTRANT_CHECK_ON;
  pos:=8;
  while pos >= 0 do
  begin
    l := l * 10;
    //RE_ENTRANT_CHECK_OFF;
    FPU_get_user(@bcd, u_char(s)+pos);
    //RE_ENTRANT_CHECK_ON;
    l :=      l + bcd shr 4;
    l := l * 10;
    l :=      l + bcd  and $0f;
    dec(pos);
  end;

  //RE_ENTRANT_CHECK_OFF;
  FPU_get_user(@sign, u_char(s)+9);
  sign := ifthen((sign and $80)<>0,SIGN_Negative,SIGN_Positive);
  //RE_ENTRANT_CHECK_ON;

  if l = 0 then
  begin
      reg_copy(@CONST_Z, st0_ptr);
      addexponent(st0_ptr, sign);   (* Set the sign. *)
      Exit(TAG_Zero);
  end else
  begin
      st0_ptr^.sigh := l;
      Result:= normalize_no_excep(st0_ptr, 63, sign);
      Exit;
  end;
end;

(*=====================================:=*)

(* Put a long double into user memory *)
function FPU_store_extended(st0_ptr:PFPU_REG; st0_tag:u_char; d:pextended):intg;
begin
  (*
    The only exception raised by an attempt to store to an
    extended format is the Invalid Stack exception, i.e.
    attempting to store from an empty register.
   *)

  if ( st0_tag <> TAG_Empty ) then
  begin
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_WRITE, d, 10);

    FPU_put_user(st0_ptr^.sigl, pu32(d), sizeof(pu32(d)^));
    FPU_put_user(st0_ptr^.sigh, pu32(LongInt(d)+4),sizeof(pu32(d)^));//!!! vedere codice originale per i cast
    FPU_put_user(exponent16(st0_ptr), pu16(LongInt(d)+8),sizeof(pu16(d)^));
    //RE_ENTRANT_CHECK_ON;

    Exit(1);
  end;

  (* Empty register (stack underflow) *)
  fpu_EXCEPTION(EX_StackUnder);
  if ( varI387.soft.cwd  and CW_Invalid )<>0 then
  begin
    (* The masked response *)
    (* Put out the QNaN indefinite *)
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_WRITE,d,10);
    FPU_put_user(0, pu32(d),sizeof(pu32(d)^));
    FPU_put_user($c0000000, pu32(longint(d)+1),sizeof(pu32(d)^));
    FPU_put_user($ffff, ps16(longint(d)+4),sizeof(ps16(d)^));
    //RE_ENTRANT_CHECK_ON;
    Exit(1);
  end else
    Exit(0);
end;

(* Put a double into user memory *)
function FPU_store_double(st0_ptr: PFPU_REG; st0_tag: u_char; dfloat: pdouble): intg;
var
  l: array[0..2] of u32;
  increment : u32;	(* aprocedure gcc warnings *)
  precision_loss: intg;
  exp: intg;
  tmp: FPU_REG;
  label denormal_arg, overflow;
begin
  increment := 0;	(* aprocedure gcc warnings *)

  if st0_tag = TAG_Valid then
  begin
    reg_copy(@st0_ptr, @tmp);
    exp := exponent(@tmp);

    if ( exp < DOUBLE_Emin ) then     (* It may be a denormal *)
    begin
      addexponent(@tmp, -DOUBLE_Emin + 52);  (* largest exp to be 51 *)

denormal_arg:
      precision_loss := FPU_round_to_int(@tmp, st0_tag);

      if precision_loss <> 0 then
      begin
{$ifdef PECULIAR_486}
	      (* Did it round to a non-denormal ? *)
	      (* This behaviour might be regarded as peculiar, it appears
		 that the 80486 rounds to the dest precision, then
		 converts to decide underflow. *)
        if ( ((tmp.sigh = $00100000) and (tmp.sigl = 0) and ((st0_ptr^.sigl and $000007ff)<>0) ))=false then
{$endif} (* PECULIAR_486 *)
        begin
          fpu_EXCEPTION(EX_Underflow);
          (* This is a special case: see sec 16.2.5.1 of
             the 80486 book *)
          if ( (varI387.soft.cwd  and CW_Underflow)=0 ) then
              Exit(0);
        end;
	      fpu_EXCEPTION(precision_loss);
        if (varI387.soft.cwd  and CW_Precision) = 0 then
          Exit(0);
      end;
      l[0] := tmp.sigl;
      l[1] := tmp.sigh;
    end else
    begin
      if ( tmp.sigl  and $000007ff )<>0 then
      begin
            precision_loss := 1;
        case (varI387.soft.cwd  and CW_RC) of
          RC_RND:
            begin
              (* Rounding can get a little messy.. *)
              increment := Word((tmp.sigl  and $7ff) > $400)or (* nearest *)
                Word((tmp.sigl  and $c00) = $c00);            (* odd ^. even *)
            end;
          RC_DOWN:   (* towards -infinity *)
            begin
              increment := ifthen(signpositive(@tmp)<>0, 0 , tmp.sigl  and $7ff);
            end;
          RC_UP:     (* towards +infinity *)
            begin
              increment := ifthen(signpositive(@tmp)<>0, tmp.sigl  and $7ff , 0);
            end;
          RC_CHOP:
            begin
              increment := 0;
            end;
        end;

        (* Truncate the mantissa *)
        tmp.sigl := tmp.sigl and $fffff800;

        if ( increment )<>0 then
        begin
          if ( tmp.sigl >= $fffff800 ) then
          begin
            (* the sigl part overflows *)
            if ( tmp.sigh = $ffffffff ) then
            begin
              (* The sigh part overflows *)
              tmp.sigh := $80000000;
              inc(exp);
              if exp >= EXP_OVER then
                goto overflow;
            end else
              inc(tmp.sigh);
            tmp.sigl := $00000000;
          end else
            (* We only need to increment sigl *)
            tmp.sigl :=		      tmp.sigl + $00000800;
        end;
      end else
        precision_loss := 0;

      l[0] := (tmp.sigl shr 11)or(tmp.sigh shl 21);
      l[1] := ((tmp.sigh shr 11)  and $fffff);

      if exp > DOUBLE_Emax then
      begin
        overflow:
	      fpu_EXCEPTION(EX_Overflow);
        if ( (varI387.soft.cwd  and CW_Overflow) = 0 ) then
          Exit(0);
	      set_precision_flag_up();
        if ( (varI387.soft.cwd  and CW_Precision) = 0 ) then
          Exit(1);

         Exit(0);

	      (* This is a special case: see sec 16.2.5.1 of the 80486 book *)
	      (* Overflow to infinity *)
	      l[0] := $00000000;	(* Set to *)
	      l[1] := $7ff00000;	(* + INF *)
      end else
      begin
        if ( precision_loss )<>0 then
        begin
          if increment <> 0 then
            set_precision_flag_up()
          else
            set_precision_flag_down();
        end;
        (* Add the exponent *)
        l[1] := l[1] or (((exp+DOUBLE_Ebias)  and $7ff) shl 20);
      end;
    end;
  end else
  if (st0_tag = TAG_Zero) then
  begin
    (* Number is zero *)
    l[0] := 0;
    l[1] := 0;
  end else
  if ( st0_tag = TAG_Special ) then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if st0_tag = TW_Denormal then
    begin
      (* A denormal will always underflow. *)
    {$ifndef PECULIAR_486}
      (* An 80486 is supposed to be able to generate
         a denormal exception here, but... *)
      (* Underflow has priority. *)
      if ( varI387.soft.cwd  and CW_Underflow )<>0 then
        denormal_operand();
    {$endif} (* PECULIAR_486 *)
      reg_copy(st0_ptr, @tmp);
      goto denormal_arg;
    end else
    if st0_tag = TW_Infinity then
    begin
      l[0] := 0;
      l[1] := $7ff00000;
    end else
    if st0_tag = TW_NaN then
    begin
      (* Is it really a NaN ? *)
      if ( (exponent(st0_ptr) = EXP_OVER) and ((st0_ptr^.sigh  and $80000000)<>0) ) then
      begin
        (* See if we can get a valid NaN from the FPU_REG *)
        l[0] := (st0_ptr^.sigl shr 11)or(st0_ptr^.sigh shl 21);
        l[1] := ((st0_ptr^.sigh shr 11)  and $fffff);
        if ( (st0_ptr^.sigh  and $40000000)=0 ) then
        begin
          (* It is a signalling NaN *)
          fpu_EXCEPTION(EX_Invalid);
          if ( (varI387.soft.cwd  and CW_Invalid)=0 ) then
                Exit(0);
          l[1] :=  l[1] or ($40000000 shr 11);
        end;
          l[1] := l[1] or $7ff00000;
      end else
      begin
        (* It is an unsupported data type *)
        fpu_EXCEPTION(EX_Invalid);
        if ( (varI387.soft.cwd  and CW_Invalid)=0 ) then
            Exit(0);
        l[0] := 0;
        l[1] := $fff80000;
      end;
    end;
  end else
  if st0_tag = TAG_Empty then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    if ( varI387.soft.cwd  and CW_Invalid )<>0 then
    begin
      (* The masked response *)
      (* Put out the QNaN indefinite *)
      //RE_ENTRANT_CHECK_OFF;
      FPU_verify_area(VERIFY_WRITE,dfloat,8);
      FPU_put_user(0, pu32(dfloat),sizeof(pu32(dfloat)^));
      FPU_put_user($fff80000, pu32(longint(dfloat)+1),sizeof(pu32(dfloat)^));
      //RE_ENTRANT_CHECK_ON;
      Exit(1);
    end else
      exit(0);
  end;

  if getsign(st0_ptr) <> 0 then
    l[1] :=    l[1] or $80000000;

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,dfloat,8);
  FPU_put_user(l[0], pu32(dfloat),sizeof(pu32(dfloat)^));
  FPU_put_user(l[1], pu32(longint(dfloat)+1),sizeof(pu32(dfloat)^));
  //RE_ENTRANT_CHECK_ON;

  Result:= 1;
end;


(* Put a float into user memory *)
function FPU_store_single(st0_ptr:PFPU_REG; st0_tag:u_char; single:psingle):intg;
var
  templ: s32;
  increment : u32;
  precision_loss: intg;
  exp: intg;
  tmp: FPU_REG;
  sigh: u32;
  sigl: u32;
  label denormal_arg, overflow;
begin
  increment := 0;     	(* aprocedure gcc warnings *)

  if ( st0_tag = TAG_Valid ) then
  begin
    reg_copy(st0_ptr, @tmp);
    exp := exponent(@tmp);

    if ( exp < SINGLE_Emin ) then
    begin
      addexponent(@tmp, -SINGLE_Emin + 23);  (* largest exp to be 22 *)

denormal_arg:
       precision_loss := FPU_round_to_int(@tmp, st0_tag);
      if precision_loss <> 0 then
      begin
  {$ifdef PECULIAR_486}
          (* Did it round to a non-denormal ? *)
          (* This behaviour might be regarded as peculiar, it appears
       that the 80486 rounds to the dest precision, then
       converts to decide underflow. *)
        if ( (tmp.sigl = $00800000) and  (((st0_ptr^.sigh and $000000ff) or st0_ptr^.sigl)<>0) )=false then
  {$endif} (* PECULIAR_486 *)
        begin
          fpu_EXCEPTION(EX_Underflow);
        (* This is a special case: see sec 16.2.5.1 of
           the 80486 book *)
        if ( (varI387.soft.cwd  and CW_Underflow)=0 ) then
              exit(0);
        end;
	      fpu_EXCEPTION(precision_loss);
        if ( (varI387.soft.cwd  and CW_Precision)=0 ) then
               exit(0);
      end;
      templ := tmp.sigl;
    end else
    begin
      if ( tmp.sigl or (tmp.sigh  and $000000ff) )<>0 then
      begin
	      sigh := tmp.sigh;
	      sigl := tmp.sigl;

	      precision_loss := 1;
        case (varI387.soft.cwd  and CW_RC) of
          RC_RND:
          begin
           increment := word((sigh  and $ff) > $80)       (* more than half *)
                     or word(((sigh  and $ff) = $80) and (sigl<>0))   (* more than half *)
                     or word((sigh  and $180) = $180);        (* round to even *)
          end;
          RC_DOWN:   (* towards -infinity *)
          begin
            increment := ifthen(signpositive(@tmp)<>0, 0 , word(sigl or (sigh  and $ff)));
          end;
          RC_UP:     (* towards +infinity *)
          begin
            increment := ifthen(signpositive(@tmp)<>0,(sigl or(sigh  and $ff)), 0);
          end;
          RC_CHOP:
          begin
            increment := 0;
          end;
        end;

	      (* Truncate part of the mantissa *)
	      tmp.sigl := 0;

        if increment <> 0 then
        begin
          if ( sigh >= $ffffff00 ) then
          begin
            (* The sigh part overflows *)
            tmp.sigh := $80000000;
            inc(exp);
            if ( exp >= EXP_OVER ) then
              goto overflow;
          end else
          begin
            tmp.sigh := tmp.sigh and $ffffff00;
            tmp.sigh :=	tmp.sigh + $100;
          end;
        end else
        begin
          tmp.sigh := tmp.sigh and $ffffff00;  (* Finish the truncation *)
        end;
      end else
        precision_loss := 0;

      templ := (tmp.sigh shr 8)  and $007fffff;

      if exp > SINGLE_Emax then
      begin
overflow:
        fpu_EXCEPTION(EX_Overflow);
        if ( (varI387.soft.cwd  and CW_Overflow)=0 ) then
          Exit(0);
        set_precision_flag_up();
        if ( (varI387.soft.cwd  and CW_Precision)=0 ) then
          Exit(0);

	      (* This is a special case: see sec 16.2.5.1 of the 80486 book. *)
	      (* Masked response is overflow to infinity. *)
	      templ := $7f800000;
      end else
      begin
        if precision_loss <> 0 then
        begin
          if increment <> 0 then
            set_precision_flag_up()
          else
            set_precision_flag_down();
        end;
	      (* Add the exponent *)
	      templ :=	 templ or ((exp+SINGLE_Ebias)  and $ff) shl 23;
      end;
    end;
  end else
  if st0_tag = TAG_Zero then
      templ := 0
  else
  if st0_tag = TAG_Special then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if (st0_tag = TW_Denormal) then
    begin
      reg_copy(st0_ptr, @tmp);

	  (* A denormal will always underflow. *)
//{$ifndef PECULIAR_486}
//	  (* An 80486 is supposed to be able to generate
//	     a denormal exception here, but... *)
//	  (* Underflow has priority. *)
//      if ( varI387.soft.cwd  and CW_Underflow )
//        denormal_operand();
//  {$endif} (* PECULIAR_486 *)
//      goto denormal_arg;
    end else
    if (st0_tag = TW_Infinity) then
	    templ := $7f800000
    else
    if (st0_tag = TW_NaN) then
    begin
	  (* Is it really a NaN ? *)
      if ( (exponent(st0_ptr) = EXP_OVER) and ((st0_ptr^.sigh  and $80000000)<>0) ) then
      begin
	      (* See if we can get a valid NaN from the FPU_REG *)
	      templ := st0_ptr^.sigh shr 8;
        if ( (st0_ptr^.sigh  and $40000000)=0 ) then
        begin
          (* It is a signalling NaN *)
          fpu_EXCEPTION(EX_Invalid);
          if ( (varI387.soft.cwd  and CW_Invalid)=0 ) then
              exit(0);
          templ :=		  templ or ($40000000 shr 8);
        end;
          templ :=	      templ or $7f800000;
      end else
      begin
	      (* It is an unsupported data type *)
	      fpu_EXCEPTION(EX_Invalid);
        if ( (varI387.soft.cwd  and CW_Invalid)=0 ) then
          exit(0);
	      templ := $ffc00000;
      end;
    end;
//{$ifdef PARANOID}
//    else
//	begin
//	  EXCEPTION(EX_INTERNAL|$164);
//	  return 0;
//	end;
//{$endif}
  end else
  if ( st0_tag = TAG_Empty ) then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    if ( varI387.soft.cwd  and EX_Invalid )<>0 then
    begin
      (* The masked response *)
      (* Put out the QNaN indefinite *)
      //RE_ENTRANT_CHECK_OFF;
      FPU_verify_area(VERIFY_WRITE,single,4);
      FPU_put_user($ffc00000, pu32(single),sizeof(pu32(single)^));
      //RE_ENTRANT_CHECK_ON;
      Exit(1);
    end else
      Exit(1);
  end;
//{$ifdef PARANOID}
//  else
//    begin
//      EXCEPTION(EX_INTERNAL|$163);
//      return 0;
//    end;
//{$endif}
  if getsign(st0_ptr) <> 0 then
    templ :=    templ or $80000000;

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,single,4);
  FPU_put_user(templ,pu32(single),sizeof(pu32(single)^));
  //RE_ENTRANT_CHECK_ON;

  Result := 1;
end;

(* Put a 64bit quantity into user memory *)
function FPU_store_int64(st0_ptr: PFPU_REG; st0_tag: u_char; d: s64): intg;
var
  t: FPU_REG;
  tll: s64;
  precision_loss: intg;
  label invalid_operand;
begin
  if ( st0_tag = TAG_Empty ) then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    goto invalid_operand;
  end else
  if ( st0_tag = TAG_Special ) then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if ( (st0_tag = TW_Infinity) or (st0_tag = TW_NaN) ) then
    begin
      fpu_EXCEPTION(EX_Invalid);
      goto invalid_operand;
    end;
  end;

  reg_copy(st0_ptr, @t);
  precision_loss := FPU_round_to_int(@t, st0_tag);
{$ifndef EMU_BIG_ENDIAN}
  pu32(@tll)^ := t.sigl;
  pu32(longint(@tll)+1)^ := t.sigh;
  //((u32 *)@tll)[0] := t.sigl;
  //((u32 *)@tll)[1] := t.sigh;
{$else}
  pu32(@tll)^ := t.sigh;
  pu32(longint(@tll)+1)^ := t.sigl;
{$endif}
  if ( (precision_loss = 1) or (((t.sigh  and $80000000)<>0) and
       (((t.sigh = $80000000)=true) and (t.sigl = 0) and (signnegative(@t)<>0)) )) then
  begin
    fpu_EXCEPTION(EX_Invalid);
    (* This is a special case: see sec 16.2.5.1 of the 80486 book *)
    invalid_operand:
    if ( varI387.soft.cwd  and EX_Invalid )<>0 then
    (* Produce something like QNaN 'indefinite' *)
      tll := $8000000000000000
    else
      Exit(0);
  end else
  begin
    if ( precision_loss )<>0 then
      set_precision_flag(precision_loss);
    if ( signnegative(@t) )<>0 then
      tll := - tll;
  end;
  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,@d,8);
{$ifndef USE_WITH_CPU_SIM}
  copy_to_user(d, @tll, 8);
{$else}
  FPU_put_user(u32(tll),  pu32(pu8(@d)),4);
  FPU_put_user(u32(tll shr 32), pu32(pu8(LongInt(@d)+4)),4);
{$endif}
  //RE_ENTRANT_CHECK_ON;
  Result := 1;
end;

(* Put a long into user memory *)
function FPU_store_int32(st0_ptr: PFPU_REG; st0_tag: u_char; d: ps32): intg;
var
  t: FPU_REG;
  precision_loss: intg;
  label invalid_operand;
begin

  if ( st0_tag = TAG_Empty ) then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    goto invalid_operand;
  end else
  if ( st0_tag = TAG_Special ) then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if ( (st0_tag = TW_Infinity) or (st0_tag = TW_NaN) ) then
    begin
      fpu_EXCEPTION(EX_Invalid);
      goto invalid_operand;
    end;
  end;

  reg_copy(st0_ptr, @t);
  precision_loss := FPU_round_to_int(@t, st0_tag);
  if ((t.sigh<>0) or (((t.sigl and $80000000)<>0) and
       ((t.sigl = $80000000) and (signnegative(@t)<>0))=false) ) then
  begin
    fpu_EXCEPTION(EX_Invalid);
    (* This is a special case: see sec 16.2.5.1 of the 80486 book *)
invalid_operand:
    if ( varI387.soft.cwd  and EX_Invalid )<>0 then
      (* Produce something like QNaN 'indefinite' *)
      t.sigl := $80000000
    else
      exit(0);
  end else
  begin
    if ( precision_loss )<>0 then
      set_precision_flag(precision_loss);
    if ( signnegative(@t) )<>0 then
      t.sigl := -s32(t.sigl);
  end;

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,d,4);
  FPU_put_user(t.sigl, pu32(d),4);
  //RE_ENTRANT_CHECK_ON;

  Result := 1;
end;

(* Put a short into user memory *)
function FPU_store_int16(st0_ptr: PFPU_REG; st0_tag: u_char; d: ps16): intg;
var
  t: FPU_REG;
  precision_loss: intg;

  label invalid_operand;
begin

  if ( st0_tag = TAG_Empty ) then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    goto invalid_operand;
  end else
  if ( st0_tag = TAG_Special ) then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if ( (st0_tag = TW_Infinity) or (st0_tag = TW_NaN) ) then
    begin
      fpu_EXCEPTION(EX_Invalid);
      goto invalid_operand;
    end;
  end;

  reg_copy(st0_ptr, @t);
  precision_loss := FPU_round_to_int(@t, st0_tag);
  if (t.sigh<>0) or (((t.sigl and $ffff8000)<>0) and (signnegative(@t)<>0))=false then
  begin
    fpu_EXCEPTION(EX_Invalid);
    (* This is a special case: see sec 16.2.5.1 of the 80486 book *)
    invalid_operand:
    if ( varI387.soft.cwd  and EX_Invalid )<>0 then
      (* Produce something like QNaN 'indefinite' *)
      t.sigl := $8000
    else
      exit(0);
  end else
  begin
    if ( precision_loss )<>0 then
      set_precision_flag(precision_loss);
    if ( signnegative(@t)<>0 ) then
      t.sigl := -t.sigl;
  end;

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,d,2);
  FPU_put_user(s16(t.sigl),ps16(d),sizeof(s16));
  //RE_ENTRANT_CHECK_ON;

  Result := 1;
end;

(* Put a packed bcd array into user memory *)
function FPU_store_bcd(st0_ptr: PFPU_REG; st0_tag: u_char; d: u_char): intg;
var
  t: FPU_REG;
  ll: u64;
  b: u_char;
  i, precision_loss: intg;
  sign: u_char;
  label invalid_operand;
begin
  sign:=ifthen(getsign(st0_ptr) = SIGN_NEG , $80 , 0);
  if ( st0_tag = TAG_Empty ) then
  begin
    (* Empty register (stack underflow) *)
    fpu_EXCEPTION(EX_StackUnder);
    goto invalid_operand;
  end
  else if ( st0_tag = TAG_Special ) then
  begin
    st0_tag := FPU_Special(st0_ptr);
    if ( (st0_tag = TW_Infinity) or (st0_tag = TW_NaN) ) then
    begin
      fpu_EXCEPTION(EX_Invalid);
      goto invalid_operand;
    end;
  end;

  reg_copy(st0_ptr, @t);
  precision_loss := FPU_round_to_int(@t, st0_tag);
  ll := significand(@t)^;

  (* Check for overflow, by comparing with 999999999999999999 decimal. *)
  if ( (t.sigh > $0de0b6b3) or
      ((t.sigh = $0de0b6b3) and (t.sigl > $a763ffff)) ) then
  begin
    fpu_EXCEPTION(EX_Invalid);
    (* This is a special case: see sec 16.2.5.1 of the 80486 book *)
invalid_operand:
    if ( varI387.soft.cwd  and CW_Invalid )<>0 then
    begin
      (* Produce the QNaN 'indefinite' *)
      //RE_ENTRANT_CHECK_OFF;
      FPU_verify_area(VERIFY_WRITE,@d,10);
      i := 0;

      while i < 7 do
      begin
        FPU_put_user(0, pu_char(longint(@d)+i),sizeof(d)); (* These bytes 'undefined' *)
        inc(i);
      end;

      FPU_put_user($c0, pu_char(longint(d)+7),sizeof(d)); (* This byte 'undefined' *)
      FPU_put_user($ff, pu_char(longint(d)+8),sizeof(d));
      FPU_put_user($ff, pu_char(longint(d)+9),sizeof(d));
      //RE_ENTRANT_CHECK_ON;
      exit(1);
    end else
      exit(0);
  end else
  if ( precision_loss )<>0 then
    (* Precision loss doesn't stop the data transfer *)
    set_precision_flag(precision_loss);

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,@d,10);
  //RE_ENTRANT_CHECK_ON;
  i := 0;
  while i < 9 do
  begin
    b := FPU_div_small(@ll, 10);
    b :=      b or (FPU_div_small(@ll, 10)) shl 4;
    //RE_ENTRANT_CHECK_OFF;
    FPU_put_user(b,pu_char(longint(@d)+i),sizeof(d));
    //RE_ENTRANT_CHECK_ON;
    inc(i);
  end;
  //RE_ENTRANT_CHECK_OFF;
  FPU_put_user(sign,pu_char(longint(@d)+9),sizeof(d));
  //RE_ENTRANT_CHECK_ON;

  Result := 1;
end;

(*=====================================:=*)

(* r gets mangled such that sig is int, sign:
   it is NOT normalized *)
(* The return value (in eax) is zero if the result is exact,
   if bits are changed due to rounding, truncation, etc, then
   a non-zero value is returned *)
(* Overflow is signalled by a non-zero return value (in eax).
   In the case of overflow, the returned significand always has the
   largest possible value *)
function FPU_round_to_int(r:PFPU_REG; tag:u_char):intg;
var
  very_big: u_char;
  eax: longword;
begin
  if (tag = TAG_Zero) then
  begin
    (* Make sure that zero is returned *)
    r^.sigh := 0;
    exit(0);
  end;

  if (exponent(r) > 63) then
  begin
    r^.sigl := r^.sigh and not 0;      (* The largest representable number *)
    exit(1);
  end;

{$ifndef EMU_BIG_ENDIAN}
  eax := FPU_shrxs(@r^.sigl, 63 - exponent(r));
{$else}
  eax := FPU_shrxs(@r^.sigh, 63 - exponent(r));
{$endif}
  very_big := word((not (r^.sigh) or not(r^.sigl))=0);  (* test for $fff...fff *)
  case (varI387.soft.cwd  and CW_RC) of
    RC_RND:
    begin
      if (word((eax  and $80000001) = $80000001))<>0(* nearest *)
        or word(((bx_cpu.eax and $80000000)<>0) and ((r^.sigl and 1)<>0)) then	(* odd ^. even *)
      begin
        if ( very_big )<>0 then
          exit(1);

        inc(Significand(r)^);
        Result := PRECISION_LOST_UP;
      end;
    end;
    RC_DOWN:
    begin
      if (eax<>0) and (getsign(r)<>0) then
      begin
        if ( very_big )<>0 then
          exit(1);

        inc(r^.sigh);
        Result := PRECISION_LOST_UP;
      end;
    end;
    RC_UP:
    begin
      if (eax<>0) and (getsign(r)=0) then
      begin
        if ( very_big )<>0 then
          exit(1);
        inc(r^.sigh);
        Result := PRECISION_LOST_UP;
      end;
    end;
    RC_CHOP:
    begin
    end;
  end;
  Result := ifthen(bx_cpu.eax<>0 , PRECISION_LOST_DOWN , 0);
end;

(*=====================================:=*)

function fldenv(addr_modes:fpu_addr_modes; s:pu_char):pu_char;
var
  tag_word: u16;
  tag: u_char;
  i: intg;
begin
  tag_word := 0;

  if ( (addr_modes.default_mode = VM86) or
      ((addr_modes.default_mode = PM16)
      or (addr_modes.override_.operand_size = OP_SIZE_PREFIX)) ) then
  begin
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_READ, s, $0e);
    varI387.soft.cwd:=FPU_get_user(s, u16(s));
    varI387.soft.swd:=FPU_get_user(pu16(longint(s)+2), 2);
    tag_word:=FPU_get_user(pu16(longint(s)+4),2);
    Paddress(varI387.soft.fip)^.offset:=FPU_get_user(pu16(longint(s)+6),2);
    Paddress(varI387.soft.fip)^.selector:=FPU_get_user(pu16(longint(s)+8), 2);
    Paddress(varI387.soft.fip)^.offset:=FPU_get_user(pu16(longint(s)+10),2);
    Paddress(varI387.soft.fip)^.selector:=FPU_get_user(pu16(longint(s)+12),2);
    //RE_ENTRANT_CHECK_ON;
    longint(s) := longint(s) + $0e;
    if ( addr_modes.default_mode = VM86 ) then
    begin
      Paddress(varI387.soft.fip)^.offset := Paddress(varI387.soft.fip)^.offset + (Paddress(varI387.soft.fip)^.selector  and $f000) shl 4;
      Paddress(varI387.soft.fip)^.offset := Paddress(varI387.soft.fip)^.offset + (Paddress(varI387.soft.fip)^.selector and $f000) shl 4;
    end;
  end else
  begin
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_READ, s, $1c);
    varI387.soft.cwd:=FPU_get_user(s, Sizeof(u16(s)));
    varI387.soft.swd:=FPU_get_user(pu16(longint(s)+4), 2);
    tag_word:=FPU_get_user(pu16(longint(s)+8),2);
    Paddress(@varI387.soft.fip)^.opcode:=FPU_get_user(pu16(longint(s)+12),2);
    Paddress(@varI387.soft.fip)^.offset:=FPU_get_user(pu16(longint(s)+14),4);
    Paddress(@varI387.soft.fip)^.selector:=FPU_get_user(pu16(longint(s)+18),4);

    //RE_ENTRANT_CHECK_ON;
    longint(s) := longint(s) + $1c;
  end;

{$ifdef PECULIAR_486}
  varI387.soft.cwd := varI387.soft.cwd and not $e080;
{$endif} (* PECULIAR_486 *)

  varI387.soft.ftop := (varI387.soft.swd shr SW_Top_Shift) and 7;

  if word(varI387.soft.swd<>0) and (not (varI387.soft.cwd and CW_Exceptions))<>0  then
    varI387.soft.swd := varI387.soft.swd or (SW_Summary or SW_Backward)
  else
    varI387.soft.swd := varI387.soft.swd and not (SW_Summary or SW_Backward);

  i := 0;
  while i < 8 do
  begin
    tag := varI387.soft.swd  and 3;
    varI387.soft.swd :=      varI387.soft.swd shr 2;

    if ( tag = TAG_Empty ) then
(* New tag is empty.  Accept it *)
      FPU_settag(i, TAG_Empty)
    else if ( FPU_gettag(i) = TAG_Empty ) then
    begin
      (* Old tag is empty and new tag is not empty.  New tag is determined
         by old reg contents *)
      if ( exponent(fpu_register(i)) = - EXTENDED_Ebias ) then
      begin
        if ( (fpu_register(i).sigl<>0) or (fpu_register(i).sigh<>0)=false) then
          FPU_settag(i, TAG_Zero)
        else
          FPU_settag(i, TAG_Special);
      end else
      if ( exponent(fpu_register(i)) = $7fff - EXTENDED_Ebias ) then
        FPU_settag(i, TAG_Special)
      else
      if ( fpu_register(i).sigh  and $80000000 )<>0 then
        FPU_settag(i, TAG_Valid)
      else
        FPU_settag(i, TAG_Special);   (* An Un-normal *)
    end;
    (* Else old tag is not empty and new tag is not empty.  Old tag
     remains correct *)
   inc(i);
  end;

  Result := s;
end;

procedure frstor(addr_modes: fpu_addr_modes; data_address: pu_char);
var
  i, regnr: intg;
  s: pu_char;
  offset, other: intg;
  fpu_reg_p: PFPU_REG;
begin
  s := fldenv(addr_modes, data_address);
  offset := (top  and 7) * sizeof(FPU_REG);
  other := 8*sizeof(FPU_REG) - offset;

  (* Copy all registers in stack order. *)
  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_READ,s,80);
{$ifndef USE_WITH_CPU_SIM}
//  __copy_from_user(register_base+offset, s, other);
//  if ( offset )
//    __copy_from_user(register_base, s+other, offset);
{$else}
  begin
    fpu_reg_p := PFPU_REG(longint(register_base)+offset);
    while other > 0 do
    begin
      fpu_reg_p^.sigl:=FPU_get_user(pu_char(longint(s)), 4);
      fpu_reg_p^.sigh:=FPU_get_user(pu_char(longint(s)+4),4);
      fpu_reg_p^.exp:=FPU_get_user(pu_char(longint(s)+8),4);
      inc(fpu_reg_p);
      longint(s) := longint(s) + 10;
      other :=  other - sizeof(FPU_REG);
    end;
    fpu_reg_p := PFPU_REG(longint(register_base));
    while offset > 0 do
    begin
      fpu_reg_p^.sigl:=FPU_get_user(pu_char(longint(s)), 4);
      fpu_reg_p^.sigh:=FPU_get_user(pu_char(longint(s)+4),4);
      fpu_reg_p^.exp:=FPU_get_user(pu_char(longint(s)+8),4);
      inc(fpu_reg_p);
  {    FPU_get_user(fpu_reg_p^.sigl, (u32*)(s+0));
      FPU_get_user(fpu_reg_p^.sigh, (u32*)(s+4));
      FPU_get_user(fpu_reg_p^.exp,  (u16*)(s+8));
      fpu_reg_p++;}
      longint(s) := longint(s) + 10;
      offset :=    offset - sizeof(FPU_REG);
    end;
  end;
{$endif}
  //RE_ENTRANT_CHECK_ON;

  i := 0;
  while i < 8 do
  begin
    regnr := (i+top)  and 7;
    if ( FPU_gettag(regnr) <> TAG_Empty ) then
    (* The loaded data over-rides all other cases. *)
      FPU_settag(regnr, FPU_tagof(st(i)));
    inc(i);
  end;
end;

function fstenv(addr_modes: fpu_addr_modes; d: pu_char): pu_char;
begin
  if ((addr_modes.default_mode = VM86) or
      ((addr_modes.default_mode = PM16) or
       (addr_modes.override_.operand_size = OP_SIZE_PREFIX))) then
  begin
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_WRITE, d, 14);
{$ifdef PECULIAR_486}
    FPU_put_user(varI387.soft.cwd  and not $e080, pu32(d),sizeof(d^));
{$else}
    FPU_put_user(varI387.soft.cwd, (u16 *) d);
{$endif} (* PECULIAR_486 *)
    FPU_put_user(((varI387.soft.swd) and not $3800 and $ffff) or (((varI387.soft.ftop) shl 11) and $3800), pu16(longint(d)+2),sizeof(d^));
    FPU_put_user(Paddress(varI387.soft.fip)^.offset, pu16(longint(d)+4),sizeof(d^));
    FPU_put_user(Paddress(varI387.soft.fip)^.offset, pu16(longint(d)+6),sizeof(d^));
    FPU_put_user(Paddress(varI387.soft.fip)^.offset, pu16(longint(d)+10),sizeof(d^));
    if ( addr_modes.default_mode = VM86 ) then
    begin
      FPU_put_user((Paddress(varI387.soft.fip)^.offset  and $f0000) shr 4, pu16(longint(d)+8),sizeof(d^));
      FPU_put_user((Paddress(varI387.soft.fip)^.offset  and $f0000) shr 4, pu16(longint(d)+10),sizeof(d^));
    end else
    begin
      FPU_put_user((Paddress(varI387.soft.fip)^.selector) shr 4, pu16(longint(d)+8),sizeof(d^));
      FPU_put_user((Paddress(varI387.soft.fip)^.selector) shr 4, pu16(longint(d)+10),sizeof(d^));
    end;
    //RE_ENTRANT_CHECK_ON;
    longint(d) :=      longint(d) + $0e;
  end else
  begin
    //RE_ENTRANT_CHECK_OFF;
    FPU_verify_area(VERIFY_WRITE, d, 7*4);
{$ifdef PECULIAR_486}
    varI387.soft.cwd := varI387.soft.cwd and not $e080;
    (* An 80486 sets nearly all of the reserved bits to 1. *)
    varI387.soft.cwd :=      varI387.soft.cwd or $ffff0040;
    varI387.soft.swd := ((varI387.soft.swd) and not $3800 and $ffff) or (((varI387.soft.ftop) shl 11) and $3800) or $ffff0000;
    varI387.soft.twd :=      varI387.soft.twd or $ffff0000;
    varI387.soft.fcs := varI387.soft.fcs and not $f8000000;
    varI387.soft.fos :=      varI387.soft.fos or $ffff0000;
{$endif} (* PECULIAR_486 *)
{$ifndef USE_WITH_CPU_SIM}
    __copy_to_user(d, @varI387.soft.cwd, 7*4);
{$else}
    FPU_put_user(u32(varI387.soft.cwd), pu32(pu8(d)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.swd), pu32(pu8(longint(d)+4)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.twd), pu32(pu8(longint(d)+8)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.fip), pu32(pu8(longint(d)+12)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.fcs), pu32(pu8(longint(d)+16)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.foo), pu32(pu8(longint(d)+20)),sizeof(d^));
    FPU_put_user(u32(varI387.soft.fos), pu32(pu8(longint(d)+24)),sizeof(d^));
{$endif}
    //RE_ENTRANT_CHECK_ON;
    longint(d) :=  longint(d) + $1c;
  end;

  varI387.soft.cwd := varI387.soft.cwd or CW_Exceptions;
  varI387.soft.swd := varI387.soft.swd and not (SW_Summary or SW_Backward);

  Result := d;
end;

procedure fsave(addr_modes: fpu_addr_modes; data_address: pu_char);
var
  d: pu_char;
  offset, other: intg;
  fpu_reg_p: PFPU_REG;
begin
  offset := (top  and 7) * sizeof(FPU_REG);
  other := 8*sizeof(FPU_REG) - offset;

  d := fstenv(addr_modes, data_address);

  //RE_ENTRANT_CHECK_OFF;
  FPU_verify_area(VERIFY_WRITE,d,80);

  (* Copy all registers in stack order. *)
{$ifndef USE_WITH_CPU_SIM}
//  __copy_to_user(d, register_base+offset, other);
//  if ( offset )
//    __copy_to_user(d+other, register_base, offset);
{$else}
  begin

    fpu_reg_p := PFPU_REG(longint(register_base)+offset);
    while other > 0 do
    begin
      FPU_put_user(fpu_reg_p^.sigl, pu32(d),sizeof(d^));
      FPU_put_user(fpu_reg_p^.sigh, pu32(longint(d)+4),sizeof(d^));
      FPU_put_user(fpu_reg_p^.exp,  pu16(longint(d)+8),sizeof(d^));
      inc(fpu_reg_p);
      longint(d) := longint(d) + 10;
      other :=    other - sizeof(FPU_REG);
    end;
    fpu_reg_p := PFPU_REG(register_base);
    while offset > 0 do
    begin
      FPU_put_user(fpu_reg_p^.sigl, pu32(d),sizeof(d^));
      FPU_put_user(fpu_reg_p^.sigh, pu32(longint(d)+4),sizeof(d^));
      FPU_put_user(fpu_reg_p^.exp,  pu16(longint(d)+8),sizeof(d^));
      inc(fpu_reg_p);
      longint(d) :=  longint(d) + 10;
      offset :=    offset - sizeof(FPU_REG);
    end;
  end;
{$endif}
  //RE_ENTRANT_CHECK_ON;
  finit();
end;

function FPU_load_store (type_: u_char; addr_modes: fpu_addr_modes;
  data_address: pointer): intg;
var
  loaded_data: FPU_REG;
  st0_ptr: PFPU_REG;
  st0_tag: u_char;  (* This is just to stop a gcc warning. *)
  loaded_tag: u_char;
begin
  st0_tag := TAG_Empty;  (* This is just to stop a gcc warning. *)
  st0_ptr := 0;    (* Initialized just to stop compiler warnings. *)

  if (addr_modes.default_mode and PROTECTED_) <> 0 then
  begin
    if (addr_modes.default_mode = SEG32) then
    begin
      if (varI387.soft.alimit < data_sizes_32[type_]) then
      begin
        math_abort(nil, 0);
        exit;
      end;
    end else
    if (addr_modes.default_mode = PM16) then
    begin
      if (varI387.soft.alimit < data_sizes_16[type_]) then
      begin
        math_abort(nil, 0);
        exit;
      end;
    end;
//{$ifdef PARANOID}
//    else
//EXCEPTION(EX_INTERNAL|$140);
//{$endif}(* PARANOID *)
  end;

  case (type_table[type_]) of
    _NONE_:
      begin
      end;
    _REG0_:
      begin
        st0_ptr := st(0);       (* Some of these instructions pop after storing *)
        st0_tag := FPU_gettag0();
      end;
    _PUSH_:
      begin
        if (FPU_gettagi(-1) <> TAG_Empty) then
        begin
          FPU_stack_overflow();
          exit(0);
        end;
        Dec(varI387.soft.ftop);
        st0_ptr := st(0);
      end;
    _null_:
      begin
        FPU_illegal();
        exit(0);
      end;
  end;

  case (type_) of
    000:       (* fld m32real *)
      begin
        clear_C1();
        loaded_tag := FPU_load_single(data_address, @loaded_data);
        if ((loaded_tag = TAG_Special) and (isNaN(@loaded_data) <> 0) and
          (real_1op_NaN(@loaded_data) < 0)) then
        begin
          Inc(varI387.soft.ftop);
          exit;
        end;
        FPU_copy_to_reg0(@loaded_data, loaded_tag);
      end;
    001:      (* fild m32int *)
      begin
        clear_C1();
        loaded_tag := FPU_load_int32(ps32(data_address), @loaded_data);
        FPU_copy_to_reg0(@loaded_data, loaded_tag);
      end;
    002:      (* fld m64real *)
      begin
        clear_C1();
        loaded_tag := FPU_load_double(data_address, @loaded_data);
        if ((loaded_tag = TAG_Special) and (isNaN(@loaded_data) <> 0) and
          (real_1op_NaN(@loaded_data) < 0)) then
        begin
          Inc(varI387.soft.ftop);
          exit;
        end;
        FPU_copy_to_reg0(@loaded_data, loaded_tag);
      end;
    003:      (* fild m16int *)
      begin
        clear_C1();
        loaded_tag := FPU_load_int16(ps16(data_address), @loaded_data);
        FPU_copy_to_reg0(@loaded_data, loaded_tag);
      end;
    8:      (* fst m32real *)
      begin
        clear_C1();
        FPU_store_single(st0_ptr, st0_tag, psingle(data_address));
      end;
    9:      (* fist m32int *)
      begin
        clear_C1();
        FPU_store_int32(st0_ptr, st0_tag, ps32(data_address));
      end;
    10:     (* fst m64real *)
      begin
        clear_C1();
        FPU_store_double(st0_ptr, st0_tag, pdouble(data_address));
      end;
    11:     (* fist m16int *)
      begin
        clear_C1();
        FPU_store_int16(st0_ptr, st0_tag, ps16(data_address));
      end;
    12:     (* fstp m32real *)
      begin
        clear_C1();
        if (FPU_store_single(st0_ptr, st0_tag, psingle(data_address))) <> 0 then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    13:     (* fistp m32int *)
      begin
        clear_C1();
        if (FPU_store_int32(st0_ptr, st0_tag, ps32(data_address))) <> 0 then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    14:     (* fstp m64real *)
      begin
        clear_C1();
        if (FPU_store_double(st0_ptr, st0_tag, pdouble(data_address)) <> 0) then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    15:     (* fistp m16int *)
      begin
        clear_C1();
        if (FPU_store_int16(st0_ptr, st0_tag, ps16(data_address)) <> 0) then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    16:     (* fldenv  m14/28byte *)
      begin
        fldenv(addr_modes, pu_char(data_address));
        (* Ensure that the values just loaded are not changed by fix-up operations. *)
        exit(1);
      end;
    18:     (* frstor m94/108byte *)
      begin
        frstor(addr_modes, pu_char(data_address));
        (* Ensure that the values just loaded are not changed by fix-up operations. *)
        exit(1);
      end;
    19:     (* fbld m80dec *)
      begin
        clear_C1();
        loaded_tag := FPU_load_bcd(pu_char(data_address));
        FPU_settag0(loaded_tag);
      end;
    20:     (* fldcw *)
      begin
        //RE_ENTRANT_CHECK_OFF;
        FPU_verify_area(VERIFY_READ, data_address, 2);
        varI387.soft.cwd := FPU_get_user(pu16(data_address), sizeof(u16));
        //RE_ENTRANT_CHECK_ON;
        if ((varI387.soft.swd <> 0) and ((not varI387.soft.cwd) <> 0) and
          (CW_Exceptions <> 0)) then
          varI387.soft.swd := varI387.soft.swd or (SW_Summary or SW_Backward)
        else
          varI387.soft.swd := varI387.soft.swd and not (SW_Summary or SW_Backward);
  //{$ifdef PECULIAR_486}
  //      control_word :=      control_word or $40;  (* An 80486 appears to always set this bit *)
  //{$endif}(* PECULIAR_486 *)
        exit(1);
      end;
    21:      (* fld m80real *)
      begin
        clear_C1();
        loaded_tag := FPU_load_extended(pextended(data_address)^, 0);
        FPU_settag0(loaded_tag);
      end;
    23:      (* fild m64int *)
      begin
        clear_C1();
        loaded_tag := FPU_load_int64(ps64(data_address));
        FPU_settag0(loaded_tag);
      end;
    24:     (* fstenv  m14/28byte *)
      begin
        fstenv(addr_modes, pu_char(data_address));
        exit(1);
      end;
    26:      (* fsave *)
      begin
        fsave(addr_modes, pu_char(data_address));
        exit(1);
      end;
    27:      (* fbstp m80dec *)
      begin
        clear_C1();
        if (FPU_store_bcd(st0_ptr, st0_tag, pu_char(data_address)^) <> 0) then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    28:      (* fstcw m16int *)
      begin
        //RE_ENTRANT_CHECK_OFF;
        FPU_verify_area(VERIFY_WRITE, data_address, 2);
        FPU_put_user(varI387.soft.cwd, pu16(data_address), sizeof(u16));
        //RE_ENTRANT_CHECK_ON;
        exit(1);
      end;
    29:      (* fstp m80real *)
      begin
        clear_C1();
        if (FPU_store_extended(st0_ptr, st0_tag, pextended(data_address))) <> 0 then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
    30:      (* fstsw m2byte *)
      begin
        //RE_ENTRANT_CHECK_OFF;
        FPU_verify_area(VERIFY_WRITE, data_address, 2);
        FPU_put_user(status_word(), pu16(data_address), sizeof(u16));
        //RE_ENTRANT_CHECK_ON;
        Result := 1;
      end;
    31:      (* fistp m64int *)
      begin
        clear_C1();
        if (FPU_store_int64(st0_ptr, st0_tag, ps64(data_address)^)) <> 0 then
          pop_0();  (* pop only if the number was actually stored
         (see the 80486 manual p16-28) *)
      end;
  end;
  Result := 0;
end;

//procedure fclex;
//  begin
//    varI387.soft.swd := varI387.soft.swd and not (SW_Backward or SW_Summary or
//      SW_Stack_Fault or SW_Precision or SW_Underflow or SW_Overflow or
//      SW_Zero_Div or SW_Denorm_Op or SW_Invalid);
//    varI387.soft.no_update := 1;
//  end;

(* Needs to be externally visible *)
procedure finit;
begin
  if not alreadygone then
    fp_fpu := fileopen('C:\bochs-1.4.1\merge\fpu$.bin', fmOpenRead);
  alreadygone := True;
  varI387.soft.cwd  := $037f;
  varI387.soft.swd  := 0;
  varI387.soft.ftop := 0;
  (* We don't keep varI387.soft.ftop in the status word internally. *)
  varI387.soft.twd  := $ffff;
(* The behaviour is different from that detailed in
   Section 15.1.6 of the Intel manual *)
  paddress(@varI387.soft.foo)^.offset := 0;
  paddress(@varI387.soft.foo)^.selector := 0;
  paddress(@varI387.soft.fip)^.offset := 0;
  paddress(@varI387.soft.fip)^.selector := 0;
  paddress(@varI387.soft.fip)^.opcode := 0;
  varI387.soft.no_update := 1;
end;

(*
 * These are nops on the varI387..
 *)
 //#define feni fnop
 //#define fdisi fnop
 //#define fsetpm fnop

procedure finit_;
const
  finit_table: array[0..7] of procedure = (
    fnop, fnop, fclex, finit,
    fnop, FPU_illegal, FPU_illegal, FPU_illegal);
var
  PosProc: integer;
begin
  PosProc := FPU_rm^;
  (finit_table[PosProc]);
end;

procedure fstsw_ax;
begin
  varfpu_cpu_ptr^.gen_reg[0].rx := (status_word()); // KPL
  varI387.soft.no_update := 1;
end;

procedure fstsw_ ();
const
  fstsw_table: array[0..7] of procedure = (
    fstsw_ax, FPU_illegal, FPU_illegal, FPU_illegal,
    FPU_illegal, FPU_illegal, FPU_illegal, FPU_illegal);
begin
  (fstsw_table[FPU_rm^]);
end;

const
  fp_nop_table: array[0..7] of procedure = (
    fnop, FPU_illegal, FPU_illegal, FPU_illegal,
    FPU_illegal, FPU_illegal, FPU_illegal, FPU_illegal);

procedure fp_nop ();
begin
  (fp_nop_table[FPU_rm^]);
end;

procedure fld_i_;
var
  st_new_ptr: PFPU_REG;
  i: intg;
  tag: u_char;
begin
  if FPU_stackoverflow(st_new_ptr) <> 0 then
  begin
    FPU_stack_overflow();
    exit;
  end;

  (* fld st(i) *)
  i := FPU_rm^;
  if (FPU_empty_i(i)) <> 0 then
  begin
    reg_copy(st(i), st_new_ptr);
    tag := FPU_gettagi(i);
    Dec(varI387.soft.ftop);
    FPU_settag0(tag);
  end else
  begin
    if (varI387.soft.cwd and CW_Invalid) <> 0 then
      (* The masked response *)
      FPU_stack_underflow()
    else
      fpu_EXCEPTION(EX_StackUnder);
  end;
end;

procedure fxch_i ();
var
  t: FPU_REG;
  i: intg;
  st0_ptr, sti_ptr: PFPU_REG;
  tag_word: s32;
  regnr, regnri: intg;
  st0_tag: u_char;
  sti_tag: u_char;
begin
  (* fxch st(i) *)
  i := fpu_rm^;
  st0_ptr := st(0);
  sti_ptr := st(i);
  tag_word := varI387.soft.twd;
  regnr := varI387.soft.ftop and 7;
  regnri := ((regnr + i) and 7);
  st0_tag := (tag_word shr (regnr * 2)) and 3;
  sti_tag := (tag_word shr (regnri * 2)) and 3;

  if st0_tag = TAG_Empty then
  begin
    if sti_tag = TAG_Empty then
    begin
      FPU_stack_underflow();
      FPU_stack_underflow_i(i);
      exit;
    end;

    if (varI387.soft.cwd and CW_Invalid) <> 0 then
      (* Masked response *)
      FPU_copy_to_reg0(sti_ptr, sti_tag);
    FPU_stack_underflow_i(i);
    exit;
  end;

  if sti_tag = TAG_Empty then
  begin
    if (varI387.soft.cwd and CW_Invalid) <> 0 then
      (* Masked response *)
      FPU_copy_to_regi(st0_ptr, st0_tag, i);
    FPU_stack_underflow();
    exit;
  end;
  clear_C1();

  reg_copy(st0_ptr, @t);
  reg_copy(sti_ptr, st0_ptr);
  reg_copy(@t, sti_ptr);

  tag_word := tag_word and not (3 shl (regnr * 2)) and not (3 shl (regnri * 2));
  tag_word := tag_word or (sti_tag shl (regnr * 2)) or (st0_tag shl (regnri * 2));
  varI387.soft.twd := tag_word;
end;


procedure ffree_ ();
begin
  (* ffree st(i) *)
  FPU_settagi(FPU_rm^, TAG_Empty);
end;


procedure ffreep ();
begin
  (* ffree st(i) + pop - unofficial code *)
  FPU_settagi(FPU_rm^, TAG_Empty);
  FPU_pop();
end;


procedure fst_i_ ();
begin
  (* fst st(i) *)
  FPU_copy_to_regi(st(0), FPU_gettag0(), FPU_rm^);
end;


procedure fstp_i ();
begin
  (* fstp st(i) *)
  FPU_copy_to_regi(st(0), FPU_gettag0(), FPU_rm^);
  FPU_pop();
end;

procedure fchs (st0_ptr: PFPU_REG; st0tag: u_char);
begin
  if (st0tag or TAG_Empty) <> 0 then
  begin
    signbyte(st0_ptr)^ := SIGN_NEG;
    clear_C1();
  end else
    FPU_stack_underflow();
end;

procedure fpu_fabs (st0_ptr: PFPU_REG; st0tag: u_char);
begin
  if (st0tag or TAG_Empty) <> 0 then
  begin
    setpositive(st0_ptr);
    clear_C1();
  end else
    FPU_stack_underflow();
end;

procedure ftst_ (st0_ptr: PFPU_REG; st0tag: u_char);
begin
  case st0tag of
    TAG_Zero:
      setcc(SW_C3);
    TAG_Valid:
      begin
        if (getsign(st0_ptr) = SIGN_POS) then
          setcc(0)
        else
          setcc(SW_C0);
      end;
    TAG_Special:
      begin
        case (FPU_Special(st0_ptr)) of
          TW_Denormal:
            begin
              if getsign(st0_ptr) = SIGN_POS then
                setcc(0)
              else
                setcc(SW_C0);

              if denormal_operand() < 0 then
              begin
    {$ifdef PECULIAR_486}
          (* This is weird! *)
          if (getsign(st0_ptr) = SIGN_POS) then
            setcc(SW_C3);
    {$endif}      (* PECULIAR_486 *)
                exit;
              end;
            end;
          TW_NaN:
            begin
              setcc(SW_C0 or SW_C2 or SW_C3);   (* Operand is not comparable *)
              fpu_EXCEPTION(EX_Invalid);
            end;
          TW_Infinity:
            begin
              if (getsign(st0_ptr) = SIGN_POS) then
                setcc(0)
              else
                setcc(SW_C0);
            end;
        else
          begin
            setcc(SW_C0 or SW_C2 or SW_C3);   (* Operand is not comparable *)
            fpu_EXCEPTION(EX_INTERNAL or $14);
          end;
        end;
      end;
    TAG_Empty:
      begin
        setcc(SW_C0 or SW_C2 or SW_C3);
        fpu_EXCEPTION(EX_StackUnder);
      end;
  end;
end;

procedure fxam (st0_ptr: PFPU_REG; st0tag: u_char);
var
  c: intg;
begin
  c := 0;
  case (st0tag) of
    TAG_Empty:
      c := SW_C3 or SW_C0;
    TAG_Zero:
      c := SW_C3;
    TAG_Valid:
      c := SW_C2;
    TAG_Special:
      begin
        case (FPU_Special(st0_ptr)) of
          TW_Denormal:
            begin
              c := SW_C2 or SW_C3;  (* Denormal *)
            end;
          TW_NaN:
            begin
              (* We also use NaN for unsupported types. *)
              if (((st0_ptr^.sigh and $8000) <> 0) and (exponent(st0_ptr) = EXP_OVER)) then
                c := SW_C0;
            end;
          TW_Infinity:
            c := SW_C2 or SW_C0;
        end;
      end;
  end;
  if (getsign(st0_ptr) = SIGN_NEG) then
    c := c or SW_C1;
  setcc(c);
end;

procedure FPU_illegalST0 (st0_ptr: PFPU_REG; st0tag: u_char);
begin
  //math_abort
end;

const
  fp_etc_table: array[0..7] of FUNC_ST0 = (
    fchs, fpu_fabs, FPU_illegalST0, FPU_illegalST0,
    ftst_, fxam, FPU_illegalST0, FPU_illegalST0);

procedure FPU_etc_;
var
  P: func_st0;
begin
  p := fp_etc_table[FPU_rm^];
  p(st(0), FPU_gettag0());
end;

function trig_arg (st0_ptr: PFPU_REG; flags: intg): intg;
var
  tmp: FPU_REG;
  tmptag: u_char;
  q: u64;
  old_cw, saved_status: intg;
  tag, st0_tag: intg;
begin
  old_cw  := varI387.soft.cwd;
  saved_status := varI387.soft.swd;
  st0_tag := TAG_Valid;

  if exponent(st0_ptr) >= 63 then
  begin
    varI387.soft.swd := varI387.soft.swd or SW_C2;     (* Reduction incomplete. *)
    Result := -1;
  end;

  if (flags and FPTAN_) <> 0 then
    Inc(st0_ptr^.exp);         (* Effectively base the following upon pi/4 *)

  varI387.soft.cwd := varI387.soft.cwd and not CW_RC;
  varI387.soft.cwd := varI387.soft.cwd or RC_CHOP;

  setpositive(st0_ptr);
  tag := FPU_u_div(st0_ptr, @CONST_PI2, @tmp, PR_64_BITS or RC_CHOP or $3f, SIGN_POS);

  FPU_round_to_int(@tmp, tag);  (* Fortunately, this can't overflow
         to 2^64 *)
  q := significand(@tmp)^;

  if (q) <> 0 then
  begin
    rem_kernel(significand(st0_ptr)^, significand(@tmp), significand(@CONST_PI2)^,
      q, exponent(st0_ptr) - exponent(@CONST_PI2));
    setexponent16(@tmp, exponent(@CONST_PI2));
    st0_tag := FPU_normalize_nuo(@tmp, EXTENDED_Ebias);  (* No underflow or overflow
                                                    is possible *)

    FPU_copy_to_reg0(@tmp, st0_tag);
  end;

  if ((flags and FCOS_) <> 0) and ((q and 1) = 0) or
    (((flags and FCOS_) = 0) and ((q and 1) <> 0)) then
  begin
    st0_tag := FPU_sub(REV or LOADED or TAG_Valid, @CONST_PI2, FULL_PRECISION);
    //bbd: arg2 used to typecast to (int)

{$ifdef BETTER_THAN_486}
    (* So far, the results are exact but based upon a 64 bit
 precision approximation to pi/2. The technique used
 now is equivalent to using an approximation to pi/2 which
 is accurate to about 128 bits. *)
    if ((exponent(st0_ptr) <= exponent(@CONST_PI2extra) + 64) or (q > 1)) then
    begin
  (* This code gives the effect of having pi/2 to better than
     128 bits precision. *)

      significand(@tmp)^ := q + 1;
      setexponent16(@tmp, 63);
      FPU_normalize_nuo(@tmp, EXTENDED_Ebias);  (* No underflow or overflow
                                               is possible *)
      tmptag :=
        FPU_u_mul(@CONST_PI2extra, @tmp, @tmp, FULL_PRECISION,
        SIGN_POS, exponent(@CONST_PI2extra) + exponent(@tmp));
      setsign(@tmp, getsign(@CONST_PI2extra));
      st0_tag := FPU_add(@tmp, tmptag, 0, FULL_PRECISION);
      if ((signnegative(st0_ptr) <> 0) and ((flags and FPTAN_) = 0)) then
      begin
      (* CONST_PI2extra is negative, so the result of the addition
   can be negative. This means that the argument is actually
   in a different quadrant. The correction is always < pi/2,
   so it can't overflow into yet another quadrant. *)
            (* The function is even, so we need just adjust the sign
               and q. *)
        setpositive(st0_ptr);
        Inc(q);
      end;
    end;
{$endif}(* BETTER_THAN_486 *)
  end
{$ifdef BETTER_THAN_486}
  else
  begin
    (* So far, the results are exact but based upon a 64 bit
 precision approximation to pi/2. The technique used
 now is equivalent to using an approximation to pi/2 which
 is accurate to about 128 bits. *)
    if (((q > 0) and (exponent(st0_ptr) <= exponent(@CONST_PI2extra) + 64)) or
      (q > 1)) then
    begin
  (* This code gives the effect of having p/2 to better than
     128 bits precision. *)

      significand(@tmp)^ := q;
      setexponent16(@tmp, 63);
      FPU_normalize_nuo(@tmp,
        EXTENDED_Ebias);  (* No underflow or overflow
                                               is possible.
                                               This must return TAG_Valid *)
      tmptag := FPU_u_mul(@CONST_PI2extra, @tmp, @tmp, FULL_PRECISION,
        SIGN_POS, exponent(@CONST_PI2extra) + exponent(@tmp));
      setsign(@tmp, getsign(@CONST_PI2extra));
      st0_tag := FPU_sub(LOADED or (tmptag and $0f), @tmp, FULL_PRECISION);
      if ((exponent(st0_ptr) = exponent(@CONST_PI2)) and
        ((st0_ptr^.sigh > CONST_PI2.sigh) or
        ((st0_ptr^.sigh = CONST_PI2.sigh) and (st0_ptr^.sigl > CONST_PI2.sigl)))) then
      begin
      (* CONST_PI2extra is negative, so the result of the
   subtraction can be larger than pi/2. This means
   that the argument is actually in a different quadrant.
   The correction is always < pi/2, so it can't overflow
   into yet another quadrant.
         bbd: arg2 used to typecast to (int), corrupting 64-bit ptrs
       *)
        st0_tag := FPU_sub(REV or LOADED or TAG_Valid, @CONST_PI2, FULL_PRECISION);
        Inc(q);
      end;
    end;
  end;
{$endif}(* BETTER_THAN_486 *)

  FPU_settag0(st0_tag);
  varI387.soft.cwd := old_cw;
  varI387.soft.swd := saved_status and not SW_C2;     (* Reduction complete. *)

  if (flags and FPTAN_) <> 0 then
  begin
    Dec(st0_ptr^.exp);
    Result := q and 7;
    exit;
  end;

  Result := (q and 3) or (flags and FCOS_);
end;

(* Convert a s32 to register *)
procedure convert_l2reg (arg: ps32; deststnr: intg);
var
  tag:  intg;
  num:  s32;
  sign: u_char;
  dest: PFPU_REG;
begin
  dest := st(deststnr);

  if (num = 0) then
  begin
    FPU_copy_to_regi(@CONST_Z, TAG_Zero, deststnr);
    exit;
  end;

  if (num > 0) then
    sign := SIGN_POS
  else
  begin
    num  := -num;
    sign := SIGN_NEG;
  end;

  dest^.sigh := num;
  dest^.sigl := 0;
  setexponent16(dest, 31);
  tag := FPU_normalize_nuo(dest, EXTENDED_Ebias);  (* No underflow or overflow
                                            is possible *)
  FPU_settagi(deststnr, tag);
  setsign(dest, sign);
end;

procedure single_arg_error (st0_ptr: PFPU_REG; st0_tag: u_char);
begin
  if (st0_tag = TAG_Empty) then
    FPU_stack_underflow()  (* Puts a QNaN in st(0) *)
  else
    if (st0_tag = TW_NaN) then
      real_1op_NaN(st0_ptr);       (* return with a NaN in st(0) *)
//{$ifdef PARANOID}
//    else
//  EXC EPTION(EX_INTERNAL|$0112);
//{$endif}(* PARANOID *)
end;

procedure single_arg_2_error (st0_ptr: PFPU_REG; st0_tag: u_char);
var
  isNaN: intg;
begin

  case (st0_tag) of
    TW_NaN:
      begin
        isNaN := word((exponent(st0_ptr) = EXP_OVER) and
          ((st0_ptr^.sigh and $80000000) <> 0));
        if ((isNaN <> 0) and ((st0_ptr^.sigh and $40000000) = 0)) then
          (* Signaling ? *)
        begin
          fpu_EXCEPTION(EX_Invalid);
          if (varI387.soft.cwd and CW_Invalid) <> 0 then
          begin
            (* The masked response *)
            (* Convert to a QNaN *)
            st0_ptr^.sigh := st0_ptr^.sigh or $40000000;
            //inc(varI387.soft.ftop);
            push();
            FPU_copy_to_reg0(st0_ptr, TAG_Special);
          end;
        end else
          if (isNaN) <> 0 then
          begin
            (* A QNaN *)
            push();
            FPU_copy_to_reg0(st0_ptr, TAG_Special);
          end else
          begin
            (* pseudoNaN or other unsupported *)
            fpu_EXCEPTION(EX_Invalid);
            if (varI387.soft.cwd and CW_Invalid) <> 0 then
            begin
              (* The masked response *)
              FPU_copy_to_reg0(@CONST_QNaN, TAG_Special);
              push();
              FPU_copy_to_reg0(@CONST_QNaN, TAG_Special);
            end;
          end;
      end;              (* return with a NaN in st(0) *)
//{$ifdef PARANOID}
//  default:
//    EXCEPTION(EX_INTERNAL|$0112);
//{$endif}(* PARANOID *)
  end;
end;

(*---------------------------------------------------------------------------*)

procedure f2xm1 (st0_ptr: PFPU_REG; tag: u_char);
var
  a: FPU_REG;
label
  denormal_arg;
begin

  clear_C1();

  if (tag = TAG_Valid) then
  begin
    (* For an 80486 FPU, the result is undefined if the arg is >= 1.0 *)
    if (exponent(st0_ptr) < 0) then
    begin
      denormal_arg:

        FPU_to_exp16(st0_ptr, @a);

      (* poly_2xm1(x) requires 0 < st(0) < 1. *)
      poly_2xm1(getsign(st0_ptr), @a, st0_ptr);
    end;
    set_precision_flag_up();   (* 80486 appears to always do this *)
    exit;
  end;

  if (tag = TAG_Zero) then
    exit;

  if (tag = TAG_Special) then
    tag := FPU_Special(st0_ptr);

  case (tag) of
    TW_Denormal:
      begin
        if denormal_operand() < 0 then
          exit;
        goto denormal_arg;
      end;
    TW_Infinity:
      begin
        if signnegative(st0_ptr) <> 0 then
        begin
          (* -infinity gives -1 (p16-10) *)
          FPU_copy_to_reg0(@CONST_1, TAG_Valid);
          setnegative(st0_ptr);
        end;
      end;
  else
    single_arg_error(st0_ptr, tag);
  end;
end;

procedure fptan (st0_ptr: PFPU_REG; st0_tag: u_char);
const
  invert: array[0..7] of byte = (0, 1, 1, 0, 0, 1, 1, 0);
var
  st_new_ptr: PFPU_REG;
  q: u32;
  arg_sign: u_char;

label
  denormal_arg;
begin
  (* Stack underflow has higher priority *)
  if st0_tag = TAG_Empty then
  begin
    FPU_stack_underflow();  (* Puts a QNaN in st(0) *)
    if (varI387.soft.cwd and CW_Invalid) <> 0 then
    begin
      st_new_ptr := st(-1);
      push();
      FPU_stack_underflow();  (* Puts a QNaN in the new st(0) *)
    end;
    exit;
  end;

  if (FPU_stackoverflow(st_new_ptr)) <> 0 then
  begin
    FPU_stack_overflow();
    exit;
  end;

  if (st0_tag = TAG_Valid) then
  begin
    if (exponent(st0_ptr) > -40) then
    begin
      q := trig_arg(st0_ptr, FPTAN_);
      if q = -1 then
      begin
        (* Operand is out of range *)
        exit;
      end;

      poly_tan(st0_ptr, invert[q]);
      setsign(st0_ptr, word(((q and 2) <> 0) or (arg_sign <> 0)));
      set_precision_flag_up();  (* We do not really know if up or down *)
    end else
    begin
      (* For a small arg, the result = the argument *)
      (* Underflow may happen *)

denormal_arg:

        FPU_to_exp16(st0_ptr, st0_ptr);

      st0_tag := FPU_round(st0_ptr, 1, 0, FULL_PRECISION, arg_sign);
      FPU_settag0(st0_tag);
    end;
    push();
    FPU_copy_to_reg0(@CONST_1, TAG_Valid);
    exit;
  end;

  if (st0_tag = TAG_Zero) then
  begin
    push();
    FPU_copy_to_reg0(@CONST_1, TAG_Valid);
    setcc(0);
    exit;
  end;

  if (st0_tag = TAG_Special) then
    st0_tag := FPU_Special(st0_ptr);

  if (st0_tag = TW_Denormal) then
  begin
    if (denormal_operand() < 0) then
      exit;

    goto denormal_arg;
  end;

  if (st0_tag = TW_Infinity) then
  begin
    (* The 80486 treats infinity as an invalid operand *)
    if arith_invalid(0) >= 0 then
    begin
      st_new_ptr := st(-1);
      push();
      arith_invalid(0);
    end;
    exit;
  end;

  single_arg_2_error(st0_ptr, st0_tag);
end;

procedure fxtract (st0_ptr: PFPU_REG; st0_tag: u_char);
var
  st_new_ptr: PFPU_REG;
  sign: u_char;
  st1_ptr: PFPU_REG;   (* anticipate *)
  e: s32;
label
  denormal_arg;
begin
  st1_ptr := st0_ptr;

  st1_ptr := st0_ptr;  (* anticipate *)

  if (FPU_stackoverflow(st_new_ptr)) <> 0 then
  begin
    FPU_stack_overflow();
    exit;
  end;

  clear_C1();

  if (st0_tag = TAG_Valid) then
  begin

    push();
    sign := getsign(st1_ptr);
    reg_copy(st1_ptr, st_new_ptr);
    setexponent16(st_new_ptr, exponent(st_new_ptr));

    denormal_arg:

      e := exponent16(st_new_ptr);
    convert_l2reg(@e, 1);
    setexponentpos(st_new_ptr, 0);
    setsign(st_new_ptr, sign);
    FPU_settag0(TAG_Valid);       (* Needed if arg was a denormal *)
    exit;
  end else
  if (st0_tag = TAG_Zero) then
  begin
    sign := getsign(st0_ptr);

    if (FPU_divide_by_zero(0, SIGN_NEG) < 0) then
      exit;

    push();
    FPU_copy_to_reg0(@CONST_Z, TAG_Zero);
    setsign(st_new_ptr, sign);
    exit;
  end;

  if st0_tag = TAG_Special then
    st0_tag := FPU_Special(st0_ptr);

  if st0_tag = TW_Denormal then
  begin
    if (denormal_operand() < 0) then
      exit;

    push();
    sign := getsign(st1_ptr);
    FPU_to_exp16(st1_ptr, st_new_ptr);
    goto denormal_arg;
  end else
  if st0_tag = TW_Infinity then
  begin
    sign := getsign(st0_ptr);
    setpositive(st0_ptr);
    push();
    FPU_copy_to_reg0(@CONST_INF, TAG_Special);
    setsign(st_new_ptr, sign);
    exit;
  end else
  if st0_tag = TW_NaN then
  begin
    if (real_1op_NaN(st0_ptr) < 0) then
      exit;

    push();
    FPU_copy_to_reg0(st0_ptr, TAG_Special);
    exit;
  end else
  if st0_tag = TAG_Empty then
  begin
    (* Is this the correct behaviour? *)
    if (varI387.soft.cwd and EX_Invalid) <> 0 then
    begin
      FPU_stack_underflow();
      push();
      FPU_stack_underflow();
    end else
      fpu_EXCEPTION(EX_StackUnder);
  end;
//{$ifdef PARANOID}
//else
//  EXCEPTION(EX_INTERNALor$119);
//{$endif}(* PARANOID *)
end;

procedure fdecstp (st0_ptr: PFPU_REG; st0_tag: u_char);
begin
  clear_C1();
  Dec(varI387.soft.ftop);
end;

procedure fincstp (st0_ptr: PFPU_REG; st0_tag: u_char);
begin
  clear_C1();
  Inc(varI387.soft.ftop);
end;

procedure fsqrt_ (st0_ptr: PFPU_REG; st0_tag: u_char);
var
  expon: intg;
  tag: u_char;

label
  denormal_arg;
begin

  clear_C1();

  if st0_tag = TAG_Valid then
  begin

    if signnegative(st0_ptr) <> 0 then
    begin
      arith_invalid(0);  (* sqrt(negative) is invalid *)
      exit;
    end;

    (* make st(0) in  [1.0 .. 4.0) *)
    expon := exponent(st0_ptr);

    denormal_arg:

      setexponent16(st0_ptr, (expon and 1));

    (* Do the computation, the sign of the result will be positive. *)
    tag := wm_sqrt(st0_ptr, 0, 0, varI387.soft.cwd, SIGN_POS);
    addexponent(st0_ptr, expon shr 1);
    FPU_settag0(tag);
    exit;
  end;

  if st0_tag = TAG_Zero then
    exit;

  if st0_tag = TAG_Special then
    st0_tag := FPU_Special(st0_ptr);

  if (st0_tag = TW_Infinity) then
  begin
    if (signnegative(st0_ptr)) <> 0 then
      arith_invalid(0);  (* sqrt(-Infinity) is invalid *)
    exit;
  end else
  if st0_tag = TW_Denormal then
  begin
    if signnegative(st0_ptr) <> 0 then
    begin
      arith_invalid(0);  (* sqrt(negative) is invalid *)
      exit;
    end;

    if denormal_operand() < 0 then
      exit;

    FPU_to_exp16(st0_ptr, st0_ptr);

    expon := exponent16(st0_ptr);

    goto denormal_arg;
  end;

  single_arg_error(st0_ptr, st0_tag);
end;

procedure frndint_ (st0_ptr: PFPU_REG; st0_tag: u_char);
var
  flags, tag: intg;
  sign: u_char;
label
  denormal_arg;
begin

  if (st0_tag = TAG_Valid) then
  begin
denormal_arg:
    sign := getsign(st0_ptr);

    if exponent(st0_ptr) > 63 then
      exit;

    if (st0_tag = TW_Denormal) then
    begin
      if denormal_operand() < 0 then
        exit;
    end;

    (* Fortunately, this can't overflow to 2^64 *)
    flags := FPU_round_to_int(st0_ptr, st0_tag);
    if flags <> 0 then
      set_precision_flag(flags);

    setexponent16(st0_ptr, 63);
    tag := FPU_normalize_nuo(st0_ptr, EXTENDED_Ebias);  (* No underflow or overflow
                                                is possible *)
    setsign(st0_ptr, sign);
    FPU_settag0(tag);
    exit;
  end;

  if st0_tag = TAG_Zero then
    exit;

  if st0_tag = TAG_Special then
    st0_tag := FPU_Special(st0_ptr);

  if st0_tag = TW_Denormal then
    goto denormal_arg
  else
    if st0_tag = TW_Infinity then
      exit
    else
      single_arg_error(st0_ptr, st0_tag);
end;

function fsin (st0_ptr: PFPU_REG; tag: u_char): intg;
var
  arg_sign: u_char;
  q: u32;
begin
  arg_sign := getsign(st0_ptr);

  if (tag = TAG_Valid) then
  begin

    if (exponent(st0_ptr) > -40) then
    begin
      q := trig_arg(st0_ptr, 0);
      if q = -1 then
        (* Operand is out of range *)
        Exit(1);

      poly_sine(st0_ptr);

      if (q and 2) <> 0 then
        changesign(st0_ptr);

      setsign(st0_ptr, getsign(st0_ptr) or arg_sign);

      (* We do not really know if up or down *)
      set_precision_flag_up();
      Exit(0);
    end else
    begin
      (* For a small arg, the result = the argument *)
      set_precision_flag_up();  (* Must be up. *)
      Exit(0);
    end;
  end;

  if tag = TAG_Zero then
  begin
    setcc(0);
    Exit(0);
  end;

  if tag = TAG_Special then
    tag := FPU_Special(st0_ptr);

  if tag = TW_Denormal then
  begin
    if denormal_operand() < 0 then
      Exit(1);

    (* For a small arg, the result = the argument *)
    (* Underflow may happen *)
    FPU_to_exp16(st0_ptr, st0_ptr);

    tag := FPU_round(st0_ptr, 1, 0, FULL_PRECISION, arg_sign);

    FPU_settag0(tag);

    Exit(0);
  end else
    if (tag = TW_Infinity) then
    begin
      (* The 80486 treats infinity as an invalid operand *)
      arith_invalid(0);
      Exit(1);
    end else
    begin
      single_arg_error(st0_ptr, tag);
      Exit(1);
    end;
end;

function f_cos (st0_ptr: PFPU_REG; tag: u_char): intg;
var
  st0_sign: u_char;
  q: u32;
label
  denormal_arg;
begin

  st0_sign := getsign(st0_ptr);

  if (tag = TAG_Valid) then
  begin

    if (exponent(st0_ptr) > -40) then
    begin
      if ((exponent(st0_ptr) < 0) or ((exponent(st0_ptr) = 0) and
          (significand(st0_ptr)^ <= $c90fdaa22168c234))) then
      begin
        poly_cos(st0_ptr);

        (* We do not really know if up or down *)
        set_precision_flag_down();

        Exit(0);
      end else
      begin
        q := trig_arg(st0_ptr, FCOS_);
        if q <> -1 then
        begin
          poly_sine(st0_ptr);

          if ((q + 1) and 2) <> 0 then
            changesign(st0_ptr);

          (* We do not really know if up or down *)
          set_precision_flag_down();

          Exit(0);
        end else
          (* Operand is out of range *)
          Exit(1);
      end;
    end else
      begin
denormal_arg:

        setcc(0);
      FPU_copy_to_reg0(@CONST_1, TAG_Valid);
{$ifdef PECULIAR_486}
  set_precision_flag_down();  (* 80486 appears to do this. *)
{$else}
      set_precision_flag_up();  (* Must be up. *)
{$endif}(* PECULIAR_486 *)
      Result := 0;
      Exit;
      end;
    end
  else
    if (tag = TAG_Zero) then
      begin
      FPU_copy_to_reg0(@CONST_1, TAG_Valid);
      setcc(0);
      Result := 0;
      exit;
      end;

  if (tag = TAG_Special) then
    tag := FPU_Special(st0_ptr);

  if (tag = TW_Denormal) then
    begin
    if (denormal_operand() < 0) then
      begin
      Result := 1;
      Exit;
      end;

    goto denormal_arg;
    end
  else
    if (tag = TW_Infinity) then
      begin
      (* The 80486 treats infinity as an invalid operand *)
      arith_invalid(0);
      Result := 1;
      Exit;
      end
    else
      begin
      single_arg_error(st0_ptr, tag);  (* requires st0_ptr = @st(0) *)
      Result := 1;
      Exit;
      end;
end;

procedure fcos (st0_ptr: PFPU_REG; st0_tag: u_char);
  begin
    f_cos(st0_ptr, st0_tag);
  end;


procedure fsincos (st0_ptr: PFPU_REG; st0_tag: u_char);
  var
    st_new_ptr: PFPU_REG;
    arg: FPU_REG;
    tag: u_char;
  begin

    (* Stack underflow has higher priority *)
    if (st0_tag = TAG_Empty) then
      begin
      FPU_stack_underflow();  (* Puts a QNaN in st(0) *)
      if (varI387.soft.cwd and CW_Invalid) <> 0 then
        begin
        st_new_ptr := st(-1);
        push();
        FPU_stack_underflow();  (* Puts a QNaN in the new st(0) *)
        end;
      exit;
      end;

    if (FPU_stackoverflow(st_new_ptr)) <> 0 then
      begin
      FPU_stack_overflow();
      exit;
      end;

    if (st0_tag = TAG_Special) then
      tag := FPU_Special(st0_ptr)
    else
      tag := st0_tag;

    if (tag = TW_NaN) then
      begin
      single_arg_2_error(st0_ptr, TW_NaN);
      exit;
      end
    else
      if (tag = TW_Infinity) then
        begin
        (* The 80486 treats infinity as an invalid operand *)
        if (arith_invalid(0) >= 0) then
          begin
          (* Masked response *)
          push();
          arith_invalid(0);
          end;
        exit;
        end;

    reg_copy(st0_ptr, @arg);
    if (fsin(st0_ptr, st0_tag)) = 0 then
      begin
      push();
      FPU_copy_to_reg0(@arg, st0_tag);
      f_cos(st(0), st0_tag);
      end
    else
      begin
      (* An error, so restore st(0) *)
      FPU_copy_to_reg0(@arg, st0_tag);
      end;
  end;


 (*---------------------------------------------------------------------------*)
 (* The following all require two arguments: st(0) and st(1) *)

(* A lean, mean kernel for the fprem instructions. This relies upon
   the division and rounding to an integer in do_fprem giving an
   exact result. Because of this, rem_kernel() needs to deal only with
   the least significant 64 bits, the more significant bits of the
   result must be zero.
 *)
procedure rem_kernel (st0: u64; y: Pu64; st1: u64; q: u64; n: intg);
  var
    x: u64;
    work: u64;
  begin

    x := st0 shl n;

    work := u32(st1);
    work := work * u32(q);
    x := x - work;

    work := st1 shr 32;
    work := work * u32(q);
    x := x - work;

    work := u32(st1);
    work := work * q shr 32;
    x := x - work;

    y^ := x;
  end;


(* Remainder of st(0) / st(1) *)
(* This routine produces exact results, i.e. there is never any
   rounding or truncation, etc of the result. *)
procedure do_fprem (st0_ptr: PFPU_REG; st0_tag: u_char; round: intg);
  var
    st1_ptr: PFPU_REG;
    st1_tag: u_char;
    tmp, st0, st1: FPU_REG;
    st0_sign, st1_sign: u_char;
    tmptag: u_char;
    tag: intg;
    old_cw: intg;
    expdif: intg;
    q:  s64;
    saved_status: u16;
    cc: intg;
    sign: u_char;
    x:  u64;
    exp_1, N: intg;
    sign_: u_char;

  label
    fprem_valid;
  begin
    st1_ptr := st(1);
    st1_tag := FPU_gettagi(1);

    if (((st0_tag or TAG_Valid) or (st1_tag or TAG_Valid)) = 0) then
      begin

      fprem_valid:
        (* Convert registers for internal use. *)
        st0_sign := FPU_to_exp16(st0_ptr, @st0);
      st1_sign := FPU_to_exp16(st1_ptr, @st1);
      expdif := exponent16(@st0) - exponent16(@st1);

      old_cw := varI387.soft.cwd;
      cc := 0;

      (* We want the status following the denorm tests, but don't want
   the status changed by the arithmetic operations. *)
      saved_status  := varI387.soft.swd;
      varI387.soft.cwd := varI387.soft.cwd and not CW_RC;
      varI387.soft.cwd := varI387.soft.cwd or RC_CHOP;

      if (expdif < 64) then
        begin
        (* This should be the most common case *)

        if (expdif > -2) then
          begin
          sign_ := st0_sign or st1_sign;
          tag := FPU_u_div(@st0, @st1, @tmp, PR_64_BITS or RC_CHOP or
            $3f, sign_);
          setsign(@tmp, sign_);

          if (exponent(@tmp) >= 0) then
            begin
            FPU_round_to_int(@tmp, tag);  (* Fortunately, this can't
               overflow to 2^64 *)
            q := significand(@tmp)^;

            rem_kernel(significand(@st0)^, @significand(@tmp)^,
              significand(@st1)^,
              q, expdif);

            setexponent16(@tmp, exponent16(@st1));
            end
          else
            begin
            reg_copy(@st0, @tmp);
            q := 0;
            end;

          if ((round = RC_RND) and ((tmp.sigh and $c0000000) <> 0)) then
            begin
      (* We may need to subtract st(1) once more,
         to get a result <= 1/2 of st(1). *)
            expdif := exponent16(@st1) - exponent16(@tmp);
            if (expdif <= 1) then
              begin
              if (expdif = 0) then
                x := significand(@st1)^ - significand(@tmp)^
              else (* expdif is 1 *)
                x := (significand(@st1)^ shl 1) - significand(@tmp)^;
              if ((x < significand(@tmp)^) or
                (* or equi-distant (from 0  and st(1)) and q is odd *)
                ((x = significand(@tmp)^) and ((q and 1) <> 0))) then
                begin
                st0_sign := word(st0_sign = 0);
                significand(@tmp)^ := x;
                Inc(q);
                end;
              end;
            end;

          if (q and 4) <> 0 then
            cc := cc or SW_C0;
          if (q and 2) <> 0 then
            cc := cc or SW_C3;
          if (q and 1) <> 0 then
            cc := cc or SW_C1;
          end
        else
          begin
          varI387.soft.cwd := old_cw;
          setcc(0);
          exit;
          end;
        end
      else
        begin
        (* There is a large exponent difference ( >= 64 ) *)
    (* To make much sense, the code in this section should
       be done at high precision. *)

        (* prevent overflow here *)
        (* N is 'a number between 32 and 63' (p26-113) *)
        reg_copy(@st0, @tmp);
        tmptag := st0_tag;
        N := (expdif and $0000001f) + 32;  (* This choice gives results
                identical to an AMD 486 *)
        setexponent16(@tmp, N);
        exp_1 := exponent16(@st1);
        setexponent16(@st1, 0);
        expdif := expdif - N;

        sign_ := getsign(@tmp) or st1_sign;
        tag := FPU_u_div(@tmp, @st1, @tmp, PR_64_BITS or RC_CHOP or $3f, sign_);
        setsign(@tmp, sign_);

        FPU_round_to_int(@tmp, tag);  (* Fortunately, this can't
             overflow to 2^64 *)

        rem_kernel(significand(@st0)^, @significand(@tmp)^,
          significand(@st1)^,
          significand(@tmp)^,
          exponent(@tmp)
          );
        setexponent16(@tmp, exp_1 + expdif);

    (* It is possible for the operation to be complete here.
       What does the IEEE standard say? The Intel 80486 manual
       implies that the operation will never be completed at this
       point, and the behaviour of a real 80486 confirms this.
     *)
        if ((tmp.sigh or tmp.sigl) = 0) then
          begin
          (* The result is zero *)
          varI387.soft.cwd := old_cw;
          varI387.soft.swd := saved_status;
          FPU_copy_to_reg0(@CONST_Z, TAG_Zero);
          setsign(@st0, st0_sign);
{$ifdef PECULIAR_486}
	      setcc(SW_C2);
{$else}
          setcc(0);
{$endif}  (* PECULIAR_486 *)
          exit;
          end;
        cc := SW_C2;
        end;

      varI387.soft.cwd := old_cw;
      varI387.soft.swd := saved_status;
      tag := FPU_normalize_nuo(@tmp, 0);
      reg_copy(@tmp, st0_ptr);

      (* The only condition to be looked for is underflow,
   and it can occur here only if underflow is unmasked. *)
      if ((exponent16(@tmp) <= EXP_UNDER) and (tag <> TAG_Zero) and
        ((varI387.soft.cwd and CW_Underflow) = 0)) then
        begin
        setcc(cc);
        tag := arith_underflow(st0_ptr);
        setsign(st0_ptr, st0_sign);
        FPU_settag0(tag);
        exit;
        end
      else
        if ((exponent16(@tmp) > EXP_UNDER) or (tag = TAG_Zero)) then
          begin
          stdexp(st0_ptr);
          setsign(st0_ptr, st0_sign);
          end
        else
          begin
          tag := FPU_round(st0_ptr, 0, 0, FULL_PRECISION, st0_sign);
          end;
      FPU_settag0(tag);
      setcc(cc);

      exit;
      end;

    if (st0_tag = TAG_Special) then
      st0_tag := FPU_Special(st0_ptr);
    if (st1_tag = TAG_Special) then
      st1_tag := FPU_Special(st1_ptr);

    if (((st0_tag = TAG_Valid) and (st1_tag = TW_Denormal)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TAG_Valid)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TW_Denormal))) then
      begin
      if (denormal_operand() < 0) then
        exit;
      goto fprem_valid;
      end
    else
      if ((st0_tag = TAG_Empty) or (st1_tag = TAG_Empty)) then
        begin
        FPU_stack_underflow();
        exit;
        end
      else
        if (st0_tag = TAG_Zero) then
          begin
          if (st1_tag = TAG_Valid) then
            begin
            setcc(0);
            exit;
            end
          else
            if (st1_tag = TW_Denormal) then
              begin
              if (denormal_operand() < 0) then
                exit;
              setcc(0);
              exit;
              end
            else
              if (st1_tag = TAG_Zero) then
                begin
                arith_invalid(0);
                exit;
                end (* fprem(?,0) always invalid *)
              else
                if (st1_tag = TW_Infinity) then
                  begin
                  setcc(0);
                  exit;
                  end;
          end
        else
          if ((st0_tag = TAG_Valid) or (st0_tag = TW_Denormal)) then
            begin
            if (st1_tag = TAG_Zero) then
              begin
              arith_invalid(0); (* fprem(Valid,Zero) is invalid *)
              exit;
              end
            else
              if (st1_tag <> TW_NaN) then
                begin
                if (((st0_tag = TW_Denormal) or (st1_tag = TW_Denormal)) and
                  (denormal_operand() < 0)) then
                  exit;

                if (st1_tag = TW_Infinity) then
                  begin
                  (* fprem(Valid,Infinity) is o.k. *)
                  setcc(0);
                  exit;
                  end;
                end;
            end
          else
            if (st0_tag = TW_Infinity) then
              begin
              if (st1_tag <> TW_NaN) then
                begin
                arith_invalid(0); (* fprem(Infinity,?) is invalid *)
                exit;
                end;
              end;

    (* One of the registers must contain a NaN if we got here. *)

{$ifdef PARANOID}
  if ( (st0_tag <> TW_NaN) @ and (st1_tag <> TW_NaN) )
      EXCEPTION(EX_INTERNALor$118);
{$endif}(* PARANOID *)

    real_2op_NaN(st1_ptr, st1_tag, 0, st1_ptr);

  end;


(* ST(1) <- ST(1) * log ST;  pop ST *)
procedure fyl2x (st0_ptr: PFPU_REG; st0_tag: u_char);
  var
    st1_ptr, exponent_: PFPU_REG;
    st1_tag: u_char;
    sign_:  u_char;
    e, tag: intg;
    esign:  u_char;

  label
    both_valid;
  begin
    st1_ptr := st(1);
    st1_tag := FPU_gettagi(1);

    clear_C1();

    if ((st0_tag = TAG_Valid) and (st1_tag = TAG_Valid)) then
      begin
      both_valid:
        (* Both regs are Valid or Denormal *)
        if (signpositive(st0_ptr) <> 0) then
          begin
          if (st0_tag = TW_Denormal) then
            FPU_to_exp16(st0_ptr, st0_ptr)
          else
            (* Convert st(0) for internal use. *)
            setexponent16(st0_ptr, exponent(st0_ptr));

          if ((st0_ptr^.sigh = $80000000) and (st0_ptr^.sigl = 0)) then
            begin
            (* Special case. The result can be precise. *)
            e := exponent16(st0_ptr);
            if (e >= 0) then
              begin
              exponent_.sigh := e;
              esign := SIGN_POS;
              end
            else
              begin
              exponent_.sigh := -e;
              esign := SIGN_NEG;
              end;
            exponent_.sigl := 0;
            setexponent16(@exponent_, 31);
            tag := FPU_normalize_nuo(@exponent_, 0);
            stdexp(@exponent_);
            setsign(@exponent_, esign);
            tag := FPU_mul(@exponent, tag, 1, FULL_PRECISION);
            if (tag >= 0) then
              FPU_settagi(1, tag);
            end
          else
            begin
            (* The usual case *)
            sign_ := getsign(st1_ptr);
            if (st1_tag = TW_Denormal) then
              FPU_to_exp16(st1_ptr, st1_ptr)
            else
              (* Convert st(1) for internal use. *)
              setexponent16(st1_ptr, exponent(st1_ptr));
            poly_l2(st0_ptr, st1_ptr, sign_);
            end;
          end
        else
          begin
          (* negative *)
          if (arith_invalid(1) < 0) then
            exit;
          end;

      FPU_pop();

      exit;
      end;

    if (st0_tag = TAG_Special) then
      st0_tag := FPU_Special(st0_ptr);
    if (st1_tag = TAG_Special) then
      st1_tag := FPU_Special(st1_ptr);

    if ((st0_tag = TAG_Empty) or (st1_tag = TAG_Empty)) then
      begin
      FPU_stack_underflow_pop(1);
      exit;
      end
    else
      if ((st0_tag <= TW_Denormal) and (st1_tag <= TW_Denormal)) then
        begin
        if (st0_tag = TAG_Zero) then
          begin
          if (st1_tag = TAG_Zero) then
            begin
            (* Both args zero is invalid *)
            if (arith_invalid(1) < 0) then
              exit;
            end
          else
            begin
            sign_ := getsign(st1_ptr) xor SIGN_NEG;
            if (FPU_divide_by_zero(1, sign_) < 0) then
              exit;

            setsign(st1_ptr, sign_);
            end;
          end
        else
          if (st1_tag = TAG_Zero) then
            begin
            (* st(1) contains zero, st(0) valid <> 0 *)
            (* Zero is the valid answer *)
            sign_ := getsign(st1_ptr);

            if (signnegative(st0_ptr)) <> 0 then
              begin
              (* log(negative) *)
              if (arith_invalid(1) < 0) then
                exit;
              end
            else
              if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                exit
              else
                begin
                if (exponent(st0_ptr) < 0) then
                  sign_ := sign_ xor SIGN_NEG;

                FPU_copy_to_reg1(@CONST_Z, TAG_Zero);
                setsign(st1_ptr, sign_);
                end;
            end
          else
            begin
            (* One or both operands are denormals. *)
            if (denormal_operand() < 0) then
              exit;
            goto both_valid;
            end;
        end
      else
        if ((st0_tag = TW_NaN) or (st1_tag = TW_NaN)) then
          begin
          if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) < 0) then
            exit;
          end
        (* One or both arg must be an infinity *)
        else
          if (st0_tag = TW_Infinity) then
            begin
            if ((signnegative(st0_ptr) <> 0) or (st1_tag = TAG_Zero)) then
              begin
              (* log(-infinity) or 0*log(infinity) *)
              if (arith_invalid(1) < 0) then
                exit;
              end
            else
              begin
              sign_ := getsign(st1_ptr);

              if ((st1_tag = TW_Denormal) and (denormal_operand() < 0)) then
                exit;

              FPU_copy_to_reg1(@CONST_INF, TAG_Special);
              setsign(st1_ptr, sign_);
              end;
            end
          (* st(1) must be infinity here *)
          else
            if (((st0_tag = TAG_Valid) or (st0_tag = TW_Denormal)) and
              (signpositive(st0_ptr) <> 0)) then
              begin
              if (exponent(st0_ptr) >= 0) then
                begin
                if ((exponent(st0_ptr) = 0) and (st0_ptr^.sigh = $80000000) and
                  (st0_ptr^.sigl = 0)) then
                  begin
                  (* st(0) holds 1.0 *)
                  (* infinity*log(1) *)
                  if (arith_invalid(1) < 0) then
                    exit;
                  end;
                (* else st(0) is positive and > 1.0 *)
                end
              else
                begin
                (* st(0) is positive and < 1.0 *)

                if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                  exit;

                changesign(st1_ptr);
                end;
              end
            else
              begin
              (* st(0) must be zero or negative *)
              if (st0_tag = TAG_Zero) then
                begin
                (* This should be invalid, but a real 80486 is happy with it. *)

{$ifndef PECULIAR_486}
                sign_ := getsign(st1_ptr);
                if (FPU_divide_by_zero(1, sign_) < 0) then
                  exit;
{$endif}        (* PECULIAR_486 *)

                changesign(st1_ptr);
                end
              else
                if (arith_invalid(1) < 0) then    (* log(negative) *)
                  exit;
              end;

    FPU_pop();
  end;


procedure fpatan (st0_ptr: PFPU_REG; st0_tag: u_char);
  var
    st1_ptr: PFPU_REG;
    st1_tag: u_char;
    tag: intg;
    sign_: u_char;

  label
    valid_atan;
  begin
    st1_ptr := st(1);
    st1_tag := FPU_gettagi(1);

    clear_C1();
    if (((st0_tag or TAG_Valid) or (st1_tag or TAG_Valid)) = 0) then
      begin
      valid_atan:

        poly_atan(st0_ptr, st0_tag, st1_ptr, st1_tag);

      FPU_pop();

      exit;
      end;

    if (st0_tag = TAG_Special) then
      st0_tag := FPU_Special(st0_ptr);
    if (st1_tag = TAG_Special) then
      st1_tag := FPU_Special(st1_ptr);

    if (((st0_tag = TAG_Valid) and (st1_tag = TW_Denormal)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TAG_Valid)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TW_Denormal))) then
      begin
      if (denormal_operand() < 0) then
        exit;

      goto valid_atan;
      end
    else
      if ((st0_tag = TAG_Empty) or (st1_tag = TAG_Empty)) then
        begin
        FPU_stack_underflow_pop(1);
        exit;
        end
      else
        if ((st0_tag = TW_NaN) or (st1_tag = TW_NaN)) then
          begin
          if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) >= 0) then
            FPU_pop();
          exit;
          end
        else
          if ((st0_tag = TW_Infinity) or (st1_tag = TW_Infinity)) then
            begin
            sign_ := getsign(st1_ptr);
            if (st0_tag = TW_Infinity) then
              begin
              if (st1_tag = TW_Infinity) then
                begin
                if (signpositive(st0_ptr) <> 0) then
                  begin
                  FPU_copy_to_reg1(@CONST_PI4, TAG_Valid);
                  end
                else
                  begin
                  setpositive(st1_ptr);
                  tag := FPU_u_add(@CONST_PI4, @CONST_PI2, st1_ptr,
                    FULL_PRECISION, SIGN_POS, exponent(@CONST_PI4),
                    exponent(@CONST_PI2));
                  if (tag >= 0) then
                    FPU_settagi(1, tag);
                  end;
                end
              else
                begin
                if ((st1_tag = TW_Denormal) and (denormal_operand() < 0)) then
                  exit;

                if (signpositive(st0_ptr) <> 0) then
                  begin
                  FPU_copy_to_reg1(@CONST_Z, TAG_Zero);
                  setsign(st1_ptr, sign_);   (* An 80486 preserves the sign_ *)
                  FPU_pop();
                  exit;
                  end
                else
                  begin
                  FPU_copy_to_reg1(@CONST_PI, TAG_Valid);
                  end;
                end;
              end
            else
              begin
              (* st(1) is infinity, st(0) not infinity *)
              if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                exit;

              FPU_copy_to_reg1(@CONST_PI2, TAG_Valid);
              end;
            setsign(st1_ptr, sign_);
            end
          else
            if (st1_tag = TAG_Zero) then
              begin
              (* st(0) must be valid or zero *)
              sign_ := getsign(st1_ptr);

              if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                exit;

              if (signpositive(st0_ptr) <> 0) then
                begin
                (* An 80486 preserves the sign_ *)
                FPU_pop();
                exit;
                end;

              FPU_copy_to_reg1(@CONST_PI, TAG_Valid);
              setsign(st1_ptr, sign_);
              end
            else
              if (st0_tag = TAG_Zero) then
                begin
                (* st(1) must be TAG_Valid here *)
                sign_ := getsign(st1_ptr);

                if ((st1_tag = TW_Denormal) and (denormal_operand() < 0)) then
                  exit;

                FPU_copy_to_reg1(@CONST_PI2, TAG_Valid);
                setsign(st1_ptr, sign_);
                end;
{$ifdef PARANOID}
  else
    EXCEPTION(EX_INTERNALor$125);
{$endif}(* PARANOID *)

    FPU_pop();
    set_precision_flag_up();  (* We do not really know if up or down *)
  end;


procedure fprem (st0_ptr: PFPU_REG; st0_tag: u_char);
  begin
    do_fprem(st0_ptr, st0_tag, RC_CHOP);
  end;


procedure fprem1 (st0_ptr: PFPU_REG; st0_tag: u_char);
  begin
    do_fprem(st0_ptr, st0_tag, RC_RND);
  end;


procedure fyl2xp1 (st0_ptr: PFPU_REG; st0_tag: u_char);
  var
    sign_, sign1: u_char;
    st1_ptr, a, b: PFPU_REG;
    st1_tag: u_char;

  label
    valid_yl2xp1;
  begin
    st1_ptr := st(1);
    st1_tag := FPU_gettagi(1);

    clear_C1();
    if (((st0_tag or TAG_Valid) <> 0) or ((st1_tag or TAG_Valid) = 0)) then
      begin
      valid_yl2xp1:

        sign_ := getsign(st0_ptr);
      sign1 := getsign(st1_ptr);

      FPU_to_exp16(st0_ptr, @a);
      FPU_to_exp16(st1_ptr, @b);

      if (poly_l2p1(sign_, sign1, @a, @b, st1_ptr)) <> 0 then
        exit;

      FPU_pop();
      exit;
      end;

    if (st0_tag = TAG_Special) then
      st0_tag := FPU_Special(st0_ptr);
    if (st1_tag = TAG_Special) then
      st1_tag := FPU_Special(st1_ptr);

    if (((st0_tag = TAG_Valid) and (st1_tag = TW_Denormal)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TAG_Valid)) or
      ((st0_tag = TW_Denormal) and (st1_tag = TW_Denormal))) then
      begin
      if (denormal_operand() < 0) then
        exit;

      goto valid_yl2xp1;
      end
    else
      if ((st0_tag = TAG_Empty) or (st1_tag = TAG_Empty)) then
        begin
        FPU_stack_underflow_pop(1);
        exit;
        end
      else
        if (st0_tag = TAG_Zero) then
          begin
          case (st1_tag) of
            TW_Denormal:
              begin
              if (denormal_operand() < 0) then
                exit;
              end;

            TAG_Zero,
            TAG_Valid:
              begin
              setsign(st0_ptr, getsign(st0_ptr) or getsign(st1_ptr));
              FPU_copy_to_reg1(st0_ptr, st0_tag);
              end;

            TW_Infinity:
              begin
              (* Infinity*log(1) *)
              if (arith_invalid(1) < 0) then
                exit;
              end;

            TW_NaN:
              begin
              if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) < 0) then
                exit;
              end;

            else
{$ifdef PARANOID}
	  EXCEPTION(EX_INTERNALor$116);
	  exit;
{$endif}    (* PARANOID *)
            end;
          end
        else
          if ((st0_tag = TAG_Valid) or (st0_tag = TW_Denormal)) then
            begin
            case (st1_tag) of
              TAG_Zero:
                begin
                if (signnegative(st0_ptr) <> 0) then
                  begin
                  if (exponent(st0_ptr) >= 0) then
                    begin
                    (* st(0) holds <= -1.0 *)
{$ifdef PECULIAR_486}   (* Stupid 80486 doesn't worry about log(negative). *)
		  changesign(st1_ptr);
{$else}
                    if (arith_invalid(1) < 0) then
                      exit;
{$endif}            (* PECULIAR_486 *)
                    end
                  else
                    if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                      exit
                    else
                      changesign(st1_ptr);
                  end
                else
                  if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                    exit;
                end;

              TW_Infinity:
                begin
                if (signnegative(st0_ptr) <> 0) then
                  begin
                  if ((exponent(st0_ptr) >= 0) and
                    (((st0_ptr^.sigh = $80000000) and (st0_ptr^.sigl = 0)) = False)) then
                    begin
                    (* st(0) holds < -1.0 *)
{$ifdef PECULIAR_486}   (* Stupid 80486 doesn't worry about log(negative). *)
		  changesign(st1_ptr);
{$else}
                    if (arith_invalid(1) < 0) then
                      exit;
{$endif}            (* PECULIAR_486 *)
                    end
                  else
                    if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                      exit
                    else
                      changesign(st1_ptr);
                  end
                else
                  if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
                    exit;
                end;

              TW_NaN:
                begin
                if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) < 0) then
                  exit;
                end;
              end;

            end
          else
            if (st0_tag = TW_NaN) then
              begin
              if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) < 0) then
                exit;
              end
            else
              if (st0_tag = TW_Infinity) then
                begin
                if (st1_tag = TW_NaN) then
                  begin
                  if (real_2op_NaN(st0_ptr, st0_tag, 1, st0_ptr) < 0) then
                    exit;
                  end
                else
                  if (signnegative(st0_ptr)) <> 0 then
                    begin
{$ifndef PECULIAR_486}
                    (* This should have higher priority than denormals, but... *)
                    if (arith_invalid(1) < 0) then (* log(-infinity) *)
                      exit;
{$endif}            (* PECULIAR_486 *)
                    if ((st1_tag = TW_Denormal) and (denormal_operand() < 0)) then
                      exit;
{$ifdef PECULIAR_486}
	  (* Denormal operands actually get higher priority *)
	  if ( arith_invalid(1) < 0 ) then  (* log(-infinity) *)
	    exit;
{$endif}            (* PECULIAR_486 *)
                    end
                  else
                    if (st1_tag = TAG_Zero) then
                      begin
                      (* log(infinity) *)
                      if (arith_invalid(1) < 0) then
                        exit;
                      end

                    (* st(1) must be valid here. *)

                    else
                      if ((st1_tag = TW_Denormal) and (denormal_operand() < 0)) then
                        exit

      (* The Manual says that log(Infinity) is invalid, but a real
   80486 sensibly says that it is o.k. *)
                      else
                        begin
                        sign_ := getsign(st1_ptr);
                        FPU_copy_to_reg1(@CONST_INF, TAG_Special);
                        setsign(st1_ptr, sign_);
                        end;
                end;
{$ifdef PARANOID}
  else
    begin
      EXCEPTION(EX_INTERNALor$117);
      exit;
    end;
{$endif}(* PARANOID *)

    FPU_pop();
    exit;

  end;


procedure fscale (st0_ptr: PFPU_REG; st0_tag: u_char);
  var
    st1_ptr: PFPU_REG;
    st1_tag: u_char;
    old_cw: intg;
    sign_: u_char;
    scale: s32;
    tmp: FPU_REG;

  label
    valid_scale;
  begin
    st1_ptr := st(1);
    st1_tag := FPU_gettagi(1);
    old_cw := varI387.soft.cwd;
    sign_ := getsign(st0_ptr);

    clear_C1();
    if (((st0_tag or TAG_Valid) or (st1_tag or TAG_Valid)) = 0) then
      begin

      (* Convert register for internal use. *)
      setexponent16(st0_ptr, exponent(st0_ptr));

      valid_scale:

        if (exponent(st1_ptr) > 30) then
          begin
          (* 2^31 is far too large, would require 2^(2^30) or 2^(-2^30) *)

          if (signpositive(st1_ptr) <> 0) then
            begin
            fpu_EXCEPTION(EX_Overflow);
            FPU_copy_to_reg0(@CONST_INF, TAG_Special);
            end
          else
            begin
            fpu_EXCEPTION(EX_Underflow);
            FPU_copy_to_reg0(@CONST_Z, TAG_Zero);
            end;
          setsign(st0_ptr, sign_);
          exit;
          end;

      varI387.soft.cwd := varI387.soft.cwd and not CW_RC;
      varI387.soft.cwd := varI387.soft.cwd or RC_CHOP;
      reg_copy(st1_ptr, @tmp);
      FPU_round_to_int(@tmp, st1_tag);      (* This can never overflow here *)
      varI387.soft.cwd := old_cw;
      scale := ifthen(signnegative(st1_ptr) <> 0, -tmp.sigl, tmp.sigl);
      scale := scale + exponent16(st0_ptr);

      setexponent16(st0_ptr, scale);

      (* Use FPU_round() to properly detect under/overflow etc *)
      FPU_round(st0_ptr, 0, 0, varI387.soft.cwd, sign_);

      exit;
      end;

    if (st0_tag = TAG_Special) then
      st0_tag := FPU_Special(st0_ptr);
    if (st1_tag = TAG_Special) then
      st1_tag := FPU_Special(st1_ptr);

    if ((st0_tag = TAG_Valid) or (st0_tag = TW_Denormal)) then
      begin
      case (st1_tag) of
        TAG_Valid:
          begin
          (* st(0) must be a denormal *)
          if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
            exit;

          FPU_to_exp16(st0_ptr, st0_ptr);  (* Will not be left on stack *)
          goto valid_scale;
          end;

        TAG_Zero:
          begin
          if (st0_tag = TW_Denormal) then
            denormal_operand();
          exit;
          end;

        TW_Denormal:
          begin
          denormal_operand();
          exit;
          end;

        TW_Infinity:
          begin
          if ((st0_tag = TW_Denormal) and (denormal_operand() < 0)) then
            exit;

          if (signpositive(st1_ptr) <> 0) then
            FPU_copy_to_reg0(@CONST_INF, TAG_Special)
          else
            FPU_copy_to_reg0(@CONST_Z, TAG_Zero);
          setsign(st0_ptr, sign_);
          exit;
          end;

        TW_NaN:
          begin
          real_2op_NaN(st1_ptr, st1_tag, 0, st0_ptr);
          exit;
          end;
        end;
      end
    else
      if (st0_tag = TAG_Zero) then
        begin
        case (st1_tag) of
          TAG_Valid,
          TAG_Zero:
            exit;

          TW_Denormal:
            begin
            denormal_operand();
            exit;
            end;

          TW_Infinity:
            begin
            if (signpositive(st1_ptr) <> 0) then
              arith_invalid(0); (* Zero scaled by +Infinity *)
            exit;
            end;

          TW_NaN:
            begin
            real_2op_NaN(st1_ptr, st1_tag, 0, st0_ptr);
            exit;
            end;
          end;
        end
      else
        if (st0_tag = TW_Infinity) then
          begin
          case (st1_tag) of
            TAG_Valid,
            TAG_Zero:
              exit;

            TW_Denormal:
              begin
              denormal_operand();
              exit;
              end;

            TW_Infinity:
              begin
              if (signnegative(st1_ptr) <> 0) then
                arith_invalid(0); (* Infinity scaled by -Infinity *)
              end;

            TW_NaN:
              begin
              real_2op_NaN(st1_ptr, st1_tag, 0, st0_ptr);
              exit;
              end;
            end;
          end
        else
          if (st0_tag = TW_NaN) then
            begin
            if (st1_tag <> TAG_Empty) then
              begin
              real_2op_NaN(st1_ptr, st1_tag, 0, st0_ptr);
              exit;
              end;
            end;

{$ifdef PARANOID}
  if ( ((st0_tag = TAG_Empty) or (st1_tag = TAG_Empty)) =0) then
    begin
      fpu_EXCEPTION(EX_INTERNAL or $115);
      exit;
    end;
{$endif}

    (* At least one of st(0), st(1) must be empty *)
    FPU_stack_underflow();

  end;


(*---------------------------------------------------------------------------*)

const
  trig_table_a: array[0..7] of FUNC_ST0 = (
    f2xm1, fyl2x, fptan, fpatan,
    fxtract, fprem1, fdecstp, fincstp);


procedure FPU_triga;
  var
    p: FUNC_ST0;
  begin
    p := trig_table_a[FPU_rm^];
    p(st(0), FPU_gettag0());
  end;

procedure fsin__ (st0_ptr: PFPU_REG; st0_tag: u_char);
  begin
    fsin(st0_ptr, st0_tag);
  end;

const
  trig_table_b: array[0..7] of FUNC_ST0 = (
    fprem, fyl2xp1, fsqrt_, fsincos, frndint_, fscale, fsin__, fcos);

procedure FPU_trigb;
  var
    p: FUNC_ST0;
  begin
    p := trig_table_b[FPU_rm^];
    p(st(0), FPU_gettag0());
  end;

procedure div_Xsig (const aa: PXsig; const b: PXsig; dest: PXsig);
  var
    a, xpr, res: Xsig;
    prodh, prodl, den, wd: u32;
    num, prod: u64;
  begin
    a := aa^;

{$ifdef PARANOID}
  if ( (b^.msw  and $8000) = 0 )
    begin
      EXCEPTION(EX_INTERNAL|$240);
      exit;
    end;
{$endif}

    (* Shift a right *)
    a.lsw := a.lsw shr 1;
    if (a.midw and 1) <> 0 then
      a.lsw := a.lsw or $8000;
    a.midw := a.midw shr 1;
    if (a.msw and 1) <> 0 then
      a.midw := a.midw or $8000;
    a.msw := a.msw shr 1;

    num := a.msw;
    num := num shl 32;
    num := num or a.midw;

    den := b^.msw + 1;
    if (den) <> 0 then
      begin
      res.msw := num div den;
      end
    else
      res.msw := a.msw;

    xpr := b^;
    mul32_Xsig(@xpr, res.msw);
    a.msw := a.msw - xpr.msw;
    wd := a.midw;
    a.midw := a.midw - xpr.midw;
    if (a.midw > wd) then
      Dec(a.msw);
    wd := a.lsw;
    a.lsw := a.lsw - xpr.lsw;
    if (a.lsw > wd) then
      begin
      Dec(a.midw);
      if (a.midw = $ffffffff) then
        Dec(a.msw);
      end;

{$ifdef PARANOID}
      if ( a.msw > 1 )
	begin
	  EXCEPTION(EX_INTERNAL|$241);
	end;
{$endif}

    while ((a.msw <> 0) or ((a.midw > b^.msw))) do
      begin
      wd := a.midw;
      a.midw := a.midw - b^.msw;
      if (a.midw > wd) then
        Dec(a.msw);
      wd := a.lsw;
      a.lsw := a.lsw - b^.midw;
      if (a.lsw > wd) then
        begin
        Dec(a.midw);
        if (a.midw = $ffffffff) then
          Dec(a.msw);
        end;
      Inc(res.msw);
      end;

    (* Whew! result.msw is now done. *)

    num := a.midw;
    num := num shl 32;
    num := num or a.lsw;

    if (den) <> 0 then
      begin
      res.midw := num div den;
      end
    else
      res.midw := a.midw;

    prod := res.midw;
    prod := prod * b^.msw;
    a.midw := a.midw - prod shr 32;
    prodl := prod;
    wd := a.lsw;
    a.lsw := a.lsw - prodl;
    if (a.lsw > wd) then
      Dec(a.midw);

    prod := res.midw;
    prod := prod * b^.midw;
    prodh := prod shr 32;
    wd := a.lsw;
    a.lsw := a.lsw - prodh;
    if (a.lsw > wd) then
      Dec(a.midw);

{$ifdef PARANOID}
      if ( a.midw > 1 ) then
	begin
          fpu_EXCEPTION(EX_INTERNAL or $242);
	end;
{$endif}

    while ((a.midw <> 0) or (a.lsw > b^.msw)) do
      begin
      wd := a.lsw;
      a.lsw := a.lsw - b^.msw;
      if (a.lsw > wd) then
        Dec(a.midw);
      Inc(res.midw);
      end;


    (* Now result.msw is done, the lsw is next... *)

    num := a.lsw;
    num := num shl 32;

    if (den) <> 0 then
      begin
      res.lsw := num div den;
      end
    else
      res.lsw := a.lsw;

    prod  := res.lsw;
    prod  := prod * b^.msw;
    a.lsw := a.lsw - prod shr 32;

{$ifdef PARANOID}
  if ( a.lsw > 2 ) then
    begin
      EXCEPTION(EX_INTERNAL|$243);
    end;
{$endif}

    res.lsw := res.lsw - a.lsw;

    (* Hey! we're done. *)

    dest^ := res;

  end;

function round_Xsig (x: PXsig): intg;
  var
    n: intg;
  begin
    n := 0;

    if (x^.msw = 0) then
      begin
      x^.msw := x^.midw;
      x^.midw := x^.lsw;
      x^.lsw := 0;
      n := 32;
      end;
    while ((x^.msw and $8000) = 0) do
      begin
      x^.msw := x^.msw shl 1;
      if (x^.midw and $8000) <> 0 then
        x^.msw := x^.msw or 1;
      x^.midw := x^.midw shl 1;
      if (x^.lsw and $8000) <> 0 then
        x^.midw := x^.midw or 1;
      x^.lsw := x^.lsw shl 1;
      Inc(n);
      end;
    if (x^.lsw and $8000) <> 0 then
      begin
      Inc(x^.midw);
      if (x^.midw = 0) then
        Inc(x^.msw);
      if (x^.msw = 0) then
        begin
        x^.msw := $8000;
        Dec(n);
        end;
      end;
    Result := -n;
  end;


function norm_Xsig (x: PXsig): intg;
  var
    n: intg;
  begin
    n := 0;

    if (x^.msw = 0) then
      begin
      if (x^.midw = 0) then
        begin
        x^.msw := x^.lsw;
        x^.midw := 0;
        x^.lsw := 0;
        n := 64;
        end
      else
        begin
        x^.msw := x^.midw;
        x^.midw := x^.lsw;
        x^.lsw := 0;
        n := 32;
        end;
      end;
    while ((x^.msw and $8000) = 0) do
      begin
      x^.msw := x^.msw shl 1;
      if (x^.midw and $8000) <> 0 then
        x^.msw := x^.msw or 1;
      x^.midw := x^.midw shl 1;
      if (x^.lsw and $8000) <> 0 then
        x^.midw := x^.midw or 1;
      x^.lsw := x^.lsw shl 1;
      Inc(n);
      end;

    Result := -n;
  end;

procedure	poly_l2(st0_ptr:PFPU_REG; st1_ptr:PFPU_REG; st1_sign:u_char);
var
  exponent_, expon, expon_expon:s32;
  accumulator, expon_accum, yaccum:Xsig;
  sign, argsign:u_char;
  x:FPU_REG;
  tag:intg;
begin

  exponent_ := exponent16(st0_ptr);

  (* From st0_ptr, make a number > sqrt(2)/2 and < sqrt(2) *)
  if ( st0_ptr^.sigh > unsigned($b504f334) ) then
    begin
      (* Treat as  sqrt(2)/2 < st0_ptr < 1 *)
      significand(@x)^ := - significand(st0_ptr)^;
      setexponent16(@x, -1);
      inc(exponent_);
      argsign := SIGN_NEG;
    end
  else
    begin
      (* Treat as  1 <= st0_ptr < sqrt(2) *)
      x.sigh := st0_ptr^.sigh - $80000000;
      x.sigl := st0_ptr^.sigl;
      setexponent16(@x, 0);
      argsign := SIGN_POS;
    end;
  tag := FPU_normalize_nuo(@x, 0);

  if ( tag = TAG_Zero ) then
  begin
    expon := 0;
    accumulator.msw := 0;
    accumulator.midw := 0;
    accumulator.lsw := 0;
  end else
    log2_kernel(@x, argsign, @accumulator, @expon);

  if ( exponent_ < 0 ) then
  begin
    sign := SIGN_NEG;
    exponent_ := -exponent_;
  end else
    sign := SIGN_POS;
  expon_accum.msw := exponent_;
  expon_accum.midw := 0;
  expon_accum.lsw := 0;
  if ( exponent_ )<>0 then
  begin
      expon_expon := 31 + norm_Xsig(@expon_accum);
      shr_Xsig(@accumulator, expon_expon - expon);

      if ( sign or argsign )<>0 then
	negate_Xsig(@accumulator);
      add_Xsig_Xsig(@accumulator, @expon_accum);
  end else
  begin
    expon_expon := expon;
    sign := argsign;
  end;

  yaccum.lsw := 0; XSIG_LL(yaccum)^ := significand(st1_ptr)^;
  mul_Xsig_Xsig(@accumulator, @yaccum);

  expon_expon :=  expon_expon + round_Xsig(@accumulator);

  if ( accumulator.msw = 0 ) then
  begin
    FPU_copy_to_reg1(@CONST_Z, TAG_Zero);
    exit;
  end;

  significand(st1_ptr)^ := XSIG_LL(accumulator)^;
  setexponent16(st1_ptr, expon_expon + exponent16(st1_ptr) + 1);

  tag := FPU_round(st1_ptr, 1, 0, FULL_PRECISION, sign or st1_sign);
  FPU_settagi(1, tag);

  set_precision_flag_up();  (* 80486 appears to always do this *)

  exit;

end;


(*--- poly_l2p1() -----------------------------------------------------------+
or  Base 2 logarithm by a polynomial approximation.                         |
or  log2(x+1)                                                               |
 +---------------------------------------------------------------------------*)
function poly_l2p1(sign0:u_char; sign1:u_char;
		  st0_ptr:PFPU_REG; st1_ptr:PFPU_REG; dest:PFPU_REG):intg;
var
  tag:u_char;
  exponent_:s32;
  accumulator, yaccum:Xsig;
begin

  if ( exponent16(st0_ptr) < 0 ) then
    begin
      log2_kernel(st0_ptr, sign0, @accumulator, @exponent_);

      yaccum.lsw := 0;
      XSIG_LL(yaccum)^ := significand(st1_ptr)^;
      mul_Xsig_Xsig(@accumulator, @yaccum);

      exponent_ :=      exponent_ + round_Xsig(@accumulator);

      exponent_ :=      exponent_ + exponent16(st1_ptr) + 1;
      if ( exponent_ < EXP_WAY_UNDER ) then
         exponent_ := EXP_WAY_UNDER;

      significand(dest)^ := XSIG_LL(accumulator)^;
      setexponent16(dest, exponent_);

      tag := FPU_round(dest, 1, 0, FULL_PRECISION, sign0 or sign1);
      FPU_settagi(1, tag);

      if ( tag = TAG_Valid ) then
	set_precision_flag_up();   (* 80486 appears to always do this *)
    end
  else
    begin
      (* The magnitude of st0_ptr is far too large. *)

      if ( sign0 <> SIGN_POS ) then
	begin
	  (* Trying to get the log of a negative number. *)
{$ifdef PECULIAR_486}   (* Stupid 80486 doesn't worry about log(negative). *)
	  changesign(st1_ptr);
{$else}
	  if ( arith_invalid(1) < 0 ) then
      begin
	    result := 1;
       exit;
      end;
{$endif} (* PECULIAR_486 *)
	end;

      (* 80486 appears to do this *)
      if ( sign0 = SIGN_NEG ) then
	set_precision_flag_down()
      else
	set_precision_flag_up();
    end;

  if ( exponent(dest) <= EXP_UNDER ) then
    fpu_EXCEPTION(EX_Underflow);

  result := 0;

end;


const	HIPOWER2 =	10;
const logterms:array[0..HIPOWER2-1] of u64 = (
  $2a8eca5705fc2ef0,
  $f6384ee1d01febce,
  $093bb62877cdf642,
  $006985d8a9ec439b,
  $0005212c4f55a9c8,
  $00004326a16927f0,
  $0000038d1d80a0e7,
  $0000003141cc80c6,
  $00000002b1668c9f,
  $000000002c7a46aa
);

const leadterm:u32 = $b8000000;

(*--- log2_kernel() ---------------------------------------------------------+
or  Base 2 logarithm by a polynomial approximation.                         |
or  log2(x+1)                                                               |
 +---------------------------------------------------------------------------*)
procedure log2_kernel(const arg:PFPU_REG; argsign:u_char; accum_result:PXsig;
			expon:ps32);
var
  exponent_, adj:s32;
  Xsq:u64;
  accumulator, Numer, Denom, argSignif, arg_signif:Xsig;
begin

  exponent_ := exponent16(arg);
  Numer.lsw := 0;
  Denom.lsw := 0;
  XSIG_LL(Denom)^ := significand(arg)^;
  XSIG_LL(Numer)^ := XSIG_LL(Denom)^;
  if ( argsign = SIGN_POS ) then
    begin
      shr_Xsig(@Denom, 2 - (1 + exponent_));
      Denom.msw :=      Denom.msw or $80000000;
      div_Xsig(@Numer, @Denom, @argSignif);
    end
  else
    begin
      shr_Xsig(@Denom, 1 - (1 + exponent_));
      negate_Xsig(@Denom);
      if ( Denom.msw  and $80000000 )<> 0 then
	begin
	  div_Xsig(@Numer, @Denom, @argSignif);
	  inc(exponent_);
	end
      else
	begin
	  (* Denom must be 1.0 *)
	  argSignif.lsw := Numer.lsw; argSignif.midw := Numer.midw;
	  argSignif.msw := Numer.msw;
	end;
    end;

//{$ifndef PECULIAR_486}
//  (* Should check here that  |local_arg|  is within the valid range *)
//  if ( exponent_ >= -2 )
//    begin
//      if ( (exponent_ > -2) or
//	  (argSignif.msw > (unsigned)$afb0ccc0) )
//	begin
//	  (* The argument is too large *)
//	end;
//    end;
//{$endif} (* PECULIAR_486 *)

  arg_signif.lsw := argSignif.lsw; XSIG_LL(arg_signif)^ := XSIG_LL(argSignif)^;
  adj := norm_Xsig(@argSignif);
  accumulator.lsw := argSignif.lsw; XSIG_LL(accumulator)^ := XSIG_LL(argSignif)^;
  mul_Xsig_Xsig(@accumulator, @accumulator);
  shr_Xsig(@accumulator, 2*(-1 - (1 + exponent_ + adj)));
  Xsq := XSIG_LL(accumulator)^;
  if ( accumulator.lsw  and $80000000 )<>0 then
    inc(Xsq);

  accumulator.msw := 0;
  accumulator.midw := 0;
  accumulator.lsw := 0;
  (* Do the basic fixed point polynomial evaluation *)
  polynomial_Xsig(@accumulator, @Xsq, @logterms, HIPOWER2-1);

  mul_Xsig_Xsig(@accumulator, @argSignif);
  shr_Xsig(@accumulator, 6 - adj);

  mul32_Xsig(@arg_signif, leadterm);
  add_two_Xsig(@accumulator, @arg_signif, @exponent_);

  expon^ := exponent_ + 1;
  accum_result^.lsw := accumulator.lsw;
  accum_result^.midw := accumulator.midw;
  accum_result^.msw := accumulator.msw;

end;

procedure poly_atan (st0_ptr: PFPU_REG; st0_tag: u_char; st1_ptr: PFPU_REG;
  st1_tag: u_char);
var
  transformed, inverted, sign1, sign2: u_char;
  exponent_: s32;
  dummy_exp: s32;
  accumulator, Numer, Denom, accumulatore, argSignif, argSq, argSqSq: Xsig;
  tag: u_char;
begin

  sign1 := getsign(st0_ptr);
  sign2 := getsign(st1_ptr);
  if (st0_tag = TAG_Valid) then
    begin
    exponent_ := exponent(st0_ptr);
    end
  else
    begin
    (* This gives non-compatible stack contents... *)
    FPU_to_exp16(st0_ptr, st0_ptr);
    exponent_ := exponent16(st0_ptr);
    end;
  if (st1_tag = TAG_Valid) then
    begin
    exponent_ := exponent_ - exponent(st1_ptr);
    end
  else
    begin
    (* This gives non-compatible stack contents... *)
    FPU_to_exp16(st1_ptr, st1_ptr);
    exponent_ := exponent_ - exponent16(st1_ptr);
    end;

  if ((exponent_ < 0) or ((exponent_ = 0) and
    ((st0_ptr^.sigh < st1_ptr^.sigh) or ((st0_ptr^.sigh = st1_ptr^.sigh) and
    (st0_ptr^.sigl < st1_ptr^.sigl))))) then
    begin
    inverted  := 1;
    Numer.lsw := 0;
    Denom.lsw := 0;
    XSIG_LL(Numer)^ := significand(st0_ptr)^;
    XSIG_LL(Denom)^ := significand(st1_ptr)^;
    end
  else
    begin
    inverted  := 0;
    exponent_ := -exponent_;
    Numer.lsw := 0;
    Denom.lsw := 0;
    XSIG_LL(Numer)^ := significand(st1_ptr)^;
    XSIG_LL(Denom)^ := significand(st0_ptr)^;
    end;
  div_Xsig(@Numer, @Denom, @argSignif);
  exponent_ := exponent_ + norm_Xsig(@argSignif);

  if ((exponent_ >= -1) or ((exponent_ = -2) and
    (argSignif.msw > $d413ccd0))) then
    begin
    (* The argument is greater than sqrt(2)-1 (:=0.414213562...) *)
    (* Convert the argument by an identity for atan *)
    transformed := 1;

    if (exponent_ >= 0) then
      begin
{$ifdef PARANOID}
  if ( ( (exponent_ = 0) and
   (argSignif.lsw = 0) @ and (argSignif.midw = 0) @@
   (argSignif.msw = $80000000) ) )= false then
    begin
      EXCEPTION(EX_INTERNAL|$104);  (* There must be a logic error *)
      exit;
    end;
{$endif}(* PARANOID *)
      argSignif.msw := 0;   (* Make the transformed arg ^. 0.0 *)
      end
    else
      begin
      Denom.lsw := argSignif.lsw;
      Numer.lsw := Denom.lsw;
      XSIG_LL(Denom)^ := XSIG_LL(argSignif)^;
      XSIG_LL(Numer)^ := XSIG_LL(Denom)^;

      if (exponent_ < -1) then
        shr_Xsig(@Numer, -1 - exponent_);
      negate_Xsig(@Numer);

      shr_Xsig(@Denom, -exponent_);
      Denom.msw := Denom.msw or $80000000;

      div_Xsig(@Numer, @Denom, @argSignif);

      exponent_ := -1 + norm_Xsig(@argSignif);
      end;
    end
  else
    begin
    transformed := 0;
    end;

  argSq.lsw  := argSignif.lsw;
  argSq.midw := argSignif.midw;
  argSq.msw  := argSignif.msw;
  mul_Xsig_Xsig(@argSq, @argSq);

  argSqSq.lsw  := argSq.lsw;
  argSqSq.midw := argSq.midw;
  argSqSq.msw  := argSq.msw;
  mul_Xsig_Xsig(@argSqSq, @argSqSq);

  accumulatore.lsw := argSq.lsw;
  XSIG_LL(accumulatore)^ := XSIG_LL(argSq)^;

  shr_Xsig(@argSq, 2 * (-1 - exponent_ - 1));
  shr_Xsig(@argSqSq, 4 * (-1 - exponent_ - 1));

(* Now have argSq etc with binary point at the left
   .1xxxxxxxx *)

  (* Do the basic fixed point polynomial evaluation *)
  accumulator.msw  := 0;
  accumulator.midw := 0;
  accumulator.lsw  := 0;
  polynomial_Xsig(@accumulator, XSIG_LL(argSqSq), @oddplterms, HIPOWERop - 1);
  mul64_Xsig(@accumulator, XSIG_LL(argSq));
  negate_Xsig(@accumulator);
  polynomial_Xsig(@accumulator, XSIG_LL(argSqSq), @oddnegterms, HIPOWERon2 - 1);
  negate_Xsig(@accumulator);
  add_two_Xsig(@accumulator, @fixedpterm, @dummy_exp);

  mul64_Xsig(@accumulatore, @denomterm);
  shr_Xsig(@accumulatore, 1 + 2 * (-1 - exponent_));
  accumulatore.msw := accumulatore.msw or $80000000;

  div_Xsig(@accumulator, @accumulatore, @accumulator);

  mul_Xsig_Xsig(@accumulator, @argSignif);
  mul_Xsig_Xsig(@accumulator, @argSq);

  shr_Xsig(@accumulator, 3);
  negate_Xsig(@accumulator);
  add_Xsig_Xsig(@accumulator, @argSignif);

  if (transformed) <> 0 then
    begin
    (* compute pi/4 - accumulator *)
    shr_Xsig(@accumulator, -1 - exponent_);
    negate_Xsig(@accumulator);
    add_Xsig_Xsig(@accumulator, @pi_signif);
    exponent_ := -1;
    end;

  if (inverted) <> 0 then
    begin
    (* compute pi/2 - accumulator *)
    shr_Xsig(@accumulator, -exponent_);
    negate_Xsig(@accumulator);
    add_Xsig_Xsig(@accumulator, @pi_signif);
    exponent_ := 0;
    end;

  if (sign1) <> 0 then
    begin
    (* compute pi - accumulator *)
    shr_Xsig(@accumulator, 1 - exponent_);
    negate_Xsig(@accumulator);
    add_Xsig_Xsig(@accumulator, @pi_signif);
    exponent_ := 1;
    end;

  exponent_ := exponent_ + round_Xsig(@accumulator);

  significand(st1_ptr)^ := XSIG_LL(accumulator)^;
  setexponent16(st1_ptr, exponent_);

  tag := FPU_round(st1_ptr, 1, 0, FULL_PRECISION, sign2);
  FPU_settagi(1, tag);


  set_precision_flag_up();  (* We do not really know if up or down,
           use this as the default. *)

end;

procedure printk (fmt: string);
begin
  LogInfo(fmt);
end;

procedure FPU_exception (n: intg);
var
  {i,} int_type: intg;
begin
  int_type := 0;         (* Needed only to stop compiler warnings *)
  if (n and EX_INTERNAL) <> 0 then
  begin
    int_type := n - EX_INTERNAL;
    n := EX_INTERNAL;
    (* Set lots of exception bits! *)
    varI387.soft.swd := varI387.soft.swd or (SW_Exc_Mask or SW_Summary or SW_Backward);
  end else
  begin
    (* Extract only the bits which we use to set the status word *)
    n := n and (SW_Exc_Mask);
    (* Set the corresponding exception bit *)
    varI387.soft.swd := varI387.soft.swd or n;
    (* Set summary bits iff exception isn't masked *)
    if (varI387.soft.swd and not varI387.soft.cwd and CW_Exceptions) <> 0 then
      varI387.soft.swd := varI387.soft.swd or (SW_Summary or SW_Backward);

    if (n and (SW_Stack_Fault or EX_Precision)) <> 0 then
    begin
      if ((n and SW_C1)) = 0 then
          (* This bit distinguishes over- from underflow for a stack fault,
           and roundup from round-down for precision loss. *)
        varI387.soft.swd := varI387.soft.swd and not SW_C1;
    end;
  end;

  //RE_ENTRANT_CHECK_OFF;
  if ((not varI387.soft.cwd and n and CW_Exceptions) <> 0) or (n = EX_INTERNAL) then
  begin
{$ifdef PRINT_MESSAGES}
    (* My message from the sponsor *)
    printk(FPU_VERSION' '__DATE__' (C) W. Metzenthen.\n');
{$endif}(* PRINT_MESSAGES *)

    (* Get a name string for error reporting *)
(*      for i:=0 to exception_names[i].type_ do
      if ( (exception_names[i].type_ and n) = exception_names[i].type_ ) then
        break;

    if (exception_names[i].type_)<>0 then
begin
end
    else
printk(Format('FPU emulator: Unknown Exception: $%04x!\n',[n]));*)

    if (n = EX_INTERNAL) then
    begin
      printk(Format('FPU emulator: Internal error type $%04x\n', [int_type]));
      //FPU_printall();
    end;
{$ifdef PRINT_MESSAGES}
    else
FPU_printall();
{$endif}(* PRINT_MESSAGES *)

    (*
     * The 80486 generates an interrupt on the next non-control FPU
     * instruction. So we need some means of flagging it.
     * We use the ES (Error Summary) bit for this.
     *)
  end;
  //RE_ENTRANT_CHECK_ON;

end;


 (* Real operation attempted on a NaN. *)
 (* Returns < 0 if the exception is unmasked *)
function real_1op_NaN (a: PFPU_REG): intg;
var
  signalling, isNaN: intg;
  cond: boolean;
begin
  cond  := (((a^.exp and $7fff) - EXTENDED_Ebias) = EXP_OVER) and
    ((a^.sigh and $8000) <> 0);

  isNaN := word(cond);

(* The default result for the case of two 'equal' NaNs (signs may
   differ) is chosen to reproduce 80486 behaviour *)
  signalling := word((isNaN <> 0) and ((a^.sigh and $40000000) = 0));

  if (signalling = 0) then
  begin
    if (isNaN) = 0 then (* pseudo-NaN, or other unsupported? *)
    begin
      if (varI387.soft.cwd and CW_Invalid) <> 0 then
        (* Masked response *)
        reg_copy(@CONST_QNaN, a);
      fpu_EXCEPTION(EX_Invalid);
//        cond := ifthen((varI387.soft.cwd and CW_Invalid) <> 0, FPU_Exception_c, 0) = 0; [hint]
//        Result := word(cond) or TAG_Special; [hint]
    end;
//      Result := TAG_Special; [hint]
  end;

  if (varI387.soft.cwd and CW_Invalid) <> 0 then
  begin
    (* The masked response *)
    if ((a^.sigh and $8000) = 0) then  (* pseudo-NaN ? *)
      reg_copy(@CONST_QNaN, a);
    (* ensure a Quiet NaN *)
    a^.sigh := a^.sigh or $40000000;
  end;

  fpu_exception(EX_Invalid);

  cond := ifthen((varI387.soft.cwd and CW_Invalid) <> 0, FPU_Exception_c, 0) = 0;
  Result := word(cond) or TAG_Special;
end;

function FPU_gettag (regnr: intg): intg;
begin
  Result := (varI387.soft.twd shr ((regnr and 7) * 2)) and 3;
end;

function FPU_gettagi (stnr: intg): intg;
begin
  Result := (varI387.soft.twd shr (((varI387.soft.ftop + stnr) and 7) * 2)) and 3;
end;

function FPU_Special (ptr: PFPU_REG): intg;
var
  exp: intg;
begin
  exp := exponent(ptr);

  Result := TW_NaN;
  if (exp = EXP_BIAS + EXP_UNDER)
    then Result := TW_Denormal
    else
  if (exp <> EXP_BIAS + EXP_OVER)
    then Result := TW_NaN
    else
  if ((ptr^.sigh = $80000000) and (ptr^.sigl = 0)) then
    Result := TW_Infinity;
end;

procedure FPU_settagi (stnr: intg; tag: intg);
var
  regnr: intg;
begin
  regnr := stnr + top;
  regnr := regnr and 7;
  varI387.soft.twd := varI387.soft.twd and not (3 shl (regnr * 2));
  varI387.soft.twd := varI387.soft.twd or (tag and 3) shl (regnr * 2);
end;

procedure FPU_copy_to_regi (r: PFPU_REG; tag: u_char; stnr: intg);
var
  p: pointer;
begin
  P := st(stnr);
  reg_copy(r, P);
  FPU_settagi(stnr, tag);
end;

procedure FPU_copy_to_reg0 (r: PFPU_REG; tag: u_char);
var
  regnr: intg;
  p: pointer;
begin
  regnr := top;
  regnr := regnr and 7;

  P := st(0);
  reg_copy(r, P);

  varI387.soft.twd := varI387.soft.twd and not (3 shl (regnr * 2));
  varI387.soft.twd := varI387.soft.twd or (tag and 3) shl (regnr * 2);
end;

function real_2op_NaN (b: PFPU_REG; tagb: u_char; deststnr: intg;
  defaultNaN: PFPU_REG): intg;
var
  dest: PFPU_REG;
  a: PFPU_REG;
  taga: u_char;
  x: PFPU_REG;
  signalling, unsupported: intg;
  p: pointer;
begin
  p := st(deststnr);
  dest := P;
  a := dest;
  taga := FPU_gettagi(deststnr);

  if (taga = TAG_Special) then
    taga := FPU_Special(a);

  if (tagb = TAG_Special) then
    tagb := FPU_Special(b);

  (* TW_NaN is also used for unsupported data types. *)
  unsupported := word((taga = TW_NaN) and (exponent(a) = EXP_OVER) and
    ((a^.sigh and $80000000) = 0) or (tagb = TW_NaN) and
    (exponent(b) = EXP_OVER) and ((b^.sigh and $80000000) = 0));
  if (unsupported) <> 0 then
  begin
    if (varI387.soft.cwd and CW_Invalid) <> 0 then
    begin
      (* Masked response *)
      FPU_copy_to_regi(@CONST_QNaN, TAG_Special, deststnr);
    end;
    fpu_exception(EX_Invalid);
//      Result := word(ifthen((varI387.soft.cwd and CW_Invalid) <> 0,
//        FPU_Exception_c, 0) = 0);  [hint]
  end;

  if (taga = TW_NaN) then
  begin
    x := a;
    if (tagb = TW_NaN) then
    begin
      signalling := word((a^.sigh and b^.sigh and $40000000) = 0);
      if (significand(b)^ > significand(a)^)
        then x := b
        else
        if (significand(b)^ = significand(a)^) then
        begin
            (* The default result for the case of two 'equal' NaNs (signs may
            differ) is chosen to reproduce 80486 behaviour *)
          x := defaultNaN;
        end;
    end else
    begin
      (* return the quiet version of the NaN in a *)
      signalling := word((a^.sigh and $40000000) = 0);
    end;
  end
  else
{$ifdef PARANOID}
  if (tagb = TW_NaN) then
{$endif}(* PARANOID *)
    begin
    signalling := word((b^.sigh and $40000000) = 0);
    x := b;
    end;
{$ifdef PARANOID}
else
  begin
    signalling := 0;
    fpu_EXCEPTION(EX_INTERNAL|$113);
    x := @CONST_QNaN;
  end;
{$endif}(* PARANOID *)

  if ((signalling = 0) or ((varI387.soft.cwd and CW_Invalid) <> 0)) then
    begin
    if (x = nil) then
      x := b;
    if ((x^.sigh and $80000000) = 0) then  (* pseudo-NaN ? *)
      x := @CONST_QNaN;
    FPU_copy_to_regi(x, TAG_Special, deststnr);
    if (signalling = 0) then
      begin
      Result := TAG_Special;
      Exit;
      end;
    (* ensure a Quiet NaN *)
    dest^.sigh := dest^.sigh or $40000000;
    end;

  fpu_EXCEPTION(EX_Invalid);

  Result := word(ifthen((varI387.soft.cwd and CW_Invalid) <> 0, FPU_Exception_c, 0) =
    0) or TAG_Special;
end;

function arith_invalid (deststnr: intg): intg;
begin
  fpu_EXCEPTION(EX_Invalid);

  if (varI387.soft.cwd and CW_Invalid) <> 0 then
    // The masked response
    FPU_copy_to_regi(@CONST_QNaN, TAG_Special, deststnr);

  Result := word(ifthen((varI387.soft.cwd and CW_Invalid) <> 0, FPU_Exception_c, 0) =
    0) or TAG_Valid;
end;

function FPU_divide_by_zero (deststnr: intg; sign: u_char): intg;
var
  dest: PFPU_REG;
  tag:  intg;
begin
  dest := st(deststnr);
  tag  := TAG_Valid;

  if (varI387.soft.cwd and CW_ZeroDiv) <> 0 then
  begin
    //The masked response
    FPU_copy_to_regi(@CONST_INF, TAG_Special, deststnr);
    setsign(dest, sign);
    tag := TAG_Special;
  end;

  fpu_exception(EX_ZeroDiv);

  Result := word(ifthen((varI387.soft.cwd and CW_ZeroDiv) <> 0,
    FPU_Exception_c, 0) = 0) or tag;
end;

function set_precision_flag (flags: intg): intg;
begin
  if (varI387.soft.cwd and CW_Precision) <> 0 then
  begin
    varI387.soft.swd := varI387.soft.swd and not (SW_C1 and flags);
    varI387.soft.swd := varI387.soft.swd or flags;   // The masked response
    Result := 0;
  end else
  begin
    fpu_exception(flags);
    Result := 1;
  end;
end;

procedure set_precision_flag_up;
begin
  if (varI387.soft.cwd and CW_Precision) <> 0
    then varI387.soft.swd := varI387.soft.swd or (SW_Precision or SW_C1)   // The masked response
    else fpu_exception(EX_Precision or SW_C1);
end;

procedure set_precision_flag_down;
begin
  if (varI387.soft.cwd and CW_Precision) <> 0 then
  begin   // The masked response
    varI387.soft.swd := varI387.soft.swd and not SW_C1;
    varI387.soft.swd := varI387.soft.swd or SW_Precision;
  end else
    fpu_exception(EX_Precision);
end;

function denormal_operand: intg;
begin
  if (varI387.soft.cwd and CW_Denormal) <> 0 then
  begin   // The masked response
    varI387.soft.swd := varI387.soft.swd or SW_Denorm_Op;
    Result := TAG_Special;
  end else
  begin
    fpu_exception(EX_Denormal);
    Result := TAG_Special or FPU_Exception_c;
  end;
end;

function arith_overflow (dest: PFPU_REG): intg;
var
  tag: intg;
begin
  tag := TAG_Valid;

  if (varI387.soft.cwd and CW_Overflow) <> 0 then
  begin
    // The masked response
    reg_copy(@CONST_INF, dest);
    tag := TAG_Special;
  end else
    // Subtract the magic number from the exponent
    addexponent(dest, (-3 * (1 shl 13)));

  fpu_exception(EX_Overflow);
  if (varI387.soft.cwd and CW_Overflow) <> 0 then
  begin
    { The overflow exception is masked. }
    { By definition, precision is lost.
 The roundup bit (C1) is also set because we have
 "rounded" upwards to Infinity. }
    fpu_exception(EX_Precision or SW_C1);
//      Result := tag; [hint]
  end;

  Result := tag;
end;

function arith_round_overflow (dest: PFPU_REG; sign: u8): intg;
var
  tag, largest: intg;
begin
  tag := TAG_Valid;

  if (varI387.soft.cwd and CW_Overflow) <> 0 then
  begin
    //The masked response */
    // The response here depends upon the rounding mode */
    case (varI387.soft.cwd and CW_RC) of
      RC_CHOP:    // Truncate */
      begin
        largest := 1;
      end;
      RC_UP:    // Towards +infinity */
      begin
        largest := word(sign = SIGN_NEG);
      end;
      RC_DOWN:    // Towards -infinity */
      begin
        largest := word(sign = SIGN_POS);
      end;
    else
      begin
        largest := 0;
      end;
    end;
    if (largest = 0) then
    begin
      reg_copy(@CONST_INF, dest);
      tag := TAG_Special;
    end else
    begin
      dest^.exp := EXTENDED_Ebias + EXP_OVER - 1;
      case (varI387.soft.cwd and CW_PC) of
        01,
        PR_64_BITS:
          pu64(@dest^.sigh)^ := $ffffffffffffffff;
        PR_53_BITS:
          pu64(@dest^.sigh)^ := $fffffffffffff800;
        PR_24_BITS:
          pu64(@dest^.sigh)^ := $ffffff0000000000;
      end;
    end;
  end else
  begin
    // Subtract the magic number from the exponent */
    addexponent(@dest, (-3 * (1 shl 13)));
    largest := 0;
  end;

  fpu_EXCEPTION(EX_Overflow);
  if (varI387.soft.cwd and CW_Overflow) <> 0 then
    begin
    // The overflow exception is masked.
    if (largest) <> 0 then
      begin
      fpu_EXCEPTION(EX_Precision);
      end
    else
      begin
      // By definition, precision is lost.
      //The roundup bit (C1) is also set because we have
      //"rounded" upwards to Infinity. */
      fpu_EXCEPTION(EX_Precision or SW_C1);
      end;
    Result := tag;
    exit;
    end;

  Result := tag;
end;

function arith_underflow (dest: PFPU_REG): intg;
var
  tag: intg;
begin
  tag := TAG_Valid;

  if (varI387.soft.cwd and CW_Underflow) <> 0 then
  begin
    // The masked response */
    if (exponent16(dest) <= EXP_UNDER - 63) then
    begin
      reg_copy(@CONST_Z, dest);
      varI387.soft.swd := varI387.soft.swd and not SW_C1;       // Round down. */
      tag := TAG_Zero;
    end else
      stdexp(dest);
  end else
    // Add the magic number to the exponent. */
    addexponent(dest, (3 * (1 shl 13)) + EXTENDED_Ebias);

  fpu_EXCEPTION(EX_Underflow);
  if (varI387.soft.cwd and CW_Underflow) <> 0 then
  begin
    // The underflow exception is masked. */
    fpu_EXCEPTION(EX_Precision);
    Result := tag;
    exit;
  end;
  Result := tag;
end;

procedure FPU_stack_overflow;
begin
  if (varI387.soft.cwd and CW_Invalid) <> 0 then
  begin
    // The masked response */
    Dec(varI387.soft.ftop);
    FPU_copy_to_reg0(@CONST_QNaN, TAG_Special);
  end;

  fpu_EXCEPTION(EX_StackOver);

end;

procedure FPU_stack_underflow;
begin
  if (varI387.soft.cwd and CW_Invalid) <> 0 then
  begin
    // The masked response */
    FPU_copy_to_reg0(@CONST_QNaN, TAG_Special);
  end;

  fpu_EXCEPTION(EX_StackUnder);
end;

procedure FPU_stack_underflow_i (i: intg);
begin
  if (varI387.soft.cwd and CW_Invalid) <> 0 then
  begin
    // The masked response */
    FPU_copy_to_regi(@CONST_QNaN, TAG_Special, i);
  end;

  fpu_EXCEPTION(EX_StackUnder);
end;

procedure FPU_stack_underflow_pop (i: intg);
begin
  if (varI387.soft.cwd and CW_Invalid) <> 0 then
  begin
    // The masked response */
    FPU_copy_to_regi(@CONST_QNaN, TAG_Special, i);
    FPU_pop();
  end;

  fpu_EXCEPTION(EX_StackUnder);
end;

procedure fadd__ ();
var
  i: intg;
begin
  (* fadd st,st(i) *)
  i := fpu_rm^;
  clear_C1();
  FPU_add(st(i), FPU_gettagi(i), 0, varI387.soft.cwd);
end;

procedure fmul__ ();
var
  i: intg;
begin
  (* fmul st,st(i) *)
  i := FPU_rm^;
  clear_C1();
  FPU_mul(st(i), FPU_gettagi(i), 0, varI387.soft.cwd);
end;

procedure fsub__ ();
begin
  (* fsub st,st(i) *)
  clear_C1();
  FPU_sub(0, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;

procedure fsubr_ ();
begin
  (* fsubr st,st(i) *)
  clear_C1();
  FPU_sub(REV, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;


procedure fdiv__ ();
begin
  (* fdiv st,st(i) *)
  clear_C1();
  FPU_div(0, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;


procedure fdivr_ ();
begin
  (* fdivr st,st(i) *)
  clear_C1();
  FPU_div(REV, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;

procedure fadd_i ();
var
  i: intg;
begin
  (* fadd st(i),st *)
  i := FPU_rm^;
  clear_C1();
  FPU_add(st(i), FPU_gettagi(i), i, varI387.soft.cwd);
end;

procedure fmul_i ();
begin
  (* fmul st(i),st *)
  clear_C1();
  FPU_mul(st(0), FPU_gettag0(), FPU_rm^, varI387.soft.cwd);
end;

procedure fsubri ();
begin
  (* fsubr st(i),st *)
  clear_C1();
  FPU_sub(DEST_RM, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;

procedure fsub_i ();
begin
  (* fsub st(i),st *)
  clear_C1();
  FPU_sub(REV or DEST_RM, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd);
end;

procedure fdivri ();
begin
  (* fdivr st(i),st *)
  clear_C1();
  FPU_div(DEST_RM, PFPU_REG(FPU_rm^), varI387.soft.cwd);
end;

procedure fdiv_i ();
begin
  (* fdiv st(i),st *)
  clear_C1();
  FPU_div(REV or DEST_RM, PFPU_REG(FPU_rm^), varI387.soft.cwd);
end;

procedure faddp_ ();
var
  i: intg;
begin
  (* faddp st(i),st *)
  i := FPU_rm^;
  clear_C1();
  if (FPU_add(st(i), FPU_gettagi(i), i, varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure fmulp_ ();
begin
  (* fmulp st(i),st *)
  clear_C1();
  if (FPU_mul(st(0), FPU_gettag0(), FPU_rm^, varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure fsubrp ();
begin
  (* fsubrp st(i),st *)
  clear_C1();
  if (FPU_sub(DEST_RM, PFPU_REG(varI387.soft.rm), varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure fsubp_ ();
begin
  (* fsubp st(i),st *)
  clear_C1();
  if (FPU_sub(REV or DEST_RM, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure fdivrp ();
begin
  (* fdivrp st(i),st *)
  clear_C1();
  if (FPU_div(DEST_RM, PFPU_REG(bx_ptr_equiv_t(FPU_rm)), varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure fdivp_ ();
begin
  (* fdivp st(i),st *)
  clear_C1();
  if (FPU_div(REV or DEST_RM, PFPU_REG(bx_ptr_equiv_t(FPU_rm)),
    varI387.soft.cwd) >= 0) then
    FPU_pop();
end;

procedure FPU_pop;
begin
  varI387.soft.twd := varI387.soft.twd or 3 shl ((varI387.soft.ftop and 7) * 2);
  Inc(varI387.soft.ftop);
end;

function NOT_EMPTY (stnr: intg): intg;
begin
  Result := word(FPU_empty_i(stnr) = 0);
end;

function FPU_empty_i (stnr: intg): intg;
var
  regnr: intg;
begin
  regnr := (varI387.soft.ftop + stnr) and 7;
  Result := word(((varI387.soft.twd shr (regnr * 2)) and 3) = TAG_Empty);
end;


end.
