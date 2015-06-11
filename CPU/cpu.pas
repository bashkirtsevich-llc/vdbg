{ ****************************************************************************** }
{ Mozaa 0.95 - Virtual PC emulator - developed by Massimiliano Boidi 2003 - 2004 }
{ ****************************************************************************** }

{ For any question write to maxboidi@libero.it }

(* **** *)
{$include defines.pas}
unit cpu;

interface

uses CONFIG, Service, Math, Memory, SysUtils, jumpfar, StrUtils;

const
  BX_INSTR_ADD8         = 1;
  BX_INSTR_ADD16        = 2;
  BX_INSTR_ADD32        = 3;

  BX_INSTR_SUB8         = 4;
  BX_INSTR_SUB16        = 5;
  BX_INSTR_SUB32        = 6;

  BX_INSTR_ADC8         = 7;
  BX_INSTR_ADC16        = 8;
  BX_INSTR_ADC32        = 9;

  BX_INSTR_SBB8         = 10;
  BX_INSTR_SBB16        = 11;
  BX_INSTR_SBB32        = 12;

  BX_INSTR_CMP8         = 13;
  BX_INSTR_CMP16        = 14;
  BX_INSTR_CMP32        = 15;

  BX_INSTR_INC8         = 16;
  BX_INSTR_INC16        = 17;
  BX_INSTR_INC32        = 18;

  BX_INSTR_DEC8         = 19;
  BX_INSTR_DEC16        = 20;
  BX_INSTR_DEC32        = 21;

  BX_INSTR_NEG8         = 22;
  BX_INSTR_NEG16        = 23;
  BX_INSTR_NEG32        = 24;

  BX_INSTR_XADD8        = 25;
  BX_INSTR_XADD16       = 26;
  BX_INSTR_XADD32       = 27;

  BX_INSTR_OR8          = 28;
  BX_INSTR_OR16         = 29;
  BX_INSTR_OR32         = 30;

  BX_INSTR_AND8         = 31;
  BX_INSTR_AND16        = 32;
  BX_INSTR_AND32        = 33;

  BX_INSTR_TEST8        = 34;
  BX_INSTR_TEST16       = 35;
  BX_INSTR_TEST32       = 36;

  BX_INSTR_XOR8         = 37;
  BX_INSTR_XOR16        = 38;
  BX_INSTR_XOR32        = 39;

  BX_INSTR_CMPS8        = 40;
  BX_INSTR_CMPS16       = 41;
  BX_INSTR_CMPS32       = 42;

  BX_INSTR_SCAS8        = 43;
  BX_INSTR_SCAS16       = 44;
  BX_INSTR_SCAS32       = 45;

  BX_INSTR_SHR8         = 46;
  BX_INSTR_SHR16        = 47;
  BX_INSTR_SHR32        = 48;

  BX_INSTR_SHL8         = 49;
  BX_INSTR_SHL16        = 50;
  BX_INSTR_SHL32        = 51;

  BX_LF_INDEX_KNOWN     = 0;
  BX_LF_INDEX_OSZAPC    = 1;
  BX_LF_INDEX_OSZAP     = 2;
  BX_LF_INDEX_P         = 3;

  BX_LF_MASK_OSZAPC     = $111111;
  BX_LF_MASK_OSZAP      = $222220;
  BX_LF_MASK_P          = $000030;

  BxImmediate         = $000f; // bits 3..0: any immediate
  BxImmediate_Ib      = $0001; // 8 bits regardless
  BxImmediate_Ib_SE   = $0002; // sign extend to OS size
  BxImmediate_Iv      = $0003; // 16 or 32 depending on OS size
  BxImmediate_Iw      = $0004; // 16 bits regardless
  BxImmediate_IvIw    = $0005; // call_Ap
  BxImmediate_IwIb    = $0006; // enter_IwIb
  BxImmediate_O       = $0007; // mov_ALOb, mov_ObAL, mov_eAXOv, mov_OveAX
  BxImmediate_BrOff8  = $0008; // Relative branch offset byte
  BxImmediate_BrOff16 = $0009; // Relative branch offset word
  BxImmediate_BrOff32 = BxImmediate_Iv;

  BxPrefix          = $0010; // bit  4
  BxAnother         = $0020; // bit  5
  BxRepeatable      = $0040; // bit  6
  BxRepeatableZF    = $0080; // bit  7
  BxGroupN          = $0100; // bits 8
  BxGroup1          = BxGroupN;
  BxGroup2          = BxGroupN;
  BxGroup3          = BxGroupN;
  BxGroup4          = BxGroupN;
  BxGroup5          = BxGroupN;
  BxGroup6          = BxGroupN;
  BxGroup7          = BxGroupN;
  BxGroup8          = BxGroupN;
  BxGroup9          = BxGroupN;
  BxGroupa          = BxGroupN;

  BX_TASK_FROM_JUMP          = 10;
  BX_TASK_FROM_CALL_OR_INT   = 11;
  BX_TASK_FROM_IRET          = 12;
  BX_DE_EXCEPTION =  0; // Divide Error (fault)
  BX_DB_EXCEPTION =  1; // Debug (fault/trap)
  BX_BP_EXCEPTION =  3; // Breakpoint (trap)
  BX_OF_EXCEPTION =  4; // Overflow (trap)
  BX_BR_EXCEPTION =  5; // BOUND (fault)
  BX_UD_EXCEPTION =  6;
  BX_NM_EXCEPTION =  7;
  BX_DF_EXCEPTION =  8;
  BX_TS_EXCEPTION = 10;
  BX_NP_EXCEPTION = 11;
  BX_SS_EXCEPTION = 12;
  BX_GP_EXCEPTION = 13;
  BX_PF_EXCEPTION = 14;
  BX_MF_EXCEPTION = 16;
  BX_AC_EXCEPTION = 17;

  SREG_ES = 0;
  SREG_CS = 1;
  SREG_SS = 2;
  SREG_DS = 3;
  SREG_FS = 4;
  SREG_GS = 5;

  // segment register encoding
  SEG_REG_ES = 0;
  SEG_REG_CS = 1;
  SEG_REG_SS = 2;
  SEG_REG_DS = 3;
  SEG_REG_FS = 4;
  SEG_REG_GS = 5;
  SEG_REG_NULL = 8;
  //BX_NULL_SEG_REG(seg) ((seg) & BX_SEG_REG_NULL)  and BX_SEG_REG_NULL
  BX_READ       = 10;
  BX_WRITE      = 11;
  BX_RW         = 12;

  BX_REG8L_OFFSET = 0;
  BX_REG8H_OFFSET = 1;
  BX_REG16_OFFSET = 0;

  REG_AL_8BIT = 0;
  REG_CL_8BIT = 1;
  REG_DL_8BIT = 2;
  REG_BL_8BIT = 3;
  REG_AH_8BIT = 0;
  REG_CH_8BIT = 1;
  REG_DH_8BIT = 2;
  REG_BH_8BIT = 3;

  REG_AX_16BIT = 0;
  REG_CX_16BIT = 1;
  REG_DX_16BIT = 2;
  REG_BX_16BIT = 3;
  REG_SP_16BIT = 4;
  REG_BP_16BIT = 5;
  REG_SI_16BIT = 6;
  REG_DI_16BIT = 7;

  REG_EAX_32BIT = 0;
  REG_ECX_32BIT = 1;
  REG_EDX_32BIT = 2;
  REG_EBX_32BIT = 3;
  REG_ESP_32BIT = 4;
  REG_EBP_32BIT = 5;
  REG_ESI_32BIT = 6;
  REG_EDI_32BIT = 7;

  BX_MSR_P5_MC_ADDR      = $0000;
  BX_MSR_MC_TYPE         = $0001;
  BX_MSR_TSC	           = $0010;
  BX_MSR_CESR		         = $0011;
  BX_MSR_CTR0		         = $0012;
  BX_MSR_CTR1		         = $0013;
  BX_MSR_APICBASE		     = $001b;
  BX_MSR_EBL_CR_POWERON  = $002a;
  BX_MSR_TEST_CTL	       = $0033;
  BX_MSR_BIOS_UPDT_TRIG  = $0079;
  BX_MSR_BBL_CR_D0	     = $0088;
  BX_MSR_BBL_CR_D1	     = $0089;
  BX_MSR_BBL_CR_D2	     = $008a;
  BX_MSR_BBL_CR_D3	     = $008b;	// = BIOS_SIGN
  BX_MSR_PERFCTR0		     = $00c1;
  BX_MSR_PERFCTR1		     = $00c2;
  BX_MSR_MTRRCAP		     = $00fe;
  BX_MSR_BBL_CR_ADDR     = $0116;
  BX_MSR_BBL_DECC		     = $0118;
  BX_MSR_BBL_CR_CTL	     = $0119;
  BX_MSR_BBL_CR_TRIG     = $011a;
  BX_MSR_BBL_CR_BUSY     = $011b;
  BX_MSR_BBL_CR_CTL3     = $011e;
  BX_MSR_MCG_CAP		     = $0179;
  BX_MSR_MCG_STATUS	     = $017a;
  BX_MSR_MCG_CTL		     = $017b;
  BX_MSR_EVNTSEL0		     = $0186;
  BX_MSR_EVNTSEL1		     = $0187;
  BX_MSR_DEBUGCTLMSR     = $01d9;
  BX_MSR_LASTBRANCHFROMIP	= $01db;
  BX_MSR_LASTBRANCHTOIP	 = $01dc;
  BX_MSR_LASTINTOIP	     = $01dd;
  BX_MSR_ROB_CR_BKUPTMPDR6	= $01e0;
  BX_MSR_MTRRPHYSBASE0	 = $0200;
  BX_MSR_MTRRPHYSMASK0	 = $0201;
  BX_MSR_MTRRPHYSBASE1	 = $0202;

  (* WARNING:
   Only BX_CPU_C member functions can use these shortcuts safely!
   Functions that use the shortcuts outside of BX_CPU_C might work
   when BX_USE_CPU_SMF=1 but will fail when BX_USE_CPU_SMF=0
   (for example in SMP mode).
  *)

type
  PCPUResetInfo = ^TCPUResetInfo;
  TCPUResetInfo = packed record
    val_EAX,
    val_ECX,
    val_EDX,
    val_EBX,
    val_ESP,
    val_EBP,
    val_ESI,
    val_EDI,
    val_ES,
    val_CS,
    val_SS,
    val_DS,
    val_FS,
    val_GS,
    val_DR0,
    val_DR1,
    val_DR2,
    val_DR3,
    val_DR6,
    val_DR7,
    val_CR0,
    val_CR1,
    val_CR2,
    val_CR3,
    val_CR4,
    val_EIP: Cardinal;
  end;

  TLF_flags_entry = record
    op1_8: Bit8u;
    op2_8: Bit8u;
    result_8: Bit8u;

    op1_16: Bit16u;
    op2_16: Bit16u;
    result_16: Bit16u;

    op1_32: Bit32u;
    op2_32: Bit32u;
    result_32: Bit32u;

    prev_CF: Bool;
    instr: Word;
  end;

  TFlags_reg_t = record
     { 31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16
      ==|==|=====|==|==|==|==|==|==|==|==|==|==|==|==
       0| 0| 0| 0| 0| 0| 0| 0| 0| 0|ID|VP|VF|AC|VM|RF

      15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0
      ==|==|=====|==|==|==|==|==|==|==|==|==|==|==|==
       0|NT| IOPL|OF|DF|IF|TF|SF|ZF| 0|AF| 0|PF| 1|CF }


    // In order to get access to these fields from the Dynamic Translation
    // code, using only 8bit offsets, I needed to move these fields
    // together.
    cf: Bit32u;
    af: Bit32u;
    zf: Bit32u;
    sf: Bit32u;
    of_: Bit32u;

    bit1    :Bool;
    pf_byte :Bit8u;  { PF derived from last result byte when needed }
    bit3    :Bool;
    bit5    :Bool;
    tf      :Bool;
    if_     :Bool;
    df: Bool;
    iopl: Bit8u;
    nt: Bool;
    bit15: Bool;
    rf: Bool;
    vm: Bool;
    ac: Bool;  // alignment check
    // Bool vif; // Virtual Interrupt Flag
    // Bool vip; // Virtual Interrupt Pending
    id: Bool;  // late model 486 and beyond had CPUID
  end;
  // +++++++++++++++++++++++++++++++++++++++++++++
  TCR0_t = packed record
    val32: Bit32u; // 32bit value of register

    // bitfields broken out for efficient access
    pg: Bool; // paging
    // CR0 notes:
    //   Each x86 level has its own quirks regarding how it handles
    //   reserved bits.  I used DOS DEBUG.EXE in real mode on the
    //   following processors, tried to clear bits 1..30, then tried
    //   to set bits 1..30, to see how these bits are handled.
    //   I found the following:
    //
    //   Processor    try to clear bits 1..30    try to set bits 1..30
    //   386          7FFFFFF0                   7FFFFFFE
    //   486DX2       00000010                   6005003E
    //   Pentium      00000010                   7FFFFFFE
    //   Pentium-II   00000010                   6005003E
    //
    // My assumptions:
    //   All processors: bit 4 is hardwired to 1 (not true on all clones)
    //   386: bits 5..30 of CR0 are also hardwired to 1
    //   Pentium: reserved bits retain value set using mov cr0, reg32
    //   486DX2/Pentium-II: reserved bits are hardwired to 0
    cd: Bool; // cache disable
    nw: Bool; // no write-through
    am: Bool; // alignment mask
    wp: Bool; // write-protect
    ne: Bool; // numerics exception
    ts: Bool; // task switched
    em: Bool; // emulate math coprocessor
    mp: Bool; // monitor coprocessor
    pe: Bool; // protected mode enable
   end;

  TRegs_msr_t = packed record
    p5_mc_addr    :Bit8u;
    p5_mc_type    :Bit8u;
    tsc           :Bit8u;
    cesr          :Bit8u;
    ctr0          :Bit8u;
    ctr1          :Bit8u;
    apicbase      :Bit64u;
  end;

  PSelector_t = ^TSelector_t;
  TSelector_t = packed record  // bx_selector_t
    value: Bit16u;        // the 16bit value of the selector
                          // the following fields are extracted from the value field in protected
                          // mode only.  They're used for sake of efficiency
    index: Bit16u;        // 13bit index extracted from value in protected mode
    ti: Bit8u;            // table indicator bit extracted from value
    rpl: Bit8u;           // RPL extracted from value
  end;

  PDescriptor_t = ^TDescriptor_t;
  TDescriptor_t = packed record
    valid: Bool;            // 0 = invalid, 1 = valid */
    p: Bool;                // present */
    dpl: Bit8u;             // descriptor privilege level 0..3 */
    segmenttype: Bool;      // 0 = system/gate, 1 = data/code segment */
    type_: Bit8u;          { For system & gate descriptors, only
                            *  0 = invalid descriptor (reserved)
                            *  1 = 286 available Task State Segment (TSS)
                            *  2 = LDT descriptor
                            *  3 = 286 busy Task State Segment (TSS)
                            *  4 = 286 call gate
                            *  5 = task gate
                            *  6 = 286 interrupt gate
                            *  7 = 286 trap gate
                            *  8 = (reserved)
                            *  9 = 386 available TSS
                            * 10 = (reserved)
                            * 11 = 386 busy TSS
                            * 12 = 386 call gate
                            * 13 = (reserved)
                            * 14 = 386 interrupt gate
                            * 15 = 386 trap gate }
    case integer of
    0 :(
        segment : packed record
          executable      : Bool;   // 1=code, 0=data or stack segment */
          c_ed            : Bool;   // for code: 1=conforming, for data/stack: 1=expand down */
          r_w             : Bool;   // for code: readable?, for data/stack: writeable? */
          a               : Bool;   // accessed? */
          base            : Bit32u; // base address: 286=24bits, 386=32bits */
          limit           : Bit32u; // limit: 286=16bits, 386=20bits */
          limit_scaled    : Bit32u; { for efficiency, this contrived field is set to
                                      * limit for byte granular, and
                                      * (limit << 12) | 0xfff for page granular seg's }
          g   :Bool;                // granularity: 0=byte, 1=4K (page) */
          d_b :Bool;                // default size: 0=16bit, 1=32bit */
          avl :Bool;                // available for use by system */
        end; // END 'segment' record type
        gate286 : packed record
          word_count    :Bit8u;     // 5bits (0..31) #words to copy from caller's stack
                                    // to called procedure's stack.  (call gates only)*/
          dest_selector : Bit16u;
          dest_offset   : Bit16u;
        end;
        taskgate: packed record            // type 5: Task Gate Descriptor
          tss_selector  : Bit16u;   // TSS segment selector
        end;
        gate386: packed record
          dword_count   : Bit8u;    // 5bits (0..31) #dwords to copy from caller's stack
                                    // to called procedure's stack.  (call gates only)*/
          dest_selector : Bit16u;
          dest_offset   : Bit32u;
        end;
        tss286: packed record
          base          : Bit32u;   // 24 bit 286 TSS base  */
          limit         : Bit16u;   // 16 bit 286 TSS limit */
        end;
        tss386: packed record
          base          : Bit32u;   // 32 bit 386 TSS base  */
          limit         : Bit32u;   // 20 bit 386 TSS limit */
          limit_scaled  : Bit32u;   // Same notes as for 'segment' field
          g             : Bool;     // granularity: 0=byte, 1=4K (page) */
          avl           : Bool;     // available for use by system */
        end;
        ldt: packed record
          base          : Bit32u;   // 286=24 386+ =32 bit LDT base */
          limit         : Bit16u;   // 286+ =16 bit LDT limit */
        end;
      );
    end;

  PSegment_reg_t = ^TSegment_reg_t;
  TSegment_reg_t = packed record
    selector: TSelector_t;
    cache: TDescriptor_t;
  end;

  TVoidFPtr_t = procedure;

  TCPUEvent = procedure (aInstruction: pointer) of object;

  PInstruction_tag = ^TInstruction_tag;
  TInstruction_tag = packed record
    // prefix stuff here...
    name                : array[0..15] of char;
    attr                : word;   // attribute from fetchdecode
    b1                  : word;   // opcode1 byte
    rep_used            : word;
    modrm               : word;   // mod-nnn-r/m byte
      mod_              : word;
      nnn               : word;
      rm                : word;
    displ16u            : Bit16u; // for 16-bit modrm forms
    displ32u            : Bit32u; // for 32-bit modrm forms
    seg                 : word;
    sib                 : word;   // scale-index-base (2nd modrm byte)
      scale             : word;
      index             : word;
      base              : word;
    addr_displacement   : Bit32u; // address displacement
    rm_addr             : Bit32u;
    Id                  : Bit32u;
    Iw                  : Bit16u;
    Ib                  : Bit8u;
    Ib2                 : Bit8u;  // for ENTER_IwIb
    Iw2                 : Bit16u; // for JMP_Ap
    ilen                : word;   // instruction length
    os_32, as_32        : word;   // OperandSize/AddressSize is 32bit
    flags_in, flags_out : word;   // flags needed, flags modified

    ResolveModrm        : procedure(Istruction:PInstruction_tag) of object;
    execute             : procedure(Istruction:PInstruction_tag) of object;
  end;

  PInstruction_t2 = ^TInstruction_t2;
  TInstruction_t2 = procedure(I: PInstruction_tag) of object;

  PInstruction_t  = ^TInstruction_t;
  TInstruction_t  = procedure(I: PInstruction_tag) of object;

  PExecutePtr_t   = ^TExecutePtr_t;
  TExecutePtr_t    = procedure(I: PInstruction_tag) of object; // typedef void (*BxExecutePtr_t)(BxInstruction_t *);

  TGlobal_segment_reg_t = packed record
    base            :Bit32u;        // base address: 24bits=286,32bits=386
    limit           :Bit16u;        // limit, 16bits
  end;

  TTLB_entry = packed record
    lpf             :Bit32u;    // linear page frame
    ppf             :Bit32u;    // physical page frame
    pte_addr        :Bit32u;    // Page Table Address for updating A & D bits
    combined_access :Bit32u;
  end;

  TGen_reg_t = packed record
    case integer of
      0: ( erx:Bit32u; );
      1: ( rx:Bit16u; );
      2: ( rl:Bit8u;
          rh:Bit8u; );
    end;

  TApic_type_t = (APIC_TYPE_NONE, APIC_TYPE_IOAPIC, APIC_TYPE_LOCAL_APIC);

  TDTShim_t = procedure;

  POpcodeInfo_t = ^TOpcodeInfo_t;
  TOpcodeInfo_t = packed record
    name: array [0..15] of char;
    Attr: Bit16u;
    ExecutePtr: TExecutePtr_t;
    AnotherArray: POpcodeInfo_t;
    Counter: int64;
  end;

{ --------------------------------------------------------------------------------- }
const
  BX_INVALID_TLB_ENTRY = $ffffffff;
  BX_PRIV_CHECK_SIZE   = 32;

var
  priv_check_array: array[0..BX_PRIV_CHECK_SIZE] of Word;

const
  BX_DEVICE_ID       = 3;
  BX_STEPPING_ID     = 0;

const
  BX_ET_BENIGN       = 0;
  BX_ET_CONTRIBUTORY = 1;
  BX_ET_PAGE_FAULT   = 2;
  BX_ET_DOUBLE_FAULT = 10;

  is_exception_OK: array[0..2,0..2] of Bool =
    ((1, 1, 1),   (* 1st exception is BENIGN *)
     (1, 0, 1) ,  (* 1st exception is CONTRIBUTORY *)
     (1, 0, 0));  (* 1st exception is PAGE_FAULT *)

const
  BX_MAX_TIMERS        = 16;
  BX_NULL_TIMER_HANDLE = 10000; // set uninitialized timer handles to this

type
  PCS_OP = (PCS_CLEAR, PCS_SET, PCS_TOGGLE );
  TTimer_handler_t = procedure(this_ptr: Pointer) of object;

  EJmp_Exception = class(Exception)

  end;

  PPC_System = ^TPC_System;
  TPC_System = class
  public
    timer :array [0..BX_MAX_TIMERS] of packed record
        period: Bit64u;
        remaining: Bit64u;
        active: Bool;
        continuous: Bool;
        triggered: Bool;
        funct: TTimer_handler_t;
        this_ptr: Pointer;
      end;
    num_timers: unsigned;
    num_cpu_ticks_in_period: Bit64u;
    num_cpu_ticks_left: Bit64u;

    DRQ: array[0..8] of Bool;  // DMA Request
    DACK: array[0..8] of Bool; // DMA Acknowlege
    TC: bool;                  // Terminal Count
    HRQ: bool;                 // Hold Request
    HLDA: bool;                // Hold Acknowlege
    //Boolean INTR;            // Interrupt

    // Address line 20 control:
    //   1 = enabled: extended memory is accessible
    //   0 = disabled: A20 address line is forced low to simulate
    //       an 8088 address map
    enable_a20: Bool;
    // start out masking physical memory addresses to:
    //   8086:      20 bits
    //    286:      24 bits
    //    386:      32 bits
    // when A20 line is disabled, mask physical memory addresses to:
    //    286:      20 bits
    //    386:      20 bits
    //
    a20_mask: Bit32u;
    COUNTER_INTERVAL: Bit64u;
    counter: Bit64u;
    counter_timer_index: integer;

    constructor Create;
    procedure   set_DRQ(channel:unsigned; val:bool);
    procedure   set_DACK(channel:unsigned; val:bool);
    procedure   set_TC(val:bool);   // set the Terminal Count line
    procedure   set_HRQ(val:bool);  // set the Hold ReQuest line
    procedure   raise_HLDA; // raise the HoLD Acknowlege line
    procedure   set_INTR(value:bool); // set the INTR line to value

    //function IntEnabled:Integer;
    //function InterruptSignal( operation:PCS_OP ):Integer;
    function    ResetSignal( operation:PCS_OP ):Integer;
    function    IAC:Bit8u;

    procedure   init_ips(ips:Bit32u);
    procedure   timer_handler;
    function    ticks_remaining(index:Integer):Int64;
    function    register_timer( this_ptr:Pointer; funct:TTimer_handler_t; useconds:Bit32u; continuous:Bool;active:Bool):Integer;
//    procedure   start_timers;
    procedure   activate_timer( timer_index:unsigned; useconds:Bit32u; continuous:Bool );
    procedure   deactivate_timer( timer_index:unsigned );
    procedure   tickn(n:Bit64u);

    function    register_timer_ticks(this_ptr:Pointer; funct:TTimer_handler_t; Instructions:Bit64u; continuous:Bool; active:Bool):Integer;
    procedure   activate_timer_ticks(timer_index:unsigned; instructions:Bit64u; continuous:Bool);
    procedure   counter_timer_handler(this_ptr:Pointer);
    //procedure wait_for_event();

    function    time_usec:Bit64u;
    function    time_ticks:Bit64u;

    procedure   dma_write8(phy_addr:Bit32u; channel:unsigned; verify:Bool);
    procedure   dma_read8(phy_addr:Bit32u; channel:unsigned);
    procedure   dma_write16(phy_addr:Bit32u; channel:unsigned; verify:Bool);
    procedure   dma_read16(phy_addr:Bit32u; channel:unsigned);

    function    inp(addr:Bit16u; io_len:unsigned):Bit32u;
    procedure   outp(addr:Bit16u; value:Bit32u; io_len:unsigned);
    procedure   set_enable_a20(value:Bit8u);
    function    get_enable_a20:Bool;
    procedure   exit;
    procedure   expire_ticks;
  end;

  PCPU = ^TCPU;
  TCPU = class
  private
    FEIP                      : Bit32u;    // instruction pointer
    FCPUName                  : array[0..64] of char;
  public
    fake_start                : byte;
//    FIP                       : Bit16u;
    gen_reg                   : array[0..8] of TGen_reg_t;
    curr_exception            : array[0..2] of Bit8u;
    // so that we can back up when handling faults, exceptions, etc.
    // we need to store the value of the instruction pointer, before
    // each fetch/execute cycle.
    prev_eip                  : Bit32u;
    // A few pointer to functions for use by the dynamic translation
    // code.  Keep them close to the gen_reg declaration, so I can
    // use an 8bit offset to access them.
    lf_flags_status           : Bit32u;
    lf_pf                     : Bool;
    eflags                    : TFlags_reg_t;
    oszapc                    : TLF_flags_entry;
    oszap                     : TLF_flags_entry;
    prev_esp                  : Bit32u;
    inhibit_mask:word;
    // user segment register set
    sregs                     : array[0..10] of TSegment_reg_t;
    // system segment registers
    gdtr                      : TGlobal_segment_reg_t; // global descriptor table register
    idtr                      : TGlobal_segment_reg_t; // interrupt descriptor table register
    ldtr                      : TSegment_reg_t;        // interrupt descriptor table register
    tr                        : TSegment_reg_t;        // task register
    // debug registers 0-7 (unimplemented)
    FDR0                      : Bit32u;
    FDR1                      : Bit32u;
    FDR2                      : Bit32u;
    FDR3                      : Bit32u;
    FDR6                      : Bit32u;
    FDR7                      : Bit32u;
    // TR3 - TR7 (Test Register 3-7), unimplemented */
    // Control registers
    FCR0                      : TCR0_t;
    FCR1                      : Bit32u;
    FCR2                      : Bit32u;
    FCR3                      : Bit32u;
    FCR4                      : Bit32u;
    msr                       : TRegs_msr_t;

    EXT                       : Bool;     // 1 if processing external interrupt or exception
                                          // or if not related to current instruction,
                                          // 0 if current CS:IP caused exception */
    errorno                   : Word;     // signal exception during instruction emulation */

    debug_trap                : Bit32u;   // holds DR6 value to be set as well
    async_event               : Bool;
    INTR                      : Bool;
                                          // wether this CPU is the BSP always set for UP */
    bsp                       : Bool;
    // for accessing registers by index number
    _16bit_base_reg           : array[0..8] of PBit16u;
    _16bit_index_reg          : array[0..8] of PBit16u;
    empty_register            : Bit32u;

    // for decoding instructions; accessing seg reg's by index
    sreg_mod00_rm16           : array[0..8] of Word;
    sreg_mod01_rm16           : array[0..8] of Word;
    sreg_mod10_rm16           : array[0..8] of Word;
    sreg_mod01_rm32           : array[0..8] of Word;
    sreg_mod10_rm32           : array[0..8] of Word;
    sreg_mod0_base32          : array[0..8] of Word;
    sreg_mod1or2_base32       : array[0..8] of Word;
    save_cs                   : TSegment_reg_t;
    save_ss                   : TSegment_reg_t;
    save_eip                  : Bit32u;
    save_esp                  : Bit32u;
    //Inserire il codice per gestire le eccezzioni
    // For prefetch'ing instructions
    bytesleft                 : Bit32u;
    fetch_ptr                 : PBit8u;
    prev_linear_page          : Bit32u;
    prev_phy_page             : Bit32u;
    max_phy_addr              : Bit32u;

{$if BX_DEBUGGER = 1}
    break_point:Bit8u;
{$if MAGIC_BREAKPOINT = 1}
    magic_break:Bit8u;
{$ifend}
    stop_reason               : Bit8u;
    trace                     : Bit8u;
    trace_reg                 : Bit8u;
    mode_break                : Bit8u;	//BW
    debug_vm                  : Bool;		// BW contains current mode
    show_eip                  : Bit8u;	// BW record eip at special instr f.ex eip
    show_flag                 : Bit8u;	// BW shows instr class executed
{$ifend}
    // for paging
    TLB : record
      entry: array[0..BX_TLB_SIZE] of TTLB_entry;
    end;

    address_xlation : record
      paddress1               : Bit32u;  // physical address after translation of 1st len1 bytes of data
      paddress2               : Bit32u;  // physical address after translation of 2nd len2 bytes of data
      len1                    : Bit32u;  // number of bytes in page 1
      len2                    : Bit32u;  // number of bytes in page 2
      pages                   : Word;    // number of pages access spans (1 or 2)
    end;
    fake_end                  : byte;
    prog                      : LongWord;

    function GetEIP:Bit16u; // (* (Bit16u *) (((Bit8u *) &BX_CPU_THIS_PTR eip) + BX_REG16_OFFSET))
    procedure SetEIP(IPValue:Bit16u);
    // for lazy flags processing
    function  get_OF:Bool; // OK
    function  get_SF:Bool; // OK
    function  get_ZF:Bool; // OK
    function  get_AF:Bool; // OK
    function  get_PF:Bool; // OK
    function  get_CF:Bool; // OK

    function  BX_READ_16BIT_REG(index:Word): Bit16u;
    function  BX_READ_32BIT_REG(index:Word): Bit32u;

    function  CPL:Bit8u;
    constructor Create;
    destructor  Destroy; override;

    // prototypes for CPU instructions...
  private
    function  BX_READ_8BIT_REG(index:Word):Bit8u;
    procedure BX_WRITE_8BIT_REG(index:Word; val:Bit8u);
    procedure BX_WRITE_16BIT_REG(index:Word; val:Bit16u);
    procedure ADD_EbGb(I: PInstruction_tag);
    procedure ADD_EdGd(I: PInstruction_tag);
    procedure ADD_GbEb(I: PInstruction_tag);
    procedure ADD_GdEd(I: PInstruction_tag);
    procedure ADD_ALIb(I: PInstruction_tag);
    procedure ADD_EAXId(I: PInstruction_tag);
    procedure OR_EbGb(I: PInstruction_tag);
    procedure OR_EdGd(I: PInstruction_tag);
    procedure OR_EwGw(I: PInstruction_tag);
    procedure OR_GbEb(I: PInstruction_tag);
    procedure OR_GdEd(I: PInstruction_tag);
    procedure OR_GwEw(I: PInstruction_tag);
    procedure OR_ALIb(I: PInstruction_tag);
    procedure OR_EAXId(I: PInstruction_tag);
    procedure OR_AXIw(I: PInstruction_tag);

    procedure PUSH_CS(I: PInstruction_tag);
    procedure PUSH_DS(I: PInstruction_tag);
    procedure POP_DS(I: PInstruction_tag);
    procedure PUSH_ES(I: PInstruction_tag);
    procedure POP_ES(I: PInstruction_tag);
    procedure PUSH_FS(I: PInstruction_tag);
    procedure POP_FS(I: PInstruction_tag);
    procedure PUSH_GS(I: PInstruction_tag);
    procedure POP_GS(I: PInstruction_tag);
    procedure PUSH_SS(I: PInstruction_tag);
    procedure POP_SS(I: PInstruction_tag);

    procedure ADC_EbGb(I: PInstruction_tag);
    procedure ADC_EdGd(I: PInstruction_tag);
    procedure ADC_GbEb(I: PInstruction_tag);
    procedure ADC_GdEd(I: PInstruction_tag);
    procedure ADC_ALIb(I: PInstruction_tag);
    procedure ADC_EAXId(I: PInstruction_tag);
    procedure SBB_EbGb(I: PInstruction_tag);
    procedure SBB_EdGd(I: PInstruction_tag);
    procedure SBB_GbEb(I: PInstruction_tag);
    procedure SBB_GdEd(I: PInstruction_tag);
    procedure SBB_ALIb(I: PInstruction_tag);
    procedure SBB_EAXId(I: PInstruction_tag);

    procedure AND_EbGb(I: PInstruction_tag);
    procedure AND_EdGd(I: PInstruction_tag);
    procedure AND_EwGw(I: PInstruction_tag);
    procedure AND_GbEb(I: PInstruction_tag);
    procedure AND_GdEd(I: PInstruction_tag);
    procedure AND_GwEw(I: PInstruction_tag);
    procedure AND_ALIb(I: PInstruction_tag);
    procedure AND_EAXId(I: PInstruction_tag);
    procedure AND_AXIw(I: PInstruction_tag);
    procedure DAA(I: PInstruction_tag);
    procedure SUB_EbGb(I: PInstruction_tag);
    procedure SUB_EdGd(I: PInstruction_tag);
    procedure SUB_GbEb(I: PInstruction_tag);
    procedure SUB_GdEd(I: PInstruction_tag);
    procedure SUB_ALIb(I: PInstruction_tag);
    procedure SUB_EAXId(I: PInstruction_tag);
    procedure DAS(I: PInstruction_tag);

    procedure XOR_EbGb(I: PInstruction_tag);
    procedure XOR_EdGd(I: PInstruction_tag);
    procedure XOR_EwGw(I: PInstruction_tag);
    procedure XOR_GbEb(I: PInstruction_tag);
    procedure XOR_GdEd(I: PInstruction_tag);
    procedure XOR_GwEw(I: PInstruction_tag);
    procedure XOR_ALIb(I: PInstruction_tag);
    procedure XOR_EAXId(I: PInstruction_tag);
    procedure XOR_AXIw(I: PInstruction_tag);
    procedure AAA(I: PInstruction_tag);
    procedure CMP_EbGb(I: PInstruction_tag);
    procedure CMP_EdGd(I: PInstruction_tag);
    procedure CMP_GbEb(I: PInstruction_tag);
    procedure CMP_GdEd(I: PInstruction_tag);
    procedure CMP_ALIb(I: PInstruction_tag);
    procedure CMP_EAXId(I: PInstruction_tag);
    procedure AAS(I: PInstruction_tag);

    procedure PUSHAD32(I: PInstruction_tag);
    procedure PUSHAD16(I: PInstruction_tag);
    procedure POPAD32(I: PInstruction_tag);
    procedure POPAD16(I: PInstruction_tag);
    procedure BOUND_GvMa(I: PInstruction_tag);
    procedure ARPL_EwGw(I: PInstruction_tag);
    procedure PUSH_Id(I: PInstruction_tag);
    procedure PUSH_Iw(I: PInstruction_tag);
    procedure IMUL_GdEdId(I: PInstruction_tag);
    procedure INSB_YbDX(I: PInstruction_tag);
    procedure INSW_YvDX(I: PInstruction_tag);
    procedure OUTSB_DXXb(I: PInstruction_tag);
    procedure OUTSW_DXXv(I: PInstruction_tag);

    procedure TEST_EbGb(I: PInstruction_tag);
    procedure TEST_EdGd(I: PInstruction_tag);
    procedure TEST_EwGw(I: PInstruction_tag);
    procedure XCHG_EbGb(I: PInstruction_tag);
    procedure XCHG_EdGd(I: PInstruction_tag);
    procedure XCHG_EwGw(I: PInstruction_tag);
    procedure MOV_EbGb(I: PInstruction_tag);
    procedure MOV_EdGd(I: PInstruction_tag);
    procedure MOV_EwGw(I: PInstruction_tag);
    procedure MOV_GbEb(I: PInstruction_tag);
    procedure MOV_GdEd(I: PInstruction_tag);
    procedure MOV_GwEw(I: PInstruction_tag);
    procedure MOV_EwSw(I: PInstruction_tag);
    procedure LEA_GdM(I: PInstruction_tag);
    procedure LEA_GwM(I: PInstruction_tag);
    procedure MOV_SwEw(I: PInstruction_tag);
  (*  procedure POP_Ev(I: PBxInstruction_tag);*)

    procedure CBW(I: PInstruction_tag);
    procedure CWD(I: PInstruction_tag);
    procedure CALL32_Ap(I: PInstruction_tag);
    procedure CALL16_Ap(I: PInstruction_tag);
    procedure FWAIT(I: PInstruction_tag);
    procedure PUSHF_Fv(I: PInstruction_tag);
    procedure POPF_Fv(I: PInstruction_tag);
    procedure SAHF(I: PInstruction_tag);
    procedure LAHF(I: PInstruction_tag);

    procedure MOV_ALOb(I: PInstruction_tag);
    procedure MOV_EAXOd(I: PInstruction_tag);
    procedure MOV_AXOw(I: PInstruction_tag);
    procedure MOV_ObAL(I: PInstruction_tag);
    procedure MOV_OdEAX(I: PInstruction_tag);
    procedure MOV_OwAX(I: PInstruction_tag);
    procedure MOVSB_XbYb(I: PInstruction_tag);
    procedure MOVSW_XvYv(I: PInstruction_tag);
    procedure CMPSB_XbYb(I: PInstruction_tag);
    procedure CMPSW_XvYv(I: PInstruction_tag);
    procedure TEST_ALIb(I: PInstruction_tag);
    procedure TEST_EAXId(I: PInstruction_tag);
    procedure TEST_AXIw(I: PInstruction_tag);
    procedure STOSB_YbAL(I: PInstruction_tag);
    procedure STOSW_YveAX(I: PInstruction_tag);
    procedure LODSB_ALXb(I: PInstruction_tag);
    procedure LODSW_eAXXv(I: PInstruction_tag);
    procedure SCASB_ALXb(I: PInstruction_tag);
    procedure SCASW_eAXXv(I: PInstruction_tag);

    procedure RETnear32(I: PInstruction_tag);
    procedure RETnear16(I: PInstruction_tag);
    procedure LES_GvMp(I: PInstruction_tag);
    procedure LDS_GvMp(I: PInstruction_tag);
    procedure MOV_EbIb(I: PInstruction_tag);
    procedure MOV_EdId(I: PInstruction_tag);
    procedure MOV_EwIw(I: PInstruction_tag);
    procedure ENTER_IwIb(I: PInstruction_tag);
    procedure LEAVE(I: PInstruction_tag);
    procedure RETfar32(I: PInstruction_tag);
    procedure RETfar16(I: PInstruction_tag);

    procedure INT1(I: PInstruction_tag);
    procedure INT3(I: PInstruction_tag);
    procedure INT_Ib(I: PInstruction_tag);
    procedure INTO(I: PInstruction_tag);
    procedure IRET32(I: PInstruction_tag);
    procedure IRET16(I: PInstruction_tag);

    procedure AAM(I: PInstruction_tag);
    procedure AAD(I: PInstruction_tag);
    procedure SALC(I: PInstruction_tag);
    procedure XLAT(I: PInstruction_tag);

    procedure LOOPNE_Jb(I: PInstruction_tag);
    procedure LOOPE_Jb(I: PInstruction_tag);
    procedure LOOP_Jb(I: PInstruction_tag);
    procedure JCXZ_Jb(I: PInstruction_tag);
    procedure IN_ALIb(I: PInstruction_tag);
    procedure IN_eAXIb(I: PInstruction_tag);
    procedure OUT_IbAL(I: PInstruction_tag);
    procedure OUT_IbeAX(I: PInstruction_tag);
    procedure CALL_Aw(I: PInstruction_tag);
    procedure CALL_Ad(I: PInstruction_tag);
    procedure JMP_Jd(I: PInstruction_tag);
    procedure JMP_Jw(I: PInstruction_tag);
    procedure JMP_Ap(I: PInstruction_tag);
    procedure IN_ALDX(I: PInstruction_tag);
    procedure IN_eAXDX(I: PInstruction_tag);
    procedure OUT_DXAL(I: PInstruction_tag);
    procedure OUT_DXeAX(I: PInstruction_tag);

    procedure HLT(I: PInstruction_tag);
    procedure CMC(I: PInstruction_tag);
    procedure CLC(I: PInstruction_tag);
    procedure STC(I: PInstruction_tag);
    procedure CLI(I: PInstruction_tag);
    procedure STI(I: PInstruction_tag);
    procedure CLD(I: PInstruction_tag);
    procedure STD(I: PInstruction_tag);

    procedure LAR_GvEw(I: PInstruction_tag);
    procedure LSL_GvEw(I: PInstruction_tag);
    procedure CLTS(I: PInstruction_tag);
    procedure INVD(I: PInstruction_tag);
    procedure WBINVD(I: PInstruction_tag);

    procedure MOV_CdRd(I: PInstruction_tag);
    procedure MOV_DdRd(I: PInstruction_tag);
    procedure MOV_RdCd(I: PInstruction_tag);
    procedure MOV_RdDd(I: PInstruction_tag);
    procedure MOV_TdRd(I: PInstruction_tag);
    procedure MOV_RdTd(I: PInstruction_tag);

    procedure JCC_Jd(I: PInstruction_tag);
    procedure JCC_Jw(I: PInstruction_tag);

    procedure SETO_Eb(I: PInstruction_tag);
    procedure SETNO_Eb(I: PInstruction_tag);
    procedure SETB_Eb(I: PInstruction_tag);
    procedure SETNB_Eb(I: PInstruction_tag);
    procedure SETZ_Eb(I: PInstruction_tag);
    procedure SETNZ_Eb(I: PInstruction_tag);
    procedure SETBE_Eb(I: PInstruction_tag);
    procedure SETNBE_Eb(I: PInstruction_tag);
    procedure SETS_Eb(I: PInstruction_tag);
    procedure SETNS_Eb(I: PInstruction_tag);
    procedure SETP_Eb(I: PInstruction_tag);
    procedure SETNP_Eb(I: PInstruction_tag);
    procedure SETL_Eb(I: PInstruction_tag);
    procedure SETNL_Eb(I: PInstruction_tag);
    procedure SETLE_Eb(I: PInstruction_tag);
    procedure SETNLE_Eb(I: PInstruction_tag);

    procedure CPUID(I: PInstruction_tag);
    procedure BT_EvGv(I: PInstruction_tag);
    procedure SHLD_EdGd(I: PInstruction_tag);
    procedure SHLD_EwGw(I: PInstruction_tag);

    procedure BTS_EvGv(I: PInstruction_tag);

    procedure SHRD_EwGw(I: PInstruction_tag);
    procedure SHRD_EdGd(I: PInstruction_tag);

    procedure IMUL_GdEd(I: PInstruction_tag);

    procedure LSS_GvMp(I: PInstruction_tag);
    procedure BTR_EvGv(I: PInstruction_tag);
    procedure LFS_GvMp(I: PInstruction_tag);
    procedure LGS_GvMp(I: PInstruction_tag);
    procedure MOVZX_GdEb(I: PInstruction_tag);
    procedure MOVZX_GwEb(I: PInstruction_tag);
    procedure MOVZX_GdEw(I: PInstruction_tag);
    procedure MOVZX_GwEw(I: PInstruction_tag);
    procedure BTC_EvGv(I: PInstruction_tag);
    procedure BSF_GvEv(I: PInstruction_tag);
    procedure BSR_GvEv(I: PInstruction_tag);
    procedure MOVSX_GdEb(I: PInstruction_tag);
    procedure MOVSX_GwEb(I: PInstruction_tag);
    procedure MOVSX_GdEw(I: PInstruction_tag);
    procedure MOVSX_GwEw(I: PInstruction_tag);

    procedure BSWAP_EAX(I: PInstruction_tag);
    procedure BSWAP_ECX(I: PInstruction_tag);
    procedure BSWAP_EDX(I: PInstruction_tag);
    procedure BSWAP_EBX(I: PInstruction_tag);
    procedure BSWAP_ESP(I: PInstruction_tag);
    procedure BSWAP_EBP(I: PInstruction_tag);
    procedure BSWAP_ESI(I: PInstruction_tag);
    procedure BSWAP_EDI(I: PInstruction_tag);

    procedure ADD_EbIb(I: PInstruction_tag);
    procedure ADC_EbIb(I: PInstruction_tag);
    procedure SBB_EbIb(I: PInstruction_tag);
    procedure SUB_EbIb(I: PInstruction_tag);
    procedure CMP_EbIb(I: PInstruction_tag);

    procedure XOR_EbIb(I: PInstruction_tag);
    procedure OR_EbIb(I: PInstruction_tag);
    procedure AND_EbIb(I: PInstruction_tag);
    procedure ADD_EdId(I: PInstruction_tag);
    procedure OR_EdId(I: PInstruction_tag);
    procedure OR_EwIw(I: PInstruction_tag);
    procedure ADC_EdId(I: PInstruction_tag);
    procedure SBB_EdId(I: PInstruction_tag);
    procedure AND_EdId(I: PInstruction_tag);
    procedure AND_EwIw(I: PInstruction_tag);
    procedure SUB_EdId(I: PInstruction_tag);
    procedure XOR_EdId(I: PInstruction_tag);
    procedure XOR_EwIw(I: PInstruction_tag);
    procedure CMP_EdId(I: PInstruction_tag);

    procedure ROL_Eb(I: PInstruction_tag);
    procedure ROR_Eb(I: PInstruction_tag);
    procedure RCL_Eb(I: PInstruction_tag);
    procedure RCR_Eb(I: PInstruction_tag);
    procedure SHL_Eb(I: PInstruction_tag);
    procedure SHR_Eb(I: PInstruction_tag);
    procedure SAR_Eb(I: PInstruction_tag);

    procedure ROL_Ed(I: PInstruction_tag);
    procedure ROL_Ew(I: PInstruction_tag);
    procedure ROR_Ed(I: PInstruction_tag);
    procedure ROR_Ew(I: PInstruction_tag);
    procedure RCL_Ed(I: PInstruction_tag);
    procedure RCL_Ew(I: PInstruction_tag);
    procedure RCR_Ed(I: PInstruction_tag);
    procedure RCR_Ew(I: PInstruction_tag);
    procedure SHL_Ed(I: PInstruction_tag);
    procedure SHL_Ew(I: PInstruction_tag);
    procedure SHR_Ed(I: PInstruction_tag);
    procedure SHR_Ew(I: PInstruction_tag);
    procedure SAR_Ed(I: PInstruction_tag);
    procedure SAR_Ew(I: PInstruction_tag);

    procedure TEST_EbIb(I: PInstruction_tag);
    procedure NOT_Eb(I: PInstruction_tag);
    procedure NEG_Eb(I: PInstruction_tag);
    procedure MUL_ALEb(I: PInstruction_tag);
    procedure IMUL_ALEb(I: PInstruction_tag);
    procedure DIV_ALEb(I: PInstruction_tag);
    procedure IDIV_ALEb(I: PInstruction_tag);

    procedure TEST_EdId(I: PInstruction_tag);
    procedure TEST_EwIw(I: PInstruction_tag);
    procedure NOT_Ed(I: PInstruction_tag);
    procedure NOT_Ew(I: PInstruction_tag);
    procedure NEG_Ed(I: PInstruction_tag);
    procedure MUL_EAXEd(I: PInstruction_tag);
    procedure IMUL_EAXEd(I: PInstruction_tag);
    procedure DIV_EAXEd(I: PInstruction_tag);
    procedure IDIV_EAXEd(I: PInstruction_tag);

    procedure INC_Eb(I: PInstruction_tag);
    procedure DEC_Eb(I: PInstruction_tag);

    procedure INC_Ed(I: PInstruction_tag);
    procedure DEC_Ed(I: PInstruction_tag);
    procedure CALL_Ed(I: PInstruction_tag);
    procedure CALL_Ew(I: PInstruction_tag);
    procedure CALL32_Ep(I: PInstruction_tag);
    procedure CALL16_Ep(I: PInstruction_tag);
    procedure JMP_Ed(I: PInstruction_tag);
    procedure JMP_Ew(I: PInstruction_tag);
    procedure JMP32_Ep(I: PInstruction_tag);
    procedure JMP16_Ep(I: PInstruction_tag);
    procedure PUSH_Ed(I: PInstruction_tag);
    procedure PUSH_Ew(I: PInstruction_tag);

    procedure SLDT_Ew(I: PInstruction_tag);
    procedure STR_Ew(I: PInstruction_tag);
    procedure LLDT_Ew(I: PInstruction_tag);
    procedure LTR_Ew(I: PInstruction_tag);
    procedure VERR_Ew(I: PInstruction_tag);
    procedure VERW_Ew(I: PInstruction_tag);

    procedure SGDT_Ms(I: PInstruction_tag);
    procedure SIDT_Ms(I: PInstruction_tag);
    procedure LGDT_Ms(I: PInstruction_tag);
    procedure LIDT_Ms(I: PInstruction_tag);
    procedure SMSW_Ew(I: PInstruction_tag);
    procedure LMSW_Ew(I: PInstruction_tag);


    procedure BT_EvIb(I: PInstruction_tag);
    procedure BTS_EvIb(I: PInstruction_tag);
    procedure BTR_EvIb(I: PInstruction_tag);
    procedure BTC_EvIb(I: PInstruction_tag);

    procedure ESC0(I: PInstruction_tag);
    procedure ESC1(I: PInstruction_tag);
    procedure ESC2(I: PInstruction_tag);
    procedure ESC3(I: PInstruction_tag);
    procedure ESC4(I: PInstruction_tag);
    procedure ESC5(I: PInstruction_tag);
    procedure ESC6(I: PInstruction_tag);
    procedure ESC7(I: PInstruction_tag);

    procedure fpu_execute(I: PInstruction_tag);
    procedure fpu_init;
  (*  procedure fpu_print_regs;*)

    procedure CMPXCHG_XBTS(I: PInstruction_tag);
    procedure CMPXCHG_IBTS(I: PInstruction_tag);
    procedure CMPXCHG_EbGb(I: PInstruction_tag);
    procedure CMPXCHG_EdGd(I: PInstruction_tag);
    procedure CMPXCHG8B(I: PInstruction_tag);
    procedure XADD_EbGb(I: PInstruction_tag);
    procedure XADD_EdGd(I: PInstruction_tag);
    procedure RETnear32_Iw(I: PInstruction_tag);
    procedure RETnear16_Iw(I: PInstruction_tag);
    procedure RETfar32_Iw(I: PInstruction_tag);
    procedure RETfar16_Iw(I: PInstruction_tag);

    procedure LOADALL(I: PInstruction_tag);
    procedure CMOV_GdEd(I: PInstruction_tag);
    procedure CMOV_GwEw(I: PInstruction_tag);

    procedure ADD_EwGw(I: PInstruction_tag);
    procedure ADD_GwEw(I: PInstruction_tag);
    procedure ADD_AXIw(I: PInstruction_tag);
    procedure ADC_EwGw(I: PInstruction_tag);
    procedure ADC_GwEw(I: PInstruction_tag);
    procedure ADC_AXIw(I: PInstruction_tag);
    procedure SBB_EwGw(I: PInstruction_tag);
    procedure SBB_GwEw(I: PInstruction_tag);
    procedure SBB_AXIw(I: PInstruction_tag);
    procedure SBB_EwIw(I: PInstruction_tag);
    procedure SUB_EwGw(I: PInstruction_tag);
    procedure SUB_GwEw(I: PInstruction_tag);
    procedure SUB_AXIw(I: PInstruction_tag);
    procedure CMP_EwGw(I: PInstruction_tag);
    procedure CMP_GwEw(I: PInstruction_tag);
    procedure CMP_AXIw(I: PInstruction_tag);
    procedure CWDE(I: PInstruction_tag);
    procedure CDQ(I: PInstruction_tag);
    procedure XADD_EwGw(I: PInstruction_tag);
    procedure ADD_EwIw(I: PInstruction_tag);
    procedure ADC_EwIw(I: PInstruction_tag);
    procedure SUB_EwIw(I: PInstruction_tag);
    procedure CMP_EwIw(I: PInstruction_tag);
    procedure NEG_Ew(I: PInstruction_tag);
    procedure INC_Ew(I: PInstruction_tag);
    procedure DEC_Ew(I: PInstruction_tag);
    procedure CMPXCHG_EwGw(I: PInstruction_tag);
    procedure MUL_AXEw(I: PInstruction_tag);
    procedure IMUL_AXEw(I: PInstruction_tag);
    procedure DIV_AXEw(I: PInstruction_tag);
    procedure IDIV_AXEw(I: PInstruction_tag);
    procedure IMUL_GwEwIw(I: PInstruction_tag);
    procedure IMUL_GwEw(I: PInstruction_tag);
    procedure NOP(I: PInstruction_tag);
    procedure MOV_RLIb(I: PInstruction_tag);
    procedure MOV_RHIb(I: PInstruction_tag);
    procedure MOV_RXIw(I: PInstruction_tag);
    procedure MOV_ERXId(I: PInstruction_tag);
    procedure INC_RX(I: PInstruction_tag);
    procedure DEC_RX(I: PInstruction_tag);
    procedure INC_ERX(I: PInstruction_tag);
    procedure DEC_ERX(I: PInstruction_tag);
    procedure PUSH_RX(I: PInstruction_tag);
    procedure POP_RX(I: PInstruction_tag);
    procedure PUSH_ERX(I: PInstruction_tag);
    procedure POP_ERX(I: PInstruction_tag);
    procedure POP_Ew(I: PInstruction_tag);
    procedure POP_Ed(I: PInstruction_tag);
    procedure XCHG_RXAX(I: PInstruction_tag);
    procedure XCHG_ERXEAX(I: PInstruction_tag);

  private
    // mch added
    procedure INVLPG(Instruction:PInstruction_tag);
  //  procedure wait_for_interrupt();
    procedure RSM(I: PInstruction_tag);

    procedure WRMSR(I: PInstruction_tag);
    procedure RDTSC(I: PInstruction_tag);
    procedure RDMSR(I: PInstruction_tag);
    procedure SetCR0(val_32:Bit32u);
  (*  procedure dynamic_translate;
    procedure dynamic_init;*)
    function  FetchDecode(iptr:PBit8u; out instruction:TInstruction_tag; out remain:Word;const is_32:Bool):Word;
    procedure UndefinedOpcode(Instruction:PInstruction_tag);
    procedure BxError(I: PInstruction_tag);
    procedure BxResolveError(I: PInstruction_tag);

    procedure Resolve16Mod0Rm0(I: PInstruction_tag);
    procedure Resolve16Mod0Rm1(I: PInstruction_tag);
    procedure Resolve16Mod0Rm2(I: PInstruction_tag);
    procedure Resolve16Mod0Rm3(I: PInstruction_tag);
    procedure Resolve16Mod0Rm4(I: PInstruction_tag);
    procedure Resolve16Mod0Rm5(I: PInstruction_tag);
    procedure Resolve16Mod0Rm7(I: PInstruction_tag);

    procedure Resolve16Mod1or2Rm0(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm1(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm2(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm3(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm4(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm5(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm6(I: PInstruction_tag);
    procedure Resolve16Mod1or2Rm7(I: PInstruction_tag);

    procedure Resolve32Mod0Rm0(I: PInstruction_tag);
    procedure Resolve32Mod0Rm1(I: PInstruction_tag);
    procedure Resolve32Mod0Rm2(I: PInstruction_tag);
    procedure Resolve32Mod0Rm3(I: PInstruction_tag);
    procedure Resolve32Mod0Rm6(I: PInstruction_tag);
    procedure Resolve32Mod0Rm7(I: PInstruction_tag);

    procedure Resolve32Mod1or2Rm0(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm1(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm2(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm3(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm5(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm6(I: PInstruction_tag);
    procedure Resolve32Mod1or2Rm7(I: PInstruction_tag);

    procedure Resolve32Mod0Base0(I: PInstruction_tag);
    procedure Resolve32Mod0Base1(I: PInstruction_tag);
    procedure Resolve32Mod0Base2(I: PInstruction_tag);
    procedure Resolve32Mod0Base3(I: PInstruction_tag);
    procedure Resolve32Mod0Base4(I: PInstruction_tag);
    procedure Resolve32Mod0Base5(I: PInstruction_tag);
    procedure Resolve32Mod0Base6(I: PInstruction_tag);
    procedure Resolve32Mod0Base7(I: PInstruction_tag);

    procedure Resolve32Mod1or2Base0(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base1(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base2(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base3(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base4(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base5(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base6(I: PInstruction_tag);
    procedure Resolve32Mod1or2Base7(I: PInstruction_tag);

  (*  procedure REP(P:Pointer);
    procedure REP_ZF(P:Pointer; rep_prefix:Word);

    procedure atexit;

    // now for some ancillary functions...
    //procedure decode_exgx16(need_fetch:Word);
    //procedure decode_exgx32(mod_rm:Word);*)

    procedure access_linear(const laddress:Bit32u; const length:unsigned; const pl:unsigned; rw:unsigned; data:Pointer);
    function  itranslate_linear(laddress:Bit32u; pl:unsigned):Bit32u;
    function  dtranslate_linear(laddress:Bit32u; pl:unsigned; rw:unsigned):Bit32u;
    procedure TLB_flush;
    procedure TLB_clear;
    procedure TLB_init;
    procedure set_INTR(value:Bool);
    function  strseg(seg:Psegment_reg_t):PChar;
    procedure interrupt(vector:Bit8u; is_INT:Bool; is_error_code:Bool;error_code:Bit16u);

    function  int_number(seg:Psegment_reg_t):Integer;
    procedure shutdown_cpu;
    procedure enable_paging;
    procedure disable_paging;
    procedure CR3_change(value32:Bit32u);

    procedure jump_protected(Istr:PInstruction_tag; cs_raw:Bit16u; disp32:Bit32u);
    procedure call_protected(I:PInstruction_tag; cs_raw:Bit16u; disp32:Bit32u);
    procedure return_protected(I:PInstruction_tag; pop_bytes:Bit16u);
    procedure iret_protected(I: PInstruction_tag);
    procedure validate_seg_regs;
    procedure stack_return_to_v86(new_eip:Bit32u; raw_cs_selector:Bit32u;flags32:Bit32u);
    procedure stack_return_from_v86(I: PInstruction_tag);
    procedure init_v8086_mode;
    (*procedure v8086_message;*)
    procedure task_switch(tss_selector:Pselector_t; tss_descriptor:Pdescriptor_t;source:unsigned;
                    dword1:Bit32u; dword2:Bit32u);
    procedure get_SS_ESP_from_TSS(pl:unsigned; ss:PBit16u; esp:PBit32u);
    procedure write_flags(flags:Bit16u; change_IOPL:Bool; change_IF:Bool);
    procedure write_eflags(eflags_raw:Bit32u; change_IOPL:Bool ; change_IF:Bool;change_VM:Bool; change_RF:Bool);
    function  read_flags:Bit16u;
    function  read_eflags:Bit32u;

    function  inp8(addr:Bit16u):Bit8u;
    procedure outp8(addr:Bit16u; value:Bit8u);
    function  inp16(addr:Bit16u):Bit16u;
    procedure outp16(addr:Bit16u; value:Bit16u);
    function  inp32(addr:Bit16u):Bit32u;
    procedure outp32(addr:Bit16u; value:Bit32u);
    function  allow_io(addr:Bit16u; len:unsigned):Bool;
    procedure enter_protected_mode;
    procedure enter_real_mode;
    procedure parse_selector(raw_selector:Bit16u; selector:Pselector_t);
    procedure parse_descriptor(dword1:Bit32u;dword2:Bit32u;temp:Pdescriptor_t);
    procedure load_ldtr(selector:Pselector_t; descriptor:Pdescriptor_t);
    procedure load_cs(selector:Pselector_t; descriptor:Pdescriptor_t; cpl:Bit8u);
    procedure load_ss(selector:Pselector_t; descriptor:Pdescriptor_t; cpl:Bit8u);
    procedure fetch_raw_descriptor(selector:Pselector_t;
                               dword1:pBit32u; dword2:pBit32u; exception_no:Bit8u);
    procedure load_seg_reg(seg:Psegment_reg_t; new_value:Bit16u);
    function  fetch_raw_descriptor2(selector:Pselector_t; dword1:pBit32u; dword2:pBit32u):Bool;
    procedure push_16(value16:Bit16u);
    procedure push_32(value32:Bit32u);
    procedure pop_16(value16_ptr:pBit16u);
    procedure pop_32(value32_ptr:pBit32u);
    function  can_push(descriptor:Pdescriptor_t; esp:Bit32u; bytes:Bit32u ):Bool;
    function  can_pop(bytes:Bit32u):Bool ;
    procedure sanity_checks;

  (*  procedure    debug(offset:Bit32u);*)

    procedure set_CF(val:Bool);
    procedure set_AF(val:Bool);
    procedure set_SF(val:Bool);
    procedure set_OF(val:Bool);
    procedure set_PF(val:Bool);
    procedure set_PF_base(val:Bit8u);
    procedure set_ZF(val:Bool);

  public
    function  real_mode:Bool;
    function  v8086_mode:Bool;
    procedure exception(vector:unsigned;error_code:Bit16u;is_INT:Bool);

    procedure revalidate_prefetch_q;
    procedure invalidate_prefetch_q;

    procedure write_virtual_checks(seg:Psegment_reg_t; offset:Bit32u; length:Bit32u);

    procedure read_virtual_checks(seg:Psegment_reg_t;  offset:Bit32u; length:Word);

    procedure write_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
    procedure write_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
    procedure write_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);
    procedure read_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
    procedure read_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
    procedure read_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);

    procedure read_RMW_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
    procedure read_RMW_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
    procedure read_RMW_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);
    procedure write_RMW_virtual_byte(val8:Bit8u);
    procedure write_RMW_virtual_word(val16:Bit16u);
    procedure write_RMW_virtual_dword(val32:Bit32u);

  public
    procedure init(addrspace:TMEM_C);
    procedure reset(ResetInfo: PCPUResetInfo);
    procedure cpu_loop;
    procedure prefetch;

  private
    procedure SET_FLAGS_OSZAPC_8(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAPC_8_CF(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word; last_CF:Bool);
    procedure SET_FLAGS_OSZAP_8(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAPC_16(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAP_16(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAPC_16_CF(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word; last_CF:Bool);
    procedure SET_FLAGS_OSZAP_32(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAPC_32(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word);
    procedure SET_FLAGS_OSZAPC_32_CF(op1:Bit32u; op2:Bit32u; result:Bit32u; ins:Word; last_CF:Bool);
    procedure SET_FLAGS_OxxxxC(new_of, new_cf:Bool);

    procedure BX_WRITE_32BIT_REG(index: Word; val: bit32u);

    procedure SetZF(const aValue: Bool);
    function GetZF: bool;
    procedure SetCF(const aValue: Bool);
    function GetCF: bool;
    procedure SetPF(const aValue: Bool);
    function GetPF: bool;
    procedure SetAF(const aValue: Bool);
    function GetAF: bool;
    procedure SetSF(const aValue: Bool);
    function GetSF: bool;
    procedure SetTF(const aValue: Bool);
    function GetTF: bool;
    procedure SetDF(const aValue: Bool);
    function GetDF: bool;
    procedure SetOF(const aValue: Bool);
    function GetOF: bool;
//    procedure Prova(Valore: PInteger);
  private
    FOnCPUCommand: TCPUEvent;
    FActive: boolean;

    procedure EvCPUCommand(aInstrPtr: pointer);
    procedure SetActive(const aValue: boolean);
  public
    property Active: boolean read FActive write SetActive default false;

    // flags
    property ZF: Bool read GetZF write SetZF;
    property CF: Bool read GetCF write SetCF;
    property PF: Bool read GetPF write SetPF;
    property AF: Bool read GetAF write SetAF;
    property SF: Bool read GetSF write SetSF;
    property TF: Bool read GetTF write SetTF;
    property DF: Bool read GetDF write SetDF;
    property OF_: Bool read GetOF write SetOF;

    // 8bits registers
    property AL: bit8u read gen_reg[REG_AL_8BIT].rl write gen_reg[REG_AL_8BIT].rl;
    property CL: bit8u read gen_reg[REG_CL_8BIT].rl write gen_reg[REG_CL_8BIT].rl;
    property DL: bit8u read gen_reg[REG_DL_8BIT].rl write gen_reg[REG_DL_8BIT].rl;
    property BL: bit8u read gen_reg[REG_BL_8BIT].rl write gen_reg[REG_BL_8BIT].rl;
    property AH: bit8u read gen_reg[REG_AH_8BIT].rh write gen_reg[REG_AH_8BIT].rh;
    property CH: bit8u read gen_reg[REG_CH_8BIT].rh write gen_reg[REG_CH_8BIT].rh;
    property DH: bit8u read gen_reg[REG_DH_8BIT].rh write gen_reg[REG_DH_8BIT].rh;
    property BH: bit8u read gen_reg[REG_BH_8BIT].rh write gen_reg[REG_BH_8BIT].rh;

    // 16bits registers
    property AX: bit16u read gen_reg[REG_AX_16BIT].rx write gen_reg[REG_AX_16BIT].rx;
    property CX: bit16u read gen_reg[REG_CX_16BIT].rx write gen_reg[REG_CX_16BIT].rx;
    property DX: bit16u read gen_reg[REG_DX_16BIT].rx write gen_reg[REG_DX_16BIT].rx;
    property BX: bit16u read gen_reg[REG_BX_16BIT].rx write gen_reg[REG_BX_16BIT].rx;
    property SP: bit16u read gen_reg[REG_SP_16BIT].rx write gen_reg[REG_SP_16BIT].rx;
    property BP: bit16u read gen_reg[REG_BP_16BIT].rx write gen_reg[REG_BP_16BIT].rx;
    property SI: bit16u read gen_reg[REG_SI_16BIT].rx write gen_reg[REG_SI_16BIT].rx;
    property DI: bit16u read gen_reg[REG_DI_16BIT].rx write gen_reg[REG_DI_16BIT].rx;

    // 32bits registers
    property EAX: bit32u read gen_reg[REG_EAX_32BIT].erx write gen_reg[REG_EAX_32BIT].erx;
    property ECX: bit32u read gen_reg[REG_ECX_32BIT].erx write gen_reg[REG_ECX_32BIT].erx;
    property EDX: bit32u read gen_reg[REG_EDX_32BIT].erx write gen_reg[REG_EDX_32BIT].erx;
    property EBX: bit32u read gen_reg[REG_EBX_32BIT].erx write gen_reg[REG_EBX_32BIT].erx;
    property ESP: bit32u read gen_reg[REG_ESP_32BIT].erx write gen_reg[REG_ESP_32BIT].erx;
    property EBP: bit32u read gen_reg[REG_EBP_32BIT].erx write gen_reg[REG_EBP_32BIT].erx;
    property ESI: bit32u read gen_reg[REG_ESI_32BIT].erx write gen_reg[REG_ESI_32BIT].erx;
    property EDI: bit32u read gen_reg[REG_EDI_32BIT].erx write gen_reg[REG_EDI_32BIT].erx;

    // segment registers
    property ES: Bit32u read sregs[SEG_REG_ES].cache.segment.base write sregs[SEG_REG_ES].cache.segment.base;
    property CS: Bit32u read sregs[SEG_REG_CS].cache.segment.base write sregs[SEG_REG_CS].cache.segment.base;
    property SS: Bit32u read sregs[SEG_REG_SS].cache.segment.base write sregs[SEG_REG_SS].cache.segment.base;
    property DS: Bit32u read sregs[SEG_REG_DS].cache.segment.base write sregs[SEG_REG_DS].cache.segment.base;
    property FS: Bit32u read sregs[SEG_REG_FS].cache.segment.base write sregs[SEG_REG_FS].cache.segment.base;
    property GS: Bit32u read sregs[SEG_REG_GS].cache.segment.base write sregs[SEG_REG_GS].cache.segment.base;

    property DR0: Bit32u read FDR0  write FDR0;
    property DR1: Bit32u read FDR1  write FDR1;
    property DR2: Bit32u read FDR2  write FDR2;
    property DR3: Bit32u read FDR3  write FDR3;
    property DR6: Bit32u read FDR6  write FDR6;
    property DR7: Bit32u read FDR7  write FDR7;
    property CR0: Bit32u read FCR0.val32  write FCR0.val32;
    property CR1: Bit32u read FCR1  write FCR1;
    property CR2: Bit32u read FCR2  write FCR2;
    property CR3: Bit32u read FCR3  write FCR3;
    property CR4: Bit32u read FCR4  write FCR4;

    property EIP :Bit16u read GetEIP         write SetEIP;
    property IOPL:Bit8u  read eflags.iopl    write eflags.iopl;

    function Is32: bool;
    function DisAssemble(aPhysicalAddress, aEIP, aCS: Int64; 
      out aDisasmStr, aOpcodeStr: string; out aOpcodeLength: integer): bool;

    function StackHeadAddr: int64;
    function InstrPointer: int64;
  public
    property OnCPUCommand: TCPUEvent read FOnCPUCommand write FOnCPUCommand;
  end; // BX_CPU_C

var
  ips_count: Int64;
  m_ips: double;
//  stoprun:boolean = True;

  bx_cpu                : TCPU;

  BxResolve16mod0       : array[0..7   ] of TInstruction_t;
  BxResolve16mod1or2    : array[0..7   ] of TInstruction_t;
  BxResolve32mod0       : array[0..7   ] of TInstruction_t;
  BxResolve32mod1or2    : array[0..7   ] of TInstruction_t;
  BxResolve32mod0Base   : array[0..7   ] of TInstruction_t;
  BxResolve32mod1or2Base: array[0..7   ] of TInstruction_t;
  BxOpcodeInfoG1EbIb    : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG1Ew      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG1Ed      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG2Eb      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG2Ew      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG2Ed      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG3Eb      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG3Ew      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG3Ed      : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG4        : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG5w       : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG5d       : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG6        : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG7        : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG8EvIb    : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfoG9        : array[0..7   ] of TOpcodeInfo_t;
  BxOpcodeInfo          : array[0..1028] of TOpcodeInfo_t;

  bx_pc_system          : TPC_System;

  procedure InitNames;
  procedure InitSystem;

var
  savejump:jmp_buf;

implementation

uses Iodev, dma, pci, pic, cmos, pit_wrap, Hdd, gui32, keyboard, fpu;

procedure InitNames;
begin
  BxOpcodeInfoG1EbIb[0].Name := 'ADD_EbIb';
  BxOpcodeInfoG1EbIb[1].Name := 'OR_EbIb';
  BxOpcodeInfoG1EbIb[2].Name := 'ADC_EbIb';
  BxOpcodeInfoG1EbIb[3].Name := 'SBB_EbIb';
  BxOpcodeInfoG1EbIb[4].Name := 'AND_EbIb';
  BxOpcodeInfoG1EbIb[5].Name := 'SUB_EbIb';
  BxOpcodeInfoG1EbIb[6].Name := 'XOR_EbIb';
  BxOpcodeInfoG1EbIb[7].Name := 'CMP_EbIb';

  BxOpcodeInfoG1Ew[0].Name := 'ADD_EwIw';
  BxOpcodeInfoG1Ew[1].Name := 'OR_EwIw';
  BxOpcodeInfoG1Ew[2].Name := 'ADC_EwIw';
  BxOpcodeInfoG1Ew[3].Name := 'SBB_EwIw';
  BxOpcodeInfoG1Ew[4].Name := 'AND_EwIw';
  BxOpcodeInfoG1Ew[5].Name := 'SUB_EwIw';
  BxOpcodeInfoG1Ew[6].Name := 'XOR_EwIw';
  BxOpcodeInfoG1Ew[7].Name := 'CMP_EwIw';

  BxOpcodeInfoG1Ed[0].Name := 'ADD_EdId';
  BxOpcodeInfoG1Ed[1].Name := 'OR_EdId';
  BxOpcodeInfoG1Ed[2].Name := 'ADC_EdId';
  BxOpcodeInfoG1Ed[3].Name := 'SBB_EdId';
  BxOpcodeInfoG1Ed[4].Name := 'AND_EdId';
  BxOpcodeInfoG1Ed[5].Name := 'SUB_EdId';
  BxOpcodeInfoG1Ed[6].Name := 'XOR_EdId';
  BxOpcodeInfoG1Ed[7].Name := 'CMP_EdId';

  // attributes defined in main area
  BxOpcodeInfoG2Eb[0].Name := 'ROL_Eb';
  BxOpcodeInfoG2Eb[1].Name := 'ROR_Eb';
  BxOpcodeInfoG2Eb[2].Name := 'RCL_Eb';
  BxOpcodeInfoG2Eb[3].Name := 'RCR_Eb';
  BxOpcodeInfoG2Eb[4].Name := 'SHL_Eb';
  BxOpcodeInfoG2Eb[5].Name := 'SHR_Eb';
  BxOpcodeInfoG2Eb[6].Name := 'SHL_Eb';
  BxOpcodeInfoG2Eb[7].Name := 'SAR_Eb';

  // attributes defined in main area
  BxOpcodeInfoG2Ew[0].Name := 'ROL_Ew';
  BxOpcodeInfoG2Ew[1].Name := 'ROR_Ew';
  BxOpcodeInfoG2Ew[2].Name := 'RCL_Ew';
  BxOpcodeInfoG2Ew[3].Name := 'RCR_Ew';
  BxOpcodeInfoG2Ew[4].Name := 'SHL_Ew';
  BxOpcodeInfoG2Ew[5].Name := 'SHR_Ew';
  BxOpcodeInfoG2Ew[6].Name := 'SHL_Ew';
  BxOpcodeInfoG2Ew[7].Name := 'SAR_Ew';

  // attributes defined in main area
  BxOpcodeInfoG2Ed[0].Name := 'ROL_Ed';
  BxOpcodeInfoG2Ed[1].Name := 'ROR_Ed';
  BxOpcodeInfoG2Ed[2].Name := 'RCL_Ed';
  BxOpcodeInfoG2Ed[3].Name := 'RCR_Ed';
  BxOpcodeInfoG2Ed[4].Name := 'SHL_Ed';
  BxOpcodeInfoG2Ed[5].Name := 'SHR_Ed';
  BxOpcodeInfoG2Ed[6].Name := 'SHL_Ed';
  BxOpcodeInfoG2Ed[7].Name := 'SAR_Ed';

  BxOpcodeInfoG3Eb[0].Name := 'TEST_EbIb';
  BxOpcodeInfoG3Eb[1].Name := 'TEST_EbIb';
  BxOpcodeInfoG3Eb[2].Name := 'NOT_Eb';
  BxOpcodeInfoG3Eb[3].Name := 'NEG_Eb';
  BxOpcodeInfoG3Eb[4].Name := 'MUL_ALEb';
  BxOpcodeInfoG3Eb[5].Name := 'IMUL_ALEb';
  BxOpcodeInfoG3Eb[6].Name := 'DIV_ALEb';
  BxOpcodeInfoG3Eb[7].Name := 'IDIV_ALEb';

  BxOpcodeInfoG3Ew[0].Name := 'TEST_EwIw';
  BxOpcodeInfoG3Ew[1].Name := 'TEST_EwIw';
  BxOpcodeInfoG3Ew[2].Name := 'NOT_Ew';
  BxOpcodeInfoG3Ew[3].Name := 'NEG_Ew';
  BxOpcodeInfoG3Ew[4].Name := 'MUL_AXEw';
  BxOpcodeInfoG3Ew[5].Name := 'IMUL_AXEw';
  BxOpcodeInfoG3Ew[6].Name := 'DIV_AXEw';
  BxOpcodeInfoG3Ew[7].Name := 'IDIV_AXEw';

  BxOpcodeInfoG3Ed[0].Name := 'TEST_EdId';
  BxOpcodeInfoG3Ed[1].Name := 'TEST_EdId';
  BxOpcodeInfoG3Ed[2].Name := 'NOT_Ed';
  BxOpcodeInfoG3Ed[3].Name := 'NEG_Ed';
  BxOpcodeInfoG3Ed[4].Name := 'MUL_EAXEd';
  BxOpcodeInfoG3Ed[5].Name := 'IMUL_EAXEd';
  BxOpcodeInfoG3Ed[6].Name := 'DIV_EAXEd';
  BxOpcodeInfoG3Ed[7].Name := 'IDIV_EAXEd';

  BxOpcodeInfoG4[0].Name := 'INC_Eb';
  BxOpcodeInfoG4[1].Name := 'DEC_Eb';
  BxOpcodeInfoG4[2].Name := 'BxError';
  BxOpcodeInfoG4[3].Name := 'BxError';
  BxOpcodeInfoG4[4].Name := 'BxError';
  BxOpcodeInfoG4[5].Name := 'BxError';
  BxOpcodeInfoG4[6].Name := 'BxError';
  BxOpcodeInfoG4[7].Name := 'BxError';

  // attributes defined in main area
  BxOpcodeInfoG5w[0].Name := 'INC_Ew';
  BxOpcodeInfoG5w[1].Name := 'DEC_Ew';
  BxOpcodeInfoG5w[2].Name := 'CALL_Ew';
  BxOpcodeInfoG5w[3].Name := 'CALL16_Ep';
  BxOpcodeInfoG5w[4].Name := 'JMP_Ew';
  BxOpcodeInfoG5w[5].Name := 'JMP16_Ep';
  BxOpcodeInfoG5w[6].Name := 'PUSH_Ew';
  BxOpcodeInfoG5w[7].Name := 'BxError';

  // attributes defined in main area
  BxOpcodeInfoG5d[0].Name := 'INC_Ed';
  BxOpcodeInfoG5d[1].Name := 'DEC_Ed';
  BxOpcodeInfoG5d[2].Name := 'CALL_Ed';
  BxOpcodeInfoG5d[3].Name := 'CALL32_Ep';
  BxOpcodeInfoG5d[4].Name := 'JMP_Ed';
  BxOpcodeInfoG5d[5].Name := 'JMP32_Ep';
  BxOpcodeInfoG5d[6].Name := 'PUSH_Ed';
  BxOpcodeInfoG5d[7].Name := 'BxError';

  // attributes defined in main area
  BxOpcodeInfoG6[00].Name := 'SLDT_Ew';
  BxOpcodeInfoG6[01].Name := 'STR_Ew';
  BxOpcodeInfoG6[02].Name := 'LLDT_Ew';
  BxOpcodeInfoG6[03].Name := 'LTR_Ew';
  BxOpcodeInfoG6[04].Name := 'VERR_Ew';
  BxOpcodeInfoG6[05].Name := 'VERW_Ew';
  BxOpcodeInfoG6[06].Name := 'BxError';
  BxOpcodeInfoG6[07].Name := 'BxError';

  BxOpcodeInfoG7[00].Name := 'SGDT_Ms';
  BxOpcodeInfoG7[01].Name := 'SIDT_Ms';
  BxOpcodeInfoG7[02].Name := 'LGDT_Ms';
  BxOpcodeInfoG7[03].Name := 'LIDT_Ms';
  BxOpcodeInfoG7[04].Name := 'SMSW_Ew';
  BxOpcodeInfoG7[05].Name := 'BxError';
  BxOpcodeInfoG7[06].Name := 'LMSW_Ew';
  BxOpcodeInfoG7[07].Name := 'INVLPG';

  BxOpcodeInfoG8EvIb[00].Name := 'BxError';
  BxOpcodeInfoG8EvIb[01].Name := 'BxError';
  BxOpcodeInfoG8EvIb[02].Name := 'BxError';
  BxOpcodeInfoG8EvIb[03].Name := 'BxError';
  BxOpcodeInfoG8EvIb[04].Name := 'BT_EvIb';
  BxOpcodeInfoG8EvIb[05].Name := 'BTS_EvIb';
  BxOpcodeInfoG8EvIb[06].Name := 'BTR_EvIb';
  BxOpcodeInfoG8EvIb[07].Name := 'BTC_EvIb';

  BxOpcodeInfoG9[00].Name := 'BxError';
  BxOpcodeInfoG9[01].Name := 'CMPXCHG8B';
  BxOpcodeInfoG9[02].Name := 'BxError';
  BxOpcodeInfoG9[03].Name := 'BxError';
  BxOpcodeInfoG9[04].Name := 'BxError';
  BxOpcodeInfoG9[05].Name := 'BxError';
  BxOpcodeInfoG9[06].Name := 'BxError';
  BxOpcodeInfoG9[07].Name := 'BxError';

  BxOpcodeInfo[$0].Name  := 'ADD_EbGb';
  BxOpcodeInfo[$1].Name  := 'ADD_EwGw';
  BxOpcodeInfo[$2].Name  := 'ADD_GbEb';
  BxOpcodeInfo[$3].Name  := 'ADD_GwEw';
  BxOpcodeInfo[$4].Name  := 'ADD_ALIb';
  BxOpcodeInfo[$5].Name  := 'ADD_AXIw';
  BxOpcodeInfo[$6].Name  := 'PUSH_ES';
  BxOpcodeInfo[$7].Name  := 'POP_ES';
  BxOpcodeInfo[$8].Name  := 'OR_EbGb';
  BxOpcodeInfo[$9].Name  := 'OR_EwGw';
  BxOpcodeInfo[$A].Name  := 'OR_GbEb';
  BxOpcodeInfo[$B].Name  := 'OR_GwEw';
  BxOpcodeInfo[$C].Name  := 'OR_ALIb';
  BxOpcodeInfo[$D].Name  := 'OR_AXIw';
  BxOpcodeInfo[$E].Name  := 'PUSH_CS';
  BxOpcodeInfo[$F].Name  := 'BxError'; // 2-byte escape
  BxOpcodeInfo[$10].Name := 'ADC_EbGb';
  BxOpcodeInfo[$11].Name := 'ADC_EwGw';
  BxOpcodeInfo[$12].Name := 'ADC_GbEb';
  BxOpcodeInfo[$13].Name := 'ADC_GwEw';
  BxOpcodeInfo[$14].Name := 'ADC_ALIb';
  BxOpcodeInfo[$15].Name := 'ADC_AXIw';
  BxOpcodeInfo[$16].Name := 'PUSH_SS';
  BxOpcodeInfo[$17].Name := 'POP_SS';
  BxOpcodeInfo[$18].Name := 'SBB_EbGb';
  BxOpcodeInfo[$19].Name := 'SBB_EwGw';
  BxOpcodeInfo[$1A].Name := 'SBB_GbEb';
  BxOpcodeInfo[$1B].Name := 'SBB_GwEw';
  BxOpcodeInfo[$1C].Name := 'SBB_ALIb';
  BxOpcodeInfo[$1D].Name := 'SBB_AXIw';
  BxOpcodeInfo[$1E].Name := 'PUSH_DS';
  BxOpcodeInfo[$1F].Name := 'POP_DS';
  BxOpcodeInfo[$20].Name := 'AND_EbGb';
  BxOpcodeInfo[$21].Name := 'AND_EwGw';
  BxOpcodeInfo[$22].Name := 'AND_GbEb';
  BxOpcodeInfo[$23].Name := 'AND_GwEw';
  BxOpcodeInfo[$24].Name := 'AND_ALIb';
  BxOpcodeInfo[$25].Name := 'AND_AXIw';
  BxOpcodeInfo[$26].Name := 'BxError'; // ES:
  BxOpcodeInfo[$27].Name := 'DAA';
  BxOpcodeInfo[$28].Name := 'SUB_EbGb';
  BxOpcodeInfo[$29].Name := 'SUB_EwGw';
  BxOpcodeInfo[$2A].Name := 'SUB_GbEb';
  BxOpcodeInfo[$2B].Name := 'SUB_GwEw';
  BxOpcodeInfo[$2C].Name := 'SUB_ALIb';
  BxOpcodeInfo[$2D].Name := 'SUB_AXIw';
  BxOpcodeInfo[$2E].Name := 'BxError'; // CS:
  BxOpcodeInfo[$2F].Name := 'DAS';
  BxOpcodeInfo[$30].Name := 'XOR_EbGb';
  BxOpcodeInfo[$31].Name := 'XOR_EwGw';
  BxOpcodeInfo[$32].Name := 'XOR_GbEb';
  BxOpcodeInfo[$33].Name := 'XOR_GwEw';
  BxOpcodeInfo[$34].Name := 'XOR_ALIb';
  BxOpcodeInfo[$35].Name := 'XOR_AXIw';
  BxOpcodeInfo[$36].Name := 'BxError'; // SS:
  BxOpcodeInfo[$37].Name := 'AAA';
  BxOpcodeInfo[$38].Name := 'CMP_EbGb';
  BxOpcodeInfo[$39].Name := 'CMP_EwGw';
  BxOpcodeInfo[$3A].Name := 'CMP_GbEb';
  BxOpcodeInfo[$3B].Name := 'CMP_GwEw';
  BxOpcodeInfo[$3C].Name := 'CMP_ALIb';
  BxOpcodeInfo[$3D].Name := 'CMP_AXIw';
  BxOpcodeInfo[$3E].Name := 'BxError'; // DS:
  BxOpcodeInfo[$3F].Name := 'AAS';
  BxOpcodeInfo[$40].Name := 'INC_RX';
  BxOpcodeInfo[$41].Name := 'INC_RX';
  BxOpcodeInfo[$42].Name := 'INC_RX';
  BxOpcodeInfo[$43].Name := 'INC_RX';
  BxOpcodeInfo[$44].Name := 'INC_RX';
  BxOpcodeInfo[$45].Name := 'INC_RX';
  BxOpcodeInfo[$46].Name := 'INC_RX';
  BxOpcodeInfo[$47].Name := 'INC_RX';
  BxOpcodeInfo[$48].Name := 'DEC_RX';
  BxOpcodeInfo[$49].Name := 'DEC_RX';
  BxOpcodeInfo[$4A].Name := 'DEC_RX';
  BxOpcodeInfo[$4B].Name := 'DEC_RX';
  BxOpcodeInfo[$4C].Name := 'DEC_RX';
  BxOpcodeInfo[$4D].Name := 'DEC_RX';
  BxOpcodeInfo[$4E].Name := 'DEC_RX';
  BxOpcodeInfo[$4F].Name := 'DEC_RX';
  BxOpcodeInfo[$50].Name := 'PUSH_RX';
  BxOpcodeInfo[$51].Name := 'PUSH_RX';
  BxOpcodeInfo[$52].Name := 'PUSH_RX';
  BxOpcodeInfo[$53].Name := 'PUSH_RX';
  BxOpcodeInfo[$54].Name := 'PUSH_RX';
  BxOpcodeInfo[$55].Name := 'PUSH_RX';
  BxOpcodeInfo[$56].Name := 'PUSH_RX';
  BxOpcodeInfo[$57].Name := 'PUSH_RX';
  BxOpcodeInfo[$58].Name := 'POP_RX';
  BxOpcodeInfo[$59].Name := 'POP_RX';
  BxOpcodeInfo[$5A].Name := 'POP_RX';
  BxOpcodeInfo[$5B].Name := 'POP_RX';
  BxOpcodeInfo[$5C].Name := 'POP_RX';
  BxOpcodeInfo[$5D].Name := 'POP_RX';
  BxOpcodeInfo[$5E].Name := 'POP_RX';
  BxOpcodeInfo[$5F].Name := 'POP_RX';
  BxOpcodeInfo[$60].Name := 'PUSHAD16';
  BxOpcodeInfo[$61].Name := 'POPAD16';
  BxOpcodeInfo[$62].Name := 'BOUND_GvMa';
  BxOpcodeInfo[$63].Name := 'ARPL_EwGw';
  BxOpcodeInfo[$64].Name := 'BxError'; // FS:
  BxOpcodeInfo[$65].Name := 'BxError'; // GS:
  BxOpcodeInfo[$66].Name := 'BxError'; // OS:
  BxOpcodeInfo[$67].Name := 'BxError'; // AS:
  BxOpcodeInfo[$68].Name := 'PUSH_Iw';
  BxOpcodeInfo[$69].Name := 'IMUL_GwEwIw';
  BxOpcodeInfo[$6A].Name := 'PUSH_Iw';
  BxOpcodeInfo[$6B].Name := 'IMUL_GwEwIw';
  BxOpcodeInfo[$6C].Name := 'INSB_YbDX';
  BxOpcodeInfo[$6D].Name := 'INSW_YvDX';
  BxOpcodeInfo[$6E].Name := 'OUTSB_DXXb';
  BxOpcodeInfo[$6F].Name := 'OUTSW_DXXv';
  BxOpcodeInfo[$70].Name := 'JCC_Jw';
  BxOpcodeInfo[$71].Name := 'JCC_Jw';
  BxOpcodeInfo[$72].Name := 'JCC_Jw';
  BxOpcodeInfo[$73].Name := 'JCC_Jw';
  BxOpcodeInfo[$74].Name := 'JCC_Jw';
  BxOpcodeInfo[$75].Name := 'JCC_Jw';
  BxOpcodeInfo[$76].Name := 'JCC_Jw';
  BxOpcodeInfo[$77].Name := 'JCC_Jw';
  BxOpcodeInfo[$78].Name := 'JCC_Jw';
  BxOpcodeInfo[$79].Name := 'JCC_Jw';
  BxOpcodeInfo[$7A].Name := 'JCC_Jw';
  BxOpcodeInfo[$7B].Name := 'JCC_Jw';
  BxOpcodeInfo[$7C].Name := 'JCC_Jw';
  BxOpcodeInfo[$7D].Name := 'JCC_Jw';
  BxOpcodeInfo[$7E].Name := 'JCC_Jw';
  BxOpcodeInfo[$7F].Name := 'JCC_Jw';
  BxOpcodeInfo[$84].Name := 'TEST_EbGb';
  BxOpcodeInfo[$85].Name := 'TEST_EwGw';
  BxOpcodeInfo[$86].Name := 'XCHG_EbGb';
  BxOpcodeInfo[$87].Name := 'XCHG_EwGw';
  BxOpcodeInfo[$88].Name := 'MOV_EbGb';
  BxOpcodeInfo[$89].Name := 'MOV_EwGw';
  BxOpcodeInfo[$8A].Name := 'MOV_GbEb';
  BxOpcodeInfo[$8B].Name := 'MOV_GwEw';
  BxOpcodeInfo[$8C].Name := 'MOV_EwSw';
  BxOpcodeInfo[$8D].Name := 'LEA_GwM';
  BxOpcodeInfo[$8E].Name := 'MOV_SwEw';
  BxOpcodeInfo[$8F].Name := 'POP_Ew';
  BxOpcodeInfo[$90].Name := 'NOP';
  BxOpcodeInfo[$91].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$92].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$93].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$94].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$95].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$96].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$97].Name := 'XCHG_RXAX';
  BxOpcodeInfo[$98].Name := 'CBW';
  BxOpcodeInfo[$99].Name := 'CWD';
  BxOpcodeInfo[$9A].Name := 'CALL16_Ap';
  BxOpcodeInfo[$9B].Name := 'FWAIT';
  BxOpcodeInfo[$9C].Name := 'PUSHF_Fv';
  BxOpcodeInfo[$9D].Name := 'POPF_Fv';
  BxOpcodeInfo[$9E].Name := 'SAHF';
  BxOpcodeInfo[$9F].Name := 'LAHF';
  BxOpcodeInfo[$A0].Name := 'MOV_ALOb';
  BxOpcodeInfo[$A1].Name := 'MOV_AXOw';
  BxOpcodeInfo[$A2].Name := 'MOV_ObAL';
  BxOpcodeInfo[$A3].Name := 'MOV_OwAX';
  BxOpcodeInfo[$A4].Name := 'MOVSB_XbYb';
  BxOpcodeInfo[$A5].Name := 'MOVSW_XvYv';
  BxOpcodeInfo[$A6].Name := 'CMPSB_XbYb';
  BxOpcodeInfo[$A7].Name := 'CMPSW_XvYv';
  BxOpcodeInfo[$A8].Name := 'TEST_ALIb';
  BxOpcodeInfo[$A9].Name := 'TEST_AXIw';
  BxOpcodeInfo[$AA].Name := 'STOSB_YbAL';
  BxOpcodeInfo[$AB].Name := 'STOSW_YveAX';
  BxOpcodeInfo[$AC].Name := 'LODSB_ALXb';
  BxOpcodeInfo[$AD].Name := 'LODSW_eAXXv';
  BxOpcodeInfo[$AE].Name := 'SCASB_ALXb';
  BxOpcodeInfo[$AF].Name := 'SCASW_eAXXv';
  BxOpcodeInfo[$B0].Name := 'MOV_RLIb';
  BxOpcodeInfo[$B1].Name := 'MOV_RLIb';
  BxOpcodeInfo[$B2].Name := 'MOV_RLIb';
  BxOpcodeInfo[$B3].Name := 'MOV_RLIb';
  BxOpcodeInfo[$B4].Name := 'MOV_RHIb';
  BxOpcodeInfo[$B5].Name := 'MOV_RHIb';
  BxOpcodeInfo[$B6].Name := 'MOV_RHIb';
  BxOpcodeInfo[$B7].Name := 'MOV_RHIb';
  BxOpcodeInfo[$B8].Name := 'MOV_RXIw';
  BxOpcodeInfo[$B9].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BA].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BB].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BC].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BD].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BE].Name := 'MOV_RXIw';
  BxOpcodeInfo[$BF].Name := 'MOV_RXIw';
  BxOpcodeInfo[$C2].Name := 'RET';
  BxOpcodeInfo[$C3].Name := 'RET';
  BxOpcodeInfo[$C4].Name := 'LES_GvMp';
  BxOpcodeInfo[$C5].Name := 'LDS_GvMp';
  BxOpcodeInfo[$C6].Name := 'MOV_EbIb';
  BxOpcodeInfo[$C7].Name := 'MOV_EwIw';
  BxOpcodeInfo[$C8].Name := 'ENTER_IwIb';
  BxOpcodeInfo[$C9].Name := 'LEAVE';
  BxOpcodeInfo[$CA].Name := 'RETfar16_Iw';
  BxOpcodeInfo[$CB].Name := 'RETfar16';
  BxOpcodeInfo[$CC].Name := 'INT3';
  BxOpcodeInfo[$CD].Name := 'INT_Ib';
  BxOpcodeInfo[$CE].Name := 'INTO';
  BxOpcodeInfo[$CF].Name := 'IRET16';
  BxOpcodeInfo[$D4].Name := 'AAM';
  BxOpcodeInfo[$D5].Name := 'AAD';
  BxOpcodeInfo[$D6].Name := 'SALC';
  BxOpcodeInfo[$D7].Name := 'XLAT';
  BxOpcodeInfo[$E0].Name := 'LOOPNE_Jb';
  BxOpcodeInfo[$E1].Name := 'LOOPE_Jb';
  BxOpcodeInfo[$E2].Name := 'LOOP_Jb';
  BxOpcodeInfo[$E3].Name := 'JCXZ_Jb';
  BxOpcodeInfo[$E4].Name := 'IN_ALIb';
  BxOpcodeInfo[$E5].Name := 'IN_eAXIb';
  BxOpcodeInfo[$E6].Name := 'OUT_IbAL';
  BxOpcodeInfo[$E7].Name := 'OUT_IbeAX';
  BxOpcodeInfo[$E8].Name := 'CALL_Aw';
  BxOpcodeInfo[$E9].Name := 'JMP_Jw';
  BxOpcodeInfo[$EA].Name := 'JMP_Ap';
  BxOpcodeInfo[$EB].Name := 'JMP_Jw';
  BxOpcodeInfo[$EC].Name := 'IN_ALDX';
  BxOpcodeInfo[$ED].Name := 'IN_eAXDX';
  BxOpcodeInfo[$EE].Name := 'OUT_DXAL';
  BxOpcodeInfo[$EF].Name := 'OUT_DXeAX';
  BxOpcodeInfo[$F0].Name := 'BxError'; // LOCK
  BxOpcodeInfo[$F1].Name := 'INT1';
  BxOpcodeInfo[$F2].Name := 'BxError'; // REPNE/REPNZ
  BxOpcodeInfo[$F3].Name := 'BxError'; // REP, REPE/REPZ
  BxOpcodeInfo[$F4].Name := 'HLT';
  BxOpcodeInfo[$F5].Name := 'CMC';
  BxOpcodeInfo[$F8].Name := 'CLC';
  BxOpcodeInfo[$F9].Name := 'STC';
  BxOpcodeInfo[$FA].Name := 'CLI';
  BxOpcodeInfo[$FB].Name := 'STI';
  BxOpcodeInfo[$FC].Name := 'CLD';
  BxOpcodeInfo[$FD].Name := 'STD';
  BxOpcodeInfo[$102].Name := 'LAR_GvEw';
  BxOpcodeInfo[$103].Name := 'LSL_GvEw';
  BxOpcodeInfo[$104].Name := 'BxError';
  BxOpcodeInfo[$105].Name := 'LOADALL';
  BxOpcodeInfo[$106].Name := 'CLTS';
  BxOpcodeInfo[$107].Name := 'BxError';
  BxOpcodeInfo[$108].Name := 'INVD';
  BxOpcodeInfo[$109].Name := 'WBINVD';
  BxOpcodeInfo[$120].Name := 'MOV_RdCd';
  BxOpcodeInfo[$121].Name := 'MOV_RdDd';
  BxOpcodeInfo[$122].Name := 'MOV_CdRd';
  BxOpcodeInfo[$123].Name := 'MOV_DdRd';
  BxOpcodeInfo[$124].Name := 'MOV_RdTd';
  BxOpcodeInfo[$125].Name := 'BxError';
  BxOpcodeInfo[$126].Name := 'MOV_TdRd';
  BxOpcodeInfo[$140].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$141].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$142].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$143].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$144].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$145].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$146].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$147].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$148].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$149].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14A].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14B].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14C].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14D].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14E].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$14F].Name := 'CMOV_GwEw';
  BxOpcodeInfo[$180].Name := 'JCC_Jw';
  BxOpcodeInfo[$181].Name := 'JCC_Jw';
  BxOpcodeInfo[$182].Name := 'JCC_Jw';
  BxOpcodeInfo[$183].Name := 'JCC_Jw';
  BxOpcodeInfo[$184].Name := 'JCC_Jw';
  BxOpcodeInfo[$185].Name := 'JCC_Jw';
  BxOpcodeInfo[$186].Name := 'JCC_Jw';
  BxOpcodeInfo[$187].Name := 'JCC_Jw';
  BxOpcodeInfo[$188].Name := 'JCC_Jw';
  BxOpcodeInfo[$189].Name := 'JCC_Jw';
  BxOpcodeInfo[$18A].Name := 'JCC_Jw';
  BxOpcodeInfo[$18B].Name := 'JCC_Jw';
  BxOpcodeInfo[$18C].Name := 'JCC_Jw';
  BxOpcodeInfo[$18D].Name := 'JCC_Jw';
  BxOpcodeInfo[$18E].Name := 'JCC_Jw';
  BxOpcodeInfo[$18F].Name := 'JCC_Jw';
  BxOpcodeInfo[$190].Name := 'SETO_Eb';
  BxOpcodeInfo[$191].Name := 'SETNO_Eb';
  BxOpcodeInfo[$192].Name := 'SETB_Eb';
  BxOpcodeInfo[$193].Name := 'SETNB_Eb';
  BxOpcodeInfo[$194].Name := 'SETZ_Eb';
  BxOpcodeInfo[$195].Name := 'SETNZ_Eb';
  BxOpcodeInfo[$196].Name := 'SETBE_Eb';
  BxOpcodeInfo[$197].Name := 'SETNBE_Eb';
  BxOpcodeInfo[$198].Name := 'SETS_Eb';
  BxOpcodeInfo[$199].Name := 'SETNS_Eb';
  BxOpcodeInfo[$19A].Name := 'SETP_Eb';
  BxOpcodeInfo[$19B].Name := 'SETNP_Eb';
  BxOpcodeInfo[$19C].Name := 'SETL_Eb';
  BxOpcodeInfo[$19D].Name := 'SETNL_Eb';
  BxOpcodeInfo[$19E].Name := 'SETLE_Eb';
  BxOpcodeInfo[$19F].Name := 'SETNLE_Eb';
  BxOpcodeInfo[$1A0].Name := 'PUSH_FS';
  BxOpcodeInfo[$1A1].Name := 'POP_FS';
  BxOpcodeInfo[$1A2].Name := 'CPUID';
  BxOpcodeInfo[$1A3].Name := 'BT_EvGv';
  BxOpcodeInfo[$1A4].Name := 'SHLD_EwGw';
  BxOpcodeInfo[$1A5].Name := 'SHLD_EwGw';
  BxOpcodeInfo[$1A6].Name := 'CMPXCHG_XBTS';
  BxOpcodeInfo[$1A7].Name := 'CMPXCHG_IBTS';
  BxOpcodeInfo[$1A8].Name := 'PUSH_GS';
  BxOpcodeInfo[$1A9].Name := 'POP_GS';
  BxOpcodeInfo[$1AA].Name := 'RSM';
  BxOpcodeInfo[$1AB].Name := 'BTS_EvGv';
  BxOpcodeInfo[$1AC].Name := 'SHRD_EwGw';
  BxOpcodeInfo[$1AD].Name := 'SHRD_EwGw';
  BxOpcodeInfo[$1AE].Name := 'BxError';
  BxOpcodeInfo[$1AF].Name := 'IMUL_GwEw';
  BxOpcodeInfo[$1B0].Name := 'CMPXCHG_EbGb';
  BxOpcodeInfo[$1B1].Name := 'CMPXCHG_EwGw';
  BxOpcodeInfo[$1B2].Name := 'LSS_GvMp';
  BxOpcodeInfo[$1B3].Name := 'BTR_EvGv';
  BxOpcodeInfo[$1B4].Name := 'LFS_GvMp';
  BxOpcodeInfo[$1B5].Name := 'LGS_GvMp';
  BxOpcodeInfo[$1B6].Name := 'MOVZX_GwEb';
  BxOpcodeInfo[$1B7].Name := 'MOVZX_GwEw';
  BxOpcodeInfo[$1B8].Name := 'BxError';
  BxOpcodeInfo[$1B9].Name := 'BxError';
  BxOpcodeInfo[$1BB].Name := 'BTC_EvGv';
  BxOpcodeInfo[$1BC].Name := 'BSF_GvEv';
  BxOpcodeInfo[$1BD].Name := 'BSR_GvEv';
  BxOpcodeInfo[$1BE].Name := 'MOVSX_GwEb';
  BxOpcodeInfo[$1BF].Name := 'MOVSX_GwEw';
  BxOpcodeInfo[$1C0].Name := 'XADD_EbGb';
  BxOpcodeInfo[$1C1].Name := 'XADD_EwGw';
  BxOpcodeInfo[$1C2].Name := 'BxError';
  BxOpcodeInfo[$1C3].Name := 'BxError';
  BxOpcodeInfo[$1C4].Name := 'BxError';
  BxOpcodeInfo[$1C5].Name := 'BxError';
  BxOpcodeInfo[$1C6].Name := 'BxError';
  BxOpcodeInfo[$1C8].Name := 'BSWAP_EAX';
  BxOpcodeInfo[$1C9].Name := 'BSWAP_ECX';
  BxOpcodeInfo[$1CA].Name := 'BSWAP_EDX';
  BxOpcodeInfo[$1CB].Name := 'BSWAP_EBX';
  BxOpcodeInfo[$1CC].Name := 'BSWAP_ESP';
  BxOpcodeInfo[$1CD].Name := 'BSWAP_EBP';
  BxOpcodeInfo[$1CE].Name := 'BSWAP_ESI';
  BxOpcodeInfo[$1CF].Name := 'BSWAP_EDI';
  BxOpcodeInfo[$1D0].Name := 'BxError';
  BxOpcodeInfo[$1D1].Name := 'BxError';
  BxOpcodeInfo[$1D2].Name := 'BxError';
  BxOpcodeInfo[$1D3].Name := 'BxError';
  BxOpcodeInfo[$1D4].Name := 'BxError';
  BxOpcodeInfo[$1D5].Name := 'BxError';
  BxOpcodeInfo[$1D6].Name := 'BxError';
  BxOpcodeInfo[$1D7].Name := 'BxError';
  BxOpcodeInfo[$1D8].Name := 'BxError';
  BxOpcodeInfo[$1D9].Name := 'BxError';
  BxOpcodeInfo[$1DA].Name := 'BxError';
  BxOpcodeInfo[$1DB].Name := 'BxError';
  BxOpcodeInfo[$1DC].Name := 'BxError';
  BxOpcodeInfo[$1DD].Name := 'BxError';
  BxOpcodeInfo[$1DE].Name := 'BxError';
  BxOpcodeInfo[$1DF].Name := 'BxError';
  BxOpcodeInfo[$1E0].Name := 'BxError';
  BxOpcodeInfo[$1E1].Name := 'BxError';
  BxOpcodeInfo[$1E2].Name := 'BxError';
  BxOpcodeInfo[$1E3].Name := 'BxError';
  BxOpcodeInfo[$1E4].Name := 'BxError';
  BxOpcodeInfo[$1E5].Name := 'BxError';
  BxOpcodeInfo[$1E6].Name := 'BxError';
  BxOpcodeInfo[$1E7].Name := 'BxError';
  BxOpcodeInfo[$1E8].Name := 'BxError';
  BxOpcodeInfo[$1E9].Name := 'BxError';
  BxOpcodeInfo[$1EA].Name := 'BxError';
  BxOpcodeInfo[$1EB].Name := 'BxError';
  BxOpcodeInfo[$1EC].Name := 'BxError';
  BxOpcodeInfo[$1ED].Name := 'BxError';
  BxOpcodeInfo[$1EE].Name := 'BxError';
  BxOpcodeInfo[$1EF].Name := 'BxError';
  BxOpcodeInfo[$1F0].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F1].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F2].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F3].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F4].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F5].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F6].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F7].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F8].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1F9].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FA].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FB].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FC].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FD].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FE].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$1FF].Name := 'UndefinedOpcode';
  BxOpcodeInfo[$200].Name := 'ADD_EbGb';
  BxOpcodeInfo[$201].Name := 'ADD_EdGd';
  BxOpcodeInfo[$202].Name := 'ADD_GbEb';
  BxOpcodeInfo[$203].Name := 'ADD_GdEd';
  BxOpcodeInfo[$204].Name := 'ADD_ALIb';
  BxOpcodeInfo[$205].Name := 'ADD_EAXId';
  BxOpcodeInfo[$206].Name := 'PUSH_ES';
  BxOpcodeInfo[$207].Name := 'POP_ES';
  BxOpcodeInfo[$208].Name := 'OR_EbGb';
  BxOpcodeInfo[$209].Name := 'OR_EdGd';
  BxOpcodeInfo[$20A].Name := 'OR_GbEb';
  BxOpcodeInfo[$20B].Name := 'OR_GdEd';
  BxOpcodeInfo[$20C].Name := 'OR_ALIb';
  BxOpcodeInfo[$20D].Name := 'OR_EAXId';
  BxOpcodeInfo[$20E].Name := 'PUSH_CS';
  BxOpcodeInfo[$20F].Name := 'BxError'; // 2-byte escape
  BxOpcodeInfo[$210].Name := 'ADC_EbGb';
  BxOpcodeInfo[$211].Name := 'ADC_EdGd';
  BxOpcodeInfo[$212].Name := 'ADC_GbEb';
  BxOpcodeInfo[$213].Name := 'ADC_GdEd';
  BxOpcodeInfo[$214].Name := 'ADC_ALIb';
  BxOpcodeInfo[$215].Name := 'ADC_EAXId';
  BxOpcodeInfo[$216].Name := 'PUSH_SS';
  BxOpcodeInfo[$217].Name := 'POP_SS';
  BxOpcodeInfo[$218].Name := 'SBB_EbGb';
  BxOpcodeInfo[$219].Name := 'SBB_EdGd';
  BxOpcodeInfo[$21A].Name := 'SBB_GbEb';
  BxOpcodeInfo[$21B].Name := 'SBB_GdEd';
  BxOpcodeInfo[$21C].Name := 'SBB_ALIb';
  BxOpcodeInfo[$21D].Name := 'SBB_EAXId';
  BxOpcodeInfo[$21E].Name := 'PUSH_DS';
  BxOpcodeInfo[$21F].Name := 'POP_DS';
  BxOpcodeInfo[$220].Name := 'AND_EbGb';
  BxOpcodeInfo[$221].Name := 'AND_EdGd';
  BxOpcodeInfo[$222].Name := 'AND_GbEb';
  BxOpcodeInfo[$223].Name := 'AND_GdEd';
  BxOpcodeInfo[$224].Name := 'AND_ALIb';
  BxOpcodeInfo[$225].Name := 'AND_EAXId';
  BxOpcodeInfo[$226].Name := 'BxError'; // ES:
  BxOpcodeInfo[$227].Name := 'DAA';
  BxOpcodeInfo[$228].Name := 'SUB_EbGb';
  BxOpcodeInfo[$229].Name := 'SUB_EdGd';
  BxOpcodeInfo[$22A].Name := 'SUB_GbEb';
  BxOpcodeInfo[$22B].Name := 'SUB_GdEd';
  BxOpcodeInfo[$22C].Name := 'SUB_ALIb';
  BxOpcodeInfo[$22D].Name := 'SUB_EAXId';
  BxOpcodeInfo[$22E].Name := 'BxError'; // CS:
  BxOpcodeInfo[$22F].Name := 'DAS';
  BxOpcodeInfo[$230].Name := 'XOR_EbGb';
  BxOpcodeInfo[$231].Name := 'XOR_EdGd';
  BxOpcodeInfo[$232].Name := 'XOR_GbEb';
  BxOpcodeInfo[$233].Name := 'XOR_GdEd';
  BxOpcodeInfo[$234].Name := 'XOR_ALIb';
  BxOpcodeInfo[$235].Name := 'XOR_EAXId';
  BxOpcodeInfo[$236].Name := 'BxError'; // SS:
  BxOpcodeInfo[$237].Name := 'AAA';
  BxOpcodeInfo[$238].Name := 'CMP_EbGb';
  BxOpcodeInfo[$239].Name := 'CMP_EdGd';
  BxOpcodeInfo[$23A].Name := 'CMP_GbEb';
  BxOpcodeInfo[$23B].Name := 'CMP_GdEd';
  BxOpcodeInfo[$23C].Name := 'CMP_ALIb';
  BxOpcodeInfo[$23D].Name := 'CMP_EAXId';
  BxOpcodeInfo[$23E].Name := 'BxError'; // DS:
  BxOpcodeInfo[$23F].Name := 'AAS';
  BxOpcodeInfo[$240].Name := 'INC_ERX';
  BxOpcodeInfo[$241].Name := 'INC_ERX';
  BxOpcodeInfo[$242].Name := 'INC_ERX';
  BxOpcodeInfo[$243].Name := 'INC_ERX';
  BxOpcodeInfo[$244].Name := 'INC_ERX';
  BxOpcodeInfo[$245].Name := 'INC_ERX';
  BxOpcodeInfo[$246].Name := 'INC_ERX';
  BxOpcodeInfo[$247].Name := 'INC_ERX';
  BxOpcodeInfo[$248].Name := 'DEC_ERX';
  BxOpcodeInfo[$249].Name := 'DEC_ERX';
  BxOpcodeInfo[$24A].Name := 'DEC_ERX';
  BxOpcodeInfo[$24B].Name := 'DEC_ERX';
  BxOpcodeInfo[$24C].Name := 'DEC_ERX';
  BxOpcodeInfo[$24D].Name := 'DEC_ERX';
  BxOpcodeInfo[$24E].Name := 'DEC_ERX';
  BxOpcodeInfo[$24F].Name := 'DEC_ERX';
  BxOpcodeInfo[$250].Name := 'PUSH_ERX';
  BxOpcodeInfo[$251].Name := 'PUSH_ERX';
  BxOpcodeInfo[$252].Name := 'PUSH_ERX';
  BxOpcodeInfo[$253].Name := 'PUSH_ERX';
  BxOpcodeInfo[$254].Name := 'PUSH_ERX';
  BxOpcodeInfo[$255].Name := 'PUSH_ERX';
  BxOpcodeInfo[$256].Name := 'PUSH_ERX';
  BxOpcodeInfo[$257].Name := 'PUSH_ERX';
  BxOpcodeInfo[$258].Name := 'POP_ERX';
  BxOpcodeInfo[$259].Name := 'POP_ERX';
  BxOpcodeInfo[$25A].Name := 'POP_ERX';
  BxOpcodeInfo[$25B].Name := 'POP_ERX';
  BxOpcodeInfo[$25C].Name := 'POP_ERX';
  BxOpcodeInfo[$25D].Name := 'POP_ERX';
  BxOpcodeInfo[$25E].Name := 'POP_ERX';
  BxOpcodeInfo[$25F].Name := 'POP_ERX';
  BxOpcodeInfo[$260].Name := 'PUSHAD32';
  BxOpcodeInfo[$261].Name := 'POPAD32';
  BxOpcodeInfo[$262].Name := 'BOUND_GvMa';
  BxOpcodeInfo[$263].Name := 'ARPL_EwGw';
  BxOpcodeInfo[$264].Name := 'BxError'; // FS:
  BxOpcodeInfo[$265].Name := 'BxError'; // GS:
  BxOpcodeInfo[$266].Name := 'BxError'; // OS:
  BxOpcodeInfo[$267].Name := 'BxError'; // AS:
  BxOpcodeInfo[$268].Name := 'PUSH_Id';
  BxOpcodeInfo[$269].Name := 'IMUL_GdEdId';
  BxOpcodeInfo[$26A].Name := 'PUSH_Id';
  BxOpcodeInfo[$26B].Name := 'IMUL_GdEdId';
  BxOpcodeInfo[$26C].Name := 'INSB_YbDX';
  BxOpcodeInfo[$26D].Name := 'INSW_YvDX';
  BxOpcodeInfo[$26E].Name := 'OUTSB_DXXb';
  BxOpcodeInfo[$26F].Name := 'OUTSW_DXXv';
  BxOpcodeInfo[$270].Name := 'JCC_Jd';
  BxOpcodeInfo[$271].Name := 'JCC_Jd';
  BxOpcodeInfo[$272].Name := 'JCC_Jd';
  BxOpcodeInfo[$273].Name := 'JCC_Jd';
  BxOpcodeInfo[$274].Name := 'JCC_Jd';
  BxOpcodeInfo[$275].Name := 'JCC_Jd';
  BxOpcodeInfo[$276].Name := 'JCC_Jd';
  BxOpcodeInfo[$277].Name := 'JCC_Jd';
  BxOpcodeInfo[$278].Name := 'JCC_Jd';
  BxOpcodeInfo[$279].Name := 'JCC_Jd';
  BxOpcodeInfo[$27A].Name := 'JCC_Jd';
  BxOpcodeInfo[$27B].Name := 'JCC_Jd';
  BxOpcodeInfo[$27C].Name := 'JCC_Jd';
  BxOpcodeInfo[$27D].Name := 'JCC_Jd';
  BxOpcodeInfo[$27E].Name := 'JCC_Jd';
  BxOpcodeInfo[$27F].Name := 'JCC_Jd';
  BxOpcodeInfo[$284].Name := 'TEST_EbGb';
  BxOpcodeInfo[$285].Name := 'TEST_EdGd';
  BxOpcodeInfo[$286].Name := 'XCHG_EbGb';
  BxOpcodeInfo[$287].Name := 'XCHG_EdGd';
  BxOpcodeInfo[$288].Name := 'MOV_EbGb';
  BxOpcodeInfo[$289].Name := 'MOV_EdGd';
  BxOpcodeInfo[$28A].Name := 'MOV_GbEb';
  BxOpcodeInfo[$28B].Name := 'MOV_GdEd';
  BxOpcodeInfo[$28C].Name := 'MOV_EwSw';
  BxOpcodeInfo[$28D].Name := 'LEA_GdM';
  BxOpcodeInfo[$28E].Name := 'MOV_SwEw';
  BxOpcodeInfo[$28F].Name := 'POP_Ed';
  BxOpcodeInfo[$290].Name := 'NOP';
  BxOpcodeInfo[$291].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$292].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$293].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$294].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$295].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$296].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$297].Name := 'XCHG_ERXEAX';
  BxOpcodeInfo[$298].Name := 'CWDE';
  BxOpcodeInfo[$299].Name := 'CDQ';
  BxOpcodeInfo[$29A].Name := 'CALL32_Ap';
  BxOpcodeInfo[$29B].Name := 'FWAIT';
  BxOpcodeInfo[$29C].Name := 'PUSHF_Fv';
  BxOpcodeInfo[$29D].Name := 'POPF_Fv';
  BxOpcodeInfo[$29E].Name := 'SAHF';
  BxOpcodeInfo[$29F].Name := 'LAHF';
  BxOpcodeInfo[$2A0].Name := 'MOV_ALOb';
  BxOpcodeInfo[$2A1].Name := 'MOV_EAXOd';
  BxOpcodeInfo[$2A2].Name := 'MOV_ObAL';
  BxOpcodeInfo[$2A3].Name := 'MOV_OdEAX';
  BxOpcodeInfo[$2A4].Name := 'MOVSB_XbYb';
  BxOpcodeInfo[$2A5].Name := 'MOVSW_XvYv';
  BxOpcodeInfo[$2A6].Name := 'CMPSB_XbYb';
  BxOpcodeInfo[$2A7].Name := 'CMPSW_XvYv';
  BxOpcodeInfo[$2A8].Name := 'TEST_ALIb';
  BxOpcodeInfo[$2A9].Name := 'TEST_EAXId';
  BxOpcodeInfo[$2AA].Name := 'STOSB_YbAL';
  BxOpcodeInfo[$2AB].Name := 'STOSW_YveAX';
  BxOpcodeInfo[$2AC].Name := 'LODSB_ALXb';
  BxOpcodeInfo[$2AD].Name := 'LODSW_eAXXv';
  BxOpcodeInfo[$2AE].Name := 'SCASB_ALXb';
  BxOpcodeInfo[$2AF].Name := 'SCASW_eAXXv';
  BxOpcodeInfo[$2B0].Name := 'MOV_RLIb';
  BxOpcodeInfo[$2B1].Name := 'MOV_RLIb';
  BxOpcodeInfo[$2B2].Name := 'MOV_RLIb';
  BxOpcodeInfo[$2B3].Name := 'MOV_RLIb';
  BxOpcodeInfo[$2B4].Name := 'MOV_RHIb';
  BxOpcodeInfo[$2B5].Name := 'MOV_RHIb';
  BxOpcodeInfo[$2B6].Name := 'MOV_RHIb';
  BxOpcodeInfo[$2B7].Name := 'MOV_RHIb';
  BxOpcodeInfo[$2B8].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2B9].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BA].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BB].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BC].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BD].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BE].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2BF].Name := 'MOV_ERXId';
  BxOpcodeInfo[$2C2].Name := 'RETnear32_Iw';
  BxOpcodeInfo[$2C3].Name := 'RETnear32';
  BxOpcodeInfo[$2C4].Name := 'LES_GvMp';
  BxOpcodeInfo[$2C5].Name := 'LDS_GvMp';
  BxOpcodeInfo[$2C6].Name := 'MOV_EbIb';
  BxOpcodeInfo[$2C7].Name := 'MOV_EdId';
  BxOpcodeInfo[$2C8].Name := 'ENTER_IwIb';
  BxOpcodeInfo[$2C9].Name := 'LEAVE';
  BxOpcodeInfo[$2CA].Name := 'RETfar32_Iw';
  BxOpcodeInfo[$2CB].Name := 'RETfar32';
  BxOpcodeInfo[$2CC].Name := 'INT3';
  BxOpcodeInfo[$2CD].Name := 'INT_Ib';
  BxOpcodeInfo[$2CE].Name := 'INTO';
  BxOpcodeInfo[$2CF].Name := 'IRET32';
  BxOpcodeInfo[$2D4].Name := 'AAM';
  BxOpcodeInfo[$2D5].Name := 'AAD';
  BxOpcodeInfo[$2D6].Name := 'SALC';
  BxOpcodeInfo[$2D7].Name := 'XLAT';
  BxOpcodeInfo[$2E0].Name := 'LOOPNE_Jb';
  BxOpcodeInfo[$2E1].Name := 'LOOPE_Jb';
  BxOpcodeInfo[$2E2].Name := 'LOOP_Jb';
  BxOpcodeInfo[$2E3].Name := 'JCXZ_Jb';
  BxOpcodeInfo[$2E4].Name := 'IN_ALIb';
  BxOpcodeInfo[$2E5].Name := 'IN_eAXIb';
  BxOpcodeInfo[$2E6].Name := 'OUT_IbAL';
  BxOpcodeInfo[$2E7].Name := 'OUT_IbeAX';
  BxOpcodeInfo[$2E8].Name := 'CALL_Ad';
  BxOpcodeInfo[$2E9].Name := 'JMP_Jd';
  BxOpcodeInfo[$2EA].Name := 'JMP_Ap';
  BxOpcodeInfo[$2EB].Name := 'JMP_Jd';
  BxOpcodeInfo[$2EC].Name := 'IN_ALDX';
  BxOpcodeInfo[$2ED].Name := 'IN_eAXDX';
  BxOpcodeInfo[$2EE].Name := 'OUT_DXAL';
  BxOpcodeInfo[$2EF].Name := 'OUT_DXeAX';
  BxOpcodeInfo[$2F1].Name := 'INT1';
  BxOpcodeInfo[$2F4].Name := 'HLT';
  BxOpcodeInfo[$2F5].Name := 'CMC';
  BxOpcodeInfo[$2F8].Name := 'CLC';
  BxOpcodeInfo[$2F9].Name := 'STC';
  BxOpcodeInfo[$2FA].Name := 'CLI';
  BxOpcodeInfo[$2FB].Name := 'STI';
  BxOpcodeInfo[$2FC].Name := 'CLD';
  BxOpcodeInfo[$2FD].Name := 'STD';
  BxOpcodeInfo[$302].Name := 'LAR_GvEw';
  BxOpcodeInfo[$303].Name := 'LSL_GvEw';
  BxOpcodeInfo[$304].Name := 'BxError';
  BxOpcodeInfo[$305].Name := 'LOADALL';
  BxOpcodeInfo[$306].Name := 'CLTS';
  BxOpcodeInfo[$307].Name := 'BxError';
  BxOpcodeInfo[$308].Name := 'INVD';
  BxOpcodeInfo[$309].Name := 'WBINVD';
  BxOpcodeInfo[$320].Name := 'MOV_RdCd';
  BxOpcodeInfo[$321].Name := 'MOV_RdDd';
  BxOpcodeInfo[$322].Name := 'MOV_CdRd';
  BxOpcodeInfo[$323].Name := 'MOV_DdRd';
  BxOpcodeInfo[$324].Name := 'MOV_RdTd';
  BxOpcodeInfo[$326].Name := 'MOV_TdRd';
  BxOpcodeInfo[$330].Name := 'WRMSR';
  BxOpcodeInfo[$331].Name := 'RDTSC';
  BxOpcodeInfo[$332].Name := 'RDMSR';
  BxOpcodeInfo[$340].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$341].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$342].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$343].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$344].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$345].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$346].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$347].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$348].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$349].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34A].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34B].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34C].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34D].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34E].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$34F].Name := 'CMOV_GdEd';
  BxOpcodeInfo[$380].Name := 'JCC_Jd';
  BxOpcodeInfo[$381].Name := 'JCC_Jd';
  BxOpcodeInfo[$382].Name := 'JCC_Jd';
  BxOpcodeInfo[$383].Name := 'JCC_Jd';
  BxOpcodeInfo[$384].Name := 'JCC_Jd';
  BxOpcodeInfo[$385].Name := 'JCC_Jd';
  BxOpcodeInfo[$386].Name := 'JCC_Jd';
  BxOpcodeInfo[$387].Name := 'JCC_Jd';
  BxOpcodeInfo[$388].Name := 'JCC_Jd';
  BxOpcodeInfo[$389].Name := 'JCC_Jd';
  BxOpcodeInfo[$38A].Name := 'JCC_Jd';
  BxOpcodeInfo[$38B].Name := 'JCC_Jd';
  BxOpcodeInfo[$38C].Name := 'JCC_Jd';
  BxOpcodeInfo[$38D].Name := 'JCC_Jd';
  BxOpcodeInfo[$38E].Name := 'JCC_Jd';
  BxOpcodeInfo[$38F].Name := 'JCC_Jd';
  BxOpcodeInfo[$390].Name := 'SETO_Eb';
  BxOpcodeInfo[$391].Name := 'SETNO_Eb';
  BxOpcodeInfo[$392].Name := 'SETB_Eb';
  BxOpcodeInfo[$393].Name := 'SETNB_Eb';
  BxOpcodeInfo[$394].Name := 'SETZ_Eb';
  BxOpcodeInfo[$395].Name := 'SETNZ_Eb';
  BxOpcodeInfo[$396].Name := 'SETBE_Eb';
  BxOpcodeInfo[$397].Name := 'SETNBE_Eb';
  BxOpcodeInfo[$398].Name := 'SETS_Eb';
  BxOpcodeInfo[$399].Name := 'SETNS_Eb';
  BxOpcodeInfo[$39A].Name := 'SETP_Eb';
  BxOpcodeInfo[$39B].Name := 'SETNP_Eb';
  BxOpcodeInfo[$39C].Name := 'SETL_Eb';
  BxOpcodeInfo[$39D].Name := 'SETNL_Eb';
  BxOpcodeInfo[$39E].Name := 'SETLE_Eb';
  BxOpcodeInfo[$39F].Name := 'SETNLE_Eb';
  BxOpcodeInfo[$3A0].Name := 'PUSH_FS';
  BxOpcodeInfo[$3A1].Name := 'POP_FS';
  BxOpcodeInfo[$3A2].Name := 'CPUID';
  BxOpcodeInfo[$3A3].Name := 'BT_EvGv';
  BxOpcodeInfo[$3A4].Name := 'SHLD_EdGd';
  BxOpcodeInfo[$3A5].Name := 'SHLD_EdGd';
  BxOpcodeInfo[$3A6].Name := 'CMPXCHG_XBTS';
  BxOpcodeInfo[$3A7].Name := 'CMPXCHG_IBTS';
  BxOpcodeInfo[$3A8].Name := 'PUSH_GS';
  BxOpcodeInfo[$3A9].Name := 'POP_GS';
  BxOpcodeInfo[$3AA].Name := 'RSM';
  BxOpcodeInfo[$3AB].Name := 'BTS_EvGv';
  BxOpcodeInfo[$3AC].Name := 'SHRD_EdGd';
  BxOpcodeInfo[$3AD].Name := 'SHRD_EdGd';
  BxOpcodeInfo[$3AF].Name := 'IMUL_GdEd';
  BxOpcodeInfo[$3B0].Name := 'CMPXCHG_EbGb';
  BxOpcodeInfo[$3B1].Name := 'CMPXCHG_EdGd';
  BxOpcodeInfo[$3B2].Name := 'LSS_GvMp';
  BxOpcodeInfo[$3B3].Name := 'BTR_EvGv';
  BxOpcodeInfo[$3B4].Name := 'LFS_GvMp';
  BxOpcodeInfo[$3B5].Name := 'LGS_GvMp';
  BxOpcodeInfo[$3B6].Name := 'MOVZX_GdEb';
  BxOpcodeInfo[$3B7].Name := 'MOVZX_GdEw';
  BxOpcodeInfo[$3BB].Name := 'BTC_EvGv';
  BxOpcodeInfo[$3BC].Name := 'BSF_GvEv';
  BxOpcodeInfo[$3BD].Name := 'BSR_GvEv';
  BxOpcodeInfo[$3BE].Name := 'MOVSX_GdEb';
  BxOpcodeInfo[$3BF].Name := 'MOVSX_GdEw';
  BxOpcodeInfo[$3C0].Name := 'XADD_EbGb';
  BxOpcodeInfo[$3C1].Name := 'XADD_EdGd';
end;

procedure InitSystem;
begin
  BxResolve16mod0[0] := bx_cpu.Resolve16mod0Rm0;
  BxResolve16mod0[1] := bx_cpu.Resolve16mod0Rm1;
  BxResolve16mod0[2] := bx_cpu.Resolve16mod0Rm2;
  BxResolve16mod0[3] := bx_cpu.Resolve16mod0Rm3;
  BxResolve16mod0[4] := bx_cpu.Resolve16mod0Rm4;
  BxResolve16mod0[5] := bx_cpu.Resolve16mod0Rm5;
  BxResolve16mod0[6] := nil; // d16, no registers used
  BxResolve16mod0[7] := bx_cpu.Resolve16mod0Rm7;

  BxResolve16mod1or2[0] := bx_cpu.Resolve16mod1or2Rm0;
  BxResolve16mod1or2[1] := bx_cpu.Resolve16mod1or2Rm1;
  BxResolve16mod1or2[2] := bx_cpu.Resolve16mod1or2Rm2;
  BxResolve16mod1or2[3] := bx_cpu.Resolve16mod1or2Rm3;
  BxResolve16mod1or2[4] := bx_cpu.Resolve16mod1or2Rm4;
  BxResolve16mod1or2[5] := bx_cpu.Resolve16mod1or2Rm5;
  BxResolve16mod1or2[6] := bx_cpu.Resolve16mod1or2Rm6;
  BxResolve16mod1or2[7] := bx_cpu.Resolve16mod1or2Rm7;

  BxResolve32mod0[0] := bx_cpu.Resolve32mod0Rm0;
  BxResolve32mod0[1] := bx_cpu.Resolve32mod0Rm1;
  BxResolve32mod0[2] := bx_cpu.Resolve32mod0Rm2;
  BxResolve32mod0[3] := bx_cpu.Resolve32mod0Rm3;
  BxResolve32mod0[4] := NULL; // escape to 2-byte
  BxResolve32mod0[5] := NULL; // d32, no registers used
  BxResolve32mod0[6] := bx_cpu.Resolve32mod0Rm6;
  BxResolve32mod0[7] := bx_cpu.Resolve32mod0Rm7;

  BxResolve32mod1or2[0] := bx_cpu.Resolve32mod1or2Rm0;
  BxResolve32mod1or2[1] := bx_cpu.Resolve32mod1or2Rm1;
  BxResolve32mod1or2[2] := bx_cpu.Resolve32mod1or2Rm2;
  BxResolve32mod1or2[3] := bx_cpu.Resolve32mod1or2Rm3;
  BxResolve32mod1or2[4] := NULL; // escape to 2-byte
  BxResolve32mod1or2[5] := bx_cpu.Resolve32mod1or2Rm5;
  BxResolve32mod1or2[6] := bx_cpu.Resolve32mod1or2Rm6;
  BxResolve32mod1or2[7] := bx_cpu.Resolve32mod1or2Rm7;

  BxResolve32mod0Base[0] := bx_cpu.Resolve32mod0Base0;
  BxResolve32mod0Base[1] := bx_cpu.Resolve32mod0Base1;
  BxResolve32mod0Base[2] := bx_cpu.Resolve32mod0Base2;
  BxResolve32mod0Base[3] := bx_cpu.Resolve32mod0Base3;
  BxResolve32mod0Base[4] := bx_cpu.Resolve32mod0Base4;
  BxResolve32mod0Base[5] := bx_cpu.Resolve32mod0Base5;
  BxResolve32mod0Base[6] := bx_cpu.Resolve32mod0Base6;
  BxResolve32mod0Base[7] := bx_cpu.Resolve32mod0Base7;

  BxResolve32mod1or2Base[0] := bx_cpu.Resolve32mod1or2Base0;
  BxResolve32mod1or2Base[1] := bx_cpu.Resolve32mod1or2Base1;
  BxResolve32mod1or2Base[2] := bx_cpu.Resolve32mod1or2Base2;
  BxResolve32mod1or2Base[3] := bx_cpu.Resolve32mod1or2Base3;
  BxResolve32mod1or2Base[4] := bx_cpu.Resolve32mod1or2Base4;
  BxResolve32mod1or2Base[5] := bx_cpu.Resolve32mod1or2Base5;
  BxResolve32mod1or2Base[6] := bx_cpu.Resolve32mod1or2Base6;
  BxResolve32mod1or2Base[7] := bx_cpu.Resolve32mod1or2Base7;

  BxOpcodeInfoG1EbIb[0].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[0].ExecutePtr := bx_cpu.ADD_EbIb;
  BxOpcodeInfoG1EbIb[1].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[1].ExecutePtr := bx_cpu.OR_EbIb;
  BxOpcodeInfoG1EbIb[2].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[2].ExecutePtr := bx_cpu.ADC_EbIb;
  BxOpcodeInfoG1EbIb[3].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[3].ExecutePtr := bx_cpu.SBB_EbIb;
  BxOpcodeInfoG1EbIb[4].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[4].ExecutePtr := bx_cpu.AND_EbIb;
  BxOpcodeInfoG1EbIb[5].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[5].ExecutePtr := bx_cpu.SUB_EbIb;
  BxOpcodeInfoG1EbIb[6].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[6].ExecutePtr := bx_cpu.XOR_EbIb;
  BxOpcodeInfoG1EbIb[7].Attr := BxImmediate_Ib;  BxOpcodeInfoG1EbIb[7].ExecutePtr := bx_cpu.CMP_EbIb;

  BxOpcodeInfoG1Ew[0].Attr := 0;  BxOpcodeInfoG1Ew[0].ExecutePtr := bx_cpu.ADD_EwIw;
  BxOpcodeInfoG1Ew[1].Attr := 0;  BxOpcodeInfoG1Ew[1].ExecutePtr := bx_cpu.OR_EwIw;
  BxOpcodeInfoG1Ew[2].Attr := 0;  BxOpcodeInfoG1Ew[2].ExecutePtr := bx_cpu.ADC_EwIw;
  BxOpcodeInfoG1Ew[3].Attr := 0;  BxOpcodeInfoG1Ew[3].ExecutePtr := bx_cpu.SBB_EwIw;
  BxOpcodeInfoG1Ew[4].Attr := 0;  BxOpcodeInfoG1Ew[4].ExecutePtr := bx_cpu.AND_EwIw;
  BxOpcodeInfoG1Ew[5].Attr := 0;  BxOpcodeInfoG1Ew[5].ExecutePtr := bx_cpu.SUB_EwIw;
  BxOpcodeInfoG1Ew[6].Attr := 0;  BxOpcodeInfoG1Ew[6].ExecutePtr := bx_cpu.XOR_EwIw;
  BxOpcodeInfoG1Ew[7].Attr := 0;  BxOpcodeInfoG1Ew[7].ExecutePtr := bx_cpu.CMP_EwIw;

  BxOpcodeInfoG1Ed[0].Attr:=0;  BxOpcodeInfoG1Ed[0].ExecutePtr:=bx_cpu.ADD_EdId;
  BxOpcodeInfoG1Ed[1].Attr:=0;  BxOpcodeInfoG1Ed[1].ExecutePtr:=bx_cpu.OR_EdId;
  BxOpcodeInfoG1Ed[2].Attr:=0;  BxOpcodeInfoG1Ed[2].ExecutePtr:=bx_cpu.ADC_EdId;
  BxOpcodeInfoG1Ed[3].Attr:=0;  BxOpcodeInfoG1Ed[3].ExecutePtr:=bx_cpu.SBB_EdId;
  BxOpcodeInfoG1Ed[4].Attr:=0;  BxOpcodeInfoG1Ed[4].ExecutePtr:=bx_cpu.AND_EdId;
  BxOpcodeInfoG1Ed[5].Attr:=0;  BxOpcodeInfoG1Ed[5].ExecutePtr:=bx_cpu.SUB_EdId;
  BxOpcodeInfoG1Ed[6].Attr:=0;  BxOpcodeInfoG1Ed[6].ExecutePtr:=bx_cpu.XOR_EdId;
  BxOpcodeInfoG1Ed[7].Attr:=0;  BxOpcodeInfoG1Ed[7].ExecutePtr:=bx_cpu.CMP_EdId;

  // attributes defined in main area
   BxOpcodeInfoG2Eb[0].Attr:=0;  BxOpcodeInfoG2Eb[0].ExecutePtr:=bx_cpu.ROL_Eb;
   BxOpcodeInfoG2Eb[1].Attr:=0;  BxOpcodeInfoG2Eb[1].ExecutePtr:=bx_cpu.ROR_Eb;
   BxOpcodeInfoG2Eb[2].Attr:=0;  BxOpcodeInfoG2Eb[2].ExecutePtr:=bx_cpu.RCL_Eb;
   BxOpcodeInfoG2Eb[3].Attr:=0;  BxOpcodeInfoG2Eb[3].ExecutePtr:=bx_cpu.RCR_Eb;
   BxOpcodeInfoG2Eb[4].Attr:=0;  BxOpcodeInfoG2Eb[4].ExecutePtr:=bx_cpu.SHL_Eb;
   BxOpcodeInfoG2Eb[5].Attr:=0;  BxOpcodeInfoG2Eb[5].ExecutePtr:=bx_cpu.SHR_Eb;
   BxOpcodeInfoG2Eb[6].Attr:=0;  BxOpcodeInfoG2Eb[6].ExecutePtr:=bx_cpu.SHL_Eb;
   BxOpcodeInfoG2Eb[7].Attr:=0;  BxOpcodeInfoG2Eb[7].ExecutePtr:=bx_cpu.SAR_Eb;

  // attributes defined in main area
   BxOpcodeInfoG2Ew[0].Attr:=0;  BxOpcodeInfoG2Ew[0].ExecutePtr:=bx_cpu.ROL_Ew;
   BxOpcodeInfoG2Ew[1].Attr:=0;  BxOpcodeInfoG2Ew[1].ExecutePtr:=bx_cpu.ROR_Ew;
   BxOpcodeInfoG2Ew[2].Attr:=0;  BxOpcodeInfoG2Ew[2].ExecutePtr:=bx_cpu.RCL_Ew;
   BxOpcodeInfoG2Ew[3].Attr:=0;  BxOpcodeInfoG2Ew[3].ExecutePtr:=bx_cpu.RCR_Ew;
   BxOpcodeInfoG2Ew[4].Attr:=0;  BxOpcodeInfoG2Ew[4].ExecutePtr:=bx_cpu.SHL_Ew;
   BxOpcodeInfoG2Ew[5].Attr:=0;  BxOpcodeInfoG2Ew[5].ExecutePtr:=bx_cpu.SHR_Ew;
   BxOpcodeInfoG2Ew[6].Attr:=0;  BxOpcodeInfoG2Ew[6].ExecutePtr:=bx_cpu.SHL_Ew;
   BxOpcodeInfoG2Ew[7].Attr:=0;  BxOpcodeInfoG2Ew[7].ExecutePtr:=bx_cpu.SAR_Ew;

  // attributes defined in main area
   BxOpcodeInfoG2Ed[0].Attr := 0;  BxOpcodeInfoG2Ed[0].ExecutePtr:=bx_cpu.ROL_Ed;
   BxOpcodeInfoG2Ed[1].Attr := 0;  BxOpcodeInfoG2Ed[1].ExecutePtr:=bx_cpu.ROR_Ed;
   BxOpcodeInfoG2Ed[2].Attr := 0;  BxOpcodeInfoG2Ed[2].ExecutePtr:=bx_cpu.RCL_Ed;
   BxOpcodeInfoG2Ed[3].Attr := 0;  BxOpcodeInfoG2Ed[3].ExecutePtr:=bx_cpu.RCR_Ed;
   BxOpcodeInfoG2Ed[4].Attr := 0;  BxOpcodeInfoG2Ed[4].ExecutePtr:=bx_cpu.SHL_Ed;
   BxOpcodeInfoG2Ed[5].Attr := 0;  BxOpcodeInfoG2Ed[5].ExecutePtr:=bx_cpu.SHR_Ed;
   BxOpcodeInfoG2Ed[6].Attr := 0;  BxOpcodeInfoG2Ed[6].ExecutePtr:=bx_cpu.SHL_Ed;
   BxOpcodeInfoG2Ed[7].Attr := 0;  BxOpcodeInfoG2Ed[7].ExecutePtr:=bx_cpu.SAR_Ed;

   BxOpcodeInfoG3Eb[0].Attr := BxImmediate_Ib; BxOpcodeInfoG3Eb[0].ExecutePtr:=bx_cpu.TEST_EbIb;
   BxOpcodeInfoG3Eb[1].Attr := BxImmediate_Ib; BxOpcodeInfoG3Eb[1].ExecutePtr:=bx_cpu.TEST_EbIb;
   BxOpcodeInfoG3Eb[2].Attr := 0;              BxOpcodeInfoG3Eb[2].ExecutePtr:=bx_cpu.NOT_Eb;
   BxOpcodeInfoG3Eb[3].Attr := 0;              BxOpcodeInfoG3Eb[3].ExecutePtr:=bx_cpu.NEG_Eb;
   BxOpcodeInfoG3Eb[4].Attr := 0;              BxOpcodeInfoG3Eb[4].ExecutePtr:=bx_cpu.MUL_ALEb;
   BxOpcodeInfoG3Eb[5].Attr := 0;              BxOpcodeInfoG3Eb[5].ExecutePtr:=bx_cpu.IMUL_ALEb;
   BxOpcodeInfoG3Eb[6].Attr := 0;              BxOpcodeInfoG3Eb[6].ExecutePtr:=bx_cpu.DIV_ALEb;
   BxOpcodeInfoG3Eb[7].Attr := 0;              BxOpcodeInfoG3Eb[7].ExecutePtr:=bx_cpu.IDIV_ALEb;

   BxOpcodeInfoG3Ew[0].Attr:=BxImmediate_Iw;  BxOpcodeInfoG3Ew[0].ExecutePtr:=bx_cpu.TEST_EwIw;
   BxOpcodeInfoG3Ew[1].Attr:=BxImmediate_Iw;  BxOpcodeInfoG3Ew[1].ExecutePtr:=bx_cpu.TEST_EwIw;
   BxOpcodeInfoG3Ew[2].Attr:=0;       BxOpcodeInfoG3Ew[2].ExecutePtr:=bx_cpu.NOT_Ew;
   BxOpcodeInfoG3Ew[3].Attr:=0;       BxOpcodeInfoG3Ew[3].ExecutePtr:=bx_cpu.NEG_Ew;
   BxOpcodeInfoG3Ew[4].Attr:=0;       BxOpcodeInfoG3Ew[4].ExecutePtr:=bx_cpu.MUL_AXEw;
   BxOpcodeInfoG3Ew[5].Attr:=0;       BxOpcodeInfoG3Ew[5].ExecutePtr:=bx_cpu.IMUL_AXEw;
   BxOpcodeInfoG3Ew[6].Attr:=0;       BxOpcodeInfoG3Ew[6].ExecutePtr:=bx_cpu.DIV_AXEw;
   BxOpcodeInfoG3Ew[7].Attr:=0;       BxOpcodeInfoG3Ew[7].ExecutePtr:=bx_cpu.IDIV_AXEw;

   BxOpcodeInfoG3Ed[0].Attr:=BxImmediate_Iv;  BxOpcodeInfoG3Ed[0].ExecutePtr:=bx_cpu.TEST_EdId;
   BxOpcodeInfoG3Ed[1].Attr:=BxImmediate_Iv;  BxOpcodeInfoG3Ed[1].ExecutePtr:=bx_cpu.TEST_EdId;
   BxOpcodeInfoG3Ed[2].Attr:=0;             BxOpcodeInfoG3Ed[2].ExecutePtr:=bx_cpu.NOT_Ed;
   BxOpcodeInfoG3Ed[3].Attr:=0;             BxOpcodeInfoG3Ed[3].ExecutePtr:=bx_cpu.NEG_Ed;
   BxOpcodeInfoG3Ed[4].Attr:=0;             BxOpcodeInfoG3Ed[4].ExecutePtr:=bx_cpu.MUL_EAXEd;
   BxOpcodeInfoG3Ed[5].Attr:=0;             BxOpcodeInfoG3Ed[5].ExecutePtr:=bx_cpu.IMUL_EAXEd;
   BxOpcodeInfoG3Ed[6].Attr:=0;             BxOpcodeInfoG3Ed[6].ExecutePtr:=bx_cpu.DIV_EAXEd;
   BxOpcodeInfoG3Ed[7].Attr:=0;             BxOpcodeInfoG3Ed[7].ExecutePtr:=bx_cpu.IDIV_EAXEd;

   BxOpcodeInfoG4[0].Attr:=0;  BxOpcodeInfoG4[0].ExecutePtr:=bx_cpu.INC_Eb;
   BxOpcodeInfoG4[1].Attr:=0;  BxOpcodeInfoG4[1].ExecutePtr:=bx_cpu.DEC_Eb;
   BxOpcodeInfoG4[2].Attr:=0;  BxOpcodeInfoG4[2].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG4[3].Attr:=0;  BxOpcodeInfoG4[3].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG4[4].Attr:=0;  BxOpcodeInfoG4[4].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG4[5].Attr:=0;  BxOpcodeInfoG4[5].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG4[6].Attr:=0;  BxOpcodeInfoG4[6].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG4[7].Attr:=0;  BxOpcodeInfoG4[7].ExecutePtr:=bx_cpu.BxError;

  // attributes defined in main area
   BxOpcodeInfoG5w[0].Attr:=0;  BxOpcodeInfoG5w[0].ExecutePtr:=bx_cpu.INC_Ew;
   BxOpcodeInfoG5w[1].Attr:=0;  BxOpcodeInfoG5w[1].ExecutePtr:=bx_cpu.DEC_Ew;
   BxOpcodeInfoG5w[2].Attr:=0;  BxOpcodeInfoG5w[2].ExecutePtr:=bx_cpu.CALL_Ew;
   BxOpcodeInfoG5w[3].Attr:=0;  BxOpcodeInfoG5w[3].ExecutePtr:=bx_cpu.CALL16_Ep;
   BxOpcodeInfoG5w[4].Attr:=0;  BxOpcodeInfoG5w[4].ExecutePtr:=bx_cpu.JMP_Ew;
   BxOpcodeInfoG5w[5].Attr:=0;  BxOpcodeInfoG5w[5].ExecutePtr:=bx_cpu.JMP16_Ep;
   BxOpcodeInfoG5w[6].Attr:=0;  BxOpcodeInfoG5w[6].ExecutePtr:=bx_cpu.PUSH_Ew;
   BxOpcodeInfoG5w[7].Attr:=0;  BxOpcodeInfoG5w[7].ExecutePtr:=bx_cpu.BxError;

  // attributes defined in main area
   BxOpcodeInfoG5d[0].Attr:=0;  BxOpcodeInfoG5d[0].ExecutePtr:=bx_cpu.INC_Ed;
   BxOpcodeInfoG5d[1].Attr:=0;  BxOpcodeInfoG5d[1].ExecutePtr:=bx_cpu.DEC_Ed;
   BxOpcodeInfoG5d[2].Attr:=0;  BxOpcodeInfoG5d[2].ExecutePtr:=bx_cpu.CALL_Ed;
   BxOpcodeInfoG5d[3].Attr:=0;  BxOpcodeInfoG5d[3].ExecutePtr:=bx_cpu.CALL32_Ep;
   BxOpcodeInfoG5d[4].Attr:=0;  BxOpcodeInfoG5d[4].ExecutePtr:=bx_cpu.JMP_Ed;
   BxOpcodeInfoG5d[5].Attr:=0;  BxOpcodeInfoG5d[5].ExecutePtr:=bx_cpu.JMP32_Ep;
   BxOpcodeInfoG5d[6].Attr:=0;  BxOpcodeInfoG5d[6].ExecutePtr:=bx_cpu.PUSH_Ed;
   BxOpcodeInfoG5d[7].Attr:=0;  BxOpcodeInfoG5d[7].ExecutePtr:=bx_cpu.BxError;

  // attributes defined in main area
   BxOpcodeInfoG6[0].Attr:=0;  BxOpcodeInfoG6[00].ExecutePtr:=bx_cpu.SLDT_Ew;
   BxOpcodeInfoG6[1].Attr:=0;  BxOpcodeInfoG6[01].ExecutePtr:=bx_cpu.STR_Ew;
   BxOpcodeInfoG6[2].Attr:=0;  BxOpcodeInfoG6[02].ExecutePtr:=bx_cpu.LLDT_Ew;
   BxOpcodeInfoG6[3].Attr:=0;  BxOpcodeInfoG6[03].ExecutePtr:=bx_cpu.LTR_Ew;
   BxOpcodeInfoG6[4].Attr:=0;  BxOpcodeInfoG6[04].ExecutePtr:=bx_cpu.VERR_Ew;
   BxOpcodeInfoG6[5].Attr:=0;  BxOpcodeInfoG6[05].ExecutePtr:=bx_cpu.VERW_Ew;
   BxOpcodeInfoG6[6].Attr:=0;  BxOpcodeInfoG6[06].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG6[7].Attr:=0;  BxOpcodeInfoG6[07].ExecutePtr:=bx_cpu.BxError;

   BxOpcodeInfoG7[0].Attr:=0;  BxOpcodeInfoG7[00].ExecutePtr:=bx_cpu.SGDT_Ms;
   BxOpcodeInfoG7[1].Attr:=0;  BxOpcodeInfoG7[01].ExecutePtr:=bx_cpu.SIDT_Ms;
   BxOpcodeInfoG7[2].Attr:=0;  BxOpcodeInfoG7[02].ExecutePtr:=bx_cpu.LGDT_Ms;
   BxOpcodeInfoG7[3].Attr:=0;  BxOpcodeInfoG7[03].ExecutePtr:=bx_cpu.LIDT_Ms;
   BxOpcodeInfoG7[4].Attr:=0;  BxOpcodeInfoG7[04].ExecutePtr:=bx_cpu.SMSW_Ew;
   BxOpcodeInfoG7[5].Attr:=0;  BxOpcodeInfoG7[05].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG7[6].Attr:=0;  BxOpcodeInfoG7[06].ExecutePtr:=bx_cpu.LMSW_Ew;
   BxOpcodeInfoG7[7].Attr:=0;  BxOpcodeInfoG7[07].ExecutePtr:=bx_cpu.INVLPG;

   BxOpcodeInfoG8EvIb[00].Attr:=0;               BxOpcodeInfoG8EvIb[00].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG8EvIb[01].Attr:=0;               BxOpcodeInfoG8EvIb[01].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG8EvIb[02].Attr:=0;               BxOpcodeInfoG8EvIb[02].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG8EvIb[03].Attr:=0;               BxOpcodeInfoG8EvIb[03].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG8EvIb[04].Attr:=BxImmediate_Ib;  BxOpcodeInfoG8EvIb[04].ExecutePtr:=bx_cpu.BT_EvIb;
   BxOpcodeInfoG8EvIb[05].Attr:=BxImmediate_Ib;  BxOpcodeInfoG8EvIb[05].ExecutePtr:=bx_cpu.BTS_EvIb;
   BxOpcodeInfoG8EvIb[06].Attr:=BxImmediate_Ib;  BxOpcodeInfoG8EvIb[06].ExecutePtr:=bx_cpu.BTR_EvIb;
   BxOpcodeInfoG8EvIb[07].Attr:=BxImmediate_Ib;  BxOpcodeInfoG8EvIb[07].ExecutePtr:=bx_cpu.BTC_EvIb;

   BxOpcodeInfoG9[00].Attr:=0;  BxOpcodeInfoG9[00].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[01].Attr:=0;  BxOpcodeInfoG9[01].ExecutePtr:=bx_cpu.CMPXCHG8B;
   BxOpcodeInfoG9[02].Attr:=0;  BxOpcodeInfoG9[02].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[03].Attr:=0;  BxOpcodeInfoG9[03].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[04].Attr:=0;  BxOpcodeInfoG9[04].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[05].Attr:=0;  BxOpcodeInfoG9[05].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[06].Attr:=0;  BxOpcodeInfoG9[06].ExecutePtr:=bx_cpu.BxError;
   BxOpcodeInfoG9[07].Attr:=0;  BxOpcodeInfoG9[07].ExecutePtr:=bx_cpu.BxError;

  BxOpcodeInfo[$0].Attr:=BxAnother;  BxOpcodeInfo[$0].ExecutePtr:=bx_cpu.ADD_EbGb;
  BxOpcodeInfo[$1].Attr:=BxAnother;  BxOpcodeInfo[$1].ExecutePtr:=bx_cpu.ADD_EwGw;
  BxOpcodeInfo[$2].Attr:=BxAnother;  BxOpcodeInfo[$2].ExecutePtr:=bx_cpu.ADD_GbEb;
  BxOpcodeInfo[$3].Attr:=BxAnother;  BxOpcodeInfo[$3].ExecutePtr:=bx_cpu.ADD_GwEw;
  BxOpcodeInfo[$4].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$4].ExecutePtr:=bx_cpu.ADD_ALIb;
  BxOpcodeInfo[$5].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$5].ExecutePtr:=bx_cpu.ADD_AXIw;
  BxOpcodeInfo[$6].Attr:=0;  BxOpcodeInfo[$6].ExecutePtr:=bx_cpu.PUSH_ES;
  BxOpcodeInfo[$7].Attr:=0;  BxOpcodeInfo[$7].ExecutePtr:=bx_cpu.POP_ES;
  BxOpcodeInfo[$8].Attr:=BxAnother;  BxOpcodeInfo[$8].ExecutePtr:=bx_cpu.OR_EbGb;
  BxOpcodeInfo[$9].Attr:=BxAnother;  BxOpcodeInfo[$9].ExecutePtr:=bx_cpu.OR_EwGw;
  BxOpcodeInfo[$A].Attr:=BxAnother;  BxOpcodeInfo[$A].ExecutePtr:=bx_cpu.OR_GbEb;
  BxOpcodeInfo[$B].Attr:=BxAnother;  BxOpcodeInfo[$B].ExecutePtr:=bx_cpu.OR_GwEw;
  BxOpcodeInfo[$C].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$C].ExecutePtr:=bx_cpu.OR_ALIb;
  BxOpcodeInfo[$D].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$D].ExecutePtr:=bx_cpu.OR_AXIw;
  BxOpcodeInfo[$E].Attr:=0;  BxOpcodeInfo[$E].ExecutePtr:=bx_cpu.PUSH_CS;
  BxOpcodeInfo[$F].Attr:=BxAnother;  BxOpcodeInfo[$F].ExecutePtr:=bx_cpu.BxError; // 2-byte escape
  BxOpcodeInfo[$10].Attr:=BxAnother;  BxOpcodeInfo[$10].ExecutePtr:=bx_cpu.ADC_EbGb;
  BxOpcodeInfo[$11].Attr:=BxAnother;  BxOpcodeInfo[$11].ExecutePtr:=bx_cpu.ADC_EwGw;
  BxOpcodeInfo[$12].Attr:=BxAnother;  BxOpcodeInfo[$12].ExecutePtr:=bx_cpu.ADC_GbEb;
  BxOpcodeInfo[$13].Attr:=BxAnother;  BxOpcodeInfo[$13].ExecutePtr:=bx_cpu.ADC_GwEw;
  BxOpcodeInfo[$14].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$14].ExecutePtr:=bx_cpu.ADC_ALIb;
  BxOpcodeInfo[$15].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$15].ExecutePtr:=bx_cpu.ADC_AXIw;
  BxOpcodeInfo[$16].Attr:=0;  BxOpcodeInfo[$16].ExecutePtr:=bx_cpu.PUSH_SS;
  BxOpcodeInfo[$17].Attr:=0;  BxOpcodeInfo[$17].ExecutePtr:=bx_cpu.POP_SS;
  BxOpcodeInfo[$18].Attr:=BxAnother;  BxOpcodeInfo[$18].ExecutePtr:=bx_cpu.SBB_EbGb;
  BxOpcodeInfo[$19].Attr:=BxAnother;  BxOpcodeInfo[$19].ExecutePtr:=bx_cpu.SBB_EwGw;
  BxOpcodeInfo[$1A].Attr:=BxAnother;  BxOpcodeInfo[$1A].ExecutePtr:=bx_cpu.SBB_GbEb;
  BxOpcodeInfo[$1B].Attr:=BxAnother;  BxOpcodeInfo[$1B].ExecutePtr:=bx_cpu.SBB_GwEw;
  BxOpcodeInfo[$1C].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$1C].ExecutePtr:=bx_cpu.SBB_ALIb;
  BxOpcodeInfo[$1D].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$1D].ExecutePtr:=bx_cpu.SBB_AXIw;
  BxOpcodeInfo[$1E].Attr:=0;  BxOpcodeInfo[$1E].ExecutePtr:=bx_cpu.PUSH_DS;
  BxOpcodeInfo[$1F].Attr:=0;  BxOpcodeInfo[$1F].ExecutePtr:=bx_cpu.POP_DS;
  BxOpcodeInfo[$20].Attr:=BxAnother;  BxOpcodeInfo[$20].ExecutePtr:=bx_cpu.AND_EbGb;
  BxOpcodeInfo[$21].Attr:=BxAnother;  BxOpcodeInfo[$21].ExecutePtr:=bx_cpu.AND_EwGw;
  BxOpcodeInfo[$22].Attr:=BxAnother;  BxOpcodeInfo[$22].ExecutePtr:=bx_cpu.AND_GbEb;
  BxOpcodeInfo[$23].Attr:=BxAnother;  BxOpcodeInfo[$23].ExecutePtr:=bx_cpu.AND_GwEw;
  BxOpcodeInfo[$24].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$24].ExecutePtr:=bx_cpu.AND_ALIb;
  BxOpcodeInfo[$25].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$25].ExecutePtr:=bx_cpu.AND_AXIw;
  BxOpcodeInfo[$26].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$26].ExecutePtr:=bx_cpu.BxError; // ES:
  BxOpcodeInfo[$27].Attr:=0;  BxOpcodeInfo[$27].ExecutePtr:=bx_cpu.DAA;
  BxOpcodeInfo[$28].Attr:=BxAnother;  BxOpcodeInfo[$28].ExecutePtr:=bx_cpu.SUB_EbGb;
  BxOpcodeInfo[$29].Attr:=BxAnother;  BxOpcodeInfo[$29].ExecutePtr:=bx_cpu.SUB_EwGw;
  BxOpcodeInfo[$2A].Attr:=BxAnother;  BxOpcodeInfo[$2A].ExecutePtr:=bx_cpu.SUB_GbEb;
  BxOpcodeInfo[$2B].Attr:=BxAnother;  BxOpcodeInfo[$2B].ExecutePtr:=bx_cpu.SUB_GwEw;
  BxOpcodeInfo[$2C].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$2C].ExecutePtr:=bx_cpu.SUB_ALIb;
  BxOpcodeInfo[$2D].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$2D].ExecutePtr:=bx_cpu.SUB_AXIw;
  BxOpcodeInfo[$2E].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$2E].ExecutePtr:=bx_cpu.BxError; // CS:
  BxOpcodeInfo[$2F].Attr:=0;  BxOpcodeInfo[$2F].ExecutePtr:=bx_cpu.DAS;
  BxOpcodeInfo[$30].Attr:=BxAnother;  BxOpcodeInfo[$30].ExecutePtr:=bx_cpu.XOR_EbGb;
  BxOpcodeInfo[$31].Attr:=BxAnother;  BxOpcodeInfo[$31].ExecutePtr:=bx_cpu.XOR_EwGw;
  BxOpcodeInfo[$32].Attr:=BxAnother;  BxOpcodeInfo[$32].ExecutePtr:=bx_cpu.XOR_GbEb;
  BxOpcodeInfo[$33].Attr:=BxAnother;  BxOpcodeInfo[$33].ExecutePtr:=bx_cpu.XOR_GwEw;
  BxOpcodeInfo[$34].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$34].ExecutePtr:=bx_cpu.XOR_ALIb;
  BxOpcodeInfo[$35].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$35].ExecutePtr:=bx_cpu.XOR_AXIw;
  BxOpcodeInfo[$36].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$36].ExecutePtr:=bx_cpu.BxError; // SS:
  BxOpcodeInfo[$37].Attr:=0;  BxOpcodeInfo[$37].ExecutePtr:=bx_cpu.AAA;
  BxOpcodeInfo[$38].Attr:=BxAnother;  BxOpcodeInfo[$38].ExecutePtr:=bx_cpu.CMP_EbGb;
  BxOpcodeInfo[$39].Attr:=BxAnother;  BxOpcodeInfo[$39].ExecutePtr:=bx_cpu.CMP_EwGw;
  BxOpcodeInfo[$3A].Attr:=BxAnother;  BxOpcodeInfo[$3A].ExecutePtr:=bx_cpu.CMP_GbEb;
  BxOpcodeInfo[$3B].Attr:=BxAnother;  BxOpcodeInfo[$3B].ExecutePtr:=bx_cpu.CMP_GwEw;
  BxOpcodeInfo[$3C].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$3C].ExecutePtr:=bx_cpu.CMP_ALIb;
  BxOpcodeInfo[$3D].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$3D].ExecutePtr:=bx_cpu.CMP_AXIw;
  BxOpcodeInfo[$3E].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$3E].ExecutePtr:=bx_cpu.BxError; // DS:
  BxOpcodeInfo[$3F].Attr:=0;  BxOpcodeInfo[$3F].ExecutePtr:=bx_cpu.AAS;
  BxOpcodeInfo[$40].Attr:=0;  BxOpcodeInfo[$40].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$41].Attr:=0;  BxOpcodeInfo[$41].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$42].Attr:=0;  BxOpcodeInfo[$42].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$43].Attr:=0;  BxOpcodeInfo[$43].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$44].Attr:=0;  BxOpcodeInfo[$44].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$45].Attr:=0;  BxOpcodeInfo[$45].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$46].Attr:=0;  BxOpcodeInfo[$46].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$47].Attr:=0;  BxOpcodeInfo[$47].ExecutePtr:=bx_cpu.INC_RX;
  BxOpcodeInfo[$48].Attr:=0;  BxOpcodeInfo[$48].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$49].Attr:=0;  BxOpcodeInfo[$49].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4A].Attr:=0;  BxOpcodeInfo[$4A].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4B].Attr:=0;  BxOpcodeInfo[$4B].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4C].Attr:=0;  BxOpcodeInfo[$4C].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4D].Attr:=0;  BxOpcodeInfo[$4D].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4E].Attr:=0;  BxOpcodeInfo[$4E].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$4F].Attr:=0;  BxOpcodeInfo[$4F].ExecutePtr:=bx_cpu.DEC_RX;
  BxOpcodeInfo[$50].Attr:=0;  BxOpcodeInfo[$50].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$51].Attr:=0;  BxOpcodeInfo[$51].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$52].Attr:=0;  BxOpcodeInfo[$52].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$53].Attr:=0;  BxOpcodeInfo[$53].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$54].Attr:=0;  BxOpcodeInfo[$54].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$55].Attr:=0;  BxOpcodeInfo[$55].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$56].Attr:=0;  BxOpcodeInfo[$56].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$57].Attr:=0;  BxOpcodeInfo[$57].ExecutePtr:=bx_cpu.PUSH_RX;
  BxOpcodeInfo[$58].Attr:=0;  BxOpcodeInfo[$58].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$59].Attr:=0;  BxOpcodeInfo[$59].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5A].Attr:=0;  BxOpcodeInfo[$5A].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5B].Attr:=0;  BxOpcodeInfo[$5B].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5C].Attr:=0;  BxOpcodeInfo[$5C].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5D].Attr:=0;  BxOpcodeInfo[$5D].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5E].Attr:=0;  BxOpcodeInfo[$5E].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$5F].Attr:=0;  BxOpcodeInfo[$5F].ExecutePtr:=bx_cpu.POP_RX;
  BxOpcodeInfo[$60].Attr:=0;  BxOpcodeInfo[$60].ExecutePtr:=bx_cpu.PUSHAD16;
  BxOpcodeInfo[$61].Attr:=0;  BxOpcodeInfo[$61].ExecutePtr:=bx_cpu.POPAD16;
  BxOpcodeInfo[$62].Attr:=BxAnother;  BxOpcodeInfo[$62].ExecutePtr:=bx_cpu.BOUND_GvMa;
  BxOpcodeInfo[$63].Attr:=BxAnother;  BxOpcodeInfo[$63].ExecutePtr:=bx_cpu.ARPL_EwGw;
  BxOpcodeInfo[$64].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$64].ExecutePtr:=bx_cpu.BxError; // FS:
  BxOpcodeInfo[$65].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$65].ExecutePtr:=bx_cpu.BxError; // GS:
  BxOpcodeInfo[$66].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$66].ExecutePtr:=bx_cpu.BxError; // OS:
  BxOpcodeInfo[$67].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$67].ExecutePtr:=bx_cpu.BxError; // AS:
  BxOpcodeInfo[$68].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$68].ExecutePtr:=bx_cpu.PUSH_Iw;
  BxOpcodeInfo[$69].Attr:=BxAnother or BxImmediate_Iv;  BxOpcodeInfo[$69].ExecutePtr:=bx_cpu.IMUL_GwEwIw;
  BxOpcodeInfo[$6A].Attr:=BxImmediate_Ib_SE;  BxOpcodeInfo[$6A].ExecutePtr:=bx_cpu.PUSH_Iw;
  BxOpcodeInfo[$6B].Attr:=BxAnother or BxImmediate_Ib_SE;  BxOpcodeInfo[$6B].ExecutePtr:=bx_cpu.IMUL_GwEwIw;
  BxOpcodeInfo[$6C].Attr:=BxRepeatable;  BxOpcodeInfo[$6C].ExecutePtr:=bx_cpu.INSB_YbDX;
  BxOpcodeInfo[$6D].Attr:=BxRepeatable;  BxOpcodeInfo[$6D].ExecutePtr:=bx_cpu.INSW_YvDX;
  BxOpcodeInfo[$6E].Attr:=BxRepeatable;  BxOpcodeInfo[$6E].ExecutePtr:=bx_cpu.OUTSB_DXXb;
  BxOpcodeInfo[$6F].Attr:=BxRepeatable;  BxOpcodeInfo[$6F].ExecutePtr:=bx_cpu.OUTSW_DXXv;
  BxOpcodeInfo[$70].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$70].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$71].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$71].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$72].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$72].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$73].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$73].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$74].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$74].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$75].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$75].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$76].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$76].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$77].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$77].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$78].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$78].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$79].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$79].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7A].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7A].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7B].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7B].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7C].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7C].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7D].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7D].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7E].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7E].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$7F].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$7F].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$80].Attr:=BxAnother or BxGroup1; BxOpcodeInfo[$80].ExecutePtr:=nil; BxOpcodeInfo[$80].AnotherArray:=@BxOpcodeInfoG1EbIb;
  BxOpcodeInfo[$81].Attr:=BxAnother or BxGroup1 or BxImmediate_Iv; BxOpcodeInfo[$81].ExecutePtr:=nil; BxOpcodeInfo[$81].AnotherArray:=@BxOpcodeInfoG1Ew;
  BxOpcodeInfo[$82].Attr:=BxAnother or BxGroup1;  BxOpcodeInfo[$82].ExecutePtr:=nil; BxOpcodeInfo[$82].AnotherArray:=@BxOpcodeInfoG1EbIb;
  BxOpcodeInfo[$83].Attr:=BxAnother or BxGroup1 or BxImmediate_Ib_SE; BxOpcodeInfo[$83].ExecutePtr:=nil; BxOpcodeInfo[$83].AnotherArray:=@BxOpcodeInfoG1Ew;
  BxOpcodeInfo[$84].Attr:=BxAnother;  BxOpcodeInfo[$84].ExecutePtr:=bx_cpu.TEST_EbGb;
  BxOpcodeInfo[$85].Attr:=BxAnother;  BxOpcodeInfo[$85].ExecutePtr:=bx_cpu.TEST_EwGw;
  BxOpcodeInfo[$86].Attr:=BxAnother;  BxOpcodeInfo[$86].ExecutePtr:=bx_cpu.XCHG_EbGb;
  BxOpcodeInfo[$87].Attr:=BxAnother;  BxOpcodeInfo[$87].ExecutePtr:=bx_cpu.XCHG_EwGw;
  BxOpcodeInfo[$88].Attr:=BxAnother;  BxOpcodeInfo[$88].ExecutePtr:=bx_cpu.MOV_EbGb;
  BxOpcodeInfo[$89].Attr:=BxAnother;  BxOpcodeInfo[$89].ExecutePtr:=bx_cpu.MOV_EwGw;
  BxOpcodeInfo[$8A].Attr:=BxAnother;  BxOpcodeInfo[$8A].ExecutePtr:=bx_cpu.MOV_GbEb;
  BxOpcodeInfo[$8B].Attr:=BxAnother;  BxOpcodeInfo[$8B].ExecutePtr:=bx_cpu.MOV_GwEw;
  BxOpcodeInfo[$8C].Attr:=BxAnother;  BxOpcodeInfo[$8C].ExecutePtr:=bx_cpu.MOV_EwSw;
  BxOpcodeInfo[$8D].Attr:=BxAnother;  BxOpcodeInfo[$8D].ExecutePtr:=bx_cpu.LEA_GwM;
  BxOpcodeInfo[$8E].Attr:=BxAnother;  BxOpcodeInfo[$8E].ExecutePtr:=bx_cpu.MOV_SwEw;
  BxOpcodeInfo[$8F].Attr:=BxAnother;  BxOpcodeInfo[$8F].ExecutePtr:=bx_cpu.POP_Ew;
  BxOpcodeInfo[$90].Attr:=0;  BxOpcodeInfo[$90].ExecutePtr:=bx_cpu.NOP;
  BxOpcodeInfo[$91].Attr:=0;  BxOpcodeInfo[$91].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$92].Attr:=0;  BxOpcodeInfo[$92].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$93].Attr:=0;  BxOpcodeInfo[$93].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$94].Attr:=0;  BxOpcodeInfo[$94].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$95].Attr:=0;  BxOpcodeInfo[$95].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$96].Attr:=0;  BxOpcodeInfo[$96].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$97].Attr:=0;  BxOpcodeInfo[$97].ExecutePtr:=bx_cpu.XCHG_RXAX;
  BxOpcodeInfo[$98].Attr:=0;  BxOpcodeInfo[$98].ExecutePtr:=bx_cpu.CBW;
  BxOpcodeInfo[$99].Attr:=0;  BxOpcodeInfo[$99].ExecutePtr:=bx_cpu.CWD;
  BxOpcodeInfo[$9A].Attr:=BxImmediate_IvIw;  BxOpcodeInfo[$9A].ExecutePtr:=bx_cpu.CALL16_Ap;
  BxOpcodeInfo[$9B].Attr:=0;  BxOpcodeInfo[$9B].ExecutePtr:=bx_cpu.FWAIT;
  BxOpcodeInfo[$9C].Attr:=0;  BxOpcodeInfo[$9C].ExecutePtr:=bx_cpu.PUSHF_Fv;
  BxOpcodeInfo[$9D].Attr:=0;  BxOpcodeInfo[$9D].ExecutePtr:=bx_cpu.POPF_Fv;
  BxOpcodeInfo[$9E].Attr:=0;  BxOpcodeInfo[$9E].ExecutePtr:=bx_cpu.SAHF;
  BxOpcodeInfo[$9F].Attr:=0;  BxOpcodeInfo[$9F].ExecutePtr:=bx_cpu.LAHF;
  BxOpcodeInfo[$A0].Attr:=BxImmediate_O;  BxOpcodeInfo[$A0].ExecutePtr:=bx_cpu.MOV_ALOb;
  BxOpcodeInfo[$A1].Attr:=BxImmediate_O;  BxOpcodeInfo[$A1].ExecutePtr:=bx_cpu.MOV_AXOw;
  BxOpcodeInfo[$A2].Attr:=BxImmediate_O;  BxOpcodeInfo[$A2].ExecutePtr:=bx_cpu.MOV_ObAL;
  BxOpcodeInfo[$A3].Attr:=BxImmediate_O;  BxOpcodeInfo[$A3].ExecutePtr:=bx_cpu.MOV_OwAX;
  BxOpcodeInfo[$A4].Attr:=BxRepeatable;  BxOpcodeInfo[$A4].ExecutePtr:=bx_cpu.MOVSB_XbYb;
  BxOpcodeInfo[$A5].Attr:=BxRepeatable;  BxOpcodeInfo[$A5].ExecutePtr:=bx_cpu.MOVSW_XvYv;
  BxOpcodeInfo[$A6].Attr:=BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$A6].ExecutePtr:=bx_cpu.CMPSB_XbYb;
  BxOpcodeInfo[$A7].Attr:=BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$A7].ExecutePtr:=bx_cpu.CMPSW_XvYv;
  BxOpcodeInfo[$A8].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$A8].ExecutePtr:=bx_cpu.TEST_ALIb;
  BxOpcodeInfo[$A9].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$A9].ExecutePtr:=bx_cpu.TEST_AXIw;
  BxOpcodeInfo[$AA].Attr:=BxRepeatable;  BxOpcodeInfo[$AA].ExecutePtr:=bx_cpu.STOSB_YbAL;
  BxOpcodeInfo[$AB].Attr:=BxRepeatable;  BxOpcodeInfo[$AB].ExecutePtr:=bx_cpu.STOSW_YveAX;
  BxOpcodeInfo[$AC].Attr:=BxRepeatable;  BxOpcodeInfo[$AC].ExecutePtr:=bx_cpu.LODSB_ALXb;
  BxOpcodeInfo[$AD].Attr:=BxRepeatable;  BxOpcodeInfo[$AD].ExecutePtr:=bx_cpu.LODSW_eAXXv;
  BxOpcodeInfo[$AE].Attr:=BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$AE].ExecutePtr:=bx_cpu.SCASB_ALXb;
  BxOpcodeInfo[$AF].Attr:=BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$AF].ExecutePtr:=bx_cpu.SCASW_eAXXv;
  BxOpcodeInfo[$B0].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B0].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$B1].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B1].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$B2].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B2].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$B3].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B3].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$B4].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B4].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$B5].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B5].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$B6].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B6].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$B7].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$B7].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$B8].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$B8].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$B9].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$B9].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BA].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BA].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BB].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BB].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BC].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BC].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BD].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BD].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BE].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BE].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$BF].Attr:=BxImmediate_Iv;  BxOpcodeInfo[$BF].ExecutePtr:=bx_cpu.MOV_RXIw;
  BxOpcodeInfo[$C0].Attr:=BxAnother or BxGroup2 or BxImmediate_Ib; BxOpcodeInfo[$C0].ExecutePtr:=nil; BxOpcodeInfo[$C0].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$C1].Attr:=BxAnother or BxGroup2 or BxImmediate_Ib; BxOpcodeInfo[$C1].ExecutePtr:=nil; BxOpcodeInfo[$C1].AnotherArray:=@BxOpcodeInfoG2Ew;
  BxOpcodeInfo[$C2].Attr:=BxImmediate_Iw;  BxOpcodeInfo[$C2].ExecutePtr:=bx_cpu.RETnear16_Iw;
  BxOpcodeInfo[$C3].Attr:=0;             BxOpcodeInfo[$C3].ExecutePtr:=bx_cpu.RETnear16;
  BxOpcodeInfo[$C4].Attr:=BxAnother;  BxOpcodeInfo[$C4].ExecutePtr:=bx_cpu.LES_GvMp;
  BxOpcodeInfo[$C5].Attr:=BxAnother;  BxOpcodeInfo[$C5].ExecutePtr:=bx_cpu.LDS_GvMp;
  BxOpcodeInfo[$C6].Attr:=BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$C6].ExecutePtr:=bx_cpu.MOV_EbIb;
  BxOpcodeInfo[$C7].Attr:=BxAnother or BxImmediate_Iv;  BxOpcodeInfo[$C7].ExecutePtr:=bx_cpu.MOV_EwIw;
  BxOpcodeInfo[$C8].Attr:=BxImmediate_IwIb;  BxOpcodeInfo[$C8].ExecutePtr:=bx_cpu.ENTER_IwIb;
  BxOpcodeInfo[$C9].Attr:=0;  BxOpcodeInfo[$C9].ExecutePtr:=bx_cpu.LEAVE;
  BxOpcodeInfo[$CA].Attr:=BxImmediate_Iw;  BxOpcodeInfo[$CA].ExecutePtr:=bx_cpu.RETfar16_Iw;
  BxOpcodeInfo[$CB].Attr:=0;  BxOpcodeInfo[$CB].ExecutePtr:=bx_cpu.RETfar16;
  BxOpcodeInfo[$CC].Attr:=0;  BxOpcodeInfo[$CC].ExecutePtr:=bx_cpu.INT3;
  BxOpcodeInfo[$CD].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$CD].ExecutePtr:=bx_cpu.INT_Ib;
  BxOpcodeInfo[$CE].Attr:=0;  BxOpcodeInfo[$CE].ExecutePtr:=bx_cpu.INTO;
  BxOpcodeInfo[$CF].Attr:=0;  BxOpcodeInfo[$CF].ExecutePtr:=bx_cpu.IRET16;
  BxOpcodeInfo[$D0].Attr:=BxAnother or BxGroup2;  BxOpcodeInfo[$D0].ExecutePtr:=nil; BxOpcodeInfo[$D0].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$D1].Attr:=BxAnother or BxGroup2;  BxOpcodeInfo[$D1].ExecutePtr:=nil; BxOpcodeInfo[$D1].AnotherArray:=@BxOpcodeInfoG2Ew;
  BxOpcodeInfo[$D2].Attr:=BxAnother or BxGroup2;  BxOpcodeInfo[$D2].ExecutePtr:=nil; BxOpcodeInfo[$D2].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$D3].Attr:=BxAnother or BxGroup2;  BxOpcodeInfo[$D3].ExecutePtr:=nil; BxOpcodeInfo[$D3].AnotherArray:=@BxOpcodeInfoG2Ew;
  BxOpcodeInfo[$D4].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$D4].ExecutePtr:=bx_cpu.AAM;
  BxOpcodeInfo[$D5].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$D5].ExecutePtr:=bx_cpu.AAD;
  BxOpcodeInfo[$D6].Attr:=0;  BxOpcodeInfo[$D6].ExecutePtr:=bx_cpu.SALC;
  BxOpcodeInfo[$D7].Attr:=0;  BxOpcodeInfo[$D7].ExecutePtr:=bx_cpu.XLAT;
  BxOpcodeInfo[$D8].Attr:=BxAnother;  BxOpcodeInfo[$D8].ExecutePtr:=bx_cpu.ESC0;
  BxOpcodeInfo[$D9].Attr:=BxAnother;  BxOpcodeInfo[$D9].ExecutePtr:=bx_cpu.ESC1;
  BxOpcodeInfo[$DA].Attr:=BxAnother;  BxOpcodeInfo[$DA].ExecutePtr:=bx_cpu.ESC2;
  BxOpcodeInfo[$DB].Attr:=BxAnother;  BxOpcodeInfo[$DB].ExecutePtr:=bx_cpu.ESC3;
  BxOpcodeInfo[$DC].Attr:=BxAnother;  BxOpcodeInfo[$DC].ExecutePtr:=bx_cpu.ESC4;
  BxOpcodeInfo[$DD].Attr:=BxAnother;  BxOpcodeInfo[$DD].ExecutePtr:=bx_cpu.ESC5;
  BxOpcodeInfo[$DE].Attr:=BxAnother;  BxOpcodeInfo[$DE].ExecutePtr:=bx_cpu.ESC6;
  BxOpcodeInfo[$DF].Attr:=BxAnother;  BxOpcodeInfo[$DF].ExecutePtr:=bx_cpu.ESC7;
  BxOpcodeInfo[$E0].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$E0].ExecutePtr:=bx_cpu.LOOPNE_Jb;
  BxOpcodeInfo[$E1].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$E1].ExecutePtr:=bx_cpu.LOOPE_Jb;
  BxOpcodeInfo[$E2].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$E2].ExecutePtr:=bx_cpu.LOOP_Jb;
  BxOpcodeInfo[$E3].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$E3].ExecutePtr:=bx_cpu.JCXZ_Jb;
  BxOpcodeInfo[$E4].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$E4].ExecutePtr:=bx_cpu.IN_ALIb;
  BxOpcodeInfo[$E5].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$E5].ExecutePtr:=bx_cpu.IN_eAXIb;
  BxOpcodeInfo[$E6].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$E6].ExecutePtr:=bx_cpu.OUT_IbAL;
  BxOpcodeInfo[$E7].Attr:=BxImmediate_Ib;  BxOpcodeInfo[$E7].ExecutePtr:=bx_cpu.OUT_IbeAX;
  BxOpcodeInfo[$E8].Attr:=BxImmediate_BrOff16;  BxOpcodeInfo[$E8].ExecutePtr:=bx_cpu.CALL_Aw;
  BxOpcodeInfo[$E9].Attr:=BxImmediate_BrOff16;  BxOpcodeInfo[$E9].ExecutePtr:=bx_cpu.JMP_Jw;
  BxOpcodeInfo[$EA].Attr:=BxImmediate_IvIw;  BxOpcodeInfo[$EA].ExecutePtr:=bx_cpu.JMP_Ap;
  BxOpcodeInfo[$EB].Attr:=BxImmediate_BrOff8;  BxOpcodeInfo[$EB].ExecutePtr:=bx_cpu.JMP_Jw;
  BxOpcodeInfo[$EC].Attr:=0;  BxOpcodeInfo[$EC].ExecutePtr:=bx_cpu.IN_ALDX;
  BxOpcodeInfo[$ED].Attr:=0;  BxOpcodeInfo[$ED].ExecutePtr:=bx_cpu.IN_eAXDX;
  BxOpcodeInfo[$EE].Attr:=0;  BxOpcodeInfo[$EE].ExecutePtr:=bx_cpu.OUT_DXAL;
  BxOpcodeInfo[$EF].Attr:=0;  BxOpcodeInfo[$EF].ExecutePtr:=bx_cpu.OUT_DXeAX;
  BxOpcodeInfo[$F0].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$F0].ExecutePtr:=bx_cpu.BxError; // LOCK
  BxOpcodeInfo[$F1].Attr:=0;  BxOpcodeInfo[$F1].ExecutePtr:=bx_cpu.INT1;
  BxOpcodeInfo[$F2].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$F2].ExecutePtr:=bx_cpu.BxError; // REPNE/REPNZ
  BxOpcodeInfo[$F3].Attr:=BxPrefix or BxAnother;  BxOpcodeInfo[$F3].ExecutePtr:=bx_cpu.BxError; // REP, REPE/REPZ
  BxOpcodeInfo[$F4].Attr:=0;  BxOpcodeInfo[$F4].ExecutePtr:=bx_cpu.HLT;
  BxOpcodeInfo[$F5].Attr:=0;  BxOpcodeInfo[$F5].ExecutePtr:=bx_cpu.CMC;
  BxOpcodeInfo[$F6].Attr:=BxAnother or BxGroup3; BxOpcodeInfo[$F6].ExecutePtr:=nil; BxOpcodeInfo[$F6].AnotherArray:=@BxOpcodeInfoG3Eb;
  BxOpcodeInfo[$F7].Attr:=BxAnother or BxGroup3; BxOpcodeInfo[$F7].ExecutePtr:=nil; BxOpcodeInfo[$F7].AnotherArray:=@BxOpcodeInfoG3Ew;
  BxOpcodeInfo[$F8].Attr:=0;  BxOpcodeInfo[$F8].ExecutePtr:=bx_cpu.CLC;
  BxOpcodeInfo[$F9].Attr:=0;  BxOpcodeInfo[$F9].ExecutePtr:=bx_cpu.STC;
  BxOpcodeInfo[$FA].Attr:=0;  BxOpcodeInfo[$FA].ExecutePtr:=bx_cpu.CLI;
  BxOpcodeInfo[$FB].Attr:=0;  BxOpcodeInfo[$FB].ExecutePtr:=bx_cpu.STI;
  BxOpcodeInfo[$FC].Attr:=0;  BxOpcodeInfo[$FC].ExecutePtr:=bx_cpu.CLD;
  BxOpcodeInfo[$FD].Attr:=0;  BxOpcodeInfo[$FD].ExecutePtr:=bx_cpu.STD;
  BxOpcodeInfo[$FE].Attr:=BxAnother or BxGroup4;  BxOpcodeInfo[$FE].ExecutePtr:=nil; BxOpcodeInfo[$FE].AnotherArray:=@BxOpcodeInfoG4;
  BxOpcodeInfo[$FF].Attr:=BxAnother or BxGroup5;  BxOpcodeInfo[$FF].ExecutePtr:=nil; BxOpcodeInfo[$FF].AnotherArray:=@BxOpcodeInfoG5w;
  BxOpcodeInfo[$100].Attr :=BxAnother or BxGroup6;  BxOpcodeInfo[$100].ExecutePtr:=nil; BxOpcodeInfo[$100].AnotherArray:=@BxOpcodeInfoG6;
  BxOpcodeInfo[$101].Attr :=BxAnother or BxGroup7;  BxOpcodeInfo[$101].ExecutePtr:=nil; BxOpcodeInfo[$101].AnotherArray:=@BxOpcodeInfoG7;
  BxOpcodeInfo[$102].Attr :=BxAnother;  BxOpcodeInfo[$102].ExecutePtr:=bx_cpu.LAR_GvEw;
  BxOpcodeInfo[$103].Attr :=BxAnother;  BxOpcodeInfo[$103].ExecutePtr:=bx_cpu.LSL_GvEw;
  BxOpcodeInfo[$104].Attr := 0;  BxOpcodeInfo[$104].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$105].Attr := 0;  BxOpcodeInfo[$105].ExecutePtr:=bx_cpu.LOADALL;
  BxOpcodeInfo[$106].Attr := 0;  BxOpcodeInfo[$106].ExecutePtr:=bx_cpu.CLTS;
  BxOpcodeInfo[$107].Attr := 0;  BxOpcodeInfo[$107].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$108].Attr := 0;  BxOpcodeInfo[$108].ExecutePtr:=bx_cpu.INVD;
  BxOpcodeInfo[$109].Attr := 0;  BxOpcodeInfo[$109].ExecutePtr:=bx_cpu.WBINVD;
  BxOpcodeInfo[$10A].Attr := 0;  BxOpcodeInfo[$10A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$10B].Attr := 0;  BxOpcodeInfo[$10B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$10C].Attr := 0;  BxOpcodeInfo[$10C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$10D].Attr := 0;  BxOpcodeInfo[$10D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$10E].Attr := 0;  BxOpcodeInfo[$10E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$10F].Attr := 0;  BxOpcodeInfo[$10F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$110].Attr := 0;  BxOpcodeInfo[$110].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$111].Attr := 0;  BxOpcodeInfo[$111].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$112].Attr := 0;  BxOpcodeInfo[$112].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$113].Attr := 0;  BxOpcodeInfo[$113].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$114].Attr := 0;  BxOpcodeInfo[$114].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$115].Attr := 0;  BxOpcodeInfo[$115].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$116].Attr := 0;  BxOpcodeInfo[$116].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$117].Attr := 0;  BxOpcodeInfo[$117].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$118].Attr := 0;  BxOpcodeInfo[$118].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$119].Attr := 0;  BxOpcodeInfo[$119].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11A].Attr := 0;  BxOpcodeInfo[$11A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11B].Attr := 0;  BxOpcodeInfo[$11B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11C].Attr := 0;  BxOpcodeInfo[$11C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11D].Attr := 0;  BxOpcodeInfo[$11D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11E].Attr := 0;  BxOpcodeInfo[$11E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$11F].Attr := 0;  BxOpcodeInfo[$11F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$120].Attr := BxAnother;  BxOpcodeInfo[$120].ExecutePtr:=bx_cpu.MOV_RdCd;
  BxOpcodeInfo[$121].Attr := BxAnother;  BxOpcodeInfo[$121].ExecutePtr:=bx_cpu.MOV_RdDd;
  BxOpcodeInfo[$122].Attr := BxAnother;  BxOpcodeInfo[$122].ExecutePtr:=bx_cpu.MOV_CdRd;
  BxOpcodeInfo[$123].Attr := BxAnother;  BxOpcodeInfo[$123].ExecutePtr:=bx_cpu.MOV_DdRd;
  BxOpcodeInfo[$124].Attr := BxAnother;  BxOpcodeInfo[$124].ExecutePtr:=bx_cpu.MOV_RdTd;
  BxOpcodeInfo[$125].Attr := 0;  BxOpcodeInfo[$125].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$126].Attr := BxAnother;  BxOpcodeInfo[$126].ExecutePtr:=bx_cpu.MOV_TdRd;
  BxOpcodeInfo[$127].Attr := 0;  BxOpcodeInfo[$127].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$128].Attr := 0;  BxOpcodeInfo[$128].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$129].Attr := 0;  BxOpcodeInfo[$129].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12A].Attr := 0;  BxOpcodeInfo[$12A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12B].Attr := 0;  BxOpcodeInfo[$12B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12C].Attr := 0;  BxOpcodeInfo[$12C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12D].Attr := 0;  BxOpcodeInfo[$12D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12E].Attr := 0;  BxOpcodeInfo[$12E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$12F].Attr := 0;  BxOpcodeInfo[$12F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$130].Attr := 0;  BxOpcodeInfo[$130].ExecutePtr:=bx_cpu.WRMSR;
  BxOpcodeInfo[$131].Attr := 0;  BxOpcodeInfo[$131].ExecutePtr:=bx_cpu.RDTSC;
  BxOpcodeInfo[$132].Attr := 0;  BxOpcodeInfo[$132].ExecutePtr:=bx_cpu.RDMSR;
  BxOpcodeInfo[$133].Attr := 0;  BxOpcodeInfo[$133].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$134].Attr := 0;  BxOpcodeInfo[$134].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$135].Attr := 0;  BxOpcodeInfo[$135].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$136].Attr := 0;  BxOpcodeInfo[$136].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$137].Attr := 0;  BxOpcodeInfo[$137].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$138].Attr := 0;  BxOpcodeInfo[$138].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$139].Attr := 0;  BxOpcodeInfo[$139].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13A].Attr := 0;  BxOpcodeInfo[$13A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13B].Attr := 0;  BxOpcodeInfo[$13B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13C].Attr := 0;  BxOpcodeInfo[$13C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13D].Attr := 0;  BxOpcodeInfo[$13D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13E].Attr := 0;  BxOpcodeInfo[$13E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$13F].Attr := 0;  BxOpcodeInfo[$13F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$140].Attr := BxAnother;  BxOpcodeInfo[$140].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$141].Attr := BxAnother;  BxOpcodeInfo[$141].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$142].Attr := BxAnother;  BxOpcodeInfo[$142].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$143].Attr := BxAnother;  BxOpcodeInfo[$143].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$144].Attr := BxAnother;  BxOpcodeInfo[$144].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$145].Attr := BxAnother;  BxOpcodeInfo[$145].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$146].Attr := BxAnother;  BxOpcodeInfo[$146].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$147].Attr := BxAnother;  BxOpcodeInfo[$147].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$148].Attr := BxAnother;  BxOpcodeInfo[$148].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$149].Attr := BxAnother;  BxOpcodeInfo[$149].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14A].Attr := BxAnother;  BxOpcodeInfo[$14A].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14B].Attr := BxAnother;  BxOpcodeInfo[$14B].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14C].Attr := BxAnother;  BxOpcodeInfo[$14C].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14D].Attr := BxAnother;  BxOpcodeInfo[$14D].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14E].Attr := BxAnother;  BxOpcodeInfo[$14E].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$14F].Attr := BxAnother;  BxOpcodeInfo[$14F].ExecutePtr:=bx_cpu.CMOV_GwEw;
  BxOpcodeInfo[$150].Attr := 0;  BxOpcodeInfo[$150].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$151].Attr := 0;  BxOpcodeInfo[$151].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$152].Attr := 0;  BxOpcodeInfo[$152].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$153].Attr := 0;  BxOpcodeInfo[$153].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$154].Attr := 0;  BxOpcodeInfo[$154].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$155].Attr := 0;  BxOpcodeInfo[$155].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$156].Attr := 0;  BxOpcodeInfo[$156].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$157].Attr := 0;  BxOpcodeInfo[$157].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$158].Attr := 0;  BxOpcodeInfo[$158].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$159].Attr := 0;  BxOpcodeInfo[$159].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15A].Attr := 0;  BxOpcodeInfo[$15A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15B].Attr := 0;  BxOpcodeInfo[$15B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15C].Attr := 0;  BxOpcodeInfo[$15C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15D].Attr := 0;  BxOpcodeInfo[$15D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15E].Attr := 0;  BxOpcodeInfo[$15E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$15F].Attr := 0;  BxOpcodeInfo[$15F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$160].Attr := 0;  BxOpcodeInfo[$160].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$161].Attr := 0;  BxOpcodeInfo[$161].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$162].Attr := 0;  BxOpcodeInfo[$162].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$163].Attr := 0;  BxOpcodeInfo[$163].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$164].Attr := 0;  BxOpcodeInfo[$164].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$165].Attr := 0;  BxOpcodeInfo[$165].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$166].Attr := 0;  BxOpcodeInfo[$166].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$167].Attr := 0;  BxOpcodeInfo[$167].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$168].Attr := 0;  BxOpcodeInfo[$168].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$169].Attr := 0;  BxOpcodeInfo[$169].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16A].Attr := 0;  BxOpcodeInfo[$16A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16B].Attr := 0;  BxOpcodeInfo[$16B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16C].Attr := 0;  BxOpcodeInfo[$16C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16D].Attr := 0;  BxOpcodeInfo[$16D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16E].Attr := 0;  BxOpcodeInfo[$16E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$16F].Attr := 0;  BxOpcodeInfo[$16F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$170].Attr := 0;  BxOpcodeInfo[$170].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$171].Attr := 0;  BxOpcodeInfo[$171].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$172].Attr := 0;  BxOpcodeInfo[$172].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$173].Attr := 0;  BxOpcodeInfo[$173].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$174].Attr := 0;  BxOpcodeInfo[$174].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$175].Attr := 0;  BxOpcodeInfo[$175].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$176].Attr := 0;  BxOpcodeInfo[$176].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$177].Attr := 0;  BxOpcodeInfo[$177].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$178].Attr := 0;  BxOpcodeInfo[$178].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$179].Attr := 0;  BxOpcodeInfo[$179].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17A].Attr := 0;  BxOpcodeInfo[$17A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17B].Attr := 0;  BxOpcodeInfo[$17B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17C].Attr := 0;  BxOpcodeInfo[$17C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17D].Attr := 0;  BxOpcodeInfo[$17D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17E].Attr := 0;  BxOpcodeInfo[$17E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$17F].Attr := 0;  BxOpcodeInfo[$17F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$180].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$180].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$181].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$181].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$182].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$182].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$183].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$183].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$184].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$184].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$185].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$185].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$186].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$186].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$187].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$187].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$188].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$188].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$189].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$189].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18A].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18A].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18B].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18B].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18C].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18C].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18D].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18D].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18E].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18E].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$18F].Attr := BxImmediate_BrOff16;  BxOpcodeInfo[$18F].ExecutePtr:=bx_cpu.JCC_Jw;
  BxOpcodeInfo[$190].Attr := BxAnother;  BxOpcodeInfo[$190].ExecutePtr:=bx_cpu.SETO_Eb;
  BxOpcodeInfo[$191].Attr := BxAnother;  BxOpcodeInfo[$191].ExecutePtr:=bx_cpu.SETNO_Eb;
  BxOpcodeInfo[$192].Attr := BxAnother;  BxOpcodeInfo[$192].ExecutePtr:=bx_cpu.SETB_Eb;
  BxOpcodeInfo[$193].Attr := BxAnother;  BxOpcodeInfo[$193].ExecutePtr:=bx_cpu.SETNB_Eb;
  BxOpcodeInfo[$194].Attr := BxAnother;  BxOpcodeInfo[$194].ExecutePtr:=bx_cpu.SETZ_Eb;
  BxOpcodeInfo[$195].Attr := BxAnother;  BxOpcodeInfo[$195].ExecutePtr:=bx_cpu.SETNZ_Eb;
  BxOpcodeInfo[$196].Attr := BxAnother;  BxOpcodeInfo[$196].ExecutePtr:=bx_cpu.SETBE_Eb;
  BxOpcodeInfo[$197].Attr := BxAnother;  BxOpcodeInfo[$197].ExecutePtr:=bx_cpu.SETNBE_Eb;
  BxOpcodeInfo[$198].Attr := BxAnother;  BxOpcodeInfo[$198].ExecutePtr:=bx_cpu.SETS_Eb;
  BxOpcodeInfo[$199].Attr := BxAnother;  BxOpcodeInfo[$199].ExecutePtr:=bx_cpu.SETNS_Eb;
  BxOpcodeInfo[$19A].Attr := BxAnother;  BxOpcodeInfo[$19A].ExecutePtr:=bx_cpu.SETP_Eb;
  BxOpcodeInfo[$19B].Attr := BxAnother;  BxOpcodeInfo[$19B].ExecutePtr:=bx_cpu.SETNP_Eb;
  BxOpcodeInfo[$19C].Attr := BxAnother;  BxOpcodeInfo[$19C].ExecutePtr:=bx_cpu.SETL_Eb;
  BxOpcodeInfo[$19D].Attr := BxAnother;  BxOpcodeInfo[$19D].ExecutePtr:=bx_cpu.SETNL_Eb;
  BxOpcodeInfo[$19E].Attr := BxAnother;  BxOpcodeInfo[$19E].ExecutePtr:=bx_cpu.SETLE_Eb;
  BxOpcodeInfo[$19F].Attr := BxAnother;  BxOpcodeInfo[$19F].ExecutePtr:=bx_cpu.SETNLE_Eb;
  BxOpcodeInfo[$1A0].Attr := 0;  BxOpcodeInfo[$1A0].ExecutePtr:=bx_cpu.PUSH_FS;
  BxOpcodeInfo[$1A1].Attr := 0;  BxOpcodeInfo[$1A1].ExecutePtr:=bx_cpu.POP_FS;
  BxOpcodeInfo[$1A2].Attr := 0;  BxOpcodeInfo[$1A2].ExecutePtr:=bx_cpu.CPUID;
  BxOpcodeInfo[$1A3].Attr := BxAnother;  BxOpcodeInfo[$1A3].ExecutePtr:=bx_cpu.BT_EvGv;
  BxOpcodeInfo[$1A4].Attr := BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$1A4].ExecutePtr:=bx_cpu.SHLD_EwGw;
  BxOpcodeInfo[$1A5].Attr := BxAnother;                 BxOpcodeInfo[$1A5].ExecutePtr:=bx_cpu.SHLD_EwGw;
  BxOpcodeInfo[$1A6].Attr := 0;  BxOpcodeInfo[$1A6].ExecutePtr:=bx_cpu.CMPXCHG_XBTS;
  BxOpcodeInfo[$1A7].Attr := 0;  BxOpcodeInfo[$1A7].ExecutePtr:=bx_cpu.CMPXCHG_IBTS;
  BxOpcodeInfo[$1A8].Attr := 0;  BxOpcodeInfo[$1A8].ExecutePtr:=bx_cpu.PUSH_GS;
  BxOpcodeInfo[$1A9].Attr := 0;  BxOpcodeInfo[$1A9].ExecutePtr:=bx_cpu.POP_GS;
  BxOpcodeInfo[$1AA].Attr := 0;  BxOpcodeInfo[$1AA].ExecutePtr:=bx_cpu.RSM;
  BxOpcodeInfo[$1AB].Attr := BxAnother;  BxOpcodeInfo[$1AB].ExecutePtr:=bx_cpu.BTS_EvGv;
  BxOpcodeInfo[$1AC].Attr := BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$1AC].ExecutePtr:=bx_cpu.SHRD_EwGw;
  BxOpcodeInfo[$1AD].Attr := BxAnother;                 BxOpcodeInfo[$1AD].ExecutePtr:=bx_cpu.SHRD_EwGw;
  BxOpcodeInfo[$1AE].Attr := 0;  BxOpcodeInfo[$1AE].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1AF].Attr := BxAnother;  BxOpcodeInfo[$1AF].ExecutePtr:=bx_cpu.IMUL_GwEw;
  BxOpcodeInfo[$1B0].Attr := BxAnother;  BxOpcodeInfo[$1B0].ExecutePtr:=bx_cpu.CMPXCHG_EbGb;
  BxOpcodeInfo[$1B1].Attr := BxAnother;  BxOpcodeInfo[$1B1].ExecutePtr:=bx_cpu.CMPXCHG_EwGw;
  BxOpcodeInfo[$1B2].Attr := BxAnother;  BxOpcodeInfo[$1B2].ExecutePtr:=bx_cpu.LSS_GvMp;
  BxOpcodeInfo[$1B3].Attr := BxAnother;  BxOpcodeInfo[$1B3].ExecutePtr:=bx_cpu.BTR_EvGv;
  BxOpcodeInfo[$1B4].Attr := BxAnother;  BxOpcodeInfo[$1B4].ExecutePtr:=bx_cpu.LFS_GvMp;
  BxOpcodeInfo[$1B5].Attr := BxAnother;  BxOpcodeInfo[$1B5].ExecutePtr:=bx_cpu.LGS_GvMp;
  BxOpcodeInfo[$1B6].Attr := BxAnother;  BxOpcodeInfo[$1B6].ExecutePtr:=bx_cpu.MOVZX_GwEb;
  BxOpcodeInfo[$1B7].Attr := BxAnother;  BxOpcodeInfo[$1B7].ExecutePtr:=bx_cpu.MOVZX_GwEw;
  BxOpcodeInfo[$1B8].Attr := 0;  BxOpcodeInfo[$1B8].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1B9].Attr := 0;  BxOpcodeInfo[$1B9].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1BA].Attr := BxAnother or BxGroup8; BxOpcodeInfo[$1BA].ExecutePtr:=nil; BxOpcodeInfo[$1BA].AnotherArray:=@BxOpcodeInfoG8EvIb;
  BxOpcodeInfo[$1BB].Attr := BxAnother;  BxOpcodeInfo[$1BB].ExecutePtr:=bx_cpu.BTC_EvGv;
  BxOpcodeInfo[$1BC].Attr := BxAnother;  BxOpcodeInfo[$1BC].ExecutePtr:=bx_cpu.BSF_GvEv;
  BxOpcodeInfo[$1BD].Attr := BxAnother;  BxOpcodeInfo[$1BD].ExecutePtr:=bx_cpu.BSR_GvEv;
  BxOpcodeInfo[$1BE].Attr := BxAnother;  BxOpcodeInfo[$1BE].ExecutePtr:=bx_cpu.MOVSX_GwEb;
  BxOpcodeInfo[$1BF].Attr := BxAnother;  BxOpcodeInfo[$1BF].ExecutePtr:=bx_cpu.MOVSX_GwEw;
  BxOpcodeInfo[$1C0].Attr := BxAnother;  BxOpcodeInfo[$1C0].ExecutePtr:=bx_cpu.XADD_EbGb;
  BxOpcodeInfo[$1C1].Attr := BxAnother;  BxOpcodeInfo[$1C1].ExecutePtr:=bx_cpu.XADD_EwGw;
  BxOpcodeInfo[$1C2].Attr := 0;  BxOpcodeInfo[$1C2].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1C3].Attr := 0;  BxOpcodeInfo[$1C3].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1C4].Attr := 0;  BxOpcodeInfo[$1C4].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1C5].Attr := 0;  BxOpcodeInfo[$1C5].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1C6].Attr := 0;  BxOpcodeInfo[$1C6].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1C7].Attr := BxAnother or BxGroup9; BxOpcodeInfo[$1C7].ExecutePtr:=nil; BxOpcodeInfo[$1C7].AnotherArray:=@BxOpcodeInfoG9;
  BxOpcodeInfo[$1C8].Attr := 0;  BxOpcodeInfo[$1C8].ExecutePtr:=bx_cpu.BSWAP_EAX;
  BxOpcodeInfo[$1C9].Attr := 0;  BxOpcodeInfo[$1C9].ExecutePtr:=bx_cpu.BSWAP_ECX;
  BxOpcodeInfo[$1CA].Attr := 0;  BxOpcodeInfo[$1CA].ExecutePtr:=bx_cpu.BSWAP_EDX;
  BxOpcodeInfo[$1CB].Attr := 0;  BxOpcodeInfo[$1CB].ExecutePtr:=bx_cpu.BSWAP_EBX;
  BxOpcodeInfo[$1CC].Attr := 0;  BxOpcodeInfo[$1CC].ExecutePtr:=bx_cpu.BSWAP_ESP;
  BxOpcodeInfo[$1CD].Attr := 0;  BxOpcodeInfo[$1CD].ExecutePtr:=bx_cpu.BSWAP_EBP;
  BxOpcodeInfo[$1CE].Attr := 0;  BxOpcodeInfo[$1CE].ExecutePtr:=bx_cpu.BSWAP_ESI;
  BxOpcodeInfo[$1CF].Attr := 0;  BxOpcodeInfo[$1CF].ExecutePtr:=bx_cpu.BSWAP_EDI;
  BxOpcodeInfo[$1D0].Attr := 0;  BxOpcodeInfo[$1D0].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D1].Attr := 0;  BxOpcodeInfo[$1D1].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D2].Attr := 0;  BxOpcodeInfo[$1D2].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D3].Attr := 0;  BxOpcodeInfo[$1D3].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D4].Attr := 0;  BxOpcodeInfo[$1D4].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D5].Attr := 0;  BxOpcodeInfo[$1D5].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D6].Attr := 0;  BxOpcodeInfo[$1D6].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D7].Attr := 0;  BxOpcodeInfo[$1D7].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D8].Attr := 0;  BxOpcodeInfo[$1D8].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1D9].Attr := 0;  BxOpcodeInfo[$1D9].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DA].Attr := 0;  BxOpcodeInfo[$1DA].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DB].Attr := 0;  BxOpcodeInfo[$1DB].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DC].Attr := 0;  BxOpcodeInfo[$1DC].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DD].Attr := 0;  BxOpcodeInfo[$1DD].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DE].Attr := 0;  BxOpcodeInfo[$1DE].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1DF].Attr := 0;  BxOpcodeInfo[$1DF].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E0].Attr := 0;  BxOpcodeInfo[$1E0].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E1].Attr := 0;  BxOpcodeInfo[$1E1].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E2].Attr := 0;  BxOpcodeInfo[$1E2].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E3].Attr := 0;  BxOpcodeInfo[$1E3].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E4].Attr := 0;  BxOpcodeInfo[$1E4].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E5].Attr := 0;  BxOpcodeInfo[$1E5].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E6].Attr := 0;  BxOpcodeInfo[$1E6].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E7].Attr := 0;  BxOpcodeInfo[$1E7].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E8].Attr := 0;  BxOpcodeInfo[$1E8].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1E9].Attr := 0;  BxOpcodeInfo[$1E9].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1EA].Attr := 0;  BxOpcodeInfo[$1EA].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1EB].Attr := 0;  BxOpcodeInfo[$1EB].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1EC].Attr := 0;  BxOpcodeInfo[$1EC].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1ED].Attr := 0;  BxOpcodeInfo[$1ED].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1EE].Attr := 0;  BxOpcodeInfo[$1EE].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1EF].Attr := 0;  BxOpcodeInfo[$1EF].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$1F0].Attr := 0;  BxOpcodeInfo[$1F0].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F1].Attr := 0;  BxOpcodeInfo[$1F1].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F2].Attr := 0;  BxOpcodeInfo[$1F2].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F3].Attr := 0;  BxOpcodeInfo[$1F3].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F4].Attr := 0;  BxOpcodeInfo[$1F4].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F5].Attr := 0;  BxOpcodeInfo[$1F5].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F6].Attr := 0;  BxOpcodeInfo[$1F6].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F7].Attr := 0;  BxOpcodeInfo[$1F7].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F8].Attr := 0;  BxOpcodeInfo[$1F8].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1F9].Attr := 0;  BxOpcodeInfo[$1F9].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FA].Attr := 0;  BxOpcodeInfo[$1FA].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FB].Attr := 0;  BxOpcodeInfo[$1FB].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FC].Attr := 0;  BxOpcodeInfo[$1FC].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FD].Attr := 0;  BxOpcodeInfo[$1FD].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FE].Attr := 0;  BxOpcodeInfo[$1FE].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$1FF].Attr := 0;  BxOpcodeInfo[$1FF].ExecutePtr:=bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$200].Attr := BxAnother;  BxOpcodeInfo[$200].ExecutePtr:=bx_cpu.ADD_EbGb;
  BxOpcodeInfo[$201].Attr := BxAnother;  BxOpcodeInfo[$201].ExecutePtr:=bx_cpu.ADD_EdGd;
  BxOpcodeInfo[$202].Attr := BxAnother;  BxOpcodeInfo[$202].ExecutePtr:=bx_cpu.ADD_GbEb;
  BxOpcodeInfo[$203].Attr := BxAnother;  BxOpcodeInfo[$203].ExecutePtr:=bx_cpu.ADD_GdEd;
  BxOpcodeInfo[$204].Attr := BxImmediate_Ib;  BxOpcodeInfo[$204].ExecutePtr:=bx_cpu.ADD_ALIb;
  BxOpcodeInfo[$205].Attr := BxImmediate_Iv;  BxOpcodeInfo[$205].ExecutePtr:=bx_cpu.ADD_EAXId;
  BxOpcodeInfo[$206].Attr := 0;  BxOpcodeInfo[$206].ExecutePtr:=bx_cpu.PUSH_ES;
  BxOpcodeInfo[$207].Attr := 0;  BxOpcodeInfo[$207].ExecutePtr:=bx_cpu.POP_ES;
  BxOpcodeInfo[$208].Attr := BxAnother;  BxOpcodeInfo[$208].ExecutePtr:=bx_cpu.OR_EbGb;
  BxOpcodeInfo[$209].Attr := BxAnother;  BxOpcodeInfo[$209].ExecutePtr:=bx_cpu.OR_EdGd;
  BxOpcodeInfo[$20A].Attr := BxAnother;  BxOpcodeInfo[$20A].ExecutePtr:=bx_cpu.OR_GbEb;
  BxOpcodeInfo[$20B].Attr := BxAnother;  BxOpcodeInfo[$20B].ExecutePtr:=bx_cpu.OR_GdEd;
  BxOpcodeInfo[$20C].Attr := BxImmediate_Ib;  BxOpcodeInfo[$20C].ExecutePtr:=bx_cpu.OR_ALIb;
  BxOpcodeInfo[$20D].Attr := BxImmediate_Iv;  BxOpcodeInfo[$20D].ExecutePtr:=bx_cpu.OR_EAXId;
  BxOpcodeInfo[$20E].Attr := 0;  BxOpcodeInfo[$20E].ExecutePtr:=bx_cpu.PUSH_CS;
  BxOpcodeInfo[$20F].Attr := BxAnother;  BxOpcodeInfo[$20F].ExecutePtr:=bx_cpu.BxError; // 2-byte escape
  BxOpcodeInfo[$210].Attr := BxAnother;  BxOpcodeInfo[$210].ExecutePtr:=bx_cpu.ADC_EbGb;
  BxOpcodeInfo[$211].Attr := BxAnother;  BxOpcodeInfo[$211].ExecutePtr:=bx_cpu.ADC_EdGd;
  BxOpcodeInfo[$212].Attr := BxAnother;  BxOpcodeInfo[$212].ExecutePtr:=bx_cpu.ADC_GbEb;
  BxOpcodeInfo[$213].Attr := BxAnother;  BxOpcodeInfo[$213].ExecutePtr:=bx_cpu.ADC_GdEd;
  BxOpcodeInfo[$214].Attr := BxImmediate_Ib;  BxOpcodeInfo[$214].ExecutePtr:=bx_cpu.ADC_ALIb;
  BxOpcodeInfo[$215].Attr := BxImmediate_Iv;  BxOpcodeInfo[$215].ExecutePtr:=bx_cpu.ADC_EAXId;
  BxOpcodeInfo[$216].Attr := 0;  BxOpcodeInfo[$216].ExecutePtr:=bx_cpu.PUSH_SS;
  BxOpcodeInfo[$217].Attr := 0;  BxOpcodeInfo[$217].ExecutePtr:=bx_cpu.POP_SS;
  BxOpcodeInfo[$218].Attr := BxAnother;  BxOpcodeInfo[$218].ExecutePtr:=bx_cpu.SBB_EbGb;
  BxOpcodeInfo[$219].Attr := BxAnother;  BxOpcodeInfo[$219].ExecutePtr:=bx_cpu.SBB_EdGd;
  BxOpcodeInfo[$21A].Attr := BxAnother;  BxOpcodeInfo[$21A].ExecutePtr:=bx_cpu.SBB_GbEb;
  BxOpcodeInfo[$21B].Attr := BxAnother;  BxOpcodeInfo[$21B].ExecutePtr:=bx_cpu.SBB_GdEd;
  BxOpcodeInfo[$21C].Attr := BxImmediate_Ib;  BxOpcodeInfo[$21C].ExecutePtr:=bx_cpu.SBB_ALIb;
  BxOpcodeInfo[$21D].Attr := BxImmediate_Iv;  BxOpcodeInfo[$21D].ExecutePtr:=bx_cpu.SBB_EAXId;
  BxOpcodeInfo[$21E].Attr := 0;  BxOpcodeInfo[$21E].ExecutePtr:=bx_cpu.PUSH_DS;
  BxOpcodeInfo[$21F].Attr := 0;  BxOpcodeInfo[$21F].ExecutePtr:=bx_cpu.POP_DS;
  BxOpcodeInfo[$220].Attr := BxAnother;  BxOpcodeInfo[$220].ExecutePtr:=bx_cpu.AND_EbGb;
  BxOpcodeInfo[$221].Attr := BxAnother;  BxOpcodeInfo[$221].ExecutePtr:=bx_cpu.AND_EdGd;
  BxOpcodeInfo[$222].Attr := BxAnother;  BxOpcodeInfo[$222].ExecutePtr:=bx_cpu.AND_GbEb;
  BxOpcodeInfo[$223].Attr := BxAnother;  BxOpcodeInfo[$223].ExecutePtr:=bx_cpu.AND_GdEd;
  BxOpcodeInfo[$224].Attr := BxImmediate_Ib;  BxOpcodeInfo[$224].ExecutePtr:=bx_cpu.AND_ALIb;
  BxOpcodeInfo[$225].Attr := BxImmediate_Iv;  BxOpcodeInfo[$225].ExecutePtr:=bx_cpu.AND_EAXId;
  BxOpcodeInfo[$226].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$226].ExecutePtr:=bx_cpu.BxError; // ES:
  BxOpcodeInfo[$227].Attr := 0;  BxOpcodeInfo[$227].ExecutePtr:=bx_cpu.DAA;
  BxOpcodeInfo[$228].Attr := BxAnother;  BxOpcodeInfo[$228].ExecutePtr:=bx_cpu.SUB_EbGb;
  BxOpcodeInfo[$229].Attr := BxAnother;  BxOpcodeInfo[$229].ExecutePtr:=bx_cpu.SUB_EdGd;
  BxOpcodeInfo[$22A].Attr := BxAnother;  BxOpcodeInfo[$22A].ExecutePtr:=bx_cpu.SUB_GbEb;
  BxOpcodeInfo[$22B].Attr := BxAnother;  BxOpcodeInfo[$22B].ExecutePtr:=bx_cpu.SUB_GdEd;
  BxOpcodeInfo[$22C].Attr := BxImmediate_Ib;  BxOpcodeInfo[$22C].ExecutePtr:=bx_cpu.SUB_ALIb;
  BxOpcodeInfo[$22D].Attr := BxImmediate_Iv;  BxOpcodeInfo[$22D].ExecutePtr:=bx_cpu.SUB_EAXId;
  BxOpcodeInfo[$22E].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$22E].ExecutePtr:=bx_cpu.BxError; // CS:
  BxOpcodeInfo[$22F].Attr := 0;  BxOpcodeInfo[$22F].ExecutePtr:=bx_cpu.DAS;
  BxOpcodeInfo[$230].Attr := BxAnother;  BxOpcodeInfo[$230].ExecutePtr:=bx_cpu.XOR_EbGb;
  BxOpcodeInfo[$231].Attr := BxAnother;  BxOpcodeInfo[$231].ExecutePtr:=bx_cpu.XOR_EdGd;
  BxOpcodeInfo[$232].Attr := BxAnother;  BxOpcodeInfo[$232].ExecutePtr:=bx_cpu.XOR_GbEb;
  BxOpcodeInfo[$233].Attr := BxAnother;  BxOpcodeInfo[$233].ExecutePtr:=bx_cpu.XOR_GdEd;
  BxOpcodeInfo[$234].Attr := BxImmediate_Ib;  BxOpcodeInfo[$234].ExecutePtr:=bx_cpu.XOR_ALIb;
  BxOpcodeInfo[$235].Attr := BxImmediate_Iv;  BxOpcodeInfo[$235].ExecutePtr:=bx_cpu.XOR_EAXId;
  BxOpcodeInfo[$236].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$236].ExecutePtr:=bx_cpu.BxError; // SS:
  BxOpcodeInfo[$237].Attr := 0;  BxOpcodeInfo[$237].ExecutePtr:=bx_cpu.AAA;
  BxOpcodeInfo[$238].Attr := BxAnother;  BxOpcodeInfo[$238].ExecutePtr:=bx_cpu.CMP_EbGb;
  BxOpcodeInfo[$239].Attr := BxAnother;  BxOpcodeInfo[$239].ExecutePtr:=bx_cpu.CMP_EdGd;
  BxOpcodeInfo[$23A].Attr := BxAnother;  BxOpcodeInfo[$23A].ExecutePtr:=bx_cpu.CMP_GbEb;
  BxOpcodeInfo[$23B].Attr := BxAnother;  BxOpcodeInfo[$23B].ExecutePtr:=bx_cpu.CMP_GdEd;
  BxOpcodeInfo[$23C].Attr := BxImmediate_Ib;  BxOpcodeInfo[$23C].ExecutePtr:=bx_cpu.CMP_ALIb;
  BxOpcodeInfo[$23D].Attr := BxImmediate_Iv;  BxOpcodeInfo[$23D].ExecutePtr:=bx_cpu.CMP_EAXId;
  BxOpcodeInfo[$23E].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$23E].ExecutePtr:=bx_cpu.BxError; // DS:
  BxOpcodeInfo[$23F].Attr := 0;  BxOpcodeInfo[$23F].ExecutePtr:=bx_cpu.AAS;
  BxOpcodeInfo[$240].Attr := 0;  BxOpcodeInfo[$240].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$241].Attr := 0;  BxOpcodeInfo[$241].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$242].Attr := 0;  BxOpcodeInfo[$242].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$243].Attr := 0;  BxOpcodeInfo[$243].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$244].Attr := 0;  BxOpcodeInfo[$244].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$245].Attr := 0;  BxOpcodeInfo[$245].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$246].Attr := 0;  BxOpcodeInfo[$246].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$247].Attr := 0;  BxOpcodeInfo[$247].ExecutePtr:=bx_cpu.INC_ERX;
  BxOpcodeInfo[$248].Attr := 0;  BxOpcodeInfo[$248].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$249].Attr := 0;  BxOpcodeInfo[$249].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24A].Attr := 0;  BxOpcodeInfo[$24A].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24B].Attr := 0;  BxOpcodeInfo[$24B].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24C].Attr := 0;  BxOpcodeInfo[$24C].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24D].Attr := 0;  BxOpcodeInfo[$24D].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24E].Attr := 0;  BxOpcodeInfo[$24E].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$24F].Attr := 0;  BxOpcodeInfo[$24F].ExecutePtr:=bx_cpu.DEC_ERX;
  BxOpcodeInfo[$250].Attr := 0;  BxOpcodeInfo[$250].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$251].Attr := 0;  BxOpcodeInfo[$251].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$252].Attr := 0;  BxOpcodeInfo[$252].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$253].Attr := 0;  BxOpcodeInfo[$253].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$254].Attr := 0;  BxOpcodeInfo[$254].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$255].Attr := 0;  BxOpcodeInfo[$255].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$256].Attr := 0;  BxOpcodeInfo[$256].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$257].Attr := 0;  BxOpcodeInfo[$257].ExecutePtr:=bx_cpu.PUSH_ERX;
  BxOpcodeInfo[$258].Attr := 0;  BxOpcodeInfo[$258].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$259].Attr := 0;  BxOpcodeInfo[$259].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25A].Attr := 0;  BxOpcodeInfo[$25A].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25B].Attr := 0;  BxOpcodeInfo[$25B].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25C].Attr := 0;  BxOpcodeInfo[$25C].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25D].Attr := 0;  BxOpcodeInfo[$25D].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25E].Attr := 0;  BxOpcodeInfo[$25E].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$25F].Attr := 0;  BxOpcodeInfo[$25F].ExecutePtr:=bx_cpu.POP_ERX;
  BxOpcodeInfo[$260].Attr := 0;  BxOpcodeInfo[$260].ExecutePtr:=bx_cpu.PUSHAD32;
  BxOpcodeInfo[$261].Attr := 0;  BxOpcodeInfo[$261].ExecutePtr:=bx_cpu.POPAD32;
  BxOpcodeInfo[$262].Attr := BxAnother;  BxOpcodeInfo[$262].ExecutePtr:=bx_cpu.BOUND_GvMa;
  BxOpcodeInfo[$263].Attr := BxAnother;  BxOpcodeInfo[$263].ExecutePtr:=bx_cpu.ARPL_EwGw;
  BxOpcodeInfo[$264].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$264].ExecutePtr:=bx_cpu.BxError; // FS:
  BxOpcodeInfo[$265].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$265].ExecutePtr:=bx_cpu.BxError; // GS:
  BxOpcodeInfo[$266].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$266].ExecutePtr:=bx_cpu.BxError; // OS:
  BxOpcodeInfo[$267].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$267].ExecutePtr:=bx_cpu.BxError; // AS:
  BxOpcodeInfo[$268].Attr := BxImmediate_Iv;  BxOpcodeInfo[$268].ExecutePtr:=bx_cpu.PUSH_Id;
  BxOpcodeInfo[$269].Attr := BxAnother or BxImmediate_Iv;  BxOpcodeInfo[$269].ExecutePtr:=bx_cpu.IMUL_GdEdId;
  BxOpcodeInfo[$26A].Attr := BxImmediate_Ib_SE;  BxOpcodeInfo[$26A].ExecutePtr:=bx_cpu.PUSH_Id;
  BxOpcodeInfo[$26B].Attr := BxAnother or BxImmediate_Ib_SE;  BxOpcodeInfo[$26B].ExecutePtr:=bx_cpu.IMUL_GdEdId;
  BxOpcodeInfo[$26C].Attr := BxRepeatable;  BxOpcodeInfo[$26C].ExecutePtr:=bx_cpu.INSB_YbDX;
  BxOpcodeInfo[$26D].Attr := BxRepeatable;  BxOpcodeInfo[$26D].ExecutePtr:=bx_cpu.INSW_YvDX;
  BxOpcodeInfo[$26E].Attr := BxRepeatable;  BxOpcodeInfo[$26E].ExecutePtr:=bx_cpu.OUTSB_DXXb;
  BxOpcodeInfo[$26F].Attr := BxRepeatable;  BxOpcodeInfo[$26F].ExecutePtr:=bx_cpu.OUTSW_DXXv;
  BxOpcodeInfo[$270].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$270].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$271].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$271].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$272].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$272].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$273].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$273].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$274].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$274].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$275].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$275].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$276].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$276].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$277].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$277].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$278].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$278].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$279].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$279].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27A].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27A].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27B].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27B].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27C].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27C].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27D].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27D].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27E].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27E].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$27F].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$27F].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$280].Attr := BxAnother or BxGroup1; BxOpcodeInfo[$280].ExecutePtr:=nil; BxOpcodeInfo[$280].AnotherArray:=@BxOpcodeInfoG1EbIb;
  BxOpcodeInfo[$281].Attr := BxAnother or BxGroup1 or BxImmediate_Iv; BxOpcodeInfo[$281].ExecutePtr:=nil; BxOpcodeInfo[$281].AnotherArray:=@BxOpcodeInfoG1Ed;
  BxOpcodeInfo[$282].Attr := BxAnother or BxGroup1;  BxOpcodeInfo[$282].ExecutePtr:=nil; BxOpcodeInfo[$282].AnotherArray:=@BxOpcodeInfoG1EbIb;
  BxOpcodeInfo[$283].Attr := BxAnother or BxGroup1 or BxImmediate_Ib_SE; BxOpcodeInfo[$283].ExecutePtr:=nil; BxOpcodeInfo[$283].AnotherArray:=@BxOpcodeInfoG1Ed;
  BxOpcodeInfo[$284].Attr := BxAnother;  BxOpcodeInfo[$284].ExecutePtr:=bx_cpu.TEST_EbGb;
  BxOpcodeInfo[$285].Attr := BxAnother;  BxOpcodeInfo[$285].ExecutePtr:=bx_cpu.TEST_EdGd;
  BxOpcodeInfo[$286].Attr := BxAnother;  BxOpcodeInfo[$286].ExecutePtr:=bx_cpu.XCHG_EbGb;
  BxOpcodeInfo[$287].Attr := BxAnother;  BxOpcodeInfo[$287].ExecutePtr:=bx_cpu.XCHG_EdGd;
  BxOpcodeInfo[$288].Attr := BxAnother;  BxOpcodeInfo[$288].ExecutePtr:=bx_cpu.MOV_EbGb;
  BxOpcodeInfo[$289].Attr := BxAnother;  BxOpcodeInfo[$289].ExecutePtr:=bx_cpu.MOV_EdGd;
  BxOpcodeInfo[$28A].Attr := BxAnother;  BxOpcodeInfo[$28A].ExecutePtr:=bx_cpu.MOV_GbEb;
  BxOpcodeInfo[$28B].Attr := BxAnother;  BxOpcodeInfo[$28B].ExecutePtr:=bx_cpu.MOV_GdEd;
  BxOpcodeInfo[$28C].Attr := BxAnother;  BxOpcodeInfo[$28C].ExecutePtr:=bx_cpu.MOV_EwSw;
  BxOpcodeInfo[$28D].Attr := BxAnother;  BxOpcodeInfo[$28D].ExecutePtr:=bx_cpu.LEA_GdM;
  BxOpcodeInfo[$28E].Attr := BxAnother;  BxOpcodeInfo[$28E].ExecutePtr:=bx_cpu.MOV_SwEw;
  BxOpcodeInfo[$28F].Attr := BxAnother;  BxOpcodeInfo[$28F].ExecutePtr:=bx_cpu.POP_Ed;
  BxOpcodeInfo[$290].Attr := 0;  BxOpcodeInfo[$290].ExecutePtr:=bx_cpu.NOP;
  BxOpcodeInfo[$291].Attr := 0;  BxOpcodeInfo[$291].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$292].Attr := 0;  BxOpcodeInfo[$292].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$293].Attr := 0;  BxOpcodeInfo[$293].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$294].Attr := 0;  BxOpcodeInfo[$294].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$295].Attr := 0;  BxOpcodeInfo[$295].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$296].Attr := 0;  BxOpcodeInfo[$296].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$297].Attr := 0;  BxOpcodeInfo[$297].ExecutePtr:=bx_cpu.XCHG_ERXEAX;
  BxOpcodeInfo[$298].Attr := 0;  BxOpcodeInfo[$298].ExecutePtr:=bx_cpu.CWDE;
  BxOpcodeInfo[$299].Attr := 0;  BxOpcodeInfo[$299].ExecutePtr:=bx_cpu.CDQ;
  BxOpcodeInfo[$29A].Attr := BxImmediate_IvIw;  BxOpcodeInfo[$29A].ExecutePtr:=bx_cpu.CALL32_Ap;
  BxOpcodeInfo[$29B].Attr := 0;  BxOpcodeInfo[$29B].ExecutePtr:=bx_cpu.FWAIT;
  BxOpcodeInfo[$29C].Attr := 0;  BxOpcodeInfo[$29C].ExecutePtr:=bx_cpu.PUSHF_Fv;
  BxOpcodeInfo[$29D].Attr := 0;  BxOpcodeInfo[$29D].ExecutePtr:=bx_cpu.POPF_Fv;
  BxOpcodeInfo[$29E].Attr := 0;  BxOpcodeInfo[$29E].ExecutePtr:=bx_cpu.SAHF;
  BxOpcodeInfo[$29F].Attr := 0;  BxOpcodeInfo[$29F].ExecutePtr:=bx_cpu.LAHF;
  BxOpcodeInfo[$2A0].Attr := BxImmediate_O;  BxOpcodeInfo[$2A0].ExecutePtr:=bx_cpu.MOV_ALOb;
  BxOpcodeInfo[$2A1].Attr := BxImmediate_O;  BxOpcodeInfo[$2A1].ExecutePtr:=bx_cpu.MOV_EAXOd;
  BxOpcodeInfo[$2A2].Attr := BxImmediate_O;  BxOpcodeInfo[$2A2].ExecutePtr:=bx_cpu.MOV_ObAL;
  BxOpcodeInfo[$2A3].Attr := BxImmediate_O;  BxOpcodeInfo[$2A3].ExecutePtr:=bx_cpu.MOV_OdEAX;
  BxOpcodeInfo[$2A4].Attr := BxRepeatable;  BxOpcodeInfo[$2A4].ExecutePtr:=bx_cpu.MOVSB_XbYb;
  BxOpcodeInfo[$2A5].Attr := BxRepeatable;  BxOpcodeInfo[$2A5].ExecutePtr:=bx_cpu.MOVSW_XvYv;
  BxOpcodeInfo[$2A6].Attr := BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$2A6].ExecutePtr:=bx_cpu.CMPSB_XbYb;
  BxOpcodeInfo[$2A7].Attr := BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$2A7].ExecutePtr:=bx_cpu.CMPSW_XvYv;
  BxOpcodeInfo[$2A8].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2A8].ExecutePtr:=bx_cpu.TEST_ALIb;
  BxOpcodeInfo[$2A9].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2A9].ExecutePtr:=bx_cpu.TEST_EAXId;
  BxOpcodeInfo[$2AA].Attr := BxRepeatable;  BxOpcodeInfo[$2AA].ExecutePtr:=bx_cpu.STOSB_YbAL;
  BxOpcodeInfo[$2AB].Attr := BxRepeatable;  BxOpcodeInfo[$2AB].ExecutePtr:=bx_cpu.STOSW_YveAX;
  BxOpcodeInfo[$2AC].Attr := BxRepeatable;  BxOpcodeInfo[$2AC].ExecutePtr:=bx_cpu.LODSB_ALXb;
  BxOpcodeInfo[$2AD].Attr := BxRepeatable;  BxOpcodeInfo[$2AD].ExecutePtr:=bx_cpu.LODSW_eAXXv;
  BxOpcodeInfo[$2AE].Attr := BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$2AE].ExecutePtr:=bx_cpu.SCASB_ALXb;
  BxOpcodeInfo[$2AF].Attr := BxRepeatable or BxRepeatableZF;  BxOpcodeInfo[$2AF].ExecutePtr:=bx_cpu.SCASW_eAXXv;
  BxOpcodeInfo[$2B0].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B0].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$2B1].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B1].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$2B2].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B2].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$2B3].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B3].ExecutePtr:=bx_cpu.MOV_RLIb;
  BxOpcodeInfo[$2B4].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B4].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$2B5].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B5].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$2B6].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B6].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$2B7].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2B7].ExecutePtr:=bx_cpu.MOV_RHIb;
  BxOpcodeInfo[$2B8].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2B8].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2B9].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2B9].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BA].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BA].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BB].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BB].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BC].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BC].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BD].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BD].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BE].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BE].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2BF].Attr := BxImmediate_Iv;  BxOpcodeInfo[$2BF].ExecutePtr:=bx_cpu.MOV_ERXId;
  BxOpcodeInfo[$2C0].Attr := BxAnother or BxGroup2 or BxImmediate_Ib; BxOpcodeInfo[$2C0].ExecutePtr:=nil; BxOpcodeInfo[$2C0].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$2C1].Attr := BxAnother or BxGroup2 or BxImmediate_Ib; BxOpcodeInfo[$2C1].ExecutePtr:=nil; BxOpcodeInfo[$2C1].AnotherArray:=@BxOpcodeInfoG2Ed;
  BxOpcodeInfo[$2C2].Attr := BxImmediate_Iw;  BxOpcodeInfo[$2C2].ExecutePtr:=bx_cpu.RETnear32_Iw;
  BxOpcodeInfo[$2C3].Attr := 0;             BxOpcodeInfo[$2C3].ExecutePtr:=bx_cpu.RETnear32;
  BxOpcodeInfo[$2C4].Attr := BxAnother;  BxOpcodeInfo[$2C4].ExecutePtr:=bx_cpu.LES_GvMp;
  BxOpcodeInfo[$2C5].Attr := BxAnother;  BxOpcodeInfo[$2C5].ExecutePtr:=bx_cpu.LDS_GvMp;
  BxOpcodeInfo[$2C6].Attr := BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$2C6].ExecutePtr:=bx_cpu.MOV_EbIb;
  BxOpcodeInfo[$2C7].Attr := BxAnother or BxImmediate_Iv;  BxOpcodeInfo[$2C7].ExecutePtr:=bx_cpu.MOV_EdId;
  BxOpcodeInfo[$2C8].Attr := BxImmediate_IwIb;  BxOpcodeInfo[$2C8].ExecutePtr:=bx_cpu.ENTER_IwIb;
  BxOpcodeInfo[$2C9].Attr := 0;  BxOpcodeInfo[$2C9].ExecutePtr:=bx_cpu.LEAVE;
  BxOpcodeInfo[$2CA].Attr := BxImmediate_Iw;  BxOpcodeInfo[$2CA].ExecutePtr:=bx_cpu.RETfar32_Iw;
  BxOpcodeInfo[$2CB].Attr := 0;  BxOpcodeInfo[$2CB].ExecutePtr:=bx_cpu.RETfar32;
  BxOpcodeInfo[$2CC].Attr := 0;  BxOpcodeInfo[$2CC].ExecutePtr:=bx_cpu.INT3;
  BxOpcodeInfo[$2CD].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2CD].ExecutePtr:=bx_cpu.INT_Ib;
  BxOpcodeInfo[$2CE].Attr := 0;  BxOpcodeInfo[$2CE].ExecutePtr:=bx_cpu.INTO;
  BxOpcodeInfo[$2CF].Attr := 0;  BxOpcodeInfo[$2CF].ExecutePtr:=bx_cpu.IRET32;
  BxOpcodeInfo[$2D0].Attr := BxAnother or BxGroup2;  BxOpcodeInfo[$2D0].ExecutePtr:=nil; BxOpcodeInfo[$2D0].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$2D1].Attr := BxAnother or BxGroup2;  BxOpcodeInfo[$2D1].ExecutePtr:=nil; BxOpcodeInfo[$2D1].AnotherArray:=@BxOpcodeInfoG2Ed;
  BxOpcodeInfo[$2D2].Attr := BxAnother or BxGroup2;  BxOpcodeInfo[$2D2].ExecutePtr:=nil; BxOpcodeInfo[$2D2].AnotherArray:=@BxOpcodeInfoG2Eb;
  BxOpcodeInfo[$2D3].Attr := BxAnother or BxGroup2;  BxOpcodeInfo[$2D3].ExecutePtr:=nil; BxOpcodeInfo[$2D3].AnotherArray:=@BxOpcodeInfoG2Ed;
  BxOpcodeInfo[$2D4].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2D4].ExecutePtr:=bx_cpu.AAM;
  BxOpcodeInfo[$2D5].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2D5].ExecutePtr:=bx_cpu.AAD;
  BxOpcodeInfo[$2D6].Attr := 0;  BxOpcodeInfo[$2D6].ExecutePtr:=bx_cpu.SALC;
  BxOpcodeInfo[$2D7].Attr := 0;  BxOpcodeInfo[$2D7].ExecutePtr:=bx_cpu.XLAT;
  BxOpcodeInfo[$2D8].Attr := BxAnother;  BxOpcodeInfo[$2D8].ExecutePtr:=bx_cpu.ESC0;
  BxOpcodeInfo[$2D9].Attr := BxAnother;  BxOpcodeInfo[$2D9].ExecutePtr:=bx_cpu.ESC1;
  BxOpcodeInfo[$2DA].Attr := BxAnother;  BxOpcodeInfo[$2DA].ExecutePtr:=bx_cpu.ESC2;
  BxOpcodeInfo[$2DB].Attr := BxAnother;  BxOpcodeInfo[$2DB].ExecutePtr:=bx_cpu.ESC3;
  BxOpcodeInfo[$2DC].Attr := BxAnother;  BxOpcodeInfo[$2DC].ExecutePtr:=bx_cpu.ESC4;
  BxOpcodeInfo[$2DD].Attr := BxAnother;  BxOpcodeInfo[$2DD].ExecutePtr:=bx_cpu.ESC5;
  BxOpcodeInfo[$2DE].Attr := BxAnother;  BxOpcodeInfo[$2DE].ExecutePtr:=bx_cpu.ESC6;
  BxOpcodeInfo[$2DF].Attr := BxAnother;  BxOpcodeInfo[$2DF].ExecutePtr:=bx_cpu.ESC7;
  BxOpcodeInfo[$2E0].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$2E0].ExecutePtr:=bx_cpu.LOOPNE_Jb;
  BxOpcodeInfo[$2E1].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$2E1].ExecutePtr:=bx_cpu.LOOPE_Jb;
  BxOpcodeInfo[$2E2].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$2E2].ExecutePtr:=bx_cpu.LOOP_Jb;
  BxOpcodeInfo[$2E3].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$2E3].ExecutePtr:=bx_cpu.JCXZ_Jb;
  BxOpcodeInfo[$2E4].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2E4].ExecutePtr:=bx_cpu.IN_ALIb;
  BxOpcodeInfo[$2E5].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2E5].ExecutePtr:=bx_cpu.IN_eAXIb;
  BxOpcodeInfo[$2E6].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2E6].ExecutePtr:=bx_cpu.OUT_IbAL;
  BxOpcodeInfo[$2E7].Attr := BxImmediate_Ib;  BxOpcodeInfo[$2E7].ExecutePtr:=bx_cpu.OUT_IbeAX;
  BxOpcodeInfo[$2E8].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$2E8].ExecutePtr:=bx_cpu.CALL_Ad;
  BxOpcodeInfo[$2E9].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$2E9].ExecutePtr:=bx_cpu.JMP_Jd;
  BxOpcodeInfo[$2EA].Attr := BxImmediate_IvIw;  BxOpcodeInfo[$2EA].ExecutePtr:=bx_cpu.JMP_Ap;
  BxOpcodeInfo[$2EB].Attr := BxImmediate_BrOff8;  BxOpcodeInfo[$2EB].ExecutePtr:=bx_cpu.JMP_Jd;
  BxOpcodeInfo[$2EC].Attr := 0;  BxOpcodeInfo[$2EC].ExecutePtr:=bx_cpu.IN_ALDX;
  BxOpcodeInfo[$2ED].Attr := 0;  BxOpcodeInfo[$2ED].ExecutePtr:=bx_cpu.IN_eAXDX;
  BxOpcodeInfo[$2EE].Attr := 0;  BxOpcodeInfo[$2EE].ExecutePtr:=bx_cpu.OUT_DXAL;
  BxOpcodeInfo[$2EF].Attr := 0;  BxOpcodeInfo[$2EF].ExecutePtr:=bx_cpu.OUT_DXeAX;
  BxOpcodeInfo[$2F0].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$2F0].ExecutePtr:=bx_cpu.BxError; // LOCK:
  BxOpcodeInfo[$2F1].Attr := 0;  BxOpcodeInfo[$2F1].ExecutePtr:=bx_cpu.INT1;
  BxOpcodeInfo[$2F2].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$2F2].ExecutePtr:=bx_cpu.BxError; // REPNE/REPNZ
  BxOpcodeInfo[$2F3].Attr := BxPrefix or BxAnother;  BxOpcodeInfo[$2F3].ExecutePtr:=bx_cpu.BxError; // REP,REPE/REPZ
  BxOpcodeInfo[$2F4].Attr := 0;  BxOpcodeInfo[$2F4].ExecutePtr:=bx_cpu.HLT;
  BxOpcodeInfo[$2F5].Attr := 0;  BxOpcodeInfo[$2F5].ExecutePtr:=bx_cpu.CMC;
  BxOpcodeInfo[$2F6].Attr := BxAnother or BxGroup3;  BxOpcodeInfo[$2F6].ExecutePtr:=nil; BxOpcodeInfo[$2F6].AnotherArray:=@BxOpcodeInfoG3Eb;
  BxOpcodeInfo[$2F7].Attr := BxAnother or BxGroup3;  BxOpcodeInfo[$2F7].ExecutePtr:=nil; BxOpcodeInfo[$2F7].AnotherArray:=@BxOpcodeInfoG3Ed;
  BxOpcodeInfo[$2F8].Attr := 0;  BxOpcodeInfo[$2F8].ExecutePtr:=bx_cpu.CLC;
  BxOpcodeInfo[$2F9].Attr := 0;  BxOpcodeInfo[$2F9].ExecutePtr:=bx_cpu.STC;
  BxOpcodeInfo[$2FA].Attr := 0;  BxOpcodeInfo[$2FA].ExecutePtr:=bx_cpu.CLI;
  BxOpcodeInfo[$2FB].Attr := 0;  BxOpcodeInfo[$2FB].ExecutePtr:=bx_cpu.STI;
  BxOpcodeInfo[$2FC].Attr := 0;  BxOpcodeInfo[$2FC].ExecutePtr:=bx_cpu.CLD;
  BxOpcodeInfo[$2FD].Attr := 0;  BxOpcodeInfo[$2FD].ExecutePtr:=bx_cpu.STD;
  BxOpcodeInfo[$2FE].Attr := BxAnother or BxGroup4;  BxOpcodeInfo[$2FE].ExecutePtr:=nil; BxOpcodeInfo[$2FE].AnotherArray:=@BxOpcodeInfoG4;
  BxOpcodeInfo[$2FF].Attr := BxAnother or BxGroup5;  BxOpcodeInfo[$2FF].ExecutePtr:=nil; BxOpcodeInfo[$2FF].AnotherArray:=@BxOpcodeInfoG5d;
  BxOpcodeInfo[$300].Attr := BxAnother or BxGroup6;  BxOpcodeInfo[$300].ExecutePtr:=nil; BxOpcodeInfo[$300].AnotherArray:=@BxOpcodeInfoG6;
  BxOpcodeInfo[$301].Attr := BxAnother or BxGroup7;  BxOpcodeInfo[$301].ExecutePtr:=nil; BxOpcodeInfo[$301].AnotherArray:=@BxOpcodeInfoG7;
  BxOpcodeInfo[$302].Attr := BxAnother;  BxOpcodeInfo[$302].ExecutePtr:=bx_cpu.LAR_GvEw;
  BxOpcodeInfo[$303].Attr := BxAnother;  BxOpcodeInfo[$303].ExecutePtr:=bx_cpu.LSL_GvEw;
  BxOpcodeInfo[$304].Attr := 0;  BxOpcodeInfo[$304].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$305].Attr := 0;  BxOpcodeInfo[$305].ExecutePtr:=bx_cpu.LOADALL;
  BxOpcodeInfo[$306].Attr := 0;  BxOpcodeInfo[$306].ExecutePtr:=bx_cpu.CLTS;
  BxOpcodeInfo[$307].Attr := 0;  BxOpcodeInfo[$307].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$308].Attr := 0;  BxOpcodeInfo[$308].ExecutePtr:=bx_cpu.INVD;
  BxOpcodeInfo[$309].Attr := 0;  BxOpcodeInfo[$309].ExecutePtr:=bx_cpu.WBINVD;
  BxOpcodeInfo[$30A].Attr := 0;  BxOpcodeInfo[$30A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$30B].Attr := 0;  BxOpcodeInfo[$30B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$30C].Attr := 0;  BxOpcodeInfo[$30C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$30D].Attr := 0;  BxOpcodeInfo[$30D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$30E].Attr := 0;  BxOpcodeInfo[$30E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$30F].Attr := 0;  BxOpcodeInfo[$30F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$310].Attr := 0;  BxOpcodeInfo[$310].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$311].Attr := 0;  BxOpcodeInfo[$311].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$312].Attr := 0;  BxOpcodeInfo[$312].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$313].Attr := 0;  BxOpcodeInfo[$313].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$314].Attr := 0;  BxOpcodeInfo[$314].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$315].Attr := 0;  BxOpcodeInfo[$315].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$316].Attr := 0;  BxOpcodeInfo[$316].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$317].Attr := 0;  BxOpcodeInfo[$317].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$318].Attr := 0;  BxOpcodeInfo[$318].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$319].Attr := 0;  BxOpcodeInfo[$319].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31A].Attr := 0;  BxOpcodeInfo[$31A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31B].Attr := 0;  BxOpcodeInfo[$31B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31C].Attr := 0;  BxOpcodeInfo[$31C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31D].Attr := 0;  BxOpcodeInfo[$31D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31E].Attr := 0;  BxOpcodeInfo[$31E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$31F].Attr := 0;  BxOpcodeInfo[$31F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$320].Attr := BxAnother;  BxOpcodeInfo[$320].ExecutePtr:=bx_cpu.MOV_RdCd;
  BxOpcodeInfo[$321].Attr := BxAnother;  BxOpcodeInfo[$321].ExecutePtr:=bx_cpu.MOV_RdDd;
  BxOpcodeInfo[$322].Attr := BxAnother;  BxOpcodeInfo[$322].ExecutePtr:=bx_cpu.MOV_CdRd;
  BxOpcodeInfo[$323].Attr := BxAnother;  BxOpcodeInfo[$323].ExecutePtr:=bx_cpu.MOV_DdRd;
  BxOpcodeInfo[$324].Attr := BxAnother;  BxOpcodeInfo[$324].ExecutePtr:=bx_cpu.MOV_RdTd;
  BxOpcodeInfo[$325].Attr := 0;  BxOpcodeInfo[$325].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$326].Attr := BxAnother;  BxOpcodeInfo[$326].ExecutePtr:=bx_cpu.MOV_TdRd;
  BxOpcodeInfo[$327].Attr := 0;  BxOpcodeInfo[$327].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$328].Attr := 0;  BxOpcodeInfo[$328].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$329].Attr := 0;  BxOpcodeInfo[$329].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32A].Attr := 0;  BxOpcodeInfo[$32A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32B].Attr := 0;  BxOpcodeInfo[$32B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32C].Attr := 0;  BxOpcodeInfo[$32C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32D].Attr := 0;  BxOpcodeInfo[$32D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32E].Attr := 0;  BxOpcodeInfo[$32E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$32F].Attr := 0;  BxOpcodeInfo[$32F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$330].Attr := 0;  BxOpcodeInfo[$330].ExecutePtr:=bx_cpu.WRMSR;
  BxOpcodeInfo[$331].Attr := 0;  BxOpcodeInfo[$331].ExecutePtr:=bx_cpu.RDTSC;
  BxOpcodeInfo[$332].Attr := 0;  BxOpcodeInfo[$332].ExecutePtr:=bx_cpu.RDMSR;
  BxOpcodeInfo[$333].Attr := 0;  BxOpcodeInfo[$333].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$334].Attr := 0;  BxOpcodeInfo[$334].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$335].Attr := 0;  BxOpcodeInfo[$335].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$336].Attr := 0;  BxOpcodeInfo[$336].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$337].Attr := 0;  BxOpcodeInfo[$337].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$338].Attr := 0;  BxOpcodeInfo[$338].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$339].Attr := 0;  BxOpcodeInfo[$339].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33A].Attr := 0;  BxOpcodeInfo[$33A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33B].Attr := 0;  BxOpcodeInfo[$33B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33C].Attr := 0;  BxOpcodeInfo[$33C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33D].Attr := 0;  BxOpcodeInfo[$33D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33E].Attr := 0;  BxOpcodeInfo[$33E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$33F].Attr := 0;  BxOpcodeInfo[$33F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$340].Attr := BxAnother;  BxOpcodeInfo[$340].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$341].Attr := BxAnother;  BxOpcodeInfo[$341].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$342].Attr := BxAnother;  BxOpcodeInfo[$342].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$343].Attr := BxAnother;  BxOpcodeInfo[$343].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$344].Attr := BxAnother;  BxOpcodeInfo[$344].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$345].Attr := BxAnother;  BxOpcodeInfo[$345].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$346].Attr := BxAnother;  BxOpcodeInfo[$346].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$347].Attr := BxAnother;  BxOpcodeInfo[$347].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$348].Attr := BxAnother;  BxOpcodeInfo[$348].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$349].Attr := BxAnother;  BxOpcodeInfo[$349].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34A].Attr := BxAnother;  BxOpcodeInfo[$34A].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34B].Attr := BxAnother;  BxOpcodeInfo[$34B].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34C].Attr := BxAnother;  BxOpcodeInfo[$34C].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34D].Attr := BxAnother;  BxOpcodeInfo[$34D].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34E].Attr := BxAnother;  BxOpcodeInfo[$34E].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$34F].Attr := BxAnother;  BxOpcodeInfo[$34F].ExecutePtr:=bx_cpu.CMOV_GdEd;
  BxOpcodeInfo[$350].Attr := 0;  BxOpcodeInfo[$350].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$351].Attr := 0;  BxOpcodeInfo[$351].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$352].Attr := 0;  BxOpcodeInfo[$352].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$353].Attr := 0;  BxOpcodeInfo[$353].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$354].Attr := 0;  BxOpcodeInfo[$354].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$355].Attr := 0;  BxOpcodeInfo[$355].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$356].Attr := 0;  BxOpcodeInfo[$356].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$357].Attr := 0;  BxOpcodeInfo[$357].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$358].Attr := 0;  BxOpcodeInfo[$358].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$359].Attr := 0;  BxOpcodeInfo[$359].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35A].Attr := 0;  BxOpcodeInfo[$35A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35B].Attr := 0;  BxOpcodeInfo[$35B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35C].Attr := 0;  BxOpcodeInfo[$35C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35D].Attr := 0;  BxOpcodeInfo[$35D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35E].Attr := 0;  BxOpcodeInfo[$35E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$35F].Attr := 0;  BxOpcodeInfo[$35F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$360].Attr := 0;  BxOpcodeInfo[$360].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$361].Attr := 0;  BxOpcodeInfo[$361].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$362].Attr := 0;  BxOpcodeInfo[$362].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$363].Attr := 0;  BxOpcodeInfo[$363].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$364].Attr := 0;  BxOpcodeInfo[$364].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$365].Attr := 0;  BxOpcodeInfo[$365].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$366].Attr := 0;  BxOpcodeInfo[$366].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$367].Attr := 0;  BxOpcodeInfo[$367].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$368].Attr := 0;  BxOpcodeInfo[$368].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$369].Attr := 0;  BxOpcodeInfo[$369].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36A].Attr := 0;  BxOpcodeInfo[$36A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36B].Attr := 0;  BxOpcodeInfo[$36B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36C].Attr := 0;  BxOpcodeInfo[$36C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36D].Attr := 0;  BxOpcodeInfo[$36D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36E].Attr := 0;  BxOpcodeInfo[$36E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$36F].Attr := 0;  BxOpcodeInfo[$36F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$370].Attr := 0;  BxOpcodeInfo[$370].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$371].Attr := 0;  BxOpcodeInfo[$371].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$372].Attr := 0;  BxOpcodeInfo[$372].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$373].Attr := 0;  BxOpcodeInfo[$373].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$374].Attr := 0;  BxOpcodeInfo[$374].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$375].Attr := 0;  BxOpcodeInfo[$375].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$376].Attr := 0;  BxOpcodeInfo[$376].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$377].Attr := 0;  BxOpcodeInfo[$377].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$378].Attr := 0;  BxOpcodeInfo[$378].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$379].Attr := 0;  BxOpcodeInfo[$379].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37A].Attr := 0;  BxOpcodeInfo[$37A].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37B].Attr := 0;  BxOpcodeInfo[$37B].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37C].Attr := 0;  BxOpcodeInfo[$37C].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37D].Attr := 0;  BxOpcodeInfo[$37D].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37E].Attr := 0;  BxOpcodeInfo[$37E].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$37F].Attr := 0;  BxOpcodeInfo[$37F].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$380].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$380].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$381].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$381].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$382].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$382].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$383].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$383].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$384].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$384].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$385].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$385].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$386].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$386].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$387].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$387].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$388].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$388].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$389].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$389].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38A].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38A].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38B].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38B].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38C].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38C].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38D].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38D].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38E].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38E].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$38F].Attr := BxImmediate_BrOff32;  BxOpcodeInfo[$38F].ExecutePtr:=bx_cpu.JCC_Jd;
  BxOpcodeInfo[$390].Attr := BxAnother;  BxOpcodeInfo[$390].ExecutePtr:=bx_cpu.SETO_Eb;
  BxOpcodeInfo[$391].Attr := BxAnother;  BxOpcodeInfo[$391].ExecutePtr:=bx_cpu.SETNO_Eb;
  BxOpcodeInfo[$392].Attr := BxAnother;  BxOpcodeInfo[$392].ExecutePtr:=bx_cpu.SETB_Eb;
  BxOpcodeInfo[$393].Attr := BxAnother;  BxOpcodeInfo[$393].ExecutePtr:=bx_cpu.SETNB_Eb;
  BxOpcodeInfo[$394].Attr := BxAnother;  BxOpcodeInfo[$394].ExecutePtr:=bx_cpu.SETZ_Eb;
  BxOpcodeInfo[$395].Attr := BxAnother;  BxOpcodeInfo[$395].ExecutePtr:=bx_cpu.SETNZ_Eb;
  BxOpcodeInfo[$396].Attr := BxAnother;  BxOpcodeInfo[$396].ExecutePtr:=bx_cpu.SETBE_Eb;
  BxOpcodeInfo[$397].Attr := BxAnother;  BxOpcodeInfo[$397].ExecutePtr:=bx_cpu.SETNBE_Eb;
  BxOpcodeInfo[$398].Attr := BxAnother;  BxOpcodeInfo[$398].ExecutePtr:=bx_cpu.SETS_Eb;
  BxOpcodeInfo[$399].Attr := BxAnother;  BxOpcodeInfo[$399].ExecutePtr:=bx_cpu.SETNS_Eb;
  BxOpcodeInfo[$39A].Attr := BxAnother;  BxOpcodeInfo[$39A].ExecutePtr:=bx_cpu.SETP_Eb;
  BxOpcodeInfo[$39B].Attr := BxAnother;  BxOpcodeInfo[$39B].ExecutePtr:=bx_cpu.SETNP_Eb;
  BxOpcodeInfo[$39C].Attr := BxAnother;  BxOpcodeInfo[$39C].ExecutePtr:=bx_cpu.SETL_Eb;
  BxOpcodeInfo[$39D].Attr := BxAnother;  BxOpcodeInfo[$39D].ExecutePtr:=bx_cpu.SETNL_Eb;
  BxOpcodeInfo[$39E].Attr := BxAnother;  BxOpcodeInfo[$39E].ExecutePtr:=bx_cpu.SETLE_Eb;
  BxOpcodeInfo[$39F].Attr := BxAnother;  BxOpcodeInfo[$39F].ExecutePtr:=bx_cpu.SETNLE_Eb;
  BxOpcodeInfo[$3A0].Attr := 0;          BxOpcodeInfo[$3A0].ExecutePtr:=bx_cpu.PUSH_FS;
  BxOpcodeInfo[$3A1].Attr := 0;          BxOpcodeInfo[$3A1].ExecutePtr:=bx_cpu.POP_FS;
  BxOpcodeInfo[$3A2].Attr := 0;          BxOpcodeInfo[$3A2].ExecutePtr:=bx_cpu.CPUID;
  BxOpcodeInfo[$3A3].Attr := BxAnother;  BxOpcodeInfo[$3A3].ExecutePtr:=bx_cpu.BT_EvGv;
  BxOpcodeInfo[$3A4].Attr := BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$3A4].ExecutePtr:=bx_cpu.SHLD_EdGd;
  BxOpcodeInfo[$3A5].Attr := BxAnother;  BxOpcodeInfo[$3A5].ExecutePtr:=bx_cpu.SHLD_EdGd;
  BxOpcodeInfo[$3A6].Attr := 0;          BxOpcodeInfo[$3A6].ExecutePtr:=bx_cpu.CMPXCHG_XBTS;
  BxOpcodeInfo[$3A7].Attr := 0;          BxOpcodeInfo[$3A7].ExecutePtr:=bx_cpu.CMPXCHG_IBTS;
  BxOpcodeInfo[$3A8].Attr := 0;          BxOpcodeInfo[$3A8].ExecutePtr:=bx_cpu.PUSH_GS;
  BxOpcodeInfo[$3A9].Attr := 0;          BxOpcodeInfo[$3A9].ExecutePtr:=bx_cpu.POP_GS;
  BxOpcodeInfo[$3AA].Attr := 0;          BxOpcodeInfo[$3AA].ExecutePtr:=bx_cpu.RSM;
  BxOpcodeInfo[$3AB].Attr := BxAnother;  BxOpcodeInfo[$3AB].ExecutePtr:=bx_cpu.BTS_EvGv;
  BxOpcodeInfo[$3AC].Attr := BxAnother or BxImmediate_Ib;  BxOpcodeInfo[$3AC].ExecutePtr:=bx_cpu.SHRD_EdGd;
  BxOpcodeInfo[$3AD].Attr := BxAnother;  BxOpcodeInfo[$3AD].ExecutePtr:=bx_cpu.SHRD_EdGd;
  BxOpcodeInfo[$3AE].Attr := 0;          BxOpcodeInfo[$3AE].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3AF].Attr := BxAnother;  BxOpcodeInfo[$3AF].ExecutePtr:=bx_cpu.IMUL_GdEd;
  BxOpcodeInfo[$3B0].Attr := BxAnother;  BxOpcodeInfo[$3B0].ExecutePtr:=bx_cpu.CMPXCHG_EbGb;
  BxOpcodeInfo[$3B1].Attr := BxAnother;  BxOpcodeInfo[$3B1].ExecutePtr:=bx_cpu.CMPXCHG_EdGd;
  BxOpcodeInfo[$3B2].Attr := BxAnother;  BxOpcodeInfo[$3B2].ExecutePtr:=bx_cpu.LSS_GvMp;
  BxOpcodeInfo[$3B3].Attr := BxAnother;  BxOpcodeInfo[$3B3].ExecutePtr:=bx_cpu.BTR_EvGv;
  BxOpcodeInfo[$3B4].Attr := BxAnother;  BxOpcodeInfo[$3B4].ExecutePtr:=bx_cpu.LFS_GvMp;
  BxOpcodeInfo[$3B5].Attr := BxAnother;  BxOpcodeInfo[$3B5].ExecutePtr:=bx_cpu.LGS_GvMp;
  BxOpcodeInfo[$3B6].Attr := BxAnother;  BxOpcodeInfo[$3B6].ExecutePtr:=bx_cpu.MOVZX_GdEb;
  BxOpcodeInfo[$3B7].Attr := BxAnother;  BxOpcodeInfo[$3B7].ExecutePtr:=bx_cpu.MOVZX_GdEw;
  BxOpcodeInfo[$3B8].Attr := 0;          BxOpcodeInfo[$3B8].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3B9].Attr := 0;          BxOpcodeInfo[$3B9].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3BA].Attr := BxAnother or BxGroup8;  BxOpcodeInfo[$3BA].ExecutePtr:=nil; BxOpcodeInfo[$3BA].AnotherArray:=@BxOpcodeInfoG8EvIb;
  BxOpcodeInfo[$3BB].Attr := BxAnother;  BxOpcodeInfo[$3BB].ExecutePtr:=bx_cpu.BTC_EvGv;
  BxOpcodeInfo[$3BC].Attr := BxAnother;  BxOpcodeInfo[$3BC].ExecutePtr:=bx_cpu.BSF_GvEv;
  BxOpcodeInfo[$3BD].Attr := BxAnother;  BxOpcodeInfo[$3BD].ExecutePtr:=bx_cpu.BSR_GvEv;
  BxOpcodeInfo[$3BE].Attr := BxAnother;  BxOpcodeInfo[$3BE].ExecutePtr:=bx_cpu.MOVSX_GdEb;
  BxOpcodeInfo[$3BF].Attr := BxAnother;  BxOpcodeInfo[$3BF].ExecutePtr:=bx_cpu.MOVSX_GdEw;
  BxOpcodeInfo[$3C0].Attr := BxAnother;  BxOpcodeInfo[$3C0].ExecutePtr:=bx_cpu.XADD_EbGb;
  BxOpcodeInfo[$3C1].Attr := BxAnother;  BxOpcodeInfo[$3C1].ExecutePtr:=bx_cpu.XADD_EdGd;
  BxOpcodeInfo[$3C2].Attr := 0;          BxOpcodeInfo[$3C2].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3C3].Attr := 0;          BxOpcodeInfo[$3C3].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3C4].Attr := 0;          BxOpcodeInfo[$3C4].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3C5].Attr := 0;          BxOpcodeInfo[$3C5].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3C6].Attr := 0;          BxOpcodeInfo[$3C6].ExecutePtr:=bx_cpu.BxError;
  BxOpcodeInfo[$3C7].Attr := BxAnother or BxGroup9;  BxOpcodeInfo[$3C7].ExecutePtr:=nil; BxOpcodeInfo[$3C7].AnotherArray:=@BxOpcodeInfoG9;
  BxOpcodeInfo[$3C8].Attr := 0;  BxOpcodeInfo[$3C8].ExecutePtr :=bx_cpu.BSWAP_EAX;
  BxOpcodeInfo[$3C9].Attr := 0;  BxOpcodeInfo[$3C9].ExecutePtr := bx_cpu.BSWAP_ECX;
  BxOpcodeInfo[$3CA].Attr := 0;  BxOpcodeInfo[$3CA].ExecutePtr := bx_cpu.BSWAP_EDX;
  BxOpcodeInfo[$3CB].Attr := 0;  BxOpcodeInfo[$3CB].ExecutePtr := bx_cpu.BSWAP_EBX;
  BxOpcodeInfo[$3CC].Attr := 0;  BxOpcodeInfo[$3CC].ExecutePtr := bx_cpu.BSWAP_ESP;
  BxOpcodeInfo[$3CD].Attr := 0;  BxOpcodeInfo[$3CD].ExecutePtr := bx_cpu.BSWAP_EBP;
  BxOpcodeInfo[$3CE].Attr := 0;  BxOpcodeInfo[$3CE].ExecutePtr := bx_cpu.BSWAP_ESI;
  BxOpcodeInfo[$3CF].Attr := 0;  BxOpcodeInfo[$3CF].ExecutePtr := bx_cpu.BSWAP_EDI;
  BxOpcodeInfo[$3D0].Attr := 0;  BxOpcodeInfo[$3D0].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D1].Attr := 0;  BxOpcodeInfo[$3D1].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D2].Attr := 0;  BxOpcodeInfo[$3D2].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D3].Attr := 0;  BxOpcodeInfo[$3D3].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D4].Attr := 0;  BxOpcodeInfo[$3D4].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D5].Attr := 0;  BxOpcodeInfo[$3D5].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D6].Attr := 0;  BxOpcodeInfo[$3D6].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D7].Attr := 0;  BxOpcodeInfo[$3D7].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D8].Attr := 0;  BxOpcodeInfo[$3D8].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3D9].Attr := 0;  BxOpcodeInfo[$3D9].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DA].Attr := 0;  BxOpcodeInfo[$3DA].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DB].Attr := 0;  BxOpcodeInfo[$3DB].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DC].Attr := 0;  BxOpcodeInfo[$3DC].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DD].Attr := 0;  BxOpcodeInfo[$3DD].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DE].Attr := 0;  BxOpcodeInfo[$3DE].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3DF].Attr := 0;  BxOpcodeInfo[$3DF].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E0].Attr := 0;  BxOpcodeInfo[$3E0].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E1].Attr := 0;  BxOpcodeInfo[$3E1].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E2].Attr := 0;  BxOpcodeInfo[$3E2].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E3].Attr := 0;  BxOpcodeInfo[$3E3].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E4].Attr := 0;  BxOpcodeInfo[$3E4].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E5].Attr := 0;  BxOpcodeInfo[$3E5].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E6].Attr := 0;  BxOpcodeInfo[$3E6].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E7].Attr := 0;  BxOpcodeInfo[$3E7].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E8].Attr := 0;  BxOpcodeInfo[$3E8].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3E9].Attr := 0;  BxOpcodeInfo[$3E9].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3EA].Attr := 0;  BxOpcodeInfo[$3EA].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3EB].Attr := 0;  BxOpcodeInfo[$3EB].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3EC].Attr := 0;  BxOpcodeInfo[$3EC].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3ED].Attr := 0;  BxOpcodeInfo[$3ED].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3EE].Attr := 0;  BxOpcodeInfo[$3EE].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3EF].Attr := 0;  BxOpcodeInfo[$3EF].ExecutePtr := bx_cpu.BxError;
  BxOpcodeInfo[$3F0].Attr := 0;  BxOpcodeInfo[$3F0].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F1].Attr := 0;  BxOpcodeInfo[$3F1].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F2].Attr := 0;  BxOpcodeInfo[$3F2].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F3].Attr := 0;  BxOpcodeInfo[$3F3].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F4].Attr := 0;  BxOpcodeInfo[$3F4].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F5].Attr := 0;  BxOpcodeInfo[$3F5].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F6].Attr := 0;  BxOpcodeInfo[$3F6].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F7].Attr := 0;  BxOpcodeInfo[$3F7].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F8].Attr := 0;  BxOpcodeInfo[$3F8].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3F9].Attr := 0;  BxOpcodeInfo[$3F9].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FA].Attr := 0;  BxOpcodeInfo[$3FA].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FB].Attr := 0;  BxOpcodeInfo[$3FB].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FC].Attr := 0;  BxOpcodeInfo[$3FC].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FD].Attr := 0;  BxOpcodeInfo[$3FD].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FE].Attr := 0;  BxOpcodeInfo[$3FE].ExecutePtr := bx_cpu.UndefinedOpcode;
  BxOpcodeInfo[$3FF].Attr := 0;  BxOpcodeInfo[$3FF].ExecutePtr := bx_cpu.UndefinedOpcode;
end;

function TPC_System.ticks_remaining(index:Integer):Int64;
begin
	Result := timer[index].remaining;
end;

procedure TPC_System.tickn(n:Bit64u);
begin
  ips_count := ips_count + n;
  if (num_cpu_ticks_left > n) then
  begin
    dec(num_cpu_ticks_left, n);
    exit;
  end;

  while (n >= num_cpu_ticks_left) do
  begin
    n := n - num_cpu_ticks_left;
    num_cpu_ticks_left := 0;
    timer_handler();
  end;
end;

constructor TPC_System.Create;
var
  I: Word;
begin
  //Self^.put('SYS');
  num_timers := 0;
  // set ticks period and remaining to max Bit32u value
  num_cpu_ticks_left := Bit32u(-1);
  num_cpu_ticks_in_period := num_cpu_ticks_left;
  m_ips := 0.0;
  for I:=0 to 8 do //for (unsigned int i:=0; i < 8; i++)
  begin
    DRQ[i] := 0;
    DACK[i] := 0;
  end;
  TC := 0;
  HRQ := 0;
  HLDA := 0;

  enable_a20 := 1;
  //set_INTR (0);

  a20_mask   := $ffffffff;

  counter := 0;
  init_ips(CPU_SPEED);
  COUNTER_INTERVAL := 100000;
  counter_timer_index := register_timer_ticks(Self, Self.counter_timer_handler, COUNTER_INTERVAL, 1, 1);
end;

procedure TPC_System.init_ips(ips: Bit32u);
begin
  // parameter 'ips' is the processor speed in Instructions-Per-Second
  m_ips := ips / 1000000.0;
end;

procedure TPC_System.raise_HLDA;
begin
  HLDA := 1;
  bx_devices.raise_hlda();
  HLDA := 0;
end;

  procedure
TPC_System.set_DRQ(channel:unsigned; val:bool);
begin
  if (channel > 7) then
    LogPanic(('set_DRQ() channel > 7'));

  DRQ[channel] := val;
  bx_devices.drq(channel, val);
end;

procedure TPC_System.set_HRQ(val:bool);
begin
  HRQ := val;
  if (val) <> 0 then
    bx_cpu.async_event := 1
  else
    HLDA := 0; // ??? needed?
end;

procedure TPC_System.set_TC(val:bool);
begin
  TC := val;
end;

procedure TPC_System.set_DACK(channel:unsigned; val:Bool);
begin
  DACK[channel] := val;
end;

procedure TPC_System.dma_write8(phy_addr:Bit32u; channel:unsigned; verify:Bool);
var
  data_byte: Bit8u;
begin
  // DMA controlled xfer of byte from I/O to Memory
  bx_devices.dma_write8(channel, @data_byte);
  if (verify = 0) then
    sysmemory.write_physical(phy_addr, 1, @data_byte);
    //BX_DBG_DMA_REPORT(phy_addr, 1, BX_WRITE, data_byte);
end;

procedure TPC_System.dma_read8(phy_addr:Bit32u; channel:unsigned);
var
  data_byte: Bit8u;
begin
  // DMA controlled xfer of byte from Memory to I/O
  sysmemory.read_physical(phy_addr, 1, @data_byte);
  bx_devices.dma_read8(channel, @data_byte);
  //BX_DBG_DMA_REPORT(phy_addr, 1, BX_READ, data_byte);
end;

procedure TPC_System.dma_write16(phy_addr:Bit32u; channel:unsigned; verify:Bool);
var
  data_word: Bit16u;
begin
  // DMA controlled xfer of word from I/O to Memory
  bx_devices.dma_write16(channel, @data_word);
  if (verify = 0) then
    sysmemory.write_physical(phy_addr, 2, @data_word);
    //BX_DBG_DMA_REPORT(phy_addr, 2, BX_WRITE, data_word);
end;

procedure TPC_System.dma_read16(phy_addr:Bit32u; channel:unsigned);
var
  data_word: Bit16u;
begin
  // DMA controlled xfer of word from Memory to I/O
  sysmemory.read_physical(phy_addr, 2, @data_word);
  bx_devices.dma_read16(channel, @data_word);
  //BX_DBG_DMA_REPORT(phy_addr, 2, BX_READ, data_word);
end;

procedure TPC_System.set_INTR(value:bool);
begin
  //INTR := value;
  bx_cpu.set_INTR(value);
end;
//
// Read from the IO memory address space
//
function TPC_System.inp(addr:Bit16u; io_len:unsigned):Bit32u;
begin
  Result:= bx_devices.inp(addr, io_len);
end;
//
// Write to the IO memory address space.
//
procedure TPC_System.outp(addr:Bit16u; value:Bit32u; io_len:unsigned);
begin
  bx_devices.outp(addr, value, io_len);
end;

procedure TPC_System.set_enable_a20(value:Bit8u);
begin
  if (value) <> 0 then
  begin
    enable_a20 := 1;
    a20_mask   := $ffffffff; (* 386: enable all 32 address lines *)
  end else
  begin
    enable_a20 := 0;
    a20_mask   := $ffefffff;   (* mask off A20 address line *)
  end;
end;

function TPC_System.get_enable_a20: Bool;
begin
  result := Bool(enable_a20 <> 0);
end;

function TPC_System.ResetSignal( operation:PCS_OP ): Integer;
begin
  //UNUSED( operation );
  // Reset the processor.
  {for (int i:=0; i<BX_SMP_PROCESSORS; i++)
    BX_CPU(i)^.reset(BX_RESET_SOFTWARE);}
  Result := 0;
end;

function TPC_System.IAC:Bit8u;
begin
  Result := bx_pic.IAC();
end;

procedure TPC_System.exit;
begin
{  if bx_devices.hard_drive) then
    bx_devices.hard_drive^.close_harddrive();}
  //BX_INFO(Format('Last time is %d',[bx_cmos.s.timeval])); !!! MANCA
  //bx_gui.exit();
end;
//
// bochs timer support
//
procedure TPC_System.timer_handler;
var
  min: Bit64u;
  i: unsigned;
  delta: Bit64u;
begin
  //  BX_ERROR(( 'Time handler ptime := %d', bx_pc_system.time_ticks() ));
  delta := num_cpu_ticks_in_period - num_cpu_ticks_left;

  min := LongWord(-1); // max number in Bit64u range

  for i := 0 to num_timers do
  begin
    timer[i].triggered := 0;
    if (timer[i].active) <> 0 then
    begin
      timer[i].remaining := timer[i].remaining - delta;
      if (timer[i].remaining = 0) then
      begin
        timer[i].triggered := 1;
        // reset remaining period for triggered timer
        timer[i].remaining := timer[i].period;

        // if triggered timer is one-shot, deactive
        if (timer[i].continuous = 0) then
          timer[i].active := 0;
      end;
    end;
    ////
    if ((timer[i].active <> 0) and (timer[i].remaining < min)) then
      min := timer[i].remaining;
    ////
  end;

//  min := LongWord(-1); // max number in Bit64u range
//  for i := 0 to num_timers do
//    if ((timer[i].active <> 0) and (timer[i].remaining < min)) then
//      min := timer[i].remaining;

  num_cpu_ticks_left := min;
  num_cpu_ticks_in_period := min;

  for  i := 0 to num_timers do
    // call requested timer function.  It may request a different
    // timer period or deactivate, all cases handled below
    if (timer[i].triggered) <> 0 then
      timer[i].funct(timer[i].this_ptr);
end;

procedure TPC_System.expire_ticks;
var
  i: unsigned;
  ticks_delta: Bit64u;
begin
  ticks_delta := num_cpu_ticks_in_period - num_cpu_ticks_left;
  if (ticks_delta = 0) then
    exit; // no ticks occurred since

  for i := 0 to num_timers do
    if (timer[i].active) <> 0 then
      timer[i].remaining := timer[i].remaining - ticks_delta; // must be >= 1 here

  // set new period to number of ticks left
  num_cpu_ticks_in_period := num_cpu_ticks_left;
end;

function TPC_System.register_timer(this_ptr: Pointer; funct: TTimer_handler_t;
  useconds: Bit32u; continuous: Bool; active: Bool):Integer;
var
  instructions: Bit64u;
begin
  // account for ticks up to now
  expire_ticks();
  // convert useconds to number of instructions
  instructions := Trunc(useconds * m_ips);
  if((useconds <> 0) and (instructions = 0)) then
    instructions := 1;

  Result:= register_timer_ticks(this_ptr, funct, instructions, continuous, active);
end;

function TPC_System.register_timer_ticks(this_ptr: Pointer; funct: TTimer_handler_t;
  Instructions: Bit64u; continuous: Bool; active: Bool):Integer;
var
  i: Word;
begin
  i := num_timers;
  inc(num_timers);
  timer[i].period     := instructions;
  timer[i].remaining  := instructions;
  timer[i].active     := active;
  timer[i].funct      := funct;
  timer[i].continuous := continuous;
  timer[i].this_ptr   := this_ptr;

  if active <> 0 then
  begin
    if (num_cpu_ticks_in_period = 0) then
    begin
      // no active timers
      num_cpu_ticks_in_period := instructions;
      num_cpu_ticks_left      := instructions;
    end
    else
    if (instructions < num_cpu_ticks_left) then
    begin
      num_cpu_ticks_in_period := instructions;
      num_cpu_ticks_left      := instructions;
    end;
  end;
  // return timer id
  Result := i;
end;

procedure TPC_System.counter_timer_handler(this_ptr: Pointer);
begin
  Inc(Self.counter);
end;

function TPC_System.time_usec: Bit64u;
begin
  Result:= Trunc(time_ticks() / m_ips );
end;

function TPC_System.time_ticks: Bit64u;
begin
  Result:= (counter + 1) * COUNTER_INTERVAL - ticks_remaining(counter_timer_index)
         + (Bit64u(num_cpu_ticks_in_period) - Bit64u(num_cpu_ticks_left));
end;

//procedure TPC_SYSTEM.start_timers;
//begin
//  // nope
//end;

procedure TPC_System.activate_timer_ticks (timer_index: unsigned; instructions: Bit64u; continuous: Bool);
begin
  if (timer_index >= num_timers) then
    LogPanic(('activate_timer(): bad timer index given'));

  // set timer continuity to new value (1:=continuous, 0:=one-shot)
  timer[timer_index].continuous := continuous;

  timer[timer_index].active    := 1;
  timer[timer_index].remaining := instructions;

  if (num_cpu_ticks_in_period = 0) then
  begin
    // no active timers
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
  end
  else
  if (instructions < num_cpu_ticks_left) then
  begin
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
  end;
end;

procedure TPC_System.activate_timer( timer_index: unsigned; useconds: Bit32u; continuous: Bool);
var
  instructions: Bit64u;
begin
  // account for ticks up to now
  expire_ticks();
  // set timer continuity to new value (1:=continuous, 0:=one-shot)
  timer[timer_index].continuous := continuous;
  // if useconds := 0, use default stored in period field
  // else set new period from useconds
  if (useconds = 0) then
    instructions := timer[timer_index].period
  else
  begin
    // convert useconds to number of instructions
    instructions := Trunc(useconds * m_ips);
    if (instructions = 0) then
      instructions := 1;
    timer[timer_index].period := instructions;
  end;

  timer[timer_index].active    := 1;
  timer[timer_index].remaining := instructions;

  if (num_cpu_ticks_in_period = 0) then
  begin
    // no active timers
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
  end
  else
  if (instructions < num_cpu_ticks_left) then
  begin
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
  end;
end;

procedure TPC_System.deactivate_timer( timer_index: unsigned );
begin
  if (timer_index >= num_timers) then
    LogPanic(('deactivate_timer(): bad timer index given'));

  timer[timer_index].active := 0;
end;

//procedure TCPU.Prova(Valore: PInteger);
//begin
//  // nope
//end;

procedure TCPU.set_CF(val: Bool);
begin
  self.eflags.cf := val;
  self.lf_flags_status := self.lf_flags_status and $fffff0;
end;

procedure TCPU.set_AF(val: Bool);
begin
  self.eflags.af := val;
  self.lf_flags_status := self.lf_flags_status and $fff0ff;
end;

procedure TCPU.set_ZF(val: Bool);
begin
  self.eflags.zf := val;
  self.lf_flags_status := self.lf_flags_status and $ff0fff;
end;

procedure TCPU.set_SF(val: Bool);
begin
  self.eflags.sf := val;
  self.lf_flags_status := self.lf_flags_status and $f0ffff;
end;

procedure TCPU.set_OF(val: Bool);
begin
  self.eflags.of_ := val;
  self.lf_flags_status := self.lf_flags_status and $0fffff;
end;

procedure TCPU.set_PF(val: Bool);
begin
  self.lf_pf := val;
  self.lf_flags_status := self.lf_flags_status and $ffff0f;
end;

procedure TCPU.set_PF_base(val: Bit8u);
begin
  self.eflags.pf_byte := val;
  self.lf_flags_status := (self.lf_flags_status and $ffff0f) or BX_LF_MASK_P;
end;

procedure TCPU.SET_FLAGS_OSZAPC_8(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);
begin
  oszapc.op1_8    := op1;
  oszapc.op2_8    := op2;
  oszapc.result_8 := result;
  oszapc.instr    := ins;
  lf_flags_status := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.SET_FLAGS_OSZAPC_8_CF(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word; last_CF: Bool);
begin
  oszapc.op1_8    := op1;
  oszapc.op2_8    := op2;
  oszapc.result_8 := result;
  oszapc.instr    := ins;
  oszapc.prev_CF  := last_CF;
  lf_flags_status := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.SET_FLAGS_OSZAP_8(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);
begin
  oszap.op1_8     := op1;
  oszap.op2_8     := op2;
  oszap.result_8  := result;
  oszap.instr     := ins;
  lf_flags_status := (lf_flags_status and $00000f) or BX_LF_MASK_OSZAP;
end;

procedure TCPU.SET_FLAGS_OSZAP_32(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);  //INLINE
begin
  oszap.op1_32    := op1;
  oszap.op2_32    := op2;
  oszap.result_32 := result;
  oszap.instr     := ins;
  lf_flags_status := (lf_flags_status and $00000f) or BX_LF_MASK_OSZAP;
end;

procedure TCPU.SET_FLAGS_OSZAPC_32(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);
begin
  oszapc.op1_32     := op1;
  oszapc.op2_32     := op2;
  oszapc.result_32  := result;
  oszapc.instr      := ins;
  lf_flags_status   := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.SET_FLAGS_OSZAPC_32_CF(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word; last_CF: Bool);
begin
  oszapc.op1_32     := op1;
  oszapc.op2_32     := op2;
  oszapc.result_32  := result;
  oszapc.instr      := ins;
  oszapc.prev_CF    := last_CF;
  lf_flags_status   := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.BX_WRITE_32BIT_REG(index: Word; val: bit32u);
begin
  gen_reg[index].erx := val;
end;

procedure TCPU.SET_FLAGS_OSZAPC_16(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);
begin
  oszapc.op1_16     := op1;
  oszapc.op2_16     := op2;
  oszapc.result_16  := result;
  oszapc.instr      := ins;
  lf_flags_status   := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.SET_FLAGS_OSZAPC_16_CF(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word; last_CF: Bool);
begin
  oszapc.op1_16     := op1;
  oszapc.op2_16     := op2;
  oszapc.result_16  := result;
  oszapc.instr      := ins;
  oszapc.prev_CF    := last_CF;
  lf_flags_status   := BX_LF_MASK_OSZAPC;
end;

procedure TCPU.SET_FLAGS_OSZAP_16(op1: Bit32u; op2: Bit32u; result: Bit32u;
  ins: Word);
begin
  oszap.op1_16    := op1;
  oszap.op2_16    := op2;
  oszap.result_16 := result;
  oszap.instr     := ins;
  lf_flags_status := (lf_flags_status and $00000f) or BX_LF_MASK_OSZAP;
end;

procedure TCPU.SET_FLAGS_OxxxxC(new_of, new_cf: Bool);
begin
  eflags.of_      := new_of;
  eflags.cf       :=  new_cf;
  lf_flags_status := lf_flags_status and $0ffff0;
end;

procedure TCPU.TLB_flush;
var
  I: Word;
begin
  i := 0;
  while i < BX_TLB_SIZE do
  begin
    TLB.entry[i].lpf := BX_INVALID_TLB_ENTRY;
    Inc(i);
  end;

  invalidate_prefetch_q();
end;

procedure TCPU.TLB_clear;
var
  I: Word;
begin
  for i := 0 to BX_TLB_SIZE do {BX_TLB_SIZE -1 ???}
    TLB.entry[i].lpf := BX_INVALID_TLB_ENTRY;
end;

procedure TCPU.TLB_init;
var
  i: Word;
  us_combined, rw_combined, us_current, rw_current: Word;
begin
  for i := 0 to BX_TLB_SIZE do
    TLB.entry[i].lpf := BX_INVALID_TLB_ENTRY;
  //
  // Setup privilege check matrix.
  //
  for i:=0 to BX_PRIV_CHECK_SIZE do
  begin
    us_current  := (i and $08) shr 3;
    us_combined := (i and $04) shr 2;
    rw_combined := (i and $02) shr 1;
    rw_current  := (i and $01) shr 0;

    if Boolean((i and $10) shr 4) then
    begin // when write protect on

      if us_current > us_combined then
        // user access, supervisor page
        priv_check_array[i] := 0
      else
        // RW access, RO page
        priv_check_array[i] := integer(rw_current <= rw_combined );
    end else
    begin
       // when write protect off
      if us_current = 0 then
        // Supervisor mode access, anything goes
        priv_check_array[i] := 1
      else
      begin
        // user mode access
        if us_combined = 0 then
          // user access, supervisor Page
          priv_check_array[i] := 0
        else
          // RW access, RO page
          priv_check_array[i] := integer(rw_current <= rw_combined);
      end;
    end;
  end;
end;

procedure TCPU.INVLPG(Instruction: PInstruction_tag);
begin
  invalidate_prefetch_q();
  // Operand must not be a register
  if (Instruction.mod_ = $c0) then
    UndefinedOpcode(Instruction);
  // Can not be executed in v8086 mode
  if Boolean(v8086_mode()) then
    exception(BX_GP_EXCEPTION, 0, 0);

  // Protected instruction: CPL0 only
  if Boolean(FCR0.pe) then
    if (CPL <> 0) then
      exception2([BX_GP_EXCEPTION, 0, 0]);
  // Just clear the entire TLB, ugh!
  TLB_clear();
  //BX_INSTR_TLB_CNTRL(BX_INSTR_INVLPG, 0); !!!vedere sorgente
end;

procedure TCPU.enable_paging;
begin
  TLB_flush();
  //if bx_dbg.paging then BX_INFO('enable_paging():');
//BX_DEBUG(( "enable_paging():-------------------------" ));
end;

procedure TCPU.disable_paging;
begin
  TLB_flush();
  //if bx_dbg.paging then BX_INFO('enable_paging():');
//BX_DEBUG(( "enable_paging():-------------------------" ));
end;

procedure TCPU.CR3_change(value32:Bit32u);
begin
{  if bx_dbg.paging then

    begin

      BX_INFO('CR3_change(): flush TLB cache');
      BX_INFO(Format('Page Directory Base %08x', value32));
    end;}
  // flush TLB even if value does not change
  TLB_flush();
  FCR3 := value32;
end;

function TCPU.itranslate_linear(laddress: Bit32u; pl: unsigned): Bit32u;
var
  lpf, ppf,
  poffset, TLB_index,
  error_code, paddress: Bit32u;
  pde, pde_addr: Bit32u;
  pte, pte_addr: Bit32u;
  priv_index: Word;
  combined_access: Bit32u;
  label priv_check, page_fault;
begin
  lpf       := laddress and $fffff000; // linear page frame
  poffset   := laddress and $00000fff; // physical offset
  TLB_index := (lpf and $003ff000) shr 12;

  if TLB.entry[TLB_index].lpf = lpf then
  begin
    paddress        := TLB.entry[TLB_index].ppf or poffset;
    combined_access := TLB.entry[TLB_index].combined_access;
priv_check:
    priv_index := (FCR0.wp shl 4) or               // bit 4
                  (pl shl 3) or                   // bit 3
                  (combined_access and $06);      // bit 2,1
                                                  // bit 0 == 0
    if Boolean(priv_check_array[priv_index]) then
    begin
      // Operation has proper privilege.
      Result := paddress;
      exit;
    end;
    error_code := $fffffff9; // RSVD=1, P=1
    goto page_fault;
  end;
  // Get page dir entry
  pde_addr := (FCR3 and $fffff000) or ((laddress and $ffc00000) shr 20);
  sysmemory.read_physical(pde_addr, 4, @pde);
  if (pde and $01) = 0 then
  begin
    // Page Directory Entry NOT present
    error_code := $fffffff8; // RSVD=1, P=0
    goto page_fault;
  end;
  // Get page table entry
  pte_addr := (pde and $fffff000) or ((laddress and $003ff000) shr 10);
  sysmemory.read_physical(pte_addr, 4, @pte);

  // update PDE if A bit was not set before
  if (pde and $20) = 0 then
  begin
    pde := pde or $20;
    sysmemory.write_physical(pde_addr, 4, @pde);
  end;

  if (pte and $01) = 0 then
  begin
    // Page Table Entry NOT present
    error_code := $fffffff8; // RSVD=1, P=0
    goto page_fault;
  end;
  //BW added: update PTE if A bit was not set before
  if (pte and $20) = 0 then
  begin
    pte := pte or $20;
    sysmemory.write_physical(pte_addr, 4, @pte);
  end;
  // 386 and 486+ have different bahaviour for combining
  // privilege from PDE and PTE.
  combined_access  := (pde and pte) and $06; // U/S and R/W

  ppf := pte and $fffff000;
  paddress := ppf or poffset;

  TLB.entry[TLB_index].lpf := lpf;
  TLB.entry[TLB_index].ppf := ppf;
  TLB.entry[TLB_index].pte_addr := pte_addr;
  TLB.entry[TLB_index].combined_access := combined_access;
  goto priv_check;
page_fault:
  error_code :=error_code or (pl shl 2);
  FCR2 := laddress;
  // invalidate entry - we can get away without maintaining A bit in PTE
  // if we don't maintain TLB entries without it set.
  TLB.entry[TLB_index].lpf := BX_INVALID_TLB_ENTRY;
  exception(BX_PF_EXCEPTION, error_code, 0);
end;

function TCPU.dtranslate_linear(laddress: Bit32u; pl: unsigned; rw: unsigned): Bit32u;
var
  lpf, ppf,
  poffset, TLB_index,
  error_code, paddress: Bit32u;
  pde, pde_addr: Bit32u;
  pte, pte_addr: Bit32u;
  priv_index: unsigned;
  is_rw: Bool;
  combined_access, new_combined_access: Bit32u;
  label priv_check, page_fault_check, page_fault_not_present, page_fault_proper;
begin
  lpf       := laddress and $fffff000; // linear page frame
  poffset   := laddress and $00000fff; // physical offset
  TLB_index := (lpf and $003ff000) shr 12;
  is_rw := Bool((rw >= BX_WRITE)); // write or r-m-w

  if (TLB.entry[TLB_index].lpf = lpf) then
  begin
    paddress        := TLB.entry[TLB_index].ppf or poffset;
    combined_access := TLB.entry[TLB_index].combined_access;
priv_check:
    priv_index := (FCR0.wp shl 4) or  // bit 4
                  (pl shl 3) or                  // bit 3
                  (combined_access and $06) or   // bit 2,1
                  is_rw;                         // bit 0
    if (Boolean(priv_check_array[priv_index])) then
    begin
      // Operation has proper privilege.
      // See if A/D bits need updating.
      //BW !! a read access does not do any updates, patched load
      new_combined_access := combined_access or is_rw;

      if new_combined_access = combined_access then
        exit(paddress); // A/D bits already up-to-date

      TLB.entry[TLB_index].combined_access := new_combined_access;
      pte_addr := TLB.entry[TLB_index].pte_addr;
      sysmemory.read_physical(pte_addr, 4, @pte); // get old PTE
      pte := pte or $20 or (is_rw shl 6);
      sysmemory.write_physical(pte_addr, 4, @pte); // write updated PTE
      exit(paddress);
    end;
    error_code := $fffffff9; // RSVD=1, P=1
    goto page_fault_check;
  end;
  // Get page dir entry
  pde_addr := (FCR3 and $fffff000) or ((laddress and $ffc00000) shr 20);
  sysmemory.read_physical(pde_addr, 4, @pde);

  if (pde and $01) = 0  then
  begin
    // Page Directory Entry NOT present
    error_code := $fffffff8; // RSVD=1, P=0
    goto page_fault_not_present;
  end;
  // Get page table entry
  pte_addr := (pde and $fffff000) or ((laddress and $003ff000) shr 10);
  sysmemory.read_physical(pte_addr, 4, @pte);
  // update PDE if A bit was not set before
  if (pde and $20) = 0 then
  begin
    pde := pde or $20;
    sysmemory.write_physical(pde_addr, 4, @pde);
  end;
  if ( (pte and $01)=0 ) then
  begin
    // Page Table Entry NOT present
    error_code := $fffffff8; // RSVD=1, P=0
    goto page_fault_not_present;
  end;
  //BW added: update PTE if A bit was not set before
  if (pte and $20) = 0 then
  begin
    pte := pte or $20;
    sysmemory.write_physical(pte_addr, 4, @pte);
  end;
  // 386 and 486+ have different bahaviour for combining
  // privilege from PDE and PTE.

  combined_access  := (pde and pte) and $06; // U/S and R/W

  ppf := pte and $fffff000;
  paddress := ppf or poffset;

  TLB.entry[TLB_index].lpf := lpf;
  TLB.entry[TLB_index].ppf := ppf;
  TLB.entry[TLB_index].pte_addr := pte_addr;
  TLB.entry[TLB_index].combined_access := combined_access;
  goto priv_check;

page_fault_check:
  // (mch) Define RMW_WRITES for old behavior
  {$ifndef RMW_WRITES}
    (* (mch) Ok, so we know it's a page fault. It the access is a
       read-modify-write access we check if the read faults, if it
       does then we (optionally) do not set the write bit *)
  if rw = BX_RW then
  begin
    priv_index := (FCR0.wp shl 4) or             // bit 4
                  (pl shl 3) or                 // bit 3
                  (combined_access and $06) or  // bit 2,1
                  0;                            // bit 0 (read)
    if (priv_check_array[priv_index] = 0) then
      is_rw := 0; // Fault on read
  end;
  {$endif} // RMW_WRITES
  goto page_fault_proper;
page_fault_not_present:
  {$ifndef RMW_WRITES}
  if (rw = BX_RW) then
    is_rw := 0;
  {$endif} // RMW_WRITES
  goto page_fault_proper;
page_fault_proper:
  error_code := error_code or (pl shl 2) or (is_rw shl 1);
  FCR2 := laddress;
  // invalidate entry - we can get away without maintaining A bit in PTE
  // if we don't maintain TLB entries without it set.
  TLB.entry[TLB_index].lpf := BX_INVALID_TLB_ENTRY;
  exception(BX_PF_EXCEPTION, error_code, 0);
end;

procedure TCPU.access_linear(const laddress: Bit32u; const length: unsigned;
  const pl: unsigned; rw: unsigned; data: Pointer);
var
  mod4096: Bit32u;
  xlate_rw: Word;
begin
  if rw = BX_RW then
  begin
    xlate_rw := BX_RW;
    rw := BX_READ;
  end else
    xlate_rw := rw;

  if FCR0.pg <> 0 then
  begin
    // check for reference across multiple pages
    mod4096 := laddress and $00000fff;
    if (mod4096 + length) <= 4096 then
    begin
      // Bit32u paddress1;
      // access within single page
      address_xlation.paddress1 := dtranslate_linear(laddress, pl, xlate_rw);
      address_xlation.pages     := 1;
      if rw = BX_READ then
        sysmemory.read_physical(address_xlation.paddress1, length, data)
      else
        sysmemory.write_physical(address_xlation.paddress1, length, data);
      exit;
    end else
    begin
      // access across 2 pages
      address_xlation.paddress1 := dtranslate_linear(laddress, pl, xlate_rw);
      address_xlation.len1      := 4096 - mod4096;
      address_xlation.len2      := length - address_xlation.len1;
      address_xlation.pages     := 2;

      address_xlation.paddress2 := dtranslate_linear(laddress + address_xlation.len1, pl, xlate_rw);
      //VEDERE CODICE ORIGINALE
      if rw = BX_READ then
      begin
        sysmemory.read_physical(address_xlation.paddress1,
                                address_xlation.len1, data);
        sysmemory.read_physical(address_xlation.paddress2,
                                address_xlation.len2,
                                PBit8u(Integer(data) + address_xlation.len1));
      end else
      begin
        sysmemory.write_physical(address_xlation.paddress1,
                                 address_xlation.len1, data);
        sysmemory.write_physical(address_xlation.paddress2,
                                 address_xlation.len2,
                                 PBit8u(Integer(data) + address_xlation.len1));
      end;
    end;
   end else
  begin
    // paging off, pass linear address thru to physical
    if rw = BX_READ then
      sysmemory.read_physical(laddress, length, data)
    else
      sysmemory.write_physical(laddress, length, data);
    exit;
  end;
end;

procedure TCPU.ADD_EbGb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
begin
  // op2 is a register, i^.rm_addr is an index of a register
  op2 := BX_READ_8BIT_REG(i^.nnn);
  // op1 is a register or memory reference
  if i^.mod_ = $c0 then
    op1 := BX_READ_8BIT_REG(i^.rm)
  else
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  sum := op1 + op2;

  // now write sum back to destination
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, sum)
  else
    write_RMW_virtual_byte(sum);

  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_ADD8);
end;

procedure TCPU.ADD_GbEb(I: PInstruction_tag);
var
  op1, op2, sum: Bit8u;
begin
  (* op1 is a register, i^.rm_addr is an index of a register *)
  op1 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if i^.mod_ = $c0
    then op2 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  sum := op1 + op2;
  (* now write sum back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, sum);
  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_ADD8);
end;

procedure TCPU.ADD_ALIb(I: PInstruction_tag);
var
  op1, op2, sum: Bit8u;
begin
  op1 := AL; //AL
  op2 := i^.Ib;
  sum := op1 + op2;
  (* now write sum back to destination, which is a register *)
  gen_reg[0].rl := sum; //AL
  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_ADD8);
end;

procedure TCPU.ADC_EbGb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  (* op2 is a register, i^.rm_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if i^.mod_ = $c0
    then op1 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  sum := op1 + op2 + temp_CF;
  (* now write sum back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, sum)
  else
    write_RMW_virtual_byte(sum);

  SET_FLAGS_OSZAPC_8_CF(op1, op2, sum, BX_INSTR_ADC8, temp_CF);
end;

procedure TCPU.ADC_GbEb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  (* op1 is a register, i^.rm_addr is an index of a register *)
  op1 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if i^.mod_ = $c0
    then op2 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  sum := op1 + op2 + temp_CF;
  SET_FLAGS_OSZAPC_8_CF(op1, op2, sum, BX_INSTR_ADC8, temp_CF);
  (* now write sum back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, sum);
end;

procedure TCPU.ADC_ALIb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  op1 := AL;
  op2 := i^.Ib;
  sum := op1 + op2 + temp_CF;
  (* now write sum back to destination, which is a register *)
  AL := sum;
  SET_FLAGS_OSZAPC_8_CF(op1, op2, sum, BX_INSTR_ADC8, temp_CF);
end;

procedure TCPU.SBB_EbGb(I: PInstruction_tag);
var
  op2_8, op1_8, diff_8: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  (* op2 is a register, i^.rm_addr is an index of a register *)
  op2_8 := BX_READ_8BIT_REG(i^.nnn);
  (* op1_8 is a register or memory reference *)
  if i^.mod_ = $c0
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := op1_8 - (op2_8 + temp_CF);
  (* now write diff back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, diff_8)
  else
    write_RMW_virtual_byte(diff_8);

  SET_FLAGS_OSZAPC_8_CF(op1_8, op2_8, diff_8, BX_INSTR_SBB8, temp_CF);
end;

procedure TCPU.SBB_GbEb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  (* op1 is a register, i^.rm_addr is an index of a register *)
  op1_8 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if i^.mod_ = $c0
    then op2_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);

  diff_8 := op1_8 - (op2_8 + temp_CF);
  (* now write diff back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, diff_8);
  SET_FLAGS_OSZAPC_8_CF(op1_8, op2_8, diff_8, BX_INSTR_SBB8, temp_CF);
end;

procedure TCPU.SBB_ALIb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  op1_8 := AL;
  op2_8 := i^.Ib;
  diff_8 := op1_8 - (op2_8 + temp_CF);
  (* now write diff back to destination, which is a register *)
  AL := diff_8;
  SET_FLAGS_OSZAPC_8_CF(op1_8, op2_8, diff_8, BX_INSTR_SBB8, temp_CF);
end;

procedure TCPU.SBB_EbIb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  op2_8 := i^.Ib;
  (* op1_8 is a register or memory reference *)
  if i^.mod_ = $c0
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := op1_8 - (op2_8 + temp_CF);
  (* now write diff back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, diff_8)
  else
    write_RMW_virtual_byte(diff_8);

  SET_FLAGS_OSZAPC_8_CF(op1_8, op2_8, diff_8, BX_INSTR_SBB8, temp_CF);
end;

procedure TCPU.SUB_EbGb(I: PInstruction_tag);
var
  op2_8, op1_8, diff_8: Bit8u;
begin
  (* op2 is a register, i^.rm_addr is an index of a register *)
  op2_8 := BX_READ_8BIT_REG(i^.nnn);
  (* op1_8 is a register or memory reference *)
  if i^.mod_ = $c0
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := op1_8 - op2_8;
  (* now write diff back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, diff_8)
  else
    write_RMW_virtual_byte(diff_8);

  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SUB8);
end;

procedure TCPU.SUB_GbEb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
begin
  (* op1 is a register, i^.rm_addr is an index of a register *)
  op1_8 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if i^.mod_ = $c0
    then op2_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);

  diff_8 := op1_8 - op2_8;
  (* now write diff back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, diff_8);
  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SUB8);
end;

procedure TCPU.SUB_ALIb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
begin
  op1_8 := AL;
  op2_8 := i^.Ib;
  diff_8 := op1_8 - op2_8;
  (* now write diff back to destination, which is a register *)
  AL := diff_8;
  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SUB8);
end;

procedure TCPU.CMP_EbGb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
begin
  (* op2 is a register, i^.rm_addr is an index of a register *)
  op2_8 := BX_READ_8BIT_REG(i^.nnn);
  (* op1_8 is a register or memory reference *)
  if i^.mod_ = $c0
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := op1_8 - op2_8;
  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMP8);
end;

procedure TCPU.CMP_GbEb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8:Bit8u;
begin
  (* op1 is a register, i^.rm_addr is an index of a register *)
  op1_8 := BX_READ_8BIT_REG(i^.nnn);

  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_8 := BX_READ_8BIT_REG(i^.rm)
  else
    read_virtual_byte(i^.seg, i^.rm_addr, @op2_8); (* pointer, segment address pair *)

  diff_8 := op1_8 - op2_8;
  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMP8);
end;

procedure TCPU.CMP_ALIb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8:Bit8u;
begin
  op1_8 := AL;
  op2_8 := i^.Ib;
  diff_8 := op1_8 - op2_8;
  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMP8);
end;


procedure TCPU.XADD_EbGb(I: PInstruction_tag);
var
  op2, op1, sum:Bit8u;
begin
  (* XADD dst(r/m8), src(r8)
   * temp <-- src + dst         | sum = op2 + op1
   * src  <-- dst               | op2 = op1
   * dst  <-- tmp               | op1 = sum
   *)

  (* op2 is a register, i^.rm_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);

  (* op1 is a register or memory reference *)
  if i^.mod_ = $c0 then
    op1 := BX_READ_8BIT_REG(i^.rm)
  else
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1); (* pointer, segment address pair *)

  sum := op1 + op2;

  (* now write sum back to destination *)
  if i^.mod_ = $c0 then
  begin
    // and write destination into source
    // Note: if both op1 @ op2 are registers, the last one written
    //       should be the sum, as op1 @ op2 may be the same register.
    //       For example:  XADD AL, AL
    BX_WRITE_8BIT_REG(i^.nnn, op1);
    BX_WRITE_8BIT_REG(i^.rm, sum);
  end else
  begin
    write_RMW_virtual_byte(sum);
    (* and write destination into source *)
    BX_WRITE_8BIT_REG(i^.nnn, op1);
  end;

  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_XADD8);
end;

procedure TCPU.ADD_EbIb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
begin
  op2 := i^.Ib;
  (* op1 is a register or memory reference *)
  if i^.mod_ = $c0 then
    op1 := BX_READ_8BIT_REG(i^.rm)
  else
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1); (* pointer, segment address pair *)

  sum := op1 + op2;

  (* now write sum back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, sum)
  else
    write_RMW_virtual_byte(sum);

  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_ADD8);
end;

procedure TCPU.ADC_EbIb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
  temp_CF: Bool;
begin
  temp_CF := get_CF();
  op2 := i^.Ib;

  (* op1 is a register or memory reference *)
  if i^.mod_ = $c0 then
    op1 := BX_READ_8BIT_REG(i^.rm)
  else
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1); (* pointer, segment address pair *)

  sum := op1 + op2 + temp_CF;

  (* now write sum back to destination *)
  if i^.mod_ = $c0 then
    BX_WRITE_8BIT_REG(i^.rm, sum)
  else
    write_RMW_virtual_byte(sum);

  SET_FLAGS_OSZAPC_8_CF(op1, op2, sum, BX_INSTR_ADC8, temp_CF);
end;

procedure TCPU.SUB_EbIb(I: PInstruction_tag);
var
  op2_8, op1_8, diff_8: Bit8u;
begin
  op2_8 := i^.Ib;
  (* op1_8 is a register or memory reference *)
  if (i^.mod_ = $c0) then begin
    op1_8 := BX_READ_8BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);
    end;

  diff_8 := op1_8 - op2_8;

  (* now write diff back to destination *)
  if (i^.mod_ = $c0) then begin
    BX_WRITE_8BIT_REG(i^.rm, diff_8);
    end
  else begin
    write_RMW_virtual_byte(diff_8);
    end;

  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SUB8);
end;

procedure TCPU.CMP_EbIb(I: PInstruction_tag);
var
  op2_8, op1_8, diff_8:Bit8u;
begin

  op2_8 := i^.Ib;

  (* op1_8 is a register or memory reference *)
  if (i^.mod_ = $c0) then begin
    op1_8 := BX_READ_8BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_byte(i^.seg, i^.rm_addr, @op1_8);
    end;

  diff_8 := op1_8 - op2_8;

  SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMP8);
end;


procedure TCPU.NEG_Eb(I: PInstruction_tag);
var
  op1_8, diff_8:Bit8u;
begin
  (* op1_8 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := 0 - op1_8;
  (* now write diff back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, diff_8)
    else write_RMW_virtual_byte(diff_8);

  SET_FLAGS_OSZAPC_8(op1_8, 0, diff_8, BX_INSTR_NEG8);
end;


procedure TCPU.INC_Eb(I: PInstruction_tag);
var
  op1: Bit8u;
begin
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  Inc(op1);
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, op1)
    else write_RMW_virtual_byte(op1);

  SET_FLAGS_OSZAP_8(0, 0, op1, BX_INSTR_INC8);
end;

procedure TCPU.DEC_Eb(I: PInstruction_tag);
var
  op1_8: Bit8u;
begin
  (* op1_8 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  Dec(op1_8);
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, op1_8)
    else write_RMW_virtual_byte(op1_8);

  SET_FLAGS_OSZAP_8(0, 0, op1_8, BX_INSTR_DEC8);
end;

procedure TCPU.CMPXCHG_EbGb(I: PInstruction_tag);
var
  op2_8, op1_8, diff_8: Bit8u;
begin
  (* op1_8 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  diff_8 := AL - op1_8;

  SET_FLAGS_OSZAPC_8(AL, op1_8, diff_8, BX_INSTR_CMP8);

  if (diff_8 = 0) then // if accumulator = dest
  begin
    // ZF = 1
    set_ZF(1);
    // dest <-- src
    op2_8 := BX_READ_8BIT_REG(i^.nnn);

    if (i^.mod_ = $c0)
      then BX_WRITE_8BIT_REG(i^.rm, op2_8)
      else write_RMW_virtual_byte(op2_8);
  end else
  begin
    // ZF = 0
    set_ZF(0);
    // accumulator <-- dest
    AL := op1_8;
  end;
end;

procedure TCPU.INC_RX(I: PInstruction_tag);
var
  rx: Bit16u;
begin
  Inc(gen_reg[i^.b1 and $07].rx);
  rx := gen_reg[i^.b1 and $07].rx;
  SET_FLAGS_OSZAP_16(0, 0, rx, BX_INSTR_INC16);
end;

procedure TCPU.DEC_RX(I: PInstruction_tag);
var
  idx: Byte;
begin
  idx := i^.b1 and $07;
  Dec(gen_reg[idx].rx);
  SET_FLAGS_OSZAP_16(0, 0, gen_reg[idx].rx, BX_INSTR_DEC16);
end;

procedure TCPU.ADD_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, sum_16: Bit16u;
begin
  (* op2_16 is a register, i^.rm_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);

  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  sum_16 := op1_16 + op2_16;
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, sum_16)
    else write_virtual_word(i^.seg, i^.rm_addr, @sum_16);

  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_ADD16);
end;

procedure TCPU.ADD_GwEw(I: PInstruction_tag);
var
  op1_16, op2_16, sum_16: Bit16u;
begin
  (* op1_16 is a register, i^.rm_addr is an index of a register *)
  op1_16 := BX_READ_16BIT_REG(i^.nnn);

  (* op2_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  sum_16 := op1_16 + op2_16;
  (* now write sum back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, sum_16);

  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_ADD16);
end;

procedure TCPU.ADD_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, sum_16: Bit16u;
begin
  op1_16 := AX;
  op2_16 := i^.Iw;
  sum_16 := op1_16 + op2_16;
  (* now write sum back to destination *)
  AX := sum_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_ADD16);
end;

procedure TCPU.ADC_EwGw(I: PInstruction_tag);
var
  temp_CF: Bool;
  op2_16, op1_16, sum_16: Bit16u;
begin

  temp_CF := get_CF();

    (* op2_16 is a register, i^.rm_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    sum_16 := op1_16 + op2_16 + temp_CF;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, sum_16);
      end
    else begin
      write_RMW_virtual_word(sum_16);
      end;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, sum_16, BX_INSTR_ADC16,
                              temp_CF);
end;

procedure TCPU.ADC_GwEw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, sum_16:Bit16u;
begin

  temp_CF := get_CF();


    (* op1_16 is a register, i^.rm_addr is an index of a register *)
    op1_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op2_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
      end;

    sum_16 := op1_16 + op2_16 + temp_CF;

    (* now write sum back to destination *)
    BX_WRITE_16BIT_REG(i^.nnn, sum_16);

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, sum_16, BX_INSTR_ADC16,
                             temp_CF);
end;

procedure TCPU.ADC_AXIw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, sum_16:Bit16u;
begin

    temp_CF := get_CF();

    op1_16 := AX;

    op2_16 := i^.Iw;

    sum_16 := op1_16 + op2_16 + temp_CF;

    (* now write sum back to destination *)
    AX := sum_16;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, sum_16, BX_INSTR_ADC16,
                           temp_CF);
end;

procedure TCPU.SBB_EwGw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, diff_16:Bit16u;
begin

  temp_CF := get_CF();


    (* op2_16 is a register, i^.rm_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - (op2_16 + temp_CF);

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, diff_16);
      end
    else begin
      write_RMW_virtual_word(diff_16);
      end;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, diff_16, BX_INSTR_SBB16,
                              temp_CF);
end;

procedure TCPU.SBB_GwEw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, diff_16:Bit16u;
begin

  temp_CF := get_CF();

    (* op1_16 is a register, i^.rm_addr is an index of a register *)
    op1_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op2_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
      end;

    diff_16 := op1_16 - (op2_16 + temp_CF);

    (* now write diff back to destination *)
    BX_WRITE_16BIT_REG(i^.nnn, diff_16);

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, diff_16, BX_INSTR_SBB16,
                              temp_CF);
end;

procedure TCPU.SBB_AXIw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, diff_16:Bit16u;
begin

  temp_CF := get_CF();


    op1_16 := AX;

    op2_16 := i^.Iw;

    diff_16 := op1_16 - (op2_16 + temp_CF);

    (* now write diff back to destination *)
    AX := diff_16;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, diff_16, BX_INSTR_SBB16,
                              temp_CF);
end;

procedure TCPU.SBB_EwIw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, diff_16:Bit16u;
begin

  temp_CF := get_CF();

    op2_16 := i^.Iw;

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - (op2_16 + temp_CF);

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, diff_16);
      end
    else begin
      write_RMW_virtual_word(diff_16);
      end;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, diff_16, BX_INSTR_SBB16,
                              temp_CF);
end;


procedure TCPU.SUB_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, diff_16:Bit16u;
begin

    (* op2_16 is a register, i^.rm_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - op2_16;

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, diff_16);
      end
    else begin
      write_RMW_virtual_word(diff_16);
      end;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SUB16);
end;

procedure TCPU.SUB_GwEw(I: PInstruction_tag);
var
  op2_16, op1_16, diff_16:Bit16u;
begin

    (* op1_16 is a register, i^.rm_addr is an index of a register *)
    op1_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op2_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
      end;

    diff_16 := op1_16 - op2_16;

    (* now write diff back to destination *)
    BX_WRITE_16BIT_REG(i^.nnn, diff_16);

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SUB16);
end;

procedure TCPU.SUB_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, diff_16:Bit16u;
begin

    op1_16 := AX;

    op2_16 := i^.Iw;

    diff_16 := op1_16 - op2_16;


    (* now write diff back to destination *)
    AX := diff_16;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SUB16);
end;


procedure TCPU.CMP_EwGw(I: PInstruction_tag);
var
  op1_16, op2_16, diff_16:Bit16u;
begin

    (* op2_16 is a register, i^.rm_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - op2_16;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMP16);
end;


procedure TCPU.CMP_GwEw(I: PInstruction_tag);
var
  op1_16, op2_16, diff_16:Bit16u;
begin

    (* op1_16 is a register, i^.rm_addr is an index of a register *)
    op1_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op2_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
      end;

    diff_16 := op1_16 - op2_16;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMP16);
end;

procedure TCPU.CMP_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, diff_16:Bit16u;
begin
    op1_16 := AX;
    op2_16 := i^.Iw;
    diff_16 := op1_16 - op2_16;
    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMP16);
end;

procedure TCPU.CBW(I: PInstruction_tag);
begin
  (* CBW: no flags are effected *)

  AX := Bit8s(AL);
end;

procedure TCPU.CWD(I: PInstruction_tag);
begin
  (* CWD: no flags are affected *)

    if (AX and $8000)<>0 then begin
      DX := $FFFF;
      end
    else begin
      DX := $0000;
      end;
end;


procedure TCPU.XADD_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, sum_16:Bit16u;
begin


    (* XADD dst(r/m), src(r)
     * temp <-- src + dst         | sum := op2 + op1
     * src  <-- dst               | op2 := op1
     * dst  <-- tmp               | op1 := sum
     *)

    (* op2 is a register, i^.rm_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    (* op1 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    sum_16 := op1_16 + op2_16;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      // and write destination into source
      // Note: if both op1 and op2 are registers, the last one written
      //       should be the sum, as op1 and op2 may be the same register.
      //       For example:  XADD AL, AL
      BX_WRITE_16BIT_REG(i^.nnn, op1_16);
      BX_WRITE_16BIT_REG(i^.rm, sum_16);
      end
    else begin
      write_RMW_virtual_word(sum_16);
      (* and write destination into source *)
      BX_WRITE_16BIT_REG(i^.nnn, op1_16);
      end;


    SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_XADD16);
end;

procedure TCPU.ADD_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, sum_16:Bit16u;
begin

    op2_16 := i^.Iw;

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    sum_16 := op1_16 + op2_16;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, sum_16);
      end
    else begin
      write_RMW_virtual_word(sum_16);
      end;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_ADD16);
end;

procedure TCPU.ADC_EwIw(I: PInstruction_tag);
var
  temp_CF:Bool;
  op2_16, op1_16, sum_16:Bit16u;
begin

  temp_CF := get_CF();


    op2_16 := i^.Iw;

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    sum_16 := op1_16 + op2_16 + temp_CF;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, sum_16);
      end
    else begin
      write_RMW_virtual_word(sum_16);
      end;

    SET_FLAGS_OSZAPC_16_CF(op1_16, op2_16, sum_16, BX_INSTR_ADC16,
                              temp_CF);
end;

procedure TCPU.SUB_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, diff_16:Bit16u;
begin

    op2_16 := i^.Iw;

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - op2_16;

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, diff_16);
      end
    else begin
      write_RMW_virtual_word(diff_16);
      end;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SUB16);
end;

procedure TCPU.CMP_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, diff_16:Bit16u;
begin

    op2_16 := i^.Iw;

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := op1_16 - op2_16;

    SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMP16);
end;

procedure TCPU.NEG_Ew(I: PInstruction_tag);
var
  op1_16, diff_16:Bit16u;
begin

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := 0 - op1_16;

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, diff_16);
      end
    else begin
      write_RMW_virtual_word(diff_16);
      end;

    SET_FLAGS_OSZAPC_16(op1_16, 0, diff_16, BX_INSTR_NEG16);
end;

procedure TCPU.INC_Ew(I: PInstruction_tag);
var
  op1_16:Bit16u;
begin

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    inc(op1_16);

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, op1_16);
      end
    else begin
      write_RMW_virtual_word(op1_16);
      end;

    SET_FLAGS_OSZAP_16(0, 0, op1_16, BX_INSTR_INC16);
end;


procedure TCPU.DEC_Ew(I: PInstruction_tag);
var
  op1_16:Bit16u;
begin
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    Dec(op1_16);

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, op1_16);
      end
    else begin
      write_RMW_virtual_word(op1_16);
      end;

    SET_FLAGS_OSZAP_16(0, 0, op1_16, BX_INSTR_DEC16);
end;

procedure TCPU.CMPXCHG_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, diff_16:Bit16u;
begin

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    diff_16 := AX - op1_16;

    SET_FLAGS_OSZAPC_16(AX, op1_16, diff_16, BX_INSTR_CMP16);

    if (diff_16 = 0) then begin  // if accumulator := dest
      // ZF := 1
      set_ZF(1);
      // dest <-- src
      op2_16 := BX_READ_16BIT_REG(i^.nnn);

      if (i^.mod_ = $c0) then begin
        BX_WRITE_16BIT_REG(i^.rm, op2_16);
        end
      else begin
        write_RMW_virtual_word(op2_16);
        end;
      end
    else begin
      // ZF := 0
      set_ZF(0);
      // accumulator <-- dest
      AX := op1_16;
      end;

end;

procedure TCPU.INC_ERX(I: PInstruction_tag);
var
  idx: Byte;
begin
  idx := i^.b1 and $07;
  Inc(gen_reg[idx].erx);
  SET_FLAGS_OSZAP_32(0, 0, gen_reg[idx].erx, BX_INSTR_INC32);
end;

procedure TCPU.DEC_ERX(I: PInstruction_tag);
var
  idx: Byte;
begin
  idx := i^.b1 and $07;
  Dec(gen_reg[idx].erx);
  SET_FLAGS_OSZAP_32(0, 0, gen_reg[idx].erx, BX_INSTR_DEC32);
end;

procedure TCPU.ADD_GdEd(I: PInstruction_tag);
var
  // for 32 bit operand size mode
  op1_32, op2_32, sum_32: Bit32u;
begin
  // op1_32 is a register, i->rm_addr is an index of a register
  op1_32 := BX_READ_32BIT_REG(i^.nnn);
  // op2_32 is a register or memory reference
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32); // pointer, segment address pair

  sum_32 := op1_32 + op2_32;

 //* now write sum back to destination
  BX_WRITE_32BIT_REG(i^.nnn, sum_32);
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_ADD32);
end;

procedure TCPU.ADD_EAXId(I: PInstruction_tag);
var
  // for 32 bit operand size mode
  op1_32, op2_32, sum_32:Bit32u;
begin
  op1_32 := EAX; //EAX
  op2_32 := i^.Id;
  sum_32 := op1_32 + op2_32;
  // now write sum back to destination
  EAX := sum_32; //EAX
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_ADD32);
end;

procedure TCPU.ADC_EdGd(I: PInstruction_tag);
var
  temp_CF: Bool;
    // for 32 bit operand size mode
  op2_32, op1_32, sum_32:Bit32u;
begin
  temp_CF := get_CF();
  // op2_32 is a register, i->rm_addr is an index of a register
  op2_32 := BX_READ_32BIT_REG(i^.nnn);

  // op1_32 is a register or memory reference
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32); // pointer, segment address pair

  sum_32 := op1_32 + op2_32 + temp_CF;

  // now write sum back to destination
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, sum_32)
    else write_RMW_virtual_dword(sum_32);

  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, sum_32, BX_INSTR_ADC32, temp_CF);
end;

procedure TCPU.ADC_EAXId(I: PInstruction_tag);
var
  temp_CF: Bool;
  op1_32, op2_32, sum_32: Bit32u;
begin
  temp_CF := get_CF();
  // for 32 bit operand size mode
  op1_32 := gen_reg[0].erx;
  op2_32 := i^.Id;
  sum_32 := op1_32 + op2_32 + temp_CF;
  // now write sum back to destination
  gen_reg[0].erx := sum_32;
  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, sum_32, BX_INSTR_ADC32, temp_CF);
end;

procedure TCPU.SBB_EdGd(I: PInstruction_tag);
var
  temp_CF: Bool;
  op1_32, op2_32, diff_32: Bit32u;
begin
  temp_CF := get_CF();
  // op2_32 is a register, i->rm_addr is an index of a register
  op2_32 := BX_READ_32BIT_REG(i^.nnn);
  // op1_32 is a register or memory reference
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
          // pointer, segment address pair
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := op1_32 - (op2_32 + temp_CF);

  // now write diff back to destination
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, diff_32)
    else write_RMW_virtual_dword(diff_32);

  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, diff_32, BX_INSTR_SBB32, temp_CF);
end;

procedure TCPU.SBB_GdEd(I: PInstruction_tag);
var
  temp_CF: Bool;
  op1_32, op2_32, diff_32: Bit32u;
begin
  temp_CF := get_CF();
  // op1_32 is a register, i->rm_addr is an index of a register */
  op1_32 := BX_READ_32BIT_REG(i^.nnn);
  // op2_32 is a register or memory reference
  if i^.mod_ = $c0
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);  //pointer, segment address pair

  diff_32 := op1_32 - (op2_32 + temp_CF);
  // now write diff back to destination
  BX_WRITE_32BIT_REG(i^.nnn, diff_32);
  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, diff_32, BX_INSTR_SBB32, temp_CF);
end;

procedure TCPU.SBB_EAXId(I: PInstruction_tag);
var
  temp_CF: Bool;
  op1_32, op2_32, diff_32: Bit32u;
begin
  temp_CF := get_CF;
  op1_32 := gen_reg[0].erx; //EAX
  op2_32 := i^.Id;
  diff_32 := op1_32 - (op2_32 + temp_CF);
  // now write diff back to destination
  gen_reg[0].erx := diff_32;
  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, diff_32, BX_INSTR_SBB32, temp_CF);
end;

procedure TCPU.SBB_EdId(I: PInstruction_tag);
var
  temp_CF:Bool;
  op1_32, op2_32, diff_32:Bit32u;
begin

  temp_CF := get_CF;
  op2_32 := i^.Id;

  // op1_32 is a register or memory reference
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32); // pointer, segment address pair

  diff_32 := op1_32 - (op2_32 + temp_CF);

  // now write diff back to destination
  if i^.mod_ = $c0 then
    BX_WRITE_32BIT_REG(i^.rm, diff_32)
  else
    write_RMW_virtual_dword(diff_32);

  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, diff_32, BX_INSTR_SBB32,
                            temp_CF);
end;

procedure TCPU.SUB_EdGd(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32:Bit32u;
begin
  // op2_32 is a register, i->rm_addr is an index of a register
  op2_32 := BX_READ_32BIT_REG(i^.nnn);

  // op1_32 is a register or memory reference
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    // pointer, segment address pair
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := op1_32 - op2_32;

  // now write diff back to destination
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, diff_32)
  else
    write_RMW_virtual_dword(diff_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SUB32);
end;

procedure TCPU.SUB_GdEd(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32:Bit32u;
begin
  // op1_32 is a register, i->rm_addr is an index of a register
  op1_32 := BX_READ_32BIT_REG(i^.nnn);

  // op2_32 is a register or memory reference
  if (i^.mod_ = $c0) then
    op2_32 := BX_READ_32BIT_REG(i^.rm)
  else
    read_virtual_dword(i^.seg, i^.rm_addr, @op2_32); // pointer, segment address pair

  diff_32 := op1_32 - op2_32;

  // now write diff back to destination
  BX_WRITE_32BIT_REG(i^.nnn, diff_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SUB32);
end;

procedure TCPU.ADD_EdGd(I: PInstruction_tag);
var
  // for 32 bit operand size mode
  op2_32, op1_32, sum_32: Bit32u;
begin
  op2_32 := BX_READ_32BIT_REG(i^.nnn);
  if i^.mod_ = $c0
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
    // pointer, segment address pair
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  sum_32 := op1_32 + op2_32;
  // now write sum back to destination
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, sum_32)
    else write_RMW_virtual_dword(sum_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_ADD32);
end;

procedure TCPU.SUB_EAXId(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32: Bit32u;
begin
  op1_32 := gen_reg[0].erx;
  op2_32 := i^.Id;
  diff_32 := op1_32 - op2_32;
  (* now write diff back to destination *)
  gen_reg[0].erx := diff_32;
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SUB32);
end;


procedure TCPU.CMP_EdGd(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32:Bit32u;
begin

    (* op2_32 is a register, i^.rm_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    diff_32 := op1_32 - op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMP32);
end;


procedure TCPU.CMP_GdEd(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32:Bit32u;
begin

    (* op1_32 is a register, i^.rm_addr is an index of a register *)
    op1_32 := BX_READ_32BIT_REG(i^.nnn);

    (* op2_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op2_32 := BX_READ_32BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);
      end;

    diff_32 := op1_32 - op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMP32);
end;

procedure TCPU.CMP_EAXId(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32:Bit32u;
begin

    op1_32 := gen_reg[0].erx;

    op2_32 := i^.Id;

    diff_32 := op1_32 - op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMP32);
end;


procedure TCPU.CWDE(I: PInstruction_tag);
begin
  (* CBW: no flags are effected *)

    gen_reg[0].erx := Bit16s(gen_reg[0].rx);
end;

procedure TCPU.CDQ(I: PInstruction_tag);
begin
  (* CWD: no flags are affected *)

    if (gen_reg[0].erx and $80000000)<>0 then begin
      gen_reg[2].erx := $FFFFFFFF;
      end
    else begin
      gen_reg[2].erx := $00000000;
      end;
end;

// Some info on the opcodes at begin0F,A6end; and begin0F,A7end;
// On 386 steps A0-B0:
//   beginOF,A6end; = XBTS
//   beginOF,A7end; = IBTS
// On 486 steps A0-B0:
//   beginOF,A6end; = CMPXCHG 8
//   beginOF,A7end; = CMPXCHG 16|32
//
// On 486 >= B steps, and further processors, the
// CMPXCHG instructions were moved to opcodes:
//   beginOF,B0end; = CMPXCHG 8
//   beginOF,B1end; = CMPXCHG 16|32

procedure TCPU.CMPXCHG_XBTS(I: PInstruction_tag);
begin
  UndefinedOpcode(i);
end;

procedure TCPU.CMPXCHG_IBTS(I: PInstruction_tag);
begin
  UndefinedOpcode(i);
end;


procedure TCPU.XADD_EdGd(I: PInstruction_tag);
var
  op2_32, op1_32, sum_32:Bit32u;
begin


    (* XADD dst(r/m), src(r)
     * temp <-- src + dst         | sum = op2 + op1
     * src  <-- dst               | op2 = op1
     * dst  <-- tmp               | op1 = sum
     *)

    (* op2 is a register, i^.rm_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);

    (* op1 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    sum_32 := op1_32 + op2_32;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      // and write destination into source
      // Note: if both op1 @ op2 are registers, the last one written
      //       should be the sum, as op1 @ op2 may be the same register.
      //       For example:  XADD AL, AL
      BX_WRITE_32BIT_REG(i^.nnn, op1_32);
      BX_WRITE_32BIT_REG(i^.rm, sum_32);
      end
    else begin
      write_RMW_virtual_dword(sum_32);
      (* and write destination into source *)
      BX_WRITE_32BIT_REG(i^.nnn, op1_32);
      end;


    SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_XADD32);
end;

procedure TCPU.ADD_EdId(I: PInstruction_tag);
var
  op1_32, op2_32, sum_32:Bit32u;
begin

    op2_32 := i^.Id;

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
    else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    sum_32 := op1_32 + op2_32;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, sum_32);
      end
    else begin
      write_RMW_virtual_dword(sum_32);
      end;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_ADD32);
end;

procedure TCPU.ADC_EdId(I: PInstruction_tag);
var
  temp_CF:Bool;
  op1_32, op2_32, sum_32: Bit32u;
begin
  temp_CF := get_CF();
  op2_32 := i^.Id;
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  sum_32 := op1_32 + op2_32 + temp_CF;
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, sum_32)
    else write_RMW_virtual_dword(sum_32);

  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, sum_32, BX_INSTR_ADC32, temp_CF);
end;

procedure TCPU.SUB_EdId(I: PInstruction_tag);
var
  op1_32, op2_32, diff_32: Bit32u;
begin
  op2_32 := i^.Id;
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := op1_32 - op2_32;
  (* now write diff back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, diff_32)
    else write_RMW_virtual_dword(diff_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SUB32);
end;

procedure TCPU.CMPXCHG8B(I: PInstruction_tag);
var
  op1_64_lo, op1_64_hi, diff: Bit32u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  (* pointer, segment address pair *)
  read_virtual_dword(i^.seg, i^.rm_addr, @op1_64_lo);
  read_RMW_virtual_dword(i^.seg, i^.rm_addr + 4, @op1_64_hi);

  diff := EAX - op1_64_lo;
  diff := diff or EDX - op1_64_hi;

//     SET_FLAGS_OSZAPC_32(EAX, op1_32, diff_32, BX_INSTR_CMP32);

  if (diff = 0) then
  begin  // if accumulator = dest
    // ZF := 1
    set_ZF(1);
    // dest <-- src
    write_RMW_virtual_dword(ECX);
    write_virtual_dword(i^.seg, i^.rm_addr, @EBX);
  end else
  begin
    // ZF := 0
    set_ZF(0);
    // accumulator <-- dest
    EAX := op1_64_lo;
    EDX := op1_64_hi;
  end;
end;

procedure TCPU.CMP_EdId(I: PInstruction_tag);
var
  op2_32, op1_32, diff_32: Bit32u;
begin
  (* for 32 bit operand size mod_e *)
  op2_32 := i^.Id;
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
      (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := op1_32 - op2_32;
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMP32);
end;

procedure TCPU.NEG_Ed(I: PInstruction_tag);
var
  op1_32, diff_32: Bit32u;
begin
  (* for 32 bit operand size mod_e *)
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := 0 - op1_32;
  (* now write diff back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, diff_32)
    else write_RMW_virtual_dword(diff_32);

  SET_FLAGS_OSZAPC_32(op1_32, 0, diff_32, BX_INSTR_NEG32);
end;

procedure TCPU.INC_Ed(I: PInstruction_tag);
var
  op1_32: Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  Inc(op1_32);
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, op1_32)
    else write_RMW_virtual_dword(op1_32);

  SET_FLAGS_OSZAP_32(0, 0, op1_32, BX_INSTR_INC32);
end;

procedure TCPU.DEC_Ed(I: PInstruction_tag);
var
  op1_32: Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
      (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  Dec(op1_32);
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, op1_32)
    else write_RMW_virtual_dword(op1_32);

  SET_FLAGS_OSZAP_32(0, 0, op1_32, BX_INSTR_DEC32);
end;

procedure TCPU.ADC_GdEd(I: PInstruction_tag);
var
  temp_CF: Bool;
  op1_32, op2_32, sum_32: Bit32u;
begin
  temp_CF := get_CF();
  (* for 32 bit operand size mod_e *)
  (* op1_32 is a register, i^.rm_addr is an index of a register *)
  op1_32 := BX_READ_32BIT_REG(i^.nnn);
  (* op2_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  sum_32 := op1_32 + op2_32 + temp_CF;
  (* now write sum back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, sum_32);

  SET_FLAGS_OSZAPC_32_CF(op1_32, op2_32, sum_32, BX_INSTR_ADC32, temp_CF);
end;

procedure TCPU.CMPXCHG_EdGd(I: PInstruction_tag);
var
  op2_32, op1_32, diff_32:Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  diff_32 := EAX - op1_32;

  SET_FLAGS_OSZAPC_32(EAX, op1_32, diff_32, BX_INSTR_CMP32);

  if (diff_32 = 0) then
  begin  // if accumulator = dest
    // ZF := 1
    set_ZF(1);
    // dest <-- src
    op2_32 := BX_READ_32BIT_REG(i^.nnn);

    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, op2_32)
      else write_RMW_virtual_dword(op2_32);
  end else
  begin
    // ZF := 0
    set_ZF(0);
    // accumulator <-- dest
    EAX := op1_32;
  end;
end;

procedure TCPU.XOR_EbGb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);
  result := op1 xor op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);

  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_XOR8);
end;

procedure TCPU.XOR_GbEb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op1 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  result := op1 xor op2;
  (* now write result back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, result);

  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_XOR8);
end;

procedure TCPU.XOR_ALIb(I: PInstruction_tag);
var
  op2, op1, sum: Bit8u;
begin
  op1 := AL;
  op2 := i^.Ib;
  sum := op1 xor op2;
  (* now write sum back to destination, which is a register *)
  AL := sum;
  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_XOR8);
end;

procedure TCPU.XOR_EbIb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op2 := i^.Ib;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 xor op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);

  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_XOR8);
end;

procedure TCPU.OR_EbIb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op2 := i^.Ib;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 or op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);

  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_OR8);
end;

procedure TCPU.NOT_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
begin
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  result_8 := not op1_8; // ~
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_RMW_virtual_byte(result_8);
end;


procedure TCPU.OR_EbGb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 or op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);

  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_OR8);
end;

procedure TCPU.OR_GbEb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op1 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  result := op1 or op2;
  (* now write result back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, result);
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_OR8);
end;

procedure TCPU.OR_ALIb(I: PInstruction_tag);
var
  op1, op2, sum: Bit8u;
begin
  op1 := AL;
  op2 := i^.Ib;
  sum := op1 or op2;
  (* now write sum back to destination, which is a register *)
  AL := sum;
  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_OR8);
end;

procedure TCPU.AND_EbGb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 and op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_AND8);
end;

procedure TCPU.AND_GbEb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op1 := BX_READ_8BIT_REG(i^.nnn);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  result := op1 and op2;
  (* now write result back to destination, which is a register *)
  BX_WRITE_8BIT_REG(i^.nnn, result);
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_AND8);
end;

procedure TCPU.AND_ALIb(I: PInstruction_tag);
var
  op1, op2, sum: Bit8u;
begin
  op1 := AL;
  op2 := i^.Ib;
  sum := op1 and op2;
  (* now write sum back to destination, which is a register *)
  AL := sum;
  SET_FLAGS_OSZAPC_8(op1, op2, sum, BX_INSTR_AND8);
end;

procedure TCPU.AND_EbIb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op2 := i^.Ib;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 and op2;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result)
    else write_RMW_virtual_byte(result);
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_AND8);
end;

procedure TCPU.TEST_EbGb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 and op2;
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_TEST8);
end;

procedure TCPU.TEST_ALIb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  (* op1 is the AL register *)
  op1 := AL;
  (* op2 is imm8 *)
  op2 := i^.Ib;
  result := op1 and op2;
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_TEST8);
end;

procedure TCPU.TEST_EbIb(I: PInstruction_tag);
var
  op2, op1, result: Bit8u;
begin
  op2 := i^.Ib;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op1);

  result := op1 and op2;
  SET_FLAGS_OSZAPC_8(op1, op2, result, BX_INSTR_TEST8);
end;

procedure TCPU.XOR_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
    (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 xor op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);

  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_XOR16);
end;

procedure TCPU.XOR_GwEw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op1_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op2_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  result_16 := op1_16 xor op2_16;
  (* now write result back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_XOR16);
end;

procedure TCPU.XOR_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, sum_16: Bit16u;
begin
  op1_16 := AX;
  op2_16 := i^.Iw;
  sum_16 := op1_16 xor op2_16;
  (* now write sum back to destination *)
  AX := sum_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_XOR16);
end;

procedure TCPU.XOR_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op2_16 := i^.Iw;

  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 xor op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_XOR16);
end;

procedure TCPU.OR_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op2_16 := i^.Iw;
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 or op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_OR16);
end;

procedure TCPU.NOT_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
begin
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := not op1_16; // ~
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
end;

procedure TCPU.OR_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 or op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_OR16);
end;

procedure TCPU.OR_GwEw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op1_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op2_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  result_16 := op1_16 or op2_16;
  (* now write result back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_OR16);
end;

procedure TCPU.OR_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, sum_16: Bit16u;
begin
  op1_16 := AX;
  op2_16 := i^.Iw;
  sum_16 := op1_16 or op2_16;
  (* now write sum back to destination *)
  AX := sum_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_OR16);
end;

procedure TCPU.AND_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 and op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_AND16);
end;

procedure TCPU.AND_GwEw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op1_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op2_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  result_16 := op1_16 and op2_16;
  (* now write result back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_AND16);
end;

procedure TCPU.AND_AXIw(I: PInstruction_tag);
var
  op1_16, op2_16, sum_16: Bit16u;
begin
  op1_16 := AX;
  op2_16 := i^.Iw;
  sum_16 := op1_16 and op2_16;
  (* now write sum back to destination *)
  AX := sum_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, sum_16, BX_INSTR_AND16);
end;

procedure TCPU.AND_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op2_16 := i^.Iw;
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 and op2_16;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_AND16);
end;

procedure TCPU.TEST_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then  op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  result_16 := op1_16 and op2_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_TEST16);
end;

procedure TCPU.TEST_AXIw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  op1_16 := AX;
  (* op2_16 is imm16 *)
  op2_16 := i^.Iw;
  result_16 := op1_16 and op2_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_TEST16);
end;

procedure TCPU.TEST_EwIw(I: PInstruction_tag);
var
  op2_16, op1_16, result_16: Bit16u;
begin
  (* op2_16 is imm16 *)
  op2_16 := i^.Iw;
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  result_16 := op1_16 and op2_16;
  SET_FLAGS_OSZAPC_16(op1_16, op2_16, result_16, BX_INSTR_TEST16);
end;

procedure TCPU.XOR_EdGd(I: PInstruction_tag);
var
  op2_32, op1_32, result_32: Bit32u;
begin
  (* op2_32 is a register, op2_addr is an index of a register *)
  op2_32 := BX_READ_32BIT_REG(i^.nnn);
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := op1_32 xor op2_32;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_XOR32);
end;

procedure TCPU.XOR_GdEd(I: PInstruction_tag);
var
  op2_32, op1_32, result_32: Bit32u;
begin
  op1_32 := BX_READ_32BIT_REG(i^.nnn);
  (* op2_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  result_32 := op1_32 xor op2_32;
  (* now write result back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, result_32);
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_XOR32);
end;

procedure TCPU.XOR_EAXId(I: PInstruction_tag);
var
  op1_32, op2_32, sum_32: Bit32u;
begin
  (* for 32 bit operand size mod_e *)
  op1_32 := EAX;
  op2_32 := i^.Id;
  sum_32 := op1_32 xor op2_32;
  (* now write sum back to destination *)
  EAX := sum_32;
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_XOR32);
end;

procedure TCPU.XOR_EdId(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin

  op2_32 := i^.Id;

    (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := op1_32 xor op2_32;

    (* now write result back to destination *)
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, result_32)
  else
    write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_XOR32);
end;

procedure TCPU.OR_EdId(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin

  op2_32 := i^.Id;

    (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := op1_32 or op2_32;

    (* now write result back to destination *)
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, result_32)
  else
    write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_OR32);
end;

procedure TCPU.NOT_Ed(I: PInstruction_tag);
var
  op1_32,
  result_32: Bit32u;
begin

    (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := not op1_32; //~

    (* now write result back to destination *)
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, result_32)
  else
    write_RMW_virtual_dword(result_32);
end;

procedure TCPU.OR_EdGd(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin
  (* op2_32 is a register, op2_addr is an index of a register *)
  op2_32 := BX_READ_32BIT_REG(i^.nnn);

  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := op1_32 or op2_32;

    (* now write result back to destination *)
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, result_32)
  else
    write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_OR32);
end;

procedure TCPU.OR_GdEd(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin
  op1_32 := BX_READ_32BIT_REG(i^.nnn);

  (* op2_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  result_32 := op1_32 or op2_32;

  (* now write result back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_OR32);
end;

procedure TCPU.OR_EAXId(I: PInstruction_tag);
var
  op1_32,
  op2_32,
  sum_32: Bit32u;
begin
  op1_32 := EAX;
  op2_32 := i^.Id;
  sum_32 := op1_32 or op2_32;
  (* now write sum back to destination *)
  EAX := sum_32;
  SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_OR32);
end;

procedure TCPU.AND_EdGd(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin
  (* op2_32 is a register, op2_addr is an index of a register *)
  op2_32 := BX_READ_32BIT_REG(i^.nnn);

  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op1_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  result_32 := op1_32  and op2_32;

  (* now write result back to destination *)
  if (i^.mod_ = $c0) then
    BX_WRITE_32BIT_REG(i^.rm, result_32)
  else
    write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_AND32);
end;

procedure TCPU.AND_GdEd(I: PInstruction_tag);
var
  op2_32,
  op1_32,
  result_32: Bit32u;
begin
  op1_32 := BX_READ_32BIT_REG(i^.nnn);

  (* op2_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_32 := BX_READ_32BIT_REG(i^.rm)
  else
    (* pointer, segment address pair *)
    read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  result_32 := op1_32  and op2_32;

  (* now write result back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, result_32);

  SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_AND32);
end;

procedure TCPU.AND_EAXId(I: PInstruction_tag);
var
  op1_32, op2_32, sum_32:Bit32u;
begin

    op1_32 := EAX;

    op2_32 := i^.Id;

    sum_32 := op1_32  and op2_32;

    (* now write sum back to destination *)
    EAX := sum_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, sum_32, BX_INSTR_AND32);
end;

procedure TCPU.AND_EdId(I: PInstruction_tag);
var
  op2_32, op1_32, result_32:Bit32u;
begin

    op2_32 := i^.Id;

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    result_32 := op1_32  and op2_32;

    (* now write result back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, result_32);
      end
  else begin
      write_RMW_virtual_dword(result_32);
      end;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_AND32);
end;


procedure TCPU.TEST_EdGd(I: PInstruction_tag);
var
  op2_32, op1_32, result_32:Bit32u;
begin

    (* op2_32 is a register, op2_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    result_32 := op1_32  and op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_TEST32);
end;

procedure TCPU.TEST_EAXId(I: PInstruction_tag);
var
  op2_32, op1_32, result_32:Bit32u;
begin

    (* op1 is EAX register *)
    op1_32 := EAX;

    (* op2 is imm32 *)
    op2_32 := i^.Id;

    result_32 := op1_32  and op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_TEST32);
end;

procedure TCPU.TEST_EdId(I: PInstruction_tag);
var
  op2_32, op1_32, result_32:Bit32u;
begin

    (* op2 is imm32 *)
    op2_32 := i^.Id;

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    result_32 := op1_32  and op2_32;

    SET_FLAGS_OSZAPC_32(op1_32, op2_32, result_32, BX_INSTR_TEST32);
end;

procedure TCPU.MUL_ALEb(I: PInstruction_tag);
var
  op2, op1: Bit8u;
  product_16: Bit16u;
  temp_flag: Bool;
begin
  op1 := AL;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  product_16 := op1 * op2;
  (* set EFLAGS:
   * MUL affects the following flags: C,O
   *)
  temp_flag := Bool((product_16  and $FF00) <> 0);
  SET_FLAGS_OxxxxC(temp_flag, temp_flag);
  (* now write product back to destination *)
  AX := product_16;
end;

procedure TCPU.IMUL_ALEb(I: PInstruction_tag);
var
  op2, op1: Bit8u;
  product_16: Bit16u;
  upper_bits: Bit16u;
begin
  op1 := AL;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_byte(i^.seg, i^.rm_addr, PBit8u(@op2));

  product_16 := op1 * op2;
  (* now write product back to destination *)
  AX := product_16;
  (* set EFLAGS:
   * IMUL affects the following flags: C,O
   * IMUL r/m8: condition for clearing CF  and OF:
   *   AL := sign-extend of AL to 16 bits
   *)
  upper_bits := AX and $ff80;
  if (upper_bits = $ff80) or (upper_bits = $0000) then
    SET_FLAGS_OxxxxC(0, 0)
  else
    SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.DIV_ALEb(I: PInstruction_tag);
var
  op2, quotient_8l, remainder_8: Bit8u;
  quotient_16, op1: Bit16u;
begin
  op1 := AX;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_byte(i^.seg, i^.rm_addr, @op2);

  if (op2 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  quotient_16 := Trunc(op1 / op2);
  remainder_8 := op1 mod op2;
  quotient_8l := quotient_16 and $FF;

  if (quotient_16 <> quotient_8l) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  (* set EFLAGS:
   * DIV affects the following flags: O,S,Z,A,P,C are undefined
   *)

{$if INTEL_DIV_FLAG_BUG = 1}
    set_CF(1);
{$ifend}

  (* now write quotient back to destination *)

  AL := quotient_8l;
  AH := remainder_8;
end;

procedure TCPU.IDIV_ALEb(I: PInstruction_tag);
var
  op2, quotient_8l, remainder_8: Bit8s;
  quotient_16, op1: Bit16s;
begin
  op1 := AX;

  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_byte(i^.seg, i^.rm_addr, PBit8u(@op2));

  if (op2 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  quotient_16 := Trunc(op1 / op2);
  remainder_8 := op1 mod op2;
  quotient_8l := quotient_16 and $FF;

  if (quotient_16 <> quotient_8l) then
  begin
    AL := quotient_8l;
    AH := remainder_8;
    exception2([BX_DE_EXCEPTION, 0, 0]);
  end;
  (* set EFLAGS:
   * DIV affects the following flags: O,S,Z,A,P,C are undefined
   *)
{$if INTEL_DIV_FLAG_BUG = 1}
    set_CF(1);
{$ifend}
  (* now write quotient back to destination *)
  AL := quotient_8l;
  AH := remainder_8;
end;

procedure TCPU.MUL_AXEw(I: PInstruction_tag);
var
  op1_16, op2_16, product_16h, product_16l: Bit16u;
  product_32: Bit32u;
  temp_flag: Bool;
begin
  op1_16 := AX;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  product_32 := (Bit32u(op1_16)) * (Bit32u(op2_16));

  product_16l := (product_32  and $FFFF);
  product_16h := product_32 shr 16;
  (* now write product back to destination *)
  AX := product_16l;
  DX := product_16h;
  (* set eflags:
   * MUL affects the following flags: C,O
   *)
  temp_flag := Bool(product_16h <> 0);
  SET_FLAGS_OxxxxC(temp_flag, temp_flag);
end;

procedure TCPU.IMUL_AXEw(I: PInstruction_tag);
var
  op1_16, op2_16: Bit16s;
  product_32: Bit32s;
  product_16h, product_16l: Bit16u;
begin
  op1_16 := AX;

  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, PBit16u(@op2_16));

  product_32 := (Bit32s(op1_16)) * (Bit32s(op2_16));

  product_16l := (product_32  and $FFFF);
  product_16h := product_32 shr 16;
  (* now write product back to destination *)
  AX := product_16l;
  DX := product_16h;
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r/m16: condition for clearing CF  and OF:
   *   DX:AX := sign-extend of AX
   *)
  if (DX=$ffff) and Boolean(AX and $8000) then
    SET_FLAGS_OxxxxC(0, 0)
  else
  if ( (DX=$0000) and (AX < $8000) ) then
    SET_FLAGS_OxxxxC(0, 0)
  else
    SET_FLAGS_OxxxxC(1, 1);
end;


procedure TCPU.DIV_AXEw(I: PInstruction_tag);
var
  op2_16, remainder_16, quotient_16l: Bit16u;
  op1_32, quotient_32: Bit32u;
begin
  op1_32 := (Bit32u(DX) shl 16) or Bit32u(AX);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  if (op2_16 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  quotient_32 := op1_32 div op2_16;
  remainder_16 := op1_32 mod op2_16;
  quotient_16l := quotient_32 and $FFFF;

  if (quotient_32 <> quotient_16l) then
    exception2([BX_DE_EXCEPTION, 0, 0]);
  (* set EFLAGS:
   * DIV affects the following flags: O,S,Z,A,P,C are undefined
   *)
{$if INTEL_DIV_FLAG_BUG = 1}
  set_CF(1);
{$ifend}
  (* now write quotient back to destination *)
  AX := quotient_16l;
  DX := remainder_16;
end;

procedure TCPU.IDIV_AXEw(I: PInstruction_tag);
var
  op2_16, remainder_16, quotient_16l: Bit16s;
  op1_32, quotient_32: Bit32s;
begin
  op1_32 := (Bit32u(DX) shl 16) or (Bit32u(AX));

  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, PBit16u(@op2_16));

  if (op2_16 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  quotient_32 := op1_32 div op2_16;
  remainder_16 := op1_32 mod op2_16;
  quotient_16l := quotient_32 and $FFFF;

  if (quotient_32 <> quotient_16l) then
    exception2([BX_DE_EXCEPTION, 0, 0]);

  (* set EFLAGS:
   * IDIV affects the following flags: O,S,Z,A,P,C are undefined
   *)

{$if INTEL_DIV_FLAG_BUG = 1}
  set_CF(1);
{$ifend}

  (* now write quotient back to destination *)

  AX := quotient_16l;
  DX := remainder_16;
end;


procedure TCPU.IMUL_GwEwIw(I: PInstruction_tag);
var
  product_16l: Bit16u;
  op2_16, op3_16: Bit16s;
  product_32: Bit32s;
begin
  op3_16 := i^.Iw;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, PBit16u(@op2_16));

  product_32 := op2_16 * op3_16;
  product_16l := (product_32  and $FFFF);
  (* now write product back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, product_16l);
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r16,r/m16,imm16: condition for clearing CF  and OF:
   *   result exactly fits within r16
   *)
  if (product_32 > -32768)  and (product_32 < 32767) then
    SET_FLAGS_OxxxxC(0, 0)
  else
    SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.IMUL_GwEw(I: PInstruction_tag);
var
  product_16l: Bit16u;
  op1_16, op2_16: Bit16s;
  product_32: Bit32s;
begin
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    op2_16 := BX_READ_16BIT_REG(i^.rm)
    (* pointer, segment address pair *)
  else
    read_virtual_word(i^.seg, i^.rm_addr, PBit16u(@op2_16));

  op1_16 := BX_READ_16BIT_REG(i^.nnn);
  product_32 := op1_16 * op2_16;
  product_16l := (product_32  and $FFFF);
  (* now write product back to destination *)
  BX_WRITE_16BIT_REG(i^.nnn, product_16l);
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r16,r/m16,imm16: condition for clearing CF  and OF:
   *   result exactly fits within r16
   *)
  if (product_32 > -32768) and (product_32 < 32767)
    then SET_FLAGS_OxxxxC(0, 0)
    else SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.MUL_EAXEd(I: PInstruction_tag);
var
  op1_32, op2_32, product_32h, product_32l: Bit32u;
  product_64: Bit64u;
  temp_flag: Bool;
begin
  op1_32 := EAX;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  product_64 := (Bit64u(op1_32)) * (Bit64u(op2_32));
  product_32l := Bit32u(product_64  and $FFFFFFFF);
  product_32h := Bit32u(product_64 shr 32);
  (* now write product back to destination *)
  EAX := product_32l;
  EDX := product_32h;
  (* set eflags:
   * MUL affects the following flags: C,O
   *)
  temp_flag := Word(product_32h <> 0);
  SET_FLAGS_OxxxxC(temp_flag, temp_flag);
end;

procedure TCPU.IMUL_EAXEd(I: PInstruction_tag);
var
  op1_32, op2_32: Bit32s;
  product_64: Bit64s;
  product_32h, product_32l: Bit32u;
begin
  op1_32 := EAX;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, PBit32u (@op2_32));

  product_64 := Bit64s(op1_32) * Bit64s(op2_32);
  product_32l := Bit32u(product_64 and $FFFFFFFF);
  product_32h := Bit32u(product_64 shr 32);
  (* now write product back to destination *)
  EAX := product_32l;
  EDX := product_32h;
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r/m16: condition for clearing CF  and OF:
   *   EDX:EAX := sign-extend of EAX
   *)
  if  (EDX=$ffffffff) and ((EAX and $80000000)<>0)
    then SET_FLAGS_OxxxxC(0, 0)
    else
  if ( (EDX=$00000000) and (EAX < $80000000) )
    then SET_FLAGS_OxxxxC(0, 0)
    else SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.DIV_EAXEd(I: PInstruction_tag);
var
  op2_32, remainder_32, quotient_32l: Bit32u;
  op1_64, quotient_64: Bit64u;
begin
  op1_64 := (Bit64u(EDX) shl 32) + Bit64u(EAX);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  if (op2_32 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);
  quotient_64 := op1_64 div op2_32;
  remainder_32 := Bit32u(op1_64 mod op2_32);
  quotient_32l := Bit32u(quotient_64 and $FFFFFFFF);

  if (quotient_64 <> quotient_32l) then
    exception2([BX_DE_EXCEPTION, 0, 0]);
  (* set EFLAGS:
   * DIV affects the following flags: O,S,Z,A,P,C are undefined
   *)
  (* now write quotient back to destination *)
  EAX := quotient_32l;
  EDX := remainder_32;
end;

procedure TCPU.IDIV_EAXEd(I: PInstruction_tag);
var
  op2_32, remainder_32, quotient_32l: Bit32s;
  op1_64, quotient_64: Bit64s;
begin
  op1_64 := (Bit64u(EDX) shl 32) or Bit64u(EAX);
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, (PBit32u(@op2_32)));

  if (op2_32 = 0) then
    exception2([BX_DE_EXCEPTION, 0, 0]);
  quotient_64 := op1_64 div op2_32;
  remainder_32 := Bit32s(op1_64 mod op2_32);
  quotient_32l := Bit32s(quotient_64 and $FFFFFFFF);

  if (quotient_64 <> quotient_32l) then
    exception2([BX_DE_EXCEPTION, 0, 0]);
  (* set EFLAGS:
   * IDIV affects the following flags: O,S,Z,A,P,C are undefined
   *)
  (* now write quotient back to destination *)
  EAX := quotient_32l;
  EDX := remainder_32;
end;

procedure TCPU.IMUL_GdEdId(I: PInstruction_tag);
var
  op2_32, op3_32, product_32: Bit32s;
  product_64: Bit64s;
begin
  op3_32 := i^.Id;
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, PBit32u(@op2_32));
  product_32 := op2_32 * op3_32;
  product_64 := Bit64s(op2_32) * Bit64s(op3_32);
  (* now write product back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, product_32);
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r16,r/m16,imm16: condition for clearing CF  and OF:
   *   result exactly fits within r16
   *)
  if (product_64 = product_32)
    then SET_FLAGS_OxxxxC(0, 0)
    else SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.IMUL_GdEd(I: PInstruction_tag);
var
  op1_32, op2_32, product_32: Bit32s;
  product_64: Bit64s;
begin
  (* op2 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, PBit32u(@op2_32));

  op1_32 := BX_READ_32BIT_REG(i^.nnn);
  product_32 := op1_32 * op2_32;
  product_64 := Bit64s(op1_32) * Bit64s(op2_32);
  (* now write product back to destination *)
  BX_WRITE_32BIT_REG(i^.nnn, product_32);
  (* set eflags:
   * IMUL affects the following flags: C,O
   * IMUL r16,r/m16,imm16: condition for clearing CF  and OF:
   *   result exactly fits within r16
   *)
  if (product_64 = product_32)
    then SET_FLAGS_OxxxxC(0, 0)
    else SET_FLAGS_OxxxxC(1, 1);
end;

procedure TCPU.push_16(value16:Bit16u);
var
  temp_ESP: Bit32u;
begin
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0
      then temp_ESP := ESP
      else temp_ESP := SP;

    if (can_push(@sregs[SEG_REG_SS].cache, temp_ESP, 2) = 0) then
    begin
      LogPanic(('push_16(): can''t push on stack'));
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;
    (* access within limits *)
    write_virtual_word(SEG_REG_SS, temp_ESP - 2, @value16);
    if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0
      then ESP := ESP - 2
      else SP := SP -2;
    exit;
  end else
  begin (* real mod_e *)
    if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
    begin
      if (ESP = 1) then
        LogPanic(('CPU shutting down due to lack of stack space, ESP=1'));
      ESP := ESP - 2;
      temp_ESP := ESP;
    end else
    begin
      if (SP = 1) then
        LogPanic(('CPU shutting down due to lack of stack space, SP=1'));
      SP := SP -2;
      temp_ESP := SP;
    end;
    write_virtual_word(SEG_REG_SS, temp_ESP, @value16);
    exit;
  end;
end;

  (* push 32 bit operand size *)
procedure TCPU.push_32(value32:Bit32u);
begin
  (* must use StackAddrSize, and either ESP or SP accordingly *)
  if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
  begin (* StackAddrSize := 32 *)
    (* 32bit stack size: pushes use SS:ESP  *)
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    begin
      if (can_push(@sregs[SEG_REG_SS].cache, ESP, 4) = 0) then
        LogPanic(('push_32(): push outside stack limits'));
        (* #SS(0) *)
    end else
    (* real mod_e *)
      if ((ESP >= 1) and (ESP <= 3)) then
        LogPanic(Format('push_32: ESP:=%08x',[ESP]));

    write_virtual_dword(SEG_REG_SS, ESP - 4, @value32);
    ESP := ESP - 4;
    (* will return after error anyway *)
    exit;
    end
  else begin (* 16bit stack size: pushes use SS:SP  *)
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    begin
      if (can_push(@sregs[SEG_REG_SS].cache, SP, 4)=0) then
        LogPanic(('push_32(): push outside stack limits'));
        (* #SS(0) *)
    end else
    begin (* real mod_e *)
      if ((SP >= 1) and (SP <= 3)) then
        LogPanic(Format('push_32: SP:=%08x',[SP]));
    end;
    write_virtual_dword(SEG_REG_SS, Bit16u(SP - 4), @value32);
    SP := SP - 4;
    (* will return after error anyway *)
    exit;
  end;
end;

procedure TCPU.pop_16(value16_ptr:pBit16u);
var
  temp_ESP: Bit32u;
begin
  if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if ( can_pop(2) = 0) then
    begin
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;
  (* access within limits *)
  read_virtual_word(SEG_REG_SS, temp_ESP, value16_ptr);

  if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then ESP := ESP + 2
    else SP := SP + 2;
end;

procedure  TCPU.pop_32(value32_ptr:pBit32u);
var
  temp_ESP: Bit32u;
begin
  (* 32 bit stack mod_e: use SS:ESP *)
  if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;

  (* 16 bit stack mod_e: use SS:SP *)
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm =0 ))) <> 0 then
    if Boolean( can_pop(4) = 0) then
    begin
      LogPanic(('pop_32(): can''t pop from stack'));
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;

  (* access within limits *)
  read_virtual_dword(SEG_REG_SS, temp_ESP, value32_ptr);

  if (sregs[SEG_REG_SS].cache.segment.d_b = 1)
    then ESP := ESP + 4
    else SP := SP + 4;
end;

function TCPU.can_push(descriptor:Pdescriptor_t; esp:Bit32u; bytes:Bit32u ):Bool;
var
  expand_down_limit: Bit32u;
begin
  if ( real_mode() ) <> 0 then
  begin (* code not needed ??? *)
    LogPanic(('can_push(): called in real mod_e'));
    Exit(0);
  end;
  // small stack compares against 16-bit SP
  if (descriptor^.segment.d_b = 0) then
    esp := esp and $0000ffff;

  if (descriptor^.valid = 0) then
  begin
    LogPanic(('can_push(): SS invalidated.'));
    Exit(0);
  end;

  if (descriptor^.p = 0) then
  begin
    LogPanic(('can_push(): not present'));
    Exit(0);
  end;

  if (descriptor^.segment.c_ed) <> 0 then (* expand down segment *)
  begin
    if (descriptor^.segment.d_b) <> 0
      then expand_down_limit := $ffffffff
      else expand_down_limit := $0000ffff;

    if (esp = 0) then
    begin
      LogPanic(('can_push(): esp:=0, wraparound?'));
      Exit(0);
    end;

    if (esp < bytes) then
    begin
      LogPanic(('can_push(): expand-down: esp < N'));
      Exit(0);
    end;
    if ( (esp - bytes) <= descriptor^.segment.limit_scaled ) then
    begin
      LogPanic(('can_push(): expand-down: esp-N < limit'));
      Exit(0);
    end;
    if ( esp > expand_down_limit ) then
    begin
      LogPanic(('can_push(): esp > expand-down-limit'));
      Exit(0);
    end;
    Result := 1;
  end else
  begin (* normal (expand-up) segment *)
    if (descriptor^.segment.limit_scaled = 0) then
    begin
      LogPanic(('can_push(): found limit of 0'));
      Exit(0);
    end;
    // Look at case where esp=0.  Possibly, it's an intentional wraparound
    // If so, limit must be the maximum for the given stack size
    if (esp = 0) then
    begin
      if ((descriptor^.segment.d_b <> 0) and (descriptor^.segment.limit_scaled=$ffffffff)) then
        Exit(1);
      if ((descriptor^.segment.d_b=0) and (descriptor^.segment.limit_scaled>=$ffff)) then
        Exit(1);
      LogPanic(Format('can_push(): esp:=0, normal, wraparound? limit:=%08x',[descriptor^.segment.limit_scaled]));
    end;

    if (esp < bytes) then
      Exit(0);
    if ((esp - 1) > descriptor^.segment.limit_scaled) then
      exit(0);
    (* all checks pass *)
    Result := 1;
  end;
end;

function TCPU.can_pop(bytes:Bit32u):Bool ;
var
  temp_ESP, expand_down_limit: Bit32u;
begin
  if (sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
  begin (* Big bit set: use ESP *)
    temp_ESP := ESP;
    expand_down_limit := $FFFFFFFF;
  end else
  begin (* Big bit clear: use SP *)
    temp_ESP := SP;
    expand_down_limit := $FFFF;
  end;

  if (sregs[SEG_REG_SS].cache.valid = 0) then
  begin
    Result := 0; (* never gets here *)
    Exit;
  end;

  if (sregs[SEG_REG_SS].cache.p = 0) then
  begin (* ??? *)
    Result := 0; (* never gets here *)
    Exit;
  end;

  if (sregs[SEG_REG_SS].cache.segment.c_ed) <> 0 then
  begin (* expand down segment *)
    if ( temp_ESP = expand_down_limit ) then
    begin
      Result := 0; (* never gets here *)
      Exit;
    end;
    if ( ((expand_down_limit - temp_ESP) + 1) >= bytes ) then
    begin
      Result := 1;
      Exit;
    end;
    Result := 0;
  end
  else begin (* normal (expand-up) segment *)
    if ( temp_ESP = expand_down_limit ) then
    begin
      Result := 0;
      Exit;
    end;
    if ( temp_ESP > sregs[SEG_REG_SS].cache.segment.limit_scaled ) then
    begin
      Result := 0;
      Exit;
    end;
    if ( ((sregs[SEG_REG_SS].cache.segment.limit_scaled - temp_ESP) + 1) >= bytes ) then
    begin
      Result := 1;
      Exit;
    end;
    Result := 0;
  end;
end;

procedure TCPU.UndefinedOpcode(Instruction:PInstruction_tag);
begin
//  BX_DEBUG(Format('UndefinedOpcode: %02x causes exception 6', [Instruction^.b1]));
  exception2([BX_UD_EXCEPTION, 0, 0]);
  //VEDERE CODICE SORGENTE ORIGINALE
end;

procedure TCPU.NOP(I: PInstruction_tag);
begin
  asm // NOP :)
    nop
  end;
end;

procedure TCPU.HLT(I: PInstruction_tag);
begin
  // hack to panic if HLT comes from BIOS
  if Boolean( sregs[SEG_REG_CS].selector.value = $f000 ) then
    LogPanic(('HALT instruction encountered in the BIOS ROM'));

  if Boolean(CPL <> 0) then
  begin
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;

  // stops instruction execution and places the processor in a
  // HALT state.  An enabled interrupt, NMI, or reset will resume
  // execution.  If interrupt (including NMI) is used to resume
  // execution after HLT, the saved CS:eIP points to instruction
  // following HLT.
  // artificial trap bit, why use another variable.
  debug_trap := debug_trap or $80000000; // artificial trap
  async_event := 1; // so processor knows to check
  // Execution of this instruction completes.  The processor
  // will remain in a halt state until one of the above conditions
  // is met.
{$if BX_USE_IDLE_HACK=1}
  bx_gui.sim_is_idle ();
{$ifend} (* BX_USE_IDLE_HACK *)
end;

procedure TCPU.CLTS(I: PInstruction_tag);
begin
  if Boolean(v8086_mode()) then
    LogPanic(('clts: v8086 mod_e unsupported'));
  (* read errata file *)
  // does CLTS also clear NT flag???
  // #GP(0) if CPL is not 0
  if Boolean(CPL <> 0) then
  begin
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;
  FCR0.ts := 0;
  Self.FCR0.val32 := Self.FCR0.val32 and not $08;
end;

procedure TCPU.INVD(I: PInstruction_tag);
begin
  invalidate_prefetch_q();
end;

procedure TCPU.WBINVD(I: PInstruction_tag);
begin
  invalidate_prefetch_q();
  if Boolean(Self.FCR0.pe) then
    if Boolean(CPL <> 0) then
      exception2([BX_GP_EXCEPTION, 0, 0]);
end;

procedure TCPU.MOV_DdRd(I: PInstruction_tag);
var
  val_32: Bit32u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('MOV_DdRd: v8086 mod_e unsupported'));
  (* NOTES:
   *   32bit operands always used
   *   r/m field specifies general register
   *   mod_ field should always be 11 binary
   *   reg field specifies which special register
   *)
  if Boolean(i^.mod_ <> $c0) then
    LogPanic(('MOV_DdRd(): rm field not a register!'));

  invalidate_prefetch_q();

  if Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) and CPL <> 0) then
  begin
    LogPanic(('MOV_DdRd: CPL <> 0'));
    (* #GP(0) if CPL is not 0 *)
    exception2([BX_GP_EXCEPTION, 0, 0]);
  end;
  val_32 := BX_READ_32BIT_REG(i^.rm);
  {if Boolean(bx_dbg.dreg)
    BX_INFO(('MOV_DdRd: DR[%u]:=%08xh unhandled',
      (unsigned) i^.nnn, (unsigned) val_32)); !!!}
  case (i^.nnn) of
    0: Self.FDR0 := val_32;// DR0
    1: Self.FDR1 := val_32;// DR1
    2: Self.FDR2 := val_32;// DR2
    3: Self.FDR3 := val_32;// DR3
    4, // DR4
    6: // DR6
      begin
        // DR4 aliased to DR6 by default.  With Debug Extensions on,
        // access to DR4 causes #UD
        if Boolean( (i^.nnn = 4) and Boolean(Self.FCR4 and $00000008) ) then
          UndefinedOpcode(i);

        // On Pentium+, bit12 is always zero
          Self.FDR6 := (Self.FDR6 and $ffff0ff0) or (val_32 and $0000e00f);
      end;
    5, // DR5
    7:
      begin// DR7
        // Note: 486+ ignore GE and LE flags.  On the 386, exact
        // data breakpoint matching does not occur unless it is enabled
        // by setting the LE and/or GE flags.

        // DR5 aliased to DR7 by default.  With Debug Extensions on,
        // access to DR5 causes #UD
        if Boolean( (i^.nnn = 5) and Boolean(Self.FCR4 and $00000008) ) then
          UndefinedOpcode(i);
        // Some sanity checks...
        if Boolean( val_32  and $00002000 ) then
          LogPanic(('MOV_DdRd: GD bit not supported yet'));
          // Note: processor clears GD upon entering debug exception
          // handler, to allow access to the debug registers

        if Boolean( (((val_32 shr 16)  and 3)=2) or
             (((val_32 shr 20)  and 3)=2) or
             (((val_32 shr 24)  and 3)=2) or
             (((val_32 shr 28)  and 3)=2) ) then
          // IO breakpoints (10b) are not yet supported.
          LogPanic(Format('MOV_DdRd: write of %08x contains IO breakpoint',[val_32]));

        if Boolean( (((val_32 shr 18)  and 3)=2) or
             (((val_32 shr 22)  and 3)=2) or
             (((val_32 shr 26)  and 3)=2) or
             (((val_32 shr 30)  and 3)=2) ) then
          // LEN0..3 contains undefined length specifier (10b)
          LogPanic(Format('MOV_DdRd: write of %08x contains undefined LENx',[val_32]));

        if Boolean( ((((val_32 shr 16)  and 3)=0) and (((val_32 shr 18)  and 3) <> 0)) or
             ((((val_32 shr 20)  and 3)=0) and (((val_32 shr 22)  and 3) <> 0)) or
             ((((val_32 shr 24)  and 3)=0) and (((val_32 shr 26)  and 3) <> 0)) or
             ((((val_32 shr 28)  and 3)=0) and (((val_32 shr 30)  and 3) <> 0)) ) then
          // Instruction breakpoint with LENx not 00b (1-byte length)
          LogPanic(Format('MOV_DdRd: write of %08x, R/W:=00b LEN <> 00b', [val_32]));
        // Pentium+: bits15,14,12 are hardwired to 0, rest are settable.
        // Even bits 11,10 are changeable though reserved.
        Self.FDR7 := (val_32  and $ffff2fff)or$00000400;
      end;
    else
      LogPanic(('MOV_DdRd: control register index out of range'));
    end;
end;

procedure TCPU.MOV_RdDd(I: PInstruction_tag);
var
  val_32: Bit32u;
begin
  if Boolean(v8086_mode()) then
    exception2([BX_GP_EXCEPTION, 0, 0]);

  if Boolean(i^.mod_ <> $c0) then
  begin
    LogPanic(('MOV_RdDd(): rm field not a register!'));
    UndefinedOpcode(i);
  end;

  if Boolean(Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) and (CPL <> 0)) then
  begin
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;
{  if Boolean(bx_dbg.dreg) then
    BX_INFO(('MOV_RdDd: DR%u not implemented yet', i^.nnn)); !!!}
  case (i^.nnn) of
    0: val_32 := Self.FDR0;// DR0
    1: val_32 := Self.FDR1;// DR1
    2: val_32 := Self.FDR2;// DR2
    3: val_32 := Self.FDR3;// DR3
    4, // DR4
    6:
      begin// DR6
        // DR4 aliased to DR6 by default.  With Debug Extensions on,
        // access to DR4 causes #UD
        if Boolean( (i^.nnn = 4) and Boolean(Self.FCR4 and $00000008) ) then
          UndefinedOpcode(i);
        val_32 := Self.FDR6;
      end;

    5, // DR5
    7:
      begin// DR7
        // DR5 aliased to DR7 by default.  With Debug Extensions on,
        // access to DR5 causes #UD
        if Boolean( (i^.nnn = 5) and Boolean(Self.FCR4 and $00000008) ) then
          UndefinedOpcode(i);
        val_32 := Self.FDR7;
      end;

    else
      begin
        LogPanic(('MOV_RdDd: control register index out of range'));
        val_32 := 0;
      end;
    end;
  BX_WRITE_32BIT_REG(i^.rm, val_32);
end;

procedure TCPU.LMSW_Ew(I: PInstruction_tag);
var
  msw: Bit16u;
  FCR0: Bit32u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('proc_ctrl: v8086 mod_e unsupported'));

  if Boolean( Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0)) ) then
    if Boolean( CPL  <>  0 ) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

  if Boolean(i^.mod_ = $c0)
    then msw := BX_READ_16BIT_REG(i^.rm)
    else read_virtual_word(i^.seg, i^.rm_addr, @msw);
  // LMSW does not affect PG,CD,NW,AM,WP,NE,ET bits, and cannot clear PE
  // LMSW cannot clear PE
  if Boolean( ((msw and $0001)=0) and Boolean(Self.FCR0.pe) ) then
    msw := msw or $0001; // adjust PE bit to current value of 1

  msw := msw and $000f; // LMSW only affects last 4 flags
  FCR0 := (Self.FCR0.val32 and $fffffff0) or msw;
  SetCR0(FCR0);
end;

procedure TCPU.SMSW_Ew(I: PInstruction_tag);
var
  msw: Bit16u;
begin
  (* reserved bits 0 ??? *)
  (* should NE bit be included here ??? *)
  // should ET bit be included here (AW)
  msw := (Self.FCR0.ts  shl  3) or
         (Self.FCR0.em  shl  2) or
         (Self.FCR0.mp  shl  1) or
          Self.FCR0.pe;

  if Boolean(i^.mod_ = $c0) then
  begin
    if Boolean(i^.os_32)
      then BX_WRITE_32BIT_REG(i^.rm, msw)  // zeros out high 16bits
      else BX_WRITE_16BIT_REG(i^.rm, msw);
  end else
    write_virtual_word(i^.seg, i^.rm_addr, @msw);
end;

procedure TCPU.MOV_CdRd(I: PInstruction_tag);
var
  val_32: Bit32u;
begin
  // mov general register data to control register
  if Boolean(v8086_mode) then
    LogPanic(('proc_ctrl: v8086 mod_e unsupported'));
  (* NOTES:
   *   32bit operands always used
   *   r/m field specifies general register
   *   mod_ field should always be 11 binary
   *   reg field specifies which special register
   *)
  if Boolean(i^.mod_ <> $c0) then
    LogPanic(('MOV_CdRd(): rm field not a register!'));

  invalidate_prefetch_q();

  if Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) and CPL <> 0) then
  begin
    LogPanic(('MOV_CdRd: CPL <> 0'));
    (* #GP(0) if CPL is not 0 *)
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;

  val_32 := BX_READ_32BIT_REG(i^.rm);

  case (i^.nnn) of
    0: SetCR0(val_32); // CR0 (MSW)
    1: LogPanic(('MOV_CdRd: CR1 not implemented yet'));(* CR1 *)
    2: (* CR2 *)
      begin
//        BX_DEBUG(('MOV_CdRd: CR2 not implemented yet'));
//	      BX_DEBUG(('MOV_CdRd: CR2 := reg'));
        Self.FCR2 := val_32;
      end;
    3: CR3_change(val_32); // CR3
    4: Self.FCR4 := 0; // CR4
    else
      LogPanic(('MOV_CdRd: control register index out of range'));
    end;
end;

procedure TCPU.MOV_RdCd(I: PInstruction_tag);
var
  val_32: Bit32u;
begin
  // mov control register data to register
  if Boolean(v8086_mode()) then
    LogPanic(('proc_ctrl: v8086 mod_e unsupported'));
  (* NOTES:
   *   32bit operands always used
   *   r/m field specifies general register
   *   mod_ field should always be 11 binary
   *   reg field specifies which special register
   *)
  if Boolean(i^.mod_  <>  $c0) then
    LogPanic(('MOV_RdCd(): rm field not a register!'));

  if Boolean(Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) and (CPL <> 0)) then
  begin
    LogPanic(('MOV_RdCd: CPL <> 0'));
    (* #GP(0) if CPL is not 0 *)
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;

  case (i^.nnn) of
    0: // CR0 (MSW)
      begin
        val_32 := Self.FCR0.val32;
      end;
    1: (* CR1 *)
      begin
        LogPanic(('MOV_RdCd: CR1 not implemented yet'));
        val_32 := 0;
      end;
    2: val_32 := Self.FCR2; (* CR2 *)
    3: val_32 := Self.FCR3; // CR3
    4: val_32 := Self.FCR4; // CR4
    else
      LogPanic(('MOV_RdCd: control register index out of range'));
      val_32 := 0;
    end;
  BX_WRITE_32BIT_REG(i^.rm, val_32);
end;

procedure TCPU.MOV_TdRd(I: PInstruction_tag);
begin
  // Pentium+ does not have TRx.  They were redesigned using the MSRs.
  UndefinedOpcode(i);
end;

procedure TCPU.MOV_RdTd(I: PInstruction_tag);
begin
  // Pentium+ does not have TRx.  They were redesigned using the MSRs.
  UndefinedOpcode(i);
end;

procedure TCPU.LOADALL(I: PInstruction_tag);
var
  msw, tr, flags, iplocal, ldtr: Bit16u;
  ds_raw, ss_raw, cs_raw, es_raw: Bit16u;
  di_, si_, bp_, sp_, bx_, dx_, cx_, ax_: Bit16u;
  base_15_0, limit: Bit16u;
  base_23_16, access: Bit8u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('proc_ctrl: v8086 mod_e unsupported'));
  LogPanic(('loadall: not implemented for 386'));
  (* ??? need to set G and other bits, and compute .limit_scaled also *)
  (* for all segments CS,DS,SS,... *)

  if Boolean(Self.FCR0.pe) then
    LogPanic((
      'LOADALL not yet supported for protected mod_e'));

  LogPanic(('LOADALL: handle CR0.val32'));
  (* MSW *)
  sysmemory.read_physical( $806, 2, @msw);
  Self.FCR0.pe := (msw and $01); msw := msw shr 1;
  Self.FCR0.mp := (msw and $01); msw := msw shr 1;
  Self.FCR0.em := (msw and $01); msw := msw shr 1;
  Self.FCR0.ts := (msw and $01);

  if Boolean(Self.FCR0.pe or Self.FCR0.mp or Self.FCR0.em or Self.FCR0.ts) then
    LogPanic(('LOADALL set PE, MP, EM or TS bits in MSW!'));

  (* TR *)
  sysmemory.read_physical($816, 2, @tr);
  Self.tr.selector.value := tr;
  Self.tr.selector.rpl   := (tr  and $03);  tr := tr shr 2;
  Self.tr.selector.ti    := (tr  and $01);  tr := tr shr 1;
  Self.tr.selector.index := tr;
  sysmemory.read_physical($860, 2, @base_15_0);
  sysmemory.read_physical($862, 1, @base_23_16);
  sysmemory.read_physical($863, 1, @access);
  sysmemory.read_physical($864, 2, @limit);

	Self.tr.cache.p            := (access  and $80) shr 7;
  Self.tr.cache.valid        := Self.tr.cache.p;
  Self.tr.cache.dpl          := (access  and $60) shr 5;
  Self.tr.cache.segmentType  := (access  and $10) shr 4;
  // don't allow busy bit in tr.cache.type, so bit 2 is masked away too.
  Self.tr.cache.type_        := (access  and $0d);
  Self.tr.cache.tss286.base  := (base_23_16 shl 16) or base_15_0;
  Self.tr.cache.tss286.limit := limit;

  if Boolean( (Self.tr.selector.value  and $fffc) = 0 ) or
     Boolean( Self.tr.cache.tss286.limit < 43 ) or
     Boolean( Self.tr.cache.type_ <> 1 ) or
     Boolean( Self.tr.cache.segmentType ) then
     Self.tr.cache.valid := 0;

  if Boolean(Self.tr.cache.valid = 0) then
  begin
    Self.tr.cache.tss286.base  := 0;
    Self.tr.cache.tss286.limit := 0;
    Self.tr.cache.p            := 0;
    Self.tr.selector.value     := 0;
    Self.tr.selector.index     := 0;
    Self.tr.selector.ti        := 0;
    Self.tr.selector.rpl       := 0;
  end;
  (* FLAGS *)
  sysmemory.read_physical($818, 2, @flags);
  //write_flags(flags, 1, 1);
  (* IP *)
  sysmemory.read_physical($81a, 2, @iplocal);
  EIP := iplocal;
  (* LDTR *)
  sysmemory.read_physical($81c, 2, @ldtr);
  Self.ldtr.selector.value      := ldtr;
  Self.ldtr.selector.rpl        := (ldtr  and $03);  ldtr:= ldtr shr  2;
  Self.ldtr.selector.ti         := (ldtr  and $01);  ldtr:= ldtr shr  1;
  Self.ldtr.selector.index      := ldtr;
  if Boolean( (Self.ldtr.selector.value  and $fffc) = 0 ) then
  begin
    Self.ldtr.cache.valid       := 0;
    Self.ldtr.cache.p           := 0;
    Self.ldtr.cache.segmentType := 0;
    Self.ldtr.cache.type_       := 0;
    Self.ldtr.cache.ldt.base    := 0;
    Self.ldtr.cache.ldt.limit   := 0;
    Self.ldtr.selector.value    := 0;
    Self.ldtr.selector.index    := 0;
    Self.ldtr.selector.ti       := 0;
  end else
  begin
    sysmemory.read_physical($854, 2, @base_15_0);
    sysmemory.read_physical($856, 1, @base_23_16);
    sysmemory.read_physical($857, 1, @access);
    sysmemory.read_physical($858, 2, @limit);
		Self.ldtr.cache.p          := access  shr  7;
    Self.ldtr.cache.valid      := Self.ldtr.cache.p;
    Self.ldtr.cache.dpl        := (access  shr  5)  and $03;
    Self.ldtr.cache.segmentType:= (access  shr  4)  and $01;
    Self.ldtr.cache.type_      := (access  and $0f);
    Self.ldtr.cache.ldt.base   := (base_23_16  shl  16) or base_15_0;
    Self.ldtr.cache.ldt.limit  := limit;
  end;
  (* DS *)
  sysmemory.read_physical($81e, 2, @ds_raw);
  Self.sregs[SEG_REG_DS].selector.value := ds_raw;
  Self.sregs[SEG_REG_DS].selector.rpl   := (ds_raw  and $03);  ds_raw := ds_raw shr 2;
  Self.sregs[SEG_REG_DS].selector.ti    := (ds_raw  and $01);  ds_raw := ds_raw shr 1;
  Self.sregs[SEG_REG_DS].selector.index := ds_raw;
  sysmemory.read_physical($848, 2, @base_15_0);
  sysmemory.read_physical($84a, 1, @base_23_16);
  sysmemory.read_physical($84b, 1, @access);
  sysmemory.read_physical($84c, 2, @limit);
  Self.sregs[SEG_REG_DS].cache.segment.base        := (base_23_16  shl  16) or base_15_0;
  Self.sregs[SEG_REG_DS].cache.segment.limit       := limit;
  Self.sregs[SEG_REG_DS].cache.segment.a           := (access  and $01); access := access  shr 1;
  Self.sregs[SEG_REG_DS].cache.segment.r_w         := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_DS].cache.segment.c_ed        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_DS].cache.segment.executable  := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_DS].cache.segmentType         := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_DS].cache.dpl                 := (access  and $03); access := access shr 2;
	Self.sregs[SEG_REG_DS].cache.p                   := (access  and $01);
  Self.sregs[SEG_REG_DS].cache.valid               := Self.sregs[SEG_REG_DS].cache.p;

  if Boolean( (Self.sregs[SEG_REG_DS].selector.value  and $fffc) = 0 ) then
    Self.sregs[SEG_REG_DS].cache.valid := 0;

  (* SS *)
  sysmemory.read_physical($820, 2, @ss_raw);
  Self.sregs[SEG_REG_SS].selector.value           := ss_raw;
  Self.sregs[SEG_REG_SS].selector.rpl             := (ss_raw and $03); ss_raw := ss_raw shr 2;
  Self.sregs[SEG_REG_SS].selector.ti              := (ss_raw and $01); ss_raw := ss_raw shr 1;
  Self.sregs[SEG_REG_SS].selector.index           := ss_raw;
  sysmemory.read_physical($842, 2, @base_15_0);
  sysmemory.read_physical($844, 1, @base_23_16);
  sysmemory.read_physical($845, 1, @access);
  sysmemory.read_physical($846, 2, @limit);
  Self.sregs[SEG_REG_SS].cache.segment.base := (base_23_16  shl  16) or base_15_0;
  Self.sregs[SEG_REG_SS].cache.segment.limit := limit;
  Self.sregs[SEG_REG_SS].cache.segment.a          := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_SS].cache.segment.r_w        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_SS].cache.segment.c_ed       := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_SS].cache.segment.executable := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_SS].cache.segmentType        := (access and $01); access := access shr 1;
  Self.sregs[SEG_REG_SS].cache.dpl                := (access and $03); access := access shr 2;
  Self.sregs[SEG_REG_SS].cache.p                  := (access and $01);

  if Boolean( (Self.sregs[SEG_REG_SS].selector.value  and $fffc) = 0 ) then
    Self.sregs[SEG_REG_SS].cache.valid := 0;

  (* CS *)
  sysmemory.read_physical($822, 2, @cs_raw);
  Self.sregs[SEG_REG_CS].selector.value := cs_raw;
  Self.sregs[SEG_REG_CS].selector.rpl   := (cs_raw  and $03); cs_raw := cs_raw shr 2;

  Self.sregs[SEG_REG_CS].selector.ti              := (cs_raw  and $01); cs_raw := cs_raw shr 1;
  Self.sregs[SEG_REG_CS].selector.index           := cs_raw;
  sysmemory.read_physical($83c, 2, @base_15_0);
  sysmemory.read_physical($83e, 1, @base_23_16);
  sysmemory.read_physical($83f, 1, @access);
  sysmemory.read_physical($840, 2, @limit);
  Self.sregs[SEG_REG_CS].cache.segment.base       := (base_23_16 shl 16) or base_15_0;
  Self.sregs[SEG_REG_CS].cache.segment.limit      := limit;
  Self.sregs[SEG_REG_CS].cache.segment.a          := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_CS].cache.segment.r_w        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_CS].cache.segment.c_ed       := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_CS].cache.segment.executable := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_CS].cache.segmentType        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_CS].cache.dpl                := (access  and $03); access := access shr 2;
  Self.sregs[SEG_REG_CS].cache.p                  := (access  and $01);

  if Boolean( (Self.sregs[SEG_REG_CS].selector.value  and $fffc) = 0 ) then
    Self.sregs[SEG_REG_CS].cache.valid := 0;

  (* ES *)
  sysmemory.read_physical($824, 2, @es_raw);
  Self.sregs[SEG_REG_ES].selector.value           := es_raw;
  Self.sregs[SEG_REG_ES].selector.rpl             := (es_raw  and $03); es_raw := es_raw shr 2;
  Self.sregs[SEG_REG_ES].selector.ti              := (es_raw  and $01); es_raw := es_raw  shr 1;
  Self.sregs[SEG_REG_ES].selector.index           := es_raw;
  sysmemory.read_physical($836, 2, @base_15_0);
  sysmemory.read_physical($838, 1, @base_23_16);
  sysmemory.read_physical($839, 1, @access);
  sysmemory.read_physical($83a, 2, @limit);
  Self.sregs[SEG_REG_ES].cache.segment.base       := (base_23_16  shl  16) or base_15_0;
  Self.sregs[SEG_REG_ES].cache.segment.limit      := limit;
  Self.sregs[SEG_REG_ES].cache.segment.a          := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_ES].cache.segment.r_w        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_ES].cache.segment.c_ed       := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_ES].cache.segment.executable := (access  and $01); access := access  shr 1;
  Self.sregs[SEG_REG_ES].cache.segmentType        := (access  and $01); access := access shr 1;
  Self.sregs[SEG_REG_ES].cache.dpl                := (access  and $03); access := access shr 2;
  Self.sregs[SEG_REG_ES].cache.p                  := (access  and $01);

  if Boolean( (Self.sregs[SEG_REG_ES].selector.value  and $fffc) = 0 ) then
    Self.sregs[SEG_REG_ES].cache.valid := 0;

  (* DI *)
  sysmemory.read_physical($826, 2, @di_);
  DI := di_;
  (* SI *)
  sysmemory.read_physical($828, 2, @si_);
  SI := si_;
  (* BP *)
  sysmemory.read_physical($82a, 2, @bp_);
  BP := bp_;
  (* SP *)
  sysmemory.read_physical($82c, 2, @sp_);
  SP := sp_;
  (* BX *)
  sysmemory.read_physical($82e, 2, @bx_);
  BX := bx_;
  (* DX *)
  sysmemory.read_physical($830, 2, @dx_);
  DX := dx_;
  (* CX *)
  sysmemory.read_physical($832, 2, @cx_);
  CX := cx_;
  (* AX *)
  sysmemory.read_physical($834, 2, @ax_);
  AX := ax_;
  (* GDTR *)
  sysmemory.read_physical($84e, 2, @base_15_0);
  sysmemory.read_physical($850, 1, @base_23_16);
  sysmemory.read_physical($851, 1, @access);
  sysmemory.read_physical($852, 2, @limit);
  Self.gdtr.base  := (base_23_16 shl 16) or base_15_0;
  Self.gdtr.limit := limit;
  (* IDTR *)
  sysmemory.read_physical($85a, 2, @base_15_0);
  sysmemory.read_physical($85c, 1, @base_23_16);
  sysmemory.read_physical($85d, 1, @access);
  sysmemory.read_physical($85e, 2, @limit);
  Self.idtr.base  := (base_23_16 shl 16) or base_15_0;
  Self.idtr.limit := limit;
end;

procedure TCPU.CPUID(I: PInstruction_tag);
var
  family, mod_el, stepping, features: Word;
begin
  invalidate_prefetch_q();
  case EAX of
    0:
      begin
        // EAX: highest input value understood by CPUID
        // EBX: vendor ID string
        // EDX: vendor ID string
        // ECX: vendor ID string
        EAX := 1; // 486 or pentium
        EBX := $756e6547; // 'Genu'
        EDX := $49656e69; // 'ineI'
        ECX := $6c65746e; // 'ntel'
      end;

    1:
      begin
        // EAX[3:0]   Stepping ID
        // EAX[7:4]   mod_el: starts at 1
        // EAX[11:8]  Family: 4:=486, 5:=Pentium, 6:=PPro
        // EAX[13:12] Type: 0:=OEM,1:=overdrive,2:=dual cpu,3:=reserved
        // EAX[31:14] Reserved
        // EBX:       Reserved (0)
        // ECX:       Reserved (0)
        // EDX:       Feature Flags
        //   [0:0]   FPU on chip
        //   [1:1]   VME: Virtual-8086 mod_e enhancements
        //   [2:2]   DE: Debug Extensions (I/O breakpoints)
        //   [3:3]   PSE: Page Size Extensions
        //   [4:4]   TSC: Time Stamp Counter
        //   [5:5]   MSR: RDMSR and WRMSR support
        //   [6:6]   PAE: Physical Address Extensions
        //   [7:7]   MCE: Machine Check Exception
        //   [8:8]   CXS: CMPXCHG8B instruction
        //   [9:9]   APIC: APIC on Chip
        //   [11:10] Reserved
        //   [12:12] MTRR: Memory Type Range Reg
        //   [13:13] PGE/PTE Global Bit
        //   [14:14] MCA: Machine Check Architecture
        //   [15:15] CMOV: Cond Mov/Cmp Instructions
        //   [22:16] Reserved
        //   [23:23] MMX Technology
        //   [31:24] Reserved

        features := 0; // start with none
//        type_ := 0; // OEM


        family := 6;
        mod_el := 1; // Pentium Pro
        stepping := 3; // ???
        features := features or (1 shl 4);   // implement TSC
  {$if BX_SUPPORT_APIC = $01}
        features := features or (1 shl 9);   // APIC on chip
  {$ifend}
        if BX_SUPPORT_FPU then
          features := features or $01;

        EAX := (family  shl 8)or(mod_el shl 4) or stepping;
        ECX := 0;
        EBX := ECX; // reserved
        EDX := features;
      end;

  else
    begin
      EAX := 0;
      EBX := 0;
      ECX := 0;
      EDX := 0; // Reserved, undefined
    end;
  end;
end;

procedure TCPU.SetCR0(val_32:Bit32u);
var
  prev_pe, prev_pg:Bool;
begin
  // from either MOV_CdRd() or debug functions
  // protection checks made already or forcing from debug

  prev_pe := Self.FCR0.pe;
  prev_pg := Self.FCR0.pg;

  Self.FCR0.pe :=  val_32  and  $01;
  Self.FCR0.mp := (val_32  shr  1 ) and $01;
  Self.FCR0.em := (val_32  shr  2 ) and $01;
  Self.FCR0.ts := (val_32  shr  3 ) and $01;
  // cr0.et is hardwired to 1
  Self.FCR0.ne := (val_32  shr  5 ) and $01;
  Self.FCR0.wp := (val_32  shr  16) and $01;
  Self.FCR0.am := (val_32  shr  18) and $01;
  Self.FCR0.nw := (val_32  shr  29) and $01;
  Self.FCR0.cd := (val_32  shr  30) and $01;
  Self.FCR0.pg := (val_32  shr  31) and $01;

  // handle reserved bits behaviour
  Self.FCR0.val32 := (val_32 or $00000010)  and $e005003f;

  if Boolean((prev_pe = 0) and Boolean(Self.FCR0.pe))
    then enter_protected_mode()
    else
  if Boolean((prev_pe = 1) and (Self.FCR0.pe = 0)) then
    enter_real_mode();

  if Boolean((prev_pg = 0) and Boolean(Self.FCR0.pg))
    then enable_paging()
    else
  if Boolean((prev_pg = 1) and Boolean(Self.FCR0.pg = 0)) then
    disable_paging();
end;

procedure TCPU.RSM(I: PInstruction_tag);
begin
  invalidate_prefetch_q();

  LogPanic(('RSM: System Management mod_e not implemented yet'));
end;

procedure TCPU.RDTSC(I: PInstruction_tag);
var
  tsd:Bool;
  ticks:Bit64u;
begin
  tsd := Bool(Boolean(Self.FCR4 and 4));
  if Boolean((tsd = 0) or ((tsd = 1) and (CPL = 0))) then
  begin
    // return ticks
    ticks := bx_pc_system.time_ticks ();
    EAX := Bit32u(ticks and $ffffffff);
    EDX := Bit32u((ticks shr 32)  and $ffffffff);
  end else
    exception2([BX_GP_EXCEPTION, 0, 0]); // not allowed to use RDTSC!
end;

procedure TCPU.RDMSR(I: PInstruction_tag);
label do_exception;
begin
	invalidate_prefetch_q();
  if Boolean(v8086_mode()) then
		goto do_exception;

  if Boolean(CPL <>  0) then
		goto do_exception;

	(* We have the requested MSR register in ECX *)
  case ECX of
		(* These are noops on i686... *)
		BX_MSR_P5_MC_ADDR,
		BX_MSR_MC_TYPE: ;	(* do nothing *)

		BX_MSR_TSC: RDTSC(i);

		(* ... And these cause an exception on i686 *)
		BX_MSR_CESR,
		BX_MSR_CTR0,
		BX_MSR_CTR1:
			goto do_exception;

		(* MSR_APICBASE
		   0:7		Reserved
		   8		This is set if its the BSP
		   9:10		Reserved
		   11		APIC Global Enable bit (1:=enabled 0:=disabled)
		   12:35	APIC Base Address
		   36:63	Reserved
		*)
		BX_MSR_APICBASE:
      begin
        (* we return low 32 bits in EAX, and high in EDX *)
        EAX := Self.msr.apicbase  and $ff;
        EDX := Self.msr.apicbase  shr  32;
      end;
  else
    goto do_exception;
  end;

do_exception:
	exception2([BX_GP_EXCEPTION, 0, 0]);
end;

procedure TCPU.WRMSR(I: PInstruction_tag);
label do_exception;
begin
	invalidate_prefetch_q();

  if Boolean(v8086_mode()) then
		goto do_exception;

  if Boolean(CPL <>  0) then
		goto do_exception;

	(* ECX has the MSR to write to *)
  case ECX of

		(* These are noops on i686... *)
		BX_MSR_P5_MC_ADDR,
		BX_MSR_MC_TYPE,
		BX_MSR_TSC:
      begin
  			(* do nothing *)
			end;

		(* ... And these cause an exception on i686 *)
		BX_MSR_CESR,
		BX_MSR_CTR0,
		BX_MSR_CTR1:
      begin
  			goto do_exception;
      end;

		(* MSR_APICBASE
		   0:7		Reserved
		   8		This is set if its the BSP
		   9:10		Reserved
		   11		APIC Global Enable bit (1:=enabled 0:=disabled)
		   12:35	APIC Base Address
		   36:63	Reserved
		*)
		BX_MSR_APICBASE: Self.msr.apicbase := Bit64u(EDX  shl  32) + EAX;
  else
    goto do_exception;
  end;

do_exception:
	exception2([BX_GP_EXCEPTION, 0, 0]);
end;

//{$if BX_X86_DEBUGGER=1}
//  Bit32u
//TCPU.hwdebug_compare(Bit32u laddr_0, unsigned size,
//                          unsigned opa, unsigned opb)
//begin
//  // Support x86 hardware debug facilities (DR0..DR7)
//  Bit32u dr7 := Self.dr7;
//
//  Boolean ibpoint_found := 0;
//  Bit32u  laddr_n := laddr_0 + (size - 1);
//  Bit32u  dr0, dr1, dr2, dr3;
//  Bit32u  dr0_n, dr1_n, dr2_n, dr3_n;
//  Bit32u  len0, len1, len2, len3;
//  static  unsigned alignment_mask[4] :=
//    //    00b:=1      01b:=2     10b:=undef     11b:=4
//    begin $ffffffff, $fffffffe, $ffffffff, $fffffffc end;;
//  Bit32u dr0_op, dr1_op, dr2_op, dr3_op;
//
//  len0 := (dr7 shr 18)  and 3;
//  len1 := (dr7 shr 22)  and 3;
//  len2 := (dr7 shr 26)  and 3;
//  len3 := (dr7 shr 30)  and 3;
//
//  dr0 := Self.dr0  and alignment_mask[len0];
//  dr1 := Self.dr1  and alignment_mask[len1];
//  dr2 := Self.dr2  and alignment_mask[len2];
//  dr3 := Self.dr3  and alignment_mask[len3];
//
//  dr0_n := dr0 + len0;
//  dr1_n := dr1 + len1;
//  dr2_n := dr2 + len2;
//  dr3_n := dr3 + len3;
//
//  dr0_op := (dr7 shr 16)  and 3;
//  dr1_op := (dr7 shr 20)  and 3;
//  dr2_op := (dr7 shr 24)  and 3;
//  dr3_op := (dr7 shr 28)  and 3;
//
//  // See if this instruction address matches any breakpoints
//  if Boolean( (dr7  and $00000003) ) then begin
//    if Boolean( (dr0_op=opa or dr0_op=opb) @@
//         (laddr_0 <= dr0_n) @@
//         (laddr_n >= dr0) )
//      ibpoint_found := 1;
//    end;
//  if Boolean( (dr7  and $0000000c) ) then begin
//    if Boolean( (dr1_op=opa or dr1_op=opb) @@
//         (laddr_0 <= dr1_n) @@
//         (laddr_n >= dr1) )
//      ibpoint_found := 1;
//    end;
//  if Boolean( (dr7  and $00000030) ) then begin
//    if Boolean( (dr2_op=opa or dr2_op=opb) @@
//         (laddr_0 <= dr2_n) @@
//         (laddr_n >= dr2) )
//      ibpoint_found := 1;
//    end;
//  if Boolean( (dr7  and $000000c0) ) then begin
//    if Boolean( (dr3_op=opa or dr3_op=opb) @@
//         (laddr_0 <= dr3_n) @@
//         (laddr_n >= dr3) )
//      ibpoint_found := 1;
//    end;
//
//  // If *any* enabled breakpoints matched, then we need to
//  // set status bits for *all* breakpoints, even disabled ones,
//  // as long as they meet the other breakpoint criteria.
//  // This code is similar to that above, only without the
//  // breakpoint enabled check.  Seems weird to duplicate effort,
//  // but its more efficient to do it this way.
//  if Boolean(ibpoint_found) then begin
//    // dr6_mask is the return value.  These bits represent the bits to
//    // be OR'd into DR6 as a result of the debug event.
//    Bit32u  dr6_mask:=0;
//    if Boolean( (dr0_op=opa or dr0_op=opb) @@
//         (laddr_0 <= dr0_n) @@
//         (laddr_n >= dr0) )
//      dr6_mask |:= $01;
//    if Boolean( (dr1_op=opa or dr1_op=opb) @@
//         (laddr_0 <= dr1_n) @@
//         (laddr_n >= dr1) )
//      dr6_mask |:= $02;
//    if Boolean( (dr2_op=opa or dr2_op=opb) @@
//         (laddr_0 <= dr2_n) @@
//         (laddr_n >= dr2) )
//      dr6_mask |:= $04;
//    if Boolean( (dr3_op=opa or dr3_op=opb) @@
//         (laddr_0 <= dr3_n) @@
//         (laddr_n >= dr3) )
//      dr6_mask |:= $08;
//    return(dr6_mask);
//    end;
//  return(0);
//end;
//{$ifend}

procedure TCPU.SETO_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_OF() <> 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNO_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_OF() = 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETB_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_CF() <> 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNB_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_CF() = 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETZ_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_ZF() <> 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNZ_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_ZF() = 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETBE_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u((get_CF() <> 0) or (get_ZF() <> 0));
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNBE_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u((get_CF() = 0) and (get_ZF() = 0));
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETS_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_SF() <> 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNS_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_SF() = 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETP_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_PF() <> 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNP_Eb(I: PInstruction_tag);
var
  result_8:Bit8u;
begin
  result_8 := Bit8u(get_PF() = 0);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETL_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_SF() <> get_OF());
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNL_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u(get_SF() = get_OF());
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETLE_Eb(I: PInstruction_tag);
var
  result_8:Bit8u;
begin
  result_8 := Bit8u((get_ZF() <> 0) or (get_SF() <> get_OF()));
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.SETNLE_Eb(I: PInstruction_tag);
var
  result_8: Bit8u;
begin
  result_8 := Bit8u((get_ZF() = 0) and (get_SF() = get_OF()));
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_virtual_byte(i^.seg, i^.rm_addr, @result_8);
end;

procedure TCPU.BSF_GvEv(I: PInstruction_tag);
var
  op1_32, op2_32: Bit32u;
  op1_16, op2_16: Bit16u;
begin
  if (i^.os_32)<>0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    (* op2_32 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op2_32 := BX_READ_32BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

    if (op2_32 = 0) then
    begin
      set_ZF(1);
      (* op1_32 undefined *)
      exit;
    end;

    op1_32 := 0;
    while (op2_32 and $01) = 0 do
    begin
      Inc(op1_32);   //op1_32++
      op2_32:=op2_32 shr 1;
    end;
    set_ZF(0);
    (* now write result back to destination *)
    BX_WRITE_32BIT_REG(i^.nnn, op1_32);
  end else
  begin (* 16 bit operand size mod_e *)
    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op2_16 := BX_READ_16BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

    if (op2_16 = 0) then
    begin
      set_ZF(1);
      (* op1_16 undefined *)
      exit;
    end;

    op1_16 := 0;
    while (op2_16 and $01) = 0 do
    begin
      Inc(op1_16); //op1_16++
      op2_16 := op2_16 shr 1;
    end;
    set_ZF(0);
    (* now write result back to destination *)
    BX_WRITE_16BIT_REG(i^.nnn, op1_16);
  end;
end;

procedure TCPU.BSR_GvEv(I: PInstruction_tag);
var
  op1_32, op2_32: Bit32u;
  op1_16, op2_16: Bit16u;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    (* op2_32 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op2_32 := BX_READ_32BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

    if (op2_32 = 0) then
    begin
      set_ZF(1);
      (* op1_32 undefined *)
      exit;
    end;

    op1_32 := 31;
    while ((op2_32  and $80000000) = 0) do
    begin
      Dec(op1_32);
      op2_32 := op2_32 shl 1;
    end;
    set_ZF(0);
    (* now write result back to destination *)
    BX_WRITE_32BIT_REG(i^.nnn, op1_32);
  end else
  begin (* 16 bit operand size mod_e *)
    (* op2_16 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op2_16 := BX_READ_16BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

    if (op2_16 = 0) then
    begin
      set_ZF(1);
      (* op1_16 undefined *)
      exit;
    end;

    op1_16 := 15;
    while ( (op2_16  and $8000) = 0 ) do
    begin
      Dec(op1_16);
      op2_16 := op2_16 shl 1;
    end;
    set_ZF(0);
    (* now write result back to destination *)
    BX_WRITE_16BIT_REG(i^.nnn, op1_16);
  end;
end;

procedure TCPU.BSWAP_EAX(I: PInstruction_tag);
var
  eax_, b0, b1, b2, b3: Bit32u;
begin
  eax_ := EAX;
  b0  := eax_ and $ff; eax_ := eax_ shr 8;
  b1  := eax_ and $ff; eax_ := eax_ shr 8;
  b2  := eax_ and $ff; eax_ := eax_ shr 8;
  b3  := eax_;
  EAX := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_ECX(I: PInstruction_tag);
var
  ecx_, b0, b1, b2, b3: Bit32u;
begin
  ecx_ := ECX;
  b0  := ecx_  and $ff; ecx_ := ecx_ shr 8;
  b1  := ecx_  and $ff; ecx_ := ecx_ shr 8;
  b2  := ecx_  and $ff; ecx_ := ecx_ shr 8;
  b3  := ecx_;
  ECX := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_EDX(I: PInstruction_tag);
var
  edx_, b0, b1, b2, b3: Bit32u;
begin
  edx_ := EDX;
  b0  := edx_  and $ff; edx_ := edx_ shr 8;
  b1  := edx_  and $ff; edx_ := edx_ shr 8;
  b2  := edx_  and $ff; edx_ := edx_ shr 8;
  b3  := edx_;
  EDX := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_EBX(I: PInstruction_tag);
var
  ebx_, b0, b1, b2, b3: Bit32u;
begin
  ebx_ := EBX;
  b0  := ebx_  and $ff; ebx_ := ebx_ shr 8;
  b1  := ebx_  and $ff; ebx_ := ebx_ shr 8;
  b2  := ebx_  and $ff; ebx_ := ebx_ shr 8;
  b3  := ebx_;
  EBX := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_ESP(I: PInstruction_tag);
var
  esp_, b0, b1, b2, b3: Bit32u;
begin
  esp_ := ESP;
  b0  := esp_  and $ff; esp_ := esp_ shr 8;
  b1  := esp_  and $ff; esp_ := esp_ shr 8;
  b2  := esp_  and $ff; esp_ := esp_ shr 8;
  b3  := esp_;
  ESP := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_EBP(I: PInstruction_tag);
var
  ebp_, b0, b1, b2, b3: Bit32u;
begin
  ebp_ := EBP;
  b0  := ebp_  and $ff; ebp_ := ebp_ shr 8;
  b1  := ebp_  and $ff; ebp_ := ebp_ shr 8;
  b2  := ebp_  and $ff; ebp_ := ebp_ shr 8;
  b3  := ebp_;
  EBP := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_ESI(I: PInstruction_tag);
var
  esi_, b0, b1, b2, b3: Bit32u;
begin
  esi_ := ESI;
  b0  := esi_  and $ff; esi_ := esi_ shr 8;
  b1  := esi_  and $ff; esi_ := esi_ shr 8;
  b2  := esi_  and $ff; esi_ := esi_ shr 8;
  b3  := esi_;
  ESI := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BSWAP_EDI(I: PInstruction_tag);
var
  edi_, b0, b1, b2, b3: Bit32u;
begin
  edi_ := EDI;
  b0  := edi_  and $ff; edi_:=edi_ shr 8;
  b1  := edi_  and $ff; edi_:=edi_ shr 8;
  b2  := edi_  and $ff; edi_:=edi_ shr 8;
  b3  := edi_;
  EDI := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
end;

procedure TCPU.BT_EvGv(I: PInstruction_tag);
var
  op1_addr: Bit32u;
  op1_32, op2_32, index: Bit32u;
  displacement32: Bit32s;
  op2_16, op1_16: Bit16u;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    (* op2_32 is a register, op2_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      op2_32 := op2_32 and $1f;
      set_CF((op1_32 shr op2_32) and $01);
      exit;
    end;

    index := op2_32  and $1f;
    //displacement32 := Bit32s(op2_32 and $ffffffe0) / 32; !!!
    displacement32 := Bit32s(op2_32 and $ffffffe0) div 32;
    op1_addr := i^.rm_addr + 4 * displacement32;

    (* pointer, segment address pair *)
    read_virtual_dword(i^.seg, op1_addr, @op1_32);

    set_CF((op1_32 shr index)  and $01);
  end else
  begin (* 16 bit operand size mod_e *)
    (* op2_16 is a register, op2_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      op2_16 := op2_16 and $0f;
      set_CF((op1_16 shr op2_16)  and $01);
      exit;
    end;

    index := op2_16  and $0f;
    //displacement32 := ((Bit16s(op2_16@$fff0)) / 16; !!!
    displacement32 := Bit16s(op2_16 and $fff0) div 16;
    op1_addr := i^.rm_addr + 2 * displacement32;
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, op1_addr, @op1_16);

    set_CF((op1_16 shr index)  and $01);
  end;
end;

procedure TCPU.BTS_EvGv(I: PInstruction_tag);
var
  op1_addr: Bit32u;
  op1_32, op2_32, bit_i, index: Bit32u;
  displacement32: Bit32s;
  op1_16, op2_16: Bit16u;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    (* op2_32 is a register, op2_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      op2_32 := op2_32 and $1f;
      set_CF((op1_32 shr op2_32) and $01);
      op1_32 := op1_32 or (Bit32u(1) shl op2_32);
      (* now write diff back to destination *)
      BX_WRITE_32BIT_REG(i^.rm, op1_32);
      exit;
    end;

    index := op2_32  and $1f;
    //displacement32 := ((Bit32s((op2_32@$ffffffe0)) / 32; !!!
    displacement32 := Bit32s(op2_32 and $ffffffe0) div 32;
    op1_addr := i^.rm_addr + 4 * displacement32;
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, op1_addr, @op1_32);

    bit_i := (op1_32 shr index)  and $01;
    op1_32 := op1_32 or (Bit32u(1) shl index);

    write_RMW_virtual_dword(op1_32);

    set_CF(bit_i);
  end else
  begin (* 16 bit operand size mod_e *)
    (* op2_16 is a register, op2_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      op2_16 := op2_16 and $0f;
      set_CF((op1_16 shr op2_16)  and $01);
      op1_16 := op1_16 or (Bit16u(1) shl op2_16);
      (* now write diff back to destination *)
      BX_WRITE_16BIT_REG(i^.rm, op1_16);
      exit;
    end;

    index := op2_16 and $0f;
    //displacement32 := ((Bit16s(op2_16 @$fff0)) / 16;
    displacement32 := Bit16s(op2_16 and $fff0) div 16;
    op1_addr := i^.rm_addr + 2 * displacement32;
    (* pointer, segment address pair *)
    read_RMW_virtual_word(i^.seg, op1_addr, @op1_16);

    bit_i := (op1_16 shr index)  and $01;
    op1_16 := op1_16 or (Bit16u(1) shl index);

    write_RMW_virtual_word(op1_16);

    set_CF(bit_i);
  end;
end;

procedure TCPU.BTR_EvGv(I: PInstruction_tag);
var
  op1_addr: Bit32u;
  op1_32, op2_32, index, temp_cf: Bit32u;
  displacement32: Bit32s;
  op2_16, op1_16: Bit16u;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    (* op2_32 is a register, op2_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      op2_32 := op2_32 and $1f;
      set_CF((op1_32 shr op2_32) and $01);
      op1_32 := op1_32 and not (Bit32u(1) shl op2_32);
      (* now write diff back to destination *)
      BX_WRITE_32BIT_REG(i^.rm, op1_32);
      exit;
    end;

    index := op2_32  and $1f;
    //displacement32 := (Bit32s(op2_32 and $ffffffe0)) / 32;
    displacement32 := (Bit32s(op2_32 and $ffffffe0)) div 32;
    op1_addr := i^.rm_addr + 4 * displacement32;
    (* pointer, segment address pair *)
    read_RMW_virtual_dword(i^.seg, op1_addr, @op1_32);

    temp_cf := (op1_32 shr index)  and $01;
    //op1_32 @:= ~(((Bit32u) 1) shl index); !!!
    op1_32 := op1_32 and not (Bit32u(1) shl index);
    (* now write back to destination *)
    write_RMW_virtual_dword(op1_32);

    set_CF(temp_cf);
  end else
  begin (* 16 bit operand size mod_e *)
    (* op2_16 is a register, op2_addr is an index of a register *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then
    begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      op2_16 := op2_16 and $0f;
      set_CF((op1_16 shr op2_16)  and $01);
      //op1_16 @:= ~(((Bit16u) 1) shl op2_16); !!!
      op1_16 := op1_16 and not (Bit16u(1) shl op2_16);

      (* now write diff back to destination *)
      BX_WRITE_16BIT_REG(i^.rm, op1_16);
      exit;
    end;

    index := op2_16 and $0f;
    //displacement32 := (Bit16s(op2_16 and $fff0)) / 16; !!!
    displacement32 := (Bit16s(op2_16 and $fff0)) div 16;
    op1_addr := i^.rm_addr + 2 * displacement32;

    (* pointer, segment address pair *)
    read_RMW_virtual_word(i^.seg, op1_addr, @op1_16);

    temp_cf := (op1_16 shr index)  and $01;
    //op1_16 @:= ~(((Bit16u) 1) shl index); !!!
    op1_16 := op1_16 and not (Bit16u(1) shl index);

    (* now write back to destination *)
    write_RMW_virtual_word(op1_16);

    set_CF(temp_cf);
  end;
end;

procedure TCPU.BTC_EvGv(I: PInstruction_tag);
var
  op1_addr: Bit32u;
  op1_32, op2_32, index_32, temp_CF: Bit32u;
  displacement32: Bit32s;
  op1_16, op2_16, index_16, temp_CF_16: Bit16u;
  displacement16: Bit16s;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);
    index_32 := op2_32  and $1f;
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then
      op1_32 := BX_READ_32BIT_REG(i^.rm)
    else
    begin
      //displacement32 := Bit32s(op2_32  and $ffffffe0)) / 32;
      displacement32 := Bit32s(op2_32 and $ffffffe0) div 32;
      op1_addr := i^.rm_addr + 4 * displacement32;
      read_RMW_virtual_dword(i^.seg, op1_addr, @op1_32);
    end;

    temp_CF := (op1_32 shr index_32)  and $01;
    //op1_32 @:= ~(((Bit32u) 1) shl index_32);  (* clear out bit *) !!!
    op1_32 := op1_32 and not (Bit32u(1) shl index_32);
    op1_32 := op1_32 or (Bit32u(temp_CF=0) shl index_32); (* set to complement *)
    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, op1_32)
      else write_RMW_virtual_dword(op1_32);
    set_CF(temp_CF);
  end else
  begin (* 16 bit operand size mod_e *)
    op2_16 := BX_READ_16BIT_REG(i^.nnn);
    index_16 := op2_16  and $0f;
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then
      op1_16 := BX_READ_16BIT_REG(i^.rm)
    else
    begin
      //displacement16 := ((Bit16s(op2_16  and $fff0)) / 16;
      displacement16 := (Bit16s(op2_16  and $fff0)) div 16;
      op1_addr := i^.rm_addr + 2 * displacement16;
      read_RMW_virtual_word(i^.seg, op1_addr, @op1_16);
    end;

    temp_CF_16 := (op1_16 shr index_16)  and $01;
    //op1_16 := op1_16 and ~(((Bit16u) 1) shl index_16);  (* clear out bit *) !!!
    op1_16 := op1_16 and not (Bit16u(1) shl index_16);
    op1_16 := op1_16 or (Bit16u(temp_CF_16=0) shl index_16); (* set to complement *)

    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, op1_16)
      else write_RMW_virtual_word(op1_16);
    set_CF(temp_CF);
  end;
end;

procedure TCPU.BT_EvIb(I: PInstruction_tag);
var
  op1_32: Bit32u;
  op2_8: Bit8u;
  op1_16: Bit16u;
  op2_8_8: Bit8u;
begin
  if (i^.os_32)<>0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    op2_8 := i^.Ib;
    //op2_8 %:= 32; !!!
    op2_8 := op2_8 mod 32;
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_32 := BX_READ_32BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

    set_CF((op1_32 shr op2_8)  and $01);
  end else
  begin (* 16 bit operand size mod_e *)
    op2_8_8 := i^.Ib;
    op2_8_8 := op2_8_8 mod 16;
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_16 := BX_READ_16BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);

    set_CF((op1_16 shr op2_8_8)  and $01);
  end;
end;

procedure TCPU.BTS_EvIb(I: PInstruction_tag);
var
  op1_32, temp_CF: Bit32u;
  op2_8: Bit8u;
  op1_16: Bit16u;
begin
  if (i^.os_32) <> 0 then (* 32 bit operand size mod_e *)
  begin
    (* for 32 bit operand size mod_e *)
    op2_8 := i^.Ib;
    //op2_8 %:= 32; !!!
    op2_8 := op2_8 mod 32;
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_32 := BX_READ_32BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

    temp_CF := (op1_32 shr op2_8) and $01;
    op1_32 := op1_32 or (Bit32u(1) shl op2_8);
    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, op1_32)
      else write_RMW_virtual_dword(op1_32);
    set_CF(temp_CF);
  end else
  begin (* 16 bit operand size mod_e *)
    op2_8 := i^.Ib;
    //op2_8 %:= 16; !!!
    op2_8 := op2_8 mod 16;
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_16 := BX_READ_16BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

    temp_CF := (op1_16 shr op2_8)  and $01;
    op1_16 := op1_16 or (Bit16u(1) shl op2_8);
    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, op1_16)
      else write_RMW_virtual_word(op1_16);
    set_CF(temp_CF);
  end;
end;

procedure TCPU.BTC_EvIb(I: PInstruction_tag);
var
  op1_32, temp_CF: Bit32u;
  op2_8: Bit8u;
  op1_16: Bit16u;
begin
  if (i^.os_32) <> 0 then
  begin (* 32 bit operand size mod_e *)
    (* for 32 bit operand size mod_e *)
    op2_8 := i^.Ib;
    //op2_8 %:= 32; !!!
    op2_8 := op2_8 mod 32;
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_32 := BX_READ_32BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

    temp_CF := (op1_32 shr op2_8)  and $01;
    //op1_32 := op1_32 and ~(((Bit32u) 1) shl op2_8);  (* clear out bit *) !!!
    op1_32 := op1_32 and not (Bit32u(1) shl op2_8);
    op1_32 := op1_32 or (Bit32u(not temp_CF) shl op2_8); (* set to complement *)
    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, op1_32)
      else write_RMW_virtual_dword(op1_32);
    set_CF(temp_CF);
  end else
  begin (* 16 bit operand size mod_e *)
    op2_8 := i^.Ib;
    //op2_8 %:= 16; !!!
    op2_8:=op2_8 mod 16;
    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0)
      then op1_16 := BX_READ_16BIT_REG(i^.rm)
          (* pointer, segment address pair *)
      else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

    temp_CF := (op1_16 shr op2_8)  and $01;
    //op1_16 := op1_16 and ~(((Bit16u) 1) shl op2_8);  (* clear out bit *) !!!
    op1_16 := op1_16 and not (Bit16u(1) shl op2_8);
    op1_16 := op1_16 or (Bit16u(not temp_CF) shl op2_8); (* set to complement *)
    (* now write diff back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, op1_16)
      else write_RMW_virtual_word(op1_16);
    set_CF(temp_CF);
  end;
end;

procedure TCPU.BTR_EvIb(I: PInstruction_tag);
var
  op1_32, temp_CF: Bit32u;
  op2_8: Bit8u;
  op1_16: Bit16u;
begin
  if (i^.os_32)<>0 then begin (* 32 bit operand size mod_e *)
    (* for 32 bit operand size mod_e *)

    op2_8 := i^.Ib;
    //op2_8 %:= 32; !!!
    op2_8 := op2_8 mod 32;

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    temp_CF := (op1_32 shr op2_8)  and $01;
    //op1_32 := op1_32 and ~(((Bit32u) 1) shl op2_8); !!!
    op1_32 := op1_32 and not (Bit32u(1) shl op2_8);

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, op1_32);
      end
  else begin
      write_RMW_virtual_dword(op1_32);
      end;
    set_CF(temp_CF);
    end
  else begin (* 16 bit operand size mod_e *)

    op2_8 := i^.Ib;
    op2_8 :=op2_8 mod 16; // % !!!

    (* op1_16 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    temp_CF := (op1_16 shr op2_8)  and $01;
    //op1_16 := op1_16 and ~(((Bit16u) 1) shl op2_8); !!!
    op1_16 := op1_16 and not (Bit16u(1) shl op2_8);

    (* now write diff back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, op1_16);
      end
  else begin
      write_RMW_virtual_word(op1_16);
      end;
    set_CF(temp_CF);
    end;
end;

procedure TCPU.load_seg_reg(seg:Psegment_reg_t; new_value:Bit16u);
var
  index:Bit16u;
  ti:Bit8u;
  rpl:Bit8u;
  descriptor:TDescriptor_t;
  dword1, dword2:Bit32u;
begin
  if Boolean(v8086_mode()) then
  begin
    { ??? don't need to set all these fields }
    seg^.selector.value := new_value;
    seg^.selector.rpl := 3;
    seg^.cache.valid := 1;
    seg^.cache.p := 1;
    seg^.cache.dpl := 3;
    seg^.cache.segmenttype := 1; { regular segment }

    if (seg = @sregs[SREG_CS]) then
      seg^.cache.segment.executable := 1 { code segment }
    else
      seg^.cache.segment.executable := 0; { data segment }

    seg^.cache.segment.c_ed := 0; { expand up }
    seg^.cache.segment.r_w := 1; { writeable }
    seg^.cache.segment.a := 1; { accessed }
    seg^.cache.segment.base := new_value shl 4;
    seg^.cache.segment.limit        := $ffff;
    seg^.cache.segment.limit_scaled := $ffff;
    seg^.cache.segment.g     := 0; { byte granular }
    seg^.cache.segment.d_b   := 0; { default 16bit size }
    seg^.cache.segment.avl   := 0;

    exit;
  end;

  if Boolean(Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))) then
  begin
    if (seg = @sregs[SREG_SS]) then
    begin

      if Boolean((new_value and $fffc) = 0) then
      begin { null selector }
        LogPanic(('load_seg_reg: SS: new_value := 0'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
      end;

      index := new_value shr 3;
      ti := (new_value shr 2) and $01;
      rpl := (new_value and $03);

      { examine AR byte of destination selector for legal values: }

      if (ti = 0) then
      begin { GDT }
        if ((index*8 + 7) >  gdtr.limit) then
        begin
          //BX_PANIC(('load_seg_reg: GDT: %s: index(%04x*8+7) > limit(%06x)',
             //strseg(seg), (unsigned) index, (unsigned)  gdtr.limit));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        access_linear( gdtr.base + index*8,     4, 0, BX_READ, @dword1);
        access_linear( gdtr.base + index*8 + 4, 4, 0, BX_READ, @dword2);
      end else
      begin { LDT }
        if ( ldtr.cache.valid = 0) then
        begin { ??? }
          LogError(('load_seg_reg: LDT invalid'));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        if ((index * 8 + 7) >  ldtr.cache.ldt.limit) then
        begin
          LogError(('load_seg_reg ss: LDT: index > limit'));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        access_linear( ldtr.cache.ldt.base + index*8,     4, 0, BX_READ, @dword1);
        access_linear( ldtr.cache.ldt.base + index*8 + 4, 4, 0, BX_READ, @dword2);
      end;

      { selector's RPL must := CPL, else #GP(selector) }
      if (rpl <> CPL) then
      begin
        LogError(('load_seg_reg(): rpl !:= CPL'));
        exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
        exit;
      end;

      parse_descriptor(dword1, dword2, @descriptor);

      if (descriptor.valid=0) then
      begin
        LogError(('load_seg_reg(): valid bit cleared'));
        exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
        exit;
      end;

      { AR byte must indicate a writable data segment else #GP(selector) }
      if Boolean((Word(descriptor.segmenttype=0) or
                 descriptor.segment.executable or
                 Bool(descriptor.segment.r_w=0 ))) then
      begin
        LogError(('load_seg_reg(): not writable data segment'));
        exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
      end;

      { DPL in the AR byte must equal CPL else #GP(selector) }
      if (descriptor.dpl <> CPL) then
      begin
        LogError(('load_seg_reg(): dpl !:= CPL'));
        exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
      end;

      { segment must be marked PRESENT else #SS(selector) }
      if (descriptor.p = 0) then
      begin
        LogError(('load_seg_reg(): not present'));
        exception2([BX_SS_EXCEPTION, new_value and $fffc, 0]);
      end;

      { load SS with selector, load SS cache with descriptor }
      sregs[SEG_REG_SS].selector.value        := new_value;
      sregs[SEG_REG_SS].selector.index        := index;
      sregs[SEG_REG_SS].selector.ti           := ti;
      sregs[SEG_REG_SS].selector.rpl          := rpl;
      sregs[SEG_REG_SS].cache := descriptor;
      sregs[SEG_REG_SS].cache.valid             := 1;

      { now set accessed bit in descriptor }
      dword2 := dword2 or $0100;
      if (ti = 0) then { GDT }
        access_linear( gdtr.base + index*8 + 4, 4, 0, BX_WRITE, @dword2)
      else { LDT }
        access_linear( ldtr.cache.ldt.base + index*8 + 4, 4, 0, BX_WRITE, @dword2);

      exit;
    end else
    if ( (seg=@sregs[SREG_DS]) or (seg=@sregs[SREG_ES])
           or (seg=@sregs[SREG_FS]) or (seg=@sregs[SREG_GS]) ) then
    begin
      if ((new_value and $fffc) = 0) then
      begin { null selector }
        seg^.selector.index := 0;
        seg^.selector.ti := 0;
        seg^.selector.rpl := 0;
        seg^.selector.value := 0;
        seg^.cache.valid := 0; { invalidate null selector }
        exit;
      end;

      index := new_value shr 3;
      ti := (new_value shr 2) and $01;
      rpl := (new_value and $03);

      { selector index must be within descriptor limits, else #GP(selector) }

      if (ti = 0) then
      begin { GDT }
        if ((index*8 + 7) >  gdtr.limit) then
        begin
          //BX_ERROR(('load_seg_reg: GDT: %s: index(%04x) > limit(%06x)',
          //   strseg(seg), (unsigned) index, (unsigned)  gdtr.limit));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        access_linear( gdtr.base + index*8,     4, 0,  BX_READ, @dword1);
        access_linear( gdtr.base + index*8 + 4, 4, 0,  BX_READ, @dword2);
      end else
      begin { LDT }
        if ( ldtr.cache.valid=0) then
        begin
          LogError(('load_seg_reg: LDT invalid'));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        if ((index*8 + 7) >  ldtr.cache.ldt.limit) then
        begin
          LogError(('load_seg_reg ds,es: LDT: index > limit'));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
        access_linear( ldtr.cache.ldt.base + index*8,     4, 0, BX_READ, @dword1);
        access_linear( ldtr.cache.ldt.base + index*8 + 4, 4, 0, BX_READ, @dword2);
      end;

      parse_descriptor(dword1, dword2, @descriptor);

      if (descriptor.valid = 0) then
      begin
        LogError(('load_seg_reg(): valid bit cleared'));
        exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
        exit;
      end;

      { AR byte must indicate data or readable code segment else #GP(selector) } //!!!
(*      if ( descriptor.segment=0 or ((descriptor.segment.executable=1) and (descriptor.segment.r_w=0)) then begin
        BX_ERROR(('load_seg_reg(): not data or readable code'));
        exception(BX_GP_EXCEPTION, new_value @ $fffc, 0);
        exit;
        end;*)

      { If data or non-conforming code, then both the RPL and the CPL
       * must be less than or equal to DPL in AR byte else #GP(selector) }
      if (descriptor.segment.executable=0) or (descriptor.segment.c_ed=0) then
      begin
        if ((rpl > descriptor.dpl) or (CPL > descriptor.dpl)) then
        begin
          LogError(('load_seg_reg: RPL @ CPL must be <:= DPL'));
          exception2([BX_GP_EXCEPTION, new_value and $fffc, 0]);
          exit;
        end;
      end;

      { segment must be marked PRESENT else #NP(selector) }
      if (descriptor.p = 0) then
      begin
        LogError(('load_seg_reg: segment not present'));
        exception2([BX_NP_EXCEPTION, new_value and $fffc, 0]);
        exit;
      end;

      { load segment register with selector }
      { load segment register-cache with descriptor }
      seg^.selector.value        := new_value;
      seg^.selector.index        := index;
      seg^.selector.ti           := ti;
      seg^.selector.rpl          := rpl;
      seg^.cache := descriptor;
      seg^.cache.valid             := 1;

      if Boolean((dword2 and $0100)=0) then
      begin
        dword2 := dword2 or $0100;
        if (ti = 0) then { GDT }
          access_linear( gdtr.base + index*8 + 4, 4, 0, BX_WRITE, @dword2)
        else { LDT }
         access_linear( ldtr.cache.ldt.base + index*8 + 4, 4, 0, BX_WRITE, @dword2);
      end;
      exit;
    end else
    begin
      LogPanic(('load_seg_reg(): invalid segment register passed!'));
      exit;
    end;
  end;

  { real mode }
  { seg^.limit := ; ??? different behaviours depening on seg reg. }
  { something about honoring previous values }

  { ??? }
  if (seg = @sregs[SREG_CS]) then
  begin
    sregs[SEG_REG_CS].selector.value := new_value;
    sregs[SEG_REG_CS].cache.valid := 1;
    sregs[SEG_REG_CS].cache.p := 1;
    sregs[SEG_REG_CS].cache.dpl := 0;
    sregs[SEG_REG_CS].cache.segmenttype := 1; { regular segment }
    sregs[SEG_REG_CS].cache.segment.executable := 1; { code segment }
    sregs[SEG_REG_CS].cache.segment.c_ed := 0; { expand up }
    sregs[SEG_REG_CS].cache.segment.r_w := 1; { writeable }
    sregs[SEG_REG_CS].cache.segment.a := 1; { accessed }
    sregs[SEG_REG_CS].cache.segment.base := new_value shl 4;
    sregs[SEG_REG_CS].cache.segment.limit        := $ffff;
    sregs[SEG_REG_CS].cache.segment.limit_scaled := $ffff;
    sregs[SEG_REG_CS].cache.segment.g     := 0; { byte granular }
    sregs[SEG_REG_CS].cache.segment.d_b   := 0; { default 16bit size }
    sregs[SEG_REG_CS].cache.segment.avl   := 0;
  end else
  begin { SS, DS, ES, FS, GS }
    seg^.selector.value := new_value;
    seg^.cache.valid := 1;
    seg^.cache.p := 1; // set this???
    seg^.cache.segment.base := new_value shl 4;
    seg^.cache.segmenttype := 1; { regular segment }
    seg^.cache.segment.a := 1; { accessed }
    { set G, D_B, AVL bits here ??? }
  end;
end;

procedure TCPU.parse_selector(raw_selector:Bit16u; selector:Pselector_t);
begin
  selector^.value  := raw_selector;
  selector^.index  := raw_selector shr 3;
  selector^.ti     := (raw_selector shr 2) and $01;
  selector^.rpl    := raw_selector and $03;
end;

procedure TCPU.parse_descriptor(dword1:Bit32u;dword2:Bit32u;temp:Pdescriptor_t);
var
  AR_byte:Bit8u;
begin

  AR_byte        := dword2 shr 8;
  temp^.p        := (AR_byte shr 7) and $01;
  temp^.dpl      := (AR_byte shr 5) and $03;
  temp^.segmenttype  := (AR_byte shr 4) and $01;
  temp^.type_     := (AR_byte and $0f);
  temp^.valid    := 0; { start out invalid }


  if Boolean(temp^.segmenttype) then begin { data/code segment descriptors }
    temp^.segment.executable := (AR_byte shr 3) and $01;
    temp^.segment.c_ed       := (AR_byte shr 2) and $01;
    temp^.segment.r_w        := (AR_byte shr 1) and $01;
    temp^.segment.a          := (AR_byte shr 0) and $01;

    temp^.segment.limit      := (dword1 and $ffff);
    temp^.segment.base       := (dword1 shr 16) or ((dword2 and $FF) shl 16);

    temp^.segment.limit        := temp^.segment.limit or (dword2 and $000F0000);
    temp^.segment.g            :=  Word((dword2 and $00800000) > 0);
    temp^.segment.d_b          :=  Word((dword2 and $00400000) > 0);
    temp^.segment.avl          :=  Word((dword2 and $00100000) > 0);
    temp^.segment.base         :=temp^.segment.base or (dword2 and $FF000000);
    if Boolean(temp^.segment.g) then begin
      if ( (temp^.segment.executable=0) and (temp^.segment.c_ed<>0) ) then
        temp^.segment.limit_scaled := (temp^.segment.limit shl 12)
      else
        temp^.segment.limit_scaled := (temp^.segment.limit shl 12) or $0fff;
      end
    else
      temp^.segment.limit_scaled := temp^.segment.limit;

    temp^.valid    := 1;
    end
  else begin // system @ gate segment descriptors
    case temp^.type_ of
      0, // reserved
      8, // reserved
      10, // reserved
      13: // reserved
        begin
          temp^.valid    := 0;
        end;
      1, // 286 TSS (available)
      3: // 286 TSS (busy)
        begin
          temp^.tss286.base  := (dword1 shr 16) or ((dword2 and $ff) shl 16);
          temp^.tss286.limit := (dword1 and $ffff);
          temp^.valid    := 1;
        end;
      2: // LDT descriptor
        begin
          temp^.ldt.base := (dword1 shr 16) or ((dword2 and $FF) shl 16);
          temp^.ldt.base := temp^.ldt.base or (dword2 and $ff000000);
        temp^.ldt.limit := (dword1 and $ffff);
        temp^.valid    := 1;
        end;
      4, // 286 call gate
      6, // 286 interrupt gate
      7: // 286 trap gate
        { word count only used for call gate }
        begin
          temp^.gate286.word_count := dword2 and $1f;
          temp^.gate286.dest_selector := dword1 shr 16;
          temp^.gate286.dest_offset   := dword1 and $ffff;
          temp^.valid := 1;
        end;
      5: // 286/386 task gate
        begin
          temp^.taskgate.tss_selector := dword1 shr 16;
          temp^.valid := 1;
        end;

      9,  // 386 TSS (available)
      11: // 386 TSS (busy)
        begin
          temp^.tss386.base  := (dword1 shr 16) or ((dword2 and $ff) shl 16) or (dword2 and $ff000000);
          temp^.tss386.limit := (dword1 and $0000ffff) or (dword2 and $000f0000);
          temp^.tss386.g     := Bool((dword2 and $00800000) > 0);
          temp^.tss386.avl   := Bool((dword2 and $00100000) > 0);
          if Boolean(temp^.tss386.g) then
            temp^.tss386.limit_scaled := (temp^.tss386.limit shl 12) or $0fff
          else
            temp^.tss386.limit_scaled := temp^.tss386.limit;
          temp^.valid := 1;
        end;

      12, // 386 call gate
      14, // 386 interrupt gate
      15: // 386 trap gate
        begin
        // word count only used for call gate
          temp^.gate386.dword_count   := dword2 and $1f;
          temp^.gate386.dest_selector := dword1 shr 16;;
          temp^.gate386.dest_offset   := (dword2 and $ffff0000) or (dword1 and $0000ffff);
          temp^.valid := 1;
        end;
      else
        begin
          temp^.valid    := 0;
        end;
      end;
    end;
end;

procedure TCPU.load_ldtr(selector:Pselector_t; descriptor:Pdescriptor_t);
begin
  { check for null selector, if so invalidate LDTR }
  if ( (selector^.value and $fffc) =0 ) then begin
     ldtr.selector := selector^;
     ldtr.cache.valid := 0;
    exit;
    end;

  if (@descriptor=nil) then
    LogPanic(('load_ldtr(): descriptor := NULL!'));

   ldtr.cache := descriptor^; { whole structure copy }
   ldtr.selector := selector^;

  if ( ldtr.cache.ldt.limit < 7) then begin
    LogPanic(('load_ldtr(): ldtr.limit < 7'));
    end;

   ldtr.cache.valid := 1;
end;

procedure TCPU.load_cs(selector:Pselector_t; descriptor:Pdescriptor_t; cpl:Bit8u);
begin
   sregs[SEG_REG_CS].selector     := selector^;
   sregs[SEG_REG_CS].cache        := descriptor^;

  { caller may request different CPL then in selector }
   sregs[SEG_REG_CS].selector.rpl := cpl;
   sregs[SEG_REG_CS].cache.valid := 1; { ??? }
  // (BW) Added cpl to the selector value.
   sregs[SEG_REG_CS].selector.value := ($fffc and sregs[SEG_REG_CS].selector.value) or cpl;
end;

procedure TCPU.load_ss(selector:Pselector_t; descriptor:Pdescriptor_t; cpl:Bit8u);
begin
   sregs[SEG_REG_SS].selector := selector^;
   sregs[SEG_REG_SS].cache := descriptor^;
   sregs[SEG_REG_SS].selector.rpl := cpl;

  if ( ( sregs[SEG_REG_SS].selector.value and $fffc) = 0 ) then
    LogPanic(('load_ss(): null selector passed'));

  if Boolean( sregs[SEG_REG_SS].cache.valid =0) then begin
    LogPanic(('load_ss(): invalid selector/descriptor passed.'));
    end;
end;

procedure TCPU.fetch_raw_descriptor(selector:Pselector_t;
                               dword1:pBit32u; dword2:pBit32u; exception_no:Bit8u);
begin
  if (selector^.ti = 0) then { GDT }
  begin
    if ((selector^.index * 8 + 7) >  gdtr.limit) then
    begin
      exception2([exception_no, selector^.value and $fffc, 0]);
      exit;
    end;
    access_linear( gdtr.base + selector^.index * 8, 4, 0,  BX_READ, dword1);
    access_linear( gdtr.base + selector^.index * 8 + 4, 4, 0, BX_READ, dword2);
  end else
  begin { LDT }
    if ((selector^.index*8 + 7) >  ldtr.cache.ldt.limit) then
    begin
      exception(exception_no, selector^.value and $fffc, 0);
      exit;
    end;
    access_linear( ldtr.cache.ldt.base + selector^.index * 8, 4, 0, BX_READ, dword1);
    access_linear( ldtr.cache.ldt.base + selector^.index * 8 + 4, 4, 0, BX_READ, dword2);
  end;
end;

function TCPU.fetch_raw_descriptor2(selector:Pselector_t; dword1:pBit32u; dword2:pBit32u):Bool;
begin
  if (selector^.ti = 0) then { GDT }
  begin
    if ((selector^.index * 8 + 7) >  gdtr.limit) then
      Exit(0);
    access_linear( gdtr.base + selector^.index * 8, 4, 0, BX_READ, dword1);
    access_linear( gdtr.base + selector^.index * 8 + 4, 4, 0, BX_READ, dword2);
    Exit(1);
  end else
  begin { LDT }
    if ((selector^.index*8 + 7) >  ldtr.cache.ldt.limit) then
      Exit(0);
    access_linear( ldtr.cache.ldt.base + selector^.index*8,     4, 0, BX_READ, dword1);
    access_linear( ldtr.cache.ldt.base + selector^.index*8 + 4, 4, 0, BX_READ, dword2);
    Exit(1);
  end;
end;

// Notes:
// ===
// Step 2: TSS descriptor is not busy TS (for IRET); GP (for JMP, CALL, INT)
//   returns error code (Task's backlink TSS)???

// *   TSS selector must map to GDT
// *   TSS is stored in linear address space
// * what to do with I/O Map Base
// * what to do with T flag
// * where to set CR3 and flush paging cache
// * what happens when fault occurs, with some seg regs having valid bit cleared?
// * should check validity of current TR(TSS) before writing into it
//

  // ===========
  // 286 Task State Segment
  // ===========
  // dynamic item                     orhex  dec  offset
  // 0       task LDT selector        or2a   42
  // 1       DS selector              or28   40
  // 1       SS selector              or26   38
  // 1       CS selector              or24   36
  // 1       ES selector              or22   34
  // 1       DI                       or20   32
  // 1       SI                       or1e   30
  // 1       BP                       or1c   28
  // 1       SP                       or1a   26
  // 1       BX                       or18   24
  // 1       DX                       or16   22
  // 1       CX                       or14   20
  // 1       AX                       or12   18
  // 1       flag word                or10   16
  // 1       IP (entry point)         or0e   14
  // 0       SS for CPL 2             or0c   12
  // 0       SP for CPL 2             or0a   10
  // 0       SS for CPL 1             or08   08
  // 0       SP for CPL 1             or06   06
  // 0       SS for CPL 0             or04   04
  // 0       SP for CPL 0             or02   02
  //         back link selector to TSSor00   00


  // ===========
  // 386 Task State Segment
  // ===========
  // |31            16|15                    0|
  // |I/O Map Base    |000000000000000000000|T| 64  static
  // |0000000000000000| LDT                  or60  static
  // |0000000000000000| GS selector          or5c  dynamic
  // |0000000000000000| FS selector          or58  dynamic
  // |0000000000000000| DS selector          or54  dynamic
  // |0000000000000000| SS selector          or50  dynamic
  // |0000000000000000| CS selector          or4c  dynamic
  // |0000000000000000| ES selector          or48  dynamic
  //or               EDI                    or44  dynamic
  //or               ESI                    or40  dynamic
  //or               EBP                    or3c  dynamic
  //or               ESP                    or38  dynamic
  //or               EBX                    or34  dynamic
  //or               EDX                    or30  dynamic
  //or               ECX                    or2c  dynamic
  //or               EAX                    or28  dynamic
  //or               EFLAGS                 or24  dynamic
  //or               EIP (entry point)      or20  dynamic
  //or          CR3 (PDPR)                  or1c  static
  // |000000000000000orSS for CPL 2         or18  static
  //or          ESP for CPL 2               or14  static
  // |000000000000000orSS for CPL 1         or10  static
  //or          ESP for CPL 1               or0c  static
  // |000000000000000orSS for CPL 0         or08  static
  //or          ESP for CPL 0               or04  static
  // |000000000000000orback link to prev TSSor00  dynamic (updated only when return expected)


  // =========================
  // Effect of task switch on Busy, NT, and Link Fields
  // =========================

  // Field         jump        call/interrupt     iret
  // ------------------------------------------------------
  // new busy bit  Set         Set                No change
  // old busy bit  Cleared     No change          Cleared
  // new NT flag   No change   Set                No change
  // old NT flag   No change   No change          Cleared
  // new link      No change   old TSS selector   No change
  // old link      No change   No change          No change
  // CR0.TS        Set         Set                Set

  // Note: I checked 386, 486, and Pentium, and they all exhibited
  //       exactly the same behaviour as above.  There seems to
  //       be some misprints in the Intel docs.


procedure TCPU.task_switch(tss_selector: Pselector_t; tss_descriptor: Pdescriptor_t; source: unsigned;
                     dword1: Bit32u; dword2: Bit32u);
var
  obase32: Bit32u; // base address of old TSS
  nbase32: Bit32u; // base address of new TSS
  temp32, newCR3: Bit32u;
  raw_cs_selector, raw_ss_selector, raw_ds_selector, raw_es_selector,
         raw_fs_selector, raw_gs_selector, raw_ldt_selector: Bit16u;
  temp16, trap_word: Bit16u;
  cs_selector, ss_selector, ds_selector, es_selector, gs_selector,
  ldt_selector,fs_selector: TSelector_t;
  cs_descriptor, ss_descriptor, ds_descriptor, es_descriptor, gs_descriptor,
  ldt_descriptor,fs_descriptor: TDescriptor_t;
  old_TSS_max, new_TSS_max, new_TSS_limit: Bit32u;
  newEAX, newECX, newEDX, newEBX: Bit32u;
  newESP, newEBP, newESI, newEDI: Bit32u;
  newEFLAGS, oldEFLAGS, newEIP: Bit32u;
  exception_no: unsigned;
  error_code: Bit16u;
  good: Bool;
  label post_exception;
begin
  invalidate_prefetch_q();
  // Discard any traps and inhibits for new context; traps will
  // resume upon return.
  Self.debug_trap := 0;
  Self.inhibit_mask := 0;
  // The following checks are made before calling task_switch(), for
  // JMP  and CALL only.  These checks are NOT made for exceptions, interrupts,  and IRET
  //
  //   1) TSS DPL must be >= CPL
  //   2) TSS DPL must be >= TSS selector RPL
  //   3) TSS descriptor is not busy.  TS(for IRET); GP(for JMP, CALL, INT)

  // Privilege and busy checks done in CALL, JUMP, INT, IRET

  exception_no := 256; // no exception
  error_code   := 0;
  oldEFLAGS    := read_eflags();
  // Gather info about old TSS
  if (Self.tr.cache.type_ <= 3) then
  begin
    // sanity check type: cannot have busy bit
    assert((Self.tr.cache.type_ and 2) = 0);
    obase32 := Self.tr.cache.tss286.base;
    old_TSS_max   := 43;
  end else
  begin
    obase32 := Self.tr.cache.tss386.base;
    old_TSS_max   := 103;
  end;
  // Gather info about new TSS
  if (tss_descriptor^.type_ <= 3) then // begin1,3end;
  begin
    nbase32 := tss_descriptor^.tss286.base; // new TSS.base
    new_TSS_max   := 43;
    new_TSS_limit := tss_descriptor^.tss286.limit;
  end else
  begin // tss_descriptor^.type := begin9,11end;
    nbase32 := tss_descriptor^.tss386.base; // new TSS.base
    new_TSS_max   := 103;
    new_TSS_limit := tss_descriptor^.tss386.limit_scaled;
  end;
  // Task State Seg must be present, else #NP(TSS selector)
  if (tss_descriptor^.p = 0) then
    exception2([BX_NP_EXCEPTION, tss_selector^.value  and $fffc, 0]);
  // TSS must have valid limit, else #TS(TSS selector)
  if ((tss_selector^.ti <> 0) or (tss_descriptor^.valid = 0) or (new_TSS_limit < new_TSS_max)) then
    exception2([BX_TS_EXCEPTION, tss_selector^.value  and $fffc, 0]);
  // Check that old TSS, new TSS, and all segment descriptors
  // used in the task switch are paged in.
  if (Self.FCR0.pg) <> 0 then
  begin
    //BX_RW, BX_READ, BX_WRITE
    // Old TSS
    dtranslate_linear(obase32, 0, (*rw*) BX_WRITE);
    dtranslate_linear(obase32+old_TSS_max, 0, (*rw*) BX_WRITE);
    // New TSS
    dtranslate_linear(nbase32, 0, (*rw*) 0);
    dtranslate_linear(nbase32+new_TSS_max, 0, (*rw*) 0);
    // ??? fix RW above
    // ??? touch old/new TSS descriptors here when necessary.
  end;
  // Need to fetch all new registers and temporarily store them.
  if (tss_descriptor^.type_ <= 3) then
  begin
    access_linear(nbase32 + 14, 2, 0, BX_READ, @temp16);
      newEIP := temp16; // zero out upper word
    access_linear(nbase32 + 16, 2, 0, BX_READ, @temp16);
      newEFLAGS := temp16;
    // incoming TSS is 16bit:
    //   - upper word of general registers is set to $FFFF
    //   - upper word of eflags is zero'd
    //   - FS, GS are zero'd
    //   - upper word of eIP is zero'd
    access_linear(nbase32 + 18, 2, 0, BX_READ, @temp16);
      newEAX := $ffff0000 or temp16;
    access_linear(nbase32 + 20, 2, 0, BX_READ, @temp16);
      newECX := $ffff0000 or temp16;
    access_linear(nbase32 + 22, 2, 0, BX_READ, @temp16);
      newEDX := $ffff0000 or temp16;
    access_linear(nbase32 + 24, 2, 0, BX_READ, @temp16);
      newEBX := $ffff0000 or temp16;
    access_linear(nbase32 + 26, 2, 0, BX_READ, @temp16);
      newESP := $ffff0000 or temp16;
    access_linear(nbase32 + 28, 2, 0, BX_READ, @temp16);
      newEBP := $ffff0000 or temp16;
    access_linear(nbase32 + 30, 2, 0, BX_READ, @temp16);
      newESI := $ffff0000 or temp16;
    access_linear(nbase32 + 32, 2, 0, BX_READ, @temp16);
      newEDI := $ffff0000 or temp16;

    access_linear(nbase32 + 34, 2, 0, BX_READ, @raw_es_selector);
    access_linear(nbase32 + 36, 2, 0, BX_READ, @raw_cs_selector);
    access_linear(nbase32 + 38, 2, 0, BX_READ, @raw_ss_selector);
    access_linear(nbase32 + 40, 2, 0, BX_READ, @raw_ds_selector);
    access_linear(nbase32 + 42, 2, 0, BX_READ, @raw_ldt_selector);

    raw_fs_selector := 0; // use a NULL selector
    raw_gs_selector := 0; // use a NULL selector
    // No CR3 change for 286 task switch
    newCR3 := 0;   // keep compiler happy (not used)
    trap_word := 0; // keep compiler happy (not used)
  end else
  begin
    access_linear(nbase32 + $1c, 4, 0, BX_READ, @newCR3);
    access_linear(nbase32 + $20, 4, 0, BX_READ, @newEIP);
    access_linear(nbase32 + $24, 4, 0, BX_READ, @newEFLAGS);
    access_linear(nbase32 + $28, 4, 0, BX_READ, @newEAX);
    access_linear(nbase32 + $2c, 4, 0, BX_READ, @newECX);
    access_linear(nbase32 + $30, 4, 0, BX_READ, @newEDX);
    access_linear(nbase32 + $34, 4, 0, BX_READ, @newEBX);
    access_linear(nbase32 + $38, 4, 0, BX_READ, @newESP);
    access_linear(nbase32 + $3c, 4, 0, BX_READ, @newEBP);
    access_linear(nbase32 + $40, 4, 0, BX_READ, @newESI);
    access_linear(nbase32 + $44, 4, 0, BX_READ, @newEDI);
    access_linear(nbase32 + $48, 2, 0, BX_READ, @raw_es_selector);
    access_linear(nbase32 + $4c, 2, 0, BX_READ, @raw_cs_selector);
    access_linear(nbase32 + $50, 2, 0, BX_READ, @raw_ss_selector);
    access_linear(nbase32 + $54, 2, 0, BX_READ, @raw_ds_selector);
    access_linear(nbase32 + $58, 2, 0, BX_READ, @raw_fs_selector);
    access_linear(nbase32 + $5c, 2, 0, BX_READ, @raw_gs_selector);
    access_linear(nbase32 + $60, 2, 0, BX_READ, @raw_ldt_selector);
    access_linear(nbase32 + $64, 2, 0, BX_READ, @trap_word);
    // I/O Map Base Address ???
  end;
  //
  // Step 6: If JMP or IRET, clear busy bit in old task TSS descriptor,
  //         otherwise leave set.
  //
  // effect on Busy bit of old task
  if ( (source=BX_TASK_FROM_JUMP) or (source=BX_TASK_FROM_IRET) ) then
  begin
    // Bit is cleared
    access_linear(Self.gdtr.base + Self.tr.selector.index*8 + 4,
                  4, 0, BX_READ, @temp32);
    //temp32 @:= ~$00000200;  !!! ~
    temp32 := temp32 and not $00000200;
    access_linear(Self.gdtr.base + Self.tr.selector.index*8 + 4,
                  4, 0, BX_WRITE, @temp32);
  end;
  //
  // Step 7: If IRET, clear NT flag in temp image of EFLAGS, otherwise
  //         leave alone.
  //
  if (source = BX_TASK_FROM_IRET) then
    oldEFLAGS := oldEFLAGS and not $00004000;
    // NT flags in old task is cleared with an IRET
    //oldEFLAGS := oldEFLAGS @:= ~$00004000; !!! ~
  //
  // Step 8: Save dynamic state of old task.
  //
  if (Self.tr.cache.type_ <= 3) then
  begin
    // sanity check: tr.cache.type cannot have busy bit
    assert ((Self.tr.cache.type_  and 2) = 0);
    temp16 := EIP; access_linear(obase32 + 14, 2, 0, BX_WRITE, @temp16);
    temp16 := oldEFLAGS; access_linear(obase32 + 16, 2, 0, BX_WRITE, @temp16);
    temp16 := AX; access_linear(obase32 + 18, 2, 0, BX_WRITE, @temp16);
    temp16 := CX; access_linear(obase32 + 20, 2, 0, BX_WRITE, @temp16);
    temp16 := DX; access_linear(obase32 + 22, 2, 0, BX_WRITE, @temp16);
    temp16 := BX; access_linear(obase32 + 24, 2, 0, BX_WRITE, @temp16);
    temp16 := SP; access_linear(obase32 + 26, 2, 0, BX_WRITE, @temp16);
    temp16 := BP; access_linear(obase32 + 28, 2, 0, BX_WRITE, @temp16);
    temp16 := SI; access_linear(obase32 + 30, 2, 0, BX_WRITE, @temp16);
    temp16 := DI; access_linear(obase32 + 32, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_ES].selector.value;
                 access_linear(obase32 + 34, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_CS].selector.value;
                 access_linear(obase32 + 36, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_SS].selector.value;
                 access_linear(obase32 + 38, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_DS].selector.value;
                 access_linear(obase32 + 40, 2, 0, BX_WRITE, @temp16);
  end else
  begin
    temp32 := FEIP; access_linear(obase32 + $20, 4, 0, BX_WRITE, @temp32);
    temp32 := oldEFLAGS; access_linear(obase32 + $24, 4, 0, BX_WRITE, @temp32);
    temp32 := EAX; access_linear(obase32 + $28, 4, 0, BX_WRITE, @temp32);
    temp32 := ECX; access_linear(obase32 + $2c, 4, 0, BX_WRITE, @temp32);
    temp32 := EDX; access_linear(obase32 + $30, 4, 0, BX_WRITE, @temp32);
    temp32 := EBX; access_linear(obase32 + $34, 4, 0, BX_WRITE, @temp32);
    temp32 := ESP; access_linear(obase32 + $38, 4, 0, BX_WRITE, @temp32);
    temp32 := EBP; access_linear(obase32 + $3c, 4, 0, BX_WRITE, @temp32);
    temp32 := ESI; access_linear(obase32 + $40, 4, 0, BX_WRITE, @temp32);
    temp32 := EDI; access_linear(obase32 + $44, 4, 0, BX_WRITE, @temp32);
    temp16 := Self.sregs[SEG_REG_ES].selector.value;
                  access_linear(obase32 + $48, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_CS].selector.value;
                  access_linear(obase32 + $4c, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_SS].selector.value;
                  access_linear(obase32 + $50, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_DS].selector.value;
                  access_linear(obase32 + $54, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_FS].selector.value;
                  access_linear(obase32 + $58, 2, 0, BX_WRITE, @temp16);
    temp16 := Self.sregs[SEG_REG_GS].selector.value;
                  access_linear(obase32 + $5c, 2, 0, BX_WRITE, @temp16);
  end;
  // Commit point.  At this point, we commit to the new
  // context.  If an unrecoverable error occurs in further
  // processing, we complete the task switch without performing
  // additional access and segment availablility checks and
  // generate the appropriate exception prior to beginning
  // execution of the new task.
  // Task switch clears LE/L3/L2/L1/L0 in DR7
  //Self.dr7 @:= ~$00000155; !!! ~
  Self.FDR7 := Self.FDR7 and not $00000155;
  // effect on link field of new task
  if ( source = BX_TASK_FROM_CALL_OR_INT ) then
  begin
    // set to selector of old task's TSS
    temp16 := Self.tr.selector.value;
    access_linear(nbase32 + 0, 2, 0, BX_WRITE, @temp16);
  end;
  // Step 9: If call or interrupt, set the NT flag in the eflags
  //         image stored in new task's TSS.  If IRET or JMP,
  //         NT is restored from new TSS eflags image. (no change)
  // effect on NT flag of new task
  if ( source = BX_TASK_FROM_CALL_OR_INT ) then
    newEFLAGS := newEFLAGS or $4000; // flag is set
  // Step 10: If CALL, interrupt, or JMP, set busy flag in new task's
  //          TSS descriptor.  If IRET, leave set.
  if ( (source = BX_TASK_FROM_JUMP) or (source = BX_TASK_FROM_CALL_OR_INT) ) then
  begin
    // set the new task's busy bit
    access_linear(Self.gdtr.base + tss_selector^.index*8 + 4,
                  4, 0, BX_READ, @dword2);
    dword2 := dword2 or $00000200;
    access_linear(Self.gdtr.base + tss_selector^.index*8 + 4,
                  4, 0, BX_WRITE, @dword2);
  end;
  // Step 11: Set TS flag in the CR0 image stored in the new task TSS.
  // set TS bit in CR0 register
  Self.FCR0.ts := 1;
  Self.FCR0.val32 := Self.FCR0.val32 or $00000008;
  // Step 12: Load the task register with the segment selector and
  //          descriptor for the new task TSS.
  Self.tr.selector := tss_selector^;
  Self.tr.cache    := tss_descriptor^;
  // Reset the busy-flag, because all functions expect non-busy types in
  // tr.cache.  From Peter Lammich <peterl@sourceforge.net>.
  //Self.tr.cache.type_ @:= ~2; !!! ~
  Self.tr.cache.type_ := Self.tr.cache.type_ and not 2;
  // Step 13: Load the new task (dynamic) state from new TSS.
  //          Any errors associated with loading and qualification of
  //          segment descriptors in this step occur in the new task's
  //          context.  State loaded here includes LDTR, CR3,
  //          EFLAGS, EIP, general purpose registers, and segment
  //          descriptor parts of the segment registers.
  if (tss_descriptor^.type_ >= 9) then
    CR3_change(newCR3); // Tell paging unit about new cr3 value

  FEIP:=newEIP;
  Self.prev_eip := FEIP;
  write_eflags(newEFLAGS, 1,1,1,1);
  EAX := newEAX;
  ECX := newECX;
  EDX := newEDX;
  EBX := newEBX;
  ESP := newESP;
  EBP := newEBP;
  ESI := newESI;
  EDI := newEDI;
  // Fill in selectors for all segment registers.  If errors
  // occur later, the selectors will at least be loaded.
  parse_selector(raw_es_selector, @es_selector);
  Self.sregs[SEG_REG_ES].selector := es_selector;
  parse_selector(raw_cs_selector, @cs_selector);
  Self.sregs[SEG_REG_CS].selector := cs_selector;
  parse_selector(raw_ss_selector, @ss_selector);
  Self.sregs[SEG_REG_SS].selector := ss_selector;
  parse_selector(raw_ds_selector, @ds_selector);
  Self.sregs[SEG_REG_DS].selector := ds_selector;
  parse_selector(raw_fs_selector, @fs_selector);
  Self.sregs[SEG_REG_FS].selector := fs_selector;
  parse_selector(raw_gs_selector, @gs_selector);
  Self.sregs[SEG_REG_GS].selector := gs_selector;
  parse_selector(raw_ldt_selector, @ldt_selector);
  Self.ldtr.selector                 := ldt_selector;
  // Start out with invalid descriptor caches, fill in
  // with values only as they are validated.
  Self.ldtr.cache.valid := 0;
  Self.sregs[SEG_REG_ES].cache.valid := 0;
  Self.sregs[SEG_REG_CS].cache.valid := 0;
  Self.sregs[SEG_REG_SS].cache.valid := 0;
  Self.sregs[SEG_REG_DS].cache.valid := 0;
  Self.sregs[SEG_REG_FS].cache.valid := 0;
  Self.sregs[SEG_REG_GS].cache.valid := 0;
  // need to test valid bit in fetch_raw_descriptor?()
  // or set limit to 0 instead when LDT is loaded with
  // null. ??? +++
  Self.ldtr.cache.ldt.limit := 0;
  // LDTR
  if (ldt_selector.ti) <> 0 then
  begin
    // LDT selector must be in GDT
    exception_no := BX_TS_EXCEPTION;
    error_code   := raw_ldt_selector  and $fffc;
    goto post_exception;
  end;
  // ??? is LDT loaded in v8086 mod_e
  if ( (raw_ldt_selector  and $fffc) <> 0 ) then
  begin
    good := fetch_raw_descriptor2(@ldt_selector, @dword1, @dword2);
    if (good = 0) then
    begin
      exception_no := BX_TS_EXCEPTION;
      error_code   := raw_ldt_selector  and $fffc;
      goto post_exception;
    end;

    parse_descriptor(dword1, dword2, @ldt_descriptor);
    // LDT selector of new task is valid, else #TS(new task's LDT)
    if ((ldt_descriptor.valid = 0) or
       (ldt_descriptor.type_ <> 2) or
       (ldt_descriptor.segmentType <> 0) or
       (ldt_descriptor.ldt.limit < 7)) then
    begin
      exception_no := BX_TS_EXCEPTION;
      error_code   := raw_ldt_selector  and $fffc;
      goto post_exception;
    end
    else
    // LDT of new task is present in memory, else #TS(new tasks's LDT)
    if (ldt_descriptor.p = 0) then
    begin
      exception_no := BX_TS_EXCEPTION;
      error_code   := raw_ldt_selector  and $fffc;
      goto post_exception;
    end;
    // All checks pass, fill in LDTR shadow cache
    Self.ldtr.cache := ldt_descriptor;
  end;

  if (v8086_mode()) <> 0 then
  begin
    // load seg regs as 8086 registers
    load_seg_reg(@Self.sregs[SEG_REG_CS], raw_cs_selector);
    load_seg_reg(@Self.sregs[SEG_REG_SS], raw_ss_selector);
    load_seg_reg(@Self.sregs[SEG_REG_DS], raw_ds_selector);
    load_seg_reg(@Self.sregs[SEG_REG_ES], raw_es_selector);
    load_seg_reg(@Self.sregs[SEG_REG_FS], raw_fs_selector);
    load_seg_reg(@Self.sregs[SEG_REG_GS], raw_gs_selector);
  end else
  begin
  // CS
    if ( (raw_cs_selector  and $fffc) <> 0 ) then
    begin
      good := fetch_raw_descriptor2(@cs_selector, @dword1, @dword2);
      if (good=0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_cs_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @cs_descriptor);
      // CS descriptor AR byte must indicate code segment else #TS(CS)
      if ((cs_descriptor.valid = 0) or
          (cs_descriptor.segmentType = 0) or
          (cs_descriptor.segment.executable = 0)) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_cs_selector  and $fffc;
        goto post_exception;
      end
      else
      // if non-conforming then DPL must equal selector RPL else #TS(CS)
      if ((cs_descriptor.segment.c_ed = 0) and (cs_descriptor.dpl <> cs_selector.rpl)) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_cs_selector  and $fffc;
        goto post_exception;
      end
      else
      // if conforming then DPL must be <= selector RPL else #TS(CS)
      if ((cs_descriptor.segment.c_ed <> 0) and (cs_descriptor.dpl>cs_selector.rpl)) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_cs_selector  and $fffc;
        goto post_exception;
      end
      else
      // Code segment is present in memory, else #NP(new code segment)
      if (cs_descriptor.p = 0) then
      begin
        exception_no := BX_NP_EXCEPTION;
        error_code   := raw_cs_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_CS].cache    := cs_descriptor;
    end else
    begin
    // If new cs selector is null #TS(CS)
      exception_no := BX_TS_EXCEPTION;
      error_code   := raw_cs_selector  and $fffc;
      goto post_exception;
    end;
    // SS
    if ( (raw_ss_selector  and $fffc) <> 0 ) then
    begin
      good := fetch_raw_descriptor2(@ss_selector, @dword1, @dword2);
      if (good = 0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ss_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @ss_descriptor);
      // SS selector must be within its descriptor table limits else #TS(SS)
      // SS descriptor AR byte must must indicate writable data segment,
      // else #TS(SS)
      if ((ss_descriptor.valid = 0) or
          (ss_descriptor.segmentType = 0) or
          (ss_descriptor.segment.executable <> 0) or
          (ss_descriptor.segment.r_w = 0)) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ss_selector  and $fffc;
        goto post_exception;
      end
      else
      //
      // Stack segment is present in memory, else #SF(new stack segment)
      //
      if (ss_descriptor.p = 0) then
      begin
        exception_no := BX_SS_EXCEPTION;
        error_code   := raw_ss_selector  and $fffc;
        goto post_exception;
      end
      else
      // Stack segment DPL matches CS.RPL, else #TS(new stack segment)
      if (ss_descriptor.dpl <> cs_selector.rpl) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ss_selector  and $fffc;
        goto post_exception;
      end
      else
      // Stack segment DPL matches selector RPL, else #TS(new stack segment)
      if (ss_descriptor.dpl <> ss_selector.rpl) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ss_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_SS].cache    := ss_descriptor;
    end else
    begin
    // SS selector is valid, else #TS(new stack segment)
      exception_no := BX_TS_EXCEPTION;
      error_code   := raw_ss_selector  and $fffc;
      goto post_exception;
    end;
    //   if new selector is not null then perform following checks:
    //     index must be within its descriptor table limits else #TS(selector)
    //     AR byte must indicate data or readable code else #TS(selector)
    //     if data or non-conforming code then:
    //       DPL must be >= CPL else #TS(selector)
    //       DPL must be >= RPL else #TS(selector)
    //     AR byte must indicate PRESENT else #NP(selector)
    //     load cache with new segment descriptor and set valid bit
    // DS
    if ( (raw_ds_selector  and $fffc) <> 0 ) then
    begin
      good := fetch_raw_descriptor2(@ds_selector, @dword1, @dword2);
      if (good = 0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ds_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @ds_descriptor);
      if ((ds_descriptor.valid = 0) or
          (ds_descriptor.segmentType = 0) or
          ((ds_descriptor.segment.executable <> 0) and
           (ds_descriptor.segment.r_w = 0))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ds_selector  and $fffc;
        goto post_exception;
      end
      else
      // if data or non-conforming code
      if (ds_descriptor.type_ < 12) and
          ((ds_descriptor.dpl < cs_selector.rpl) or
           (ds_descriptor.dpl < ds_selector.rpl)) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_ds_selector  and $fffc;
        goto post_exception;
      end else
      if (ds_descriptor.p = 0) then
      begin
        exception_no := BX_NP_EXCEPTION;
        error_code   := raw_ds_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_DS].cache    := ds_descriptor;
    end;
    // ES
    if ( (raw_es_selector  and $fffc) <> 0 ) then
    begin
      good := fetch_raw_descriptor2(@es_selector, @dword1, @dword2);
      if (good = 0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_es_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @es_descriptor);
      if ((es_descriptor.valid = 0) or
          (es_descriptor.segmentType = 0) or
          ((es_descriptor.segment.executable <> 0) and
           (es_descriptor.segment.r_w = 0))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_es_selector  and $fffc;
        goto post_exception;
      end
      else
      // if data or non-conforming code
      if ((es_descriptor.type_ < 12) and ((es_descriptor.dpl < cs_selector.rpl) or
          (es_descriptor.dpl < es_selector.rpl))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_es_selector  and $fffc;
        goto post_exception;
      end else
      if (es_descriptor.p = 0) then
      begin
        exception_no := BX_NP_EXCEPTION;
        error_code   := raw_es_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_ES].cache    := es_descriptor;
    end;
    // FS
    if ( (raw_fs_selector  and $fffc) <> 0 ) then // not NULL
    begin
      good := fetch_raw_descriptor2(@fs_selector, @dword1, @dword2);
      if (good = 0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_fs_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @fs_descriptor);
      if ((fs_descriptor.valid = 0) or (fs_descriptor.segmentType = 0) or
          ((fs_descriptor.segment.executable <> 0) and
           (fs_descriptor.segment.r_w = 0))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_fs_selector  and $fffc;
        goto post_exception;
      end else
      // if data or non-conforming code
      if ((fs_descriptor.type_ < 12) and
          ((fs_descriptor.dpl < cs_selector.rpl) or
           (fs_descriptor.dpl < fs_selector.rpl))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_fs_selector  and $fffc;
        goto post_exception;
      end else
      if (fs_descriptor.p = 0) then
      begin
        exception_no := BX_NP_EXCEPTION;
        error_code   := raw_fs_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_FS].cache    := fs_descriptor;
    end;
    // GS
    if ( (raw_gs_selector  and $fffc) <> 0 ) then
    begin
      good := fetch_raw_descriptor2(@gs_selector, @dword1, @dword2);
      if (good = 0) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_gs_selector  and $fffc;
        goto post_exception;
      end;

      parse_descriptor(dword1, dword2, @gs_descriptor);
      if ((gs_descriptor.valid = 0) or (gs_descriptor.segmentType = 0) or
          ((gs_descriptor.segment.executable <> 0) and
           (gs_descriptor.segment.r_w = 0))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_gs_selector  and $fffc;
        goto post_exception;
      end else
      // if data or non-conforming code
      if ((gs_descriptor.type_ < 12) and
          ((gs_descriptor.dpl < cs_selector.rpl) or
           (gs_descriptor.dpl < gs_selector.rpl))) then
      begin
        exception_no := BX_TS_EXCEPTION;
        error_code   := raw_gs_selector  and $fffc;
        goto post_exception;
      end else
      if (gs_descriptor.p = 0) then
      begin
        exception_no := BX_NP_EXCEPTION;
        error_code   := raw_gs_selector  and $fffc;
        goto post_exception;
      end;
      // All checks pass, fill in shadow cache
      Self.sregs[SEG_REG_GS].cache    := gs_descriptor;
    end;
  end;
  if ((tss_descriptor^.type_ >= 9) and ((trap_word  and $0001) <> 0)) then
  begin
    Self.debug_trap := Self.debug_trap or $00008000; // BT flag in DR6
    Self.async_event := 1; // so processor knows to check
  end;
  // Step 14: Begin execution of new task.
  exit;

post_exception:
  Self.debug_trap := 0;
  Self.inhibit_mask := 0;
  exception2([exception_no, error_code, 0]);
end;

procedure TCPU.get_SS_ESP_from_TSS(pl: unsigned; ss: PBit16u; esp: PBit32u);
var
  TSSstackaddr: Bit32u;
  temp16: Bit16u;
begin
  if (Self.tr.cache.type_ = 9) then
  begin
    // 32-bit TSS
    TSSstackaddr := 8 * pl + 4;
    if ( (TSSstackaddr + 7) > Self.tr.cache.tss386.limit_scaled ) then
      exception2([BX_TS_EXCEPTION, Self.tr.selector.value  and $fffc, 0]);

    access_linear(Self.tr.cache.tss386.base +
      TSSstackaddr + 4, 2, 0, BX_READ, ss);
    access_linear(Self.tr.cache.tss386.base +
      TSSstackaddr,   4, 0, BX_READ, esp);
  end else
  if (Self.tr.cache.type_ = 1) then
  begin
    // 16-bit TSS
    TSSstackaddr := 4 * pl + 2;
    if ( (TSSstackaddr + 4) > Self.tr.cache.tss286.limit ) then
      exception2([BX_TS_EXCEPTION, Self.tr.selector.value  and $fffc, 0]);

    access_linear(Self.tr.cache.tss286.base +
      TSSstackaddr + 2, 2, 0, BX_READ, ss);
    access_linear(Self.tr.cache.tss286.base +
      TSSstackaddr,   2, 0, BX_READ, @temp16);
    esp^ := temp16; // truncate
  end;
end;

procedure TCPU.init(addrspace: TMEM_C);
begin
//  BX_DEBUG(( 'Init $Id: init.cc,v 1.15 2002/03/27 16:04:05 bdenney Exp $'));
  // BX_CPU_C constructor
  Self.set_INTR(0);
  // in SMP mod_e, the prefix of the CPU will be changed to [CPUn] in
  // bx_local_apic_c.set_id as soon as the apic ID is assigned.
  (* hack for the following fields.  Its easier to decode mod_-rm bytes if
     you can assume there's always a base  and index register used.  For
     mod_es which don't really use them, point to an empty (zeroed) register.
   *)
  empty_register := 0;

  // 16bit address mod_e base register, used for mod_-rm decoding

  _16bit_base_reg[0] := @gen_reg[REG_BX_16BIT].rx;
  _16bit_base_reg[1] := @gen_reg[REG_BX_16BIT].rx;
  _16bit_base_reg[2] := @gen_reg[REG_BP_16BIT].rx;
  _16bit_base_reg[3] := @gen_reg[REG_BP_16BIT].rx;
  _16bit_base_reg[4] := PBit16u(@empty_register);
  _16bit_base_reg[5] := PBit16u(@empty_register);
  _16bit_base_reg[6] := @gen_reg[REG_BP_16BIT].rx;
  _16bit_base_reg[7] := @gen_reg[REG_BX_16BIT].rx;

  // 16bit address mod_e index register, used for mod_-rm decoding
  _16bit_index_reg[0] := @gen_reg[REG_SI_16BIT].rx;
  _16bit_index_reg[1] := @gen_reg[REG_DI_16BIT].rx;
  _16bit_index_reg[2] := @gen_reg[REG_SI_16BIT].rx;
  _16bit_index_reg[3] := @gen_reg[REG_DI_16BIT].rx;
  _16bit_index_reg[4] := @gen_reg[REG_SI_16BIT].rx;
  _16bit_index_reg[5] := @gen_reg[REG_DI_16BIT].rx;
  _16bit_index_reg[6] := PBit16u(@empty_register);
  _16bit_index_reg[7] := PBit16u(@empty_register);

  // for decoding instructions: access to seg reg's via index number
  sreg_mod00_rm16[0] := SEG_REG_DS;
  sreg_mod00_rm16[1] := SEG_REG_DS;
  sreg_mod00_rm16[2] := SEG_REG_SS;
  sreg_mod00_rm16[3] := SEG_REG_SS;
  sreg_mod00_rm16[4] := SEG_REG_DS;
  sreg_mod00_rm16[5] := SEG_REG_DS;
  sreg_mod00_rm16[6] := SEG_REG_DS;
  sreg_mod00_rm16[7] := SEG_REG_DS;

  sreg_mod01_rm16[0] := SEG_REG_DS;
  sreg_mod01_rm16[1] := SEG_REG_DS;
  sreg_mod01_rm16[2] := SEG_REG_SS;
  sreg_mod01_rm16[3] := SEG_REG_SS;
  sreg_mod01_rm16[4] := SEG_REG_DS;
  sreg_mod01_rm16[5] := SEG_REG_DS;
  sreg_mod01_rm16[6] := SEG_REG_SS;
  sreg_mod01_rm16[7] := SEG_REG_DS;

  sreg_mod10_rm16[0] := SEG_REG_DS;
  sreg_mod10_rm16[1] := SEG_REG_DS;
  sreg_mod10_rm16[2] := SEG_REG_SS;
  sreg_mod10_rm16[3] := SEG_REG_SS;
  sreg_mod10_rm16[4] := SEG_REG_DS;
  sreg_mod10_rm16[5] := SEG_REG_DS;
  sreg_mod10_rm16[6] := SEG_REG_SS;
  sreg_mod10_rm16[7] := SEG_REG_DS;

  // the default segment to use for a one-byte mod_rm with mod_=01b
  // and rm=i
  //
  sreg_mod01_rm32[0] := SEG_REG_DS;
  sreg_mod01_rm32[1] := SEG_REG_DS;
  sreg_mod01_rm32[2] := SEG_REG_DS;
  sreg_mod01_rm32[3] := SEG_REG_DS;
  sreg_mod01_rm32[4] := SEG_REG_NULL;
    // this entry should never be accessed
    // (escape to 2-byte)
  sreg_mod01_rm32[5] := SEG_REG_SS;
  sreg_mod01_rm32[6] := SEG_REG_DS;
  sreg_mod01_rm32[7] := SEG_REG_DS;

  // the default segment to use for a one-byte mod_rm with mod_=10b
  // and rm=i
  //
  sreg_mod10_rm32[0] := SEG_REG_DS;
  sreg_mod10_rm32[1] := SEG_REG_DS;
  sreg_mod10_rm32[2] := SEG_REG_DS;
  sreg_mod10_rm32[3] := SEG_REG_DS;
  sreg_mod10_rm32[4] := SEG_REG_NULL;
    // this entry should never be accessed
    // (escape to 2-byte)
  sreg_mod10_rm32[5] := SEG_REG_SS;
  sreg_mod10_rm32[6] := SEG_REG_DS;
  sreg_mod10_rm32[7] := SEG_REG_DS;


  // the default segment to use for a two-byte mod_rm with mod_=00b
  // and base=i
  //
  sreg_mod0_base32[0] := SEG_REG_DS;
  sreg_mod0_base32[1] := SEG_REG_DS;
  sreg_mod0_base32[2] := SEG_REG_DS;
  sreg_mod0_base32[3] := SEG_REG_DS;
  sreg_mod0_base32[4] := SEG_REG_SS;
  sreg_mod0_base32[5] := SEG_REG_DS;
  sreg_mod0_base32[6] := SEG_REG_DS;
  sreg_mod0_base32[7] := SEG_REG_DS;

  // the default segment to use for a two-byte mod_rm with
  // mod_=01b or mod_=10b and base=i
  sreg_mod1or2_base32[0] := SEG_REG_DS;
  sreg_mod1or2_base32[1] := SEG_REG_DS;
  sreg_mod1or2_base32[2] := SEG_REG_DS;
  sreg_mod1or2_base32[3] := SEG_REG_DS;
  sreg_mod1or2_base32[4] := SEG_REG_SS;
  sreg_mod1or2_base32[5] := SEG_REG_SS;
  sreg_mod1or2_base32[6] := SEG_REG_DS;
  sreg_mod1or2_base32[7] := SEG_REG_DS;

{$if BX_DYNAMIC_TRANSLATION <> 0}
  DTWrite8vShim := NULL;
  DTWrite16vShim := NULL;
  DTWrite32vShim := NULL;
  DTRead8vShim := NULL;
  DTRead16vShim := NULL;
  DTRead32vShim := NULL;
  DTReadRMW8vShim := (TDTShim_t) DTASReadRMW8vShim;
  BX_DEBUG(( 'DTReadRMW8vShim is %x', (unsigned) DTReadRMW8vShim ));
  BX_DEBUG(( '@DTReadRMW8vShim is %x', (unsigned) @DTReadRMW8vShim ));
  DTReadRMW16vShim := NULL;
  DTReadRMW32vShim := NULL;
  DTWriteRMW8vShim := (TDTShim_t) DTASWriteRMW8vShim;
  DTWriteRMW16vShim := NULL;
  DTWriteRMW32vShim := NULL;
  DTSetFlagsOSZAPCPtr := (TDTShim_t) DTASSetFlagsOSZAPC;
  DTIndBrHandler := (TDTShim_t) DTASIndBrHandler;
  DTDirBrHandler := (TDTShim_t) DTASDirBrHandler;
{$ifend}

  //mem := addrspace;
  FCPUName := 'CPU1';

  //BX_INSTR_INIT();
end;

procedure TCPU.reset(ResetInfo: PCPUResetInfo);
var
  hasInfo: boolean;
begin
  hasInfo := ResetInfo <> nil;
  //UNUSED(source); // either BX_RESET_HARDWARE or BX_RESET_SOFTWARE

  // general registers
  EAX := IfThen(hasInfo, ResetInfo^.val_EAX, 0); // processor passed test :-)
  EBX := IfThen(hasInfo, ResetInfo^.val_EBX, 0); // undefined
  ECX := IfThen(hasInfo, ResetInfo^.val_ECX, 0); // undefined
  EDX := IfThen(hasInfo, ResetInfo^.val_EDX, (BX_DEVICE_ID shl 8) or BX_STEPPING_ID); // ???
  EBP := IfThen(hasInfo, ResetInfo^.val_EBP, 0); // undefined
  ESI := IfThen(hasInfo, ResetInfo^.val_ESI, 0); // undefined
  EDI := IfThen(hasInfo, ResetInfo^.val_EDI, 0); // undefined
  ESP := IfThen(hasInfo, ResetInfo^.val_ESP, 0); // undefined

  // all status flags at known values, use Self.eflags structure
  Self.lf_flags_status := $000000;
  Self.lf_pf := 0;

  // флаги
  // status and control flags register set
  Self.set_CF(0);
  Self.eflags.bit1 := 1;
  Self.set_PF(0);
  Self.eflags.bit3 := 0;
  Self.set_AF(0);
  Self.eflags.bit5 := 0;
  Self.set_ZF(0);
  Self.set_SF(0);
  Self.eflags.tf := 0;
  Self.eflags.if_ := 0;
  Self.eflags.df := 0;
  Self.set_OF(0);
  Self.eflags.iopl := 0;
  Self.eflags.nt := 0;
  Self.eflags.bit15 := 0;
  Self.eflags.rf := 0;
  Self.eflags.vm := 0;
  Self.eflags.ac := 0;

  Self.inhibit_mask := 0;
  Self.debug_trap := 0;

  (* instruction pointer *)

  Self.FEIP := IfThen(hasInfo, ResetInfo^.val_EIP, $0000FFF0);
  Self.prev_eip := Self.FEIP;


  (* CS (Code Segment) and descriptor cache *)
  (* Note: on a real cpu, CS initially points to upper memory.  After
   * the 1st jump, the descriptor base is zero'd out.  Since I'm just
   * going to jump to my BIOS, I don't need to do this.
   * For future reference:
   *   processor  cs_.selector   cs_.base    cs_.limit    EIP
   *        8086    FFFF          FFFF0        FFFF   0000
   *        286     F000         FF0000        FFFF   FFF0
   *        386+    F000       FFFF0000        FFFF   FFF0
   *)
  Self.sregs[SEG_REG_CS].selector.value := IfThen(hasInfo, ResetInfo^.val_CS, $f000);
  Self.sregs[SEG_REG_CS].selector.index :=     $0000;
  Self.sregs[SEG_REG_CS].selector.ti := 0;
  Self.sregs[SEG_REG_CS].selector.rpl := 0;

  Self.sregs[SEG_REG_CS].cache.valid :=     1;
  Self.sregs[SEG_REG_CS].cache.p := 1;
  Self.sregs[SEG_REG_CS].cache.dpl := 0;
  Self.sregs[SEG_REG_CS].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_CS].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_CS].cache.segment.executable   := 1; (* data/stack segment *)
  Self.sregs[SEG_REG_CS].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_CS].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_CS].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_CS].cache.segment.base         := $000F0000;
  Self.sregs[SEG_REG_CS].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_CS].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_CS].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_CS].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_CS].cache.segment.avl := 0;

  (* SS (Stack Segment) and descriptor cache *)
  Self.sregs[SEG_REG_SS].selector.value :=     IfThen(hasInfo, ResetInfo^.val_SS, $0000);
  Self.sregs[SEG_REG_SS].selector.index :=     $0000;
  Self.sregs[SEG_REG_SS].selector.ti := 0;
  Self.sregs[SEG_REG_SS].selector.rpl := 0;

  Self.sregs[SEG_REG_SS].cache.valid :=     1;
  Self.sregs[SEG_REG_SS].cache.p := 1;
  Self.sregs[SEG_REG_SS].cache.dpl := 0;
  Self.sregs[SEG_REG_SS].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_SS].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_SS].cache.segment.executable   := 0; (* data/stack segment *)
  Self.sregs[SEG_REG_SS].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_SS].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_SS].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_SS].cache.segment.base         := $00000000;
  Self.sregs[SEG_REG_SS].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_SS].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_SS].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_SS].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_SS].cache.segment.avl := 0;

  (* DS (Data Segment) and descriptor cache *)
  Self.sregs[SEG_REG_DS].selector.value :=     IfThen(hasInfo, ResetInfo^.val_DS, $0000);
  Self.sregs[SEG_REG_DS].selector.index :=     $0000;
  Self.sregs[SEG_REG_DS].selector.ti := 0;
  Self.sregs[SEG_REG_DS].selector.rpl := 0;

  Self.sregs[SEG_REG_DS].cache.valid :=     1;
  Self.sregs[SEG_REG_DS].cache.p := 1;
  Self.sregs[SEG_REG_DS].cache.dpl := 0;
  Self.sregs[SEG_REG_DS].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_DS].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_DS].cache.segment.executable   := 0; (* data/stack segment *)
  Self.sregs[SEG_REG_DS].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_DS].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_DS].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_DS].cache.segment.base         := $00000000;
  Self.sregs[SEG_REG_DS].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_DS].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_DS].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_DS].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_DS].cache.segment.avl := 0;

  (* ES (Extra Segment) and descriptor cache *)
  Self.sregs[SEG_REG_ES].selector.value :=     IfThen(hasInfo, ResetInfo^.val_ES, $0000);
  Self.sregs[SEG_REG_ES].selector.index :=     $0000;
  Self.sregs[SEG_REG_ES].selector.ti := 0;
  Self.sregs[SEG_REG_ES].selector.rpl := 0;

  Self.sregs[SEG_REG_ES].cache.valid :=     1;
  Self.sregs[SEG_REG_ES].cache.p := 1;
  Self.sregs[SEG_REG_ES].cache.dpl := 0;
  Self.sregs[SEG_REG_ES].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_ES].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_ES].cache.segment.executable   := 0; (* data/stack segment *)
  Self.sregs[SEG_REG_ES].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_ES].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_ES].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_ES].cache.segment.base         := $00000000;
  Self.sregs[SEG_REG_ES].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_ES].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_ES].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_ES].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_ES].cache.segment.avl := 0;

  (* FS and descriptor cache *)
  Self.sregs[SEG_REG_FS].selector.value :=     IfThen(hasInfo, ResetInfo^.val_FS, $0000);
  Self.sregs[SEG_REG_FS].selector.index :=     $0000;
  Self.sregs[SEG_REG_FS].selector.ti := 0;
  Self.sregs[SEG_REG_FS].selector.rpl := 0;

  Self.sregs[SEG_REG_FS].cache.valid :=     1;
  Self.sregs[SEG_REG_FS].cache.p := 1;
  Self.sregs[SEG_REG_FS].cache.dpl := 0;
  Self.sregs[SEG_REG_FS].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_FS].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_FS].cache.segment.executable   := 0; (* data/stack segment *)
  Self.sregs[SEG_REG_FS].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_FS].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_FS].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_FS].cache.segment.base         := $00000000;
  Self.sregs[SEG_REG_FS].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_FS].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_FS].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_FS].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_FS].cache.segment.avl := 0;

  (* GS and descriptor cache *)
  Self.sregs[SEG_REG_GS].selector.value :=     IfThen(hasInfo, ResetInfo^.val_GS, $0000);
  Self.sregs[SEG_REG_GS].selector.index :=     $0000;
  Self.sregs[SEG_REG_GS].selector.ti := 0;
  Self.sregs[SEG_REG_GS].selector.rpl := 0;

  Self.sregs[SEG_REG_GS].cache.valid :=     1;
  Self.sregs[SEG_REG_GS].cache.p := 1;
  Self.sregs[SEG_REG_GS].cache.dpl := 0;
  Self.sregs[SEG_REG_GS].cache.segmentType := 1; (* data/code segment *)
  Self.sregs[SEG_REG_GS].cache.type_ := 3; (* read/write access *)

  Self.sregs[SEG_REG_GS].cache.segment.executable   := 0; (* data/stack segment *)
  Self.sregs[SEG_REG_GS].cache.segment.c_ed         := 0; (* normal expand up *)
  Self.sregs[SEG_REG_GS].cache.segment.r_w          := 1; (* writeable *)
  Self.sregs[SEG_REG_GS].cache.segment.a            := 1; (* accessed *)
  Self.sregs[SEG_REG_GS].cache.segment.base         := $00000000;
  Self.sregs[SEG_REG_GS].cache.segment.limit        :=     $FFFF;
  Self.sregs[SEG_REG_GS].cache.segment.limit_scaled :=     $FFFF;
  Self.sregs[SEG_REG_GS].cache.segment.g   := 0; (* byte granular *)
  Self.sregs[SEG_REG_GS].cache.segment.d_b := 0; (* 16bit default size *)
  Self.sregs[SEG_REG_GS].cache.segment.avl := 0;

  (* GDTR (Global Descriptor Table Register) *)
  Self.gdtr.base         := $00000000;  (* undefined *)
  Self.gdtr.limit        :=     $0000;  (* undefined *)
  (* ??? AR:=Present, Read/Write *)

  (* IDTR (Interrupt Descriptor Table Register) *)
  Self.idtr.base         := $00000000;
  Self.idtr.limit        :=     $03FF; (* always byte granular *) (* ??? *)
  (* ??? AR:=Present, Read/Write *)

  (* LDTR (Local Descriptor Table Register) *)
  Self.ldtr.selector.value :=     $0000;
  Self.ldtr.selector.index :=     $0000;
  Self.ldtr.selector.ti := 0;
  Self.ldtr.selector.rpl := 0;

  Self.ldtr.cache.valid   := 0; (* not valid *)
  Self.ldtr.cache.p       := 0; (* not present *)
  Self.ldtr.cache.dpl     := 0; (* field not used *)
  Self.ldtr.cache.segmentType := 0; (* system segment *)
  Self.ldtr.cache.type_    := 2; (* LDT descriptor *)

  Self.ldtr.cache.ldt.base      := $00000000;
  Self.ldtr.cache.ldt.limit     :=     $FFFF;

  (* TR (Task Register) *)
  (* ??? I don't know what state the TR comes up in *)
  Self.tr.selector.value :=     $0000;
  Self.tr.selector.index :=     $0000; (* undefined *)
  Self.tr.selector.ti    :=     0;
  Self.tr.selector.rpl   :=     0;

  Self.tr.cache.valid    := 0;
  Self.tr.cache.p        := 0;
  Self.tr.cache.dpl      := 0; (* field not used *)
  Self.tr.cache.segmentType  := 0;
  Self.tr.cache.type_     := 0; (* invalid *)
  Self.tr.cache.tss286.base             := $00000000; (* undefined *)
  Self.tr.cache.tss286.limit            :=     $0000; (* undefined *)

  // DR0 - DR7 (Debug Registers)
  Self.FDR0 := IfThen(hasInfo, ResetInfo^.val_DR0, 0);   (* undefined *)
  Self.FDR1 := IfThen(hasInfo, ResetInfo^.val_DR1, 0);   (* undefined *)
  Self.FDR2 := IfThen(hasInfo, ResetInfo^.val_DR2, 0);   (* undefined *)
  Self.FDR3 := IfThen(hasInfo, ResetInfo^.val_DR3, 0);   (* undefined *)
  Self.FDR6 := IfThen(hasInfo, ResetInfo^.val_DR6, $FFFF0FF0);
  Self.FDR7 := IfThen(hasInfo, ResetInfo^.val_DR7, $00000400);

  // MSW (Machine Status Word), so called on 286
  // CR0 (Control Register 0), so called on 386+
  Self.FCR0.ts := 0; // no task switch
  Self.FCR0.em := 0; // emulate math coprocessor
  Self.FCR0.mp := 0; // wait instructions not trapped
  Self.FCR0.pe := 0; // real mod_e
  Self.FCR0.val32 := 0;

  Self.FCR0.pg := 0; // paging disabled
  // no change to cr0.val32

  Self.FCR0.cd := 1; // caching disabled
  Self.FCR0.nw := 1; // not write-through
  Self.FCR0.am := 0; // disable alignment check
  Self.FCR0.wp := 0; // disable write-protect
  Self.FCR0.ne := 0; // ndp exceptions through int 13H, DOS compat
  Self.FCR0.val32 := Self.FCR0.val32 or $60000000;

  // handle reserved bits
  // reserved bits all set to 1 on 386
  Self.FCR0.val32 := Self.FCR0.val32 or $7ffffff0;
  // bit 4 is hardwired to 1 on all x86
  Self.FCR0.val32 := Self.FCR0.val32 or $00000010;

  Self.FCR2 := IfThen(hasInfo, ResetInfo^.val_CR2, 0);
  Self.FCR3 := IfThen(hasInfo, ResetInfo^.val_CR3, 0);
  Self.FCR4 := IfThen(hasInfo, ResetInfo^.val_CR4, 0);

(* initialise MSR registers to defaults *)
  (* APIC Address, APIC enabled and BSP is default, we'll fill in the rest later *)
  Self.msr.apicbase := (APIC_BASE_ADDR shl 12) + $900;

  Self.EXT := 0;
  //BX_INTR := 0;

  TLB_init();

  Self.bytesleft := 0;
  Self.fetch_ptr := nil;
  Self.prev_linear_page := 0;
  Self.prev_phy_page := 0;
  Self.max_phy_addr := 0;

//(*
//#if BX_DEBUGGER
//#ifdef MAGIC_BREAKPOINT
//  Self.magic_break := 0;
//{$ifend}
//  Self.stop_reason := STOP_NO_REASON;
//  Self.trace := 0;
//{$ifend}*)

  // Init the Floating Point Unit
  bx_cpu.fpu_init;

//{$if BX_DYNAMIC_TRANSLATION = 1}
//  dynamic_init();
//{$ifend}

//(*
//#if (BX_SMP_PROCESSORS > 1)
//  // notice if I'm the bootstrap processor.  If not, do the equivalent of
//  // a HALT instruction.
//  int apic_id := local_apic.get_id ();
//  if (BX_BOOTSTRAP_PROCESSOR = apic_id)
//  begin
//    // boot normally
//    Self.bsp := 1;
//    Self.msr.apicbase |:= $0100;	// set bit 8 BSP
//    BX_INFO(('CPU[%d] is the bootstrap processor', apic_id));
//  end; else begin
//    // it's an application processor, halt until IPI is heard.
//    Self.bsp := 0;
//    Self.msr.apicbase @:= ~$0100;	// clear bit 8 BSP
//    BX_INFO(('CPU[%d] is an application processor. Halting until IPI.', apic_id));
//    debug_trap |:= $80000000;
//    async_event := 1;
//  end;
//{$ifend}*)
end;


procedure TCPU.sanity_checks;
begin
  EAX := $FFEEDDCC;
  ECX := $BBAA9988;
  EDX := $77665544;
  EBX := $332211FF;
  ESP := $EEDDCCBB;
  EBP := $AA998877;
  ESI := $66554433;
  EDI := $2211FFEE;
end;

procedure TCPU.set_INTR(value:Bool);
begin
  Self.INTR := value;
  Self.async_event := 1;
end;

procedure TCPU.enter_protected_mode;
begin

end;

procedure TCPU.enter_real_mode;
begin

end;

procedure TCPU.Resolve16mod0Rm0(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX + SI);
end;

procedure TCPU.Resolve16mod0Rm1(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX + DI);
end;

procedure TCPU.Resolve16mod0Rm2(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BP + SI);
end;

procedure TCPU.Resolve16mod0Rm3(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BP + DI);
end;

procedure TCPU.Resolve16mod0Rm4(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(SI);
end;

procedure TCPU.Resolve16mod0Rm5(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(DI);
end;

procedure TCPU.Resolve16mod0Rm7(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX);
end;

procedure TCPU.Resolve16mod1or2Rm0(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX + SI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm1(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX + DI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm2(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BP + SI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm3(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BP + DI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm4(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(SI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm5(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(DI + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm6(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BP + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve16mod1or2Rm7(I: PInstruction_tag);
begin
  i^.rm_addr := Bit16u(BX + Bit16s(i^.displ16u));
end;

procedure TCPU.Resolve32mod0Rm0(I: PInstruction_tag);
begin
  i^.rm_addr := EAX;
end;

procedure TCPU.Resolve32mod0Rm1(I: PInstruction_tag);
begin
  i^.rm_addr := ECX;
end;

procedure TCPU.Resolve32mod0Rm2(I: PInstruction_tag);
begin
  i^.rm_addr := EDX;
end;

procedure TCPU.Resolve32mod0Rm3(I: PInstruction_tag);
begin
  i^.rm_addr := EBX;
end;

procedure TCPU.Resolve32mod0Rm6(I: PInstruction_tag);
begin
  i^.rm_addr := ESI;
end;

procedure TCPU.Resolve32mod0Rm7(I: PInstruction_tag);
begin
  i^.rm_addr := EDI;
end;

procedure TCPU.Resolve32mod1or2Rm0(I: PInstruction_tag);
begin
  i^.rm_addr := EAX + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm1(I: PInstruction_tag);
begin
  i^.rm_addr := ECX + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm2(I: PInstruction_tag);
begin
  i^.rm_addr := EDX + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm3(I: PInstruction_tag);
begin
  i^.rm_addr := EBX + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm5(I: PInstruction_tag);
begin
  i^.rm_addr := EBP + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm6(I: PInstruction_tag);
begin
  i^.rm_addr := ESI + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Rm7(I: PInstruction_tag);
begin
  i^.rm_addr := EDI + i^.displ32u;
end;

procedure TCPU.Resolve32mod0Base0(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EAX + scaled_index;
end;

procedure TCPU.Resolve32mod0Base1(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ECX + scaled_index;
end;

procedure TCPU.Resolve32mod0Base2(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EDX + scaled_index;
end;

procedure TCPU.Resolve32mod0Base3(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EBX + scaled_index;
end;

procedure TCPU.Resolve32mod0Base4(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ESP + scaled_index;
end;

procedure TCPU.Resolve32mod0Base5(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := i^.displ32u + scaled_index;
end;

procedure TCPU.Resolve32mod0Base6(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ESI + scaled_index;
end;

procedure TCPU.Resolve32mod0Base7(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EDI + scaled_index;
end;

procedure TCPU.Resolve32mod1or2Base0(I: PInstruction_tag);
var
  scaled_index:Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EAX + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base1(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ECX + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base2(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EDX + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base3(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EBX + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base4(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ESP + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base5(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EBP + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base6(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := ESI + scaled_index + i^.displ32u;
end;

procedure TCPU.Resolve32mod1or2Base7(I: PInstruction_tag);
var
  scaled_index: Bit32u;
begin
  if (i^.index <> 4)
    then scaled_index := BX_READ_32BIT_REG(i^.index) shl i^.scale
    else scaled_index := 0;
  i^.rm_addr := EDI + scaled_index + i^.displ32u;
end;

procedure TCPU.MOV_RLIb(I: PInstruction_tag);
begin
  Self.gen_reg[i^.b1  and $03].rl := i^.Ib;
end;

procedure TCPU.MOV_RHIb(I: PInstruction_tag);
begin
  Self.gen_reg[i^.b1  and $03].rh := i^.Ib;
end;

procedure TCPU.MOV_EbGb(I: PInstruction_tag);
var
  op2: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* now write op2 to op1 *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, op2)
    else write_virtual_byte(i^.seg, i^.rm_addr, @op2);
end;

procedure TCPU.MOV_GbEb(I: PInstruction_tag);
var
  op2: Bit8u;
begin
  if (i^.mod_ = $c0)
    then op2 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2);
  BX_WRITE_8BIT_REG(i^.nnn, op2);
end;

procedure TCPU.MOV_ALOb(I: PInstruction_tag);
var
  temp_8: Bit8u;
  addr_32: Bit32u;
begin
  addr_32 := i^.Id;
  (* read from memory address *)
  if Boolean((i^.seg and SEG_REG_NULL) = 0)
    then read_virtual_byte(i^.seg, addr_32, @temp_8)
    else read_virtual_byte(SEG_REG_DS, addr_32, @temp_8);
  (* write to register *)
  AL := temp_8;
end;

procedure TCPU.MOV_ObAL(I: PInstruction_tag);
var
  temp_8: Bit8u;
  addr_32: Bit32u;
begin
  addr_32 := i^.Id;
  (* read from register *)
  temp_8 := AL;
  (* write to memory address *)
  if (i^.seg and SEG_REG_NULL) = 0
    then write_virtual_byte(i^.seg, addr_32, @temp_8)
    else write_virtual_byte(SEG_REG_DS, addr_32, @temp_8);
end;

procedure TCPU.MOV_EbIb(I: PInstruction_tag);
var
  op2:Bit8u;
begin
  op2 := i^.Ib;
  (* now write op2 back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, op2)
    else write_virtual_byte(i^.seg, i^.rm_addr, @op2);
end;

procedure TCPU.XLAT(I: PInstruction_tag);
var
  offset_32: Bit32u;
  al_: Bit8u;
begin
  if (i^.as_32) <> 0
    then offset_32 := EBX + AL
    else offset_32 := BX + AL;

  if Boolean((i^.seg and SEG_REG_NULL) = 0)
    then read_virtual_byte(i^.seg, offset_32, @al_)
    else read_virtual_byte(SEG_REG_DS, offset_32, @al_);
  AL := al_;
end;

procedure TCPU.XCHG_EbGb(I: PInstruction_tag);
var
  op2, op1: Bit8u;
begin
  (* op2 is a register, op2_addr is an index of a register *)
  op2 := BX_READ_8BIT_REG(i^.nnn);
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0) then
  begin
    op1 := BX_READ_8BIT_REG(i^.rm);
    BX_WRITE_8BIT_REG(i^.rm, op2);
  end else
  begin
    (* pointer, segment address pair *)
    read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1);
    write_RMW_virtual_byte(op2);
  end;
  BX_WRITE_8BIT_REG(i^.nnn, op1);
end;

procedure TCPU.PUSH_RX(I: PInstruction_tag);
begin
  push_16(Self.gen_reg[i^.b1 and $07].rx);
end;

procedure TCPU.POP_RX(I: PInstruction_tag);
var
  rx: Bit16u;
begin
  pop_16(@rx);
  Self.gen_reg[i^.b1 and $07].rx := rx;
end;

procedure TCPU.POP_Ew(I: PInstruction_tag);
var
  val16: Bit16u;
begin
  pop_16(@val16);
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, val16)
    else
    begin
      // Note: there is one little weirdism here.  When 32bit addressing
      // is used, it is possible to use ESP in the mod_rm addressing.
      // If used, the value of ESP after the pop is used to calculate
      // the address.
      if ((i^.as_32 <> 0) and (i^.mod_ <> $c0) and (i^.rm = 4) and (i^.base = 4)) then
        i^.Resolvemodrm(i);
      write_virtual_word(i^.seg, i^.rm_addr, @val16);
    end;
end;

procedure TCPU.PUSHAD16(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  sp_: Bit16u;
begin
  if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if (can_push(@Self.sregs[SEG_REG_SS].cache, temp_ESP, 16) = 0) then
    begin
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;
  end;
  sp_ := SP;
  (* ??? optimize this by using virtual write, all checks passed *)
  push_16(AX);
  push_16(CX);
  push_16(DX);
  push_16(BX);
  push_16(sp_);
  push_16(BP);
  push_16(SI);
  push_16(DI);
end;

procedure TCPU.POPAD16(I: PInstruction_tag);
var
  di_, si_, bp_, tmp, bx_, dx_, cx_, ax_: Bit16u;
begin
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if (can_pop(16) = 0) then
    begin
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;
  (* ??? optimize this *)
  pop_16(@di_);
  pop_16(@si_);
  pop_16(@bp_);
  pop_16(@tmp); (* value for SP discarded *)
  pop_16(@bx_);
  pop_16(@dx_);
  pop_16(@cx_);
  pop_16(@ax_);

  DI := di_;
  SI := si_;
  BP := bp_;
  BX := bx_;
  DX := dx_;
  CX := cx_;
  AX := ax_;
end;

procedure TCPU.PUSH_Iw(I: PInstruction_tag);
var
  imm16: Bit16u;
begin
  imm16 := i^.Iw;
  push_16(imm16);
end;

procedure TCPU.PUSH_Ew(I: PInstruction_tag);
var
  op1_16: Bit16u;
begin
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  push_16(op1_16);
end;

procedure TCPU.BOUND_GvMa(I: PInstruction_tag);
var
  bound_min, bound_max: Bit32s;
  op1_32: Bit32s;
  bound_min_16, bound_max_16: Bit16s;
  op1_16: Bit16s;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i); (* undefined opcode exception *)

  if Boolean(i^.os_32) then
  begin
    op1_32 := BX_READ_32BIT_REG(i^.nnn);
    read_virtual_dword(i^.seg, i^.rm_addr, PBit32u(@bound_min));
    read_virtual_dword(i^.seg, i^.rm_addr + 4, PBit32u(@bound_max));
    (* ??? *)
    if ( (op1_32 < bound_min) or (op1_32 > bound_max) ) then
      exception2([5, 0, 0]);
  end else
  begin
    op1_16 := BX_READ_16BIT_REG(i^.nnn);
    read_virtual_word(i^.seg, i^.rm_addr, PBit16u(@bound_min_16));
    read_virtual_word(i^.seg, i^.rm_addr + 2, PBit16u(@bound_max_16));
    (* ??? *)
    if ( (op1_16 < bound_min_16) or (op1_16 > bound_max_16) ) then
      exception2([5, 0, 0]);
  end;
end;

procedure TCPU.INT1(I: PInstruction_tag);
begin
  // This is an undocumented instrucion (opcode $f1)
  // which is useful for an ICE system.
{$if BX_DEBUGGER=1}
  BX_CPU_THIS_PTR show_flag |:= Flag_int;
{$ifend}
  interrupt(1, 1, 0, 0);
end;

procedure TCPU.INT3(I: PInstruction_tag);
begin
  // INT 3 is not IOPL sensitive
{$if BX_DEBUGGER = 1}
  BX_CPU_THIS_PTR show_flag |:= Flag_int;
{$ifend}
   interrupt(3, 1, 0, 0);
end;

procedure TCPU.INT_Ib(I: PInstruction_tag);
var
  imm8: Bit8u;
begin
{$if BX_DEBUGGER=1}
  BX_CPU_THIS_PTR show_flag |:= Flag_int;
{$ifend}
  imm8 := i^.Ib;

  if Boolean((v8086_mode() <> 0) and (IOPL < 3)) then
    exception2([BX_GP_EXCEPTION, 0, 0]);

{$if SHOW_EXIT_STATUS=1}
//if ( (imm8 = $21) and (AH = $4c) ) then begin
//  BX_INFO(Format('INT 21/4C called AL:=$%02x, BX:=$%04x',[AL,BX]));
//  end;
{$ifend}

  interrupt(imm8, 1, 0, 0);
(*  BX_INSTR_FAR_BRANCH(BX_INSTR_IS_INT,
                      BX_CPU_THIS_PTR sregs[BX_SEG_REG_CS].selector.value,
                      BX_CPU_THIS_PTR eip);*)
end;

procedure TCPU.INTO(I: PInstruction_tag);
begin
{$if BX_DEBUGGER=1}
  BX_CPU_THIS_PTR show_flag |:= Flag_int;
{$ifend}
  if Boolean(get_OF()) then
     interrupt(4, 1, 0, 0);
    {BX_INSTR_FAR_BRANCH(BX_INSTR_IS_INT,
                        BX_CPU_THIS_PTR sregs[BX_SEG_REG_CS].selector.value,
                        BX_CPU_THIS_PTR eip);}
end;

procedure TCPU.JCXZ_Jb(I: PInstruction_tag);
var
  temp_ECX: Bit32u;
  new_EIP: Bit32u;
begin
  if (i^.as_32) <> 0
    then temp_ECX := ECX
    else temp_ECX := CX;

  if ( temp_ECX = 0 ) then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    if (i^.os_32=0) then
      new_EIP := new_EIP and $0000ffff;
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
      if ( new_EIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
    FEIP := new_EIP;
    {BX_INSTR_CNEAR_BRANCH_TAKEN(new_EIP);}
    revalidate_prefetch_q();
  end;
end;

procedure TCPU.LOOPNE_Jb(I: PInstruction_tag);
var
  count, new_EIP: Bit32u;
begin
  if (i^.as_32) <> 0
    then count := ECX
    else count := CX;

  count := count - 1;
  if ( (count <> 0) and (get_ZF() = 0) ) then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    if (i^.os_32=0) then
      new_EIP := new_EIP and $0000ffff;
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
      if (new_EIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
    Self.FEIP := new_EIP;
    //BX_INSTR_CNEAR_BRANCH_TAKEN(new_EIP);
    revalidate_prefetch_q();
  end;

  if (i^.as_32) <> 0
    then ECX := ECX - 1
    else CX := CX - 1;
end;

procedure TCPU.LOOPE_Jb(I: PInstruction_tag);
var
  count, new_EIP:Bit32u;
begin

  if (i^.as_32) <> 0
    then count := ECX
    else count := CX;

  Dec(count);
  if ( (count <> 0) and (get_ZF <> 0)) then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    if (i^.os_32=0) then
      new_EIP := new_EIP and $0000ffff;
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
      if (new_EIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
    Self.FEIP := new_EIP;
    //BX_INSTR_CNEAR_BRANCH_TAKEN(new_EIP);
    revalidate_prefetch_q();
  end;
  if (i^.as_32) <> 0
    then ECX := ECX - 1
    else CX := CX - 1;
end;

procedure TCPU.LOOP_Jb(I: PInstruction_tag);
var
  count, new_EIP: Bit32u;
begin
  if (i^.as_32) <> 0
    then count := ECX
    else count := CX;

  Dec(count);
  if (count <> 0) then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    if (i^.os_32 = 0) then
      new_EIP := new_EIP and $0000ffff;
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
      if (new_EIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
    Self.FEIP := new_EIP;
    //BX_INSTR_CNEAR_BRANCH_TAKEN(new_EIP);
    revalidate_prefetch_q();
  end;

  if (i^.as_32) <> 0
    then ECX := ECX - 1
    else CX := CX - 1;
end;

procedure TCPU.SAHF(I: PInstruction_tag);
begin
  set_SF((AH and $80) shr 7);
  set_ZF((AH and $40) shr 6);
  set_AF((AH and $10) shr 4);
  set_CF(AH  and $01);
  set_PF((AH and $04) shr 2);
end;

procedure TCPU.LAHF(I: PInstruction_tag);
begin
  AH := IfThen(Boolean(get_SF()) , $80 , 0) or
        IfThen(Boolean(get_ZF()) , $40 , 0) or
        IfThen(Boolean(get_AF()) , $10 , 0) or
        IfThen(Boolean(get_PF()) , $04 , 0) or
        ($02) or
        IfThen(Boolean(get_CF()) , $01 , 0);
end;

procedure TCPU.CLC(I: PInstruction_tag);
begin
  set_CF(0);
end;

procedure TCPU.STC(I: PInstruction_tag);
begin
  set_CF(1);
end;

procedure TCPU.CLI(I: PInstruction_tag);
begin
  if Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) then
  begin
    if (CPL > IOPL) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;
  end else
    if Boolean(v8086_mode()) then
      if (IOPL <> 3) then
      begin
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
      end;

  Self.eflags.if_ := 0;
end;

procedure TCPU.STI(I: PInstruction_tag);
begin
  if Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) then
  begin
    if (CPL > IOPL) then
    begin
      exception(BX_GP_EXCEPTION, 0, 0);
      exit;
    end;
  end else
  if Boolean(v8086_mode()) then
    if (IOPL <> 3) then
    begin
      exception(BX_GP_EXCEPTION, 0, 0);
      exit;
    end;

  if Boolean(Self.eflags.if_ = 0) then
  begin
    Self.eflags.if_ := 1;
    Self.inhibit_mask := Self.inhibit_mask or BX_INHIBIT_INTERRUPTS;
    Self.async_event := 1;
  end;
end;

procedure TCPU.CLD(I: PInstruction_tag);
begin
  Self.eflags.df := 0;
end;

procedure TCPU.STD(I: PInstruction_tag);
begin
  Self.eflags.df := 1;
end;

procedure TCPU.CMC(I: PInstruction_tag);
begin
  set_CF(Word(get_CF() = 0));
end;

procedure TCPU.PUSHF_Fv(I: PInstruction_tag);
begin
  if (Boolean(v8086_mode()) and (IOPL < 3)) then
  begin
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;

  if Boolean(i^.os_32)
    then push_32(read_eflags()  and $00fcffff)
    else push_16(read_flags());
end;

procedure TCPU.POPF_Fv(I: PInstruction_tag);
var
  eflags: Bit32u;
  flags: Bit16u;
begin
  if Boolean(v8086_mode()) then
  begin
    if (IOPL < 3) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;
    if Boolean(i^.os_32) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;
  end;

  if Boolean(i^.os_32) then
  begin
    pop_32(@eflags);
    eflags := eflags and $00277fd7;
    if Boolean(real_mode() = 0)
      then write_eflags(eflags, (* change IOPL? *) Bool(CPL=0), (* change IF? *) Bool(CPL<=IOPL), 0, 0)
           (* real mod_e *)
      else write_eflags(eflags, (* change IOPL? *) 1, (* change IF? *) 1, 0, 0);
  end else
  begin (* 16 bit opsize *)
    pop_16(@flags);

    if Boolean(real_mode()=0)
      then write_flags(flags, (* change IOPL? *) Bool(CPL=0), (* change IF? *) Bool(CPL<=IOPL))
           (* real mod_e *)
      else write_flags(flags, (* change IOPL? *) 1, (* change IF? *) 1);
  end;
end;

procedure TCPU.SALC(I: PInstruction_tag);
begin
  AL := $ff * bit8u(Boolean(get_CF));
//  if Boolean(get_CF) then begin
//    AL := $ff;
//    end
//  else begin
//    AL := $00;
//    end;
end;

function TCPU.inp16(addr:Bit16u):Bit16u;
var
  ret16: Bit16u;
begin
  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean(Self.allow_io(addr, 2) = 0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      Result := 0;
      exit;
    end;

  ret16 := bx_pc_system.inp(addr, 2);
  Result:= ret16;
end;

procedure TCPU.outp16(addr:Bit16u;value:Bit16u);
begin
  (* If CPL <= IOPL, then all IO addresses are accessible.
   * Otherwise, must check the IO permission map on >286.
   * On the 286, there is no IO permissions map *)

  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean( Self.allow_io(addr, 2) = 0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

  bx_pc_system.outp(addr, value, 2);
end;

function TCPU.inp32(addr:Bit16u):Bit32u;
var
  ret32: Bit32u;
begin
  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean(Self.allow_io(addr, 4) = 0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      Result := 0;
      exit;
    end;

  ret32 := bx_pc_system.inp(addr, 4);
  Result:= ret32;
end;

procedure TCPU.outp32(addr:Bit16u; value:Bit32u);
begin
  (* If CPL <= IOPL, then all IO addresses are accessible.
   * Otherwise, must check the IO permission map on >286.
   * On the 286, there is no IO permissions map *)

  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean( Self.allow_io(addr, 4)=0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

  bx_pc_system.outp(addr, value, 4);
end;

function TCPU.inp8(addr:Bit16u):Bit8u;
var
  ret8: Bit8u;
begin
  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean(Self.allow_io(addr, 1) = 0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      Result := 0;
      Exit;
    end;

  ret8 := bx_pc_system.inp(addr, 1);
  Result:=ret8;
end;

procedure TCPU.outp8(addr:Bit16u; value:Bit8u);
begin
  (* If CPL <= IOPL, then all IO addresses are accessible.
   * Otherwise, must check the IO permission map on >286.
   * On the 286, there is no IO permissions map *)

  if (Boolean(Self.FCR0.pe) and (Boolean(Self.eflags.vm) or (CPL > IOPL))) then
    if Boolean( Self.allow_io(addr, 1) = 0) then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

  bx_pc_system.outp(addr, value, 1);
end;


function TCPU.allow_io(addr:Bit16u; len:unsigned):Bool;
var
  io_base, permission16: Bit16u;
  bit_index, i: Word;
begin
  if ((Self.tr.cache.valid = 0) or (Self.tr.cache.type_ <> 9)) then
  begin
    Result := 0;
    exit;
  end;

  access_linear(Self.tr.cache.tss386.base + 102, 2, 0, BX_READ, @io_base);

//  if (io_base <= 103) then begin
    {BX_INFO(('PE is %u', Self.cr0.pe));
    BX_INFO(('VM is %u', Self.eflags.vm));
    BX_INFO(('CPL is %u', CPL));
    BX_INFO(('IOPL is %u', IOPL));
    BX_INFO(('addr is %u', addr));
    BX_INFO(('len is %u', len));
    BX_PANIC(('allow_io(): TR:io_base <= 103'));}
//    end;

  if (io_base > Self.tr.cache.tss386.limit_scaled) then
  begin
    Result := 0;
    Exit;
  end;

  access_linear(Self.tr.cache.tss386.base + io_base + addr div 8,
                   2, 0, BX_READ, @permission16);

  bit_index := addr  and $07;
  permission16 := permission16 shr bit_index;
  I := 0;
  while I < len do
  begin
    if Boolean(permission16 and $01) then
    begin
      Result:=0;
      Exit;
    end;
    permission16 := permission16 shr 1;
    Inc(I);
  end;

  Result := 1;
end;

procedure TCPU.LES_GvMp(I: PInstruction_tag);
var
  es: Bit16u;
  reg_32: Bit32u;
  reg_16, es_16: Bit16u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  if Boolean(i^.os_32) then
  begin
    read_virtual_dword(i^.seg, i^.rm_addr, @reg_32);
    read_virtual_word(i^.seg, i^.rm_addr + 4, @es);

    load_seg_reg(@Self.sregs[SEG_REG_ES], es);

    BX_WRITE_32BIT_REG(i^.nnn, reg_32);
  end else
  begin (* 16 bit mod_e *)
    read_virtual_word(i^.seg, i^.rm_addr, @reg_16);
    read_virtual_word(i^.seg, i^.rm_addr + 2, @es_16);

    load_seg_reg(@Self.sregs[SEG_REG_ES], es_16);

    BX_WRITE_16BIT_REG(i^.nnn, reg_16);
  end;
end;

procedure TCPU.LDS_GvMp(I: PInstruction_tag);
var
  ds: Bit16u;
  reg_32: Bit32u;
  reg_16, ds_16: Bit16u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  if Boolean(i^.os_32) then
  begin
    read_virtual_dword(i^.seg, i^.rm_addr, @reg_32);
    read_virtual_word(i^.seg, i^.rm_addr + 4, @ds);

    load_seg_reg(@Self.sregs[SEG_REG_DS], ds);

    BX_WRITE_32BIT_REG(i^.nnn, reg_32);
  end else
  begin (* 16 bit mod_e *)
    read_virtual_word(i^.seg, i^.rm_addr, @reg_16);
    read_virtual_word(i^.seg, i^.rm_addr + 2, @ds_16);

    load_seg_reg(@Self.sregs[SEG_REG_DS], ds_16);

    BX_WRITE_16BIT_REG(i^.nnn, reg_16);
  end;
end;

procedure TCPU.LFS_GvMp(I: PInstruction_tag);
var
  reg_32: Bit32u;
  fs,fs_16: Bit16u;
  reg_16: Bit16u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  if Boolean(i^.os_32) then
  begin
    read_virtual_dword(i^.seg, i^.rm_addr, @reg_32);
    read_virtual_word(i^.seg, i^.rm_addr + 4, @fs);

    load_seg_reg(@Self.sregs[SEG_REG_FS], fs);

    BX_WRITE_32BIT_REG(i^.nnn, reg_32);
  end else
  begin (* 16 bit operand size *)
    read_virtual_word(i^.seg, i^.rm_addr, @reg_16);
    read_virtual_word(i^.seg, i^.rm_addr + 2, @fs_16);

    load_seg_reg(@Self.sregs[SEG_REG_FS], fs_16);

    BX_WRITE_16BIT_REG(i^.nnn, reg_16);
  end;
end;

procedure TCPU.LGS_GvMp(I: PInstruction_tag);
var
  reg_32: Bit32u;
  gs,gs_16: Bit16u;
  reg_16: Bit16u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  if Boolean(i^.os_32) then
  begin
    read_virtual_dword(i^.seg, i^.rm_addr, @reg_32);
    read_virtual_word(i^.seg, i^.rm_addr + 4, @gs);

    load_seg_reg(@Self.sregs[SEG_REG_GS], gs);

    BX_WRITE_32BIT_REG(i^.nnn, reg_32);
  end else
  begin (* 16 bit operand size *)
    read_virtual_word(i^.seg, i^.rm_addr, @reg_16);
    read_virtual_word(i^.seg, i^.rm_addr + 2, @gs_16);

    load_seg_reg(@Self.sregs[SEG_REG_GS], gs_16);

    BX_WRITE_16BIT_REG(i^.nnn, reg_16);
  end;
end;

procedure TCPU.LSS_GvMp(I: PInstruction_tag);
var
  reg_32: Bit32u;
  reg_16: Bit16u;
  ss_raw, ss_raw_16: Bit16u;
begin
  if (i^.mod_ = $c0) then
    UndefinedOpcode(i);

  if Boolean(i^.os_32) then
  begin
    read_virtual_dword(i^.seg, i^.rm_addr, @reg_32);
    read_virtual_word(i^.seg, i^.rm_addr + 4, @ss_raw);

    load_seg_reg(@Self.sregs[SEG_REG_SS], ss_raw);

    BX_WRITE_32BIT_REG(i^.nnn, reg_32);
  end else
  begin (* 16 bit operand size *)
    read_virtual_word(i^.seg, i^.rm_addr, @reg_16);
    read_virtual_word(i^.seg, i^.rm_addr + 2, @ss_raw_16);

    load_seg_reg(@Self.sregs[SEG_REG_SS], ss_raw_16);

    BX_WRITE_16BIT_REG(i^.nnn, reg_16);
  end;
end;

procedure TCPU.write_flags(flags:Bit16u; change_IOPL:Bool; change_IF:Bool);
begin
  Self.set_CF(flags  and $01);
  Self.set_PF((flags shr 2)  and $01);
  Self.set_AF((flags shr 4)  and $01);
  Self.set_ZF((flags shr 6)  and $01);
  Self.set_SF((flags shr 7)  and $01);

  Self.eflags.tf := (flags shr 8)  and $01;
  if Boolean(Self.eflags.tf) then
    Self.async_event := 1;

  if Boolean(change_IF) then
    Self.eflags.if_ := (flags shr 9)  and $01;

  Self.eflags.df := (flags shr 10)  and $01;
  Self.set_OF((flags shr 11)  and $01);

  if Boolean(change_IOPL) then
    Self.eflags.iopl := (flags shr 12)  and $03;
  Self.eflags.nt := (flags shr 14)  and $01;
end;


procedure TCPU.write_eflags(eflags_raw:Bit32u; change_IOPL:Bool ; change_IF:Bool;change_VM:Bool; change_RF:Bool);
begin
  Self.set_CF(eflags_raw and $01);
  Self.set_PF((eflags_raw shr 2)  and $01);
  Self.set_AF((eflags_raw shr 4)  and $01);
  Self.set_ZF((eflags_raw shr 6)  and $01);
  Self.set_SF((eflags_raw shr 7)  and $01);

  Self.eflags.tf := (eflags_raw shr 8)  and $01;
  if Boolean(Self.eflags.tf) then
    Self.async_event := 1;

  if Boolean(change_IF) then
    Self.eflags.if_ := (eflags_raw shr 9)  and $01;

  Self.eflags.df := (eflags_raw shr 10)  and $01;
  Self.set_OF((eflags_raw shr 11)  and $01);

  if Boolean(change_IOPL) then
    Self.eflags.iopl := (eflags_raw shr 12)  and $03;
  Self.eflags.nt := (eflags_raw shr 14)  and $01;

  if Boolean(change_VM) then
    Self.eflags.vm := (eflags_raw shr 17)  and $01;
  if Boolean(change_RF) then
    Self.eflags.rf := (eflags_raw shr 16)  and $01;

  Self.eflags.ac := (eflags_raw shr 18)  and $01;
  Self.eflags.id := (eflags_raw shr 21)  and $01;
end;

function TCPU.read_flags:Bit16u;
var
  flags: Bit16u;
begin
  flags := (get_CF()) or (Self.eflags.bit1 shl 1) or ((get_PF()) shl 2) or (Self.eflags.bit3 shl 3) or
           (Bool(get_AF()>0) shl 4) or (Self.eflags.bit5 shl 5) or (Bool(get_ZF()>0) shl 6) or (Bool(get_SF()>0) shl 7) or
           (Self.eflags.tf shl 8) or (Self.eflags.if_ shl 9) or (Self.eflags.df shl 10) or (Bool(get_OF()>0) shl 11) or
           (Self.eflags.iopl shl 12) or (Self.eflags.nt shl 14) or (Self.eflags.bit15 shl 15);

  (* 8086: bits 12-15 always set to 1.
   * 286: in real mod_e, bits 12-15 always cleared.
   * 386+: real-mod_e: bit15 cleared, bits 14..12 are last loaded value
   *       protected-mod_e: bit 15 clear, bit 14 := last loaded, IOPL?
   *)

  Result := Flags;
end;

function TCPU.read_eflags:Bit32u;
var
  eflags_raw: Bit32u;
begin
  eflags_raw :=
          (get_CF()) or
          (Self.eflags.bit1 shl 1) or
          ((get_PF()) shl 2) or
          (Self.eflags.bit3 shl 3) or
          (Bool(get_AF()>0) shl 4) or
          (Self.eflags.bit5 shl 5) or
          (Bool(get_ZF()>0) shl 6) or
          (Bool(get_SF()>0) shl 7) or
          (Self.eflags.tf shl 8) or
          (Self.eflags.if_ shl 9) or
          (Self.eflags.df shl 10) or
          (Bool(get_OF()>0) shl 11) or
          (Self.eflags.iopl shl 12) or
          (Self.eflags.nt shl 14) or
          (Self.eflags.bit15 shl 15) or
          (Self.eflags.rf shl 16) or
          (Self.eflags.vm shl 17)
         or (Self.eflags.ac shl 18)
         or (Self.eflags.id shl 21)
           ;

  Result:=eflags_raw;
end;

procedure TCPU.INSB_YbDX(I: PInstruction_tag);
var
  value8: Bit8u;
begin
  Value8 := 0;
  if ((self.FCR0.pe <> 0) and ((self.eflags.vm <> 0) or (CPL > IOPL))) then
    if Boolean(self.allow_io(DX, 1)=0) then
      exception2([BX_GP_EXCEPTION, 0, 0]);

  if (i^.as_32 <> 0) then
  begin
    // Write a zero to memory, to trigger any segment or page
    // faults before reading from IO port.
    write_virtual_byte(SEG_REG_ES, EDI, @value8);

    value8 := bx_pc_system.inp(DX, 1);

    (* no seg override possible *)
    write_virtual_byte(SEG_REG_ES, EDI, @value8);

    if Boolean(self.eflags.df)
      then EDI := EDI - 1
      else EDI := EDI + 1;
  end else
  begin
    // Write a zero to memory, to trigger any segment or page
    // faults before reading from IO port.
    write_virtual_byte(SEG_REG_ES, DI, @value8);

    value8 := bx_pc_system.inp(DX, 1);

    (* no seg override possible *)
    write_virtual_byte(SEG_REG_ES, DI, @value8);

    if Boolean(self.eflags.df)
      then DI := DI - 1
      else DI := DI + 1;
  end;
end;

procedure TCPU.INSW_YvDX(I: PInstruction_tag);
  // input word/doubleword from port to string
var
  edi_: Bit32u;
  incr: Word;
  value32: Bit32u;
  value16: Bit16u;
begin
  if Boolean(i^.as_32)
    then edi_ := EDI
    else edi_ := DI;

  if Boolean(i^.os_32) then
  begin
    value32 := 0;

    if (Boolean(self.FCR0.pe) and (Boolean(self.eflags.vm) or (CPL > IOPL))) then
      if Boolean(self.allow_io(DX, 4) = 0) then
        exception2([BX_GP_EXCEPTION, 0, 0]);

    // Write a zero to memory, to trigger any segment or page
    // faults before reading from IO port.
    write_virtual_dword(SEG_REG_ES, edi, @value32);

    value32 := bx_pc_system.inp(DX, 4);
    (* no seg override allowed *)
    write_virtual_dword(SEG_REG_ES, edi_, @value32);
    incr := 4;
  end else
  begin
    value16 := 0;

    if (Boolean(self.FCR0.pe) and (Boolean(self.eflags.vm) or (CPL > IOPL))) then
      if Boolean(self.allow_io(DX, 2) = 0) then
        exception2([BX_GP_EXCEPTION, 0, 0]);

    // Write a zero to memory, to trigger any segment or page
    // faults before reading from IO port.
    write_virtual_word(SEG_REG_ES, edi_, @value16);

    value16 := bx_pc_system.inp(DX, 2);
    (* no seg override allowed *)
    write_virtual_word(SEG_REG_ES, edi_, @value16);
    incr := 2;
  end;

  if Boolean(i^.as_32) then
  begin
    if Boolean(self.eflags.df)
      then EDI := EDI - incr
      else EDI := EDI + incr;
  end else
  begin
    if Boolean(self.eflags.df)
      then DI := DI - incr
      else DI := DI + incr;
  end;
end;

procedure TCPU.OUTSB_DXXb(I: PInstruction_tag);
var
  seg: Word;
  value8: Bit8u;
  esi_: Bit32u;
begin
  if (Boolean(self.FCR0.pe) and (Boolean(self.eflags.vm) or (CPL > IOPL))) then
    if Boolean(self.allow_io(DX, 1) = 0) then
      exception2([BX_GP_EXCEPTION, 0, 0]);

  if Boolean((i^.seg and SEG_REG_NULL)=0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if Boolean(i^.as_32)
    then esi_ := ESI
    else esi_ := SI;

  read_virtual_byte(seg, esi_, @value8);

  bx_pc_system.outp(DX, value8, 1);

  if Boolean(i^.as_32) then
  begin
    if Boolean(self.eflags.df)
      then ESI := ESI - 1
      else ESI := ESI + 1;
  end else
  begin
    if Boolean(self.eflags.df)
      then SI:=SI - 1
      else SI:=SI + 1
  end;
end;

procedure TCPU.OUTSW_DXXv(I: PInstruction_tag);
  // output word/doubleword string to port
var
  seg: Word;
  esi_: Bit32u;
  incr: Word;
  value32: Bit32u;
  value16: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL) = 0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0
    then esi_ := ESI
    else esi_ := SI;

  if (i^.os_32) <> 0 then
  begin
    if ((self.FCR0.pe<>0) and ((self.eflags.vm<>0) or (CPL>IOPL))) then
      if (( self.allow_io(DX, 4) ) = 0) then
        exception2([BX_GP_EXCEPTION, 0, 0]);

    read_virtual_dword(seg, esi_, @value32);

    bx_pc_system.outp(DX, value32, 4);
    incr := 4;
  end else
  begin
    if ((self.FCR0.pe<>0) and ((self.eflags.vm<>0) or (CPL > IOPL))) then
      if ( self.allow_io(DX, 2) = 0) then
        exception2([BX_GP_EXCEPTION, 0, 0]);

    read_virtual_word(seg, esi_, @value16);

    bx_pc_system.outp(DX, value16, 2);
    incr := 2;
  end;

  if (i^.as_32) <> 0 then
  begin
    if (self.eflags.df) <> 0
      then ESI := ESI - incr
      else ESI := ESI + incr;
  end else
  begin
    if (self.eflags.df) <> 0
      then SI := SI - incr
      else SI := SI + incr;
  end;
end;

procedure TCPU.IN_ALIb(I: PInstruction_tag);
var
  al_, imm8: Bit8u;
begin
  imm8 := i^.Ib;
  al_ := self.inp8(imm8);
  AL := al_;
end;

procedure TCPU.IN_eAXIb(I: PInstruction_tag);
var
  imm8: Bit8u;
  eax_: Bit32u;
  ax_: Bit16u;
begin
  imm8 := i^.Ib;

  if Boolean(i^.os_32) then
  begin
    eax_ := self.inp32(imm8);
    EAX := eax_;
  end else
  begin
    ax_ := self.inp16(imm8);
    AX := ax_;
  end;
end;

procedure TCPU.OUT_IbAL(I: PInstruction_tag);
var
  al_, imm8: Bit8u;
begin
  imm8 := i^.Ib;
  al_ := AL;
  self.outp8(imm8, al_);
end;

procedure TCPU.OUT_IbeAX(I: PInstruction_tag);
var
  imm8: Bit8u;
begin
  imm8 := i^.Ib;

  if Boolean(i^.os_32)
    then self.outp32(imm8, EAX)
    else self.outp16(imm8, AX);
end;

procedure TCPU.IN_ALDX(I: PInstruction_tag);
var
  al_: Bit8u;
begin
  al_ := self.inp8(DX);
  AL := al_;
end;

procedure TCPU.IN_eAXDX(I: PInstruction_tag);
var
  eax_: Bit32u;
  ax_: Bit16u;
begin
  if Boolean(i^.os_32) then
  begin
    eax_ := self.inp32(DX);
    EAX := eax_;
  end else
  begin
    ax_ := self.inp16(DX);
    AX := ax_;
  end;
end;

procedure TCPU.OUT_DXAL(I: PInstruction_tag);
var
  dx_: Bit16u;
  al_: Bit8u;
begin
  dx_ := DX;
  al_ := AL;

  self.outp8(dx_, al_);
end;

procedure TCPU.OUT_DXeAX(I: PInstruction_tag);
var
  dx_: Bit16u;
begin
  dx_ := DX;

  if Boolean(i^.os_32)
    then self.outp32(dx_, EAX)
    else self.outp16(dx_, AX);
end;

procedure TCPU.XCHG_ERXEAX(I: PInstruction_tag);
var
  temp32: Bit32u;
  idx: Byte;
begin
  temp32 := EAX;
  idx := i^.b1 and $07;
  EAX := self.gen_reg[idx].erx;
  self.gen_reg[idx].erx := temp32;
end;

procedure TCPU.MOV_ERXId(I: PInstruction_tag);
begin
  self.gen_reg[i^.b1 and $07].erx := i^.Id;
end;

procedure TCPU.MOV_EdGd(I: PInstruction_tag);
var
  op2_32: Bit32u;
begin
  (* op2_32 is a register, op2_addr is an index of a register *)
  op2_32 := BX_READ_32BIT_REG(i^.nnn);
  (* op1_32 is a register or memory reference *)
  (* now write op2 to op1 *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, op2_32)
    else write_virtual_dword(i^.seg, i^.rm_addr, @op2_32);
end;

procedure TCPU.MOV_GdEd(I: PInstruction_tag);
var
  op2_32: Bit32u;
begin
  if (i^.mod_ = $c0)
    then op2_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);

  BX_WRITE_32BIT_REG(i^.nnn, op2_32);
end;

procedure TCPU.LEA_GdM(I: PInstruction_tag);
begin
  if (i^.mod_ = $c0) then
  begin
    LogPanic(('LEA_GvM: op2 is a register'));
    UndefinedOpcode(i);
    exit;
  end;

  (* write effective address of op2 in op1 *)
  BX_WRITE_32BIT_REG(i^.nnn, i^.rm_addr);
end;

procedure TCPU.MOV_EAXOd(I: PInstruction_tag);
var
  temp_32: Bit32u;
  addr_32: Bit32u;
begin
  addr_32 := i^.Id;
  (* read from memory address *)
  if ((i^.seg and SEG_REG_NULL) = 0)
    then read_virtual_dword(i^.seg, addr_32, @temp_32)
    else read_virtual_dword(SEG_REG_DS, addr_32, @temp_32);

  (* write to register *)
  EAX := temp_32;
end;

procedure TCPU.MOV_OdEAX(I: PInstruction_tag);
var
  temp_32:Bit32u;
  addr_32:Bit32u;
begin

  addr_32 := i^.Id;

  (* read from register *)
  temp_32 := EAX;

  (* write to memory address *)
  if ((i^.seg and SEG_REG_NULL)=0) then begin
    write_virtual_dword(i^.seg, addr_32, @temp_32);
    end
  else begin
    write_virtual_dword(SEG_REG_DS, addr_32, @temp_32);
    end;
end;

procedure TCPU.MOV_EdId(I: PInstruction_tag);
var
  op2_32:Bit32u;
begin

    op2_32 := i^.Id;

    (* now write sum back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, op2_32);
      end
  else begin
      write_virtual_dword(i^.seg, i^.rm_addr, @op2_32);
      end;
end;

procedure TCPU.MOVZX_GdEb(I: PInstruction_tag);
var
  op2_8:Bit8u;
begin

  if (i^.mod_ = $c0) then begin
    op2_8 := BX_READ_8BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);
    end;

    (* zero extend byte op2 into dword op1 *)
    BX_WRITE_32BIT_REG(i^.nnn, Bit32u(op2_8));
end;

procedure TCPU.MOVZX_GdEw(I: PInstruction_tag);
var
  op2_16:Bit16u;
begin

  if (i^.mod_ = $c0) then begin
    op2_16 := BX_READ_16BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
    end;

    (* zero extend word op2 into dword op1 *)
    BX_WRITE_32BIT_REG(i^.nnn, Bit32u(op2_16));
end;

procedure TCPU.MOVSX_GdEb(I: PInstruction_tag);
var
  op2_8:Bit8u;
begin

  if (i^.mod_ = $c0) then begin
    op2_8 := BX_READ_8BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);
    end;

    (* sign extend byte op2 into dword op1 *)
    BX_WRITE_32BIT_REG(i^.nnn, Bit8s(op2_8));
end;

procedure TCPU.MOVSX_GdEw(I: PInstruction_tag);
var
  op2_16:Bit16u;
begin

  if (i^.mod_ = $c0) then begin
    op2_16 := BX_READ_16BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
    end;

    (* sign extend word op2 into dword op1 *)
    BX_WRITE_32BIT_REG(i^.nnn, Bit16s(op2_16));
end;

procedure TCPU.XCHG_EdGd(I: PInstruction_tag);
var
  op2_32, op1_32:Bit32u;
begin

    (* op2_32 is a register, op2_addr is an index of a register *)
    op2_32 := BX_READ_32BIT_REG(i^.nnn);

    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      BX_WRITE_32BIT_REG(i^.rm, op2_32);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      write_RMW_virtual_dword(op2_32);
      end;

    BX_WRITE_32BIT_REG(i^.nnn, op1_32);
end;

procedure TCPU.CMOV_GdEd(I: PInstruction_tag);
var
  condition:Bool;
  op2_32:Bit32u;
begin
{$if (BX_CPU_LEVEL >= 6) or (BX_CPU_LEVEL_HACKED >= 6) }
  // Note: CMOV accesses a memory source operand (read), regardless
  //       of whether condition is true or not.  Thus, exceptions may
  //       occur even if the MOV does not take place.

  case (i^.b1) of
    $140: condition := get_OF();
    $141: condition := not get_OF();
    $142: condition := get_CF();
    $143: condition := not get_CF();
    $144: condition := get_ZF();
    $145: condition := not get_ZF();
    $146: condition := get_CF() or get_ZF();
//    $147: condition := not get_CF() @ and not get_ZF();
    $148: condition := get_SF();
    $149: condition := not get_SF();
    $14A: condition := get_PF();
    $14B: condition := not get_PF();
    $14C: condition := word(Boolean(get_SF() <> get_OF())); //get_SF() !:= get_OF();
    $14D: condition := word(Boolean(get_SF() = get_OF()));
    $14E: condition := word(Boolean((get_ZF() or (get_SF()) <> get_OF()))); //get_ZF() or (get_SF() !:= get_OF());
//    $14F: condition := not get_ZF() @ and (get_SF() = get_OF());
  end;

//  switch (i^.b1) then begin
//    // CMOV opcodes:
//    case $140: condition := get_OF(); break;
//    case $141: condition := !get_OF(); break;
//    case $142: condition := get_CF(); break;
//    case $143: condition := !get_CF(); break;
//    case $144: condition := get_ZF(); break;
//    case $145: condition := !get_ZF(); break;
//    case $146: condition := get_CF() or get_ZF(); break;
//    case $147: condition := !get_CF() @ and !get_ZF(); break;
//    case $148: condition := get_SF(); break;
//    case $149: condition := !get_SF(); break;
//    case $14A: condition := get_PF(); break;
//    case $14B: condition := !get_PF(); break;
//    case $14C: condition := get_SF() !:= get_OF(); break;
//    case $14D: condition := get_SF() = get_OF(); break;
//    case $14E: condition := get_ZF() or (get_SF() !:= get_OF()); break;
//    case $14F: condition := !get_ZF() @ and (get_SF() = get_OF()); break;
//    default:
//      condition := 0;
//      BX_PANIC(('CMOV_GdEd: default case'));
//    end;

  if (i^.mod_ = $c0) then begin
    op2_32 := BX_READ_32BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_dword(i^.seg, i^.rm_addr, @op2_32);
    end;

  if Boolean(condition) then begin
    BX_WRITE_32BIT_REG(i^.nnn, op2_32);
    end;
{$else}
  LogPanic(('cmov_gded called'));
{$ifend}
end;

procedure TCPU.MOV_RXIw(I: PInstruction_tag);
begin
  self.gen_reg[i^.b1 and $07].rx := i^.Iw;
end;

procedure TCPU.XCHG_RXAX(I: PInstruction_tag);
var
  temp16: Bit16u;
  idx: Byte;
begin
  temp16 := AX;
  idx := i^.b1 and $07;
  AX := self.gen_reg[idx].rx;
  self.gen_reg[idx].rx := temp16;
end;

procedure TCPU.MOV_EwGw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op1_16 is a register or memory reference *)
  (* now write op2 to op1 *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, op2_16)
    else write_virtual_word(i^.seg, i^.rm_addr, @op2_16);
end;

procedure TCPU.MOV_GwEw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
         (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
  BX_WRITE_16BIT_REG(i^.nnn, op2_16);
end;

procedure TCPU.MOV_EwSw(I: PInstruction_tag);
var
  seg_reg: Bit16u;
begin
  seg_reg := self.sregs[i^.nnn].selector.value;
  if (i^.mod_ = $c0)
  then
    if (i^.os_32 <> 0)
      then BX_WRITE_32BIT_REG(i^.rm, seg_reg)
      else BX_WRITE_16BIT_REG(i^.rm, seg_reg)
  else
    write_virtual_word(i^.seg, i^.rm_addr, @seg_reg);
end;

procedure TCPU.MOV_SwEw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);

  load_seg_reg(@self.sregs[i^.nnn], op2_16);

  if (i^.nnn = SEG_REG_SS) then
  begin
    // MOV SS inhibits interrupts, debug exceptions and single-step
    // trap exceptions until the execution boundary following the
    // next instruction is reached.
    // Same code as POP_SS()
    self.inhibit_mask := self.inhibit_mask or BX_INHIBIT_INTERRUPTS or BX_INHIBIT_DEBUG;
    self.async_event := 1;
  end;
end;

procedure TCPU.LEA_GwM(I: PInstruction_tag);
begin
  if (i^.mod_ = $c0) then
  begin
    LogPanic(('LEA_GvM: op2 is a register'));
    UndefinedOpcode(i);
    exit;
  end;
  BX_WRITE_16BIT_REG(i^.nnn, Bit16u(i^.rm_addr));
end;

procedure TCPU.MOV_AXOw(I: PInstruction_tag);
var
  temp_16: Bit16u;
  addr_32: Bit32u;
begin
  addr_32 := i^.Id;
  (* read from memory address *)
  if ((i^.seg and SEG_REG_NULL)=0)
    then read_virtual_word(i^.seg, addr_32, @temp_16)
    else read_virtual_word(SEG_REG_DS, addr_32, @temp_16);
  (* write to register *)
  AX := temp_16;
end;

procedure TCPU.MOV_OwAX(I: PInstruction_tag);
var
  temp_16: Bit16u;
  addr_32: Bit32u;
begin
  addr_32 := i^.Id;
  (* read from register *)
  temp_16 := AX;
  (* write to memory address *)
  if ((i^.seg and SEG_REG_NULL)=0)
    then write_virtual_word(i^.seg, addr_32, @temp_16)
    else write_virtual_word(SEG_REG_DS, addr_32, @temp_16);
end;

procedure TCPU.MOV_EwIw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  op2_16 := i^.Iw;
  (* now write sum back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, op2_16)
    else write_virtual_word(i^.seg, i^.rm_addr, @op2_16);
end;

procedure TCPU.MOVZX_GwEb(I: PInstruction_tag);
var
  op2_8: Bit8u;
begin
  if (i^.mod_ = $c0)
    then op2_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);
    (* zero extend byte op2 into word op1 *)
  BX_WRITE_16BIT_REG(i^.nnn, (Bit16u(op2_8)));
end;

procedure TCPU.MOVZX_GwEw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
  (* normal move *)
  BX_WRITE_16BIT_REG(i^.nnn, op2_16);
end;

procedure TCPU.MOVSX_GwEb(I: PInstruction_tag);
var
  op2_8: Bit8u;
begin
  if (i^.mod_ = $c0)
    then op2_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_byte(i^.seg, i^.rm_addr, @op2_8);
  (* sign extend byte op2 into word op1 *)
  BX_WRITE_16BIT_REG(i^.nnn, (Bit8s(op2_8)));
end;

procedure TCPU.MOVSX_GwEw(I: PInstruction_tag);
var
  op2_16: Bit16u;
begin
  if (i^.mod_ = $c0)
    then op2_16 := BX_READ_16BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
  (* normal move *)
  BX_WRITE_16BIT_REG(i^.nnn, op2_16);
end;

procedure TCPU.XCHG_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16: Bit16u;
begin
  {$if BX_DEBUGGER=01}
  // (mch) Magic break point
  if (i^.nnn = 3 @ and i^.mod_ = $c0 @ and i^.rm = 3)
    then self.magic_break := 1;
  {$ifend}
  (* op2_16 is a register, op2_addr is an index of a register *)
  op2_16 := BX_READ_16BIT_REG(i^.nnn);
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0) then
  begin
    op1_16 := BX_READ_16BIT_REG(i^.rm);
    BX_WRITE_16BIT_REG(i^.rm, op2_16);
  end else
  begin
    (* pointer, segment address pair *)
    read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
    write_RMW_virtual_word(op2_16);
  end;
  BX_WRITE_16BIT_REG(i^.nnn, op1_16);
end;

procedure TCPU.CMOV_GwEw(I: PInstruction_tag);
var
  op2_16: Bit16u;
  condition: Bool;
begin
{$if (BX_CPU_LEVEL >= 6) or (BX_CPU_LEVEL_HACKED >= 6)}
  // Note: CMOV accesses a memory source operand (read), regardless
  //       of whether condition is true or not.  Thus, exceptions may
  //       occur even if the MOV does not take place.

//  Boolean condition;
//  Bit16u op2_16;
//

  case (i^.b1) of
    $140: condition := get_OF();
    $141: condition := not get_OF();
    $142: condition := get_CF();
    $143: condition := not get_CF();
    $144: condition := get_ZF();
    $145: condition := not get_ZF();
    $146: condition := get_CF() or get_ZF();
//    $147: condition := not get_CF() @ and not get_ZF();
    $148: condition := get_SF();
    $149: condition := not get_SF();
    $14A: condition := get_PF();
    $14B: condition := not get_PF();
    $14C: condition := word(Boolean(get_SF() <> get_OF()));
    $14D: condition := word(Boolean(get_SF() = get_OF()));
    $14E: condition := word(Boolean(Boolean(get_ZF()) or (get_SF() <> get_OF())));
//    $14F: condition := not get_ZF() @ and (get_SF() = get_OF());
  else
    begin
      condition := 0;
      LogPanic(('CMOV_GwEw: default case'));
    end;
  end;
//  switch (i^.b1) then begin
//    // CMOV opcodes:
//    case $140: condition := get_OF(); break;
//    case $141: condition := !get_OF(); break;
//    case $142: condition := get_CF(); break;
//    case $143: condition := !get_CF(); break;
//    case $144: condition := get_ZF(); break;
//    case $145: condition := !get_ZF(); break;
//    case $146: condition := get_CF() or get_ZF(); break;
//    case $147: condition := !get_CF() @ and !get_ZF(); break;
//    case $148: condition := get_SF(); break;
//    case $149: condition := !get_SF(); break;
//    case $14A: condition := get_PF(); break;
//    case $14B: condition := !get_PF(); break;
//    case $14C: condition := get_SF() !:= get_OF(); break;
//    case $14D: condition := get_SF() = get_OF(); break;
//    case $14E: condition := get_ZF() or (get_SF() !:= get_OF()); break;
//    case $14F: condition := !get_ZF() @ and (get_SF() = get_OF()); break;
//    default:
//      condition := 0;
//      BX_PANIC(('CMOV_GwEw: default case'));
//    end;

  if (i^.mod_ = $c0) then begin
    op2_16 := BX_READ_16BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, i^.rm_addr, @op2_16);
    end;

  if boolean(condition) then begin
    BX_WRITE_16BIT_REG(i^.nnn, op2_16);
    end;
{$else}
  LogPanic(('cmov_gwew called'));
{$ifend}
end;

procedure TCPU.ROL_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  count: word;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := count and $07; // use only lowest 3 bits

  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if Boolean(count) then
  begin
    result_8 := (op1_8 shl count)or(op1_8 shr (8 - count));
    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_8BIT_REG(i^.rm, result_8)
      else write_RMW_virtual_byte(result_8);
    (* set eflags:
     * ROL count affects the following flags: C*)
    set_CF(result_8 and $01);
    if (count = 1) then
      set_OF(Bool(((op1_8 or result_8) and $80) > 0));
  end;
end;

procedure TCPU.ROR_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  result_b7: Bit8u;
  count: unsigned;
begin

  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := count and $07; (* use only bottom 3 bits *)
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if Boolean(count) then
  begin
    result_8 := (op1_8 shr count)or(op1_8 shl (8 - count));
    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_8BIT_REG(i^.rm, result_8)
      else write_RMW_virtual_byte(result_8);
    (* set eflags:
     * ROR count affects the following flags: C *)
    result_b7 := result_8  and $80;

    set_CF(Bool(result_b7 <> 0));
    if (count = 1) then
      set_OF(Bool(((op1_8 or result_8) and $80) > 0));
  end;
end;

procedure TCPU.RCL_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  count: unsigned;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := (count and $1F) mod 9;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if Boolean(count) then
  begin
    result_8 := (op1_8 shl count) or
             (get_CF() shl (count - 1)) or
             (op1_8 shr (9 - count));

    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_8BIT_REG(i^.rm, result_8)
      else write_RMW_virtual_byte(result_8);
    (* set eflags:
     * RCL count affects the following flags: C *)
    if (count = 1) then
      set_OF(Bool(((op1_8 or result_8)  and $80) > 0));
    set_CF(Bool((op1_8 shr (8 - count))  and $01));
  end;
end;

procedure TCPU.RCR_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  count: unsigned;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := ( count and $1F ) mod 9;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if Boolean(count) then
  begin
    result_8 := (op1_8 shr count) or
             (get_CF() shl (8 - count)) or
             (op1_8 shl (9 - count));

    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_8BIT_REG(i^.rm, result_8)
      else write_RMW_virtual_byte(result_8);
    (* set eflags:
     * RCR count affects the following flags: C *)
    set_CF((op1_8 shr (count - 1)) and $01);
    if (count = 1) then
      set_OF(Bool(((op1_8 or result_8) and $80) > 0));
  end;
end;

procedure TCPU.SHL_Eb(I: PInstruction_tag);
var
  op1_8, result_8:Bit8u;
  count:unsigned;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := count and $1F;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if count = 0 then
    exit;

  result_8 := (op1_8 shl count);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_RMW_virtual_byte(result_8);

  SET_FLAGS_OSZAPC_8(op1_8, count, result_8, BX_INSTR_SHL8);
end;

procedure TCPU.SHR_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  count: unsigned;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
      if (i^.b1 = $d0)
        then count := 1
        else count := CL;

  count := count and $1F;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
    (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if count = 0 then
    exit;

  result_8 := (op1_8 shr count);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_RMW_virtual_byte(result_8);

  SET_FLAGS_OSZAPC_8(op1_8, count, result_8, BX_INSTR_SHR8);
end;

procedure TCPU.SAR_Eb(I: PInstruction_tag);
var
  op1_8, result_8: Bit8u;
  count: unsigned;
begin
  if (i^.b1 = $c0)
    then count := i^.Ib
    else
  if (i^.b1 = $d0)
    then count := 1
    else count := CL; // $d2

  count := count and $1F;
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_8 := BX_READ_8BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_byte(i^.seg, i^.rm_addr, @op1_8);

  if Boolean(count = 0) then
    exit;

  if (count < 8) then
  begin
    if Boolean(op1_8 and $80)
      then result_8 := (op1_8 shr count)or($ff shl (8 - count))
      else result_8 := (op1_8 shr count);
  end else
  begin
    if Boolean(op1_8 and $80)
      then result_8 := $ff
      else result_8 := 0;
  end;
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_8BIT_REG(i^.rm, result_8)
    else write_RMW_virtual_byte(result_8);
  (* set eflags:
   * SAR count affects the following flags: S,Z,P,C
   *)
  if (count < 8)
    then set_CF((op1_8 shr (count - 1))  and $01)
    else
  if Boolean(op1_8 and $80)
    then set_CF(1)
    else set_CF(0);

  set_ZF(Bool(result_8 = 0));
  set_SF(result_8 shr 7);
  if (count = 1) then
    set_OF(0);
  set_PF_base(result_8);
end;

procedure TCPU.SHLD_EdGd(I: PInstruction_tag);
var
  op1_32, op2_32, result_32: Bit32u;
  count: unsigned;
begin
  (* op1:op2 shl count.  result stored in op1 *)
  if (i^.b1 = $1a4)
    then count := i^.Ib  and $1f
    else count := CL  and $1f; // $1a5

  if (count=0) then
    exit; (* NOP *)
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
  op2_32 := BX_READ_32BIT_REG(i^.nnn);

  result_32 := (op1_32 shl count) or (op2_32 shr (32 - count));
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);
  (* set eflags:
   * SHLD count affects the following flags: S,Z,P,C,O
   *)
  set_CF((op1_32 shr (32 - count)) and $01);
  if (count = 1) then
    set_OF(Bool(((op1_32 xor result_32)  and $80000000) > 0));
  set_ZF(Bool(result_32 = 0));
  set_PF_base(Bool(result_32));
  set_SF(result_32 shr 31);
end;

procedure TCPU.SHRD_EdGd(I: PInstruction_tag);
var
  op1_32, op2_32, result_32: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $1ac)
    then count := i^.Ib  and $1f
    else count := CL  and $1f; // $1ad

  if (count = 0) then
    exit; (* NOP *)
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
  op2_32 := BX_READ_32BIT_REG(i^.nnn);
  result_32 := (op2_32 shl (32 - count))or(op1_32 shr count);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);
  (* set eflags:
   * SHRD count affects the following flags: S,Z,P,C,O
   *)
  set_CF((op1_32 shr (count - 1)) and $01);
  set_ZF(Bool(result_32 = 0));
  set_SF(result_32 shr 31);
  (* for shift of 1, OF set if sign change occurred. *)
  if (count = 1) then
    set_OF(Bool(((op1_32 xor result_32) and $80000000) > 0));
  set_PF_base(result_32);
end;

procedure TCPU.ROL_Ed(I: PInstruction_tag);
var
  op1_32, result_32: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $c1)
    then count := i^.Ib  and $1f
    else
  if (i^.b1 = $d1)
    then count := 1
    else count := CL  and $1f; // (i^.b1 = $d3)

  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  if (count) <> 0 then
  begin
    result_32 := (op1_32 shl count) or (op1_32 shr (32 - count));
    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, result_32)
      else write_RMW_virtual_dword(result_32);
    (* set eflags:
     * ROL count affects the following flags: C
     *)
    set_CF(result_32  and $01);
    if (count = 1) then
      set_OF(Bool(((op1_32 xor result_32)  and $80000000) > 0));
  end;
end;

procedure TCPU.ROR_Ed(I: PInstruction_tag);
var
  op1_32, result_32, result_b31: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $c1)
    then count := i^.Ib  and $1f
    else
  if (i^.b1 = $d1)
    then count := 1
    else count := CL  and $1f; // (i^.b1 = $d3)

  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  if (count) <> 0 then
  begin
    result_32 := (op1_32 shr count)or(op1_32 shl (32 - count));
    (* now write result back to destination *)
    if (i^.mod_ = $c0)
      then BX_WRITE_32BIT_REG(i^.rm, result_32)
      else write_RMW_virtual_dword(result_32);
    (* set eflags:
     * ROR count affects the following flags: C
     *)
    result_b31 := result_32  and $80000000;

    set_CF(Bool(result_b31 <> 0));
    if (count = 1) then
      set_OF(Bool(((op1_32 xor result_32)  and $80000000) > 0));
  end;
end;

procedure TCPU.RCL_Ed(I: PInstruction_tag);
var
  op1_32, result_32: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $c1)
    then count := i^.Ib  and $1f
    else
  if (i^.b1 = $d1)
    then count := 1
    else count := CL  and $1f; // (i^.b1 = $d3)

  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  if (count = 0) then
    exit;

  if (count = 1)
    then result_32 := (op1_32 shl 1) or get_CF()
    else result_32 := (op1_32 shl count) or
                      (get_CF() shl (count - 1)) or
                      (op1_32 shr (33 - count));

  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);
  (* set eflags:
   * RCL count affects the following flags: C
   *)
  if (count = 1) then
    set_OF(Bool(((op1_32 xor result_32)  and $80000000) > 0));
  set_CF((op1_32 shr (32 - count))  and $01);
end;

procedure TCPU.RCR_Ed(I: PInstruction_tag);
var
  op1_32, result_32:Bit32u;
  count:unsigned;
begin

  if (i^.b1 = $c1) then
    count := i^.Ib  and $1f
  else if (i^.b1 = $d1) then
    count := 1
  else // (i^.b1 = $d3)
    count := CL  and $1f;


    (* op1 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    if (count=0) then exit;

    if (count=1) then begin
      result_32 := (op1_32 shr 1)or(get_CF() shl 31);
      end
  else begin
      result_32 := (op1_32 shr count) or
                (get_CF() shl (32 - count)) or
                (op1_32 shl (33 - count));
      end;

    (* now write result back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, result_32);
      end
  else begin
      write_RMW_virtual_dword(result_32);
      end;

    (* set eflags:
     * RCR count affects the following flags: C
     *)

    set_CF((op1_32 shr (count - 1))  and $01);
    if (count = 1) then
      set_OF(Bool(((op1_32 xor result_32)  and $80000000) > 0));
end;

procedure TCPU.SHL_Ed(I: PInstruction_tag);
var
  op1_32, result_32: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $c1)
    then count := i^.Ib  and $1f
    else
  if (i^.b1 = $d1)
    then count := 1
    else count := CL  and $1f; // (i^.b1 = $d3)
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  if (count = 0) then
    exit;

  result_32 := (op1_32 shl count);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, count, result_32, BX_INSTR_SHL32);
end;

procedure TCPU.SHR_Ed(I: PInstruction_tag);
var
  op1_32, result_32: Bit32u;
  count: unsigned;
begin
  if (i^.b1 = $c1)
    then count := i^.Ib  and $1f
    else
  if (i^.b1 = $d1)
    then count := 1
    else count := CL  and $1f; // (i^.b1 = $d3)
  (* op1 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  if (count = 0) then
    exit;

  result_32 := (op1_32 shr count);
  (* now write result back to destination *)
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, result_32)
    else write_RMW_virtual_dword(result_32);

  SET_FLAGS_OSZAPC_32(op1_32, count, result_32, BX_INSTR_SHR32);
end;

procedure TCPU.SAR_Ed(I: PInstruction_tag);
var
  op1_32, result_32: Bit32u;
  count: unsigned;
begin

  if (i^.b1 = $c1) then
    count := i^.Ib  and $1f
  else if (i^.b1 = $d1) then
    count := 1
  else // (i^.b1 = $d3)
    count := CL  and $1f;

    (* op1 is a register or memory reference *)
    if (i^.mod_ = $c0) then begin
      op1_32 := BX_READ_32BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
      end;

    if (count=0) then exit;

    (* count < 32, since only lower 5 bits used *)
    if (op1_32 and $80000000)<>0 then begin
      result_32 := (op1_32 shr count) or ($ffffffff shl (32 - count));
      end
  else begin
      result_32 := (op1_32 shr count);
      end;

    (* now write result back to destination *)
    if (i^.mod_ = $c0) then begin
      BX_WRITE_32BIT_REG(i^.rm, result_32);
      end
  else begin
      write_RMW_virtual_dword(result_32);
      end;

    (* set eflags:
     * SAR count affects the following flags: S,Z,P,C
     *)

    set_CF((op1_32 shr (count - 1))  and $01);
    set_ZF(Bool(result_32 = 0));
    set_SF(result_32 shr 31);
    if (count = 1) then
      set_OF(0);
    set_PF_base(result_32);
end;

procedure TCPU.DAS(I: PInstruction_tag);
var
  tmpCF, tmpAL:Bit8u;
begin

  (* ??? *)
  (* the algorithm for DAS is fashioned after the pseudo code in the
   * Pentium Processor Family Developer's Manual, volume 3.  It seems
   * to have changed from earlier processor's manuals.  I'm not sure
   * if this is a correction in the algorithm printed, or Intel has
   * changed the handling of instruction.  It might not even be
   * correct yet...
   *)

  tmpCF := 0;
  tmpAL := AL;

  (* DAS effect the following flags: A,C,S,Z,P *)

  if (((tmpAL  and $0F) > $09) or (get_AF()<>0)) then begin
    set_AF(1);
    tmpCF := Bool(AL < $06) or get_CF();
    AL := AL - $06;
    (*tmpCF := (AL < 0) or CF;*)
    end;
  if ( (tmpAL > $99) or (get_CF()<>0) ) then begin
    AL := AL - $60;
    tmpCF := 1;
    end;

  set_CF(tmpCF);
  set_SF(AL shr 7);
  set_ZF(Bool(AL=0));
  set_PF_base(AL);
end;

procedure TCPU.AAA(I: PInstruction_tag);
var
  ALcarry:Bit8u;
begin

  ALcarry := Bool(AL > $f9);

  (* AAA effects the following flags: A,C *)
  if ( ((AL  and $0f) > 9) or (get_AF()<>0)) then begin
    AL := (AL + 6)  and $0f;
    AH := AH + 1 + ALcarry;
    set_AF(1);
    set_CF(1);
    end
  else begin
    set_AF(0);
    set_CF(0);
    AL := AL  and $0f;
    end;
end;

procedure TCPU.AAS(I: PInstruction_tag);
var
  ALborrow:Bit8u;
begin
  (* AAS affects the following flags: A,C *)
  ALborrow := Bool(AL < 6);
  if ( ((AL  and $0F) > $09) or (get_AF() <> 0) ) then
  begin
    AL := (AL - 6)  and $0f;
    AH := AH - 1 - ALborrow;
    set_AF(1);
    set_CF(1);
  end else
  begin
    set_CF(0);
    set_AF(0);
    AL := AL  and $0f;
  end;
end;

procedure TCPU.AAM(I: PInstruction_tag);
var
  al_, imm8: Bit8u;
begin
  imm8 := i^.Ib;
  al_ := AL;
  AH := al_ div imm8;
  AL := al_ mod imm8;
  (* AAM affects the following flags: S,Z,P *)
  set_SF(Bool((AH  and $80) > 0));
  set_ZF(Bool(AX = 0));
  set_PF_base(AL); (* ??? *)
end;

procedure TCPU.AAD(I: PInstruction_tag);
var
  imm8: Bit8u;
begin
  imm8 := i^.Ib;
  AL := AH * imm8 + AL;
  AH := 0;
  (* AAD effects the following flags: S,Z,P *)
  set_SF(Bool(AL >= $80));
  set_ZF(Bool(AL = 0));
  set_PF_base(AL);
end;

procedure TCPU.DAA(I: PInstruction_tag);
var
  al_: Bit8u;
begin
  al_ := AL;
  // DAA affects the following flags: S,Z,A,P,C
  // ???
  if (((al_ and $0F) > $09) or (get_AF()<>0)) then
  begin
    al_ := al_ + $06;
    set_AF(1);
  end else
    set_AF(0);

  if ((al_ > $9F) or (get_CF()<>0)) then
  begin
    al_ := al_ + $60;
    set_CF(1);
  end;
  AL := al_;
  set_SF(al_ shr 7);
  set_ZF(Bool(al_=0));
  set_PF_base(al);
end;

procedure TCPU.RETnear16_Iw(I: PInstruction_tag);
var
  imm16: Bit16u;
  temp_ESP: Bit32u;
  return_IP: Bit16u;
begin
{$if BX_DEBUGGER=01}
  self.show_flag |:= Flag_ret;
{$ifend}

  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
    then temp_ESP := ESP
    else temp_ESP := SP;

  imm16 := i^.Iw;
  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if (can_pop(2) = 0)
      then LogPanic(('retnear_iw: can''t pop IP')); (* ??? #SS(0) -or #GP(0) *)

    access_linear(self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
      2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_IP);

    if ( return_IP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled )
      then LogPanic(('retnear_iw: IP > limit'));

    if (can_pop(2 + imm16)=0 )
      then LogPanic(('retnear_iw: can''t release bytes from stack')); (* #GP(0) -or #SS(0) ??? *)

    self.FEIP := return_IP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP := ESP + 2 + imm16 (* ??? should it be 2*imm16 ? *)
      else SP  := SP  + 2 + imm16;
  end else
  begin
    pop_16(@return_IP);
    self.FEIP := return_IP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP := ESP + imm16 (* ??? should it be 2*imm16 ? *)
      else SP  := SP + imm16;
  end;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_RET, self.eip);
end;

procedure TCPU.RETnear16(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  return_IP: Bit16u;
begin
{$if BX_DEBUGGER=01}
  self.show_flag |:= Flag_ret;
{$ifend}
  invalidate_prefetch_q();

  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
    then temp_ESP := ESP
    else temp_ESP := SP;


  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if ( can_pop(2)=0 )
      then LogPanic(('retnear: can''t pop IP')); (* ??? #SS(0) -or #GP(0) *)

    access_linear(self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
      2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_IP);

    if ( return_IP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled )
      then LogPanic(('retnear: IP > limit'));

    self.FEIP := return_IP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP := ESP + 2
      else SP  := SP + 2;
  end else
  begin
    pop_16(@return_IP);
    self.FEIP := return_IP;
  end;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_RET, self.eip);
end;

procedure TCPU.RETfar16_Iw(I: PInstruction_tag);
var
  imm16: Bit16s;
  EIP, cs_raw: Bit16u;
begin
{$if BX_DEBUGGER=01}
  self.show_flag |:= Flag_ret;
{$ifend}
  (* ??? is imm16, number of bytes/words depending on operandsize ? *)
  imm16 := i^.Iw;
  invalidate_prefetch_q();
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    self.return_protected(i, imm16);
    Exit;
  end;

  pop_16(@EIP);
  pop_16(@cs_raw);
  self.FEIP := Bit32u(EIP);
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then ESP := ESP + imm16
    else SP  := SP  + imm16;
end;

procedure TCPU.RETfar16(I: PInstruction_tag);
var
  ip_, cs_raw: Bit16u;
begin
{$if BX_DEBUGGER = 01}
  self.show_flag |:= Flag_ret;
{$ifend}
  invalidate_prefetch_q();

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    self.return_protected(i, 0);
    Exit;
  end;
  pop_16(@ip_);
  pop_16(@cs_raw);
  self.FEIP := Bit32u(ip_);
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.CALL_Aw(I: PInstruction_tag);
var
  new_EIP: Bit32u;
begin
{$if BX_DEBUGGER=01}
  self.show_flag |:= Flag_call;
{$ifend}
  invalidate_prefetch_q();

  new_EIP := FEIP + Bit32s(i^.Id);
  new_EIP := new_EIP and $0000ffff;
  if ((Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) <> 0) and
     (new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled)) then
  begin
    LogPanic(('call_av: new_IP > self.sregs[BX_SEG_REG_CS].limit'));
    exception2([BX_GP_EXCEPTION, 0, 0]);
  end;
  (* push 16 bit EA of next instruction *)
  push_16(EIP);
  self.FEIP := new_EIP;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_CALL, self.eip);
end;

procedure TCPU.CALL16_Ap(I: PInstruction_tag);
var
  cs_raw: Bit16u;
  disp16: Bit16u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  disp16 := i^.Iw;
  cs_raw := i^.Iw2;
  invalidate_prefetch_q();
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    self.call_protected(i, cs_raw, disp16);
    Exit;
  end;
  push_16(self.sregs[SEG_REG_CS].selector.value);
  push_16(Bit16u(self.FEIP));
  self.FEIP := Bit32u(disp16);
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.CALL_Ew(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  op1_16: Bit16u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0)))<>0 then
  begin
    if (op1_16 > self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
    begin
      LogPanic(('call_ev: IP out of CS limits!'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
    if ( can_push(@self.sregs[SEG_REG_SS].cache, temp_ESP, 2)=0 ) then
      LogPanic(('call_ev: can''t push IP'));
  end;
  push_16(EIP);
  self.FEIP := op1_16;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_CALL, self.eip);
end;

procedure TCPU.CALL16_Ep(I: PInstruction_tag);
var
  cs_raw: Bit16u;
  op1_16: Bit16u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    LogPanic(('CALL_Ep: op1 is a register'));
  (* pointer, segment address pair *)
  read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  read_virtual_word(i^.seg, i^.rm_addr+2, @cs_raw);
  invalidate_prefetch_q();

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    self.call_protected(i, cs_raw, op1_16);
    Exit;
  end;

  push_16(self.sregs[SEG_REG_CS].selector.value);
  push_16(EIP);

  self.FEIP := op1_16;
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.JMP_Jw(I: PInstruction_tag);
var
  new_EIP: Bit32u;
begin
  invalidate_prefetch_q();

  new_EIP := FEIP + Bit32s(i^.Id);
  new_EIP := new_EIP and $0000ffff;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if ( new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
    begin
      LogPanic(('jmp_jv: offset outside of CS limits'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;

  self.FEIP := new_EIP;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_JMP, new_EIP);
end;

procedure TCPU.JCC_Jw(I: PInstruction_tag);
var
  condition: Boolean;
  new_EIP: Bit32u;
begin
  condition:=false;
  case (i^.b1 and $0f) of
    $00: begin (* JO *)   condition := get_OF() <> 0; end;
    $01: begin (* JNO *)  condition := not (get_OF() <> 0); end;
    $02: begin (* JB *)   condition := get_CF()<>0; end;
    $03: begin (* JNB *)  condition := (get_CF() = 0); end;
    $04: begin (* JZ *)   condition := get_ZF() <> 0; end;
    $05: begin (* JNZ *)  condition := not (get_ZF() <> 0); end;
    $06: begin (* JBE *)  condition := (get_CF() <> 0) or (get_ZF() <> 0); end;
    $07: begin (* JNBE *) condition := not (get_CF() <> 0) and not (get_ZF() <> 0); end;
    $08: begin (* JS *)   condition := get_SF() <> 0; end;
    $09: begin (* JNS *)  condition := not (get_SF() <> 0); end;
    $0A: begin (* JP *)   condition := (get_PF())<>0; end;
    $0B: begin (* JNP *)  condition := (get_PF() = 0); end;
    $0C: begin (* JL *)   condition := get_SF() <> get_OF(); end;
    $0D: begin (* JNL *)  condition := get_SF() = get_OF(); end;
    $0E: begin (* JLE *)  condition := (get_ZF()<>0) or (get_SF() <> get_OF()); end;
    $0F: begin (* JNLE *) condition := (get_SF() = get_OF()) and (get_ZF()=0); end;
  end;

  if (condition) then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    new_EIP := new_EIP and $0000ffff;
    if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0)))<>0 then
      if ( new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
      begin
        LogPanic(('jo_routine: offset outside of CS limits'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
      end;
    FEIP := new_EIP;
    //BX_INSTR_CNEAR_BRANCH_TAKEN(new_EIP);
    revalidate_prefetch_q();
  end;
{$if BX_INSTRUMENTATION=1}
  else
  begin
    //BX_INSTR_CNEAR_BRANCH_NOT_TAKEN();
  end;
{$ifend}
end;

procedure TCPU.JMP_Ew(I: PInstruction_tag);
var
  new_EIP: Bit32u;
  op1_16: Bit16u;
begin
  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
         (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  invalidate_prefetch_q();
  new_EIP := op1_16;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if (new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
    begin
      LogPanic(('jmp_ev: IP out of CS limits!'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
  self.FEIP := new_EIP;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_JMP, new_EIP);
end;

  (* Far indirect jump *)
procedure TCPU.JMP16_Ep(I: PInstruction_tag);
var
  cs_raw: Bit16u;
  op1_16: Bit16u;
begin

  (* op1_16 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    LogPanic(('JMP_Ep(): op1 is a register'));
    (* far indirect must specify a memory address *)

  (* pointer, segment address pair *)
  read_virtual_word(i^.seg, i^.rm_addr, @op1_16);
  read_virtual_word(i^.seg, i^.rm_addr+2, @cs_raw);
  invalidate_prefetch_q();

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    self.jump_protected(i, cs_raw, op1_16);
    exit;
  end;

  self.FEIP := op1_16;
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.IRET16(I: PInstruction_tag);
var
  ip_, cs_raw, flags: Bit16u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_iret;
  self.show_eip := self.eip;
{$ifend}
  invalidate_prefetch_q();

  if (v8086_mode()) <> 0 then
  begin
    // IOPL check in stack_return_from_v86()
    stack_return_from_v86(i);
    Exit;
  end;

  if (self.FCR0.pe) <> 0 then
  begin
    iret_protected(i);
    Exit;
  end;

  pop_16(@ip_);
  pop_16(@cs_raw);
  pop_16(@flags);

  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
  self.FEIP := Bit32u(ip_);
  write_flags(flags, (* change IOPL? *) 1, (* change IF? *) 1);
end;

procedure TCPU.RETnear32_Iw(I: PInstruction_tag);
var
  imm16: Bit16u;
  temp_ESP: Bit32u;
  return_EIP: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_ret;
{$ifend}
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
    then temp_ESP := ESP
    else temp_ESP := SP;

  imm16 := i^.Iw;
  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0)))<>0 then
  begin
    if ( can_pop(4)=0 ) then
      LogPanic(('retnear_iw: can''t pop EIP'));
      (* ??? #SS(0) -or #GP(0) *)

    access_linear(self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
      4, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_EIP);

    if ((Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))<>0) and
        (return_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled) ) then
    begin
//      BX_DEBUG(('retnear_iw: EIP > limit'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
    (* Pentium book says imm16 is number of words ??? *)
    if ( can_pop(4 + imm16)=0 ) then
      LogPanic(('retnear_iw: can''t release bytes from stack'));
      (* #GP(0) -or #SS(0) ??? *)

    self.FEIP := return_EIP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP := ESP + 4 + imm16 (* ??? should it be 2*imm16 ? *)
      else SP := SP + 4 + imm16;
  end else
  begin
    pop_32(@return_EIP);
    self.FEIP := return_EIP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP := ESP + imm16 (* ??? should it be 2*imm16 ? *)
      else SP := SP + imm16;
  end;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_RET, self.eip);
end;

procedure TCPU.RETnear32(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  return_EIP: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_ret;
{$ifend}
  invalidate_prefetch_q();

  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
    then temp_ESP := ESP
    else temp_ESP := SP;


  if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0)))<>0 then
  begin
    if ( can_pop(4)=0 ) then
      LogPanic(('retnear: can''t pop EIP'));
      (* ??? #SS(0) -or #GP(0) *)

    access_linear(self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
      4, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_EIP);

    if ( return_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
    begin
      LogPanic(('retnear: EIP > limit'));
      exception(BX_GP_EXCEPTION, 0, 0);
    end;
    self.FEIP := return_EIP;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 (* 32bit stack *)
      then ESP:=ESP + 4
      else SP:=SP + 4;
  end else
  begin
    pop_32(@return_EIP);
    self.FEIP := return_EIP;
  end;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_RET, self.eip);
end;

procedure TCPU.RETfar32_Iw(I: PInstruction_tag);
var
  _eip,
  _ecs_raw: Bit32u;
  _imm16: Bit16s;
begin

{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_ret;
{$ifend}
  (* ??? is imm16, number of bytes/words depending on operandsize ? *)

  _imm16 := i^.Iw;

  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    self.return_protected(i, _imm16);
    Exit;
  end;

  pop_32(@_eip);
  pop_32(@_ecs_raw);
  self.FEIP := _eip;
  load_seg_reg(@self.sregs[SEG_REG_CS], Bit16u(_ecs_raw));
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then ESP := ESP + _imm16
    else SP  := SP + _imm16;
end;

procedure TCPU.RETfar32(I: PInstruction_tag);
var
  _eip,
  _ecs_raw: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_ret;
{$ifend}
  invalidate_prefetch_q();
  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) )<>0 then
  begin
    self.return_protected(i, 0);
    Exit;
  end;
  pop_32(@_eip);
  pop_32(@_ecs_raw); (* 32bit pop, MSW discarded *)
  self.FEIP := _eip;
  load_seg_reg(@self.sregs[SEG_REG_CS], Bit16u(_ecs_raw));
end;

procedure TCPU.CALL_Ad(I: PInstruction_tag);
var
  new_EIP: Bit32u;
  disp32: Bit32s;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  disp32 := i^.Id;
  invalidate_prefetch_q();
  new_EIP := FEIP + disp32;

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
    if ( new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
    begin
      LogPanic(('call_av: offset outside of CS limits'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
  (* push 32 bit EA of next instruction *)
  push_32(self.FEIP);
  self.FEIP := new_EIP;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_CALL, self.eip);
end;

procedure TCPU.CALL32_Ap(I: PInstruction_tag);
var
  cs_raw: Bit16u;
  disp32: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  disp32 := i^.Id;
  cs_raw := i^.Iw2;
  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    self.call_protected(i, cs_raw, disp32);
    Exit;
  end;
  push_32(self.sregs[SEG_REG_CS].selector.value);
  push_32(self.FEIP);
  self.FEIP := disp32;
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.CALL_Ed(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  op1_32: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  invalidate_prefetch_q();

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if (op1_32 > self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
    begin
//      BX_DEBUG(('call_ev: EIP out of CS limits! at %s:%d'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
    if ( can_push(@self.sregs[SEG_REG_SS].cache, temp_ESP, 4) = 0 ) then
      LogPanic(('call_ev: can''t push EIP'));
  end;
  push_32(self.FEIP);
  self.FEIP := op1_32;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_CALL, self.eip);
end;

procedure TCPU.CALL32_Ep(I: PInstruction_tag);
var
  cs_raw: Bit16u;
  op1_32: Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_call;
{$ifend}
    (* op1_32 is a register or memory reference *)
    if (i^.mod_ = $c0) then
      LogPanic(('CALL_Ep: op1 is a register'));
  (* pointer, segment address pair *)
  read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
  read_virtual_word(i^.seg, i^.rm_addr+4, @cs_raw);
  invalidate_prefetch_q();

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    self.call_protected(i, cs_raw, op1_32);
    Exit;
  end;

  push_32(self.sregs[SEG_REG_CS].selector.value);
  push_32(self.FEIP);

  self.FEIP := op1_32;
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.JMP_Jd(I: PInstruction_tag);
var
  new_EIP: Bit32u;
begin
  invalidate_prefetch_q();
  new_EIP := FEIP + Bit32s(i^.Id);

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if ( new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
    begin
      LogPanic(('jmp_jv: offset outside of CS limits'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;

  self.FEIP := new_EIP;
  //BX_INSTR_UCNEAR_BRANCH(BX_INSTR_IS_JMP, new_EIP);
end;

procedure TCPU.JCC_Jd(I: PInstruction_tag);
var
  condition: Bool;
  new_EIP: Bit32u;
begin
  condition := 0;
  case (i^.b1 and $0f) of
    $00: (* JO *)   begin condition := get_OF(); end;
    $01: (* JNO *)  begin condition := Bool(get_OF() = 0); end;
    $02: (* JB *)   begin condition := Bool(get_CF() <> 0); end;
    $03: (* JNB *)  begin condition := Bool(get_CF() = 0); end;
    $04: (* JZ *)   begin condition := Bool(get_ZF() <> 0); end;
    $05: (* JNZ *)  begin condition := Bool(get_ZF() = 0); end;
    $06: (* JBE *)  begin condition := Bool(get_CF() or get_ZF()); end;
    $07: (* JNBE *) begin condition := Bool(((get_CF() = 0) and (get_ZF() = 0))); end;
    $08: (* JS *)   begin condition := Bool(get_SF() <> 0); end;
    $09: (* JNS *)  begin condition := Bool(get_SF() = 0); end;
    $0A: (* JP *)   begin condition := Bool(get_PF() <> 0); end;
    $0B: (* JNP *)  begin condition := Bool(get_PF() = 0); end;
    $0C: (* JL *)   begin condition := Bool((get_SF() <> get_OF())); end;
    $0D: (* JNL *)  begin condition := Bool((get_SF() = get_OF())); end;
    $0E: (* JLE *)  begin condition := Bool(((get_ZF() <> 0) or (get_SF() <> get_OF()))); end;
    $0F: (* JNLE *) begin condition := Bool(((get_SF() = get_OF()) and (get_ZF() = 0))); end;
    end;

  if (condition)<>0 then
  begin
    new_EIP := FEIP + Bit32s(i^.Id);
    if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
      if ( new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled ) then
      begin
        LogPanic(('jo_routine: offset outside of CS limits'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
      end;
    FEIP := new_EIP;
    revalidate_prefetch_q();
  end;
end;

procedure TCPU.JMP_Ap(I: PInstruction_tag);
var
  disp32: Bit32u;
  cs_raw: Bit16u;
begin
  invalidate_prefetch_q();
  if (i^.os_32) <> 0
    then disp32 := i^.Id
    else disp32 := i^.Iw;
  cs_raw := i^.Iw2;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    self.jump_protected(i, cs_raw, disp32);
    Exit;
  end;

  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
  self.FEIP := disp32;
end;

procedure TCPU.JMP_Ed(I: PInstruction_tag);
var
  new_EIP: Bit32u;
  op1_32: Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  invalidate_prefetch_q();
  new_EIP := op1_32;

  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if (new_EIP > self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
    begin
      LogPanic(('jmp_ev: IP out of CS limits!'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;
  self.FEIP := new_EIP;
end;
  (* Far indirect jump *)
procedure TCPU.JMP32_Ep(I: PInstruction_tag);
var
  cs_raw:Bit16u;
  op1_32:Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if (i^.mod_ = $c0) then
    (* far indirect must specify a memory address *)
    LogPanic(('JMP_Ep(): op1 is a register'));

  (* pointer, segment address pair *)
  read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);
  read_virtual_word(i^.seg, i^.rm_addr + 4, @cs_raw);
  invalidate_prefetch_q();

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    self.jump_protected(i, cs_raw, op1_32);
    Exit;
  end;

  self.FEIP := op1_32;
  load_seg_reg(@self.sregs[SEG_REG_CS], cs_raw);
end;

procedure TCPU.IRET32(I: PInstruction_tag);
var
  _eip,
  _ecs_raw,
  _eflags:Bit32u;
begin
{$if BX_DEBUGGER=1}
  self.show_flag |:= Flag_iret;
  self.show_eip := self._eip;
{$ifend}
  invalidate_prefetch_q();

  if (v8086_mode()) <> 0 then
  begin
    // IOPL check in stack_return_from_v86()
    stack_return_from_v86(i);
    Exit;
  end;

  if (self.FCR0.pe) <> 0 then
  begin
    iret_protected(i);
    Exit;
  end;

  LogError(('IRET32 called when you''re not in vm8086 mod_e or protected mod_e.'));
  LogError(('IRET32 may not be implemented right, since it doesn''t check anything.'));
  LogPanic(('Please report that you have found a test case for BX_CPU_C.IRET32.'));

  pop_32(@_eip);
  pop_32(@_ecs_raw);
  pop_32(@_eflags);

  load_seg_reg(@self.sregs[SEG_REG_CS], Bit16u(_ecs_raw));
  self.FEIP := _eip;
  //FIXME: this should do (eflags  and $257FD5)or(EFLAGSor$1A0000)
  write_eflags(_eflags, (* change IOPL? *) 1, (* change IF? *) 1, 0, 1);
end;

procedure TCPU.jump_protected(Istr:PInstruction_tag; cs_raw:Bit16u; disp32:Bit32u);
var
  descriptor: TDescriptor_t;
  selector: TSelector_t;
  dword1, dword2: Bit32u;

  raw_tss_selector: Bit16u;
  tss_selector, gate_cs_selector: TSelector_t;
  tss_descriptor, gate_cs_descriptor: TDescriptor_t;
  gate_cs_raw: Bit16u;
  temp_eIP: Bit32u;
begin
  (* destination selector is not null else #GP(0) *)
  if ((cs_raw and $fffc) = 0) then
  begin
    LogPanic(('jump_protected: cs = 0'));
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
  end;

  parse_selector(cs_raw, @selector);
  (* destination selector index is whithin its descriptor table
     limits else #GP(selector) *)
  fetch_raw_descriptor(@selector, @dword1, @dword2, BX_GP_EXCEPTION);
  (* examine AR byte of destination selector for legal values: *)
  parse_descriptor(dword1, dword2, @descriptor);

  if ( descriptor.segmentType )<>0 then
  begin
    if ( descriptor.segment.executable = 0 ) then
    begin
      LogError(('jump_protected: S:=1: descriptor not executable'));
      exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
      exit;
    end;
    // CASE: JUMP CONFORMING CODE SEGMENT:
    if ( descriptor.segment.c_ed ) <> 0 then
    begin
      // descripor DPL must be <= CPL else #GP(selector)
      if (descriptor.dpl > CPL) then
      begin
        LogError(('jump_protected: dpl > CPL'));
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
      (* segment must be PRESENT else #NP(selector) *)
      if (descriptor.p = 0) then
      begin
        LogError(('jump_protected: p = 0'));
        exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
      (* instruction pointer must be in code segment limit else #GP(0) *)
      if (disp32 > descriptor.segment.limit_scaled) then
      begin
        LogPanic(('jump_protected: IP > limit'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
      end;
      (* Load CS:IP from destination pointer *)
      (* Load CS-cache with new segment descriptor *)
      (* CPL does not change for conforming code segment *)
      load_cs(@selector, @descriptor, CPL);
      Self.FEIP := disp32;
      exit;
    end else
    // CASE: jump nonconforming code segment:
    begin
      (* RPL of destination selector must be <= CPL else #GP(selector) *)
      if (selector.rpl > CPL) then
      begin
        LogPanic(('jump_protected: rpl > CPL'));
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
      // descriptor DPL must := CPL else #GP(selector)
      if (descriptor.dpl <> CPL) then
      begin
        LogError(('jump_protected: dpl !:= CPL'));
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
      (* segment must be PRESENT else #NP(selector) *)
      if (descriptor.p = 0) then
      begin
        LogError(('jump_protected: p = 0'));
        exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
      (* IP must be in code segment limit else #GP(0) *)
      if (disp32 > descriptor.segment.limit_scaled) then
      begin
        LogPanic(('jump_protected: IP > limit'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
      end;
      (* load CS:IP from destination pointer *)
      (* load CS-cache with new segment descriptor *)
      (* set RPL field of CS register to CPL *)
      load_cs(@selector, @descriptor, CPL);
      Self.FEIP := disp32;
      exit;
    end;
    LogPanic(('jump_protected: segment:=1'));
  end else
  begin
    case descriptor.type_ of
      1, // 286 available TSS
      9: // 386 available TSS
      begin
        //if ( descriptor.type=1 )
        //  BX_INFO(('jump to 286 TSS'));
        //else
        //  BX_INFO(('jump to 386 TSS'));

        // TSS DPL must be >= CPL, else #GP(TSS selector)
        if (descriptor.dpl < CPL) then
        begin
          LogPanic(('jump_protected: TSS.dpl < CPL'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // TSS DPL must be >= TSS selector RPL, else #GP(TSS selector)
        if (descriptor.dpl < selector.rpl) then
        begin
          LogPanic(('jump_protected: TSS.dpl < selector.rpl'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // descriptor AR byte must specify available TSS,
        //   else #GP(TSS selector) *)
        // this is taken care of by the 'default' case of switch statement *)

        // Task State Seg must be present, else #NP(TSS selector)
        // checked in task_switch()

        // SWITCH_TASKS _without_ nesting to TSS
        task_switch(@selector, @descriptor, BX_TASK_FROM_JUMP, dword1, dword2);

        // IP must be in code seg limit, else #GP(0)
        if (FEIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        begin
          LogError(('jump_protected: TSS.p = 0'));
          exception2([BX_GP_EXCEPTION, 0, 0]);
          exit;
        end;
        exit;
      end;

      3: // Busy 286 TSS
      begin
        LogPanic(('jump_protected: JUMP to busy 286 TSS unsupported.'));
        exit;
      end;

      4: // 286 call gate
      begin
        LogError(('jump_protected: JUMP TO 286 CALL GATE:'));
        // descriptor DPL must be >= CPL else #GP(gate selector)
        if (descriptor.dpl < CPL) then
        begin
          LogError(('jump_protected: gate.dpl < CPL'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // descriptor DPL must be >= gate selector RPL else #GP(gate selector)
        if (descriptor.dpl < selector.rpl) then
        begin
          LogError(('jump_protected: gate.dpl < selector.rpl'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // gate must be present else #NP(gate selector)
        if (descriptor.p = 0) then
        begin
          LogPanic(('jump_protected: task gate.p = 0'));
          exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // examine selector to code segment given in call gate descriptor
        // selector must not be null, else #GP(0)
        gate_cs_raw := descriptor.gate286.dest_selector;
        if ( (gate_cs_raw  and $fffc) = 0 ) then
        begin
          LogPanic(('jump_protected: CS selector null'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
        parse_selector(gate_cs_raw, @gate_cs_selector);
        // selector must be within its descriptor table limits else #GP(CS selector)
        fetch_raw_descriptor(@gate_cs_selector, @dword1, @dword2, BX_GP_EXCEPTION);
        parse_descriptor(dword1, dword2, @gate_cs_descriptor);
        // descriptor AR byte must indicate code segment else #GP(CS selector)
        if ( (gate_cs_descriptor.valid=0) or (gate_cs_descriptor.segmentType=0) or
             (gate_cs_descriptor.segment.executable=0) ) then
        begin
          LogError(('jump_protected: AR byte: not code segment.'));
          exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
        end;
        // if non-conforming, code segment descriptor DPL must := CPL else #GP(CS selector)
        if (gate_cs_descriptor.segment.c_ed=0) then
        begin
          if (gate_cs_descriptor.dpl <> CPL) then
          begin
            LogError(('jump_protected: non-conform: code seg des DPL !:= CPL.'));
            exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
          end;
        end else
        // if conforming, then code segment descriptor DPL must <= CPL else #GP(CS selector)
        begin
          if (gate_cs_descriptor.dpl > CPL) then
          begin
            LogError(('jump_protected: conform: code seg des DPL > CPL.'));
            exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
          end;
        end;
        // code segment must be present else #NP(CS selector)
        if (gate_cs_descriptor.p = 0) then
        begin
          LogError(('jump_protected: code seg not present.'));
          exception2([BX_NP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
        end;
        // IP must be in code segment limit else #GP(0)
        if ( descriptor.gate286.dest_offset >
             gate_cs_descriptor.segment.limit_scaled ) then
        begin
          LogPanic(('jump_protected: IP > limit'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
        // load CS:IP from call gate
        // load CS cache with new code segment
        // set rpl of CS to CPL
        load_cs(@gate_cs_selector, @gate_cs_descriptor, CPL);
        FEIP := descriptor.gate286.dest_offset;
        exit;
      end;

      5: // task gate
      begin
        // gate descriptor DPL must be >= CPL else #GP(gate selector)
        if (descriptor.dpl < CPL) then
        begin
          LogPanic(('jump_protected: gate.dpl < CPL'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // gate descriptor DPL must be >= gate selector RPL
        //   else #GP(gate selector)
        if (descriptor.dpl < selector.rpl) then
        begin
          LogPanic(('jump_protected: gate.dpl < selector.rpl'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // task gate must be present else #NP(gate selector)
        if (descriptor.p=0) then
        begin
          LogPanic(('jump_protected: task gate.p = 0'));
          exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // examine selector to TSS, given in Task Gate descriptor
        // must specify global in the local/global bit else #GP(TSS selector)
        raw_tss_selector := descriptor.taskgate.tss_selector;
        parse_selector(raw_tss_selector, @tss_selector);
        if (tss_selector.ti) <> 0 then
        begin
          LogPanic(('jump_protected: tss_selector.ti:=1'));
          exception2([BX_GP_EXCEPTION, raw_tss_selector  and $fffc, 0]);
          exit;
        end;
        // index must be within GDT limits else #GP(TSS selector)
        fetch_raw_descriptor(@tss_selector, @dword1, @dword2,
          BX_GP_EXCEPTION);
        // descriptor AR byte must specify available TSS
        //   else #GP(TSS selector)
        parse_descriptor(dword1, dword2, @tss_descriptor);
        if (tss_descriptor.valid = 0 or tss_descriptor.segmentType) then
        begin
          LogError(('jump_protected: TSS selector points to bad TSS'));
          exception2([BX_GP_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        end;
        if ((tss_descriptor.type_ <> 9) and (tss_descriptor.type_ <> 1)) then
        begin
          LogError(('jump_protected: TSS selector points to bad TSS'));
          exception2([BX_GP_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        end;
        // task state segment must be present, else #NP(tss selector)
        if (tss_descriptor.p=0) then
        begin
          LogPanic(('jump_protected: task descriptor.p = 0'));
          exception2([BX_NP_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        end;
        // SWITCH_TASKS _without_ nesting to TSS
        task_switch(@tss_selector, @tss_descriptor,
                    BX_TASK_FROM_JUMP, dword1, dword2);
        // eIP must be within code segment limit, else #GP(0)
        if (Self.sregs[SEG_REG_CS].cache.segment.d_b) <> 0 then
          temp_eIP := FEIP
        else
          temp_eIP :=  EIP;

        if (temp_eIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        begin
          LogPanic(('jump_protected: eIP > cs.limit'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
      end;

      11: // Busy 386 TSS
      begin
        LogPanic(('jump_protected: JUMP to busy 386 TSS unsupported.'));
        exit;
      end;

      12: // 386 call gate
      begin
        // descriptor DPL must be >= CPL else #GP(gate selector)
        if (descriptor.dpl < CPL) then
        begin
          LogPanic(('jump_protected: gate.dpl < CPL'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // descriptor DPL must be >= gate selector RPL else #GP(gate selector)
        if (descriptor.dpl < selector.rpl) then
        begin
          LogPanic(('jump_protected: gate.dpl < selector.rpl'));
          exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // gate must be present else #NP(gate selector)
        if (descriptor.p=0) then
        begin
          LogPanic(('jump_protected: task gate.p = 0'));
          exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // examine selector to code segment given in call gate descriptor
        // selector must not be null, else #GP(0)
        gate_cs_raw := descriptor.gate386.dest_selector;
        if ( (gate_cs_raw  and $fffc) = 0 ) then
        begin
          LogPanic(('jump_protected: CS selector null'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
        parse_selector(gate_cs_raw, @gate_cs_selector);
        // selector must be within its descriptor table limits else #GP(CS selector)
        fetch_raw_descriptor(@gate_cs_selector, @dword1, @dword2, BX_GP_EXCEPTION);
        parse_descriptor(dword1, dword2, @gate_cs_descriptor);
        // descriptor AR byte must indicate code segment else #GP(CS selector)
        if ( (gate_cs_descriptor.valid = 0) or
             (gate_cs_descriptor.segmentType = 0) or
             (gate_cs_descriptor.segment.executable = 0) ) then
        begin
          LogPanic(('jump_protected: AR byte: not code segment.'));
          exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
        end;
        // if non-conforming, code segment descriptor DPL must := CPL else #GP(CS selector)
        if (gate_cs_descriptor.segment.c_ed=0) then
        begin
          if (gate_cs_descriptor.dpl <> CPL) then
          begin
            LogPanic(('jump_protected: non-conform: code seg des DPL !:= CPL.'));
            exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
          end;
        end else
        // if conforming, then code segment descriptor DPL must <= CPL else #GP(CS selector)
        begin
          if (gate_cs_descriptor.dpl > CPL) then
          begin
            LogPanic(('jump_protected: conform: code seg des DPL > CPL.'));
            exception2([BX_GP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
          end;
        end;
        // code segment must be present else #NP(CS selector)
        if (gate_cs_descriptor.p = 0) then
        begin
          LogPanic(('jump_protected: code seg not present.'));
          exception2([BX_NP_EXCEPTION, gate_cs_raw  and $fffc, 0]);
        end;
        // IP must be in code segment limit else #GP(0)
        if ( descriptor.gate386.dest_offset >
             gate_cs_descriptor.segment.limit_scaled ) then
        begin
          LogPanic(('jump_protected: IP > limit'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
        // load CS:IP from call gate
        // load CS cache with new code segment
        // set rpl of CS to CPL
        load_cs(@gate_cs_selector, @gate_cs_descriptor, CPL);
        FEIP := descriptor.gate386.dest_offset;
        exit;
      end;
      else
      begin
        LogError(Format('jump_protected: gate type %u unsupported', [descriptor.type_]));
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
    end;
  end;
end;

procedure TCPU.call_protected(I:PInstruction_tag; cs_raw:Bit16u; disp32:Bit32u);
var
  cs_selector: TSelector_t;
  dword1, dword2: Bit32u;
  cs_descriptor: TDescriptor_t;
  temp_ESP: Bit32u;
  gate_descriptor: TDescriptor_t;
  gate_selector: TSelector_t;
  new_EIP: Bit32u;
  dest_selector: Bit16u;
  raw_tss_selector: Bit16u;
  tss_selector: TSelector_t;
  tss_descriptor: TDescriptor_t;
  temp_eIP: Bit32u;

  SS_for_cpl_x: Bit16u ;
  ESP_for_cpl_x: Bit32u;
  ss_selector: TSelector_t;
  ss_descriptor: TDescriptor_t;
  room_needed: unsigned;
  param_count: Bit8u;
  return_SS, return_CS: Bit16u;
  return_ESP, return_EIP: Bit32u;
  return_ss_base: Bit32u;
  parameter_word: array[0..32] of Bit16u;
  parameter_dword: array[0..32] of Bit32u;
  i_: Bit8u;
begin
  (* Opsize in effect for CALL is specified by the D bit for the
   * segment containing dest  and by any opsize prefix.
   * For gate descriptor, deterermined by type of call gate:
   * 4:=16bit, 12:=32bit
   * count field: 16bit specifies #words, 32bit specifies #dwords
   *)
  (* new cs selector must not be null, else #GP(0) *)
  if ( (cs_raw  and $fffc) = 0 ) then
    exception2([BX_GP_EXCEPTION, 0, 0]);
  parse_selector(cs_raw, @cs_selector);
  // check new CS selector index within its descriptor limits,
  // else #GP(new CS selector)
  fetch_raw_descriptor(@cs_selector, @dword1, @dword2, BX_GP_EXCEPTION);
  parse_descriptor(dword1, dword2, @cs_descriptor);
  // examine AR byte of selected descriptor for various legal values
  if (cs_descriptor.valid = 0) then
    exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);

  if (cs_descriptor.segmentType) <> 0 then // normal segment
  begin
    if (cs_descriptor.segment.executable = 0) then
    begin
      exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
      exit;
    end;

    if (cs_descriptor.segment.c_ed) <> 0 then // conforming code segment
    begin
      // DPL must be <= CPL, else #GP(code seg selector)
      if (cs_descriptor.dpl > CPL) then
      begin
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
        exit;
      end;
    end else
    begin // non-conforming code segment
      // RPL must be <= CPL, else #GP(code seg selector)
      // DPL must be := CPL, else #GP(code seg selector)
      if ( (cs_selector.rpl > CPL) or
           (cs_descriptor.dpl <> CPL) ) then
        exception2([BX_GP_EXCEPTION, cs_raw  and $fffc, 0]);
    end;
    // segment must be present, else #NP(code seg selector)
    if (cs_descriptor.p = 0) then
      exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);

    if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
      then temp_ESP := ESP
      else temp_ESP := SP;
    // stack must be big enough for return addr, else #SS(0)
    if (i^.os_32) <> 0 then
    begin
      if ( can_push(@Self.sregs[SEG_REG_SS].cache, temp_ESP, 8) = 0 ) then
        exception2([BX_SS_EXCEPTION, 0, 0]);
      // IP must be in code seg limit, else #GP(0)
      if (disp32 > cs_descriptor.segment.limit_scaled) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
      // push return address onto stack (CS padded to 32bits)
      push_32(Bit32u(Self.sregs[SEG_REG_CS].selector.value));
      push_32(FEIP);
    end else
    begin // 16bit opsize
      if ( can_push(@Self.sregs[SEG_REG_SS].cache, temp_ESP, 4) = 0 ) then
        exception2([BX_SS_EXCEPTION, 0, 0]);
      // IP must be in code seg limit, else #GP(0)
      if (disp32 > cs_descriptor.segment.limit_scaled) then
        exception2([BX_GP_EXCEPTION, 0, 0]);
      push_16(Self.sregs[SEG_REG_CS].selector.value);
      push_16(EIP);
    end;
    // load code segment descriptor into CS cache
    // load CS with new code segment selector
    // set RPL of CS to CPL
    // load eIP with new offset
    load_cs(@cs_selector, @cs_descriptor, CPL);
    Self.FEIP := disp32;
    if (cs_descriptor.segment.d_b = 0) then
      Self.FEIP := Self.FEIP and $0000ffff;
    exit;
  end else
  begin // gate  and special segment
    (* 1 level of indirection via gate, switch gate  and cs *)
    gate_descriptor := cs_descriptor;
    gate_selector   := cs_selector;
    case gate_descriptor.type_ of
      1, // available 16bit TSS
      9: // available 32bit TSS
        //if (gate_descriptor.type=1)
        //  BX_INFO(('call_protected: 16bit available TSS'));
        //else
        //  BX_INFO(('call_protected: 32bit available TSS'));

        // TSS DPL must be >= CPL, else #TS(TSS selector)
      begin
        if (gate_descriptor.dpl < CPL) then
        begin
          exception2([BX_TS_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // TSS DPL must be >= TSS selector RPL, else #TS(TSS selector)
        if (gate_descriptor.dpl < gate_selector.rpl) then
        begin
          exception2([BX_TS_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // descriptor AR byte must specify available TSS,
        //   else #TS(TSS selector) *)
        // this is taken care of by the 'default' case of switch statement *)

        // Task State Seg must be present, else #NP(TSS selector)
        // checked in task_switch()

        // SWITCH_TASKS _without_ nesting to TSS
        task_switch(@gate_selector, @gate_descriptor,
          BX_TASK_FROM_CALL_OR_INT, dword1, dword2);

        // IP must be in code seg limit, else #TS(0)
        if (FEIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        begin
//          BX_INFO(('call_protected: TSS.p = 0'));
          exception2([BX_TS_EXCEPTION, 0, 0]);
          exit;
        end;
        exit;
      end;

      5: // TASK GATE
      begin
        //BX_INFO(('call_protected: task gate'));
        // gate descriptor DPL must be >= CPL else #TS(gate selector)
        if (gate_descriptor.dpl < CPL) then
        begin
          exception2([BX_TS_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // gate descriptor DPL must be >= gate selector RPL
        //   else #TS(gate selector)
        if (gate_descriptor.dpl < gate_selector.rpl) then
        begin
          exception2([BX_TS_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // task gate must be present else #NP(gate selector)
        if (gate_descriptor.p = 0) then
        begin
          exception2([BX_NP_EXCEPTION, cs_raw  and $fffc, 0]);
          exit;
        end;
        // examine selector to TSS, given in Task Gate descriptor
        // must specify global in the local/global bit else #TS(TSS selector)
        raw_tss_selector := gate_descriptor.taskgate.tss_selector;
        parse_selector(raw_tss_selector, @tss_selector);
        if (tss_selector.ti) <> 0 then
        begin
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
          exit;
        end;
        // index must be within GDT limits else #TS(TSS selector)
        fetch_raw_descriptor(@tss_selector, @dword1, @dword2, BX_TS_EXCEPTION);
        // descriptor AR byte must specify available TSS
        //   else #TS(TSS selector)
        parse_descriptor(dword1, dword2, @tss_descriptor);
        if ((tss_descriptor.valid = 0) or (tss_descriptor.segmentType <> 0)) then
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        if ((tss_descriptor.type_ <> 9) and (tss_descriptor.type_ <> 1)) then
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        // task state segment must be present, else #NP(tss selector)
        if (tss_descriptor.p = 0) then
          exception2([BX_NP_EXCEPTION, raw_tss_selector  and $fffc, 0]);
        // SWITCH_TASKS without nesting to TSS
        task_switch(@tss_selector, @tss_descriptor, BX_TASK_FROM_CALL_OR_INT, dword1, dword2);
        // eIP must be within code segment limit, else #TS(0)
        if (Self.sregs[SEG_REG_CS].cache.segment.d_b) <> 0
          then temp_eIP := FEIP
          else temp_eIP :=  EIP;

        if (temp_eIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
          exception2([BX_TS_EXCEPTION, $0000, 0]);
        exit;
      end;

      4, // 16bit CALL GATE
      12: // 32bit CALL GATE
//if (gate_descriptor.type=4)
//  BX_INFO(('CALL: 16bit call gate'));
//else
//  BX_INFO(('CALL: 32bit call gate'));

        // call gate DPL must be >= CPL, else #GP(call gate selector)
        // call gate DPL must be >= RPL, else #GP(call gate selector)
      begin
        if ( (gate_descriptor.dpl < CPL) or
             (gate_descriptor.dpl < gate_selector.rpl) ) then
          exception2([BX_GP_EXCEPTION, gate_selector.value  and $fffc, 0]);

        // call gate must be present, else #NP(call gate selector)
        if (gate_descriptor.p = 0) then
          exception2([BX_NP_EXCEPTION, gate_selector.value  and $fffc, 0]);

        // examine code segment selector in call gate descriptor

        if (gate_descriptor.type_ = 4) then
        begin
          dest_selector := gate_descriptor.gate286.dest_selector;
          new_EIP := gate_descriptor.gate286.dest_offset;
        end else
        begin
          dest_selector := gate_descriptor.gate386.dest_selector;
          new_EIP := gate_descriptor.gate386.dest_offset;
        end;
        // selector must not be null else #GP(0)
        if ( (dest_selector  and $fffc) = 0 ) then
          exception2([BX_GP_EXCEPTION, 0, 0]);

        parse_selector(dest_selector, @cs_selector);

        // selector must be within its descriptor table limits,
        //   else #GP(code segment selector)
        fetch_raw_descriptor(@cs_selector, @dword1, @dword2,
          BX_GP_EXCEPTION);
        parse_descriptor(dword1, dword2, @cs_descriptor);

        // AR byte of selected descriptor must indicate code segment,
        //   else #GP(code segment selector)
        // DPL of selected descriptor must be <= CPL,
        // else #GP(code segment selector)
        if ((cs_descriptor.valid=0) or (cs_descriptor.segmentType=0) or (cs_descriptor.segment.executable=0) or
            (cs_descriptor.dpl > CPL)) then
          exception2([BX_GP_EXCEPTION, cs_selector.value  and $fffc, 0]);

        // CALL GATE TO MORE PRIVILEGE
        // if non-conforming code segment and DPL < CPL then
        // ??? use gate_descriptor.dpl or cs_descriptor.dpl ???
        if ( (cs_descriptor.segment.c_ed = 0)  and (cs_descriptor.dpl < CPL) ) then
        begin
          // get new SS selector for new privilege level from TSS
          get_SS_ESP_from_TSS(cs_descriptor.dpl,
                              @SS_for_cpl_x, @ESP_for_cpl_x);

(* ??? use dpl or rpl ??? *)

          // check selector  and descriptor for new SS:
          // selector must not be null, else #TS(0)
          if ( (SS_for_cpl_x  and $fffc) = 0 ) then
          begin
            LogPanic(('call_protected: new SS null'));
            exception2([BX_TS_EXCEPTION, 0, 0]);
            exit;
          end;
          // selector index must be within its descriptor table limits,
          //   else #TS(SS selector)
          parse_selector(SS_for_cpl_x, @ss_selector);
          fetch_raw_descriptor(@ss_selector, @dword1, @dword2,
            BX_TS_EXCEPTION);

          parse_descriptor(dword1, dword2, @ss_descriptor);

          // selector's RPL must equal DPL of code segment,
          //   else #TS(SS selector)
          if (ss_selector.rpl <> cs_descriptor.dpl) then
          begin
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
            exit;
          end;

          // stack segment DPL must equal DPL of code segment,
          //   else #TS(SS selector)
          if (ss_descriptor.dpl <> cs_descriptor.dpl) then
          begin
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
            exit;
          end;

          // descriptor must indicate writable data segment,
          //   else #TS(SS selector)
          if ((ss_descriptor.valid = 0) or (ss_descriptor.segmentType = 0) or
              (ss_descriptor.segment.executable <> 0) or (ss_descriptor.segment.r_w = 0)) then
          begin
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
            exit;
          end;

          // segment must be present, else #SS(SS selector)
          if (ss_descriptor.p = 0) then
          begin
            exception2([BX_SS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
            exit;
          end;

          room_needed := 16 shr Byte(( cs_descriptor.segment.d_b ) = 0);
//          if ( cs_descriptor.segment.d_b ) <> 0
//            // new stack must have room for parameters plus 16 bytes
//            then room_needed := 16
//            // new stack must have room for parameters plus 8 bytes
//            else room_needed :=  8;

          if (gate_descriptor.type_ = 4) then
          begin
            // get word count from call gate, mask to 5 bits
            param_count := gate_descriptor.gate286.word_count  and $1f;
            room_needed := room_needed + param_count * 2;
          end else
          begin
            // get word count from call gate, mask to 5 bits
            param_count := gate_descriptor.gate386.dword_count  and $1f;
            room_needed := room_needed + param_count * 4;
          end;

          // new stack must have room for parameters plus return info
          //   else #SS(SS selector)

          if ( can_push(@ss_descriptor, ESP_for_cpl_x, room_needed) = 0 ) then
          begin
            exception2([BX_SS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
            exit;
          end;

          // new eIP must be in code segment limit else #GP(0)
          if ( new_EIP > cs_descriptor.segment.limit_scaled ) then
          begin
            exception2([BX_GP_EXCEPTION, 0, 0]);
            exit;
          end;

          // save return SS:eSP to be pushed on new stack
          return_SS := Self.sregs[SEG_REG_SS].selector.value;
          if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
            then return_ESP := ESP
            else return_ESP :=  SP;
          return_ss_base := Self.sregs[SEG_REG_SS].cache.segment.base;

          // save return CS:eIP to be pushed on new stack
          return_CS := Self.sregs[SEG_REG_CS].selector.value;
          if ( cs_descriptor.segment.d_b ) <> 0 then
            return_EIP := FEIP
          else
            return_EIP :=  EIP;

          if (gate_descriptor.type_ = 4) then
          begin
            i_ := 0;
            while i_ < param_count do // !!! param_count - 1 ??
            begin
              access_linear(return_ss_base + return_ESP + i_ * 2,
                2, 0, BX_READ, @parameter_word[i_]);
                Inc(i_);
            end;
          end else
          begin
            i_ := 0;
            while i_ < param_count do // !!! param_count - 1 ??
              access_linear(return_ss_base + return_ESP + i_ * 4,
                4, 0, BX_READ, @parameter_dword[i_]);
//              Inc(i_); // <<<< strange
          end;
        end;

        (* load new SS:SP value from TSS *)
        (* load SS descriptor *)
        load_ss(@ss_selector, @ss_descriptor, ss_descriptor.dpl);
        if (ss_descriptor.segment.d_b) <> 0
          then ESP := ESP_for_cpl_x
          else SP :=  Bit16u(ESP_for_cpl_x);

        (* load new CS:IP value from gate *)
        (* load CS descriptor *)
        (* set CPL to stack segment DPL *)
        (* set RPL of CS to CPL *)
        load_cs(@cs_selector, @cs_descriptor, cs_descriptor.dpl);
        FEIP := new_EIP;

        // push pointer of old stack onto new stack
        if (gate_descriptor.type_ = 4) then
        begin
          push_16(return_SS);
          push_16(Bit16u(return_ESP));
        end else
        begin
          push_32(return_SS);
          push_32(return_ESP);
        end;

        (* get word count from call gate, mask to 5 bits *)
        (* copy parameters from old stack onto new stack *)
//          if (Self.sregs[BX_SEG_REG_SS].cache.segment.d_b)<>0 then
//            temp_ESP := ESP
//          else
//            temp_ESP :=  SP;

        if (gate_descriptor.type_ = 4) then
        begin
          i_ := param_count;
          while i_ > 0 do // !!! for i:=param_count-1 downto 0 do ???
          begin
            push_16(parameter_word[i_ - 1]);
            dec(i_);
            //access_linear(Self.sregs[BX_SEG_REG_SS].cache.u.segment.base + temp_ESP + i*2,
            //  2, 0, BX_WRITE, @parameter_word[i]);
          end;
        end else
        begin
          i_:=param_count;
          while i_ > 0 do // !!! for i:=param_count-1 downto 0 do ???
          begin
            push_32(parameter_dword[i_ - 1]);
            dec(i_);
            //access_linear(Self.sregs[BX_SEG_REG_SS].cache.u.segment.base + temp_ESP + i*4,
            //  4, 0, BX_WRITE, @parameter_dword[i]);
          end;
        end;

        // push return address onto new stack
        if (gate_descriptor.type_ = 4) then
        begin
          push_16(return_CS);
          push_16(Bit16u(return_EIP));
        end else
        begin
          push_32(return_CS);
          push_32(return_EIP);
        end;

        exit;
      end;
        // CALL GATE TO SAME PRIVILEGE
      else
        begin
          if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
            then temp_ESP := ESP
            else temp_ESP := SP;

          if (gate_descriptor.type_ = 12) then
          begin
          //if (i^.os_32) then beginend;
            // stack must room for 8-byte return address (2 are padding)
            //   else #SS(0)
            if ( can_push(@Self.sregs[SEG_REG_SS].cache, temp_ESP, 8) = 0 ) then
              exception2([BX_SS_EXCEPTION, 0, 0]);
          end else
          begin
            // stack must room for 4-byte return address
            //   else #SS(0)
            if ( can_push(@Self.sregs[SEG_REG_SS].cache, temp_ESP, 4)=0 ) then
              exception2([BX_SS_EXCEPTION, 0, 0]);
          end;

          // EIP must be within code segment limit, else #GP(0)
          if ( new_EIP > cs_descriptor.segment.limit_scaled ) then
            exception2([BX_GP_EXCEPTION, 0, 0]);

          if (gate_descriptor.type_ = 12) then
          begin
            // push return address onto stack
            push_32(Self.sregs[SEG_REG_CS].selector.value);
            push_32(FEIP);
          end else
          begin
            // push return address onto stack
            push_16(Self.sregs[SEG_REG_CS].selector.value);
            push_16(EIP);
          end;

          // load CS:EIP from gate
          // load code segment descriptor into CS register
          // set RPL of CS to CPL
          load_cs(@cs_selector, @cs_descriptor, CPL);
          FEIP := new_EIP;

          exit;
          end;

        exit;
      end;

    end;

  exit;
end;


procedure TCPU.return_protected(I:PInstruction_tag; pop_bytes:Bit16u);
var
  return_SP: Bit16u;
  raw_cs_selector, raw_ss_selector: Bit16u ;
  cs_selector, ss_selector: TSelector_t;
  cs_descriptor, ss_descriptor: TDescriptor_t;
  stack_cs_offset, stack_param_offset: Bit32u;
  return_EIP, return_ESP, temp_ESP: Bit32u;
  dword1, dword2: Bit32u;
  Return_IP: Bit16u;
begin
  (* + 6+N*2: SS     or+12+N*4:     SS *)
  (* + 4+N*2: SP     or+ 8+N*4:    ESP *)
  (*          parm N or+        parm N *)
  (*          parm 3 or+        parm 3 *)
  (*          parm 2 or+        parm 2 *)
  (*          parm 1 or+ 8:     parm 1 *)
  (* + 2:     CS     or+ 4:         CS *)
  (* + 0:     IP     or+ 0:        EIP *)

  if ( i^.os_32 ) <> 0 then
  begin
    (* operand size:=32: third word on stack must be within stack limits,
     *   else #SS(0); *)
    if (can_pop(6) = 0) then
      exit; (* #SS(0) *)
    stack_cs_offset := 4;
    stack_param_offset := 8;
  end else
  begin
    (* operand size:=16: second word on stack must be within stack limits,
     *   else #SS(0);
     *)
    if ( can_pop(4)=0) then
      exit; (* #SS(0) *)
    stack_cs_offset := 2;
    stack_param_offset := 4;
  end;

  if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;

  // return selector RPL must be >= CPL, else #GP(return selector)
  access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP +
                       stack_cs_offset, 2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @raw_cs_selector);
  parse_selector(raw_cs_selector, @cs_selector);
  if ( cs_selector.rpl < CPL ) then
  begin
    exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
    exit;
  end;

  // if return selector RPL = CPL then
  // RETURN TO SAME LEVEL
  if ( cs_selector.rpl = CPL ) then
  begin
    //BX_INFO(('return: to same level %04x:%08x',
    //   Self.sregs[BX_SEG_REG_CS].selector.value,
    //   Self.prev_eip));
    // return selector must be non-null, else #GP(0)
    if ( (raw_cs_selector  and $fffc) = 0 ) then
      exit; (* #GP(0) *)

    // selector index must be within its descriptor table limits,
    // else #GP(selector)
    fetch_raw_descriptor(@cs_selector, @dword1, @dword2,
      BX_GP_EXCEPTION);

    // descriptor AR byte must indicate code segment, else #GP(selector)
    parse_descriptor(dword1, dword2, @cs_descriptor);
    if ((cs_descriptor.valid = 0) or
        (cs_descriptor.segmentType = 0) or
        (cs_descriptor.segment.executable = 0)) then
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);

    // if non-conforming then code segment DPL must := CPL,
    // else #GP(selector)
    if ((cs_descriptor.segment.c_ed=0) and (cs_descriptor.dpl<>CPL)) then
      exit; (* #GP(selector) *)

    // if conforming then code segment DPL must be <= CPL,
    // else #GP(selector)
    if ((cs_descriptor.segment.c_ed <> 0) and (cs_descriptor.dpl>CPL)) then
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);

    // code segment must be present, else #NP(selector)
    if (cs_descriptor.p=0) then
    begin
      exception2([BX_NP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
    end;

    // top word on stack must be within stack limits, else #SS(0)
    if ( can_pop(stack_param_offset + pop_bytes) = 0) then
      exit; (* #SS(0) *)

    // eIP must be in code segment limit, else #GP(0)
    if (i^.os_32) <> 0
      then access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
              4, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_EIP)
      else
      begin
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
              2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @return_IP);
        return_EIP := return_IP;
      end;

    if ( return_EIP > cs_descriptor.segment.limit_scaled ) then
      exit; (* #GP(0) *)

    // load CS:eIP from stack
    // load CS register with descriptor
    // increment eSP
    load_cs(@cs_selector, @cs_descriptor, CPL);
    Self.FEIP := return_EIP;
    if (Self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
      then ESP := ESP + stack_param_offset + pop_bytes
      else SP := SP + stack_param_offset + pop_bytes;

    exit;
  end
  (* RETURN TO OUTER PRIVILEGE LEVEL *)
  else
  begin
    (* + 6+N*2: SS     or+12+N*4:     SS *)
    (* + 4+N*2: SP     or+ 8+N*4:    ESP *)
    (*          parm N or+        parm N *)
    (*          parm 3 or+        parm 3 *)
    (*          parm 2 or+        parm 2 *)
    (*          parm 1 or+ 8:     parm 1 *)
    (* + 2:     CS     or+ 4:         CS *)
    (* + 0:     IP     or+ 0:        EIP *)

//BX_INFO(('return: to outer level %04x:%08x',
//  Self.sregs[BX_SEG_REG_CS].selector.value,
//  Self.prev_eip));

    if (i^.os_32) <> 0 then
    begin
      (* top 16+immediate bytes on stack must be within stack limits, else #SS(0) *)
      if ( can_pop(16 + pop_bytes) = 0) then
        exit; (* #SS(0) *)
    end
  else begin
      (* top 8+immediate bytes on stack must be within stack limits, else #SS(0) *)
      if ( can_pop(8 + pop_bytes)=0) then begin
        LogPanic(('return_protected: 8 bytes not within stack limits'));
        (* #SS(0) *)
        exit;
        end;
      end;

    (* examine return CS selector and associated descriptor *)

    (* selector must be non-null else #GP(0) *)
    if ( (raw_cs_selector  and $fffc) = 0 ) then begin
      LogPanic(('return_protected: CS selector null'));
      (* #GP(0) *)
      exit;
      end;

    (* selector index must be within its descriptor table limits,
     * else #GP(selector) *)
    fetch_raw_descriptor(@cs_selector, @dword1, @dword2,
      BX_GP_EXCEPTION);
    parse_descriptor(dword1, dword2, @cs_descriptor);

    (* descriptor AR byte must indicate code segment else #GP(selector) *)
    if ((cs_descriptor.valid=0) or (cs_descriptor.segmentType=0) or (cs_descriptor.segment.executable=0)) then begin
      LogPanic(('return_protected: AR byte not code'));
      (* #GP(selector) *)
      exit;
      end;

    (* if non-conforming code then code seg DPL must equal return selector RPL
     * else #GP(selector) *)
    if ((cs_descriptor.segment.c_ed=0) and (cs_descriptor.dpl<>cs_selector.rpl)) then begin
      LogPanic(('return_protected: non-conforming seg DPL !:= selector.rpl'));
      (* #GP(selector) *)
      exit;
      end;

    (* if conforming then code segment DPL must be <= return selector RPL
     * else #GP(selector) *)
    if ((cs_descriptor.segment.c_ed <>0) and (cs_descriptor.dpl>cs_selector.rpl)) then begin
      LogPanic(('return_protected: conforming seg DPL > selector.rpl'));
      (* #GP(selector) *)
      exit;
      end;

    (* segment must be present else #NP(selector) *)
    if (cs_descriptor.p=0) then begin
      LogPanic(('return_protected: segment not present'));
      (* #NP(selector) *)
      exit;
      end;

    (* examine return SS selector and associated descriptor: *)
    if (i^.os_32)<>0 then begin
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 12 + pop_bytes,
        2, 0, BX_READ, @raw_ss_selector);
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 8 + pop_bytes,
        4, 0, BX_READ, @return_ESP);
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
        4, 0, BX_READ, @return_EIP);
      end
  else begin

      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 6 + pop_bytes,
        2, 0, BX_READ, @raw_ss_selector);
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 4 + pop_bytes,
        2, 0, BX_READ, @return_SP);
      return_ESP := return_SP;
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
        2, 0, BX_READ, @return_IP);
      return_EIP := return_IP;
      end;

    (* selector must be non-null else #GP(0) *)
    if ( (raw_ss_selector  and $fffc) = 0 ) then begin
      LogPanic(('return_protected: SS selector null'));
      (* #GP(0) *)
      exit;
      end;

    (* selector index must be within its descriptor table limits,
     * else #GP(selector) *)
    parse_selector(raw_ss_selector, @ss_selector);
    fetch_raw_descriptor(@ss_selector, @dword1, @dword2,
      BX_GP_EXCEPTION);
    parse_descriptor(dword1, dword2, @ss_descriptor);

    (* selector RPL must := RPL of the return CS selector,
     * else #GP(selector) *)
    if (ss_selector.rpl <> cs_selector.rpl) then begin
//      BX_INFO(('return_protected: ss.rpl !:= cs.rpl'));
      exception2([BX_GP_EXCEPTION, raw_ss_selector  and $fffc, 0]);
      exit;
      end;

    (* descriptor AR byte must indicate a writable data segment,
     * else #GP(selector) *)
    if ((ss_descriptor.valid=0) or (ss_descriptor.segmentType=0) or (ss_descriptor.segment.executable<>0) or
        (ss_descriptor.segment.r_w=0)) then begin
      LogPanic(('return_protected: SS.AR byte not writable data'));
      (* #GP(selector) *)
      exit;
      end;

    (* descriptor dpl must := RPL of the return CS selector,
     * else #GP(selector) *)
    if (ss_descriptor.dpl <> cs_selector.rpl) then begin
      LogPanic(('return_protected: SS.dpl !:= cs.rpl'));
      (* #GP(selector) *)
      exit;
      end;

    (* segment must be present else #SS(selector) *)
    if (ss_descriptor.p=0) then begin
      LogPanic(('ss.p = 0'));
      (* #NP(selector) *)
      exit;
      end;

    (* eIP must be in code segment limit, else #GP(0) *)
    if (return_EIP > cs_descriptor.segment.limit_scaled) then begin
      LogPanic(('return_protected: eIP > cs.limit'));
      (* #GP(0) *)
      exit;
      end;

    (* set CPL to RPL of return CS selector *)
    (* load CS:IP from stack *)
    (* set CS RPL to CPL *)
    (* load the CS-cache with return CS descriptor *)
    load_cs(@cs_selector, @cs_descriptor, cs_selector.rpl);
    Self.FEIP := return_EIP;

    (* load SS:SP from stack *)
    (* load SS-cache with return SS descriptor *)
    load_ss(@ss_selector, @ss_descriptor, cs_selector.rpl);
    if (ss_descriptor.segment.d_b)<>0 then
      ESP := return_ESP + pop_bytes
    else
      SP  := Bit16u(return_ESP + pop_bytes);

    (* check ES, DS, FS, GS for validity *)
    validate_seg_regs();

    exit;
    end;

  exit;
end;

procedure TCPU.iret_protected(I: PInstruction_tag);
var
  raw_cs_selector, raw_ss_selector:Bit16u;
  cs_selector, ss_selector:TSelector_t;
  dword1, dword2:Bit32u;
  cs_descriptor, ss_descriptor:TDescriptor_t;
  base32:Bit32u;
  raw_link_selector:Bit16u;
  link_selector:TSelector_t;
  tss_descriptor:TDescriptor_t;

  top_nbytes_same, top_nbytes_outer:Bit16u;
  cs_offset, ss_offset:Bit32u;
  new_eip, new_esp, temp_ESP, new_eflags:Bit32u;
  new_ip, new_sp, new_flags:Bit16u;
  prev_cpl:Bit8u;
begin

  if (Self.eflags.nt)<>0 then begin (* NT := 1: RETURN FROM NESTED TASK *)
    (* what's the deal with NT  and VM ? *)

    if (Self.eflags.vm)<>0 then
      LogPanic(('IRET: vm set?'));

    // TASK_RETURN:

    //BX_INFO(('IRET: nested task return'));

    if (Self.tr.cache.valid=0) then
      LogPanic(('IRET: TR not valid'));
    if (Self.tr.cache.type_ = 1) then
      base32 := Self.tr.cache.tss286.base
    else if (Self.tr.cache.type_ = 9) then
      base32 := Self.tr.cache.tss386.base
    else begin
      LogPanic(('IRET: TR not valid'));
      base32 := 0; // keep compiler happy
      end;

    // examine back link selector in TSS addressed by current TR:
    access_linear(base32 + 0, 2, 0, BX_READ, @raw_link_selector);

    // must specify global, else #TS(new TSS selector)
    parse_selector(raw_link_selector, @link_selector);
    if (link_selector.ti)<>0 then begin
      LogPanic(('iret: link selector.ti:=1'));
      exception2([BX_TS_EXCEPTION, raw_link_selector  and $fffc, 0]);
      end;

    // index must be within GDT limits, else #TS(new TSS selector)
    fetch_raw_descriptor(@link_selector, @dword1, @dword2, BX_TS_EXCEPTION);

    // AR byte must specify TSS, else #TS(new TSS selector)
    // new TSS must be busy, else #TS(new TSS selector)
    parse_descriptor(dword1, dword2, @tss_descriptor);
    if ((tss_descriptor.valid=0) or (tss_descriptor.segmentType<>0)) then begin
//      BX_INFO(('iret: TSS selector points to bad TSS'));
      exception2([BX_TS_EXCEPTION, raw_link_selector  and $fffc, 0]);
      end;
    if ((tss_descriptor.type_<>11) and (tss_descriptor.type_<>3)) then begin
//      BX_INFO(('iret: TSS selector points to bad TSS'));
      exception2([BX_TS_EXCEPTION, raw_link_selector  and $fffc, 0]);
      end;


    // TSS must be present, else #NP(new TSS selector)
    if (tss_descriptor.p=0) then begin
//      BX_INFO(('iret: task descriptor.p = 0'));
      exception2([BX_NP_EXCEPTION, raw_link_selector  and $fffc, 0]);
      end;

    // switch tasks (without nesting) to TSS specified by back link selector
    task_switch(@link_selector, @tss_descriptor,
                BX_TASK_FROM_IRET, dword1, dword2);

    // mark the task just abandoned as not busy

    // eIP must be within code seg limit, else #GP(0)
    if (FEIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then begin
      LogPanic(('iret: eIP > cs.limit'));
      exception2([BX_GP_EXCEPTION, $0000, 0]);
      end;
    exit;
    end

  else begin (* NT := 0: INTERRUPT RETURN ON STACK -or STACK_RETURN_TO_V86 *)

    (* 16bit opsize or  32bit opsize
     * ===============
     * SS     eSP+8 or  SS     eSP+16
     * SP     eSP+6 or  ESP    eSP+12
     * -------------------------------
     * FLAGS  eSP+4 or  EFLAGS eSP+8
     * CS     eSP+2 or  CS     eSP+4
     * IP     eSP+0 or  EIP    eSP+0
     *)

    if (i^.os_32)<>0 then begin
      top_nbytes_same    := 12;
      top_nbytes_outer   := 20;
      cs_offset := 4;
      ss_offset := 16;
      end
  else begin
      top_nbytes_same    := 6;
      top_nbytes_outer   := 10;
      cs_offset := 2;
      ss_offset := 8;
      end;

    (* CS on stack must be within stack limits, else #SS(0) *)
    if ( can_pop(top_nbytes_same)=0) then begin
      LogPanic(('iret: CS not within stack limits'));
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
      end;

    if (Self.sregs[SEG_REG_SS].cache.segment.d_b)<>0 then
      temp_ESP := ESP
    else
      temp_ESP := SP;

    access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + cs_offset,
      2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @raw_cs_selector);

    if (i^.os_32)<>0 then begin
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
        4, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @new_eip);
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 8,
        4, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @new_eflags);

      // if VM:=1 in flags image on stack then STACK_RETURN_TO_V86
      if (new_eflags  and $00020000)<>0 then begin
        if (CPL <> 0) then
          LogPanic(('iret: VM set on stack, CPL!:=0'));
        Self.stack_return_to_v86(new_eip, raw_cs_selector, new_eflags);
        exit;
        end;
      end
  else begin
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
        2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @new_ip);
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 4,
        2, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, @new_flags);
      end;

    parse_selector(raw_cs_selector, @cs_selector);

    // return CS selector must be non-null, else #GP(0)
    if ( (raw_cs_selector  and $fffc) = 0 ) then begin
      LogPanic(('iret: return CS selector null'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
      end;

    // selector index must be within descriptor table limits,
    // else #GP(return selector)
    fetch_raw_descriptor(@cs_selector, @dword1, @dword2,
      BX_GP_EXCEPTION);

    parse_descriptor(dword1, dword2, @cs_descriptor);

    // AR byte must indicate code segment else #GP(return selector)
    if ( (cs_descriptor.valid=0) or (cs_descriptor.segmentType=0) or (cs_descriptor.segment.executable=0)) then begin
      LogPanic(('iret: AR byte indicated non code segment'));
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
      end;

    // return CS selector RPL must be >= CPL, else #GP(return selector)
    if (cs_selector.rpl < CPL) then begin
      LogPanic(('iret: return selector RPL < CPL'));
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
      end;

    // if return code seg descriptor is conforming
    //   and return code seg DPL > return code seg selector RPL
    //     then #GP(return selector)
    if ((cs_descriptor.segment.c_ed<>0) and (cs_descriptor.dpl > cs_selector.rpl)) then begin
      LogPanic(('iret: conforming, DPL > cs_selector.RPL'));
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
      end;

    // if return code seg descriptor is non-conforming
    //   and return code seg DPL !:= return code seg selector RPL
    //     then #GP(return selector)
    if ((cs_descriptor.segment.c_ed=0) and (cs_descriptor.dpl <> cs_selector.rpl)) then begin
//      BX_INFO(('(mch) iret: Return with DPL !:= RPL. #GP(selector)'));
      exception2([BX_GP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
      end;

    // segment must be present else #NP(return selector)
    if ( cs_descriptor.p=0 ) then begin
      LogPanic(('iret: not present'));
      exception2([BX_NP_EXCEPTION, raw_cs_selector  and $fffc, 0]);
      exit;
      end;

    if (cs_selector.rpl = CPL) then begin (* INTERRUPT RETURN TO SAME LEVEL *)
      (* top 6/12 bytes on stack must be within limits, else #SS(0) *)
      (* satisfied above *)

      if (i^.os_32)<>0 then begin
        (* return EIP must be in code segment limit else #GP(0) *)
        if ( new_eip > cs_descriptor.segment.limit_scaled ) then begin
          LogPanic(('iret: IP > descriptor limit'));
          exception2([BX_GP_EXCEPTION, 0, 0]);
          exit;
          end;
        (* load CS:EIP from stack *)
        (* load CS-cache with new code segment descriptor *)
        load_cs(@cs_selector, @cs_descriptor, CPL);
        FEIP := new_eip;

        (* load EFLAGS with 3rd doubleword from stack *)
        write_eflags(new_eflags, Bool(CPL=0), Bool(CPL<=IOPL), 0, 1);
        end
      else begin
        (* return IP must be in code segment limit else #GP(0) *)
        if ( new_ip > cs_descriptor.segment.limit_scaled ) then begin
          LogPanic(('iret: IP > descriptor limit'));
          exception2([BX_GP_EXCEPTION, 0, 0]);
          exit;
          end;
        (* load CS:IP from stack *)
        (* load CS-cache with new code segment descriptor *)
        load_cs(@cs_selector, @cs_descriptor, CPL);
        FEIP := new_ip;

        (* load flags with third word on stack *)
        write_flags(new_flags, Bool(CPL=0), Bool(CPL<=IOPL));
        end;

      (* increment stack by 6/12 *)
      if (Self.sregs[SEG_REG_SS].cache.segment.d_b)<>0 then
        ESP := ESP + top_nbytes_same
      else
        SP := SP + top_nbytes_same;
      exit;
      end
  else begin (* INTERRUPT RETURN TO OUTER PRIVILEGE LEVEL *)
      (* 16bit opsize or  32bit opsize
       * ===============
       * SS     eSP+8 or  SS     eSP+16
       * SP     eSP+6 or  ESP    eSP+12
       * FLAGS  eSP+4 or  EFLAGS eSP+8
       * CS     eSP+2 or  CS     eSP+4
       * IP     eSP+0 or  EIP    eSP+0
       *)

      (* top 10/20 bytes on stack must be within limits else #SS(0) *)
      if ( can_pop(top_nbytes_outer)=0) then begin
        LogPanic(('iret: top 10/20 bytes not within stack limits'));
        exception2([BX_SS_EXCEPTION, 0, 0]);
        exit;
        end;

      (* examine return SS selector and associated descriptor *)
      access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + ss_offset,
        2, 0, BX_READ, @raw_ss_selector);

      (* selector must be non-null, else #GP(0) *)
      if ( (raw_ss_selector  and $fffc) = 0 ) then begin
        LogPanic(('iret: SS selector null'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
        end;

      parse_selector(raw_ss_selector, @ss_selector);

      (* selector RPL must := RPL of return CS selector,
       * else #GP(SS selector) *)
      if ( ss_selector.rpl <> cs_selector.rpl) then begin
        LogPanic(('iret: SS.rpl !:= CS.rpl'));
        exception2([BX_GP_EXCEPTION, raw_ss_selector  and $fffc, 0]);
        exit;
        end;

      (* selector index must be within its descriptor table limits,
       * else #GP(SS selector) *)
      fetch_raw_descriptor(@ss_selector, @dword1, @dword2,
        BX_GP_EXCEPTION);

      parse_descriptor(dword1, dword2, @ss_descriptor);

      (* AR byte must indicate a writable data segment,
       * else #GP(SS selector) *)
      if ((ss_descriptor.valid=0) or (ss_descriptor.segmentType=0) or (ss_descriptor.segment.executable<>0) or
           (ss_descriptor.segment.r_w=0)) then begin
        LogPanic(('iret: SS AR byte not writable code segment'));
        exception2([BX_GP_EXCEPTION, raw_ss_selector  and $fffc, 0]);
        exit;
        end;

      (* stack segment DPL must equal the RPL of the return CS selector,
       * else #GP(SS selector) *)
      if ( ss_descriptor.dpl <> cs_selector.rpl ) then begin
        LogPanic(('iret: SS.dpl !:= CS selector RPL'));
        exception2([BX_GP_EXCEPTION, raw_ss_selector  and $fffc, 0]);
        exit;
        end;

      (* SS must be present, else #NP(SS selector) *)
      if ( ss_descriptor.p=0 ) then begin
        LogPanic(('iret: SS not present!'));
        exception2([BX_NP_EXCEPTION, raw_ss_selector  and $fffc, 0]);
        exit;
        end;


      if (i^.os_32)<>0 then begin
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
          4, 0, BX_READ, @new_eip);
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 8,
          4, 0, BX_READ, @new_eflags);
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 12,
          4, 0, BX_READ, @new_esp);
        end
      else begin
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 0,
          2, 0, BX_READ, @new_ip);
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 4,
          2, 0, BX_READ, @new_flags);
        access_linear(Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP + 6,
          2, 0, BX_READ, @new_sp);
        new_eip := new_ip;
        new_esp := new_sp;
        new_eflags := new_flags;
        end;

      (* EIP must be in code segment limit, else #GP(0) *)
      if ( new_eip > cs_descriptor.segment.limit_scaled ) then begin
        LogPanic(('iret: IP > descriptor limit'));
        exception2([BX_GP_EXCEPTION, 0, 0]);
        exit;
        end;

      (* load CS:EIP from stack *)
      (* load the CS-cache with CS descriptor *)
      (* set CPL to the RPL of the return CS selector *)
      prev_cpl := CPL; (* previous CPL *)
      load_cs(@cs_selector, @cs_descriptor, cs_selector.rpl);
      Self.FEIP := new_eip;

      (* load flags from stack *)
      // perhaps I should always write_eflags(), thus zeroing
      // out the upper 16bits of eflags for CS.D_B=0 ???
      if (cs_descriptor.segment.d_b)<>0 then
        write_eflags(new_eflags, Bool(prev_cpl=0), Bool(prev_cpl<=IOPL), 0, 1)
      else
        write_flags(Bit16u(new_eflags), Bool(prev_cpl=0), Bool(prev_cpl<=IOPL));

      // load SS:eSP from stack
      // load the SS-cache with SS descriptor
      load_ss(@ss_selector, @ss_descriptor, cs_selector.rpl);
      if (ss_descriptor.segment.d_b)<>0 then
        ESP := new_esp
      else
        SP  := new_esp;

      validate_seg_regs();

      exit;
      end;
    end;
  LogPanic(('IRET: shouldn''t get here!'));
end;

procedure TCPU.validate_seg_regs;
begin
  if ( Self.sregs[SEG_REG_ES].cache.dpl<CPL ) then begin
    Self.sregs[SEG_REG_ES].cache.valid := 0;
    Self.sregs[SEG_REG_ES].selector.value := 0;
    end;
  if ( Self.sregs[SEG_REG_DS].cache.dpl<CPL ) then begin
    Self.sregs[SEG_REG_DS].cache.valid := 0;
    Self.sregs[SEG_REG_DS].selector.value := 0;
    end;
  if ( Self.sregs[SEG_REG_FS].cache.dpl<CPL ) then begin
    Self.sregs[SEG_REG_FS].cache.valid := 0;
    Self.sregs[SEG_REG_FS].selector.value := 0;
    end;
  if ( Self.sregs[SEG_REG_GS].cache.dpl<CPL ) then begin
    Self.sregs[SEG_REG_GS].cache.valid := 0;
    Self.sregs[SEG_REG_GS].selector.value := 0;
    end;
end;

procedure TCPU.ARPL_EwGw(I: PInstruction_tag);
var
  op2_16, op1_16:Bit16u;
  op1_32:Bit32u;
begin
  if Boolean(Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))) then
  begin
    (* op1_16 is a register or memory reference *)
    if Boolean(i^.mod_ = $c0) then
      op1_16 := BX_READ_16BIT_REG(i^.rm)
    else
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    if Boolean( (op1_16  and $03) < (op2_16  and $03) ) then
    begin
      op1_16 := (op1_16  and $fffc)or(op2_16  and $03);
      (* now write back to destination *)
      if Boolean(i^.mod_ = $c0) then
      begin
        if Boolean(i^.os_32) then
        begin
          // if 32bit opsize, then $ff3f is or'd into
          // upper 16bits of register

          op1_32 := BX_READ_32BIT_REG(i^.rm);
          op1_32 := (op1_32 and $ffff0000) or op1_16;
          op1_32 := op1_32 or $ff3f0000;
          BX_WRITE_32BIT_REG(i^.rm, op1_32);
        end else
          BX_WRITE_16BIT_REG(i^.rm, op1_16);
      end else
        write_RMW_virtual_word(op1_16);
      set_ZF(1);
    end
    else
      set_ZF(0);
  end else
  begin
    // ARPL not recognized in real or v8086 mod_e
    UndefinedOpcode(i);
    exit;
  end;
end;

procedure TCPU.LAR_GvEw(I: PInstruction_tag);
var
  raw_selector:Bit16u;
  descriptor:TDescriptor_t;
  selector:TSelector_t;
  dword1, dword2:Bit32u;
begin
  (* for 16 bit operand size mod_e *)

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  if Boolean(real_mode()) then begin
    LogPanic(('LAR_GvEw: not recognized in real mod_e'));
    UndefinedOpcode(i);
    exit;
    end;


  if Boolean(i^.mod_ = $c0) then begin
    raw_selector := BX_READ_16BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);
    end;

  (* if selector null, clear ZF and done *)
  if Boolean( (raw_selector  and $fffc) = 0 ) then begin
    set_ZF(0);
    exit;
    end;

  parse_selector(raw_selector, @selector);

  if Boolean( fetch_raw_descriptor2(@selector, @dword1, @dword2)=0) then begin
    (* not within descriptor table *)
    set_ZF(0);
    exit;
    end;

  parse_descriptor(dword1, dword2, @descriptor);

  if Boolean(descriptor.valid=0) then begin
    set_ZF(0);
    //BX_DEBUG(('lar(): descriptor valid bit cleared'));
    exit;
    end;

  (* if source selector is visible at CPL  and RPL,
   * within the descriptor table, and of type accepted by LAR instruction,
   * then load register with segment limit and set ZF
   *)

  if Boolean( descriptor.segmentType ) then begin (* normal segment *)
    if Boolean( descriptor.segment.executable and descriptor.segment.c_ed ) then begin
      (* ignore DPL for conforming segments *)
      end
  else begin
      if Boolean( (descriptor.dpl<CPL) or (descriptor.dpl<selector.rpl) ) then begin
        set_ZF(0);
        exit;
        end;
      end;
    set_ZF(1);
    if Boolean(i^.os_32) then begin
      (* masked by 00FxFF00, where x is undefined *)
      BX_WRITE_32BIT_REG(i^.nnn, dword2  and $00ffff00);
      end
  else begin
      BX_WRITE_16BIT_REG(i^.nnn, dword2  and $ff00);
      end;
    exit;
    end
  else begin (* system or gate segment *)
    case ( descriptor.type_ ) of
      1, (* available TSS *)
      2, (* LDT *)
      3, (* busy TSS *)
      4, (* 286 call gate *)
      5, (* task gate *)
      9,  (* available 32bit TSS *)
      11, (* busy 32bit TSS *)
      12: (* 32bit call gate *)
        begin
        end;
      else (* rest not accepted types to LAR *)
        begin
          set_ZF(0);
//          BX_DEBUG(('lar(): not accepted type'));
          exit;
        end;
      end;

    if Boolean( (descriptor.dpl<CPL) or (descriptor.dpl<selector.rpl) ) then begin
      set_ZF(0);
      exit;
      end;
    set_ZF(1);
    if Boolean(i^.os_32) then begin
      (* masked by 00FxFF00, where x is undefined ??? *)
      BX_WRITE_32BIT_REG(i^.nnn, dword2  and $00ffff00);
      end
  else begin
      BX_WRITE_16BIT_REG(i^.nnn, dword2  and $ff00);
      end;
    exit;
    end;
end;

procedure TCPU.LSL_GvEw(I: PInstruction_tag);
var
  raw_selector: Bit16u;
  limit32: Bit32u;
  selector: TSelector_t;
  dword1, dword2: Bit32u;
  descriptor_dpl: Bit32u;
  type_: Bit32u;
  label lsl_ok;
begin
  (* for 16 bit operand size mod_e *)
  //bx_descriptor_t descriptor;
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  if Boolean(real_mode()) then
  begin
    LogPanic(('LSL_GvEw: not recognized in real mod_e'));
    UndefinedOpcode(i);
    exit;
  end;

  if Boolean(i^.mod_ = $c0)
    then raw_selector := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);
  (* if selector null, clear ZF and done *)
  if Boolean( (raw_selector  and $fffc) = 0 ) then
  begin
    set_ZF(0);
    exit;
  end;

  parse_selector(raw_selector, @selector);

  if Boolean( fetch_raw_descriptor2(@selector, @dword1, @dword2) = 0) then
  begin
    (* not within descriptor table *)
    set_ZF(0);
    exit;
  end;
  //parse_descriptor(dword1, dword2, @descriptor);
  descriptor_dpl := (dword2 shr 13)  and $03;

  if Boolean( (dword2  and $00001000) = 0 ) then // system segment
  begin
    type_ := (dword2 shr 8)  and $0000000f;
    case (type_) of
      1, // 16bit TSS
      3, // 16bit TSS
      2, // LDT
      9, // 32bit TSS    G00A
      11:// 32bit TSS    G00A
        begin
          limit32 := (dword1 and $0000ffff) or (dword2 and $000f0000);
          if Boolean( Bool(dword2 and $00800000) ) then
            limit32 := (limit32 shl 12) or $00000fff;
          if Boolean( (descriptor_dpl<CPL) or (descriptor_dpl<selector.rpl) ) then
          begin
            set_ZF(0);
            exit;
          end;
          goto lsl_ok;
        end;
      else
          set_ZF(0);
      end;
  end else
  begin // data  and code segment
    limit32 := (dword1  and $0000ffff)or(dword2  and $000f0000);
    if Boolean( dword2  and $00800000 ) then
      limit32 := (limit32 shl 12)or$00000fff;
    if Boolean( (dword2  and $00000c00) = $00000c00 ) then
      goto lsl_ok; // conforming code segment, no check done

    if Boolean( (descriptor_dpl<CPL) or (descriptor_dpl<selector.rpl) ) then
    begin
      set_ZF(0);
      exit;
    end;
    goto lsl_ok;
  end;
lsl_ok:
  (* all checks pass, limit32 is now byte granular, write to op1 *)
  set_ZF(1);

  if Boolean(i^.os_32)
    then BX_WRITE_32BIT_REG(i^.nnn, limit32)
    // chop off upper 16 bits
    else BX_WRITE_16BIT_REG(i^.nnn, Bit16u(limit32));
end;

procedure TCPU.SLDT_Ew(I: PInstruction_tag);
var
  val16: Bit16u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  if Boolean(real_mode()) then
  begin
    (* not recognized in real address mod_e *)
    LogError(('SLDT_Ew: encountered in real mod_e.'));
    UndefinedOpcode(i);
  end else
  begin
    val16 := self.ldtr.selector.value;
    if Boolean(i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, val16)
      else write_virtual_word(i^.seg, i^.rm_addr, @val16);
  end;
end;

procedure TCPU.STR_Ew(I: PInstruction_tag);
var
  val16: Bit16u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  if Boolean(real_mode()) then
  begin
    // not recognized in real address mod_e
    LogPanic(('STR_Ew: encountered in real mod_e.'));
    UndefinedOpcode(i);
  end else
  begin
    val16 := self.tr.selector.value;
    if Boolean(i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, val16)
      else write_virtual_word(i^.seg, i^.rm_addr, @val16);
    end;
end;

procedure TCPU.LLDT_Ew(I: PInstruction_tag);
var
  descriptor: TDescriptor_t;
  selector: TSelector_t;
  raw_selector: Bit16u;
  dword1, dword2: Bit32u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  invalidate_prefetch_q();

  if Boolean(real_mode()) then
  begin
    LogPanic(('lldt: not recognized in real mod_e'));
    UndefinedOpcode(i);
    exit;
  end else
  begin (* protected mod_e *)
    (* #GP(0) if the current privilege level is not 0 *)
    if Boolean(CPL <> 0) then
    begin
      LogPanic(('LLDT: CPL !:= 0'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

    if Boolean(i^.mod_ = $c0)
      then raw_selector := BX_READ_16BIT_REG(i^.rm)
      else read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);

    (* if selector is NULL, invalidate and done *)
    if Boolean((raw_selector  and $fffc) = 0) then
    begin
      self.ldtr.selector.value := raw_selector;
      self.ldtr.cache.valid := 0;
      exit;
    end;
    (* parse fields in selector *)
    parse_selector(raw_selector, @selector);
    // #GP(selector) if the selector operand does not point into GDT
    if Boolean(selector.ti <> 0) then
    begin
      LogError(('LLDT: selector.ti !:= 0'));
      exception2([BX_GP_EXCEPTION, raw_selector  and $fffc, 0]);
    end;

    if Boolean((selector.index*8 + 7) > self.gdtr.limit) then
    begin
      LogPanic(('lldt: GDT: index > limit'));
      exception2([BX_GP_EXCEPTION, raw_selector  and $fffc, 0]);
      exit;
    end;

    access_linear(self.gdtr.base + selector.index*8,     4, 0, BX_READ, @dword1);
    access_linear(self.gdtr.base + selector.index*8 + 4, 4, 0, BX_READ, @dword2);

    parse_descriptor(dword1, dword2, @descriptor);
    (* if selector doesn't point to an LDT descriptor #GP(selector) *)
    if Boolean( (descriptor.valid = 0) or (descriptor.segmentType <> 0)  or
                (descriptor.type_ <> 2) ) then
    begin
      LogError(('lldt: doesn''t point to an LDT descriptor!'));
      exception2([BX_GP_EXCEPTION, raw_selector  and $fffc, 0]);
    end;
    (* #NP(selector) if LDT descriptor is not present *)
    if Boolean(descriptor.p=0) then
    begin
      LogError(('lldt: LDT descriptor not present!'));
      exception2([BX_NP_EXCEPTION, raw_selector  and $fffc, 0]);
    end;

    if Boolean(descriptor.ldt.limit < 7) then
      LogError(('lldt: ldtr.limit < 7'));

    self.ldtr.selector := selector;
    self.ldtr.cache := descriptor;
    self.ldtr.cache.valid := 1;
  end;
end;

procedure TCPU.LTR_Ew(I: PInstruction_tag);
var
  descriptor: TDescriptor_t;
  selector: TSelector_t;
  raw_selector: Bit16u;
  dword1, dword2: Bit32u;
begin
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  invalidate_prefetch_q();

  if Boolean(Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) then
  begin
    (* #GP(0) if the current privilege level is not 0 *)
    if Boolean(CPL <> 0) then
    begin
      LogPanic(('LTR: CPL !:= 0'));
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

    if Boolean(i^.mod_ = $c0)
      then raw_selector := BX_READ_16BIT_REG(i^.rm)
      else read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);
    (* if selector is NULL, invalidate and done *)
    if Boolean((raw_selector  and $fffc) = 0) then
    begin
      LogPanic(('ltr: loading with NULL selector!'));
      (* if this is OK, then invalidate and load selector  and descriptor cache *)
      (* load here *)
      self.tr.selector.value := raw_selector;
      self.tr.cache.valid := 0;
      exit;
    end;
    (* parse fields in selector, then check for null selector *)
    parse_selector(raw_selector, @selector);
    if Boolean(selector.ti) then
    begin
      LogPanic(('ltr: selector.ti !:= 0'));
      exit;
    end;
    (* fetch 2 dwords of descriptor; call handles out of limits checks *)
    fetch_raw_descriptor(@selector, @dword1, @dword2, BX_GP_EXCEPTION);
    parse_descriptor(dword1, dword2, @descriptor);
    (* #GP(selector) if object is not a TSS or is already busy *)
    if Boolean( (descriptor.valid = 0) or (descriptor.segmentType <> 0)  or
              ((descriptor.type_ <> 1) and (descriptor.type_ <> 9)) ) then
    begin
      LogPanic(('ltr: doesn''t point to an available TSS descriptor!'));
      exception2([BX_GP_EXCEPTION, raw_selector  and $fffc, 0]); (* 0 ??? *)
      exit;
    end;
    (* #NP(selector) if TSS descriptor is not present *)
    if Boolean(descriptor.p = 0) then
    begin
      LogPanic(('ltr: LDT descriptor not present!'));
      exception2([BX_NP_EXCEPTION, raw_selector  and $fffc, 0]); (* 0 ??? *)
      exit;
    end;

    if Boolean((descriptor.type_ = 1) and (descriptor.tss286.limit < 43))
      then LogPanic(('ltr:286TSS: loading tr.limit < 43'))
      else
    if Boolean((descriptor.type_ = 9) and (descriptor.tss386.limit_scaled < 103)) then
      LogPanic(('ltr:386TSS: loading tr.limit < 103'));

    self.tr.selector := selector;
    self.tr.cache    := descriptor;
    self.tr.cache.valid := 1;
    // tr.cache.type should not have busy bit, or it would not get
    // through the conditions above.
    assert((self.tr.cache.type_ and 2) = 0);
    (* mark as busy *)
    dword2 := dword2 or $00000200; (* set busy bit *)
    access_linear(self.gdtr.base + selector.index*8 + 4, 4, 0, BX_WRITE, @dword2);
  end else
  begin
    LogPanic(('ltr_ew: not recognized in real-mod_e!'));
    UndefinedOpcode(i);
  end;
end;

procedure TCPU.VERR_Ew(I: PInstruction_tag);
var
  raw_selector: Bit16u;
  descriptor: TDescriptor_t;
  selector: TSelector_t;
  dword1, dword2: Bit32u;
begin
  (* for 16 bit operand size mod_e *)
  if Boolean(v8086_mode()) then
    LogPanic(('protect_ctrl: v8086 mod_e unsupported'));


  if Boolean(real_mode()) then
  begin
    LogPanic(('VERR_Ew: not recognized in real mod_e'));
    UndefinedOpcode(i);
    exit;
  end;

  if Boolean(i^.mod_ = $c0)
    then raw_selector := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);
  (* if selector null, clear ZF and done *)
  if Boolean( (raw_selector  and $fffc) = 0 ) then
  begin
    set_ZF(0);
    LogError(('VERR: null selector'));
    exit;
  end;
  (* if source selector is visible at CPL  and RPL,
   * within the descriptor table, and of type accepted by VERR instruction,
   * then load register with segment limit and set ZF *)
  parse_selector(raw_selector, @selector);

  if Boolean( fetch_raw_descriptor2(@selector, @dword1, @dword2) = 0) then
  begin
    (* not within descriptor table *)
    set_ZF(0);
    LogError(('VERR: not in table'));
    exit;
  end;

  parse_descriptor(dword1, dword2, @descriptor);

  if Boolean( descriptor.segmentType = 0 ) then (* system or gate descriptor *)
  begin
    set_ZF(0); (* inaccessible *)
    LogError(('VERR: system descriptor'));
    exit;
  end;

  if Boolean( descriptor.valid = 0 ) then
  begin
    set_ZF(0);
//    BX_INFO(('VERR: valid bit cleared'));
    exit;
  end;
  (* normal data/code segment *)
  if Boolean( descriptor.segment.executable ) then (* code segment *)
  begin
    (* ignore DPL for readable conforming segments *)
    if Boolean( descriptor.segment.c_ed and
         descriptor.segment.r_w) then begin
      set_ZF(1); (* accessible *)
//      BX_INFO(('VERR: conforming code, OK'));
      exit;
      end;
    if Boolean( descriptor.segment.r_w=0 ) then begin
      set_ZF(0); (* inaccessible *)
//      BX_INFO(('VERR: code not readable'));
      exit;
      end;
    (* readable, non-conforming code segment *)
    if Boolean( (descriptor.dpl<CPL) or (descriptor.dpl<selector.rpl) ) then begin
      set_ZF(0); (* inaccessible *)
//      BX_INFO(('VERR: non-coforming code not withing priv level'));
      exit;
      end;
    set_ZF(1); (* accessible *)
//    BX_INFO(('VERR: code seg readable'));
    exit;
    end
  else begin (* data segment *)
    if Boolean( (descriptor.dpl<CPL) or (descriptor.dpl<selector.rpl) ) then begin
      set_ZF(0); (* not accessible *)
//      BX_INFO(('VERR: data seg not withing priv level'));
      exit;
      end;
    set_ZF(1); (* accessible *)
    LogError(('VERR: data segment OK'));
    exit;
    end;
end;

procedure TCPU.VERW_Ew(I: PInstruction_tag);
var
  raw_selector:Bit16u;
  descriptor:TDescriptor_t;
  selector:TSelector_t;
  dword1, dword2:Bit32u;
begin
  (* for 16 bit operand size mod_e *)

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  if Boolean(real_mode()) then begin
    LogPanic(('VERW_Ew: not recognized in real mod_e'));
    UndefinedOpcode(i);
    exit;
    end;

  if Boolean(i^.mod_ = $c0) then begin
    raw_selector := BX_READ_16BIT_REG(i^.rm);
    end
  else begin
    (* pointer, segment address pair *)
    read_virtual_word(i^.seg, i^.rm_addr, @raw_selector);
    end;

  (* if selector null, clear ZF and done *)
  if Boolean( (raw_selector  and $fffc) = 0 ) then begin
    set_ZF(0);
    LogError(('VERW: null selector'));
    exit;
    end;

  (* if source selector is visible at CPL  and RPL,
   * within the descriptor table, and of type accepted by VERW instruction,
   * then load register with segment limit and set ZF *)
  parse_selector(raw_selector, @selector);

  if Boolean( fetch_raw_descriptor2(@selector, @dword1, @dword2)=0) then begin
    (* not within descriptor table *)
    set_ZF(0);
    LogError(('VERW: not in table'));
    exit;
    end;

  parse_descriptor(dword1, dword2, @descriptor);

  (* rule out system segments  and code segments *)
  if Boolean( (descriptor.segmentType=0) or (descriptor.segment.executable<>0) ) then begin
    set_ZF(0);
    LogError(('VERW: system seg or code'));
    exit;
    end;

  if Boolean( descriptor.valid=0 ) then begin
    set_ZF(0);
//    BX_INFO(('VERW: valid bit cleared'));
    exit;
    end;

  (* data segment *)
  if Boolean( descriptor.segment.r_w ) then begin (* writable *)
    if Boolean( (descriptor.dpl<CPL) or (descriptor.dpl<selector.rpl) ) then begin
      set_ZF(0); (* not accessible *)
//      BX_INFO(('VERW: writable data seg not within priv level'));
      exit;
      end;
    set_ZF(1); (* accessible *)
    LogError(('VERW: data seg writable'));
    exit;
    end;

  set_ZF(0); (* not accessible *)
//  BX_INFO(('VERW: data seg not writable'));
  exit;
end;

procedure TCPU.SGDT_Ms(I: PInstruction_tag);
var
  limit_16:Bit16u;
  base_32:Bit32u;
begin

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));


  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0) then begin
    (* undefined opcode exception *)
    LogPanic(('SGDT_Ms: use of register is undefined opcode.'));
    UndefinedOpcode(i);
    exit;
    end;

  limit_16 := self.gdtr.limit;
  base_32  := self.gdtr.base;

  write_virtual_word(i^.seg, i^.rm_addr, @limit_16);

  write_virtual_dword(i^.seg, i^.rm_addr+2, @base_32);

end;

procedure TCPU.SIDT_Ms(I: PInstruction_tag);
var
  limit_16:Bit16u;
  base_32:Bit32u;
begin

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0) then begin
    (* undefined opcode exception *)
    LogPanic(('SIDT: use of register is undefined opcode.'));
    UndefinedOpcode(i);
    exit;
    end;

  limit_16 := self.idtr.limit;
  base_32  := self.idtr.base;


  write_virtual_word(i^.seg, i^.rm_addr, @limit_16);

  write_virtual_dword(i^.seg, i^.rm_addr+2, @base_32);

end;

procedure TCPU.LGDT_Ms(I: PInstruction_tag);
var
  limit_16:Bit16u;
  base0_31:Bit32u;
  base0_15:Bit16u;
  base16_23:Bit8u;
begin

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  invalidate_prefetch_q();

  if Boolean((Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))<>0) and (CPL<>0)) then begin
    LogPanic(('LGDT: protected mod_e: CPL!:=0'));
    exception2([BX_GP_EXCEPTION, 0, 0]);
    exit;
    end;

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0) then begin
    LogPanic(('LGDT generating exception 6'));
    UndefinedOpcode(i);
    exit;
    end;

  if Boolean(i^.os_32) then begin

    read_virtual_word(i^.seg, i^.rm_addr, @limit_16);

    read_virtual_dword(i^.seg, i^.rm_addr + 2, @base0_31);

    self.gdtr.limit := limit_16;
    self.gdtr.base := base0_31;
    end
  else
    begin

    read_virtual_word(i^.seg, i^.rm_addr, @limit_16);

    read_virtual_word(i^.seg, i^.rm_addr + 2, @base0_15);

    read_virtual_byte(i^.seg, i^.rm_addr + 4, @base16_23);

    (* ignore high 8 bits *)

    self.gdtr.limit := limit_16;
    self.gdtr.base := (base16_23 shl 16) or base0_15;
    end;
end;

procedure TCPU.LIDT_Ms(I: PInstruction_tag);
var
  limit_16:Bit16u;
  base_32:Bit32u;
begin

  if Boolean(v8086_mode()) then LogPanic(('protect_ctrl: v8086 mod_e unsupported'));

  invalidate_prefetch_q();

  if Boolean(Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))) then begin
    if Boolean(CPL <> 0) then begin
      LogPanic(Format('LIDT(): CPL(%u) !:= 0',[CPL]));
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
      end;
    end;

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0) then begin
    (* undefined opcode exception *)
    LogPanic(('LIDT generating exception 6'));
    UndefinedOpcode(i);
    exit;
    end;

  read_virtual_word(i^.seg, i^.rm_addr, @limit_16);

  read_virtual_dword(i^.seg, i^.rm_addr + 2, @base_32);

  self.idtr.limit := limit_16;

  if Boolean(i^.os_32) then
    self.idtr.base := base_32
  else
    self.idtr.base := base_32 and $00ffffff; (* ignore upper 8 bits *)

end;

procedure TCPU.SHLD_EwGw(I: PInstruction_tag);
var
  op1_16, op2_16, result_16:Bit16u;
  temp_32, result_32:Bit32u;
  count:unsigned;
begin

  (* op1:op2 shl count.  result stored in op1 *)
  if Boolean(i^.b1 = $1a4) then
    count := i^.Ib
  else // $1a5
    count := CL;

  count := count and $1f; // use only 5 LSB's


    if Boolean(count=0) then exit; (* NOP *)
    // count is 1..31

    (* op1 is a register or memory reference *)
    if Boolean(i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    temp_32 := (op1_16 shl 16)or(op2_16); // double formed by op1:op2
    result_32 := temp_32 shl count;
    if Boolean(count > 16) then begin
      // hack to act like x86 SHLD when count > 16
      // actually shifting op1:op2:op2 shl count
      result_32 := result_32 or (op2_16 shl (count - 16));
      end;
    result_16 := result_32 shr 16;

    (* now write result back to destination *)
    if Boolean(i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, result_16);
      end
  else begin
      write_RMW_virtual_word(result_16);
      end;

    (* set eflags:
     * SHLD count affects the following flags: S,Z,P,C,O
     *)
    set_CF( (temp_32 shr (32 - count))  and $01 );
    if Boolean(count = 1) then
      set_OF(Bool(((op1_16 or result_16) and $8000) > 0));
    set_ZF(Bool(result_16 = 0));
    set_SF(result_16 shr 15);
    set_PF_base(Bit8u(result_16));
end;

procedure TCPU.SHRD_EwGw(I: PInstruction_tag);
var
  op1_16, op2_16, result_16:Bit16u;
  temp_32, result_32:Bit32u;
  count:unsigned;
begin

  if Boolean(i^.b1 = $1ac) then
    count := i^.Ib
  else // $1ad
    count := CL;
  count := count and $1F; (* use only 5 LSB's *)

  if Boolean(count=0) then exit; (* NOP *)

    // count is 1..31

    (* op1 is a register or memory reference *)
    if Boolean(i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;
    op2_16 := BX_READ_16BIT_REG(i^.nnn);

    temp_32 := (op2_16 shl 16) or op1_16; // double formed by op2:op1
    result_32 := temp_32 shr count;
    if Boolean(count > 16) then begin
      // hack to act like x86 SHLD when count > 16
      // actually shifting op2:op2:op1 shr count
      result_32 := result_32 or (op2_16 shl (32 - count));
      end;
    result_16 := result_32;

    (* now write result back to destination *)
    if Boolean(i^.mod_ = $c0) then begin
      BX_WRITE_16BIT_REG(i^.rm, result_16);
      end
  else begin
      write_RMW_virtual_word(result_16);
      end;

    (* set eflags:
     * SHRD count affects the following flags: S,Z,P,C,O
     *)

    set_CF((temp_32 shr (count - 1))  and $01);
    set_ZF(Bool(result_16 = 0));
    set_SF(result_16 shr 15);
    (* for shift of 1, OF set if sign change occurred. *)
    if Boolean(count = 1) then
      set_OF(Bool(((op1_16 or result_16)  and $8000) > 0));
    set_PF_base(Bit8u(result_16));
end;

procedure TCPU.ROL_Ew(I: PInstruction_tag);
var
  op1_16, result_16:Bit16u;
  count:unsigned;
begin

  if Boolean( i^.b1 = $c1 ) then
    count := i^.Ib
  else if Boolean( i^.b1 = $d1 ) then
    count := 1
  else // $d3
    count := CL;

    count := count and $0f; // only use bottom 4 bits

    (* op1 is a register or memory reference *)
    if Boolean(i^.mod_ = $c0) then begin
      op1_16 := BX_READ_16BIT_REG(i^.rm);
      end
  else begin
      (* pointer, segment address pair *)
      read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);
      end;

    if Boolean(count) then begin
      result_16 := (op1_16 shl count)or(op1_16 shr (16 - count));

      (* now write result back to destination *)
      if Boolean(i^.mod_ = $c0) then begin
        BX_WRITE_16BIT_REG(i^.rm, result_16);
        end
      else begin
        write_RMW_virtual_word(result_16);
        end;

      (* set eflags:
       * ROL count affects the following flags: C
       *)

      set_CF(result_16  and $01);
      if Boolean(count = 1) then
        set_OF(Bool(((op1_16 or result_16)  and $8000) > 0));
      end;
end;

procedure TCPU.ROR_Ew(I: PInstruction_tag);
var
  op1_16, result_16, result_b15: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;// $d3

  count := count and $0f;  // use only 4 LSB's

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  if Boolean(count) then
  begin
    result_16 := (op1_16 shr count)or(op1_16 shl (16 - count));
    (* now write result back to destination *)
    if Boolean(i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, result_16)
      else write_RMW_virtual_word(result_16);

    (* set eflags:
     * ROR count affects the following flags: C
     *)
    result_b15 := result_16  and $8000;

    set_CF(Bool(result_b15 <> 0));
    if Boolean(count = 1) then
      set_OF(Bool(((op1_16 or result_16)  and $8000) > 0));
  end;
end;

procedure TCPU.RCL_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;

  count := count and $1F;

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  count := count mod 17;

  if Boolean(count = 0) then exit;

  if Boolean(count = 1)
    then result_16 := (op1_16 shl 1) or get_CF()
    else
  if Boolean(count=16)
    then result_16 := (get_CF() shl 15) or (op1_16 shr 1)
    else result_16 := (op1_16 shl count) or (get_CF() shl (count - 1)) or (op1_16 shr (17 - count));

  (* now write result back to destination *)
  if Boolean(i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);

  (* set eflags:
   * RCL count affects the following flags: C
   *)

  if Boolean(count = 1) then
    set_OF(Bool(((op1_16 or result_16)  and $8000) > 0));
  set_CF((op1_16 shr (16 - count))  and $01);
end;

procedure TCPU.RCR_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;

  count := count  and $1F;

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  count := count mod 17;
  if Boolean(count) then
  begin
    result_16 := (op1_16 shr count) or	(get_CF() shl (16 - count)) or
                 (op1_16 shl (17 - count));
    (* now write result back to destination *)
    if Boolean(i^.mod_ = $c0)
      then BX_WRITE_16BIT_REG(i^.rm, result_16)
      else write_RMW_virtual_word(result_16);
    (* set eflags:
     * RCR count affects the following flags: C
     *)
    set_CF((op1_16 shr (count - 1))  and $01);
    if Boolean(count = 1) then
      set_OF(Bool(((op1_16 or result_16)  and $8000) > 0));
  end;
end;

procedure TCPU.SHL_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;

  count := count and $1F; (* use only 5 LSB's *)

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  if Boolean(count = 0) then exit;

  result_16 := (op1_16 shl count);

  (* now write result back to destination *)
  if Boolean(i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);

  SET_FLAGS_OSZAPC_16(op1_16, count, result_16, BX_INSTR_SHL16);
end;

procedure TCPU.SHR_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;

  count := count and $1F; (* use only 5 LSB's *)

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  if Boolean(count = 0) then exit;

  result_16 := (op1_16 shr count);

  (* now write result back to destination *)
  if Boolean(i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);

  SET_FLAGS_OSZAPC_16(op1_16, count, result_16, BX_INSTR_SHR16);
end;

procedure TCPU.SAR_Ew(I: PInstruction_tag);
var
  op1_16, result_16: Bit16u;
  count: unsigned;
begin
  if Boolean( i^.b1 = $c1 )
    then count := i^.Ib
    else
  if Boolean( i^.b1 = $d1 )
    then count := 1
    else count := CL;

  count := count and $1F;  (* use only 5 LSB's *)

  (* op1 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_16 := BX_READ_16BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_RMW_virtual_word(i^.seg, i^.rm_addr, @op1_16);

  if Boolean(count = 0) then exit;

  if Boolean(count < 16) then
  begin
    if Boolean(op1_16  and $8000)
      then result_16 := (op1_16 shr count)or($ffff shl (16 - count))
      else result_16 := (op1_16 shr count);
  end else
  begin
    result_16 := 0 - Bit16u(Boolean(op1_16  and $8000));
//    if Boolean(op1_16  and $8000) then begin
//result_16 := $ffff;
//end
//    else begin
//result_16 := 0;
//end;
  end;

    (* now write result back to destination *)
  if Boolean(i^.mod_ = $c0)
    then BX_WRITE_16BIT_REG(i^.rm, result_16)
    else write_RMW_virtual_word(result_16);
    (* set eflags:
     * SAR count affects the following flags: S,Z,P,C
     *)
  if Boolean(count < 16)
    then set_CF((op1_16 shr (count - 1))  and $01)
    else set_CF(Bool(Boolean(op1_16  and $8000)));
//    begin
//      if Boolean(op1_16  and $8000)
//        then set_CF(1)
//        else set_CF(0);
//    end;

  set_ZF(Bool(result_16 = 0));
  set_SF(result_16 shr 15);
  if Boolean(count = 1) then
    set_OF(0);
  set_PF_base(Bit8u(result_16));
end;

procedure TCPU.POP_Ed(I: PInstruction_tag);
var
  val32: Bit32u;
begin
  pop_32(@val32);
  if (i^.mod_ = $c0)
    then BX_WRITE_32BIT_REG(i^.rm, val32)
    else
    begin
      // Note: there is one little weirdism here.  When 32bit addressing
      // is used, it is possible to use ESP in the mod_rm addressing.
      // If used, the value of ESP after the pop is used to calculate
      // the address.
      if ((i^.as_32<>0) and (i^.mod_<>$c0) and (i^.rm=4) and (i^.base=4)) then
        // call method on BX_CPU_C object
        i^.Resolvemodrm(i);
      write_virtual_dword(i^.seg, i^.rm_addr, @val32);
    end;
end;

procedure TCPU.PUSH_ERX(I: PInstruction_tag);
begin
  push_32(self.gen_reg[i^.b1 and $07].erx);
end;

procedure TCPU.POP_ERX(I: PInstruction_tag);
var
  erx: Bit32u;
begin
  pop_32(@erx);
  self.gen_reg[i^.b1  and $07].erx := erx;
end;

procedure TCPU.PUSH_CS(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_CS].selector.value)
    else push_16(self.sregs[SEG_REG_CS].selector.value);
end;

procedure TCPU.PUSH_DS(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_DS].selector.value)
    else push_16(self.sregs[SEG_REG_DS].selector.value);
end;

procedure TCPU.PUSH_ES(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_ES].selector.value)
    else push_16(self.sregs[SEG_REG_ES].selector.value);
end;

procedure TCPU.PUSH_FS(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_FS].selector.value)
    else push_16(self.sregs[SEG_REG_FS].selector.value);
end;

procedure TCPU.PUSH_GS(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_GS].selector.value)
    else push_16(self.sregs[SEG_REG_GS].selector.value);
end;

procedure TCPU.PUSH_SS(I: PInstruction_tag);
begin
  if (i^.os_32) <> 0
    then push_32(self.sregs[SEG_REG_SS].selector.value)
    else push_16(self.sregs[SEG_REG_SS].selector.value);
end;

procedure TCPU.POP_DS(I: PInstruction_tag);
var
  ds: Bit32u;
  ds_16: Bit16u;
begin
  if (i^.os_32) <> 0 then
  begin
    pop_32(@ds);
    load_seg_reg(@self.sregs[SEG_REG_DS], Bit16u(ds));
  end else
  begin
    pop_16(@ds_16);
    load_seg_reg(@self.sregs[SEG_REG_DS], ds_16);
  end;
end;

procedure TCPU.POP_ES(I: PInstruction_tag);
var
  es: Bit32u;
  es_16: Bit16u;
begin
  if (i^.os_32) <> 0 then
  begin
    pop_32(@es);
    load_seg_reg(@self.sregs[SEG_REG_ES], Bit16u(es));
  end else
  begin
    pop_16(@es_16);
    load_seg_reg(@self.sregs[SEG_REG_ES], es_16);
  end;
end;

procedure TCPU.POP_FS(I: PInstruction_tag);
var
  fs: Bit32u;
begin
  if (i^.os_32) <> 0 then
  begin
    pop_32(@fs);
    load_seg_reg(@self.sregs[SEG_REG_FS], Bit16u(fs));
  end else
  begin
    pop_16(@fs);
    load_seg_reg(@self.sregs[SEG_REG_FS], fs);
  end;
end;

procedure TCPU.POP_GS(I: PInstruction_tag);
var
  gs: Bit32u;
begin
  if (i^.os_32) <> 0 then
  begin
    pop_32(@gs);
    load_seg_reg(@self.sregs[SEG_REG_GS], Bit16u(gs));
  end else
  begin
    pop_16(@gs);
    load_seg_reg(@self.sregs[SEG_REG_GS], gs);
  end;
end;

procedure TCPU.POP_SS(I: PInstruction_tag);
var
  ss: Bit32u;
begin
  if (i^.os_32) <> 0 then
  begin
    pop_32(@ss);
    load_seg_reg(@self.sregs[SEG_REG_SS], Bit16u(ss));
  end else
  begin
    pop_16(@ss);
    load_seg_reg(@self.sregs[SEG_REG_SS], ss);
  end;
  // POP SS inhibits interrupts, debug exceptions and single-step
  // trap exceptions until the execution boundary following the
  // next instruction is reached.
  // Same code as MOV_SwEw()
  self.inhibit_mask := self.inhibit_mask or BX_INHIBIT_INTERRUPTS or BX_INHIBIT_DEBUG;
  self.async_event := 1;
end;

procedure TCPU.PUSHAD32(I: PInstruction_tag);
var
  temp_ESP: Bit32u;
  esp_: Bit32u;
begin
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_ESP := ESP
    else temp_ESP := SP;

  if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))) <> 0 then
    if ( can_push(@self.sregs[SEG_REG_SS].cache, temp_ESP, 32) = 0) then
    begin
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;

  esp_ := ESP;

  (* ??? optimize this by using virtual write, all checks passed *)
  push_32(EAX);
  push_32(ECX);
  push_32(EDX);
  push_32(EBX);
  push_32(esp_);
  push_32(EBP);
  push_32(ESI);
  push_32(EDI);
end;

procedure TCPU.POPAD32(I: PInstruction_tag);
var
  edi_, esi_, ebp_, etmp_, ebx_, edx_, ecx_, eax_: Bit32u;
begin
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
    if Boolean( can_pop(32) = 0) then
    begin
      exception2([BX_SS_EXCEPTION, 0, 0]);
      exit;
    end;

  (* ??? optimize this *)
  pop_32(@edi_);
  pop_32(@esi_);
  pop_32(@ebp_);
  pop_32(@etmp_); (* value for ESP discarded *)
  pop_32(@ebx_);
  pop_32(@edx_);
  pop_32(@ecx_);
  pop_32(@eax_);

  EDI := edi_;
  ESI := esi_;
  EBP := ebp_;
  EBX := ebx_;
  EDX := edx_;
  ECX := ecx_;
  EAX := eax_;
end;

procedure TCPU.PUSH_Id(I: PInstruction_tag);
var
  imm32: Bit32u;
begin
  imm32 := i^.Id;
  push_32(imm32);
end;

procedure TCPU.PUSH_Ed(I: PInstruction_tag);
var
  op1_32: Bit32u;
begin
  (* op1_32 is a register or memory reference *)
  if Boolean(i^.mod_ = $c0)
    then op1_32 := BX_READ_32BIT_REG(i^.rm)
        (* pointer, segment address pair *)
    else read_virtual_dword(i^.seg, i^.rm_addr, @op1_32);

  push_32(op1_32);
end;

procedure TCPU.ENTER_IwIb(I: PInstruction_tag);
var
  frame_ptr32: Bit32u;
  frame_ptr16: Bit16u;
  level: Bit8u;
//  first_time: Bit8u;
  bytes_to_push, temp_ESP: Bit32u;
  temp32: Bit32u;
  temp16: Bit16u;
begin
//  first_time := 1;
  level := i^.Ib2;
  invalidate_prefetch_q();
  level := level mod 32;
(* ??? *)
  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    if (level = 0) then
    begin
      if (i^.os_32) <> 0
        then bytes_to_push := 4 + i^.Iw
        else bytes_to_push := 2 + i^.Iw;
    end else
    begin (* level > 0 *)
      if (i^.os_32) <> 0
        then bytes_to_push := 4 + (level-1)*4 + 4 + i^.Iw
        else bytes_to_push := 2 + (level-1)*2 + 2 + i^.Iw;
    end;
    if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
      then temp_ESP := ESP
      else temp_ESP := SP;
    if ( can_push(@self.sregs[SEG_REG_SS].cache, temp_ESP, bytes_to_push) = 0) then
      exception2([BX_SS_EXCEPTION, 0, 0]);
  end;

  if (i^.os_32) <> 0
    then push_32(EBP)
    else push_16(BP);

  // can just do frame_ptr32 := ESP for either case ???
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then frame_ptr32 := ESP
    else frame_ptr32 := SP;

  if (level > 0) then
  begin
    (* do level-1 times *)
    while (level > 0) do
    begin
      if (i^.os_32) <> 0 then
      begin
        if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
        begin (* 32bit stacksize *)
          EBP := EBP - 4;
          read_virtual_dword(SEG_REG_SS, EBP, @temp32);
          ESP := ESP - 4;
          write_virtual_dword(SEG_REG_SS, ESP, @temp32);
        end else
        begin (* 16bit stacksize *)
          BP := BP - 4;
          read_virtual_dword(SEG_REG_SS, BP, @temp32);
          SP := SP - 4;
          write_virtual_dword(SEG_REG_SS, SP, @temp32);
        end;
      end else
      begin (* 16bit opsize *)
        if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
        begin (* 32bit stacksize *)
          EBP := EBP - 2;
          read_virtual_word(SEG_REG_SS, EBP, @temp16);
          ESP := ESP - 2;
          write_virtual_word(SEG_REG_SS, ESP, @temp16);
        end else
        begin (* 16bit stacksize *)
          BP := BP - 2;
          read_virtual_word(SEG_REG_SS, BP, @temp16);
          SP := SP - 2;
          write_virtual_word(SEG_REG_SS, SP, @temp16);
        end;
      end;
        Dec(level);
    end; (* while (--level) *)

    (* push(frame pointer) *)
    if (i^.os_32) <> 0 then
    begin
      if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
      begin (* 32bit stacksize *)
        ESP := ESP - 4;
        write_virtual_dword(SEG_REG_SS, ESP, @frame_ptr32);
      end else
      begin
        SP := SP - 4;
        write_virtual_dword(SEG_REG_SS, SP, @frame_ptr32);
      end;
    end else
    begin (* 16bit opsize *)
      if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0 then
      begin (* 32bit stacksize *)
        frame_ptr16 := frame_ptr32;
        ESP := ESP - 2;
        write_virtual_word(SEG_REG_SS, ESP, @frame_ptr16);
      end else
      begin
        frame_ptr16 := frame_ptr32;
        SP := SP - 2;
        write_virtual_word(SEG_REG_SS, SP, @frame_ptr16);
      end;
    end;
  end; (* if Boolean(level > 0) ... *)

  if (i^.os_32) <> 0
    then EBP := frame_ptr32
    else BP := frame_ptr32;

  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then ESP := ESP - i^.Iw
    else SP := SP - i^.Iw;
end;

procedure TCPU.LEAVE(I: PInstruction_tag);
var
  temp_EBP: Bit32u;
  temp32: Bit32u;
  temp16: Bit16u;
begin
  invalidate_prefetch_q();

  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then temp_EBP := EBP
    else temp_EBP := BP;

  if ( Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0)) ) <> 0 then
  begin
    if (self.sregs[SEG_REG_SS].cache.segment.c_ed)<>0 then
    begin (* expand up *)
      if (temp_EBP <= self.sregs[SEG_REG_SS].cache.segment.limit_scaled) then
      begin
        exception2([BX_SS_EXCEPTION, 0, 0]);
        exit;
      end;
    end else
    begin (* normal *)
      if (temp_EBP > self.sregs[SEG_REG_SS].cache.segment.limit_scaled) then
      begin
        exception2([BX_SS_EXCEPTION, 0, 0]);
        exit;
      end;
    end;
  end;
  // delete frame
  if (self.sregs[SEG_REG_SS].cache.segment.d_b) <> 0
    then ESP := EBP
    else SP := BP;

  // restore frame pointer
  if (i^.os_32) <> 0 then
  begin
    pop_32(@temp32);
    EBP := temp32;
  end else
  begin

    pop_16(@temp16);
    BP := temp16;
  end;
end;

procedure TCPU.stack_return_to_v86(new_eip:Bit32u; raw_cs_selector:Bit32u;flags32:Bit32u);
var
  temp_ESP, new_esp, esp_laddr: Bit32u;
  raw_es_selector, raw_ds_selector, raw_fs_selector,
         raw_gs_selector, raw_ss_selector: Bit16u;
begin
  // Must be 32bit effective opsize, VM is in upper 16bits of eFLAGS
  // CPL := 0 to get here

  // ----------------
  //or   orOLD GSoreSP+32
  //or   orOLD FSoreSP+28
  //or   orOLD DSoreSP+24
  //or   orOLD ESoreSP+20
  //or   orOLD SSoreSP+16
  //or OLD ESP    oreSP+12
  //orOLD EFLAGS  oreSP+8
  //or   orOLD CSoreSP+4
  //or OLD EIP    oreSP+0
  // ----------------

  if Boolean(Self.sregs[SEG_REG_SS].cache.segment.d_b)
    then temp_ESP := ESP
    else temp_ESP := SP;

  // top 36 bytes of stack must be within stack limits, else #GP(0)
  if Boolean( can_pop(36) = 0) then
  begin
    exception2([BX_SS_EXCEPTION, 0, 0]);
    exit;
  end;

  if Boolean( new_eip  and $ffff0000 ) then
    new_eip := new_eip  and $ffff;

  esp_laddr := Self.sregs[SEG_REG_SS].cache.segment.base + temp_ESP;

  // load SS:ESP from stack
  access_linear(esp_laddr + 12, 4, 0, BX_READ, @new_esp);
  access_linear(esp_laddr + 16, 2, 0, BX_READ, @raw_ss_selector);

  // load ES,DS,FS,GS from stack
  access_linear(esp_laddr + 20, 2, 0, BX_READ, @raw_es_selector);
  access_linear(esp_laddr + 24, 2, 0, BX_READ, @raw_ds_selector);
  access_linear(esp_laddr + 28, 2, 0, BX_READ, @raw_fs_selector);
  access_linear(esp_laddr + 32, 2, 0, BX_READ, @raw_gs_selector);

  write_eflags(flags32, (*change IOPL*) 1, (*change IF*) 1,
                  (*change VM*) 1, (*change RF*) 1);

  // load CS:EIP from stack; already read and passed as args
  Self.sregs[SEG_REG_CS].selector.value := raw_cs_selector;
  FEIP := new_eip;

  Self.sregs[SEG_REG_ES].selector.value := raw_es_selector;
  Self.sregs[SEG_REG_DS].selector.value := raw_ds_selector;
  Self.sregs[SEG_REG_FS].selector.value := raw_fs_selector;
  Self.sregs[SEG_REG_GS].selector.value := raw_gs_selector;
  Self.sregs[SEG_REG_SS].selector.value := raw_ss_selector;
  ESP := new_esp; // Full 32bits are loaded.

  init_v8086_mode();
end;

procedure TCPU.stack_return_from_v86(I: PInstruction_tag);
var
//  times: Bit32u;
  eip_, ecs_raw, eflags: Bit32u;
  ip_, cs_raw, flags: Bit16u;
begin
//  Inc(Times);
  if Boolean(IOPL <> 3) then
    // trap to virtual 8086 monitor
    exception2([BX_GP_EXCEPTION, 0, 0]);

  if Boolean(i^.os_32) then
  begin
// ??? should be some stack checks here
    pop_32(@eip_);
    pop_32(@ecs_raw);
    pop_32(@eflags);

    load_seg_reg(@Self.sregs[SEG_REG_CS], Bit16u(ecs_raw));
    Self.FEIP := eip_;
    write_eflags(eflags, (*IOPL*) Bool(CPL=0), (*IF*) 1, (*VM*) 0, (*RF*) 1);
  end else
  begin
// ??? should be some stack checks here
    pop_16(@ip_);
    pop_16(@cs_raw);
    pop_16(@flags);

    load_seg_reg(@Self.sregs[SEG_REG_CS], cs_raw);
    Self.FEIP := Bit32u(ip_);
    write_flags(flags, (*IOPL*) Bool(CPL=0), (*IF*) 1);
  end;
end;

procedure TCPU.init_v8086_mode;
begin
  Self.sregs[SEG_REG_CS].cache.valid                := 1;
  Self.sregs[SEG_REG_CS].cache.p                    := 1;
  Self.sregs[SEG_REG_CS].cache.dpl                  := 3;
  Self.sregs[SEG_REG_CS].cache.segmentType          := 1;
  Self.sregs[SEG_REG_CS].cache.segment.executable   := 1;
  Self.sregs[SEG_REG_CS].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_CS].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_CS].cache.segment.a            := 1;
  Self.sregs[SEG_REG_CS].cache.segment.base         :=
    Self.sregs[SEG_REG_CS].selector.value shl 4;
  Self.sregs[SEG_REG_CS].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_CS].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_CS].cache.segment.g            := 0;
  Self.sregs[SEG_REG_CS].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_CS].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_CS].selector.rpl                 := 3;

  Self.sregs[SEG_REG_SS].cache.valid                  := 1;
  Self.sregs[SEG_REG_SS].cache.p                      := 1;
  Self.sregs[SEG_REG_SS].cache.dpl                    := 3;
  Self.sregs[SEG_REG_SS].cache.segmentType                := 1;
  Self.sregs[SEG_REG_SS].cache.segment.executable   := 0;
  Self.sregs[SEG_REG_SS].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_SS].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_SS].cache.segment.a            := 1;
  Self.sregs[SEG_REG_SS].cache.segment.base         :=
    Self.sregs[SEG_REG_SS].selector.value shl 4;
  Self.sregs[SEG_REG_SS].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_SS].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_SS].cache.segment.g            := 0;
  Self.sregs[SEG_REG_SS].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_SS].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_SS].selector.rpl                 := 3;

  Self.sregs[SEG_REG_ES].cache.valid                  := 1;
  Self.sregs[SEG_REG_ES].cache.p                      := 1;
  Self.sregs[SEG_REG_ES].cache.dpl                    := 3;
  Self.sregs[SEG_REG_ES].cache.segmentType                := 1;
  Self.sregs[SEG_REG_ES].cache.segment.executable   := 0;
  Self.sregs[SEG_REG_ES].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_ES].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_ES].cache.segment.a            := 1;
  Self.sregs[SEG_REG_ES].cache.segment.base         :=
    Self.sregs[SEG_REG_ES].selector.value shl 4;
  Self.sregs[SEG_REG_ES].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_ES].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_ES].cache.segment.g            := 0;
  Self.sregs[SEG_REG_ES].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_ES].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_ES].selector.rpl                 := 3;

  Self.sregs[SEG_REG_DS].cache.valid                  := 1;
  Self.sregs[SEG_REG_DS].cache.p                      := 1;
  Self.sregs[SEG_REG_DS].cache.dpl                    := 3;
  Self.sregs[SEG_REG_DS].cache.segmentType                := 1;
  Self.sregs[SEG_REG_DS].cache.segment.executable   := 0;
  Self.sregs[SEG_REG_DS].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_DS].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_DS].cache.segment.a            := 1;
  Self.sregs[SEG_REG_DS].cache.segment.base         :=
    Self.sregs[SEG_REG_DS].selector.value shl 4;
  Self.sregs[SEG_REG_DS].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_DS].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_DS].cache.segment.g            := 0;
  Self.sregs[SEG_REG_DS].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_DS].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_DS].selector.rpl                 := 3;

  Self.sregs[SEG_REG_FS].cache.valid                  := 1;
  Self.sregs[SEG_REG_FS].cache.p                      := 1;
  Self.sregs[SEG_REG_FS].cache.dpl                    := 3;
  Self.sregs[SEG_REG_FS].cache.segmentType                := 1;
  Self.sregs[SEG_REG_FS].cache.segment.executable   := 0;
  Self.sregs[SEG_REG_FS].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_FS].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_FS].cache.segment.a            := 1;
  Self.sregs[SEG_REG_FS].cache.segment.base         :=
    Self.sregs[SEG_REG_FS].selector.value shl 4;
  Self.sregs[SEG_REG_FS].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_FS].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_FS].cache.segment.g            := 0;
  Self.sregs[SEG_REG_FS].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_FS].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_FS].selector.rpl                 := 3;

  Self.sregs[SEG_REG_GS].cache.valid                  := 1;
  Self.sregs[SEG_REG_GS].cache.p                      := 1;
  Self.sregs[SEG_REG_GS].cache.dpl                    := 3;
  Self.sregs[SEG_REG_GS].cache.segmentType                := 1;
  Self.sregs[SEG_REG_GS].cache.segment.executable   := 0;
  Self.sregs[SEG_REG_GS].cache.segment.c_ed         := 0;
  Self.sregs[SEG_REG_GS].cache.segment.r_w          := 1;
  Self.sregs[SEG_REG_GS].cache.segment.a            := 1;
  Self.sregs[SEG_REG_GS].cache.segment.base         :=
    Self.sregs[SEG_REG_GS].selector.value shl 4;
  Self.sregs[SEG_REG_GS].cache.segment.limit        := $ffff;
  Self.sregs[SEG_REG_GS].cache.segment.limit_scaled := $ffff;
  Self.sregs[SEG_REG_GS].cache.segment.g            := 0;
  Self.sregs[SEG_REG_GS].cache.segment.d_b          := 0;
  Self.sregs[SEG_REG_GS].cache.segment.avl          := 0;
  Self.sregs[SEG_REG_GS].selector.rpl                 := 3;
end;


procedure TCPU.MOVSB_XbYb(I: PInstruction_tag);
var
  seg: unsigned;
  temp8: Bit8u;
  esi_, edi_: Bit32u;
  si_, di_: Bit16u;
begin
  if (i^.seg and SEG_REG_NULL) = 0
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;
    edi_ := EDI;

    read_virtual_byte(seg, esi_, @temp8);

    write_virtual_byte(SEG_REG_ES, edi_, @temp8);

    if (Self.eflags.df) <> 0 then
    begin
      (* decrement ESI, EDI *)
      dec(esi_);
      dec(edi_);
    end else
    begin
      (* increment ESI, EDI *)
      inc(esi_);
      inc(edi_);
    end;

    ESI := esi_;
    EDI := edi_;
  end else
  begin (* 16 bit address mod_e *)

    si_ := SI;
    di_ := DI;

    read_virtual_byte(seg, si_, @temp8);

    write_virtual_byte(SEG_REG_ES, di_, @temp8);

    if (Self.eflags.df) <> 0 then
    begin
      (* decrement SI, DI *)
      dec(si_);
      dec(di_);
    end else
    begin
      (* increment SI, DI *)
      inc(si_);
      inc(di_);
    end;

    SI := si_;
    DI := di_;
  end;
end;

procedure TCPU.MOVSW_XvYv(I: PInstruction_tag);
var
  seg: unsigned;
  temp32: Bit32u;

  esi_, edi_: Bit32u;
  temp16: Bit16u;
  si_, di_: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL) = 0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;
    edi_ := EDI;

    if (i^.os_32) <> 0 then
    begin
      read_virtual_dword(seg, esi_, @temp32);

      write_virtual_dword(SEG_REG_ES, edi_, @temp32);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(esi_, 4);
        dec(edi_, 4);
      end else
      begin
        (* increment ESI *)
        inc(esi_, 4);
        inc(edi_, 4);
      end;
    end else (* if Boolean(i^.os_32) ... *)
    begin (* 16 bit opsize mod_e *)
      read_virtual_word(seg, esi_, @temp16);
      write_virtual_word(SEG_REG_ES, edi_, @temp16);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(esi_,2);
        dec(edi_,2);
      end else
      begin
        (* increment ESI *)
        inc(esi_,2);
        inc(edi_,2);
      end;
    end;

    ESI := esi_;
    EDI := edi_;
  end else
  begin (* 16bit address mod_e *)

    si_ := SI;
    di_ := DI;

    if (i^.os_32)<>0 then
    begin

      read_virtual_dword(seg, si_, @temp32);

      write_virtual_dword(SEG_REG_ES, di_, @temp32);

      if (Self.eflags.df)<>0 then begin
        (* decrement ESI *)
        dec(si_,4);
        dec(di_,4);
        end
      else begin
        (* increment ESI *)
        inc(si_,4);
        inc(di_,4);
        end;
    end else (* if Boolean(i^.os_32) ... *)
    begin (* 16 bit opsize mod_e *)
      read_virtual_word(seg, si_, @temp16);
      write_virtual_word(SEG_REG_ES, di_, @temp16);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement SI, DI *)
        dec(si_, 2);
        dec(di_, 2);
      end else
      begin
        (* increment SI, DI *)
        inc(si_, 2);
        inc(di_, 2);
      end;
    end;

    SI := si_;
    DI := di_;
  end;
end;

procedure TCPU.CMPSB_XbYb(I: PInstruction_tag);
var
  seg: unsigned;
  op1_8, op2_8, diff_8: Bit8u;
  esi_, edi_: Bit32u;
  si_, di_: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL)=0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;
    edi_ := EDI;
    read_virtual_byte(seg, esi_, @op1_8);
    read_virtual_byte(SEG_REG_ES, edi_, @op2_8);
    diff_8 := op1_8 - op2_8;
    SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMPS8);

    if (Self.eflags.df) <> 0 then
    begin
      (* decrement ESI *)
      dec(esi_);
      dec(edi_);
    end else
    begin
      (* increment ESI *)
      inc(esi_);
      inc(edi_);
    end;

    EDI := edi_;
    ESI := esi_;
  end else
  begin (* 16bit address mod_e *)
    si_ := SI;
    di_ := DI;
    read_virtual_byte(seg, si_, @op1_8);
    read_virtual_byte(SEG_REG_ES, di_, @op2_8);
    diff_8 := op1_8 - op2_8;
    SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_CMPS8);

    if (Self.eflags.df) <> 0 then
    begin
      (* decrement ESI *)
      dec(si_);
      dec(di_);
    end else
    begin
      (* increment ESI *)
      inc(si_);
      inc(di_);
    end;

    DI := di_;
    SI := si_;
  end;
end;

procedure TCPU.CMPSW_XvYv(I: PInstruction_tag);
var
  seg: unsigned;
  op1_32, op2_32, diff_32: Bit32u;
  esi_, edi_: Bit32u;
  op1_16, op2_16, diff_16: Bit16u;
  si_, di_: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL) = 0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;
    edi_ := EDI;

    if (i^.os_32) <> 0 then
    begin
      read_virtual_dword(seg, esi_, @op1_32);
      read_virtual_dword(SEG_REG_ES, edi_, @op2_32);

      diff_32 := op1_32 - op2_32;
      SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMPS32);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(esi_, 4);
        dec(edi_, 4);
      end else
      begin
        (* increment ESI *)
        inc(esi_, 4);
        inc(edi_, 4);
      end;
    end else
    begin (* 16 bit opsize *)
      read_virtual_word(seg, esi_, @op1_16);
      read_virtual_word(SEG_REG_ES, edi_, @op2_16);

      diff_16 := op1_16 - op2_16;

      SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMPS16);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(esi_, 2);
        dec(edi_, 2);
      end else
      begin
        (* increment ESI *)
        inc(esi_, 2);
        inc(edi_, 2);
      end;
    end;

    EDI := edi_;
    ESI := esi_;
  end else
  begin (* 16 bit address mod_e *)
    si_ := SI;
    di_ := DI;

    if (i^.os_32) <> 0 then
    begin
      read_virtual_dword(seg, si_, @op1_32);
      read_virtual_dword(SEG_REG_ES, di_, @op2_32);

      diff_32 := op1_32 - op2_32;

      SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_CMPS32);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(si_, 4);
        dec(di_, 4);
      end else
      begin
        (* increment ESI *)
        inc(si_, 4);
        inc(di_, 4);
      end;
    end else
    begin (* 16 bit opsize *)
      read_virtual_word(seg, si_, @op1_16);
      read_virtual_word(SEG_REG_ES, di_, @op2_16);

      diff_16 := op1_16 - op2_16;

      SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_CMPS16);

      if (Self.eflags.df) <> 0 then
      begin
        (* decrement ESI *)
        dec(si_, 2);
        dec(di_, 2);
      end else
      begin
        (* increment ESI *)
        inc(si_, 2);
        inc(di_, 2);
      end;
    end;

    DI := di_;
    SI := si_;
  end;
end;

procedure TCPU.SCASB_ALXb(I: PInstruction_tag);
var
  op1_8, op2_8, diff_8: Bit8u;
  edi_: Bit32u;
  di_: Bit16u;
begin
  if (i^.as_32) <> 0 then
  begin
    edi_ := EDI;
    op1_8 := AL;
    read_virtual_byte(SEG_REG_ES, edi_, @op2_8);
    diff_8 := op1_8 - op2_8;
    SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SCAS8);

    if (Self.eflags.df) <> 0
      then dec(edi_) (* decrement ESI *)
      else inc(edi_); (* increment ESI *)

    EDI := edi_;
  end else
  begin (* 16bit address mod_e *)
    di_ := DI;
    op1_8 := AL;
    read_virtual_byte(SEG_REG_ES, di_, @op2_8);
    diff_8 := op1_8 - op2_8;
    SET_FLAGS_OSZAPC_8(op1_8, op2_8, diff_8, BX_INSTR_SCAS8);

    if (Self.eflags.df) <> 0
      then dec(di_) (* decrement ESI *)
      else inc(di_); (* increment ESI *)

    DI := di_;
  end;
end;

procedure TCPU.SCASW_eAXXv(I: PInstruction_tag);
var
  edi_: Bit32u;
  op1_32, op2_32, diff_32: Bit32u;
  op1_16, op2_16, diff_16: Bit16u;
  di_:Bit16u;
begin
  if (i^.as_32) <> 0 then
  begin
    edi_ := EDI;

    if (i^.os_32) <> 0 then
    begin
      op1_32 := EAX;
      read_virtual_dword(SEG_REG_ES, edi_, @op2_32);

      diff_32 := op1_32 - op2_32;

      SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SCAS32);

      if (Self.eflags.df) <> 0
        then dec(edi_, 4)
        else inc(edi_, 4);
    end else
    begin (* 16 bit opsize *)
      op1_16 := AX;
      read_virtual_word(SEG_REG_ES, edi_, @op2_16);

      diff_16 := op1_16 - op2_16;

      SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SCAS16);

      if (Self.eflags.df) <> 0
        then dec(edi_, 2)
        else inc(edi_, 2);
    end;

    EDI := edi_;
  end else
  begin (* 16bit address mod_e *)
    di_ := DI;

    if (i^.os_32) <> 0 then
    begin
      op1_32 := EAX;
      read_virtual_dword(SEG_REG_ES, di_, @op2_32);
      diff_32 := op1_32 - op2_32;
      SET_FLAGS_OSZAPC_32(op1_32, op2_32, diff_32, BX_INSTR_SCAS32);

      if (Self.eflags.df) <> 0
        then dec(di_, 4)
        else inc(di_, 4);
    end else
    begin (* 16 bit opsize *)
      op1_16 := AX;
      read_virtual_word(SEG_REG_ES, di_, @op2_16);

      diff_16 := op1_16 - op2_16;

      SET_FLAGS_OSZAPC_16(op1_16, op2_16, diff_16, BX_INSTR_SCAS16);

      if (Self.eflags.df) <> 0
        then dec(di_, 2)
        else inc(di_, 2);
    end;

    DI := di_;
  end;
end;

procedure TCPU.STOSB_YbAL(I: PInstruction_tag);
var
  al_: Bit8u;
  edi_: Bit32u;
  di_: Bit16u;
begin
  if (i^.as_32) <> 0 then
  begin
    edi_ := EDI;
    al_ := AL;
    write_virtual_byte(SEG_REG_ES, edi_, @al_);

    if (Self.eflags.df) <> 0
      then dec(edi_)
      else inc(edi_);

    EDI := edi_;
  end else
  begin (* 16bit address size *)
    di_ := DI;
    al_ := AL;
    write_virtual_byte(SEG_REG_ES, di_, @al_);

    if (Self.eflags.df) <> 0
      then dec(di_)
      else inc(di_);

    DI := di_;
  end;
end;

procedure TCPU.STOSW_YveAX(I: PInstruction_tag);
var
  edi_: Bit32u;
  eax_: Bit32u;
  ax_: Bit16u;
  di_: Bit16u;
begin
  if (i^.as_32) <> 0 then
  begin
    edi_ := EDI;

    if (i^.os_32) <> 0 then
    begin
      eax_ := EAX;
      write_virtual_dword(SEG_REG_ES, edi_, @eax_);

      if (Self.eflags.df) <> 0
        then dec(edi_, 4)
        else  inc(edi_, 4);
    end else(* if Boolean(i^.os_32) ... *)
    begin (* 16 bit opsize mod_e *)

      ax_ := AX;
      write_virtual_word(SEG_REG_ES, edi_, @ax_);

      if (Self.eflags.df) <> 0
        then dec(edi_, 2)
        else inc(edi_, 2);
    end;

    EDI := edi_;
  end else
  begin (* 16bit address size *)
    di_ := DI;

    if (i^.os_32) <> 0 then
    begin
      eax_ := EAX;
      write_virtual_dword(SEG_REG_ES, di_, @eax_);

      if (Self.eflags.df) <> 0
        then dec(di_, 4)
        else inc(di_, 4);
    end else (* if Boolean(i^.os_32) ... *)
    begin (* 16 bit opsize mod_e *)
      ax_ := AX;
      write_virtual_word(SEG_REG_ES, di_, @ax_);

      if (Self.eflags.df) <> 0
        then dec(di_, 2)
        else inc(di_,2);
    end;

    DI := di_;
  end;
end;

procedure TCPU.LODSB_ALXb(I: PInstruction_tag);
var
  seg: unsigned;
  al_: Bit8u;
  esi_: Bit32u;
  si_: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL) = 0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;
    read_virtual_byte(seg, esi_, @al_);

    AL := al_;
    if (Self.eflags.df) <> 0
      then dec(esi_)
      else inc(esi_);

    ESI := esi_;
  end else
  begin (* 16bit address mod_e *)
    si_ := SI;

    read_virtual_byte(seg, si_, @al_);

    AL := al_;
    if (Self.eflags.df) <> 0
      then si_ := si - 1
      else si_ := si + 1;

    SI := si_;
  end;
end;

procedure TCPU.LODSW_eAXXv(I: PInstruction_tag);
var
  seg: unsigned;
  esi_: Bit32u;
  eax_: Bit32u;
  ax_: Bit16u;
  si_: Bit16u;
begin
  if ((i^.seg and SEG_REG_NULL) = 0)
    then seg := i^.seg
    else seg := SEG_REG_DS;

  if (i^.as_32) <> 0 then
  begin
    esi_ := ESI;

    if (i^.os_32) <> 0 then
    begin
      read_virtual_dword(seg, esi_, @eax_);

      EAX := eax_;
      if (Self.eflags.df) <> 0
        then dec(esi_, 4)
        else inc(esi_, 4);
    end else (* if Boolean(i^.os_32) ... *)
    begin (* 16 bit opsize mod_e *)
      read_virtual_word(seg, esi_, @ax_);

      AX := ax_;
      if (Self.eflags.df) <> 0
        then dec(esi_, 2)
        else inc(esi_, 2);
    end;

    ESI := esi_;
  end else
  begin (* 16bit address mod_e *)
    si_ := SI;

    if (i^.os_32) <> 0 then
    begin

      read_virtual_dword(seg, si_, @eax_);

      EAX := eax_;
      if (Self.eflags.df) <> 0
        then si_ := si - 4
        else si_ := si + 4;
    end else
    begin (* 16 bit opsize mod_e *)
      read_virtual_word(seg, si_, @ax_);
      AX := ax_;
      if (Self.eflags.df) <> 0
        then si_ := si - 2
        else si_ := si + 2;
    end;
    SI := si_;
  end;
end;

procedure TCPU.BxError(I: PInstruction_tag);
begin
  // extern void dump_core();
//  BX_INFO(Format('BxError: instruction with op1=0x%x',[i^.b1]));
//  BX_INFO(Format('nnn was %u',[i^.nnn]));

//  BX_INFO(('WARNING: Encountered an unknown instruction (signalling illegal instruction):'));
  // dump_core();

  Self.UndefinedOpcode(i);
end;

function TCPU.FetchDecode(iptr: PBit8u; out instruction: TInstruction_tag; out remain: Word; const is_32: Bool):Word;
var
  b1, b2, ilen, attr: unsigned;
  imm_mod_e, offset: unsigned;
  rm: unsigned;
  imm32u: Bit32u;
  sib, base: unsigned;
  displ16u: Bit16u;
  OpcodeInfoPtr: POpcodeInfo_t;
  saveBxInfo: POpcodeInfo_t;
  temp8s: Bit8s;
  imm16u: Bit16u;
  label fetch_b1, another_byte, modrm_done, get_8bit_displ, get_32bit_displ, end_proc;
begin
  // remain must be at least 1
  ilen := 1;
  instruction.as_32 := is_32;
  instruction.os_32 := instruction.as_32;
  instruction.Resolvemodrm := NULL;
  instruction.seg := SEG_REG_NULL;
  instruction.rep_used := 0;

fetch_b1:
  b1 := iptr^;
  inc(iptr);

another_byte:
  offset := instruction.os_32 shl 9; // * 512
  attr := BxOpcodeInfo[b1+offset].Attr;
  instruction.attr := attr;

  if (attr and BxAnother) <> 0 then
  begin
    if (attr  and BxPrefix) <> 0 then
    begin
      case b1 of
        $66: // OpSize
          begin
            instruction.os_32 := Word(is_32 = 0);
            if (ilen < remain) then
            begin
              inc(ilen);
              goto fetch_b1;
            end;
            exit(0);
          end;

        $67: // AddrSize
          begin
            instruction.as_32 := Word(is_32 = 0);
            if (ilen < remain) then
            begin
              inc(ilen);
              goto fetch_b1;
            end;
            exit(0);
          end;

        $f2,$f3: // REPNE/REPNZ
          begin
            instruction.rep_used := b1;
            if (ilen < remain) then
            begin
              Inc(ilen);
              goto fetch_b1;
            end;
            exit(1);
          end;

        $2e: // CS:
          begin
            instruction.seg := SEG_REG_CS;
            inc(ilen); goto fetch_b1;
          end;
        $26: // ES:
          begin
            instruction.seg := SEG_REG_ES;
            inc(ilen); goto fetch_b1;
          end;
        $36: // SS:
          begin
            instruction.seg := SEG_REG_SS;
            inc(ilen); goto fetch_b1;
          end;
        $3e: // DS:
          begin
            instruction.seg := SEG_REG_DS;
            inc(ilen); goto fetch_b1;
          end;
        $64: // FS:
          begin
            instruction.seg := SEG_REG_FS;
            inc(ilen); goto fetch_b1;
          end;
        $65: // GS:
          begin
            instruction.seg := SEG_REG_GS;
            inc(ilen); goto fetch_b1;
          end;
        $f0: // LOCK:
          begin
            inc(ilen); goto fetch_b1;
          end;
      end;
    end;
    // opcode requires another byte
    if (ilen < remain) then
    begin
      inc(ilen);
      b2 := iptr^;
      inc(iptr);
      if (b1 = $0f) then
      begin
        // 2-byte prefix
        b1 := $100 or b2;
        goto another_byte;
      end;
    end else
      exit(0);

    // Parse mod_-nnn-rm and related bytes
    instruction.modrm := b2;
    instruction.rm := b2 and $07;
    rm := instruction.rm;
    instruction.mod_  := b2  and $c0; // leave unshifted
    instruction.nnn   := (b2 shr 3)  and $07;
    if (instruction.mod_ = $c0) then
      goto modrm_done;
    if (instruction.as_32)<>0 then
    begin
      // 32-bit addressing mod_es; note that mod_=11b handled above
      if (rm <> 4) then
      begin // no s-i-b byte
//{$if BX_DYNAMIC_TRANSLATION = 1}
//        instruction.DTMemRegsUsed := 1 shl rm; // except for mod_:=00b rm:=100b
//{$ifend}
        if (instruction.mod_ = $00) then
        begin // mod_ = 00b
          instruction.Resolvemodrm := BxResolve32mod0[rm];
//{$if BX_DYNAMIC_TRANSLATION = 1}
//          instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_0[rm];
//{$ifend}
          if (instruction.seg and SEG_REG_NULL) <> 0 then
            instruction.seg := SEG_REG_DS;
          if (rm = 5) then
          begin
            if ((ilen + 3) < remain) then
            begin
              imm32u := iptr^;
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 8);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 16);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 24);
              inc(iptr);
              instruction.rm_addr := imm32u;
              //inc(iptr);
              Inc(ilen, 4);
//{$if BX_DYNAMIC_TRANSLATION = 1}
//              instruction.DTMemRegsUsed := 0;
//{$ifend}
              goto modrm_done;
            end else
              exit(0);
          end;
          // mod_=00b, rm!:=4, rm!:=5
          goto modrm_done;
        end;
        if (instruction.mod_ = $40) then
        begin // mod_ = 01b
          instruction.Resolvemodrm := BxResolve32mod1or2[rm];
//{$if BX_DYNAMIC_TRANSLATION = 1}
//          instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_1or2[rm];
//{$ifend}
          if (instruction.seg and SEG_REG_NULL) <> 0 then
            instruction.seg := Self.sreg_mod01_rm32[rm];
get_8bit_displ:
          if (ilen < remain) then
          begin
            // 8 sign extended to 32
            instruction.displ32u := Bit8s(iptr^);
            inc(iptr);
            inc(ilen);
            goto modrm_done;
          end else
            exit(0);
        end;
        // (mod_ = $80) mod_ = 10b
        instruction.Resolvemodrm := BxResolve32mod1or2[rm];
//{$if BX_DYNAMIC_TRANSLATION = 1}
//        instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_1or2[rm];
//{$ifend}
        if (instruction.seg and SEG_REG_NULL) <> 0 then
          instruction.seg := Self.sreg_mod10_rm32[rm];
get_32bit_displ:
        if ((ilen + 3) < remain) then
        begin
          imm32u := iptr^;
          inc(iptr);
          imm32u := imm32u or (iptr^ shl 8);
          inc(iptr);
          imm32u := imm32u or (iptr^ shl 16);
          inc(iptr);
          imm32u := imm32u or (iptr^ shl 24);
          inc(iptr);
          instruction.displ32u := imm32u;
          inc(ilen, 4);
          goto modrm_done;
        end else
          exit(0);
      end else
      begin // mod_!:=11b, rm=4, s-i-b byte follows
        if (ilen < remain) then
        begin
          sib := iptr^;
          inc(iptr);
          inc(ilen);
        end else
          exit(0);
        instruction.sib   := sib;
        instruction.base  := sib and $07; sib := sib shr 3;
        base := instruction.base;
        instruction.index := sib  and $07; sib := sib shr 3;
        instruction.scale := sib;
//{$if BX_DYNAMIC_TRANSLATION=1}
//        if (instruction.index = $04) // 100b
//          instruction.DTMemRegsUsed := 0;
//        else
//          instruction.DTMemRegsUsed := 1shlinstruction.index;
//{$ifend}
        if (instruction.mod_ = $00) then
        begin // mod_=00b, rm=4
          instruction.Resolvemodrm := BxResolve32mod0Base[base];
//{$if BX_DYNAMIC_TRANSLATION=1}
//          instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_0Base[base];
//{$ifend}
          if (instruction.seg and SEG_REG_NULL) <> 0 then
            instruction.seg := Self.sreg_mod0_base32[base];
          if (instruction.base = $05) then
            goto get_32bit_displ;
          // mod_=00b, rm=4, base!:=5
//{$if BX_DYNAMIC_TRANSLATION=1}
//          instruction.DTMemRegsUsed |:= 1shlbase;
//{$ifend}
          goto modrm_done;
        end;
//{$if BX_DYNAMIC_TRANSLATION=1}
//        // for remaining 32bit cases
//        instruction.DTMemRegsUsed := instruction.DTMemRegsUsed or (1 shl base);
//{$ifend}
        if (instruction.mod_ = $40) then
        begin // mod_=01b, rm=4
          instruction.Resolvemodrm := BxResolve32mod1or2Base[base];
//{$if BX_DYNAMIC_TRANSLATION=1}
//          instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_1or2Base[base];
//{$ifend}
          if (instruction.seg and SEG_REG_NULL) <> 0 then
            instruction.seg := Self.sreg_mod1or2_base32[base];
          goto get_8bit_displ;
        end;
        // (instruction.mod_ = $80),  mod_=10b, rm=4
        instruction.Resolvemodrm := BxResolve32mod1or2Base[base];
//{$if BX_DYNAMIC_TRANSLATION=1}
//        instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve32mod_1or2Base[base];
//{$ifend}
        if (instruction.seg and SEG_REG_NULL)<>0 then
          instruction.seg := Self.sreg_mod1or2_base32[base];
        goto get_32bit_displ;
      end;
    end
    else
    begin
      // 16-bit addressing mod_es, mod_=11b handled above
      if (instruction.mod_ = $40) then
      begin // mod_ = 01b
        instruction.Resolvemodrm := BxResolve16mod1or2[rm];
//{$if BX_DYNAMIC_TRANSLATION=1}
//        instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve16mod_1or2[rm];
//{$ifend}
        if (instruction.seg and SEG_REG_NULL)<>0 then
          instruction.seg := Self.sreg_mod01_rm16[rm];
//{$if BX_DYNAMIC_TRANSLATION=1}
//        instruction.DTMemRegsUsed := BxMemRegsUsed16[rm];
//{$ifend}
        if (ilen < remain) then
        begin
          // 8 sign extended to 16
          instruction.displ16u := Bit8s(iptr^);
          inc(iptr);
          inc(ilen);
          goto modrm_done;
        end else
          exit(0);
      end;
      if (instruction.mod_ = $80) then
      begin // mod_ = 10b
        instruction.Resolvemodrm := BxResolve16mod1or2[rm];
//{$if BX_DYNAMIC_TRANSLATION=1}
//        instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve16mod_1or2[rm];
//{$ifend}
        if (instruction.seg and SEG_REG_NULL) <> 0 then
          instruction.seg := Self.sreg_mod10_rm16[rm];
//{$if BX_DYNAMIC_TRANSLATION=1}
//        instruction.DTMemRegsUsed := BxMemRegsUsed16[rm];
//{$ifend}
        if ((ilen+1) < remain) then
        begin
          displ16u := iptr^;
          inc(iptr);
          displ16u := displ16u or (iptr^ shl 8);
          inc(iptr);
          instruction.displ16u := displ16u;
          inc(ilen,2);
          goto modrm_done;
        end else
          exit(0);
      end;
      // mod_ must be 00b at this point
      instruction.Resolvemodrm := BxResolve16mod0[rm];
//{$if BX_DYNAMIC_TRANSLATION=1}
//      instruction.DTResolvemod_rm := (BxprocedureFPtr_t) BxDTResolve16mod_0[rm];
//{$ifend}
      if (instruction.seg and SEG_REG_NULL) <> 0 then
        instruction.seg := Self.sreg_mod00_rm16[rm];
      if (rm = $06) then
      begin
        if ((ilen+1) < remain) then
        begin
          displ16u := iptr^;
          inc(iptr);
          displ16u := displ16u or (iptr^ shl 8);
          inc(iptr);
          instruction.rm_addr := displ16u;
          inc(ilen,2);
          goto modrm_done;
        end else
          exit(0);
      end;
      // mod_:=00b rm!:=6
//{$if BX_DYNAMIC_TRANSLATION=1}
//      instruction.DTMemRegsUsed := BxMemRegsUsed16[rm];
//{$ifend}
    end;

modrm_done:
    if (attr and BxGroupN) <> 0 then
    begin
      OpcodeInfoPtr := BxOpcodeInfo[b1+offset].AnotherArray;
      instruction.execute := POpcodeInfo_t(Integer(OpcodeInfoPtr) + (instruction.nnn * sizeof(TOpcodeInfo_t)))^.ExecutePtr;
      Inc(POpcodeInfo_t(Integer(OpcodeInfoPtr) + (instruction.nnn * sizeof(TOpcodeInfo_t)))^.Counter);
      StrCopy(instruction.name, POpcodeInfo_t(Integer(OpcodeInfoPtr) + (instruction.nnn * sizeof(TOpcodeInfo_t)))^.Name);
      // get additional attributes from group table
      attr := attr or POpcodeInfo_t(Integer(OpcodeInfoPtr) + (instruction.nnn * sizeof(TOpcodeInfo_t)))^.Attr;
      instruction.attr := attr;
//{$if BX_DYNAMIC_TRANSLATION=1}
//      instruction.DTAttr := 0; // for now
//{$ifend}
    end
    else
    begin
      instruction.execute := BxOpcodeInfo[b1+offset].ExecutePtr;
      Inc(BxOpcodeInfo[b1+offset].Counter);
      StrCopy(instruction.name, BxOpcodeInfo[b1+offset].Name);
//{$if BX_DYNAMIC_TRANSLATION=1}
//      instruction.DTAttr := BxDTOpcodeInfo[b1+offset].DTAttr;
//      instruction.DTFPtr := BxDTOpcodeInfo[b1+offset].DTASFPtr;
//{$ifend}
    end;
  end
  else
  begin
    // Opcode does not require a mod_RM byte.
    // Note that a 2-byte opcode (0F XX) will jump to before
    // the if() above after fetching the 2nd byte, so this path is
    // taken in all cases if a mod_rm byte is NOT required.
    instruction.execute := BxOpcodeInfo[b1+offset].ExecutePtr;
    StrCopy(instruction.name, BxOpcodeInfo[b1+offset].Name);
    Inc(BxOpcodeInfo[b1+offset].Counter);
//{$if BX_DYNAMIC_TRANSLATION=1}
//    instruction.DTAttr := BxDTOpcodeInfo[b1+offset].DTAttr;
//    instruction.DTFPtr := BxDTOpcodeInfo[b1+offset].DTASFPtr;
//{$ifend}
  end;


  imm_mod_e := attr  and BxImmediate;

  if (imm_mod_e) <> 0 then
  begin
    case imm_mod_e of
      BxImmediate_Ib:
        begin
          if (ilen < remain) then
          begin
            instruction.Ib := iptr^;
            inc(ilen);
          end else
            exit(0);
        end;
      BxImmediate_Ib_SE: // Sign extend to OS size
        begin
          if (ilen < remain) then
          begin
            temp8s := iptr^;
            if (instruction.os_32) <> 0 then
              instruction.Id := Bit32s(temp8s)
            else
              instruction.Iw := Bit16s(temp8s);
            inc(ilen);
          end else
            exit(0);
        end;
      BxImmediate_Iv, // same as BxImmediate_BrOff32
      BxImmediate_IvIw: // CALL_Ap
        begin
          if (instruction.os_32) <> 0 then
          begin
            if ((ilen+3) < remain) then
            begin
              imm32u := iptr^;
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 8);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 16);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 24);
              instruction.Id := imm32u;
              Inc(ilen,4);
            end else
              Exit(0);
          end else
          begin
            if ((ilen+1) < remain) then
            begin
              imm16u := iptr^;
              inc(iptr);
              imm16u := imm16u or (iptr^ shl 8);
              instruction.Iw := imm16u;
              inc(ilen,2);
            end else
              exit(0);
          end;
          if (imm_mod_e <> BxImmediate_IvIw) then
            goto end_proc;
            //break;
          inc(iptr);
          // Get Iw for BxImmediate_IvIw
          if ((ilen+1) < remain) then
          begin
            imm16u := iptr^;
            inc(iptr);
            imm16u := imm16u or (iptr^ shl 8);
            instruction.Iw2 := imm16u;
            inc(ilen,2);
          end else
            Exit(0);
        end;
      BxImmediate_O:
        begin
          if (instruction.as_32)<>0 then
          begin
            // fetch 32bit address into Id
            if ((ilen+3) < remain) then
            begin
              imm32u := iptr^;
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 8);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 16);
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 24);
              instruction.Id := imm32u;
              inc(ilen,4);
            end else
              Exit(0);
          end else
          begin
            // fetch 16bit address into Id
            if ((ilen+1) < remain) then
            begin
              imm32u := iptr^;
              inc(iptr);
              imm32u := imm32u or (iptr^ shl 8);
              instruction.Id := imm32u;
              inc(ilen,2);
            end else
              Exit(0);
          end;
        end;
      BxImmediate_Iw,
      BxImmediate_IwIb:
        begin
          if ((ilen+1) < remain) then
          begin
            imm16u := iptr^;
            inc(iptr);
            imm16u := imm16u or (iptr^ shl 8);
            instruction.Iw := imm16u;
            inc(ilen,2);
          end else
            Exit(0);
          if (imm_mod_e = BxImmediate_Iw) then
            goto end_proc;
          inc(iptr);
          if (ilen < remain) then
          begin
            instruction.Ib2 := iptr^;
  //          inc(iptr);
            inc(ilen);
          end else
            Exit(0);
        end;
      BxImmediate_BrOff8:
        begin
          if (ilen < remain) then
          begin
            temp8s := iptr^;
            instruction.Id := temp8s;
            inc(ilen);
          end else
            Exit(0);
        end;
      BxImmediate_BrOff16:
        begin
          if ((ilen+1) < remain) then
          begin
            imm16u := iptr^;
            inc(iptr);
            imm16u := imm16u or (iptr^ shl 8);
            {$R-}
            instruction.Id := Bit16s(imm16u);
            inc(ilen,2);
          end else
            Exit(0);
        end;
    else
      begin
//        BX_INFO(Format('b1 was %x',[b1]));
        LogPanic(Format('fetchdecode: imm_mod_e := %u',[imm_mod_e]));
      end;
    end;
  end;

end_proc:
  instruction.b1 := b1;
  instruction.ilen := ilen;
  //instruction.flags_in  := 0; // for now
  //instruction.flags_out := 0; // for now
  Result := 1;
end;

procedure TCPU.BxResolveError(I: PInstruction_tag);
begin
  LogPanic(Format('BxResolveError: instruction with op1:=$%x',[i^.b1]));
end;

procedure TCPU.interrupt(vector:Bit8u; is_INT:Bool; is_error_code:Bool;error_code:Bit16u);
var
  dword1, dword2:Bit32u;
  gate_descriptor, cs_descriptor:TDescriptor_t;
  cs_selector:TSelector_t;

  raw_tss_selector:Bit16u;
  tss_selector:TSelector_t;
  tss_descriptor:TDescriptor_t;

  gate_dest_selector:Bit16u;
  gate_dest_offset:Bit32u;

  old_SS, old_CS, SS_for_cpl_x:Bit16u;
  ESP_for_cpl_x, old_EIP, old_ESP:Bit32u;
  ss_descriptor:TDescriptor_t;
  ss_selector:TSelector_t;
  bytes:Integer;
  temp_ESP:Bit32u;
  cs_selector_2, ip_:Bit16u;

  label end_case;
begin

//BX_DEBUG(( '.interrupt(%u)', vector ));

  //BX_INSTR_INTERRUPT(vector);
  invalidate_prefetch_q();

  // Discard any traps and inhibits for new context; traps will
  // resume upon return.
  Self.debug_trap := 0;
  Self.inhibit_mask := 0;

//  unsigned prev_errno;

//  BX_DEBUG(Format('interrupt(): vector := %u, INT := %u, EXT := %u',[vector, is_INT, Self.EXT]));

  Self.save_cs  := Self.sregs[SEG_REG_CS];
  Self.save_ss  := Self.sregs[SEG_REG_SS];
  Self.save_eip := FEIP;
  Self.save_esp := ESP;

//  prev_errno := Self.errorno;

  if Boolean(real_mode() = 0) then
  begin
    // interrupt vector must be within IDT table limits,
    // else #GP(vector number*8 + 2 + EXT)
    if Boolean( (vector*8 + 7) > Self.idtr.limit) then
    begin
//      BX_DEBUG(Format('IDT.limit := %04x', [Self.idtr.limit]));
//      BX_DEBUG(Format('IDT.base  := %06x', [Self.idtr.base]));
//      BX_DEBUG(('interrupt vector must be within IDT table limits'));
//      BX_DEBUG(('bailing'));
//      BX_DEBUG(('interrupt(): vector > idtr.limit'));

      exception2([BX_GP_EXCEPTION, vector*8 + 2, 0]);
    end;

    // descriptor AR byte must indicate interrupt gate, trap gate,
    // or task gate, else #GP(vector*8 + 2 + EXT)
    access_linear(Self.idtr.base + vector*8,     4, 0,
      BX_READ, @dword1);
    access_linear(Self.idtr.base + vector*8 + 4, 4, 0,
      BX_READ, @dword2);

    parse_descriptor(dword1, dword2, @gate_descriptor);

    if Boolean( (gate_descriptor.valid=0) or (gate_descriptor.segmentType<>0)) then
//      BX_DEBUG(('interrupt(): gate descriptor is not valid sys seg'));
      exception2([BX_GP_EXCEPTION, vector*8 + 2, 0]);

    case gate_descriptor.type_ of
      5, // task gate
      6, // 286 interrupt gate
      7, // 286 trap gate
      14, // 386 interrupt gate
      15: // 386 trap gate
        goto end_case;
    else
      begin
//        BX_DEBUG(Format('interrupt(): gate.type(%u) <> 5,6,7,14,15 ',[gate_descriptor.type_]));
        exception2([BX_GP_EXCEPTION, vector*8 + 2, 0]);
        exit;
      end;
    end;
end_case:

    // if software interrupt, then gate descripor DPL must be >= CPL,
    // else #GP(vector * 8 + 2 + EXT)
    if Boolean((is_INT <> 0) and (gate_descriptor.dpl < CPL)) then
    begin
(* ??? *)
//      BX_DEBUG(('interrupt(): is_INT @ and (dpl < CPL)'));
      exception2([BX_GP_EXCEPTION, vector*8 + 2, 0]);
      exit;
    end;

    // Gate must be present, else #NP(vector * 8 + 2 + EXT)
    if Boolean(gate_descriptor.p = 0) then
//      BX_DEBUG(('interrupt(): p = 0'));
      exception2([BX_NP_EXCEPTION, vector*8 + 2, 0]);

    case gate_descriptor.type_ of
      5: // 286/386 task gate
        // examine selector to TSS, given in task gate descriptor
      begin
          raw_tss_selector := gate_descriptor.taskgate.tss_selector;
          parse_selector(raw_tss_selector, @tss_selector);

        // must specify global in the local/global bit,
        //      else #TS(TSS selector)
// +++
// 486/Pent books say #TSS(selector)
// PPro+ says #GP(selector)
        if Boolean(tss_selector.ti) then
        begin
          LogPanic(('interrupt: tss_selector.ti:=1'));
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
          exit;
        end;

        // index must be within GDT limits, else #TS(TSS selector)
        fetch_raw_descriptor(@tss_selector, @dword1, @dword2,
            BX_TS_EXCEPTION);

        // AR byte must specify available TSS,
        //   else #TS(TSS selector)
        parse_descriptor(dword1, dword2, @tss_descriptor);
        if Boolean(tss_descriptor.valid=0 or tss_descriptor.segmentType) then
        begin
          LogPanic(('exception: TSS selector points to bad TSS'));
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
          exit;
        end;

        if Boolean((tss_descriptor.type_<>9) and (tss_descriptor.type_<>1)) then
        begin
          LogPanic(('exception: TSS selector points to bad TSS'));
          exception2([BX_TS_EXCEPTION, raw_tss_selector  and $fffc, 0]);
          exit;
        end;


        // TSS must be present, else #NP(TSS selector)
        // done in task_switch()

        // switch tasks with nesting to TSS
        task_switch(@tss_selector, @tss_descriptor,
                    BX_TASK_FROM_CALL_OR_INT, dword1, dword2);

        // if interrupt was caused by fault with error code
        //   stack limits must allow push of 2 more bytes, else #SS(0)
        // push error code onto stack

        //??? push_16 vs push_32
        if Boolean( is_error_code ) then
        begin
          //if Boolean(tss_descriptor.type=9)
          if Boolean(Self.sregs[SEG_REG_CS].cache.segment.d_b)
            then push_32(error_code)
            else push_16(error_code);
        end;

        // instruction pointer must be in CS limit, else #GP(0)
        //if Boolean(EIP > cs_descriptor.u.segment.limit_scaled) then beginend;
        if Boolean(FEIP > Self.sregs[SEG_REG_CS].cache.segment.limit_scaled) then
        begin
          LogPanic(('exception(): eIP > CS.limit'));
          exception2([BX_GP_EXCEPTION, $0000, 0]);
        end;
        exit;
      end;

      6, // 286 interrupt gate
      7, // 286 trap gate
      14, // 386 interrupt gate
      15: // 386 trap gate
      begin
        if Boolean( gate_descriptor.type_ >= 14 ) then
        begin // 386 gate
          gate_dest_selector := gate_descriptor.gate386.dest_selector;
          gate_dest_offset   := gate_descriptor.gate386.dest_offset;
        end else
        begin // 286 gate
          gate_dest_selector := gate_descriptor.gate286.dest_selector;
          gate_dest_offset   := gate_descriptor.gate286.dest_offset;
        end;

        // examine CS selector and descriptor given in gate descriptor
        // selector must be non-null else #GP(EXT)
        if Boolean( (gate_dest_selector  and $fffc) = 0 ) then
        begin
          LogPanic(('int_trap_gate(): selector null'));
          exception2([BX_GP_EXCEPTION, 0, 0]);
        end;

        parse_selector(gate_dest_selector, @cs_selector);

        // selector must be within its descriptor table limits
        // else #GP(selector+EXT)
        fetch_raw_descriptor(@cs_selector, @dword1, @dword2,
                                BX_GP_EXCEPTION);
        parse_descriptor(dword1, dword2, @cs_descriptor);

        // descriptor AR byte must indicate code seg
        // and code segment descriptor DPL<=CPL, else #GP(selector+EXT)
        if Boolean((cs_descriptor.valid=0) or (cs_descriptor.segmentType=0) or (cs_descriptor.segment.executable=0) or
             (cs_descriptor.dpl>CPL)) then
//          BX_DEBUG(('interrupt(): not code segment'));
          exception2([BX_GP_EXCEPTION, cs_selector.value  and $fffc, 0]);

        // segment must be present, else #NP(selector + EXT)
        if Boolean( cs_descriptor.p=0 ) then
//          BX_DEBUG(('interrupt(): segment not present'));
          exception2([BX_NP_EXCEPTION, cs_selector.value  and $fffc, 0]);

        // if code segment is non-conforming and DPL < CPL then
        // INTERRUPT TO INNER PRIVILEGE:
        if Boolean( (cs_descriptor.segment.c_ed=0) and (cs_descriptor.dpl<CPL)) then
        begin

//          BX_DEBUG(('interrupt(): INTERRUPT TO INNER PRIVILEGE'));

          // check selector and descriptor for new stack in current TSS
          get_SS_ESP_from_TSS(cs_descriptor.dpl,
                              @SS_for_cpl_x, @ESP_for_cpl_x);

          // Selector must be non-null else #TS(EXT)
          if Boolean( (SS_for_cpl_x  and $fffc) = 0 ) then
          begin
            LogPanic(('interrupt(): SS selector null'));
            (* TS(ext) *)
            exception2([BX_TS_EXCEPTION, 0, 0]);
          end;

          // selector index must be within its descriptor table limits
          // else #TS(SS selector + EXT)
          parse_selector(SS_for_cpl_x, @ss_selector);
          // fetch 2 dwords of descriptor; call handles out of limits checks
          fetch_raw_descriptor(@ss_selector, @dword1, @dword2,
                                  BX_TS_EXCEPTION);
          parse_descriptor(dword1, dword2, @ss_descriptor);

          // selector rpl must := dpl of code segment,
          // else #TS(SS selector + ext)
          if Boolean(ss_selector.rpl <> cs_descriptor.dpl) then
          begin
            LogPanic(('interrupt(): SS.rpl !:= CS.dpl'));
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
          end;

          // stack seg DPL must := DPL of code segment,
          // else #TS(SS selector + ext)
          if Boolean(ss_descriptor.dpl <> cs_descriptor.dpl) then
          begin
            LogPanic(('interrupt(): SS.dpl !:= CS.dpl'));
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
          end;

          // descriptor must indicate writable data segment,
          // else #TS(SS selector + EXT)
          if Boolean((ss_descriptor.valid=0) or (ss_descriptor.segmentType=0) or (ss_descriptor.segment.executable=1) or
              (ss_descriptor.segment.r_w=0)) then
          begin
            LogPanic(('interrupt(): SS not writable data segment'));
            exception2([BX_TS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
          end;

          // seg must be present, else #SS(SS selector + ext)
          if Boolean(ss_descriptor.p=0) then
          begin
            LogPanic(('interrupt(): SS not present'));
            exception2([BX_SS_EXCEPTION, SS_for_cpl_x  and $fffc, 0]);
          end;

          if Boolean(gate_descriptor.type_>=14) then
          begin
            // 386 int/trap gate
            // new stack must have room for 20|24 bytes, else #SS(0)
            if Boolean( is_error_code )
              then bytes := 24
              else bytes := 20;

            if Boolean(v8086_mode()) then
              bytes := bytes + 16;
          end else
          begin
            // new stack must have room for 10|12 bytes, else #SS(0)
            if Boolean( is_error_code )
              then bytes := 12
              else bytes := 10;

            if Boolean(v8086_mode()) then
            begin
              bytes := bytes +8;
              LogPanic(('interrupt: int/trap gate VM'));
            end;
          end;

// 486,Pentium books
// new stack must have room for 10/12 bytes, else #SS(0) 486 book
// PPro+
// new stack must have room for 10/12 bytes, else #SS(seg selector)
          if Boolean( can_push(@ss_descriptor, ESP_for_cpl_x, bytes)=0 ) then
            LogPanic(Format('interrupt(): new stack doesn''t have room for %u bytes',[bytes]));
            // SS(???)

          // IP must be within CS segment boundaries, else #GP(0)
          if Boolean(gate_dest_offset > cs_descriptor.segment.limit_scaled) then
          begin
            LogPanic(('interrupt(): gate eIP > CS.limit'));
            exception2([BX_GP_EXCEPTION, 0, 0]);
          end;

          old_ESP := ESP;
          old_SS  := Self.sregs[SEG_REG_SS].selector.value;
          old_EIP := FEIP;
          old_CS  := Self.sregs[SEG_REG_CS].selector.value;

          // load new SS:SP values from TSS
          load_ss(@ss_selector, @ss_descriptor, cs_descriptor.dpl);

          if Boolean(ss_descriptor.segment.d_b)
            then ESP := ESP_for_cpl_x
            else SP := ESP_for_cpl_x; // leave upper 16bits

          // load new CS:IP values from gate
          // set CPL to new code segment DPL
          // set RPL of CS to CPL
          load_cs(@cs_selector, @cs_descriptor, cs_descriptor.dpl);
          FEIP := gate_dest_offset;

          if Boolean(gate_descriptor.type_>=14) then
          begin // 386 int/trap gate
            if Boolean(v8086_mode()) then
            begin
              push_32(Self.sregs[SEG_REG_GS].selector.value);
              push_32(Self.sregs[SEG_REG_FS].selector.value);
              push_32(Self.sregs[SEG_REG_DS].selector.value);
              push_32(Self.sregs[SEG_REG_ES].selector.value);
              Self.sregs[SEG_REG_GS].cache.valid := 0;
              Self.sregs[SEG_REG_GS].selector.value := 0;
              Self.sregs[SEG_REG_FS].cache.valid := 0;
              Self.sregs[SEG_REG_FS].selector.value := 0;
              Self.sregs[SEG_REG_DS].cache.valid := 0;
              Self.sregs[SEG_REG_DS].selector.value := 0;
              Self.sregs[SEG_REG_ES].cache.valid := 0;
              Self.sregs[SEG_REG_ES].selector.value := 0;
            end;
            // push long pointer to old stack onto new stack
            push_32(old_SS);
            push_32(old_ESP);

            // push EFLAGS
            push_32(read_eflags());

            // push long pointer to return address onto new stack
            push_32(old_CS);
            push_32(old_EIP);

            if Boolean( is_error_code ) then
              push_32(error_code);
          end else
          begin // 286 int/trap gate
            if Boolean(v8086_mode()) then
              LogPanic(('286 int/trap gate, VM'));

            // push long pointer to old stack onto new stack
            push_16(old_SS);
            push_16(old_ESP); // ignores upper 16bits

            // push FLAGS
            push_16(read_flags());

            // push return address onto new stack
            push_16(old_CS);
            push_16(old_EIP); // ignores upper 16bits

            if Boolean( is_error_code ) then
              push_16(error_code);
          end;

          // if INTERRUPT GATE set IF to 0
          if Boolean( (gate_descriptor.type_ and 1)=0 ) then// even is int-gate
            Self.eflags.if_ := 0;

          Self.eflags.tf := 0;
          Self.eflags.vm := 0;
          Self.eflags.rf := 0;
          Self.eflags.nt := 0;
          exit;
        end;

        if Boolean(v8086_mode()) then
          exception2([BX_GP_EXCEPTION, cs_selector.value  and $fffc, 0]);

        // if code segment is conforming OR code segment DPL := CPL then
        // INTERRUPT TO SAME PRIVILEGE LEVEL:
        if Boolean((cs_descriptor.segment.c_ed=1) or (cs_descriptor.dpl=CPL)) then
        begin

          if Boolean(Self.sregs[SEG_REG_SS].cache.segment.d_b)
            then temp_ESP := ESP
            else temp_ESP := SP;

//          BX_DEBUG(('int_trap_gate286(): INTERRUPT TO SAME PRIVILEGE'));

          // Current stack limits must allow pushing 6|8 bytes, else #SS(0)
          if Boolean(gate_descriptor.type_ >= 14) then
          begin // 386 gate
            if Boolean( is_error_code )
              then bytes := 16
              else bytes := 12;
          end else
          begin // 286 gate
            if Boolean( is_error_code )
              then bytes := 8
              else bytes := 6;
          end;

          if Boolean( can_push(@Self.sregs[SEG_REG_SS].cache,
                         temp_ESP, bytes)=0 ) then
//            BX_DEBUG(('interrupt(): stack doesn''t have room'));
            exception2([BX_SS_EXCEPTION, 0, 0]);

          // eIP must be in CS limit else #GP(0)
          if Boolean(gate_dest_offset > cs_descriptor.segment.limit_scaled) then
          begin
            LogPanic(('interrupt(): IP > cs descriptor limit'));
            exception2([BX_GP_EXCEPTION, 0, 0]);
          end;

          // push flags onto stack
          // push current CS selector onto stack
          // push return offset onto stack
          if Boolean(gate_descriptor.type_ >= 14) then
          begin // 386 gate
            push_32(read_eflags());
            push_32(Self.sregs[SEG_REG_CS].selector.value);
            push_32(FEIP);
            if Boolean( is_error_code ) then
              push_32(error_code)
          end else
          begin // 286 gate
            push_16(read_flags());
            push_16(Self.sregs[SEG_REG_CS].selector.value);
            push_16(EIP);
            if Boolean( is_error_code ) then
              push_16(error_code);
          end;

          // load CS:IP from gate
          // load CS descriptor
          // set the RPL field of CS to CPL
          load_cs(@cs_selector, @cs_descriptor, CPL);
          FEIP := gate_dest_offset;

          // if interrupt gate then set IF to 0
          if Boolean( (gate_descriptor.type_ and 1)=0 ) then // even is int-gate
            Self.eflags.if_ := 0;

          Self.eflags.tf := 0;
          Self.eflags.nt := 0;
          Self.eflags.vm := 0;
          Self.eflags.rf := 0;
          exit;
        end;

        // else #GP(CS selector + ext)
//        BX_DEBUG(('interrupt: bad descriptor'));
//        BX_DEBUG(Format('c_ed:=%u, descriptor.dpl:=%u, CPL:=%u',[cs_descriptor.segment.c_ed,cs_descriptor.dpl,CPL]));
//        BX_DEBUG(Format('cs.segment := %u',[cs_descriptor.segmentType]));
        exception2([BX_GP_EXCEPTION, cs_selector.value  and $fffc, 0]);
      end;
      (*else
        BX_PANIC(('bad descriptor type in interrupt()!'));*)
    end;
  end else
  begin (* real mod_e *)

    if Boolean( (vector*4+3) > Self.idtr.limit ) then
      LogPanic(('interrupt(real mod_e) vector > limit'));

    push_16(read_flags());

    cs_selector_2 := Self.sregs[SEG_REG_CS].selector.value;
    push_16(cs_selector_2);
    ip_ := Self.FEIP;
    push_16(ip_);

    access_linear(Self.idtr.base + 4 * vector,     2, 0, BX_READ, @ip_);
    EIP := ip_;
    access_linear(Self.idtr.base + 4 * vector + 2, 2, 0, BX_READ, @cs_selector_2);
    load_seg_reg(@Self.sregs[SEG_REG_CS], cs_selector_2);

    (* INT affects the following flags: I,T *)
    Self.eflags.if_ := 0;
    Self.eflags.tf  := 0;
    Self.eflags.ac  := 0;
    Self.eflags.rf  := 0;
  end;
end;

procedure TCPU.exception(vector:unsigned;error_code:Bit16u;is_INT:Bool);
  // vector:     0..255: vector in IDT
  // error_code: if exception generates and error, push this error code

var
  push_error:Bool;
  exception_type:Bit8u;
//  prev_errno:unsigned;
begin

{$if BX_DEBUGGER=1}
  if Boolean(bx_guard.special_unwind_stack) then begin
//    BX_INFO (('exception() returning early because special_unwind_stack is set'));
    exit;
  end;
{$ifend}

//BX_DEBUG(( '.exception(%u)', vector ));

  //BX_INSTR_EXCEPTION(vector);
  invalidate_prefetch_q();

  //UNUSED(is_INT);

  //BX_DEBUG(('exception(%02x h)', (unsigned) vector));

  // if not initial error, restore previous register values from
  // previous attempt to handle exception
  if Boolean(Self.errorno) then
  begin
    Self.sregs[SEG_REG_CS]  := Self.save_cs;
    Self.sregs[SEG_REG_SS]  := Self.save_ss;
    FEIP := Self.save_eip;
    ESP := Self.save_esp;
  end;

  Inc(Self.errorno);
  if Boolean(Self.errorno >= 3) then
  begin
    LogPanic(('exception(): 3rd exception with no resolution'));
    LogError(('WARNING: Any simulation after this point is completely bogus.'));
{$if BX_DEBUGGER=1}
    bx_guard.special_unwind_stack := true;
{$ifend}
    exit;
  end;

  (* careful not to get here with curr_exception[1]=DOUBLE_FAULT *)
  (* ...index on DOUBLE_FAULT below, will be out of bounds *)

  (* if 1st was a double fault (software INT?), then shutdown *)
  if Boolean( (Self.errorno=2) and (Self.curr_exception[0]=BX_ET_DOUBLE_FAULT) ) then begin
    LogPanic(('exception(): triple fault encountered'));
    LogError(('WARNING: Any simulation after this point is completely bogus.'));
{$if BX_DEBUGGER=1}
    bx_guard.special_unwind_stack := true;
{$ifend}
    exit;
    end;

  (* ??? this is not totally correct, should be done depending on
   * vector *)
  (* backup IP to value before error occurred *)
  FEIP := Self.prev_eip;
  ESP := Self.prev_esp;

  // note: fault-class exceptions _except_ #DB set RF in
  //       eflags image.

  case vector of
    0: // DIV by 0
      begin
        push_error := 0;
        exception_type := BX_ET_CONTRIBUTORY;
        Self.eflags.rf := 1;
      end;
    1: // debug exceptions
      begin
        push_error := 0;
        exception_type := BX_ET_BENIGN;
      end;
    2: // NMI
      begin
        push_error := 0;
        exception_type := BX_ET_BENIGN;
      end;
    3: // breakpoint
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      end;
    4: // overflow
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      end;
    5: // bounds check
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      Self.eflags.rf := 1;
      end;
    6: // invalid opcode
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      Self.eflags.rf := 1;
      end;
    7: // device not available
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      Self.eflags.rf := 1;
      end;
    8: // double fault
      begin
      push_error := 1;
      exception_type := BX_ET_DOUBLE_FAULT;
      end;
    9: // coprocessor segment overrun (286,386 only)
      begin
      push_error := 0;
      exception_type := BX_ET_CONTRIBUTORY;
      Self.eflags.rf := 1;
      LogPanic(('exception(9): unfinished'));
      end;
    10: // invalid TSS
      begin
      push_error := 1;
      exception_type := BX_ET_CONTRIBUTORY;
      error_code := (error_code  and $fffe) or Self.EXT;
      Self.eflags.rf := 1;
      end;
    11: // segment not present
      begin
      push_error := 1;
      exception_type := BX_ET_CONTRIBUTORY;
      error_code := (error_code  and $fffe) or Self.EXT;
      Self.eflags.rf := 1;
      end;
    12: // stack fault
      begin
      push_error := 1;
      exception_type := BX_ET_CONTRIBUTORY;
      error_code := (error_code  and $fffe) or Self.EXT;
      Self.eflags.rf := 1;
      end;
    13: // general protection
      begin
      push_error := 1;
      exception_type := BX_ET_CONTRIBUTORY;
      error_code := (error_code  and $fffe) or Self.EXT;
      Self.eflags.rf := 1;
      end;
    14: // page fault
      begin
      push_error := 1;
      exception_type := BX_ET_PAGE_FAULT;
      // ??? special format error returned
      Self.eflags.rf := 1;
      end;
    15: // reserved
      begin
      LogPanic(('exception(15): reserved'));
      push_error := 0;     // keep compiler happy for now
      exception_type := 0; // keep compiler happy for now
      end;
    16: // floating-point error
      begin
      push_error := 0;
      exception_type := BX_ET_BENIGN;
      Self.eflags.rf := 1;
      end;
    17: // alignment check
      begin
      LogPanic(('exception(): alignment-check, vector 17 unimplemented'));
      push_error := 0;     // keep compiler happy for now
      exception_type := 0; // keep compiler happy for now
      Self.eflags.rf := 1;
      end;
    18: // machine check
      begin
      LogPanic(('exception(): machine-check, vector 18 unimplemented'));
      push_error := 0;     // keep compiler happy for now
      exception_type := 0; // keep compiler happy for now
      end;
    else
      begin
      LogPanic(Format('exception(%u): bad vector',[vector]));
      push_error := 0;     // keep compiler happy for now
      exception_type := 0; // keep compiler happy for now
      end;
    end;

  if Boolean(exception_type <> BX_ET_PAGE_FAULT) then begin
    // Page faults have different format
    error_code := (error_code  and $fffe) or Self.EXT;
    end;
  Self.EXT := 1;

  (* if we've already had 1st exception, see if 2nd causes a
   * Double Fault instead.  Otherwise, just record 1st exception
   *)
  if Boolean(Self.errorno >= 2) then begin
    if Boolean(is_exception_OK[Self.curr_exception[0]][exception_type]) then
      Self.curr_exception[1] := exception_type
    else begin
      Self.curr_exception[1] := BX_ET_DOUBLE_FAULT;
      vector := 8;
    end;
  end
  else begin
    Self.curr_exception[0] := exception_type;
  end;


  if Boolean(real_mode()=0) then begin
//    prev_errno := Self.errorno;
    Self.interrupt(vector, 0, push_error, error_code);
//    if Boolean(Self.errorno > prev_errno) then begin
//      BX_INFO(('segment_exception(): errorno changed'));
//      longjmp(jmp_buf_env, 1); // go back to main decode loop
//      exit;
//      end;

//    if Boolean(push_error) then begin
//      (* push error code on stack, after handling interrupt *)
//      (* pushed as a word or dword depending upon default size ??? *)
//      if Boolean(ss.cache.u.segment.d_b)
//        push_32((Bit32u) error_code); (* upper bits reserved *)
//      else
//        push_16(error_code);
//      if Boolean(Self.errorno > prev_errno) then begin
//        BX_PANIC(('segment_exception(): errorno changed'));
//        exit;
//        end;
//      end;
    Self.errorno := 0; // error resolved
    if vector = 16 then
      reloop:=True
    else
      longjmp(savejump, 1); // go back to main decode loop
    end
  else // real mod_e
    begin
    // not INT, no error code pushed
    Self.interrupt(vector, 0, 0, 0);
    Self.errorno := 0; // error resolved
    if vector = 16 then
      reloop:=True
    else
      longjmp(savejump, 1); // go back to main decode loop
    //longjmp(Self.jmp_buf_env, 1); // go back to main decode loop
    end;
end;


function TCPU.int_number(seg:Psegment_reg_t):Integer;
begin
  if Boolean(seg = @Self.sregs[SEG_REG_SS])
    then Result := (BX_SS_EXCEPTION)
    else Result := (BX_GP_EXCEPTION);
end;

procedure TCPU.shutdown_cpu;
begin
  LogPanic(('shutdown_cpu(): not implemented for 386'));

  invalidate_prefetch_q();
  LogPanic(('shutdown_cpu(): not finished'));

end;

procedure TCPU.FWAIT(I: PInstruction_tag);
begin
  if ( (Self.FCR0.ts<>0) and (Self.FCR0.mp<>0 )) then begin
    exception(BX_NM_EXCEPTION, 0, 0);
    end;
if BX_SUPPORT_FPU
then
begin
  fpu_execute(i);
end
else
begin
//  BX_INFO(('FWAIT not implemented'));
end;
end;

procedure TCPU.ESC0(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em<>0) or (Self.FCR0.ts<>0) ) then begin
    exception(BX_NM_EXCEPTION, 0, 0);
    end;
if BX_SUPPORT_FPU then
begin
  fpu_execute(i);
end
else
begin
//  BX_INFO(('ESC0 not implemented'));
end;
end;

procedure TCPU.ESC1(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em<>0) or (Self.FCR0.ts<>0) ) then begin
    exception(BX_NM_EXCEPTION, 0, 0);
    end;
if BX_SUPPORT_FPU then
begin
  fpu_execute(i);
end else begin
//  BX_INFO(('ESC0 not implemented'));
end;
end;

procedure TCPU.ESC2(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em<>0) or (Self.FCR0.ts<>0) ) then begin
    exception(BX_NM_EXCEPTION, 0, 0);
    end;

if BX_SUPPORT_FPU then
  fpu_execute(i)
else
//  BX_INFO(('ESC0 not implemented'));

end;

procedure TCPU.ESC3(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em<>0) or (Self.FCR0.ts<>0) ) then begin
    exception(BX_NM_EXCEPTION, 0, 0);
    end;
if BX_SUPPORT_FPU  then
  fpu_execute(i)
else
//  BX_INFO(('ESC0 not implemented'));
end;

procedure TCPU.ESC4(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em <> 0) or (Self.FCR0.ts <> 0) ) then
    exception(BX_NM_EXCEPTION, 0, 0);
  if BX_SUPPORT_FPU then
    fpu_execute(i);
end;

procedure TCPU.ESC5(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em <> 0) or (Self.FCR0.ts <> 0) ) then
    exception(BX_NM_EXCEPTION, 0, 0);
  if BX_SUPPORT_FPU then
    fpu_execute(i);
end;

procedure TCPU.ESC6(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em <> 0) or (Self.FCR0.ts <> 0) ) then
    exception(BX_NM_EXCEPTION, 0, 0);
  if BX_SUPPORT_FPU then
    fpu_execute(i);
end;

procedure TCPU.ESC7(I: PInstruction_tag);
begin
  if ( (Self.FCR0.em <> 0) or (Self.FCR0.ts <> 0) ) then
    exception(BX_NM_EXCEPTION, 0, 0);
  if BX_SUPPORT_FPU then
    fpu_execute(i);
end;

function TCPU.get_SF:Bool;
begin
  case ( (lf_flags_status shr 16) and $00000f ) of
    BX_LF_INDEX_KNOWN:
      begin
        Result:=eflags.sf;
        exit;
      end;
    BX_LF_INDEX_OSZAPC:
      begin
        case oszapc.instr of
          BX_INSTR_ADD8,
          BX_INSTR_ADC8,
          BX_INSTR_SUB8,
          BX_INSTR_SBB8,
          BX_INSTR_CMP8,
          BX_INSTR_NEG8,
          BX_INSTR_XADD8,
          BX_INSTR_OR8,
          BX_INSTR_AND8,
          BX_INSTR_TEST8,
          BX_INSTR_XOR8,
          BX_INSTR_CMPS8,
          BX_INSTR_SCAS8,
          BX_INSTR_SHR8,
          BX_INSTR_SHL8:
            begin
              eflags.sf := Word(oszapc.result_8 >= $80);
            end;
          BX_INSTR_ADD16,
          BX_INSTR_ADC16,
          BX_INSTR_SUB16,
          BX_INSTR_SBB16,
          BX_INSTR_CMP16,
          BX_INSTR_NEG16,
          BX_INSTR_XADD16,
          BX_INSTR_OR16,
          BX_INSTR_AND16,
          BX_INSTR_TEST16,
          BX_INSTR_XOR16,
          BX_INSTR_CMPS16,
          BX_INSTR_SCAS16,
          BX_INSTR_SHR16,
          BX_INSTR_SHL16:
            begin
              eflags.sf := Word(oszapc.result_16 >= $8000);

            end;
          BX_INSTR_ADD32,
          BX_INSTR_ADC32,
          BX_INSTR_SUB32,
          BX_INSTR_SBB32,
          BX_INSTR_CMP32,
          BX_INSTR_NEG32,
          BX_INSTR_XADD32,
          BX_INSTR_OR32,
          BX_INSTR_AND32,
          BX_INSTR_TEST32,
          BX_INSTR_XOR32,
          BX_INSTR_CMPS32,
          BX_INSTR_SCAS32,
          BX_INSTR_SHR32,
          BX_INSTR_SHL32:
            begin
              eflags.sf := Word(oszapc.result_32 >= $80000000);

            end;
          else
            LogPanic(('get_SF: OSZAPC: unknown instr'));
        end; //case oszapc.instr of
        lf_flags_status := lf_flags_status and $f0ffff;
        Result:=(eflags.sf);
      end; //BX_LF_INDEX_OSZAPC
      BX_LF_INDEX_OSZAP:
        begin
          case oszap.instr of
            BX_INSTR_INC8, BX_INSTR_DEC8:
              begin
                eflags.sf := Word(oszap.result_8 >= $80);

              end;
            BX_INSTR_INC16, BX_INSTR_DEC16:
              begin
                eflags.sf := Word(oszap.result_16 >= $8000);

              end;
            BX_INSTR_INC32, BX_INSTR_DEC32:
              begin
                eflags.sf := Word(oszap.result_32 >= $80000000);

              end;
            else
              LogPanic(('get_SF: OSZAP: unknown instr'));
          end;  //case oszap.instr of
          lf_flags_status := lf_flags_status and $f0ffff;
          Result:=(eflags.sf);
        end; //BX_LF_INDEX_OSZAP
    else
      begin
        LogPanic(('get_SF: unknown case'));
        Result:=0;

      end;
  end;
end;

function TCPU.get_ZF:Bool;
begin
  case ( (lf_flags_status shr 12) and $00000f ) of
    BX_LF_INDEX_KNOWN:
      begin
        Result:=eflags.zf;
      end;
    BX_LF_INDEX_OSZAPC:
      begin
        case oszapc.instr of
          BX_INSTR_ADD8,
          BX_INSTR_ADC8,
          BX_INSTR_SUB8,
          BX_INSTR_SBB8,
          BX_INSTR_CMP8,
          BX_INSTR_NEG8,
          BX_INSTR_XADD8,
          BX_INSTR_OR8,
          BX_INSTR_AND8,
          BX_INSTR_TEST8,
          BX_INSTR_XOR8,
          BX_INSTR_CMPS8,
          BX_INSTR_SCAS8,
          BX_INSTR_SHR8,
          BX_INSTR_SHL8:
            begin
              eflags.zf := Word(oszapc.result_8 = 0);
            end;
          BX_INSTR_ADD16,
          BX_INSTR_ADC16,
          BX_INSTR_SUB16,
          BX_INSTR_SBB16,
          BX_INSTR_CMP16,
          BX_INSTR_NEG16,
          BX_INSTR_XADD16,
          BX_INSTR_OR16,
          BX_INSTR_AND16,
          BX_INSTR_TEST16,
          BX_INSTR_XOR16,
          BX_INSTR_CMPS16,
          BX_INSTR_SCAS16,
          BX_INSTR_SHR16,
          BX_INSTR_SHL16:
            begin
              eflags.zf := Word(oszapc.result_16 = 0);
            end;
          BX_INSTR_ADD32,
          BX_INSTR_ADC32,
          BX_INSTR_SUB32,
          BX_INSTR_SBB32,
          BX_INSTR_CMP32,
          BX_INSTR_NEG32,
          BX_INSTR_XADD32,
          BX_INSTR_OR32,
          BX_INSTR_AND32,
          BX_INSTR_TEST32,
          BX_INSTR_XOR32,
          BX_INSTR_CMPS32,
          BX_INSTR_SCAS32,
          BX_INSTR_SHR32,
          BX_INSTR_SHL32:
            begin
              eflags.zf := Word(oszapc.result_32 = 0);
            end;
          else
            LogPanic(('get_ZF: OSZAPC: unknown instr'));
       end;
      lf_flags_status := lf_flags_status and $ff0fff;
      Result:=eflags.zf;
    end;
    BX_LF_INDEX_OSZAP:
      begin
        case oszap.instr of
          BX_INSTR_INC8,
          BX_INSTR_DEC8:
            begin
              eflags.zf := Word(oszap.result_8 = 0);
            end;
          BX_INSTR_INC16,
          BX_INSTR_DEC16:
            begin
              eflags.zf := Word(oszap.result_16 = 0);
            end;
          BX_INSTR_INC32,
          BX_INSTR_DEC32:
            begin
              eflags.zf := Word(oszap.result_32 = 0);
            end;
          else
            LogPanic(('get_ZF: OSZAP: unknown instr'));
        end;
        lf_flags_status := lf_flags_status and $ff0fff;
        Result:= eflags.zf;
      end;
    else
      LogPanic(('get_ZF: unknown case'));
      Result:=0;
  end;
end;

function TCPU.get_AF:Bool;
begin
  case ( (lf_flags_status shr 8) and $00000f ) of
    BX_LF_INDEX_KNOWN:
      Result := eflags.af;
    BX_LF_INDEX_OSZAPC:
      begin
        case oszapc.instr of
          BX_INSTR_ADD8,
          BX_INSTR_ADC8,
          BX_INSTR_SUB8,
          BX_INSTR_SBB8,
          BX_INSTR_CMP8,
          BX_INSTR_XADD8,
          BX_INSTR_CMPS8,
          BX_INSTR_SCAS8:
            begin
              eflags.af := ((oszapc.op1_8 xor oszapc.op2_8) xor oszapc.result_8) and $10;
            end;
          BX_INSTR_ADD16,
          BX_INSTR_ADC16,
          BX_INSTR_SUB16,
          BX_INSTR_SBB16,
          BX_INSTR_CMP16,
          BX_INSTR_XADD16,
          BX_INSTR_CMPS16,
          BX_INSTR_SCAS16:
            begin
              eflags.af := ((oszapc.op1_16 xor oszapc.op2_16) xor oszapc.result_16) and $10;
            end;
          BX_INSTR_ADD32,
          BX_INSTR_ADC32,
          BX_INSTR_SUB32,
          BX_INSTR_SBB32,
          BX_INSTR_CMP32,
          BX_INSTR_XADD32,
          BX_INSTR_CMPS32,
          BX_INSTR_SCAS32:
            begin
              eflags.af := ((oszapc.op1_32 xor oszapc.op2_32) xor oszapc.result_32) and $10;
            end;
          BX_INSTR_NEG8:
            begin
              eflags.af := Word((oszapc.op1_8 and $0f) > 0);
            end;
          BX_INSTR_NEG16:
            begin
              eflags.af := Word((oszapc.op1_16 and $0f) > 0);
            end;
          BX_INSTR_NEG32:
            begin
              eflags.af := Word((oszapc.op1_32 and $0f) > 0);
            end;
          BX_INSTR_OR8,
          BX_INSTR_OR16,
          BX_INSTR_OR32,
          BX_INSTR_AND8,
          BX_INSTR_AND16,
          BX_INSTR_AND32,
          BX_INSTR_TEST8,
          BX_INSTR_TEST16,
          BX_INSTR_TEST32,
          BX_INSTR_XOR8,
          BX_INSTR_XOR16,
          BX_INSTR_XOR32,
          BX_INSTR_SHR8,
          BX_INSTR_SHR16,
          BX_INSTR_SHR32,
          BX_INSTR_SHL8,
          BX_INSTR_SHL16,
          BX_INSTR_SHL32:
            begin
              eflags.af := 0;
            end;
          else
            LogPanic((Format('get_AF: OSZAPC: unknown instr %u',[oszapc.instr])));
        end;
        lf_flags_status := lf_flags_status and $fff0ff;
        Result:= eflags.af;
      end;
    BX_LF_INDEX_OSZAP:
      begin
        case oszap.instr of
          BX_INSTR_INC8:
            begin
              eflags.af := Word((oszap.result_8 and $0f) = 0);
            end;
          BX_INSTR_INC16:
            begin
              eflags.af := Word((oszap.result_16 and $0f) = 0);
            end;
          BX_INSTR_INC32:
            begin
              eflags.af := Word((oszap.result_32 and $0f) = 0);
            end;
          BX_INSTR_DEC8:
            begin
              eflags.af := Word((oszap.result_8 and $0f) = $0f);
            end;
          BX_INSTR_DEC16:
            begin
              eflags.af := Word((oszap.result_16 and $0f) = $0f);
            end;
          BX_INSTR_DEC32:
            begin
              eflags.af := Word((oszap.result_32 and $0f) = $0f);
            end;
          else
            LogPanic(Format('get_AF: OSZAP: unknown instr %u', [oszap.instr]));
        end;
      lf_flags_status := lf_flags_status and $fff0ff;
      Result:=eflags.af;
     end;
    else
      begin
        LogPanic(('get_AF: unknown case'));
        Result:=0;
      end;
   end;
end;

function TCPU.get_OF:Bool;
var
  op1_b7, op2_b7, result_b7:Bit8u;
  op1_b15, op2_b15, result_b15:Bit16u;
  op1_b31, op2_b31, result_b31:Bit32u;
  cond:Boolean;
begin
  case ( (lf_flags_status shr 20) and $00000f ) of
    BX_LF_INDEX_KNOWN:
      Result:=eflags.of_;
    BX_LF_INDEX_OSZAPC:
    begin
    case (oszapc.instr) of
      BX_INSTR_ADD8,BX_INSTR_ADC8,BX_INSTR_XADD8:
        begin
          op1_b7    := oszapc.op1_8 and $80;
          op2_b7    := oszapc.op2_8 and $80;
          result_b7 := oszapc.result_8 and $80;
          Cond:=(op1_b7 = op2_b7) and ((result_b7 xor op2_b7)<>0);
          eflags.of_ :=Word(Cond);

        end;
      BX_INSTR_ADD16,BX_INSTR_ADC16,BX_INSTR_XADD16:
        begin
          op1_b15 := oszapc.op1_16 and $8000;
          op2_b15 := oszapc.op2_16 and $8000;
          result_b15 := oszapc.result_16 and $8000;
          Cond:=(op1_b15 = op2_b15) and ((result_b15 xor op2_b15)<>0);
          eflags.of_ :=  Word(Cond);

        end;
      BX_INSTR_ADD32,BX_INSTR_ADC32,BX_INSTR_XADD32:
        begin
          op1_b31 := oszapc.op1_32 and $80000000;
          op2_b31 := oszapc.op2_32 and $80000000;
          result_b31 := oszapc.result_32 and $80000000;
          Cond:=(op1_b31 = op2_b31) and ((result_b31 xor op2_b31)<>0);
          eflags.of_ := Word(Cond);

        end;
      BX_INSTR_SUB8,BX_INSTR_SBB8,BX_INSTR_CMP8,BX_INSTR_CMPS8,BX_INSTR_SCAS8:
        begin
          op1_b7 := oszapc.op1_8 and $80;
          op2_b7 := oszapc.op2_8 and $80;
          result_b7 := oszapc.result_8 and $80;
          Cond:=((op1_b7 xor op2_b7)<>0) and ((op1_b7 xor result_b7)<>0);
          eflags.of_ := Word(Cond);

        end;
      BX_INSTR_SUB16,BX_INSTR_SBB16,BX_INSTR_CMP16,BX_INSTR_CMPS16,BX_INSTR_SCAS16:
        begin
          op1_b15 := oszapc.op1_16 and $8000;
          op2_b15 := oszapc.op2_16 and $8000;
          result_b15 := oszapc.result_16 and $8000;
          Cond:=((op1_b15 xor op2_b15)<>0) and ((op1_b15 xor result_b15)<>0);
          eflags.of_ :=  Word(Cond);

        end;
      BX_INSTR_SUB32,BX_INSTR_SBB32,BX_INSTR_CMP32,BX_INSTR_CMPS32,BX_INSTR_SCAS32:
        begin
          op1_b31 := oszapc.op1_32 and $80000000;
          op2_b31 := oszapc.op2_32 and $80000000;
          result_b31 := oszapc.result_32 and $80000000;
          Cond:=((op1_b31 xor op2_b31)<>0) and ((op1_b31 xor result_b31)<>0);
          eflags.of_ := Word(Cond);

        end;
      BX_INSTR_NEG8:
        begin
          eflags.of_ := Word((oszapc.op1_8 = $80));

        end;
      BX_INSTR_NEG16:
        begin
          eflags.of_ := Word(oszapc.op1_16 = $8000);

        end;
      BX_INSTR_NEG32:
        begin
          eflags.of_ := Word((oszapc.op1_32 = $80000000));

        end;
      BX_INSTR_OR8, BX_INSTR_OR16,BX_INSTR_OR32,BX_INSTR_AND8,BX_INSTR_AND16,BX_INSTR_AND32,BX_INSTR_TEST8,
        BX_INSTR_TEST16,BX_INSTR_TEST32,BX_INSTR_XOR8,BX_INSTR_XOR16,BX_INSTR_XOR32:
        begin
          eflags.of_ := 0;

        end;
      BX_INSTR_SHR8:
        begin
          if (oszapc.op2_8 = 1) then
            eflags.of_ := Word((oszapc.op1_8 >= $80));

        end;
      BX_INSTR_SHR16:
        begin
          if (oszapc.op2_16 = 1) then
            eflags.of_ := Word((oszapc.op1_16 >= $8000));

        end;
      BX_INSTR_SHR32:
        begin
          if (oszapc.op2_32 = 1) then
            eflags.of_ := Word((oszapc.op1_32 >= $80000000));

        end;
      BX_INSTR_SHL8:
        begin
          if (oszapc.op2_8 = 1) then eflags.of_ :=Word(((oszapc.op1_8 xor oszapc.result_8) and $80) > 0);

        end;
      BX_INSTR_SHL16:
        begin
          if (oszapc.op2_16 = 1) then eflags.of_ :=  Word(((oszapc.op1_16 xor oszapc.result_16) and $8000) > 0);

        end;
      BX_INSTR_SHL32:
        begin
          if (oszapc.op2_32 = 1) then eflags.of_ :=Word(((oszapc.op1_32 xor oszapc.result_32) and $80000000) > 0);

        end;
      else
        LogPanic('get_OF: OSZAPC: unknown instr');
    end;
    lf_flags_status := lf_flags_status and $0fffff;
    Result:=eflags.of_;
    end;

    BX_LF_INDEX_OSZAP:
      begin
      case oszap.instr of
        BX_INSTR_INC8:
          begin
            eflags.of_ := Word(oszap.result_8 = $80);

          end;
        BX_INSTR_INC16:
          begin
            eflags.of_ := Word(oszap.result_16 = $8000);

          end;
        BX_INSTR_INC32:
          begin
            eflags.of_ := Word(oszap.result_32 = $80000000);

          end;
        BX_INSTR_DEC8:
          begin
            eflags.of_ := Word(oszap.result_8 = $7F);

          end;
        BX_INSTR_DEC16:
          begin
            eflags.of_ := Word(oszap.result_16 = $7FFF);

          end;
        BX_INSTR_DEC32:
          begin
            eflags.of_ := Word(oszap.result_32 = $7FFFFFFF);

          end;
        else
          LogPanic('get_OF: OSZAP: unknown instr');
        end;
      lf_flags_status := lf_flags_status and $0fffff;
      Result:= eflags.of_;
    end;
  end;
end;

function TCPU.get_PF:Bool;
begin
  case Word((lf_flags_status shr 4) and $00000f ) of
    BX_LF_INDEX_KNOWN:
      begin
        Result:=lf_pf;
      end;
    BX_LF_INDEX_OSZAPC:
      begin
      case oszapc.instr of
        BX_INSTR_ADD8,
        BX_INSTR_ADC8,
        BX_INSTR_SUB8,
        BX_INSTR_SBB8,
        BX_INSTR_CMP8,
        BX_INSTR_NEG8,
        BX_INSTR_XADD8,
        BX_INSTR_OR8,
        BX_INSTR_AND8,
        BX_INSTR_TEST8,
        BX_INSTR_XOR8,
        BX_INSTR_CMPS8,
        BX_INSTR_SCAS8,
        BX_INSTR_SHR8,
        BX_INSTR_SHL8:
          begin
            lf_pf := bx_parity_lookup[oszapc.result_8];

          end;
        BX_INSTR_ADD16,
        BX_INSTR_ADC16,
        BX_INSTR_SUB16,
        BX_INSTR_SBB16,
        BX_INSTR_CMP16,
        BX_INSTR_NEG16,
        BX_INSTR_XADD16,
        BX_INSTR_OR16,
        BX_INSTR_AND16,
        BX_INSTR_TEST16,
        BX_INSTR_XOR16,
        BX_INSTR_CMPS16,
        BX_INSTR_SCAS16,
        BX_INSTR_SHR16,
        BX_INSTR_SHL16:
          begin
            lf_pf := bx_parity_lookup[Bit8u(oszapc.result_16)];

          end;
        BX_INSTR_ADD32,
        BX_INSTR_ADC32,
        BX_INSTR_SUB32,
        BX_INSTR_SBB32,
        BX_INSTR_CMP32,
        BX_INSTR_NEG32,
        BX_INSTR_XADD32,
        BX_INSTR_OR32,
        BX_INSTR_AND32,
        BX_INSTR_TEST32,
        BX_INSTR_XOR32,
        BX_INSTR_CMPS32,
        BX_INSTR_SCAS32,
        BX_INSTR_SHR32,
        BX_INSTR_SHL32:
          begin
            lf_pf := bx_parity_lookup[Bit8u(oszapc.result_32)];

          end;
        else
          LogPanic(('get_PF: OSZAPC: unknown instr'));
       end;
       lf_flags_status := lf_flags_status and $ffff0f;
       Result:=lf_pf;

     end;
    BX_LF_INDEX_OSZAP:
      begin
        case oszap.instr of
          BX_INSTR_INC8, BX_INSTR_DEC8:
            begin
              lf_pf := bx_parity_lookup[oszap.result_8];

            end;
          BX_INSTR_INC16, BX_INSTR_DEC16:
            begin
              lf_pf := bx_parity_lookup[Bit8u(oszap.result_16)];

            end;
          BX_INSTR_INC32, BX_INSTR_DEC32:
            begin
              lf_pf := bx_parity_lookup[Bit8u(oszap.result_32)];

            end;
          else
            LogPanic(('get_PF: OSZAP: unknown instr'));
        end;  //case oszap.instr of
        lf_flags_status := lf_flags_status and $ffff0f;
        Result:=lf_pf;

      end;    //BX_LF_INDEX_OSZAP
    BX_LF_INDEX_P:
      begin
        lf_pf := bx_parity_lookup[eflags.pf_byte];
        lf_flags_status := lf_flags_status and $ffff0f;
        Result:=lf_pf;

      end;  //case BX_LF_INDEX_P:
    else
      begin
        LogPanic(('get_PF: unknown case'));
        Result:=0;
      end;
  end; //case Word((lf_flags_status shr 4) and $00000f ) of
end;

function TCPU.BX_READ_16BIT_REG(index:Word):Bit16u;
begin
  Result:=gen_reg[index].rx;
end;

function TCPU.BX_READ_32BIT_REG(index:Word):Bit32u;
begin
  Result:=gen_reg[index].erx;
end;

function TCPU.get_CF:Bool;
var
  cond:Boolean;
begin
  case lf_flags_status and $00000f of
    BX_LF_INDEX_KNOWN:
      begin
        Result:=eflags.cf;
      end;

    BX_LF_INDEX_OSZAPC:
      begin
        case oszapc.instr of
          BX_INSTR_ADD8,
          BX_INSTR_XADD8:
            begin
              eflags.cf := Word(oszapc.result_8 < oszapc.op1_8);
            end;
          BX_INSTR_ADD16,
          BX_INSTR_XADD16:
            begin
              eflags.cf := Word(oszapc.result_16 < oszapc.op1_16);
            end;
          BX_INSTR_ADD32,
          BX_INSTR_XADD32:
            begin
              eflags.cf := Word(oszapc.result_32 < oszapc.op1_32);
            end;
          BX_INSTR_ADC8:
            begin
              cond := (oszapc.result_8 < oszapc.op1_8) or ((oszapc.prev_CF<>0) and (oszapc.result_8 = oszapc.op1_8));
              eflags.cf := Word(cond);
            end;
          BX_INSTR_ADC16:
            begin
              cond := (oszapc.result_16 < oszapc.op1_16) or ((oszapc.prev_CF<>0) and (oszapc.result_16 = oszapc.op1_16));
              eflags.cf := Word(Cond);
            end;
          BX_INSTR_ADC32:
            begin
              cond := (oszapc.result_32 < oszapc.op1_32) or ((oszapc.prev_CF<>0) and (oszapc.result_32 = oszapc.op1_32));
              eflags.cf := Word(Cond);
            end;
          BX_INSTR_SUB8,
          BX_INSTR_CMP8,
          BX_INSTR_CMPS8,
          BX_INSTR_SCAS8:
            begin
              eflags.cf := Word(oszapc.op1_8 < oszapc.op2_8);
            end;
          BX_INSTR_SUB16,
          BX_INSTR_CMP16,
          BX_INSTR_CMPS16,
          BX_INSTR_SCAS16:
            begin
              eflags.cf := Word(oszapc.op1_16 < oszapc.op2_16);
            end;
          BX_INSTR_SUB32,
          BX_INSTR_CMP32,
          BX_INSTR_CMPS32,
          BX_INSTR_SCAS32:
            begin
              eflags.cf := Word(oszapc.op1_32 < oszapc.op2_32);
            end;
          BX_INSTR_SBB8:
            begin
              cond := (oszapc.op1_8 < oszapc.result_8) or ((oszapc.op2_8 = $ff) and (oszapc.prev_CF<>0));
              eflags.cf := Word(Cond);
            end;
          BX_INSTR_SBB16:
            begin
              Cond := (oszapc.op1_16 < oszapc.result_16) or ((oszapc.op2_16 = $ffff) and (oszapc.prev_CF<>0));
              eflags.cf := Word(Cond);
            end;
          BX_INSTR_SBB32:
            begin
              Cond :=(oszapc.op1_32 < oszapc.result_32) or ((oszapc.op2_32 = $ffffffff) and (oszapc.prev_CF<>0));
              eflags.cf := Word(cond);
            end;
          BX_INSTR_NEG8:
            begin
              eflags.cf := Word(oszapc.op1_8 <> 0);
            end;
          BX_INSTR_NEG16:
            begin
              eflags.cf := Word(oszapc.op1_16 <> 0);
            end;
          BX_INSTR_NEG32:
            begin
              eflags.cf := Word(oszapc.op1_32 <> 0);
            end;
          BX_INSTR_OR8,
          BX_INSTR_OR16,
          BX_INSTR_OR32,
          BX_INSTR_AND8,
          BX_INSTR_AND16,
          BX_INSTR_AND32,
          BX_INSTR_TEST8,
          BX_INSTR_TEST16,
          BX_INSTR_TEST32,
          BX_INSTR_XOR8,
          BX_INSTR_XOR16,
          BX_INSTR_XOR32:
            begin
              eflags.cf := 0;
            end;
          BX_INSTR_SHR8:
            begin
              eflags.cf := (oszapc.op1_8 shr (oszapc.op2_8 - 1)) and $01;
            end;
          BX_INSTR_SHR16:
            begin
              eflags.cf := (oszapc.op1_16 shr (oszapc.op2_16 - 1)) and $01;
            end;
          BX_INSTR_SHR32:
            begin
              eflags.cf := (oszapc.op1_32 shr (oszapc.op2_32 - 1)) and $01;
            end;
          BX_INSTR_SHL8:
            begin
              if oszapc.op2_8 <= 8 then
                eflags.cf := (oszapc.op1_8 shr (8 - oszapc.op2_8)) and $01
              else
                eflags.cf := 0;
            end;
        BX_INSTR_SHL16:
          begin
            if oszapc.op2_16 <= 16 then
              eflags.cf := (oszapc.op1_16 shr (16 - oszapc.op2_16)) and $01
            else
              eflags.cf := 0;
          end;
        BX_INSTR_SHL32:
          begin
            eflags.cf := (oszapc.op1_32 shr (32 - oszapc.op2_32)) and $01;
          end;
        else
          LogPanic(Format('get_CF: OSZAPC: unknown instr %u',[oszapc.instr]));
      end;
      lf_flags_status := lf_flags_status and $fffff0;
      Result:= eflags.cf;
    end;
    else
      begin
        LogPanic(('get_CF: unknown case'));
        Result:=0;
      end;
  end;
end;

constructor TCPU.Create;
begin
  {$ifdef RECORD_VM}
  inherited Create(LongWord(@fake_start),LongWord(@fake_end),idCPU);
  {$endif}
end;

destructor TCPU.Destroy;
begin
  DoneLogFiles;
  {$ifdef RECORD_VM}
  inherited;
  {$endif}
end;

function TCPU.real_mode:Bool;
begin
  Result:=Bool(FCR0.pe=0);
end;

function TCPU.v8086_mode:Bool;
begin
  result:=eflags.vm;
end;

function TCPU.CPL:Bit8u;
begin
  Result:=sregs[SEG_REG_CS].selector.rpl;
end;

procedure TCPU.read_virtual_checks(seg:Psegment_reg_t;  offset:Bit32u; length:Word);
var
  upper_limit: Bit32u;
begin
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if  seg.cache.valid=0 then
    begin
      exception2([BX_GP_EXCEPTION, 0, 0]);
      exit;
    end;

    if seg.cache.p = 0 then
    begin  // not present
      exception2([int_number(seg), 0, 0]);
      exit;
    end;
    case seg.cache.type_ of
      0,1,    // read only
      10, 11, // execute/read
      14, 15: // execute/read-only, conforming
          if (offset > (seg.cache.segment.limit_scaled - length + 1)) or (length-1 > seg.cache.segment.limit_scaled) then
            begin
              exception(int_number(seg), 0, 0);
              exit;
            end;

      2,3: // read/write
          if (offset > (seg.cache.segment.limit_scaled - length + 1)) or (length-1 > seg.cache.segment.limit_scaled) then
              exception(int_number(seg), 0, 0);
      4,5: // read only, expand down
        begin
          if (seg.cache.segment.d_b) <> 0
            then upper_limit := $ffffffff
            else upper_limit := $0000ffff;

          if ( (offset <= seg.cache.segment.limit_scaled) or (offset > upper_limit)
             or ((upper_limit - offset) < (length - 1)) ) then
          begin
            exception(int_number(seg), 0, 0);
            exit;
          end;
        end;
      6,7: // read write, expand down
        begin
          if (seg.cache.segment.d_b) <> 0
            then upper_limit := $ffffffff
            else upper_limit := $0000ffff;

          if ( (offset <= seg.cache.segment.limit_scaled) or
               (offset > upper_limit) or
               ((upper_limit - offset) < (length - 1)) ) then
          begin
            exception(int_number(seg), 0, 0);
            exit;
          end;
        end;
      8, 9, // execute only
      12, 13: exit;// execute only, conforming // can't read or write an execute-only segment

    end; //CASE
  end else
  begin
    if (offset > (seg.cache.segment.limit_scaled - length + 1)) or (length-1 > seg.cache.segment.limit_scaled) then
      if (seg = @sregs[2])
        then exception2([BX_SS_EXCEPTION, 0, 0])
        else exception(BX_GP_EXCEPTION, 0, 0);
  end;
end;

procedure TCPU.write_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
var
  laddr:Bit32u;
  seg:Psegment_reg_t;
begin

  seg := @sregs[s];
  write_virtual_checks(seg, offset, 1);

  laddr := seg.cache.segment.base + offset;
  //BX_INSTR_MEM_DATA(laddr, 1, BX_WRITE);

  // all checks OK
  access_linear(laddr, 1, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_WRITE, data);
end;

procedure TCPU.write_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  write_virtual_checks(seg, offset, 2);

  laddr := seg.cache.segment.base + offset;
  // all checks OK
  access_linear(laddr, 2, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_WRITE, data);
end;

procedure TCPU.write_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  write_virtual_checks(seg, offset, 4);

  laddr := seg.cache.segment.base + offset;
  // all checks OK
  access_linear(laddr, 4, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_WRITE, data);
end;

procedure TCPU.read_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  read_virtual_checks(seg, offset, 1);

  laddr := seg.cache.segment.base + offset;
  // all checks OK
  access_linear(laddr, 1, Word(CPL=3), BX_READ, data);
end;

procedure TCPU.read_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  read_virtual_checks(seg, offset, 2);

  laddr := seg.cache.segment.base + offset;
  // all checks OK
  access_linear(laddr, 2, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, data);
end;

procedure TCPU.read_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  read_virtual_checks(seg, offset, 4);
  laddr := seg.cache.segment.base + offset;
  // all checks OK
  access_linear(laddr, 4, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_READ, data);
end;

procedure TCPU.read_RMW_virtual_byte(s:Word; offset:Bit32u; data:PBit8u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  write_virtual_checks(seg, offset, 1);
  laddr := seg.cache.segment.base + offset;
  // all checks OK
  if ((FCR0.pg))<>0
    then access_linear(laddr, 1, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_RW, data)
    else
    begin
      address_xlation.paddress1 := laddr;
      sysmemory.read_physical(laddr, 1, data);
    end;
end;

procedure TCPU.read_RMW_virtual_word(s:Word; offset:Bit32u; data:PBit16u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  write_virtual_checks(seg, offset, 2);

  laddr := seg.cache.segment.base + offset;
  // all checks OK
  if (FCR0.pg)<>0
    then access_linear(laddr, 2, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_RW, data)
    else
    begin
      address_xlation.paddress1 := laddr;
      sysmemory.read_physical(laddr, 2, data);
    end;
end;

procedure TCPU.read_RMW_virtual_dword(s:Word; offset:Bit32u; data:PBit32u);
var
  laddr: Bit32u;
  seg: Psegment_reg_t;
begin
  seg := @sregs[s];
  write_virtual_checks(seg, offset, 4);
  laddr := seg.cache.segment.base + offset;
  // all checks OK
  if (FCR0.pg) <> 0
    then access_linear(laddr, 4, Word(bx_cpu.sregs[SEG_REG_CS].selector.rpl), BX_RW, data)
    else
    begin
      address_xlation.paddress1 := laddr;
      sysmemory.read_physical(laddr, 4, data);
    end;
end;

procedure TCPU.write_RMW_virtual_word(val16:Bit16u);
begin
  if (FCR0.pg) <> 0 then
  begin
    if address_xlation.pages = 1 then
      sysmemory.write_physical(address_xlation.paddress1, 2, @val16)
    else
    begin
      sysmemory.write_physical(address_xlation.paddress1, 1, @val16);
      sysmemory.write_physical(address_xlation.paddress2, 1, (PBit8u(LongInt(@val16) + 1)));
//        sysmemory.write_physical(address_xlation.paddress1, 1, (PBit8u(LongInt(@val16) + 1)));
//        sysmemory.write_physical(address_xlation.paddress2, 1, @val16);
    end;
  end
  else
    sysmemory.write_physical(address_xlation.paddress1, 2, @val16);
end;

procedure TCPU.write_RMW_virtual_dword(val32:Bit32u);
begin
  if (FCR0.pg) <> 0 then
  begin
    if address_xlation.pages = 1
      then sysmemory.write_physical(address_xlation.paddress1, 4, @val32)
      else
      begin
        sysmemory.write_physical(address_xlation.paddress1, 1, @val32);
        sysmemory.write_physical(address_xlation.paddress2, 1, (PBit8u(LongInt(@val32) + address_xlation.len1)));
//        sysmemory.write_physical(address_xlation.paddress1, 1, (PBit8u(LongInt(@val32) + (4 - address_xlation.len1))));
//        sysmemory.write_physical(address_xlation.paddress2, 1, @val32);
      end;
  end else
    sysmemory.write_physical(address_xlation.paddress1, 4, @val32);
end;

procedure TCPU.write_virtual_checks(seg:Psegment_reg_t; offset:Bit32u; length:Bit32u);
var
  upper_limit:Bit32u;
begin
  if (Bool((Self.FCR0.pe <> 0) and (Self.eflags.vm = 0))) <> 0 then
  begin
    if ( seg.cache.valid=0 ) then
    begin
      LogError(Format('seg = %s', [strseg(@seg)]));
      LogError(Format('seg->selector.value = %04x',[Word(seg.selector.value)]));
      LogError('write_virtual_checks: valid bit = 0');
      LogError(Format('CS: %04x', [Word(sregs[1].selector.value)]));
      LogError(Format('IP: %04x', [Word(prev_eip)]));
      exception2([BX_GP_EXCEPTION, 0, 0]);
    end;

    if (seg.cache.p = 0) then
    begin // not present
      LogInfo(('write_virtual_checks(): segment not present'));
      exception(int_number(seg), 0, 0);
      exit;
    end;

    case seg^.cache.type_ of
      0, 1,   // read only
      4, 5,   // read only, expand down
      8, 9,   // execute only
      10, 11, // execute/read
      12, 13, // execute only, conforming
      14, 15: // execute/read-only, conforming
       begin
         LogInfo('write_virtual_checks(): no write access to seg');
         exception(int_number(seg), 0, 0);
         exit;
       end;

      2,3: // read/write
       begin
        	if (offset > seg^.cache.segment.limit_scaled - length + 1)
      	    or (length-1 > seg^.cache.segment.limit_scaled) then
          begin
      		  LogInfo(('write_virtual_checks(): write beyond limit, r/w'));
            exception(int_number(seg), 0, 0);
            exit;
          end;
       end;

      6,7: // read write, expand down
       begin
          if ((seg^.cache.segment.d_b)) <> 0
            then upper_limit := $ffffffff
            else upper_limit := $0000ffff;
          if ( (offset <= seg^.cache.segment.limit_scaled) or (offset > upper_limit) or
               ((upper_limit - offset) < (length - 1)) ) then
               begin
        		    LogInfo(('write_virtual_checks(): write beyond limit, r/w ED'));
                exception(int_number(seg), 0, 0);
               end;
       end;
    else  // real mode
      if (offset > seg^.cache.segment.limit_scaled - length + 1) or (length-1 > seg^.cache.segment.limit_scaled) then
        if (seg = @sregs[2])
          then exception(BX_SS_EXCEPTION, 0, 0)
          else exception(BX_GP_EXCEPTION, 0, 0);
    end;
  end;
end;

function TCPU.strseg(seg:Psegment_reg_t):PChar;
begin
  if (seg = @sregs[0]) then
    Result := 'ES'
  else
  if (seg = @sregs[1]) then
    Result := 'CS'
  else
  if (seg = @sregs[2]) then
    Result := 'SS'
  else
  if (seg = @sregs[3]) then
    Result := 'DS'
  else
  if (seg = @sregs[4]) then
    Result := 'FS'
  else
  if (seg = @sregs[5]) then
    Result := 'GS'
  else
  begin
    LogError(('undefined segment passed to strseg()!'));
    Result := '??';
  end;
end;

procedure TCPU.write_RMW_virtual_byte(val8:Bit8u);
begin
  // лишнее
  {if (cr0.pg <> 0) then
    sysmemory.write_physical(address_xlation.paddress1, 1, @val8)
  else}
    sysmemory.write_physical(address_xlation.paddress1, 1, @val8);
end;

function TCPU.BX_READ_8BIT_REG(index:Word):Bit8u;
begin
  if Index < 4 then
    Result := gen_reg[Index].rl
  else
    Result := gen_reg[index - 4].rh;
end;

procedure TCPU.BX_WRITE_8BIT_REG(index:Word; val:Bit8u);
begin
  if index < 4 then
    gen_reg[index].rl := val
  else
    gen_reg[index - 4].rh := val;
end;

procedure TCPU.BX_WRITE_16BIT_REG(index:Word; val:Bit16u);
begin
  gen_reg[index].rx := val;
end;

procedure TCPU.invalidate_prefetch_q;
begin
  bytesleft := 0;
end;

procedure TCPU.revalidate_prefetch_q;
var
  new_linear_addr, new_linear_page, new_linear_offset: Bit32u;
  new_phy_addr: Bit32u;
begin
  new_linear_addr := Self.sregs[SEG_REG_CS].cache.segment.base + Self.FEIP;

  new_linear_page := new_linear_addr  and $fffff000;
  if (new_linear_page = Self.prev_linear_page) then
  begin
    // same linear address, old linear^.physical translation valid
    new_linear_offset := new_linear_addr  and $00000fff;
    new_phy_addr := Self.prev_phy_page or new_linear_offset;
{$if BX_PCI_SUPPORT=1}
    if (new_phy_addr >= $000C0000) and (new_phy_addr <= $000FFFFF) then
    begin
      Self.bytesleft := $4000 - (new_phy_addr  and $3FFF);
      Self.fetch_ptr := bx_pci.i440fx_fetch_ptr(new_phy_addr);
    end else
    begin
      Self.bytesleft := (Self.max_phy_addr - new_phy_addr) + 1;
      Self.fetch_ptr := PBit8u(Integer(sysmemory.vector) + new_phy_addr);
    end;
{$else}
    Self.bytesleft := (Self.max_phy_addr - new_phy_addr) + 1;
    Self.fetch_ptr := @sysmemory.vector[new_phy_addr];
{$ifend}
  end else
    Self.bytesleft := 0; // invalidate prefetch Q
end;

function TCPU.GetEIP:Bit16u;
begin
  Result := PBit16u(Integer(@Self.FEIP) + BX_REG16_OFFSET)^; //(* (Bit16u *) (((Bit8u *) &BX_CPU_THIS_PTR eip) + BX_REG16_OFFSET))
end;

procedure TCPU.SetEIP(IPValue:Bit16u);
begin
  PBit16u(Integer(@Self.FEIP) + BX_REG16_OFFSET)^ := IPValue;
end;

procedure Confronta(var RecPascal,RecC:recstate;var fpout:textfile;var Errori:LongWord);
begin
  if (RecPascal.a0 <> RecC.a0) or
     (RecPascal.a1 <> RecC.a1) or
     (RecPascal.a2 <> RecC.a2) or
     (RecPascal.a3 <> RecC.a3) or
     (RecPascal.a4 <> RecC.a4) or
     (RecPascal.a5 <> RecC.a5) or
     (RecPascal.a6 <> RecC.a6) or
     (RecPascal.a7 <> RecC.a7) or
     (RecPascal.a8 <> RecC.a8) or
     (RecPascal.a9 <> RecC.a9) then
  begin
    WriteLn(fpout,Format('P %d;[%x];%x;%x;%x;%x;%x;%x;%x;%x;',
        [RecPascal.a0, RecPascal.a1, RecPascal.a2, RecPascal.a3, RecPascal.a4,
         RecPascal.a5, RecPascal.a6, RecPascal.a7, RecPascal.a8, RecPascal.a9]));
    WriteLn(fpout,Format('C %d;[%x];%x;%x;%x;%x;%x;%x;%x;%x;',
        [RecC.a0, RecC.a1, RecC.a2, RecC.a3, RecC.a4, RecC.a5, RecC.a6, RecC.a7,
         RecC.a8, RecC.a9]));
    WriteLn(fpout, '-------------------------------------------------');
    Inc(Errori);
  end;
end;

procedure TCPU.cpu_loop;
var
//  ret: unsigned;
  i: TInstruction_tag;
  maxisize: Word;
  fetch_ptr: PBit8u;
  is_32: Bool;
  fakeword: word;
  remain, j: unsigned;
  FetchBuffer: array[0..16] of Bit8u;
  temp_ptr: PBit8u;
  vector: Bit8u;

  // delete it
  _disasmStr,
  _opcodeStr: string;
  _opcodeLen: integer;

  label main_cpu_loop, handle_async_event, async_events_processed;
  label fetch_decode_OK, repeat_loop, repeat_done;
  label repeat_not_done, debugger_check, theend, begin_cpu_loop;
begin

  // not sure if these two are used during the async handling... --bbd
  FillChar(i, SizeOf(i), 0);
  Prog := 0;
begin_cpu_loop:
  reloop := False;
  setjmp(savejump);
  self.prev_eip := self.FEIP; // commit new EIP
  self.prev_esp := self.ESP; // commit new ESP

main_cpu_loop:

  // ???
  self.EXT := 0;
  self.errorno := 0;

  // First check on events which occurred for previous instructions
  // (traps) and ones which are asynchronous to the CPU
  // (hardware interrupts).
  if (self.async_event) <> 0 then
    goto handle_async_event;

async_events_processed:
  // added so that all debugging/tracing code uses the correct EIP even in the
  // instruction just after a trap/interrupt.  If you use the prev_eip that was
  // set before handle_async_event, traces and breakpoints fail to show the
  // first instruction of int/trap handlers.
  self.prev_eip := self.FEIP; // commit new EIP
  self.prev_esp := self.ESP; // commit new ESP
  // Now we can handle things which are synchronous to instruction
  // execution.
  if (self.eflags.rf) <> 0 then
    self.eflags.rf := 0;
  // We have ignored processing of external interrupts and
  // debug events on this boundary.  Reset the mask so they
  // will be processed on the next boundary.
  self.inhibit_mask := 0;

  is_32 := self.sregs[SEG_REG_CS].cache.segment.d_b;

  if (self.bytesleft = 0) then
    self.prefetch();
  fetch_ptr := self.fetch_ptr;

  maxisize := 16;
  if (self.bytesleft < 16) then
    maxisize := self.bytesleft;
//  ret := self.FetchDecode(fetch_ptr, i, maxisize, is_32);

  if self.FetchDecode(fetch_ptr, i, maxisize, is_32) <> 0 then
  begin
    if (@i.Resolvemodrm <> nil) then
      i.Resolvemodrm(@i); // call method on BX_CPU_C object
    inc(self.fetch_ptr, i.ilen);
    Dec(self.bytesleft, i.ilen);
fetch_decode_OK:
    if (i.rep_used <> 0) and ((i.attr and BxRepeatable) <> 0) then
    begin
repeat_loop:
      if (i.attr and BxRepeatableZF) <> 0 then
      begin
        if (i.as_32) <> 0 then
        begin
          if (self.ECX <> 0) then
          begin
            EvCPUCommand(@i);
            i.execute(@i);
            if Reloop then
              goto begin_cpu_loop;
            //ECX -:= 1; ????
            self.ECX := self.ECX - 1;
          end;
          if ((i.rep_used = $f3) and (self.get_ZF() = 0)) or
             ((i.rep_used = $f2) and (self.get_ZF() <> 0)) or
             (self.ECX = 0) then
            goto repeat_done;
          goto repeat_not_done;
        end else
        begin
          if (self.CX <> 0) then
          begin
            EvCPUCommand(@i);
            i.execute(@i);
            if Reloop then
              goto begin_cpu_loop;
            self.CX := self.CX - 1;
          end;
          if ((i.rep_used = $f3) and (self.get_ZF() = 0)) or
             ((i.rep_used = $f2) and (self.get_ZF() <> 0)) or
             (self.CX = 0) then
            goto repeat_done;
          goto repeat_not_done;
        end;
      end else
      begin // normal repeat, no concern for ZF
        if (i.as_32) <> 0 then
        begin
          if (self.ECX <> 0) then
          begin
            EvCPUCommand(@i);
            i.execute(@i);
            if Reloop then
              goto begin_cpu_loop;
            self.ECX := self.ECX - 1;
          end;
          if (self.ECX = 0) then
            goto repeat_done;
          goto repeat_not_done;
        end else
        begin // 16bit addrsize
          if (self.CX <> 0) then
          begin
            EvCPUCommand(@i);
            i.execute(@i);
            if Reloop then
              goto begin_cpu_loop;
            self.CX:=self.CX-1;
          end;
          if (self.CX = 0) then
            goto repeat_done;
          goto repeat_not_done;
        end;
      end;
      // shouldn't get here from above
repeat_not_done:
//      inc(ips_count);
      dec(bx_pc_system.num_cpu_ticks_left);
      if (bx_pc_system.num_cpu_ticks_left = 0) then
        bx_pc_system.timer_handler();

      if (self.async_event) <> 0 then
      begin
        self.invalidate_prefetch_q();
        goto debugger_check;
      end;
      goto repeat_loop;

repeat_done:
      self.FEIP := self.FEIP + i.ilen;
    end else
    begin
      // тестовый пример дизассемблирования кода
//      Self.DisAssemble((Self.sregs[SEG_REG_CS].cache.segment.base + Self.FEIP) and bx_pc_system.a20_mask, eip,
//        _disasmStr, _opcodeStr, _opcodeLen);
      // check for breakpoint
      EvCPUCommand(@i);
      // non repeating instruction
      self.FEIP := self.FEIP + i.ilen;
      i.execute(@i);
      // interrupt
      if Reloop then
        goto begin_cpu_loop;
      Inc(Prog);
    end;

    self.prev_eip := self.FEIP; // commit new EIP
    self.prev_esp := self.ESP; // commit new ESP
    dec(bx_pc_system.num_cpu_ticks_left);
    if (bx_pc_system.num_cpu_ticks_left = 0) then
        bx_pc_system.timer_handler();

debugger_check:
    if not FActive then
      goto theend;
    goto main_cpu_loop;
  end else
  begin
    // read all leftover bytes in current page
    j := 0;
    while j < self.bytesleft do
    begin
      FetchBuffer[j] := fetch_ptr^;
      Inc(fetch_ptr);
      Inc(j);
    end;
    // get remaining bytes for prefetch in next page
    // prefetch() needs eip current
    self.FEIP := self.FEIP + self.bytesleft;
    remain := self.bytesleft;
    self.prefetch();

    if (self.bytesleft < 16) then
      LogPanic(('fetch_decode: bytesleft=0 after prefetch')); // make sure (bytesleft - remain) below doesn't go negative

    fetch_ptr := self.fetch_ptr;
    temp_ptr := fetch_ptr;

    // read leftover bytes in next page
    while j < 16 do
    begin
      FetchBuffer[j] := temp_ptr^;
      inc(temp_ptr);
      Inc(j);
    end;
    fakeword := 16;
//    ret := self.FetchDecode(@FetchBuffer, i, fakeword, is_32);
    if (self.FetchDecode(@FetchBuffer, i, fakeword, is_32) = 0) then
      LogPanic(('fetchdecode: cross boundary: ret=0'));

    if (@i.Resolvemodrm)<>nil then
      i.Resolvemodrm(@i);
    remain := i.ilen - remain;
    // note: eip has already been advanced to beginning of page
    self.fetch_ptr := PBit8u(Integer(fetch_ptr) + remain);
    self.bytesleft := self.bytesleft - remain;
    //self.eip +:= remain;
    self.FEIP := self.prev_eip;
    goto fetch_decode_OK;
  end;
  // This area is where we process special conditions and events.
handle_async_event:

  if (self.debug_trap and $80000000) <> 0 then
  begin
    // I made up the bitmask above to mean HALT state.
//{$if BX_SMP_PROCESSORS=1}
    self.debug_trap := 0; // clear traps for after resume
    self.inhibit_mask := 0; // clear inhibits for after resume
    // for one processor, pass the time as quickly as possible until
    // an interrupt wakes up the CPU.
    while (True) do
    begin
      if ((self.INTR <> 0) and (self.eflags.if_ <> 0)) then
        break;
//      inc(ips_count);
      dec(bx_pc_system.num_cpu_ticks_left);
      if (bx_pc_system.num_cpu_ticks_left = 0) then
        bx_pc_system.timer_handler();
    end;
//{$else}      (* BX_SMP_PROCESSORS <> 1 *)
//    // for multiprocessor simulation, even if this CPU is halted we still
//    // must give the others a chance to simulate.  If an interrupt has
//    // arrived, then clear the HALT condition; otherwise just return from
//    // the CPU loop with stop_reason STOP_CPU_HALTED.
//    if self.INTR @ and self.eflags.if_) then begin
//      // interrupt ends the HALT condition
//      self.debug_trap := 0; // clear traps for after resume
//      self.inhibit_mask := 0; // clear inhibits for after resume
//      //bx_printf ('halt condition has been cleared in %s', name);
//    end; else begin
//      // HALT condition remains, return so other CPUs have a chance
//      exit;
//    end;
//{$ifend}
  end;
  // Priority 1: Hardware Reset and Machine Checks
  //   RESET
  //   Machine Check
  // (bochs doesn't support these)
  // Priority 2: Trap on Task Switch
  //   T flag in TSS is set
  if (self.debug_trap and $00008000)<>0 then
  begin
    self.FDR6 := self.FDR6 or self.debug_trap;
    exception2([BX_DB_EXCEPTION, 0, 0]); // no error, not interrupt
  end;
  // Priority 3: External Hardware Interventions
  //   FLUSH
  //   STOPCLK
  //   SMI
  //   INIT
  // (bochs doesn't support these)
  // Priority 4: Traps on Previous Instruction
  //   Breakpoints
  //   Debug Trap Exceptions (TF flag set or data/IO breakpoint)
  if (self.debug_trap<>0) and ((self.inhibit_mask and BX_INHIBIT_DEBUG)=0) then
  begin
    // A trap may be inhibited on this boundary due to an instruction
    // which loaded SS.  If so we clear the inhibit_mask below
    // and don't execute this code until the next boundary.
    // Commit debug events to DR6
    self.FDR6 := self.FDR6 or  self.debug_trap;
    exception2([BX_DB_EXCEPTION, 0, 0]); // no error, not interrupt
  end;
  // Priority 5: External Interrupts
  //   NMI Interrupts
  //   Maskable Hardware Interrupts
  if (self.inhibit_mask  and BX_INHIBIT_INTERRUPTS) <> 0 then
  begin
    // Processing external interrupts is inhibited on this
    // boundary because of certain instructions like STI.
    // inhibit_mask is cleared below, in which case we will have
    // an opportunity to check interrupts on the next instruction
    // boundary.
  end else
  if (self.INTR <> 0) and (self.eflags.if_ <> 0) and ((BX_DBG_ASYNC_INTR <> 0)) then
  begin
    // NOTE: similar code in .take_irq()
    vector := bx_pc_system.IAC(); // may set INTR with next interrupt
    //BX_DEBUG(('decode: interrupt %u',
    //                                   (unsigned) vector));
    self.errorno := 0;
    self.EXT     := 1; (* external event *)
    self.interrupt(vector, 0, 0, 0);
    //BX_INSTR_HWINTERRUPT(vector, self.sregs[BX_SEG_REG_CS].selector.value, self.eip);
  end else
    if (bx_pc_system.HRQ <> 0) then
      // NOTE: similar code in .take_dma()
      // assert Hold Acknowledge (HLDA) and go into a bus hold state
      bx_pc_system.raise_HLDA;

  // Priority 6: Faults from fetching next instruction
  //   Code breakpoint fault
  //   Code segment limit violation (priority 7 on 486/Pentium)
  //   Code page fault (priority 7 on 486/Pentium)
  // (handled in main decode loop)

  // Priority 7: Faults from decoding next instruction
  //   Instruction length > 15 bytes
  //   Illegal opcode
  //   Coprocessor not available
  // (handled in main decode loop etc)

  // Priority 8: Faults on executing an instruction
  //   Floating point execution
  //   Overflow
  //   Bound error
  //   Invalid TSS
  //   Segment not present
  //   Stack fault
  //   General protection
  //   Data page fault
  //   Alignment check
  // (handled by rest of the code)

  if (self.eflags.tf) <> 0 then
    // TF is set before execution of next instruction.  Schedule
    // a debug trap (#DB) after execution.  After completion of
    // next instruction, the code above will invoke the trap.
    self.debug_trap := self.debug_trap or $00004000; // BS flag in DR6


  if (self.INTR <> 0) or (self.debug_trap <> 0) or (bx_pc_system.HRQ <> 0) or (self.eflags.tf <> 0) = false then
    self.async_event := 0;
  goto async_events_processed;
  theend:
end;

// boundaries of consideration:
//
//  * physical memory boundary: 1024k (1Megabyte) (increments of...)
//  * A20 boundary:             1024k (1Megabyte)
//  * page boundary:            4k
//  * ROM boundary:             2k (dont care since we are only reading)
//  * segment boundary:         any

procedure TCPU.prefetch;
var
  new_linear_addr: Bit32u;
  new_phy_addr: Bit32u;
//  temp_eip: Bit32u;
begin
  // cs:eIP
  // prefetch QSIZE byte quantity aligned on corresponding boundary

//  temp_eip   := Self.FEIP;
//  temp_limit := Self.sregs[BX_SEG_REG_CS].cache.segment.limit_scaled;

  new_linear_addr := Self.sregs[SEG_REG_CS].cache.segment.base + Self.FEIP;
  Self.prev_linear_page := new_linear_addr  and $fffff000;

  if (Self.FCR0.pg)<>0 then
  begin
    // aligned block guaranteed to be all in one page, same A20 address
    new_phy_addr := itranslate_linear(new_linear_addr, Bool(bx_cpu.sregs[SEG_REG_CS].selector.rpl));
    new_phy_addr := new_phy_addr and bx_pc_system.a20_mask;
  end else
    new_phy_addr := new_linear_addr and bx_pc_system.a20_mask;

  if  (new_phy_addr >= sysmemory.len ) then
    // don't take this out if dynamic translation enabled,
    // otherwise you must make a check to see if bytesleft is 0 after
    // a call to prefetch() in the dynamic code.
    LogError(('prefetch: running in bogus memory'));

  // max physical address as confined by page boundary
  Self.prev_phy_page := new_phy_addr  and $fffff000;
  Self.max_phy_addr := Self.prev_phy_page or $00000fff;

  // check if segment boundary comes into play
  //if (temp_limit - temp_eip) < 4096) then begin
  //  end;

{$if BX_PCI_SUPPORT = 1}
  if (new_phy_addr >= $000C0000) and (new_phy_addr <= $000FFFFF) then
  begin
    Self.bytesleft := $4000 - (new_phy_addr  and $3FFF);
    Self.fetch_ptr := bx_pci.i440fx_fetch_ptr(new_phy_addr);
  end else
  begin
    Self.bytesleft := (Self.max_phy_addr - new_phy_addr) + 1;
    Self.fetch_ptr := @sysmemory.vector[new_phy_addr];
  end;
{$else}
  Self.bytesleft := (Self.max_phy_addr - new_phy_addr) + 1;
  Self.fetch_ptr := @sysmemory.vector[new_phy_addr];
{$ifend}
end;

procedure TCPU.fpu_execute(I: PInstruction_tag);
var
  addr_modes: fpu_addr_modes;
  data_address: LongWord;
  data_sel_off: address;
  entry_sel_off: address;
  is_32: Bool;
//  ValueTestSwd, ValueTestTwd:LongWord;
//  ValueTestFtop:byte;
//  ValueFpuReg:FPU_REG;
//  Readed:Integer;
begin
  varfpu_iptr := i;
  varfpu_cpu_ptr := @Self;
  if (Bool((Self.FCR0.pe<>0) and (Self.eflags.vm=0))<>0) then
    addr_modes.default_mode := SEG32
  else
  if (v8086_mode()<>0) then
    addr_modes.default_mode := VM86
    // real mod_e, use vm86 for now
  else
    addr_modes.default_mode := VM86;

  // Mark if instruction used opsize or addrsize prefixes
  // Actually, addr_mod_es.override.address_size is not used,
  // could delete that code.
  is_32 := Self.sregs[SEG_REG_CS].cache.segment.d_b;
  if (i^.as_32 = is_32) then
    addr_modes.override_.address_size := 0
  else
    addr_modes.override_.address_size := ADDR_SIZE_PREFIX;

  if (i^.os_32 = is_32) then
    addr_modes.override_.operand_size := 0
  else
    addr_modes.override_.operand_size := OP_SIZE_PREFIX;

  // For now set access_limit to max.  It seems to be
  // a number from 0..255 denoting how many bytes the
  // current instruction can access according to its
  // memory operand.  255 means >= 255.
  access_limit^ := $ff;

  // fill in orig eip here in offset
  // fill in CS in selector
  entry_sel_off.offset := Self.prev_eip;
  entry_sel_off.selector := Self.sregs[SEG_REG_CS].selector.value;

// should set these fields to 0 if mem operand not used
  Data_Address := i^.rm_addr;
  data_sel_off.offset := i^.rm_addr;
  data_sel_off.selector := Self.sregs[i^.seg].selector.value;

  math_emulate2(addr_modes, i^.modrm, i^.b1, data_address,
                data_sel_off, entry_sel_off);
  if not ReLoop then
  begin
    {$ifdef LOG_FPU}
    Readed := FileRead(fp_fpu,ValueTestSwd,  sizeof(I387.soft.swd));
    Readed := FileRead(fp_fpu,ValueTestTwd,  sizeof(I387.soft.Twd));
    Readed := FileRead(fp_fpu,ValueTestFTop, sizeof(I387.soft.Ftop));
    Readed := FileRead(fp_fpu,ValueFpuReg,   sizeof(ValueFpuReg));
    if Readed > 0 then
    begin
      if (ValueFpuReg.sigl<>st(0)^.sigl) or
         (ValueFpuReg.sigh<>st(0)^.sigh) or
         (ValueFpuReg.Exp<>st(0)^.exp) then
      begin
(*      {$ifdef LOG_FPU}
      WriteLn(OutLogTxt,Format('FPUREG ST(0) ERROR AT : %d',[prog]));
      WriteLn(OutLogTxt,Format('PASCAL : sigl=%d ; sigh=%d ; exp=%d',[st(0)^.sigl,st(0)^.sigh,st(0)^.exp]));
      WriteLn(OutLogTxt,Format('C      : sigl=%d ; sigh=%d ; exp=%d',[ValueFpuReg.sigl,ValueFpuReg.sigh,ValueFpuReg.exp]));
      WriteLn(OutLogTxt,'-------------------------------------------------');
{$else}
Vuoto;
{$endif}*)
      end;
      if (I387.soft.swd <> ValueTestSwd) or (I387.soft.twd <> ValueTesttwd) or (I387.soft.ftop <> ValueTestftop) then
      begin
         WriteLn(OutLogTxt,Format('FPU ERROR: prog=%d;PSwd=%x;PTwd=%x;PFtop=%x;CSwd=%x;CTwd=%x;CFtop=%x;',
          [prog,I387.soft.swd,I387.soft.twd,I387.soft.ftop,ValueTestSwd,ValueTestTwd,ValueTestftop]));
           WriteLn(OutLogTxt,'-------------------------------------------------');
      end;
    end
    else
    begin
      {$ifdef LOG_FPU}
      if not ShowAgain then
        ShowAgain := True;
  {$endif}
    end;
  {$endif}
  end;

end;

//static double sigh_scale_factor := pow(2.0, -31.0);
//static double sigl_scale_factor := pow(2.0, -63.0);

{procedure BX_CPU_C::fpu_print_regs()
begin
  Bit32u reg;
  reg := i387.soft.cwd;
  fprintf(stderr, 'cwd            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.swd;
  fprintf(stderr, 'swd            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.twd;
  fprintf(stderr, 'twd            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.fip;
  fprintf(stderr, 'fip            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.fcs;
  fprintf(stderr, 'fcs            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.foo;
  fprintf(stderr, 'foo            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  reg := i387.soft.fos;
  fprintf(stderr, 'fos            $%-8x\t%d\n', (unsigned) reg, (int) reg);
  // print stack too
  for (int i:=0; i<8; i++) then begin
    FPU_REG *fpr := @st(i);
    double f1 := pow(2.0, (($7fff@fpr^.exp) - EXTENDED_Ebias));
    if (fpr^.exp  and SIGN_Negative) f1 := -f1;
    double f2 := ((double)fpr^.sigh * sigh_scale_factor);
    double f3 := ((double)fpr^.sigl * sigl_scale_factor);
    double f := f1*(f2+f3);
    fprintf(stderr, 'st%d            %.10f (raw $%04x%08x%08x)\n', i, f, $ffff@fpr^.exp, fpr^.sigh, fpr^.sigl);
  end;
end;}

procedure TCPU.fpu_init;
begin
  finit;
end;

function TCPU.DisAssemble(aPhysicalAddress, aEIP, aCS: Int64; 
  out aDisasmStr, aOpcodeStr: string; out aOpcodeLength: integer): bool;

  function GetERXNameByCode(aRegCode: Byte): string;
  begin
    case aRegCode of
      REG_EAX_32BIT: Result := 'EAX';
      REG_ECX_32BIT: Result := 'ECX';
      REG_EDX_32BIT: Result := 'EDX';
      REG_EBX_32BIT: Result := 'EBX';
      REG_ESP_32BIT: Result := 'ESP';
      REG_EBP_32BIT: Result := 'EBP';
      REG_ESI_32BIT: Result := 'ESI';
      REG_EDI_32BIT: Result := 'EDI';
    else
      Result := '??'
    end;
  end;

  function GetRXNameByCode(aRegCode: Byte): string;
  begin
    case aRegCode of
      REG_AX_16BIT: Result := 'AX';
      REG_CX_16BIT: Result := 'CX';
      REG_DX_16BIT: Result := 'DX';
      REG_BX_16BIT: Result := 'BX';
      REG_SP_16BIT: Result := 'SP';
      REG_BP_16BIT: Result := 'BP';
      REG_SI_16BIT: Result := 'SI';
      REG_DI_16BIT: Result := 'DI';
    else
      Result := '??'
    end;
  end;

  function GetRLNameByCode(aRegCode: Byte): string;
  begin
    case aRegCode of
      REG_AL_8BIT: Result := 'AL';
      REG_CL_8BIT: Result := 'CL';
      REG_DL_8BIT: Result := 'DL';
      REG_BL_8BIT: Result := 'BL';
    else
      Result := '??'
    end;
  end;

  function GetRHNameByCode(aRegCode: Byte): string;
  begin
    case aRegCode of
      REG_AH_8BIT: Result := 'AH';
      REG_CH_8BIT: Result := 'CH';
      REG_DH_8BIT: Result := 'DH';
      REG_BH_8BIT: Result := 'BH';
    else
      Result := '??'
    end;
  end;

  function GetRLHNameByCode(aRegCode: Byte): string;
  begin
    if aRegCode < 4 then
      Result := GetRLNameByCode(aRegCode)
    else
      Result := GetRHNameByCode(aRegCode - 4);
  end;

  function GetSegRegNameByCode(aRegCode: Byte): string;
  begin
    case aRegCode of
      SREG_ES: Result := 'ES';
      SREG_CS: Result := 'CS';
      SREG_SS: Result := 'SS';
      SREG_DS: Result := 'DS';
      SREG_FS: Result := 'FS';
      SREG_GS: Result := 'GS';
    else
      Result := '??'
    end;
  end;

var
  _fetchPtr: Pointer;
  i: TInstruction_tag;
  _i: integer;
  remain: word;
  _bufferstr: string;
begin
  Result := 0;
  _fetchPtr := @sysmemory.vector[aPhysicalAddress];
  remain := 16;
  if Self.FetchDecode(_fetchPtr, i, remain, Self.Is32) = 1 then
  begin
    //aDisasmStr := i.name;
    aOpcodeLength := i.ilen;
    Inc(aEIP, aOpcodeLength);
    
    // опкод
    aOpcodeStr := '';
    for _i := 0 to aOpcodeLength - 1 do
      aOpcodeStr := aOpcodeStr + IntToHex(sysmemory.vector[aPhysicalAddress + _i], 2);

    case i.b1 of
      // обыкновенные команды
      $6..$7, $E, $F, $16..$17, $1E..$1F, $26..$27, $2E..$2F, $36..$37, $3E..$3F,
      $60..$61, $64..$67, $90, $C3, $C9, $98, $99, $9B, $9E..$9F, $CB, $CC, $CE,
      $CF, $D4..$D7, $F0, $F1, $F4, $F5, $F8..$FD, $104..$109, $1A2, $1AA, $1AE, $1B8, $1B9,
      $1C2..$1C6, $1D0..$1FF:
        aDisasmStr := i.name;
      // обыкновенные команды, с жестео зафиксированным регистром (убираем символ "_")
      $1C8..$1CF:
      begin
        aDisasmStr := StringReplace(i.name, '_', ' ', [rfReplaceAll]);
      end;
      // команды с одним регистром rX
      $40..$5F:
      begin
        aDisasmStr := StringReplace(i.name, '_RX', ' ' + GetRXNameByCode(i.b1 and $07), [rfReplaceAll]);
      end;
      // команды с двумя регистрами rX и AX
      $91..$97:
      begin
        aDisasmStr := StringReplace(i.name, '_RXAX', ' ' + GetRXNameByCode(i.b1 and $07), [rfReplaceAll]) + ', AX';
      end;
      // команды с RL/RH/RX и данными
      $B0..$BF:
      begin
        // RL
        if i.b1 in [$B0..$B3] then
          aDisasmStr := StringReplace(i.name, '_RLIb', ' ' + GetRLNameByCode(i.b1 and $03), [rfReplaceAll]) + ', ' + IntToHex(i.Ib, 2)
        else
        // RH
        if i.b1 in [$B4..$B7] then
          aDisasmStr := StringReplace(i.name, '_RHIb', ' ' + GetRHNameByCode(i.b1 and $03), [rfReplaceAll]) + ', ' + IntToHex(i.Ib, 2)
        else
        // RX
          aDisasmStr := StringReplace(i.name, '_RXIw', ' ' + GetRXNameByCode(i.b1 and $07), [rfReplaceAll]) + ', ' + IntToHex(i.Iw, 4);
      end;
      // прерывания с одним значением
      $CD, $2CD:
      begin
        aDisasmStr := StringReplace(i.name, '_Ib', ' ' + IntToHex(i.Ib, 2), [rfReplaceAll]);
      end;
      //[$9A].Name := 'CALL16_Ap';
      $9A:
      begin
        aDisasmStr := StringReplace(i.name, '16_Ap', ' ' + IntToHex(i.Iw, 4), [rfReplaceAll]);
      end;
      // безусловные джампы
      $E9..$EB, $2E9..$2EB:
      begin
        if i.b1 in [$E9, $EB] then
          aDisasmStr := StringReplace(i.name, '_Jw', ' ' + IntToHex((aEIP + Bit32s(i.Id)) and $0000ffff, 4), [rfReplaceAll])
        else
        if (i.b1 = $02E9) or (i.b1 = $02EB) then
          aDisasmStr := StringReplace(i.name, '_Jd', ' ' + IntToHex(Bit32s(i.Id), 4), [rfReplaceAll])
        else
          aDisasmStr := StringReplace(i.name, '_Ap', ' ' + IntToHex(i.Id, 2 + 2 * i.os_32), [rfReplaceAll]);
      end;
      // ввод/вывод?
      $E6, $E7, $EE, $EF:
      begin
        if i.b1 = $EE then
          aDisasmStr := StringReplace(i.name, '_DXAL', ' DX, AL', [rfReplaceAll])
        else
        if i.b1 = $E6 then
          aDisasmStr := StringReplace(i.name, '_IbAL', ' ' + IntToHex(i.Ib, 2) + ', AL', [rfReplaceAll])
        else
        if i.b1 = $E7 then
          aDisasmStr := StringReplace(i.name, '_IbeAX', ' ' + IntToHex(i.Ib, 2 + 2 * i.os_32) + ifthen(i.os_32 = 0, ', AX', ', EAX'), [rfReplaceAll])
        else
        if i.b1 = $EF then
          aDisasmStr := StringReplace(i.name, '_DXeAX', ' DX, ' + ifthen(i.os_32 = 0, 'AX', 'EAX'), [rfReplaceAll])
      end;
      // условные джампы
      $70..$7F, $180..$18F:
      begin
        // JCC_Jw, JCC_Jd
        case (i.b1 and $0f) of
          $00: (* JO *)  aDisasmStr := 'JO '  ;
          $01: (* JNO *) aDisasmStr := 'JNO ' ;
          $02: (* JB *)  aDisasmStr := 'JB '  ;
          $03: (* JNB *) aDisasmStr := 'JNB ' ;
          $04: (* JZ *)  aDisasmStr := 'JZ '  ;
          $05: (* JNZ *) aDisasmStr := 'JNZ ' ;
          $06: (* JBE *) aDisasmStr := 'JBE ' ;
          $07: (* JNBE *)aDisasmStr := 'JNBE ';
          $08: (* JS *)  aDisasmStr := 'JS '  ;
          $09: (* JNS *) aDisasmStr := 'JNS ' ;
          $0A: (* JP *)  aDisasmStr := 'JP '  ;
          $0B: (* JNP *) aDisasmStr := 'JNP ' ;
          $0C: (* JL *)  aDisasmStr := 'JL '  ;
          $0D: (* JNL *) aDisasmStr := 'JNL ' ;
          $0E: (* JLE *) aDisasmStr := 'JLE ' ;
          $0F: (* JNLE *)aDisasmStr := 'JNLE ';
        end;

        if (i.b1 in [$70..$7F]) or ((i.b1 >= $180) and (i.b1 <= $18F)) then
          aDisasmStr := aDisasmStr + IntToHex((aEIP + Bit32s(i.Id)) and $0000ffff, 4);

        if ((i.b1 >= $270) and (i.b1 <= $27F)) or ((i.b1 >= $380) and (i.b1 <= $38F)) then
          aDisasmStr := aDisasmStr + IntToHex(aEIP + Bit32s(i.Id), 8);
      end;
      $38:
      begin
        if  i.mod_ = $C0 then
          _bufferstr := '[' + GetRLHNameByCode(i.rm) + ']'
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.os_32 = 0, 4, 8)) + ']';

        // часть после запятой
        _bufferstr := _bufferstr + ', ' + GetRLHNameByCode(i.nnn);

        aDisasmStr := StringReplace(i.name, '_EbGb', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $39:
      begin
        if i.mod_ = $c0 then
          _bufferstr := '[' + GetRXNameByCode(i.rm) + ']'
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.os_32 = 0, 4, 8)) + ']';

        _bufferstr := _bufferstr + ', ' + GetRXNameByCode(i.nnn);

        aDisasmStr := StringReplace(i.name, '_EwGw', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $3A:
      begin
        if i.mod_ = $c0 then
          _bufferstr := '[' + GetRLHNameByCode(i.rm) + ']'
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.os_32 = 0, 4, 8)) + ']';

        _bufferstr := GetRLHNameByCode(i.nnn) + ', ' + _bufferstr;

        aDisasmStr := StringReplace(i.name, '_GbEb', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $3B:
      begin
        if i.mod_ = $c0 then
          _bufferstr := '[' + GetRXNameByCode(i.rm) + ']'
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.os_32 = 0, 4, 8)) + ']';

        _bufferstr := GetRXNameByCode(i.nnn) + ', ' + _bufferstr;

        aDisasmStr := StringReplace(i.name, '_GwEw', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $3C:
      begin
        _bufferstr := 'AL, ' + IntToHex(i.Ib, 2);
        aDisasmStr := StringReplace(i.name, '_ALIb', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $3D:
      begin
        _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, '_AXIw', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $E4:
      begin
        aDisasmStr := StringReplace(i.name, '_ALIb', ' AL, ' + IntToHex(i.Ib, 2), [rfReplaceAll]);
      end;
      $E5:
      begin
        if i.os_32 = 1 then
          aDisasmStr := StringReplace(i.name, '_eAXIb', ' EAX, ' + IntToHex(i.Ib, 2), [rfReplaceAll])
        else
          aDisasmStr := StringReplace(i.name, '_eAXIb', ' AX, ' + IntToHex(i.Ib, 2), [rfReplaceAll])
      end;
      $0..$5:
      begin
        if i.b1 in [$0, $1] then
        begin
          if i.b1 = $0 then
          begin
            if i.mod_ = $c0 then
              _bufferstr := GetRLHNameByCode(i.rm)
            else
              _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 2) + ']';

            _bufferstr := _bufferstr + ', ' + GetRLHNameByCode(i.nnn);

            aDisasmStr := StringReplace(i.name, '_EbGb', ' ' + _bufferstr, [rfReplaceAll]);
          end else
          begin
            if i.mod_ = $c0 then
              _bufferstr := GetRXNameByCode(i.rm)
            else
              _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

            _bufferstr := _bufferstr + ', ' + GetRXNameByCode(i.nnn);

            aDisasmStr := StringReplace(i.name, '_EwGw', ' ' + _bufferstr, [rfReplaceAll]);
          end;
        end;

        if i.b1 in [$2, $3] then
        begin
          if i.b1 = $2 then
          begin
            _bufferstr := GetRLHNameByCode(i.nnn);
            if i.mod_ = $c0 then
              _bufferstr := _bufferstr + ', ' + GetRLHNameByCode(i.rm)
            else
              _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

            aDisasmStr := StringReplace(i.name, '_GbEb', ' ' + _bufferstr, [rfReplaceAll]);
          end else
          begin
            _bufferstr := GetRXNameByCode(i.nnn);
            if i.mod_ = $c0 then
              _bufferstr := _bufferstr + ', ' + GetRXNameByCode(i.rm)
            else
              _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

            aDisasmStr := StringReplace(i.name, '_GwEw', ' ' + _bufferstr, [rfReplaceAll]);
          end;
        end;

        if i.b1 in [$4, $5] then
        begin
          _bufferstr := IfThen(i.b1 = $4, GetRLNameByCode(REG_AL_8BIT), GetRXNameByCode(REG_AX_16BIT));
          _bufferstr := _bufferstr + ', ' + ifthen(i.b1 = $4, IntToHex(i.Ib, 2), IntToHex(i.Iw, 4));

          aDisasmStr := StringReplace(i.name, ifthen(i.b1 = $4, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
        end;
      end;
      $88, $89:
      begin
        if i.mod_ = $c0 then
        begin
          if i.b1 = $88 then
            _bufferstr := GetRLHNameByCode(i.rm)
          else
            _bufferstr := GetRXNameByCode(i.rm)
        end else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $88, 2, 4)) + ']';

        if i.b1 = $88 then
          _bufferstr := _bufferstr + ', ' + GetRLHNameByCode(i.nnn)
        else
          _bufferstr := _bufferstr + ', ' + GetRXNameByCode(i.nnn);

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $88, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $8A, $8B:
      begin
        if i.b1 = $8a then
          _bufferstr := GetRLHNameByCode(i.nnn)
        else
          _bufferstr := GetRXNameByCode(i.nnn);

        if i.mod_ = $c0 then
        begin
          if i.b1 = $8a then
            _bufferstr := GetRLHNameByCode(i.rm)
          else
            _bufferstr := GetRXNameByCode(i.rm);
        end else
          _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $88, 2, 4)) + ']';

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $8A, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $8C:
      begin
        if i.mod_ = $c0 then
          _bufferstr := ifthen(i.os_32 <> 0, GetERXNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr :=  GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

        _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.nnn);

        aDisasmStr := StringReplace(i.name, '_EwSw', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $8D:
      begin
        aDisasmStr := StringReplace(i.name, '_GwM', ' ' + GetRXNameByCode(i.nnn) +
          ', ' + IntToHex(i.rm_addr, 4), [rfReplaceAll]);
      end;
      $8E:
      begin
        _bufferstr := GetSegRegNameByCode(i.nnn) + ', ';
        if i.mod_ = $C0 then
          _bufferstr := _bufferstr + GetRXNameByCode(i.rm)
        else
          _bufferstr := _bufferstr + GetSegRegNameByCode(i.seg) + ':[' +
            IntToHex(i.rm_addr, 4) + ']';
        aDisasmStr := StringReplace(i.name, '_SwEw', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $30, $31:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $30, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $30, 2, 4)) + ']';

        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $30, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $30, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $32, $33:
      begin
        _bufferstr := IfThen(i.b1 = $32, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn)) + ', ';

        if i.mod_ = $C0 then
          _bufferstr := _bufferstr + IfThen(i.b1 = $32, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := _bufferstr + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $32, 2, 4)) + ']';

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $32, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $34, $35:
      begin
        if i.b1 = $34 then
          _bufferstr := 'AL, ' + IntToHex(i.Ib, 2)
        else
          _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $34, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $EC:
      begin
        aDisasmStr := StringReplace(i.name, '_ALDX', ' AL, DX', [rfReplaceAll]);
      end;
      $ED:
      begin
        aDisasmStr := StringReplace(i.name, '_eAXDX', ' ' + IfThen(i.os_32 = 0, 'AX', 'EAX') + ', DX', [rfReplaceAll]);
      end;
      $E8:
      begin
        aDisasmStr := StringReplace(i.name, '_Aw', ' ' + IntToHex((aEIP + Bit32s(i.Id)) and $0000ffff, 4), [rfReplaceAll]);
      end;
      // команды возврата
      $C2, $CA:
      begin
        aDisasmStr := 'RET';
      end;
      $68, $6A:
      begin
        aDisasmStr := StringReplace(i.name, '_Iw', ' ' + IntToHex(i.Iw, 4), [rfReplaceAll]);
      end;
      $8, $9:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $8, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $8, 2, 4)) + ']';

        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $8, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $8, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $A, $B:
      begin
        _bufferstr := IfThen(i.b1 = $A, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));

        if i.mod_ = $C0 then
          _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $A, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $A, 2, 4)) + ']';

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $A, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $C, $D:
      begin
        _bufferstr := IfThen(i.b1 = $C, 'AL', 'AX') + ', ';
        _bufferstr := _bufferstr + IntToHex(IfThen(i.b1 = $C, i.Ib, i.Iw), IfThen(i.b1 = $C, 2, 4));

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $C, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $A0, $A1:
      begin
        _bufferstr := IfThen(i.b1 = $A0, 'AL', 'AX') + ', ';
        if Boolean((i.seg and SEG_REG_NULL) = 0) then
          _bufferstr := _bufferstr + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.Id, IfThen(i.b1 = $A, 2, 4)) + ']'
        else
          _bufferstr := _bufferstr + 'DS:[' + IntToHex(i.Id, IfThen(i.b1 = $A, 2, 4)) + ']';

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $A0, '_ALOb', '_AXOw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $A2, $A3:
      begin
        if Boolean((i.seg and SEG_REG_NULL) = 0) then
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.Id, IfThen(i.b1 = $A, 2, 4)) + ']'
        else
          _bufferstr := 'DS:[' + IntToHex(i.Id, IfThen(i.b1 = $A, 2, 4)) + ']';

        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $A2, 'AL', 'AX');

        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $A2, '_ObAL', '_OwAX'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $C6, $C7:
      begin
        if i.mod_ = $C0 then
          _bufferstr := ifthen(i.b1 = $C6, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $C6, IntToHex(i.Ib, 2), IntToHex(i.Iw, 4));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $C6, '_EbIb', '_EwIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $E0..$E2:
      begin
        if (i.os_32 = 0) then
          _bufferstr := IntToHex(aEIP + Bit32s(i.Id) and $0000ffff, IfThen(i.os_32 = 0, 4, 8))
        else
          _bufferstr := IntToHex(aEIP + Bit32s(i.Id), IfThen(i.os_32 = 0, 4, 8));

        aDisasmStr := StringReplace(i.name, '_Jb', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $C8:
      begin
        // надо разобраться >>> [$C8].Name := 'ENTER_IwIb';
        aDisasmStr := StringReplace(i.name, '_IwIb', '', [rfReplaceAll]);
      end;
      $62:
      begin
        _bufferstr := IfThen(i.os_32 = 1, GetERXNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        _bufferstr := _bufferstr + ', ' + GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.os_32 = 1, 8, 4)) + ']';
        aDisasmStr := StringReplace(i.name, '_GvMa', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $63:
      begin
        // разобраться >>>>> [$63].Name := 'ARPL_EwGw';
        aDisasmStr := StringReplace(i.name, '_EwGw', '', [rfReplaceAll]);
      end;
      $24, $25:
      begin
        if i.b1 = $24 then
          _bufferstr := 'AL, ' + IntToHex(i.Ib, 2)
        else
          _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $24, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $20, $21:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $20, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $20, 2, 4)) + ']';
        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $20, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $20, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $22, $23:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $22, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $22, 2, 4)) + ']';
        _bufferstr := IfThen(i.b1 = $22, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn)) + ', ' + _bufferstr;
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $22, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $14, $15:
      begin
        if i.b1 = $44 then
          _bufferstr := 'AL, ' + IntToHex(i.Ib, 2)
        else
          _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $14, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $1C, $1D:
      begin
        if i.b1 = $1C then
          _bufferstr := 'AL, ' + IntToHex(i.Ib, 2)
        else
          _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $1C, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $A8, $A9:
      begin
        if i.b1 = $A8 then
          _bufferstr := 'AL, ' + IntToHex(i.Ib, 2)
        else
          _bufferstr := 'AX, ' + IntToHex(i.Iw, 4);
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $A8, '_ALIb', '_AXIw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $10, $11:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $10, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $10, 2, 4)) + ']';
        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $10, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $10, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $12, $13:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $12, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $12, 2, 4)) + ']';
        _bufferstr := IfThen(i.b1 = $12, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn)) + ', ' + _bufferstr;
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $12, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $1A, $1B:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $1A, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $1A, 2, 4)) + ']';
        _bufferstr := IfThen(i.b1 = $1A, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn)) + ', ' + _bufferstr;
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $12, '_GbEb', '_GwEw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $18, $19:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $18, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $18, 2, 4)) + ']';
        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $18, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $18, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $8F:
      begin
        if i.mod_ = $C0 then
          _bufferstr := GetRLHNameByCode(i.rm)
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, 4) + ']';

        aDisasmStr := StringReplace(i.name, '_Ew', ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $9C:
      begin
        aDisasmStr := 'PUSHFD';
      end;
      $9D:
      begin
        aDisasmStr := 'POPFD';
      end;
//      $F2, $F3:
//      begin
//        aDisasmStr := 'REP';
//      end;
      $86, $87:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $86, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $86, 2, 4)) + ']';
        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $86, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $86, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
      $84, $85:
      begin
        if i.mod_ = $C0 then
          _bufferstr := IfThen(i.b1 = $84, GetRLHNameByCode(i.rm), GetRXNameByCode(i.rm))
        else
          _bufferstr := GetSegRegNameByCode(i.seg) + ':[' + IntToHex(i.rm_addr, IfThen(i.b1 = $84, 2, 4)) + ']';
        _bufferstr := _bufferstr + ', ' + IfThen(i.b1 = $84, GetRLHNameByCode(i.nnn), GetRXNameByCode(i.nnn));
        aDisasmStr := StringReplace(i.name, IfThen(i.b1 = $84, '_EbGb', '_EwGw'), ' ' + _bufferstr, [rfReplaceAll]);
      end;
{
       [$28].Name := 'SUB_EbGb';
       [$29].Name := 'SUB_EwGw';
       [$2A].Name := 'SUB_GbEb';
       [$2B].Name := 'SUB_GwEw';

       [$2C].Name := 'SUB_ALIb';
       [$2D].Name := 'SUB_AXIw';
}
    end;
{
   [$69].Name := 'IMUL_GwEwIw';

   [$6B].Name := 'IMUL_GwEwIw';
   [$6C].Name := 'INSB_YbDX';
   [$6D].Name := 'INSW_YvDX';
   [$6E].Name := 'OUTSB_DXXb';
   [$6F].Name := 'OUTSW_DXXv';

   [$A4].Name := 'MOVSB_XbYb';
   [$A5].Name := 'MOVSW_XvYv';
   [$A6].Name := 'CMPSB_XbYb';
   [$A7].Name := 'CMPSW_XvYv';

   [$AA].Name := 'STOSB_YbAL';
   [$AB].Name := 'STOSW_YveAX';
   [$AC].Name := 'LODSB_ALXb';
   [$AD].Name := 'LODSW_eAXXv';
   [$AE].Name := 'SCASB_ALXb';
   [$AF].Name := 'SCASW_eAXXv';

   [$C4].Name := 'LES_GvMp';
   [$C5].Name := 'LDS_GvMp';

   [$E3].Name := 'JCXZ_Jb';
   /////////
   [$102].Name := 'LAR_GvEw';
   [$103].Name := 'LSL_GvEw';

   [$120].Name := 'MOV_RdCd';
   [$121].Name := 'MOV_RdDd';
   [$122].Name := 'MOV_CdRd';
   [$123].Name := 'MOV_DdRd';
   [$124].Name := 'MOV_RdTd';
   [$125].Name := 'BxError';
   [$126].Name := 'MOV_TdRd';
   [$140].Name := 'CMOV_GwEw';
   [$141].Name := 'CMOV_GwEw';
   [$142].Name := 'CMOV_GwEw';
   [$143].Name := 'CMOV_GwEw';
   [$144].Name := 'CMOV_GwEw';
   [$145].Name := 'CMOV_GwEw';
   [$146].Name := 'CMOV_GwEw';
   [$147].Name := 'CMOV_GwEw';
   [$148].Name := 'CMOV_GwEw';
   [$149].Name := 'CMOV_GwEw';
   [$14A].Name := 'CMOV_GwEw';
   [$14B].Name := 'CMOV_GwEw';
   [$14C].Name := 'CMOV_GwEw';
   [$14D].Name := 'CMOV_GwEw';
   [$14E].Name := 'CMOV_GwEw';
   [$14F].Name := 'CMOV_GwEw';

   [$190].Name := 'SETO_Eb';
   [$191].Name := 'SETNO_Eb';
   [$192].Name := 'SETB_Eb';
   [$193].Name := 'SETNB_Eb';
   [$194].Name := 'SETZ_Eb';
   [$195].Name := 'SETNZ_Eb';
   [$196].Name := 'SETBE_Eb';
   [$197].Name := 'SETNBE_Eb';
   [$198].Name := 'SETS_Eb';
   [$199].Name := 'SETNS_Eb';
   [$19A].Name := 'SETP_Eb';
   [$19B].Name := 'SETNP_Eb';
   [$19C].Name := 'SETL_Eb';
   [$19D].Name := 'SETNL_Eb';
   [$19E].Name := 'SETLE_Eb';
   [$19F].Name := 'SETNLE_Eb';
   [$1A0].Name := 'PUSH_FS';
   [$1A1].Name := 'POP_FS';

   [$1A3].Name := 'BT_EvGv';
   [$1A4].Name := 'SHLD_EwGw';
   [$1A5].Name := 'SHLD_EwGw';
   [$1A6].Name := 'CMPXCHG_XBTS';
   [$1A7].Name := 'CMPXCHG_IBTS';
   [$1A8].Name := 'PUSH_GS';
   [$1A9].Name := 'POP_GS';

   [$1AB].Name := 'BTS_EvGv';
   [$1AC].Name := 'SHRD_EwGw';
   [$1AD].Name := 'SHRD_EwGw';

   [$1AF].Name := 'IMUL_GwEw';
   [$1B0].Name := 'CMPXCHG_EbGb';
   [$1B1].Name := 'CMPXCHG_EwGw';
   [$1B2].Name := 'LSS_GvMp';
   [$1B3].Name := 'BTR_EvGv';
   [$1B4].Name := 'LFS_GvMp';
   [$1B5].Name := 'LGS_GvMp';
   [$1B6].Name := 'MOVZX_GwEb';
   [$1B7].Name := 'MOVZX_GwEw';

   [$1BB].Name := 'BTC_EvGv';
   [$1BC].Name := 'BSF_GvEv';
   [$1BD].Name := 'BSR_GvEv';
   [$1BE].Name := 'MOVSX_GwEb';
   [$1BF].Name := 'MOVSX_GwEw';
   [$1C0].Name := 'XADD_EbGb';
   [$1C1].Name := 'XADD_EwGw';

   [$200].Name := 'ADD_EbGb';
   [$201].Name := 'ADD_EdGd';
   [$202].Name := 'ADD_GbEb';
   [$203].Name := 'ADD_GdEd';
   [$204].Name := 'ADD_ALIb';
   [$205].Name := 'ADD_EAXId';
   [$206].Name := 'PUSH_ES';
   [$207].Name := 'POP_ES';
   [$208].Name := 'OR_EbGb';
   [$209].Name := 'OR_EdGd';
   [$20A].Name := 'OR_GbEb';
   [$20B].Name := 'OR_GdEd';
   [$20C].Name := 'OR_ALIb';
   [$20D].Name := 'OR_EAXId';
   [$20E].Name := 'PUSH_CS';
   [$20F].Name := 'BxError'; // 2-byte escape
   [$210].Name := 'ADC_EbGb';
   [$211].Name := 'ADC_EdGd';
   [$212].Name := 'ADC_GbEb';
   [$213].Name := 'ADC_GdEd';
   [$214].Name := 'ADC_ALIb';
   [$215].Name := 'ADC_EAXId';
   [$216].Name := 'PUSH_SS';
   [$217].Name := 'POP_SS';
   [$218].Name := 'SBB_EbGb';
   [$219].Name := 'SBB_EdGd';
   [$21A].Name := 'SBB_GbEb';
   [$21B].Name := 'SBB_GdEd';
   [$21C].Name := 'SBB_ALIb';
   [$21D].Name := 'SBB_EAXId';
   [$21E].Name := 'PUSH_DS';
   [$21F].Name := 'POP_DS';
   [$220].Name := 'AND_EbGb';
   [$221].Name := 'AND_EdGd';
   [$222].Name := 'AND_GbEb';
   [$223].Name := 'AND_GdEd';
   [$224].Name := 'AND_ALIb';
   [$225].Name := 'AND_EAXId';
   [$226].Name := 'BxError'; // ES:
   [$227].Name := 'DAA';
   [$228].Name := 'SUB_EbGb';
   [$229].Name := 'SUB_EdGd';
   [$22A].Name := 'SUB_GbEb';
   [$22B].Name := 'SUB_GdEd';
   [$22C].Name := 'SUB_ALIb';
   [$22D].Name := 'SUB_EAXId';
   [$22E].Name := 'BxError'; // CS:
   [$22F].Name := 'DAS';
   [$230].Name := 'XOR_EbGb';
   [$231].Name := 'XOR_EdGd';
   [$232].Name := 'XOR_GbEb';
   [$233].Name := 'XOR_GdEd';
   [$234].Name := 'XOR_ALIb';
   [$235].Name := 'XOR_EAXId';
   [$236].Name := 'BxError'; // SS:
   [$237].Name := 'AAA';
   [$238].Name := 'CMP_EbGb';
   [$239].Name := 'CMP_EdGd';
   [$23A].Name := 'CMP_GbEb';
   [$23B].Name := 'CMP_GdEd';
   [$23C].Name := 'CMP_ALIb';
   [$23D].Name := 'CMP_EAXId';
   [$23E].Name := 'BxError'; // DS:
   [$23F].Name := 'AAS';
   [$240].Name := 'INC_ERX';
   [$241].Name := 'INC_ERX';
   [$242].Name := 'INC_ERX';
   [$243].Name := 'INC_ERX';
   [$244].Name := 'INC_ERX';
   [$245].Name := 'INC_ERX';
   [$246].Name := 'INC_ERX';
   [$247].Name := 'INC_ERX';
   [$248].Name := 'DEC_ERX';
   [$249].Name := 'DEC_ERX';
   [$24A].Name := 'DEC_ERX';
   [$24B].Name := 'DEC_ERX';
   [$24C].Name := 'DEC_ERX';
   [$24D].Name := 'DEC_ERX';
   [$24E].Name := 'DEC_ERX';
   [$24F].Name := 'DEC_ERX';
   [$250].Name := 'PUSH_ERX';
   [$251].Name := 'PUSH_ERX';
   [$252].Name := 'PUSH_ERX';
   [$253].Name := 'PUSH_ERX';
   [$254].Name := 'PUSH_ERX';
   [$255].Name := 'PUSH_ERX';
   [$256].Name := 'PUSH_ERX';
   [$257].Name := 'PUSH_ERX';
   [$258].Name := 'POP_ERX';
   [$259].Name := 'POP_ERX';
   [$25A].Name := 'POP_ERX';
   [$25B].Name := 'POP_ERX';
   [$25C].Name := 'POP_ERX';
   [$25D].Name := 'POP_ERX';
   [$25E].Name := 'POP_ERX';
   [$25F].Name := 'POP_ERX';
   [$260].Name := 'PUSHAD32';
   [$261].Name := 'POPAD32';
   [$262].Name := 'BOUND_GvMa';
   [$263].Name := 'ARPL_EwGw';
   [$264].Name := 'BxError'; // FS:
   [$265].Name := 'BxError'; // GS:
   [$266].Name := 'BxError'; // OS:
   [$267].Name := 'BxError'; // AS:
   [$268].Name := 'PUSH_Id';
   [$269].Name := 'IMUL_GdEdId';
   [$26A].Name := 'PUSH_Id';
   [$26B].Name := 'IMUL_GdEdId';
   [$26C].Name := 'INSB_YbDX';
   [$26D].Name := 'INSW_YvDX';
   [$26E].Name := 'OUTSB_DXXb';
   [$26F].Name := 'OUTSW_DXXv';

   [$284].Name := 'TEST_EbGb';
   [$285].Name := 'TEST_EdGd';
   [$286].Name := 'XCHG_EbGb';
   [$287].Name := 'XCHG_EdGd';
   [$288].Name := 'MOV_EbGb';
   [$289].Name := 'MOV_EdGd';
   [$28A].Name := 'MOV_GbEb';
   [$28B].Name := 'MOV_GdEd';
   [$28C].Name := 'MOV_EwSw';
   [$28D].Name := 'LEA_GdM';
   [$28E].Name := 'MOV_SwEw';
   [$28F].Name := 'POP_Ed';
   [$290].Name := 'NOP';
   [$291].Name := 'XCHG_ERXEAX';
   [$292].Name := 'XCHG_ERXEAX';
   [$293].Name := 'XCHG_ERXEAX';
   [$294].Name := 'XCHG_ERXEAX';
   [$295].Name := 'XCHG_ERXEAX';
   [$296].Name := 'XCHG_ERXEAX';
   [$297].Name := 'XCHG_ERXEAX';
   [$298].Name := 'CWDE';
   [$299].Name := 'CDQ';
   [$29A].Name := 'CALL32_Ap';
   [$29B].Name := 'FWAIT';
   [$29C].Name := 'PUSHF_Fv';
   [$29D].Name := 'POPF_Fv';
   [$29E].Name := 'SAHF';
   [$29F].Name := 'LAHF';
   [$2A0].Name := 'MOV_ALOb';
   [$2A1].Name := 'MOV_EAXOd';
   [$2A2].Name := 'MOV_ObAL';
   [$2A3].Name := 'MOV_OdEAX';
   [$2A4].Name := 'MOVSB_XbYb';
   [$2A5].Name := 'MOVSW_XvYv';
   [$2A6].Name := 'CMPSB_XbYb';
   [$2A7].Name := 'CMPSW_XvYv';
   [$2A8].Name := 'TEST_ALIb';
   [$2A9].Name := 'TEST_EAXId';
   [$2AA].Name := 'STOSB_YbAL';
   [$2AB].Name := 'STOSW_YveAX';
   [$2AC].Name := 'LODSB_ALXb';
   [$2AD].Name := 'LODSW_eAXXv';
   [$2AE].Name := 'SCASB_ALXb';
   [$2AF].Name := 'SCASW_eAXXv';
   [$2B0].Name := 'MOV_RLIb';
   [$2B1].Name := 'MOV_RLIb';
   [$2B2].Name := 'MOV_RLIb';
   [$2B3].Name := 'MOV_RLIb';
   [$2B4].Name := 'MOV_RHIb';
   [$2B5].Name := 'MOV_RHIb';
   [$2B6].Name := 'MOV_RHIb';
   [$2B7].Name := 'MOV_RHIb';
   [$2B8].Name := 'MOV_ERXId';
   [$2B9].Name := 'MOV_ERXId';
   [$2BA].Name := 'MOV_ERXId';
   [$2BB].Name := 'MOV_ERXId';
   [$2BC].Name := 'MOV_ERXId';
   [$2BD].Name := 'MOV_ERXId';
   [$2BE].Name := 'MOV_ERXId';
   [$2BF].Name := 'MOV_ERXId';
   [$2C2].Name := 'RETnear32_Iw';
   [$2C3].Name := 'RETnear32';
   [$2C4].Name := 'LES_GvMp';
   [$2C5].Name := 'LDS_GvMp';
   [$2C6].Name := 'MOV_EbIb';
   [$2C7].Name := 'MOV_EdId';
   [$2C8].Name := 'ENTER_IwIb';
   [$2C9].Name := 'LEAVE';
   [$2CA].Name := 'RETfar32_Iw';
   [$2CB].Name := 'RETfar32';
   [$2CC].Name := 'INT3';

   [$2CE].Name := 'INTO';
   [$2CF].Name := 'IRET32';
   [$2D4].Name := 'AAM';
   [$2D5].Name := 'AAD';
   [$2D6].Name := 'SALC';
   [$2D7].Name := 'XLAT';
   [$2E0].Name := 'LOOPNE_Jb';
   [$2E1].Name := 'LOOPE_Jb';
   [$2E2].Name := 'LOOP_Jb';
   [$2E3].Name := 'JCXZ_Jb';
   [$2E4].Name := 'IN_ALIb';
   [$2E5].Name := 'IN_eAXIb';
   [$2E6].Name := 'OUT_IbAL';
   [$2E7].Name := 'OUT_IbeAX';
   [$2E8].Name := 'CALL_Ad';

   [$2EC].Name := 'IN_ALDX';
   [$2ED].Name := 'IN_eAXDX';
   [$2EE].Name := 'OUT_DXAL';
   [$2EF].Name := 'OUT_DXeAX';
   [$2F1].Name := 'INT1';
   [$2F4].Name := 'HLT';
   [$2F5].Name := 'CMC';
   [$2F8].Name := 'CLC';
   [$2F9].Name := 'STC';
   [$2FA].Name := 'CLI';
   [$2FB].Name := 'STI';
   [$2FC].Name := 'CLD';
   [$2FD].Name := 'STD';
   [$302].Name := 'LAR_GvEw';
   [$303].Name := 'LSL_GvEw';
   [$304].Name := 'BxError';
   [$305].Name := 'LOADALL';
   [$306].Name := 'CLTS';
   [$307].Name := 'BxError';
   [$308].Name := 'INVD';
   [$309].Name := 'WBINVD';
   [$320].Name := 'MOV_RdCd';
   [$321].Name := 'MOV_RdDd';
   [$322].Name := 'MOV_CdRd';
   [$323].Name := 'MOV_DdRd';
   [$324].Name := 'MOV_RdTd';
   [$326].Name := 'MOV_TdRd';
   [$330].Name := 'WRMSR';
   [$331].Name := 'RDTSC';
   [$332].Name := 'RDMSR';
   [$340].Name := 'CMOV_GdEd';
   [$341].Name := 'CMOV_GdEd';
   [$342].Name := 'CMOV_GdEd';
   [$343].Name := 'CMOV_GdEd';
   [$344].Name := 'CMOV_GdEd';
   [$345].Name := 'CMOV_GdEd';
   [$346].Name := 'CMOV_GdEd';
   [$347].Name := 'CMOV_GdEd';
   [$348].Name := 'CMOV_GdEd';
   [$349].Name := 'CMOV_GdEd';
   [$34A].Name := 'CMOV_GdEd';
   [$34B].Name := 'CMOV_GdEd';
   [$34C].Name := 'CMOV_GdEd';
   [$34D].Name := 'CMOV_GdEd';
   [$34E].Name := 'CMOV_GdEd';
   [$34F].Name := 'CMOV_GdEd';

   [$390].Name := 'SETO_Eb';
   [$391].Name := 'SETNO_Eb';
   [$392].Name := 'SETB_Eb';
   [$393].Name := 'SETNB_Eb';
   [$394].Name := 'SETZ_Eb';
   [$395].Name := 'SETNZ_Eb';
   [$396].Name := 'SETBE_Eb';
   [$397].Name := 'SETNBE_Eb';
   [$398].Name := 'SETS_Eb';
   [$399].Name := 'SETNS_Eb';
   [$39A].Name := 'SETP_Eb';
   [$39B].Name := 'SETNP_Eb';
   [$39C].Name := 'SETL_Eb';
   [$39D].Name := 'SETNL_Eb';
   [$39E].Name := 'SETLE_Eb';
   [$39F].Name := 'SETNLE_Eb';
   [$3A0].Name := 'PUSH_FS';
   [$3A1].Name := 'POP_FS';
   [$3A2].Name := 'CPUID';
   [$3A3].Name := 'BT_EvGv';
   [$3A4].Name := 'SHLD_EdGd';
   [$3A5].Name := 'SHLD_EdGd';
   [$3A6].Name := 'CMPXCHG_XBTS';
   [$3A7].Name := 'CMPXCHG_IBTS';
   [$3A8].Name := 'PUSH_GS';
   [$3A9].Name := 'POP_GS';
   [$3AA].Name := 'RSM';
   [$3AB].Name := 'BTS_EvGv';
   [$3AC].Name := 'SHRD_EdGd';
   [$3AD].Name := 'SHRD_EdGd';
   [$3AF].Name := 'IMUL_GdEd';
   [$3B0].Name := 'CMPXCHG_EbGb';
   [$3B1].Name := 'CMPXCHG_EdGd';
   [$3B2].Name := 'LSS_GvMp';
   [$3B3].Name := 'BTR_EvGv';
   [$3B4].Name := 'LFS_GvMp';
   [$3B5].Name := 'LGS_GvMp';
   [$3B6].Name := 'MOVZX_GdEb';
   [$3B7].Name := 'MOVZX_GdEw';
   [$3BB].Name := 'BTC_EvGv';
   [$3BC].Name := 'BSF_GvEv';
   [$3BD].Name := 'BSR_GvEv';
   [$3BE].Name := 'MOVSX_GdEb';
   [$3BF].Name := 'MOVSX_GdEw';
   [$3C0].Name := 'XADD_EbGb';
   [$3C1].Name := 'XADD_EdGd';
}
    aDisasmStr := StringReplace(aDisasmStr, '_', ' ', [rfReplaceAll]);
    Exit(1);
  end;
end;

function TCPU.StackHeadAddr: int64;
begin
  Result := (SS + ESP) and $0000ffff;
end;

function TCPU.InstrPointer: int64;
begin
  Result := (CS + EIP){ and $0000ffff};
end;

procedure TCPU.SetZF(const aValue: Bool);
begin
  eflags.zf := aValue;
end;

function TCPU.GetZF: bool;
begin
  Result := eflags.zf;
end;

procedure TCPU.SetCF(const aValue: Bool);
begin
  eflags.cf := aValue;
end;

function TCPU.GetCF: bool;
begin
  Result := eflags.cf;
end;

procedure TCPU.SetPF(const aValue: Bool);
begin
  eflags.pf_byte := aValue;
end;

function TCPU.GetPF: bool;
begin
  Result := eflags.pf_byte;
end;

procedure TCPU.SetAF(const aValue: Bool);
begin
  eflags.af := aValue;
end;

function TCPU.GetAF: bool;
begin
  Result := eflags.af;
end;

procedure TCPU.SetSF(const aValue: Bool);
begin
  eflags.sf := aValue;
end;

function TCPU.GetSF: bool;
begin
  Result := eflags.sf;
end;

procedure TCPU.SetTF(const aValue: Bool);
begin
  eflags.tf := aValue;
end;

function TCPU.GetTF: bool;
begin
  Result := eflags.tf;
end;

procedure TCPU.SetDF(const aValue: Bool);
begin
  eflags.df := aValue;
end;

function TCPU.GetDF: bool;
begin
  Result := eflags.df;
end;

procedure TCPU.SetOF(const aValue: Bool);
begin
  eflags.of_ := aValue;
end;

function TCPU.GetOF: bool;
begin
  Result := eflags.of_;
end;

procedure TCPU.EvCPUCommand(aInstrPtr: pointer);
begin
  if Assigned(FOnCPUCommand) then
    FOnCPUCommand(aInstrPtr);
end;

function TCPU.Is32: bool;
begin
  Result := self.sregs[SEG_REG_CS].cache.segment.d_b;
end;

procedure TCPU.SetActive(const aValue: boolean);
begin
  FActive := aValue;
  // something else;
end;

end.

