unit dma;

interface

uses iodev,config,cpu,service,SysUtils;

const
  DMA_MODE_DEMAND  = 0;
  DMA_MODE_SINGLE  = 1;
  DMA_MODE_BLOCK   = 2;
  DMA_MODE_CASCADE = 3;

  // index to find channel from register number (only [0],[1],[2],[6] used)
  channelindex:array[0..6] of Bit8u = (2, 3, 1, 0, 0, 0, 0);

type
  bx_dma_c = class
  public
    devices:pbx_devices_c;

    s:array[0..2] of record
      mask:array[0..4] of bool;
      flip_flop:bool;
      status_reg:Bit8u;
      command_reg:Bit8u;
      request_reg:Bit8u;
      temporary_reg:Bit8u;
      chan:array[0..4] of record
        mode:record
          mode_type:Bit8u;
          address_decrement:Bit8u;
          autoinit_enable:Bit8u;
          transfer_type:Bit8u;
          end;
        base_address:Bit16u;
        current_address:Bit16u;
        base_count:Bit16u;
        current_count:Bit16u;
        page_reg:Bit8u;
        end; // DMA channels 0..3
    end;  // state information DMA-1 / DMA-2
    constructor Create;
    destructor Destroy;

    procedure init(d:pbx_devices_c);
    procedure DRQ(channel:unsigned; val:Bool);
    procedure raise_HLDA(pc_sys:PPC_SYSTEM);

  private

    function read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:Pointer; address:Bit32u;value:Bit32u ; io_len:unsigned);
  {$if BX_USE_DMA_SMF=0}
    Bit32u   read( Bit32u   address, unsigned io_len);
    void     write(Bit32u   address, Bit32u   value, unsigned io_len);
  {$ifend}
    procedure control_HRQ(ma_sl:bool);
  end;

var
  bx_dma:bx_dma_c;  
implementation

constructor bx_dma_c.Create;
begin
	//put('DMA');
	//settype(DMALOG);
end;

destructor bx_dma_c.Destroy;
begin
//	BX_DEBUG(('Exit.'));
  inherited;
end;

procedure bx_dma_c.init(d:pbx_devices_c);
var
  c:unsigned;
  i:unsigned;
begin
//  BX_DEBUG(('Init $Id: dma.cc,v 1.17.2.1 2002/06/19 15:38:20 cbothamy Exp $'));

  Self.devices := d;

  (* 8237 DMA controller *)

  // 0000..000F
	I:=$0000;
  while i <= $000F do
  begin
    Self.devices^.register_io_read_handler(Self, read_handler, i, 'DMA controller');
    Self.devices^.register_io_write_handler(Self, write_handler, i, 'DMA controller');
		Inc(i);
  end;

  // 00081..008F
	I:=$0081;
  while i <= $008F do
  begin
    Self.devices^.register_io_read_handler(Self, read_handler, i, 'DMA controller');
    Self.devices^.register_io_write_handler(Self, write_handler, i, 'DMA controller');
		Inc(i);
  end;

  // 000C0..00DE
  I:=$000C0;
  while I <= $00DE do
  begin
    Self.devices^.register_io_read_handler(Self, read_handler, i, 'DMA controller');
    Self.devices^.register_io_write_handler(Self, write_handler, i, 'DMA controller');
    Inc(I,2);
  end;

	i:=0;
  while i < 2 do begin
    Self.s[i].mask[0] := 1; // channel 0 masked
    Self.s[i].mask[1] := 1; // channel 1 masked
    Self.s[i].mask[2] := 1; // channel 2 masked
    Self.s[i].mask[3] := 1; // channel 3 masked

    Self.s[i].flip_flop := 0; (* cleared *)
    Self.s[i].status_reg := 0; // no requests, no terminal counts reached
		c:=0;
    while c < 4 do begin
      Self.s[i].chan[c].mode.mode_type := 0;         // demand mod_e
      Self.s[i].chan[c].mode.address_decrement := 0; // address increment
      Self.s[i].chan[c].mode.autoinit_enable := 0;   // autoinit disable
      Self.s[i].chan[c].mode.transfer_type := 0;     // verify
      Self.s[i].chan[c].base_address := 0;
      Self.s[i].chan[c].current_address := 0;
      Self.s[i].chan[c].base_count := 0;
      Self.s[i].chan[c].current_count := 0;
      Self.s[i].chan[c].page_reg := 0;
			inc(c);
      end;
		inc(i);
    end;
end;

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_dma_c.read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
var
  retval:Bit8u;
  channel:Bit8u;
  ma_sl:Bool;
begin
  if Boolean(io_len > 1) then
  begin
    LogError(Format('io read from address %08x, len:=%u',[address, io_len]));
  	exit($ff);
  end;

//  BX_DEBUG(Format('read addr:=%04x', [address]));

{$if BX_DMA_FLOPPY_IO < 1}
  (* if we're not supporting DMA/floppy IO just return a bogus value *)
  Exit($ff);
{$ifend}

  case address of
    $00, (* DMA-1 current address, channel 0 *)
    $02, (* DMA-1 current address, channel 1 *)
    $04, (* DMA-1 current address, channel 2 *)
    $06, (* DMA-1 current address, channel 3 *)
    $c0, (* DMA-2 current address, channel 0 *)
    $c4, (* DMA-2 current address, channel 1 *)
    $c8, (* DMA-2 current address, channel 2 *)
    $cc: (* DMA-2 current address, channel 3 *)
    begin
      ma_sl := Bool(address >= $c0);
      channel := (address shr (1 + ma_sl))  and $03;
      if Boolean(Self.s[ma_sl].flip_flop=0) then
      begin
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        Result:=(Self.s[ma_sl].chan[channel].current_address  and $ff);
        Exit;
      end else
      begin
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        Result:=(Self.s[ma_sl].chan[channel].current_address shr 8);
        Exit;
      end;
    end;
    $01, (* DMA-1 current count, channel 0 *)
    $03, (* DMA-1 current count, channel 1 *)
    $05, (* DMA-1 current count, channel 2 *)
    $07, (* DMA-1 current count, channel 3 *)
    $c2, (* DMA-2 current count, channel 0 *)
    $c6, (* DMA-2 current count, channel 1 *)
    $ca, (* DMA-2 current count, channel 2 *)
    $ce: (* DMA-2 current count, channel 3 *)
    begin
      ma_sl := Bool(address >= $c2);
      channel := (address shr (1 + ma_sl))  and $03;
      if Boolean(Self.s[ma_sl].flip_flop=0) then
      begin
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        Result:=(Self.s[ma_sl].chan[channel].current_count  and $ff);
        Exit;
      end else
      begin
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        Result:=(Self.s[ma_sl].chan[channel].current_count shr 8);
        exit;
      end;
    end;
    $08, // DMA-1 Status Register
    $d0: // DMA-2 Status Register
      // bit 7: 1 := channel 3 request
      // bit 6: 1 := channel 2 request
      // bit 5: 1 := channel 1 request
      // bit 4: 1 := channel 0 request
      // bit 3: 1 := channel 3 has reached terminal count
      // bit 2: 1 := channel 2 has reached terminal count
      // bit 1: 1 := channel 1 has reached terminal count
      // bit 0: 1 := channel 0 has reached terminal count
      // reading this register clears lower 4 bits (hold flags)
    begin
      ma_sl := Bool(address = $d0);
      retval := Self.s[ma_sl].status_reg;
      Self.s[ma_sl].status_reg := $f0;
      Exit(retval);
    end;
    $0d, // DMA-1: temporary register
    $da: // DMA-2: temporary register
    begin
      ma_sl := Bool(address = $da);
      LogError(Format('DMA-%d: read of temporary register',[ ma_sl+1]));
      // Note: write to $0D clears temporary register
      Result:=0; exit;
    end;
    $0081, // DMA-1 page register, channel 2
    $0082, // DMA-1 page register, channel 3
    $0083, // DMA-1 page register, channel 1
    $0087: // DMA-1 page register, channel 0
    begin
      channel := channelindex[address - $81];
      Result:=( Self.s[0].chan[channel].page_reg );
      exit;
    end;

    $0089, // DMA-2 page register, channel 2
    $008a, // DMA-2 page register, channel 3
    $008b, // DMA-2 page register, channel 1
    $008f: // DMA-2 page register, channel 0
    begin
      channel := channelindex[address - $89];
      Result:=( Self.s[1].chan[channel].page_reg );
      Exit;
    end;

    $0084,
    $0085,
    $0086,
    $0088,
    $008c,
    $008d,
    $008e:
    begin
//        BX_DEBUG(Format('read: extra page register $%04x unsupported', [address]));
      exit(0);
    end;

  else
    begin
      LogError(Format('read: unsupported address:=%04x',[address]));
      exit(0);
    end;
  end;
end;

  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_dma_c.write_handler(this_ptr:Pointer; address:Bit32u;value:Bit32u ; io_len:unsigned);
var
  set_mask_bit:Bit8u;
  channel:Bit8u;
  ma_sl:Bool;
begin
{$if BX_USE_DMA_SMF=0}
  bx_dma_c *class_ptr := (bx_dma_c *) this_ptr;

  class_ptr^.write(address, value, io_len);
end;


  (* 8237 DMA controller *)
procedure bx_dma_c.write(Bit32u address, Bit32u   value, unsigned io_len)
begin
{$else}
  //UNUSED(this_ptr);
{$ifend}  // !BX_USE_DMA_SMF

  if Boolean(io_len > 1) then
  begin
    if Boolean( (io_len = 2) and (address = $0b) ) then
    begin
{$if BX_USE_DMA_SMF=1}
      Self.write_handler(NULL, address,   value  and $ff, 1);
      Self.write_handler(NULL, address+1, value shr 8,   1);
{$else}
      Self.write(address,   value  and $ff, 1);
      Self.write(address+1, value shr 8,   1);
{$ifend}
      exit;
    end;

    LogError(Format('io write to address %08x, len:=%u',[address, io_len]));
  	exit;
  end;

//  BX_DEBUG(Format('write: address:=%04x value:=%02x',[address, value]));

{$if BX_DMA_FLOPPY_IO < 1}
  (* if we're not supporting DMA/floppy IO just return *)
  exit;
{$ifend}

  case address of
    $00,
    $02,
    $04,
    $06,
    $c0,
    $c4,
    $c8,
    $cc:
      begin
        ma_sl := Bool(address >= $c0);
        channel := (address shr (1 + ma_sl)) and $03;
//        BX_DEBUG(Format('  DMA-%d base and current address, channel %d',[ma_sl+1, channel]));
        if Boolean(Self.s[ma_sl].flip_flop=0) then begin (* 1st byte *)
          Self.s[ma_sl].chan[channel].base_address := value;
          Self.s[ma_sl].chan[channel].current_address := value;
          end
        else begin (* 2nd byte *)
          Self.s[ma_sl].chan[channel].base_address := Self.s[ma_sl].chan[channel].base_address or (value shl 8);
          Self.s[ma_sl].chan[channel].current_address := Self.s[ma_sl].chan[channel].current_address or (value shl 8);
//            BX_DEBUG(Format('    base := %04x', [Self.s[ma_sl].chan[channel].base_address]));
//            BX_DEBUG(Format('    curr := %04x', [Self.s[ma_sl].chan[channel].current_address]));
          end;
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        exit;
      end;

     $01,
     $03,
     $05,
     $07,
     $c2,
     $c6,
     $ca,
     $ce:
      begin
        ma_sl := Bool(address >= $c2);
        channel := (address shr (1 + ma_sl))  and $03;
//        BX_DEBUG(Format('  DMA-%d base and current count, channel %d', [ma_sl+1, channel]));
        if Boolean(Self.s[ma_sl].flip_flop=0) then begin (* 1st byte *)
          Self.s[ma_sl].chan[channel].base_count := value;
          Self.s[ma_sl].chan[channel].current_count := value;
          end
        else begin (* 2nd byte *)
          Self.s[ma_sl].chan[channel].base_count := Self.s[ma_sl].chan[channel].base_count or (value shl 8);
          Self.s[ma_sl].chan[channel].current_count := Self.s[ma_sl].chan[channel].current_count or (value shl 8);
//          BX_DEBUG(Format('    base := %04x', [Self.s[ma_sl].chan[channel].base_count]));
//          BX_DEBUG(Format('    curr := %04x', [Self.s[ma_sl].chan[channel].current_count]));
          end;
        Self.s[ma_sl].flip_flop := not Self.s[ma_sl].flip_flop;
        exit;
      end;

    $08, (* DMA-1: command register *)
    $d0: (* DMA-2: command register *)
      begin
        ma_sl := Bool(address = $d0);
        if Boolean(value <> $04) then
          LogError(Format('DMA: write to $%02x: value(%02xh) not 04h',[address, value]));
        Self.s[ma_sl].command_reg := value;
        exit;
      end;

    $09, // DMA-1: request register
    $d2: // DMA-2: request register
      begin
        ma_sl := Bool(address = $d2);
        channel := value  and $03;
        LogError(Format('DMA-%d: write to request register (%02x)',[ma_sl+1,value]));
        // note: write to $0d clears this register
        if Boolean(value  and $04) then begin
          // set request bit
          Self.s[ma_sl].status_reg := Self.s[ma_sl].status_reg or (1 shl (channel+4));
//          BX_DEBUG(Format('DMA-%d: set request bit for channel %u',[ma_sl+1, channel]));
          end
        else begin
          // clear request bit
          Self.s[ma_sl].status_reg := Self.s[ma_sl].status_reg and  not(1 shl (channel+4)); // !!! ~(1 shl (channel+4))
//          BX_DEBUG(Format('DMA-%d: cleared request bit for channel %u',[ma_sl+1, channel]));
          end;
        control_HRQ(ma_sl);
        exit;
      end;

    $0a,
    $d4:
      begin
        ma_sl := Bool(address = $d4);
        set_mask_bit := value  and $04;
        channel := value  and $03;
        Self.s[ma_sl].mask[channel] := Bool(set_mask_bit > 0);
//        BX_DEBUG(Format('DMA-%d: set_mask_bit:=%u, channel:=%u, mask now:=%02xh',[ma_sl+1,
//            set_mask_bit, channel, Self.s[ma_sl].mask[channel]]));
        control_HRQ(ma_sl);
        exit;
      end;

    $0b, (* DMA-1 mod_e register *)
    $d6: (* DMA-2 mod_e register *)
      begin
        ma_sl := Bool(address = $d6);
        channel := value and $03;
        Self.s[ma_sl].chan[channel].mode.mode_type := (value shr 6)  and $03;
        Self.s[ma_sl].chan[channel].mode.address_decrement := (value shr 5)  and $01;
        Self.s[ma_sl].chan[channel].mode.autoinit_enable := (value shr 4)  and $01;
        Self.s[ma_sl].chan[channel].mode.transfer_type := (value shr 2)  and $03;
//        BX_DEBUG(Format('DMA-%d: mod_e register[%u] := %02x', [ma_sl+1, channel, value]));
        exit;
      end;

    $0c, (* DMA-1 clear byte flip/flop *)
    $d8: (* DMA-2 clear byte flip/flop *)
      begin
        ma_sl := Bool(address = $d8);
//        BX_DEBUG(Format('DMA-%d: clear flip/flop',[ma_sl+1]));
        Self.s[ma_sl].flip_flop := 0;
        exit;
      end;

    $0d, // DMA-1: master disable
    $da: // DMA-2: master disable
      begin
        ma_sl := Bool(address = $da);
        (* ??? *)
//        BX_DEBUG(Format('DMA-%d: master disable',[ma_sl+1]));
        // writing any value to this port resets DMA controller 1 / 2
        // same action as a hardware reset
        // mask register is set (chan 0..3 disabled)
        // command, status, request, temporary, and byte flip-flop are all cleared
        Self.s[ma_sl].mask[0] := 1;
        Self.s[ma_sl].mask[1] := 1;
        Self.s[ma_sl].mask[2] := 1;
        Self.s[ma_sl].mask[3] := 1;
        Self.s[ma_sl].command_reg := 0;
        Self.s[ma_sl].status_reg := 0;
        Self.s[ma_sl].request_reg := 0;
        Self.s[ma_sl].temporary_reg := 0;
        Self.s[ma_sl].flip_flop := 0;
        exit;
      end;

    $0e, // DMA-1: clear mask register
    $dc: // DMA-2: clear mask register
      begin
        ma_sl := Bool(address = $dc);
//        BX_DEBUG(Format('DMA-%d: clear mask register',[ma_sl+1]));
        Self.s[ma_sl].mask[0] := 0;
        Self.s[ma_sl].mask[1] := 0;
        Self.s[ma_sl].mask[2] := 0;
        Self.s[ma_sl].mask[3] := 0;
        control_HRQ(ma_sl);
        exit;
      end;

    $0f, // DMA-1: write all mask bits
    $de: // DMA-2: write all mask bits
      begin
        ma_sl := Bool(address = $de);
//        BX_DEBUG(Format('DMA-%d: write all mask bits',[ma_sl+1]));
        Self.s[ma_sl].mask[0] := value  and $01; value := value shr 1;
        Self.s[ma_sl].mask[1] := value  and $01; value := value shr 1;
        Self.s[ma_sl].mask[2] := value  and $01; value := value shr 1;
        Self.s[ma_sl].mask[3] := value  and $01;
        control_HRQ(ma_sl);
        exit;
      end;

    $81, (* DMA-1 page register, channel 2 *)
    $82, (* DMA-1 page register, channel 3 *)
    $83, (* DMA-1 page register, channel 1 *)
    $87: (* DMA-1 page register, channel 0 *)
      (* address bits A16-A23 for DMA channel *)
      begin
        channel := channelindex[address - $81];
        Self.s[0].chan[channel].page_reg := value;
//        BX_DEBUG(Format('DMA-1: page register %d := %02x',[channel,value]));
        exit;
      end;

    $89, (* DMA-2 page register, channel 2 *)
    $8A, (* DMA-2 page register, channel 3 *)
    $8B, (* DMA-2 page register, channel 1 *)
    $8F: (* DMA-2 page register, channel 0 *)
      begin
        (* address bits A16-A23 for DMA channel *)
        channel := channelindex[address - $89];
        Self.s[1].chan[channel].page_reg := value;
//        BX_DEBUG(Format('DMA-2: page register %d := %02x',[channel + 4, value]));
        exit;
      end;

    $0084,
    $0085,
    $0086,
    $0088,
    $008C,
    $008D,
    $008E:
      begin
//        BX_DEBUG(Format('write: extra page register $%04x unsupported',[address]));
        exit;
      end;

    else
      LogError(Format('write ignored: %04xh := %02xh',[address, value]));
    end;
end;

procedure bx_dma_c.DRQ(channel:unsigned; val:Bool);
var
  dma_base, dma_roof:Bit32u;
  ma_sl:Bool;
begin

  if Boolean( ( channel <> 2 ) and ( channel <> 4 ) ) then
    LogPanic(Format('DRQ(): channel %d !:= 2 (floppy) or 4 (cascade)',[channel]));
  ma_sl := Bool(channel > 3);
  channel := channel and $03;
  if Boolean(val=0) then begin
    //BX_DEBUG(('bx_dma_c.DRQ(): val = 0'));
    // clear bit in status reg
    Self.s[ma_sl].status_reg := Self.s[ma_sl].status_reg and not(1 shl (channel+4)); //!!! ~(1 shl (channel+4))

    control_HRQ(ma_sl);
    exit;
  end;


  Self.s[ma_sl].status_reg := Self.s[ma_sl].status_reg or (1 shl (channel+4));

  if Boolean( (Self.s[ma_sl].chan[channel].mode.mode_type <> DMA_MODE_SINGLE) and
       (Self.s[ma_sl].chan[channel].mode.mode_type <> DMA_MODE_DEMAND) and
       (Self.s[ma_sl].chan[channel].mode.mode_type <> DMA_MODE_CASCADE) ) then
        LogPanic(Format('DRQ: mod_e_type(%02x) not handled',[Self.s[ma_sl].chan[channel].mode.mode_type]));

  dma_base := (Self.s[ma_sl].chan[channel].page_reg shl 16) or
             (Self.s[ma_sl].chan[channel].base_address shl ma_sl);
  if Boolean(Self.s[ma_sl].chan[channel].mode.address_decrement=0) then begin
    dma_roof := dma_base + (Self.s[ma_sl].chan[channel].base_count shl ma_sl);
  end else begin
    dma_roof := dma_base - (Self.s[ma_sl].chan[channel].base_count shl ma_sl);
  end;
  if Boolean( (dma_base  and ($7fff0000 shl ma_sl)) <> (dma_roof  and ($7fff0000 shl ma_sl)) ) then begin
    LogInfo(Format('dma_base := %08x', [dma_base]));
    LogInfo(Format('dma_base_count := %08x',[Self.s[ma_sl].chan[channel].base_count]));
    LogInfo(Format('dma_roof := %08x',[dma_roof]));
    LogPanic(Format('request outside %dk boundary',[64 shl ma_sl]));
  end;

  control_HRQ(ma_sl);
end;

procedure bx_dma_c.control_HRQ(ma_sl:Bool);
var
  channel:unsigned;
begin

  // deassert HRQ if no DRQ is pending
  if Boolean((Self.s[ma_sl].status_reg  and $f0) = 0) then begin
    if Boolean(ma_sl) then begin
      bx_pc_system.set_HRQ(0);
    end else begin
      bx_pc_system.set_DRQ(4, 0);
    end;
    exit;
  end;
  // find highest priority channel
  for channel:=0 to 4 do begin
    if Boolean(((Self.s[ma_sl].status_reg and (1 shl (channel+4)))<>0) and (Self.s[ma_sl].mask[channel]=0) ) then begin
      if Boolean(ma_sl) then begin
        // assert Hold ReQuest line to CPU
        bx_pc_system.set_HRQ(1);
      end else begin
        // send DRQ to cascade channel of the master
        bx_pc_system.set_DRQ(4, 1);
      end;
      break;
    end;
  end;
end;

procedure bx_dma_c.raise_HLDA(pc_sys:PPC_SYSTEM);
var
  channel:unsigned;
  phy_addr:Bit32u;
  count_expired:Bool;
  ma_sl:Bool;
begin
  count_expired:=0;
  ma_sl := 0;
  // find highest priority channel
  for channel:=0 to 4 do begin
    if Boolean( ((Self.s[1].status_reg  and (1 shl (channel+4)))<>0) and (Self.s[1].mask[channel]=0) ) then begin
      ma_sl := 1;
      break;
      end;
    end;
  if Boolean(channel = 0) then begin // master cascade channel
    bx_pc_system.set_DACK(channel + (ma_sl shl 2), 1);
    for channel:=0 to 4 do begin
      if Boolean( ((Self.s[0].status_reg and (1 shl (channel+4)))<>0) and (Self.s[0].mask[channel]=0) ) then begin
        ma_sl := 0;
        break;
        end;
      end;
    end;
  if Boolean(channel >= 4) then begin
    // wait till they're unmasked
    exit;
    end;

  //BX_DEBUG(('hlda: OK in response to DRQ(%u)', (unsigned) channel));
  phy_addr := (Self.s[ma_sl].chan[channel].page_reg shl 16) or
             (Self.s[ma_sl].chan[channel].current_address shl ma_sl);

  bx_pc_system.set_DACK(channel + (ma_sl shl 2), 1);
  // check for expiration of count, so we can signal TC and DACK(n)
  // at the same time.
  if Boolean(Self.s[ma_sl].chan[channel].mode.address_decrement=0) then
    Inc(Self.s[ma_sl].chan[channel].current_address)
  else
    dec(Self.s[ma_sl].chan[channel].current_address);
  dec(Self.s[ma_sl].chan[channel].current_count);
  if Boolean(Self.s[ma_sl].chan[channel].current_count = $ffff) then begin
    // count expired, done with transfer
    // assert TC, deassert HRQ  and DACK(n) lines
    Self.s[ma_sl].status_reg := Self.s[ma_sl].status_reg or (1 shl channel); // hold TC in status reg
    bx_pc_system.set_TC(1);
    count_expired := 1;
    if Boolean(Self.s[ma_sl].chan[channel].mode.autoinit_enable = 0) then begin
      // set mask bit if not in autoinit mod_e
      Self.s[ma_sl].mask[channel] := 1;
      end
  else begin
      // count expired, but in autoinit mod_e
      // reload count and base address
      Self.s[ma_sl].chan[channel].current_address :=
        Self.s[ma_sl].chan[channel].base_address;
      Self.s[ma_sl].chan[channel].current_count :=
        Self.s[ma_sl].chan[channel].base_count;
      end;
    end;

  if Boolean(Self.s[ma_sl].chan[channel].mode.transfer_type = 1) then begin // write
    // xfer from I/O to Memory
    if Boolean(ma_sl=0) then begin
      pc_sys^.dma_write8(phy_addr, channel, 0);
      end
  else begin
      pc_sys^.dma_write16(phy_addr, channel+4, 0);
      end;
    end
  else if Boolean(Self.s[ma_sl].chan[channel].mode.transfer_type = 2) then begin // read
    // xfer from Memory to I/O
    if Boolean(ma_sl=0) then begin
      pc_sys^.dma_read8(phy_addr, channel);
      end
  else begin
      pc_sys^.dma_read16(phy_addr, channel+4);
      end;
    end
  else if Boolean(Self.s[ma_sl].chan[channel].mode.transfer_type = 0) then begin
    // verify
    if Boolean(ma_sl=0) then begin
      pc_sys^.dma_write8(phy_addr, channel, 1);
      end
  else begin
      pc_sys^.dma_write16(phy_addr, channel+4, 1);
      end;
    end
  else begin
    LogPanic(('hlda: transfer_type 3 is undefined'));
    end;

  if Boolean(count_expired) then begin
    bx_pc_system.set_TC(0);            // clear TC, adapter card already notified
    bx_pc_system.set_HRQ(0);           // clear HRQ to CPU
    bx_pc_system.set_DACK(channel + (ma_sl shl 2), 0); // clear DACK to adapter card
    if Boolean(ma_sl=0) then begin
      bx_pc_system.set_DRQ(4, 0); // clear DRQ to cascade
      bx_pc_system.set_DACK(4, 0); // clear DACK to cascade
      end;
    end;
end;
end.
