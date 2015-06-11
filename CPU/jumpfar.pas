 { ****************************************************************************** }
 { Mozaa 0.95 - Virtual PC emulator - developed by Massimiliano Boidi 2003 - 2004 }
 { ****************************************************************************** }

{ For any question write to maxboidi@libero.it }

(* **** *)
unit jumpfar;

interface

uses SysUtils;

type
  jmp_buf = record
    _ebx,
    _esi,
    _edi,
    _ebp,
    _esp,
    _eip: longint;
  end;

  pjmp_buf = ^jmp_buf;

function setjmp (var rec: jmp_buf): longint; stdcall;
procedure longjmp (const rec: jmp_buf; return_value: longint); stdcall;

implementation

{$STACKFRAMES ON}
function setjmp (var rec: jmp_buf): longint; assembler;
      { [ebp+12]: [ebp+8]:@rec, [ebp+4]:eip', [ebp+0]:ebp' }
asm // free: eax, ecx, edx
  { push ebp; mov ebp,esp }
  MOV  EDX, rec
  MOV  [EDX].jmp_buf._ebx, EBX  { ebx }
  MOV  [EDX].jmp_buf._esi, ESI  { esi }
  MOV  [EDX].jmp_buf._edi, EDI  { edi }
  MOV  EAX, [EBP]               { ebp (caller stack frame) }
  MOV  [EDX].jmp_buf._ebp, EAX
  LEA  EAX, [EBP+12] { esp [12]: [8]:@rec, [4]:eip, [0]:ebp }
  MOV  [EDX].jmp_buf._esp, EAX
  MOV  EAX, [EBP+4]
  MOV  [EDX].jmp_buf._eip, EAX
  XOR  EAX, EAX
  { leave }
  { ret  4 }
end;

procedure longjmp (const rec: jmp_buf; return_value: longint); assembler;
  { [ebp+12]: return_value [ebp+8]:@rec, [ebp+4]:eip', [ebp+0]:ebp' }
asm
  { push ebp, mov ebp,esp }
  MOV  EDX,rec
  MOV  ECX,return_value
  MOV  EBX,[EDX].jmp_buf._ebx  { ebx }
  MOV  ESI,[EDX].jmp_buf._esi  { esi }
  MOV  EDI,[EDX].jmp_buf._edi  { edi }
  MOV  EBP,[EDX].jmp_buf._ebp  { ebp }
  MOV  ESP,[EDX].jmp_buf._esp  { esp }
  MOV  EAX,[EDX].jmp_buf._eip  { eip }
  PUSH EAX
  MOV  EAX,ECX
  RET  0
end;
{$STACKFRAMES OFF}

end.
