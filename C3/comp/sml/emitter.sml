(*
**  emitter.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure EmitFuns = struct
  type t = {
    emitMeta: int*int*int -> unit,
    emitInstr: int -> unit,
    emitInstr_: int*int -> unit
  }
end
  
signature EMITTER = sig
  type outstream

  val withStream : outstream -> EmitFuns.t
  
  val emitMeta: outstream -> int*int*int -> unit
  val emitInstr: outstream -> int -> unit
  val emitInstr_: outstream -> int*int -> unit
end

(*
 * VM File format Emitter
 *)
structure VMFEmitter : EMITTER = struct
  type outstream = BinIO.outstream

  fun emitInt (out, i) = let
    open Word
    infix andb >>
 
    val w = Word.fromInt i
    val wl = [ 
         (w >> 0w24) andb 0wxFF, 
         (w >> 0w16) andb 0wxFF, 
         (w >> 0w08) andb 0wxFF,
         (w        ) andb 0wxFF ]
    val w8l = map (fn w => Word8.fromInt (Word.toInt w)) wl
  in
    BinIO.output (out, Word8Vector.fromList w8l)
  end
 
  fun emitMeta out (magic, major, minor) = (emitInt (out, magic); emitInt (out, major); emitInt (out, minor))
  fun emitInstr out i = emitInt (out, i)
  fun emitInstr_ out (i, arg) = (emitInt (out, i); emitInt (out, arg))

  fun withStream stream = {emitMeta = emitMeta stream,
        emitInstr = emitInstr stream, emitInstr_ = emitInstr_ stream} 
end

(*
 * Asm Emitter
 *)
structure AsmEmitter : EMITTER = struct
  type outstream = TextIO.outstream

  fun emitMeta out (magic, major, minor) = TextIO.output (out, 
         "//\r\n" ^ "// vm-asm " ^ Int.toString(major) ^ "." ^ Int.toString(minor) ^ "\r\n//\r\n")
  fun emitInstr out i = TextIO.output (out, 
         (Instr.instrName i) ^ "\r\n")
  fun emitInstr_ out (i, arg) = TextIO.output (out,
         (Instr.instrName i) ^ " " ^ Int.toString(arg) ^ "\r\n")
  
  fun withStream stream = {emitMeta = emitMeta stream,
        emitInstr = emitInstr stream,
        emitInstr_ = emitInstr_ stream} 
end

 