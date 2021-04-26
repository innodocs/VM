(*
**  emitter.sml
**  vm-comp
**
**  Created by Ovidiu Podisor on 03/15/19.
**  Copyright © 2019-2021 innodocs. All rights reserved.
*)
  
signature EMITTER =
sig
  datatype label = Label of string * int
  
  type iostream
  type memstream

  val withIOStream: iostream  -> (unit -> unit) -> int
  val withStream  : memstream -> (unit -> unit) -> int
  
  val newMemStream: unit-> memstream
  val mergeStream : memstream -> unit
  
  val emitMeta   : int * int * int -> unit
  val emitInstr  : int -> unit
  val emitInstrI : int * int -> unit
  val emitInstrIS: int * int * string -> unit
  val emitLabel  : label -> unit
  val emitJump   : int * label -> unit
  val emitStrings: unit -> unit
end


functor EmitterFun(
          structure IosT:
          sig
            type e
            type s
            val emit: s -> e -> unit
          end):
  sig
    datatype label = Label of string * int
    type iostream
    type memstream
    
    val emit: IosT.e -> unit
    
    val newMemStream: unit -> memstream
    val mergeStream : memstream -> unit

    val withIOStream: iostream  -> (unit -> unit) -> int
    val withStream  : memstream -> (unit -> unit) -> int  
  end =
struct
  datatype label = Label of string * int
  
  type iostream = IosT.s
  type memstream = IosT.e list ref
  
  datatype stream = IOS of iostream
                  | MemS of memstream
                  
  exception InvStream of string

  val stream: stream ref = ref (MemS(ref []))
  val pos: int ref = ref 0
  
  fun emit i = let
    val _ = pos := !pos + 1
  in
    case !stream of
      IOS(s) => IosT.emit s i
    | MemS(s) => (s := i :: !s)
  end   
  
  fun newMemStream() = ref []
  
  fun mergeStream(ms) = (case !stream of
        IOS(s)  => List.app (IosT.emit s) (List.rev (!ms))
      | MemS(s) => (pos := !pos + (length (!ms)); s := (!ms) @ (!s)))
    
  fun withstream(s: stream, compileFn: (unit -> unit)): int =
  let
    val oldPos = !pos
    val oldStream = !stream
    val _ = pos := 0
    val _ = stream := s
     
    val streamPos = ref 0
  in
    (compileFn() before
       streamPos := !pos;
       pos := oldPos;
       stream := oldStream; !streamPos)
     handle e => (
       (* streamPos := !pos; *)
       pos := oldPos;
       stream := oldStream; raise e)
  end

  fun withIOStream stream compileFn = withstream(IOS(stream), compileFn)
  fun withStream   stream compileFn = withstream(MemS(stream), compileFn)
end


(**
 * VM File format Emitter
 *)
structure VMFEmitter : EMITTER = struct

  fun tow8v i =
  let
    open Word
    infix andb >>
    
    val w = Word.fromInt i
    val w8l = map (fn w => Word8.fromInt (Word.toInt w)) [ 
         (w >> 0w24) andb 0wxFF, 
         (w >> 0w16) andb 0wxFF, 
         (w >> 0w08) andb 0wxFF,
         (w        ) andb 0wxFF ]
  in
    Word8Vector.fromList w8l
  end 
  
  structure E = EmitterFun(structure IosT = struct
                  type e = int
                  type s = BinIO.outstream
                  fun emit s i = BinIO.output (s, tow8v i)
                end)
  open E
 
  fun emitMeta (magic, major, minor) = (emit magic; emit major; emit minor)
  fun emitInstr i = emit i
  fun emitInstrI (i, arg1) = (emit i; emit arg1)
  fun emitInstrIS (i, arg1, arg2) = (emit i; emit arg1)
  fun emitLabel _ = ()
  fun emitJump (i, Label(_, offset)) = (emit i; emit offset)
  fun emitStrings() = StringPool.emit emit
end (* structure VMFEmitter *)


(**
 * Asm Emitter
 *)
structure AsmEmitter : EMITTER = struct

  structure E = EmitterFun(structure IosT = struct
                  type e = string
                  type s = TextIO.outstream
                  fun emit out str = TextIO.output (out, str)
                end)
  open E

  fun emitMeta (magic, major, minor) = emit (
         "//\n" ^ "// vm-asm " ^ Int.toString(major) ^ "." ^ Int.toString(minor) ^ "\n//\n")
  fun emitInstr i = emit ((Instr.instrName i) ^ "\n")
  fun emitInstrI (i, arg1) = emit (
         (Instr.instrName i) ^ " " ^ Int.toString(arg1) ^ "\n")
  fun emitInstrIS (i, arg1, arg2) = emit (
         (Instr.instrName i) ^ " \"" ^ PrettyPrint.formatString(arg2)  ^ "\"\n")
  fun emitLabel (Label(name, _)) = emit (name ^ ":\n")
  fun emitJump (opcode, Label(name, _)) = emit (
         (Instr.instrName opcode) ^ " " ^ name ^ "\n")
  fun emitStrings() = ()
end (* structure AsmEmitter *)
