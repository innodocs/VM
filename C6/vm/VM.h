//
//  VM.hpp
//  vm
//
//  Created by Ovidiu Podisor on 03/30/19.
//  Copyright © 2019 innodocs. All rights reserved.
//

#ifndef VM_h
#define VM_h

#include <stdexcept>
#include <sstream>
#include <tuple>

#include "typestring.h" // support for templates parameterized by string literals
#define _vm_ts typestring_is
#include "tupleprint.h"


class VM
{
public:
  // code file markers
  static constexpr int MAGIC         = 0x12345678;
  static constexpr int MAJOR_VERSION = 0x01;
  static constexpr int MINOR_VERSION = 0x03;

  // pool sizes
  static constexpr int GLOBALS = 0xFF01;
  static constexpr int STRINGS = 0xFF02;

  // instruction set
  static constexpr int NOP  = 0x00;
  static constexpr int HALT = 0x01;

  // load/save globals
  static constexpr int ILOADG  = 0x10;
  static constexpr int ISTOREG = 0x11;

  // push constants
  static constexpr int IPUSHC  = 0x16;
  static constexpr int SPUSHC  = 0x17;

  // stack ops
  static constexpr int IPOP    = 0x20;
  static constexpr int ISWAP   = 0x21;
  static constexpr int IDUP    = 0x22;

  // branch ops
  static constexpr int GOTO      = 0x30;
  static constexpr int IF_ICMPEQ = 0x31;
  static constexpr int IF_ICMPNE = 0x32;
  static constexpr int IF_ICMPLT = 0x33;
  static constexpr int IF_ICMPLE = 0x34;
  static constexpr int IF_ICMPGT = 0x35;
  static constexpr int IF_ICMPGE = 0x36;
  static constexpr int IF_IEQ    = 0x37;
  static constexpr int IF_INE    = 0x38;

  // arithmetic ops
  static constexpr int IADD   = 0x40;
  static constexpr int ISUB   = 0x41;
  static constexpr int IMULT  = 0x42;
  static constexpr int IDIV   = 0x43;
  static constexpr int IMOD   = 0x44;
  static constexpr int IPOW   = 0x45;

  // logical ops
  static constexpr int IAND   = 0x4A;
  static constexpr int IOR    = 0x4B;

  // comparison ops
  static constexpr int ICMP   = 0x50;
  static constexpr int ICMPEQ = 0x51;
  static constexpr int ICMPNE = 0x52;
  static constexpr int ICMPLT = 0x53;
  static constexpr int ICMPLE = 0x54;
  static constexpr int ICMPGT = 0x55;
  static constexpr int ICMPGE = 0x56;

  // unary ops
  static constexpr int INEG   = 0x60;
  static constexpr int INOT   = 0x61;

  // built-in functions
  static constexpr int IPRINT = 0x100;
  static constexpr int SPRINT = 0x101;
  
public: // construction
  struct Options {
    int     stackSize;
    bool    warn        : 1;
    bool    disassemble : 1;
    bool    dumpMem     : 1;
  };
  
  VM(const char* codeFileName, Options options) noexcept;
  ~VM();
  void  run();
  static const char* version() noexcept;
  
protected:
  const char* codeFileName;
  Options     options;

  void init(const char* codeFileName, unsigned long stackSize);
  void loadCode(const char* codeFileName);
  int  readByte(istream_iterator<uint8_t>& si);
  int  readInt(istream_iterator<uint8_t>& si);
  uintmax_t fileSize;
  uintmax_t filePos;

  
protected: // registers
  union VMObj
  {
    void* ref;
    int   i;
    char* s;
  };
    
  int*    CB;   // base pointer to code buffer
  int*    CL;   // code buffer limit
  VMObj*  GB;   // base pointer to the globals area
  VMObj*  GL;   // globals area limit
  char**  SPB; // string pool base
  char**  SPL; // string pool limit

  VMObj*  OB;   // base pointer to operand stack buffer
  VMObj*  OL;   // operand stack limit (end of operand stack buffer)

  int*    IP;   // instruction pointer
  VMObj*  OP;   // operand stack pointer (top of stack)

  
protected: // instruction execution and handlers
  void  execCode();

  void  halt();

  void  loadGlobal();
  void  storeGlobal();

  void  pushIntC();
  void  pushStrC();

  void  pop();
  void  swap();
  void  dup();
  
  void  branch();
  void  branchIf();

  void  binop();
  void  unop();

  void  print();
  
  
public: // VM errors
  struct VMError : public runtime_error {
    public: VMError() : runtime_error("VMError") {}
    protected: VMError(const char *tid) : runtime_error(tid) {}
  };
  template<typename TS, typename ...TArgs> struct VMErrorT : public VMError {
	tuple<const TArgs...> args;
    VMErrorT(const TArgs&... _args) : VMError(TS::data()), args(_args...) {}
    const char* what() const noexcept {
      static string s;
      ostringstream os; os << "(" << VMError::what() << "): " << hex << args;
      s = std::move(os.str()); return s.c_str();
    }
  };
  
  using InvalidCodeFileError    = VMErrorT<_vm_ts("InvalidCodeFileError"), const char*>;
  using BadStringPoolError      = VMErrorT<_vm_ts("BadStringPoolError"), int, int>;
  using UnsupportedVersionError = VMErrorT<_vm_ts("UnsupportedVersionError"), int, int>;
  using MemoryError             = VMErrorT<_vm_ts("MemoryError")>;
  using StackOverflowError      = VMErrorT<_vm_ts("StackOverflowError"), VMObj*, int>;
  using StackUnderflowError     = VMErrorT<_vm_ts("StackUnderflowError"), VMObj*, int>;
  using InvalidInstrError       = VMErrorT<_vm_ts("InvalidInstrError"), int>;
  using UndefinedGlobalError    = VMErrorT<_vm_ts("UndefinedGlobalError"), int>;
  using UndefinedStringError    = VMErrorT<_vm_ts("UndefinedStringError"), int>;

public: // memory dump, debugging
  void disassemble(ostream& os) const;
  void dumpMem(ostream& os) const;
protected:
  void dumpGlobals(ostream& os) const;
  void dumpStrings(ostream& os) const;
  void dumpRegisters(ostream& os) const;
  void dumpStack(ostream& os) const;
  void dumpCode(ostream& os, int *startIP = NULL) const;
};

#endif /* VM_h */
