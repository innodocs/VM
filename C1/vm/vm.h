//
//  VM.hpp
//  vm
//
//  Created by Ovidiu Podisor on 01/30/19.
//  Copyright © 2019 innodocs. All rights reserved.
//

#ifndef VM_h
#define VM_h

#include <iterator>

class VM
{
public:
  // code file markers
  static const int MAGIC         = 0x12345678;
  static const int MAJOR_VERSION = 0x00000001;
  static const int MINOR_VERSION = 0x00000001;
  
  // instruction set
  static const int HALT     = 0xFFFF;

  static const int NOP      = 0x00;

  static const int IGETGLBL = 0x10;
  static const int ISETGLBL = 0x11;

  static const int IPUSHC   = 0x20;
  static const int IPOP     = 0x21;
  static const int ISWAP    = 0x22;
  static const int IDUP     = 0x23;

  static const int IADD     = 0x40;
  static const int ISUB     = 0x41;
  static const int IMULT    = 0x42;
  static const int IDIV     = 0x43;
  static const int IMOD     = 0x44;

  static const int INEG     = 0x60;

  static const int IPRINT   = 0x100;
  
  
public: // construction
  VM(const char* codeFileName, unsigned long stackSize, bool isVerbose, bool dumpCode);
  ~VM();

  void init(const char* codeFileName, unsigned long stackSize);
  int  run();
  void dumpCode();

public: // VM errors
  struct VMError {};
  struct MemoryError           : public VMError {};
  struct InvalidCodeFileError  : public VMError {};
  struct InvalidInstrError     : public VMError {};
  struct StackOverflowError    : public VMError {};
  struct StackUnderflowError   : public VMError {};
  struct UndefinedGlobalError  : public VMError {};

protected: // registers
  union VMObj
  {
    void* ref;
    int   i;
  };
    
  int    *CB;   // base pointer to code buffer
  int    *CL;   // code buffer limit
  VMObj  *GB;   // base pointer to the globals area
  VMObj  *GL;   // globals area limit
  VMObj  *SB;   // base pointer to stack buffer
  VMObj  *SL;   // stack limit (end of stack buffer)

  int    *IP;   // instruction pointer
  VMObj  *TS;   // top of stack

protected: // instruction evaluators
  void  halt();

  void  getglobal();
  void  setglobal();

  void  pushc();
  void  pop();
  void  swap();
  void  dup();
  
  void  binop();
  void  unop();

  void  print();

protected:
  bool      isVerbose;
  bool      includeDump;
  
  uintmax_t fileSize;
  uintmax_t filePos;

  int  readInt(std::istream_iterator<uint8_t>& si);
  void loadCode(const char* codeFileName);
};

#endif /* VM_h */
