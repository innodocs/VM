//
//  VM.cpp
//  vm
//
//  Created by Ovidiu Podisor on 01/30/19.
//  Copyright © 2019 innodocs. All rights reserved.
//

#include <iostream>
#include <fstream>
#include <filesystem>
using namespace std;

#include "vm.h"


VM::VM(const char* _codeFileName, unsigned long _stackSize, bool _isVerbose, bool _includeDump)
{
  isVerbose = _isVerbose;
  includeDump = _includeDump;

  CB = 0; CL = 0;
  GB = 0; GL = 0;
  SB = 0; SL = 0;

  IP = 0;
  TS = 0;

  fileSize = 0;
  filePos = 0;
  init(_codeFileName, _stackSize);
}

VM::~VM()
{
  delete CB; CB = 0; CL = 0;
  delete GB; GB = 0; GL = 0;
  delete SB; SB = 0; SL = 0;

  IP = NULL;
  TS = NULL;
}

void VM::init(const char* codeFileName, unsigned long stackSize)
{
  // init code storage
  loadCode(codeFileName);

  // init stack storage
  SB = new VMObj[stackSize];
  if (SB == NULL)
    throw MemoryError();
  SL = SB + stackSize;

  // init registers
  IP = CB;
  TS = SB-1;
}

int VM::readInt(istream_iterator<uint8_t>& si)
{
  uint8_t b3 = *si++;
  uint8_t b2 = *si++;
  uint8_t b1 = *si++;
  uint8_t b0 = *si++;
  filePos += 4;

  return (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
}

void VM::loadCode(const char *codeFileName)
{
  error_code ec;
  fileSize = filesystem::file_size(filesystem::path(codeFileName), ec);
  if (fileSize == static_cast<std::uintmax_t>(-1))
	  throw new InvalidCodeFileError(/*codeFileName*/);
  if (isVerbose)
    cerr << "File size: " << fileSize << endl;

  ifstream cfs(codeFileName, ifstream::in|ifstream::binary);
  if (! cfs.is_open())
    throw new InvalidCodeFileError(/*codeFileName*/);
  noskipws(cfs); // why do you need to call this, when you opened in binary mode????
  istream_iterator<uint8_t> cfi(cfs);

  // check validity of code file: magic number
  int magic = readInt(cfi);
  if (magic != MAGIC)
    throw new InvalidCodeFileError(/*codeFileName*/);
  int majorVersion = readInt(cfi);
  if (majorVersion > MAJOR_VERSION)
    throw new InvalidCodeFileError(/*codeFileName*/);
  int minorVersion = readInt(cfi);
  if (minorVersion > MINOR_VERSION)
    throw new InvalidCodeFileError(/*codeFileName*/);
  // TODO: check validity of code file: checksum
  unsigned long checksum = 0;

  int globalsSize = readInt(cfi);
  if (globalsSize < 0)
	  globalsSize = 0;
  if (globalsSize > 0) {
	  GB = new VMObj[globalsSize];
	  if (GB == NULL)
		  throw MemoryError();
  }
  GL = GB + globalsSize;
  if (isVerbose)
    cerr << "Globals size: " << globalsSize << endl;

  uintmax_t codeSize = fileSize - filePos;
  if (isVerbose)
    cerr << "Code size: " << codeSize << endl;

  CB = new int[(codeSize+sizeof(int)-1)/sizeof(int) + 2];
  if (CB == NULL)
    throw MemoryError();
  CL = CB + (codeSize+sizeof(int)-1)/sizeof(int) + 2;

  int pos = 0;
  while (filePos + 4 <= fileSize)
  {
    int instr = readInt(cfi);
    CB[pos++] = instr;
  }

  // end buffer w/ halt instruction
  CB[pos++] = HALT;
  CB[pos++] = HALT;
}

// globals
void VM::getglobal()
{
  if (TS+1 >= SL)
    throw StackOverflowError();
  if (IP[1] < 0 || IP[1] >= GL-GB)
    throw UndefinedGlobalError();

  TS[1] = GB[ IP[1] ];
  TS++; IP++;
}

void VM::setglobal()
{
  if (TS < SB)
    throw new StackUnderflowError();
  if (IP[1] < 0 || IP[1] >= GL-GB)
    throw UndefinedGlobalError();

  GB[ IP[1] ] = TS[0];
  TS--; IP++;
}

// stack manipulation
inline void VM::pushc()
{
  if (TS+1 >= SL)
    throw StackOverflowError();

  TS[1].i = IP[1];
  TS++; IP++;
}

inline void VM::pop()
{
  if (TS < SB)
    throw StackUnderflowError();

  TS--;
}

inline void VM::swap()
{
  if (TS < SB)
    throw StackUnderflowError();

  VMObj temp = TS[-1];
  TS[-1] = TS[0];
  TS[0] = temp;
}

inline void VM::dup()
{
  if (TS+1 >= SL)
    throw StackOverflowError();

  TS[1] = TS[0];
  TS++;
}

// expression evaluation
inline void VM::binop()
{
  if (TS-1 < SB)
    throw StackUnderflowError();

  switch (*IP) {
  case IADD : TS[-1].i = TS[-1].i +  TS[0].i; break;
  case ISUB : TS[-1].i = TS[-1].i -  TS[0].i; break;
  case IMULT: TS[-1].i = TS[-1].i *  TS[0].i; break;
  case IDIV : TS[-1].i = TS[-1].i /  TS[0].i; break;
  case IMOD : TS[-1].i = TS[-1].i %  TS[0].i; break;
  }

  TS--;
}

inline void VM::unop()
{
  if (TS < SB)
    throw StackUnderflowError();

  switch (*IP) {
  case INEG : TS[0].i = -TS[0].i; break;
  }
}

// builtin functions
inline void VM::print()
{
  if (TS < SB)
    throw StackUnderflowError();
  int nrExps = TS[0].i;
  if (TS - nrExps < SB)
    throw StackUnderflowError();

  for (int sp = nrExps; sp > 1; --sp)
    cout << TS[-sp].i << ", ";
  cout << TS[-1].i << endl;

  TS -= nrExps + 1;
}

int VM::run()
{
  if (includeDump)
    dumpCode();

  for (IP = CB; IP < CL && *IP != HALT; IP++)
  {
    switch(*IP) {
    case NOP     :              break;
    case IGETGLBL: getglobal(); break;
    case ISETGLBL: setglobal(); break;
    case IPUSHC  : pushc();     break;
    case IPOP    : pop();       break;
    case ISWAP   : swap();      break;
    case IDUP    : dup();       break;
    case IPRINT  : print();     break;
    default:
      if      (IADD <= *IP && *IP <= IMOD)   binop();
      else if (INEG <= *IP && *IP <= INEG)   unop();
      else
    	  throw InvalidInstrError();
      break;
    }
  }

  if (TS >= SB) {
    cerr << "Warning: unused elements left on stack" << endl;
    for (; TS >= SB; --TS)
      cerr << hex << TS[0].i << endl;
  }

  return 0;
}

void VM::dumpCode()
{
  cerr << "Code dump:" << endl;

  for (IP = CB; IP < CL && *IP != HALT; IP++)
  {
    switch(*IP) {
      case HALT     : cerr << "HALT";     break;
      case NOP      : cerr << "NOP";      break;
      case IGETGLBL : cerr << "IGETGLBL" << " " << *(++IP);; break;
      case ISETGLBL : cerr << "ISETGLBL" << " " << *(++IP);; break;
      case IPUSHC   : cerr << "IPUSHC"   << " " << *(++IP);  break;
      case IPOP     : cerr << "IPOP";     break;
      case ISWAP    : cerr << "ISWAP";    break;
      case IDUP     : cerr << "IDUP";     break;
      case IADD     : cerr << "IADD";     break;
      case ISUB     : cerr << "ISUB";     break;
      case IMULT    : cerr << "IMULT";    break;
      case IDIV     : cerr << "IDIV";     break;
      case IMOD     : cerr << "IMOD";     break;
      case INEG     : cerr << "INEG";     break;
      case IPRINT   : cerr << "IPRINT";   break;
      default       : cerr << "unknown opcode 0x" << hex << *IP; break;
    }
    cerr << endl;
  }

  cerr << endl;
}
