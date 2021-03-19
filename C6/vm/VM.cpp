//
//  VM.cpp
//  vm
//
//  Created by Ovidiu Podisor on 04/12/19.
//  Copyright © 2019-2021 innodocs. All rights reserved.
//

#include <iostream>
#include <iomanip>
#include <fstream>
#include <filesystem>
#include <functional>
#include <math.h>
#include <time.h>
using namespace std;

#include "VM.h"


const char* VM::version() noexcept
{
  static char VM_version[128];
  sprintf(VM_version,
    "VM code runner version %d.%d -- Copyright 2002-2021, InnoDocs & Innovative Systems, Inc.",
	MAJOR_VERSION, MINOR_VERSION);

  return VM_version;
}

VM::VM(const char* _codeFileName, Options _options) noexcept
      : codeFileName(_codeFileName), options(_options)
{
  CB = NULL; CL = NULL;
  GB = NULL; GL = NULL;
  SPB= NULL; SPL= NULL;
  OB = NULL; OL = NULL;

  IP = NULL;
  OP = NULL;

  fileSize = 0;
  filePos = 0;
}

VM::~VM()
{
  delete CB;  CB = NULL; CL = NULL;
  delete GB;  GB = NULL; GL = NULL;
  delete SPB; SPB= NULL; SPL= NULL;
  delete OB;  OB = NULL; OL = NULL;

  IP = NULL;
  OP = NULL;
}

void VM::init(const char* codeFileName, unsigned long stackSize)
{
  // init code storage
  loadCode(codeFileName);

  // init stack storage
  OB = new VMObj[stackSize];
  if (OB == NULL)
    throw MemoryError();
  OL = OB + stackSize;

  // init registers
  IP = CB;
  OP = OB-1;
}

inline int VM::readByte(istream_iterator<uint8_t>& si)
{
  uint8_t b0 = *si++; filePos++;
  return b0;
}

inline int VM::readInt(istream_iterator<uint8_t>& si)
{
  uint8_t b3 = *si++; filePos++;
  uint8_t b2 = *si++; filePos++;
  uint8_t b1 = *si++; filePos++;
  uint8_t b0 = *si++; filePos++;
  
  return (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
}

void VM::loadCode(const char *codeFileName)
{
  error_code ec;
  fileSize = filesystem::file_size(filesystem::path(codeFileName), ec);
  if (fileSize == static_cast<std::uintmax_t>(-1))
    throw InvalidCodeFileError(codeFileName);
  if (options.warn || options.disassemble)
    cout << (options.disassemble ? "// " : "") << "File size: " << fileSize << endl;

  ifstream cfs(codeFileName, ifstream::in|ifstream::binary);
  if (! cfs.is_open())
    throw InvalidCodeFileError(codeFileName);
  noskipws(cfs); // why do you need to call this, when you opened in binary mode????
  istream_iterator<uint8_t> cfi(cfs);

  // check validity of code file: magic number
  int magic = readInt(cfi);
  if (magic != MAGIC)
    throw InvalidCodeFileError(codeFileName);
  int majorVersion = readInt(cfi);
  if (majorVersion > MAJOR_VERSION)
    throw InvalidCodeFileError(codeFileName);
  int minorVersion = readInt(cfi);
  if (minorVersion != MINOR_VERSION)
    throw InvalidCodeFileError(codeFileName);
  // TODO: check validity of code file: checksum
  unsigned long checksum = 0;

  // globals
  int globalsMarker = readInt(cfi);
  if (globalsMarker != GLOBALS)
    throw InvalidCodeFileError(codeFileName);
  int nrGlobals = readInt(cfi);
  if (nrGlobals < 0)
    nrGlobals = 0;
  if (nrGlobals > 0) {
    GB = new VMObj[nrGlobals];
    if (GB == NULL)
      throw MemoryError();
  }
  GL = GB + nrGlobals;
  if (options.warn || options.disassemble)
    cout << (options.disassemble ? "// " : "") << "Globals: " << nrGlobals << endl;

  // string pool
  int stringsMarker = readInt(cfi);
  if (stringsMarker != STRINGS)
    throw InvalidCodeFileError(codeFileName);
  int nrStrings = readInt(cfi);
  if (nrStrings < 0)
    nrStrings = 0;
  if (nrStrings > 0) {
      int stringPoolSize = readInt(cfi);
      SPB = new char*[nrStrings +
                  ((stringPoolSize + 1)*sizeof(char) + sizeof(char*)-1)/sizeof(char*)];
      if (SPB == NULL)
        throw MemoryError();

      // install empty string in first slot, and point all strings there (fallback)
      char* emptyStr = reinterpret_cast<char*>(SPB + nrStrings);
      emptyStr[0] = '\0';
      for (int i = 0; i < nrStrings; i++)
        SPB[i] = emptyStr;

      // read all strings into pool
      char* str = emptyStr+1;
      int   pos = 0;
      while ((str-emptyStr-1) < stringPoolSize) {

        if (pos >= nrStrings)
          throw BadStringPoolError(nrStrings, pos);
        SPB[pos] = str;

        int strLen = readByte(cfi);
        for (int i = 0; i < strLen; i++)
          str[i] = readByte(cfi);
        str[strLen] = '\0';

        str += strLen+1;
        pos += 1;
      }
  }
  SPL = SPB + nrStrings;
  if (options.warn || options.disassemble)
    cout << (options.disassemble ? "// " : "") << "Strings: " << nrStrings << endl;

  // code
  uintmax_t codeSize = fileSize - filePos;
  if (options.warn || options.disassemble)
    cout << (options.disassemble ? "// " : "") << "Code size: " << codeSize << endl;

  CB = new int[(codeSize+sizeof(int)-1)/sizeof(int) + 1];
  if (CB == NULL)
    throw MemoryError();
  CL = CB + (codeSize+sizeof(int)-1)/sizeof(int) + 1;

  int pos = 0;
  while (filePos + 4 <= fileSize) {
    int instr = readInt(cfi);
    CB[pos++] = instr;
  }

  // end buffer w/ 'HALT' sentinel, just in case compiler missed it
  CB[pos++] = HALT;
  //CB[pos++] = HALT;

  if (options.warn || options.disassemble)
    cout << endl;
}


void VM::run()
{
  unsigned long stackSize = options.stackSize;
  if (stackSize < 1024)
    stackSize = 1024;
  stackSize *= 1024;

  init(codeFileName, stackSize);

  if (options.disassemble)
    disassemble(cout);
  else {
    clock_t startTime = 0;
    if (options.time)
      startTime = clock();

    execCode();

    if (options.time) {
      clock_t endTime = clock();
      long double elapsedTime = (1000.0 * (endTime - startTime)) / CLOCKS_PER_SEC;
      cout << endl << "Time: " << elapsedTime << " ms" << endl;
    }

    if (OP >= OB) {
      cerr << "Warning: unused elements left on stack" << endl;
      dumpStack(cerr);
    }
  }
}


/*
* memory dump
*/
void VM::dumpGlobals(ostream& os) const
{
  if (GB != NULL && GL != NULL && GB < GL) {
    int pos = 0;
    for (VMObj* gp = GB; gp < GL; gp++) {
      os << pos << ": 0x" << hex << gp[0].i
    		    << " ("   << dec << gp[0].i << ")" << endl;
      pos++;
    }
  }
  else
    os << "<empty>" << endl;
}

void VM::dumpStrings(ostream& os) const
{
  if (SPB != NULL && SPL != NULL && SPB < SPL) {
    int pos = 0;
    for (char** spp = SPB; spp < SPL; spp++) {
      os << pos << ": \"";
      for (char* s = spp[0]; *s != '\0'; s++) {
        switch(*s) {
        case '\\':  os << "\\\\"; break;
        case '\"':  os << "\\\""; break;
        case '\b':  os << "\\b";  break;
        case '\f':  os << "\\f";  break;
        case '\n':  os << "\\n";  break;
        case '\r':  os << "\\r";  break;
        case '\t':  os << "\\t";  break;
        default  :  os << *s;     break;
        }
      }
      os << "\"" << endl;
      pos++;
    }
  }
  else
    os << "<empty>" << endl;
}

void VM::dumpRegisters(ostream& os) const
{
  os << "IP: " << hex << IP << " (" << dec << IP-CB << ")" << endl;
  os << "OP: " << hex << OP << " (" << dec << OP-OB << ")" << endl;
}

void VM::dumpStack(ostream& os) const
{
  if (OB != NULL && OP != NULL && OP >= OB) {
    for (VMObj *op = OP; op >= OB; --op)
      os << "0x" << hex << op[0].i
         << " (" << dec << op[0].i << ")" << endl;
  }
  else
    os << "<empty>" << endl;
}

void VM::dumpCode(ostream& os, int *startIP) const
{
  if (startIP != NULL) {
    for (int* ip = startIP; ip < CL-1 /* && *ip != HALT */; ip++)
    {
      switch(*ip) {
        case HALT     : os << "HALT";     break;
        case NOP      : os << "NOP";      break;
        case ILOADG   : os << "ILOADG"    << " " << *(++ip); break;
        case ISTOREG  : os << "ISTOREG"   << " " << *(++ip); break;
        case IPUSHC   : os << "IPUSHC"    << " " << *(++ip); break;
        case SPUSHC   : os << "SPUSHC"    << " " << *(++ip); break;
        case IPOP     : os << "IPOP";     break;
        case ISWAP    : os << "ISWAP";    break;
        case IDUP     : os << "IDUP";     break;
        case GOTO     : os << "GOTO"      << " " << dec << *(++ip); break;
        case IF_ICMPEQ: os << "IF_ICMPEQ" << " " << dec  << dec << *(++ip); break;
        case IF_ICMPNE: os << "IF_ICMPNE" << " " << dec << *(++ip); break;
        case IF_ICMPLT: os << "IF_ICMPLT" << " " << dec << *(++ip); break;
        case IF_ICMPLE: os << "IF_ICMPLE" << " " << dec << *(++ip); break;
        case IF_ICMPGT: os << "IF_ICMPGT" << " " << dec << *(++ip); break;
        case IF_ICMPGE: os << "IF_ICMPGE" << " " << dec << *(++ip); break;
        case IF_IEQ   : os << "IF_IEQ"    << " " << dec << *(++ip); break;
        case IF_INE   : os << "IF_NE"     << " " << dec << *(++ip); break;
        case IADD     : os << "IADD";     break;
        case ISUB     : os << "ISUB";     break;
        case IMULT    : os << "IMULT";    break;
        case IDIV     : os << "IDIV";     break;
        case IMOD     : os << "IMOD";     break;
        case IPOW     : os << "IPOW";     break;
        case IAND     : os << "IAND";     break;
        case IOR      : os << "IOR";      break;
        case ICMP     : os << "ICMP";     break;
        case ICMPEQ   : os << "ICMPEQ";   break;
        case ICMPNE   : os << "ICMPNE";   break;
        case ICMPLT   : os << "ICMPLT";   break;
        case ICMPLE   : os << "ICMPLE";   break;
        case ICMPGT   : os << "ICMPGT";   break;
        case ICMPGE   : os << "ICMPGE";   break;
        case INEG     : os << "INEG";     break;
        case INOT     : os << "INOT";     break;
        case IPRINT   : os << "IPRINT"    << " " << *(++ip); break;
        case SPRINT   : os << "SPRINT"    << " " << *(++ip); break;
        default       : os << "unknown opcode 0x" << hex << *ip; break;
      }
      os << endl;
    }
  }
  else
    os << "<empty>" << endl;
}

void VM::dumpMem(ostream& os) const
{
  os << "Globals:"   << endl; dumpGlobals(os);   os << endl;
  os << "Strings:"   << endl; dumpStrings(os);   os << endl;
  os << "Registers:" << endl; dumpRegisters(os); os << endl;
  os << "Stack:"     << endl; dumpStack(os);     os << endl;
  os << "Code:"      << endl; dumpCode(os, IP);  os << endl;
}

/*
 * operations
 */
void VM::disassemble(ostream& os) const
{
  os << "// Code:" << endl
     << "GLOBALS " << GL-GB   << endl
     << "STRINGS " << SPL-SPB << endl;
  dumpCode(os, CB);
  os << endl;
}


/*
 * VM instruction handlers
 */

// globals
inline void VM::loadGlobal()
{
  if (OP+1 >= OL)
    throw StackOverflowError(OP, OP-OB);
  if (IP[1] < 0 || IP[1] >= GL-GB)
    throw UndefinedGlobalError(IP[1]);

  OP[1] = GB[ IP[1] ];
  OP++; IP++;
}

inline void VM::storeGlobal()
{
  if (OP < OB)
    throw StackUnderflowError(OP, OP-OB);
  if (IP[1] < 0 || IP[1] >= GL-GB)
    throw UndefinedGlobalError(IP[1]);

  GB[ IP[1] ] = OP[0];
  OP--; IP++;
}

// stack manipulation
inline void VM::pushIntC()
{
  if (OP+1 >= OL)
    throw StackOverflowError(OP, OP-OB);

  OP[1].i = IP[1];
  OP++; IP++;
}

inline void VM::pushStrC()
{
  if (OP+1 >= OL)
    throw StackOverflowError(OP, OP-OB);
  if (IP[1] < 0 || IP[1] >= SPL-SPB)
    throw UndefinedStringError(IP[1]);

  OP[1].s = SPB[ IP[1] ];
  OP++; IP++;
}

inline void VM::pop()
{
  if (OP < OB)
    throw StackUnderflowError(OP, OP-OB);

  OP--;
}

inline void VM::swap()
{
  if (OP < OB)
    throw StackUnderflowError(OP, OP-OB);

  VMObj temp = OP[-1];
  OP[-1] = OP[0];
  OP[0] = temp;
}

inline void VM::dup()
{
  if (OP+1 >= OL)
    throw StackOverflowError(OP, OP-OB);

  OP[1] = OP[0];
  OP++;
}

inline void VM::branch()
{
  // adjust for the IP increment in run loop
  IP += IP[1] - 1;
}

inline void VM::branchIf()
{
  if (OP-(*IP == IF_IEQ || *IP == IF_INE ? 0 : 1) < OB)
    throw StackUnderflowError(OP, OP-OB);

  int offset = 2;
  switch (*IP) {
  case IF_ICMPEQ: if (OP[-1].i == OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_ICMPNE: if (OP[-1].i != OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_ICMPLT: if (OP[-1].i <  OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_ICMPLE: if (OP[-1].i <= OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_ICMPGT: if (OP[-1].i >  OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_ICMPGE: if (OP[-1].i >= OP[0].i) offset = IP[1]; OP -= 2; break;
  case IF_IEQ   : if (0        == OP[0].i) offset = IP[1]; OP -= 1; break;
  case IF_INE   : if (0        != OP[0].i) offset = IP[1]; OP -= 1; break;
  }

  // adjust for the IP increment in run loop
  IP += offset - 1;
}

// expression evaluation
inline void VM::binop()
{
  if (OP-1 < OB)
    throw StackUnderflowError(OP, OP-OB);

  switch (*IP) {
  case IADD  : OP[-1].i = OP[-1].i +  OP[0].i; break;
  case ISUB  : OP[-1].i = OP[-1].i -  OP[0].i; break;
  case IMULT : OP[-1].i = OP[-1].i *  OP[0].i; break;
  case IDIV  : OP[-1].i = OP[-1].i /  OP[0].i; break;
  case IMOD  : OP[-1].i = OP[-1].i %  OP[0].i; break;
  case IPOW  : OP[-1].i = (int)pow(OP[-1].i, OP[0].i); break;

  case IAND  : OP[-1].i = OP[-1].i && OP[0].i; break;
  case IOR   : OP[-1].i = OP[-1].i || OP[0].i; break;

  case ICMP  : OP[-1].i = OP[-1].i -  OP[0].i; break;
  case ICMPEQ: OP[-1].i = OP[-1].i == OP[0].i; break;
  case ICMPNE: OP[-1].i = OP[-1].i != OP[0].i; break;
  case ICMPLT: OP[-1].i = OP[-1].i <  OP[0].i; break;
  case ICMPLE: OP[-1].i = OP[-1].i <= OP[0].i; break;
  case ICMPGT: OP[-1].i = OP[-1].i >  OP[0].i; break;
  case ICMPGE: OP[-1].i = OP[-1].i >= OP[0].i; break;
  }

  OP--;
}

inline void VM::unop()
{
  if (OP < OB)
    throw StackUnderflowError(OP, OP-OB);

  switch (*IP) {
  case INEG : OP[0].i = -OP[0].i; break;
  case INOT : OP[0].i = !OP[0].i; break;
  }
}

inline void VM::print()
{
  int opcode = *IP++;
  int nrExps = *IP;
  if (OP - (nrExps-1) < OB)
    throw StackUnderflowError(OP, OP-OB);

  if (opcode == IPRINT) {
    for (int sp = nrExps-1; sp > 0; --sp)
      cout << OP[-sp].i << ", ";
    cout << OP[0].i;
  }
  else {
    for (int sp = nrExps-1; sp > 0; --sp)
      cout << OP[-sp].s << ", ";
    cout << OP[0].s;
  }

  OP -= nrExps;
}

void VM::execCode()
{
  for (IP = CB; *IP != HALT; IP++)
  {
    switch(*IP) {
    case NOP     :                break;
    case ILOADG  : loadGlobal();  break;
    case ISTOREG : storeGlobal(); break;
    case IPUSHC  : pushIntC();    break;
    case SPUSHC  : pushStrC();    break;
    case IPOP    : pop();         break;
    case ISWAP   : swap();        break;
    case IDUP    : dup();         break;
    case GOTO    : branch();      break;
    case IPRINT  :
    case SPRINT  : print();       break;
    default:
      if      (GOTO <  *IP && *IP <= IF_INE)  branchIf();
      else if (IADD <= *IP && *IP <= IOR)     binop();
      else if (INEG <= *IP && *IP <= INOT)    unop();
      else
        throw InvalidInstrError(*IP);
      break;
    }
  }
}
