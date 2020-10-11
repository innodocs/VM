//
//  main.cpp
//  vm
//
//  Created by Ovidiu Podisor on 01/30/19.
//  Copyright © 2019 innodocs. All rights reserved.
//

#include <unistd.h>
#include <iostream>
using namespace std;

#include "VM.h"

void usage()
{
  cerr <<
      "Usage: vm [options] prog\n"
      "\n"
      "options:\n"
      "\t-v               version\n"
      "\t-s <stacksize>   set stack size of VM (in MB)\n"
      "\t-w               show warnings\n"
      "\t-d               disassemble\n"
	  "\t-m               dump VM memory on exit\n"
      "\t-h               show this help\n\n";
}

int main(int argc, char* const argv[])
{
  VM::Options options;
  options.warn = false;
  options.disassemble = false;
  options.dumpMem = false;
  options.stackSize = 1024;

  int ch;
  while ((ch = getopt(argc, argv, "hvwdms:")) != -1)
  {
    switch (ch) {
    case 's':
      options.stackSize = (int)strtol(optarg, NULL, 10);
      if (options.stackSize == 0)
          usage();
      break;
        
    case 'w':
      options.warn = true;
      break;

    case 'd':
      options.disassemble = true;
      break;

    case 'm':
      options.dumpMem = true;
      break;

    case 'v':
      cout << VM::version() << endl;
      return 0;

    case '?': case 'h':
      usage();
      return 0;

    default:
      cerr << "bad option -" << ch << endl << endl;
      usage();
      return 1;
    }
  }
  argc -= optind;
  argv += optind;
  if (argc < 1) {
    usage();
    return 1;
  }

  VM vm(argv[0], options);
  try {
    vm.run();

    if (options.dumpMem) {
      cerr << endl;
      vm.dumpMem(cerr);
    }
    return 0;
  }
  catch (VM::VMError const& vme) {
    cerr << "VM Error " << vme.what() << endl;
    if (options.dumpMem || options.warn) {
      cerr << endl;
      vm.dumpMem(cerr);
    }
    return 1;
  }
}
