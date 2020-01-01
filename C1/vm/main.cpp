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

#include "vm.h"

void usage()
{
  cerr <<
      "usage: vm [options] prog\n"
      "\n"
      "options:\n"
      "\t-s <stacksize>   set stack size of VM (in MB)\n"
      "\t-v               verbose\n"
      "\t-d               dump code\n"
      "\t-h               show this help\n";
  exit(1);
}

int main(int argc, char* const argv[])
{
  unsigned long stackSize = 1024*1024L;
  bool isVerbose = false;
  bool dumpCode = false;

  int ch;
  while ((ch = getopt(argc, argv, "hvds:")) != -1)
  {
    switch (ch) {
    case 's':
      stackSize = strtol(optarg, NULL, 10);
      if (stackSize == 0)
          usage();
      stackSize *= 1024;
      break;

    case 'v':
      isVerbose = true;
      break;

    case 'd':
      dumpCode = true;
      break;

    case '?': case 'h':
    default:
      usage();
    }
  }
  argc -= optind;
  argv += optind;

  if (argc < 1)
    usage();

  VM vm(argv[0], stackSize, isVerbose, dumpCode);
  vm.run();
}
