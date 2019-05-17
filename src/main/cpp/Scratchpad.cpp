#include <stdio.h>
#include <iostream>
#include "wallet2.h"
using namespace std;

/**
 * Main entry point.
 */
int main(int argc, const char* argv[]) {

  cout << "Hello world!" << endl;

  // print each argument from the command line
  for (int i = 0; i < argc; i++) {
    printf("arg %d: %s\n", i, argv[i]);
  }
}
