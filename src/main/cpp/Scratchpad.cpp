#include <stdio.h>
#include <iostream>
#include "wallet2.h"
//#include <boost/stacktrace.hpp>
using namespace std;

bool walletExists(string path) {
  //std::cout << boost::stacktrace::stacktrace();
  bool keys_file_exists;
  bool wallet_file_exists;
  tools::wallet2::wallet_exists(path, keys_file_exists, wallet_file_exists);
  return wallet_file_exists;
  throw runtime_error("Not implemented");
}

/**
 * Scratchppad main entry point.
 */
int main(int argc, const char* argv[]) {

  // print header
  cout << "===== Scratchpad =====" << endl;
  for (int i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  cout << endl;

  cout << "Wallet exists: " << walletExists("test_wallet_2") << endl;
}

