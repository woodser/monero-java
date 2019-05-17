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
}

void openWallet(string path, string password, int networkType) {
  cout << "openWallet(" << path << ", " << password << ", " << networkType << ")" << endl;
  std::unique_ptr<tools::wallet2> wallet;
  wallet.reset(new tools::wallet2(cryptonote::network_type::STAGENET, 1, true));
  throw runtime_error("Not implemented");
}

/**
 * Scratchpad main entry point.
 */
int main(int argc, const char* argv[]) {

  // print header
  cout << "===== Scratchpad =====" << endl;
  for (int i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  cout << endl;

  string path = "test_wallet_1";
  string password = "supersecretpassword123";
  int networkType = 2;

  cout << "Wallet exists: " << walletExists(path) << endl;
  openWallet(path, password, networkType);
}

