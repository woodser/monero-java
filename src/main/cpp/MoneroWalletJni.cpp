#include "MoneroWalletJni.h"
#include "wallet/MoneroWallet.h"
#include <iostream>

using namespace std;
using namespace monero;

// --------------------------------- LISTENER ---------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

static JavaVM *cachedJVM;
//static jclass class_ArrayList;
static jclass class_WalletListener;
//static jclass class_TransactionInfo;
//static jclass class_Transfer;
//static jclass class_Ledger;

std::mutex _listenerMutex;

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
  cachedJVM = jvm;
  JNIEnv *jenv;
  if (jvm->GetEnv(reinterpret_cast<void **>(&jenv), JNI_VERSION_1_6) != JNI_OK) {
    return -1;
  }

//  class_ArrayList = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("java/util/ArrayList")));
//  class_TransactionInfo = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/TransactionInfo")));
//  class_Transfer = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/Transfer")));
  class_WalletListener = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("monero/wallet/MoneroWalletJni$Wallet2Listener")));
//  class_Ledger = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/ledger/Ledger")));
  return JNI_VERSION_1_6;
}
#ifdef __cplusplus
}
#endif

int attachJVM(JNIEnv **jenv) {
  int envStat = cachedJVM->GetEnv((void **) jenv, JNI_VERSION_1_6);
  if (envStat == JNI_EDETACHED) {
    if (cachedJVM->AttachCurrentThread((void **) jenv, nullptr) != 0) {
      return JNI_ERR;
    }
  } else if (envStat == JNI_EVERSION) {
    return JNI_ERR;
  }
  return envStat;
}

void detachJVM(JNIEnv *jenv, int envStat) {
  if (jenv->ExceptionCheck()) {
    jenv->ExceptionDescribe();
  }
  if (envStat == JNI_EDETACHED) {
    cachedJVM->DetachCurrentThread();
  }
}

///**
// * Invokes Java callbacks on wallet notifications.
// */
//struct WalletListenerJni : public tools::i_wallet2_callback {
//  jobject jlistener;
//
//  WalletListenerJni(JNIEnv *env, jobject listener) {
//    jlistener = env->NewGlobalRef(listener);
//  }
//
//  ~WalletListenerJni() { };
//
//  void deleteGlobalJavaRef(JNIEnv *env) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    env->DeleteGlobalRef(jlistener);
//    jlistener = nullptr;
//  }
//
//  // TODO: throttle notifications like wallet.cpp::on_new_block?
//  void on_new_block(uint64_t height, const cryptonote::block& block) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//    JNIEnv *jenv;
//    int envStat = attachJVM(&jenv);
//    if (envStat == JNI_ERR) return;
//    jlong h = static_cast<jlong>(height);
//    jmethodID listenerClass_newBlock = jenv->GetMethodID(class_WalletListener, "onNewBlock", "(J)V");
//    jenv->CallVoidMethod(jlistener, listenerClass_newBlock, h);
//    detachJVM(jenv, envStat);
//  }
//
//  void on_money_received(uint64_t height, const crypto::hash &txid, const cryptonote::transaction& tx, uint64_t amount, const cryptonote::subaddress_index& subaddr_index) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void on_unconfirmed_money_received(uint64_t height, const crypto::hash &txid, const cryptonote::transaction& tx, uint64_t amount, const cryptonote::subaddress_index& subaddr_index) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void on_money_spent(uint64_t height, const crypto::hash &txid, const cryptonote::transaction& in_tx, uint64_t amount, const cryptonote::transaction& spend_tx, const cryptonote::subaddress_index& subaddr_index) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void on_pool_tx_removed(const crypto::hash &txid) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//};

// ----------------------------- COMMON HELPERS -------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

void setDaemonConnection(JNIEnv *env, MoneroWallet* wallet, jstring juri, jstring jusername, jstring jpassword) {
  const char* _url = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  const char* _username = jusername ? env->GetStringUTFChars(jusername, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;

  // set daemon connection
  wallet->setDaemonConnection(string(juri ? _url : ""), string(jusername ? _username : ""), string(jpassword ? _password : ""));

  env->ReleaseStringUTFChars(juri, _url);
  env->ReleaseStringUTFChars(jusername, _username);
  env->ReleaseStringUTFChars(jpassword, _password);
}

// ------------------------------- JNI STATIC ---------------------------------

JNIEXPORT jboolean JNICALL
Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring path) {
  cout << "Java_monero_wallet_MoneroWalletJni_walletExistsJni" << endl;
  const char* _path = env->GetStringUTFChars(path, NULL);
  bool walletExists = MoneroWallet::walletExists(string(_path));
  env->ReleaseStringUTFChars(path, _path);
  return static_cast<jboolean>(walletExists);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType) {
  cout << "Java_monero_wallet_MoneroWalletJni_openWalletJni" << endl;
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  const char* _password = env->GetStringUTFChars(jpassword, NULL);

  // load wallet from file
  MoneroWallet* wallet = new MoneroWallet(string(_path), string(_password), static_cast<MoneroNetworkType>(jnetworkType));

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jint jnetworkType, jstring jdaemonUri, jstring jdaemonUsername, jstring jdaemonPassword, jstring jlanguage) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletRandomJni" << endl;
  const char* _uri = jdaemonUri ? env->GetStringUTFChars(jdaemonUri, NULL) : nullptr;
  const char* _username = jdaemonUsername ? env->GetStringUTFChars(jdaemonUsername, NULL) : nullptr;
  const char* _password = jdaemonPassword ? env->GetStringUTFChars(jdaemonPassword, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;

  // construct wallet
  MoneroRpcConnection daemonConnection = MoneroRpcConnection(string(_uri ? _uri : ""), string(_username ? _username : ""), string(_password ? _password : ""));
  MoneroWallet* wallet = new MoneroWallet(static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, string(_language ? _language : ""));

  env->ReleaseStringUTFChars(jdaemonUri, _uri);
  env->ReleaseStringUTFChars(jdaemonUsername, _username);
  env->ReleaseStringUTFChars(jdaemonPassword, _password);
  env->ReleaseStringUTFChars(jlanguage, _language);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring jmnemonic, jint jnetworkType, jlong jrestoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni" << endl;
  const char* _mnemonic = env->GetStringUTFChars(jmnemonic, NULL);

  // construct wallet
  MoneroRpcConnection daemonConnection;
  MoneroWallet* wallet = new MoneroWallet(string(_mnemonic), static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, (uint64_t) jrestoreHeight);

  env->ReleaseStringUTFChars(jmnemonic, _mnemonic);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring address, jstring viewKey, jstring spendKey, jint networkType, jlong restoreHeight, jstring language) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni" << endl;
  throw std::runtime_error("Not implemented");

//  const char *_language = env->GetStringUTFChars(language, NULL);
//  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);
//  const char *_address = env->GetStringUTFChars(address, NULL);
//  const char *_viewKey = env->GetStringUTFChars(viewKey, NULL);
//  const char *_spendKey = env->GetStringUTFChars(spendKey, NULL);

//  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->createWalletFromKeys( std::string(_path), std::string(_password), std::string(_language), _networkType, (uint64_t) restoreHeight, std::string(_address), std::string(_viewKey), std::string(_spendKey));
//
//  env->ReleaseStringUTFChars(path, _path);
//  env->ReleaseStringUTFChars(password, _password);
//  env->ReleaseStringUTFChars(language, _language);
//  env->ReleaseStringUTFChars(address, _address);
//  env->ReleaseStringUTFChars(viewKey, _viewKey);
//  env->ReleaseStringUTFChars(spendKey, _spendKey);
//  return reinterpret_cast<jlong>(wallet);
}

// --------------------------------- JNI INSTANCE -----------------------------

JNIEXPORT jobjectArray JNICALL
Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni()" << endl;

  // get wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");

  // get daemon connection
  MoneroRpcConnection daemonConnection = wallet->getDaemonConnection();

  // return string[uri, username, password]
  jobjectArray vals = env->NewObjectArray(3, env->FindClass("java/lang/String"), nullptr);
  if (!daemonConnection.uri.empty()) env->SetObjectArrayElement(vals, 0, env->NewStringUTF(daemonConnection.uri.c_str()));
  if (!daemonConnection.username.empty()) env->SetObjectArrayElement(vals, 1, env->NewStringUTF(daemonConnection.username.c_str()));
  if (!daemonConnection.password.empty()) env->SetObjectArrayElement(vals, 2, env->NewStringUTF(daemonConnection.password.c_str()));
  return vals;
}

JNIEXPORT void JNICALL
Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring juri, jstring jusername, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  setDaemonConnection(env, wallet, juri, jusername, jpassword);
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPathJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return env->NewStringUTF(wallet->getPath().c_str());
}

JNIEXPORT jint JNICALL
Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return wallet->getNetworkType();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguageJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return env->NewStringUTF(wallet->getLanguage().c_str());
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return wallet->getHeight();
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getChainHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getChainHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return wallet->getChainHeight();
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return wallet->getRestoreHeight();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getMnemonicJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  return env->NewStringUTF(wallet->getMnemonic().c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "walletHandle");
  string address = wallet->getAddress((uint32_t) accountIdx, (uint32_t) subaddressIdx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->balance_all();
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->balance(accountIdx);
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->balance_per_subaddress(accountIdx).at(subaddressIdx);
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->unlocked_balance_all();
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->unlocked_balance(accountIdx);
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t balance = wallet->unlocked_balance_per_subaddress(accountIdx).at(subaddressIdx).first;
//  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  cout << "Java_monero_wallet_MoneroWalletJni_setListenerJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//
//  // clear old listener
//  wallet->callback(nullptr);
//  WalletListenerJni *oldListener = getHandle<WalletListenerJni>(env, instance, "listenerHandle");
//  if (oldListener != nullptr) {
//    oldListener->deleteGlobalJavaRef(env);
//    delete oldListener;
//  }
//
//  // set new listener
//  if (jlistener == nullptr) {
//    return 0;
//  } else {
//    WalletListenerJni *listener = new WalletListenerJni(env, jlistener);
//    wallet->callback(listener);
//    return reinterpret_cast<jlong>(listener);
//  }
}

JNIEXPORT jobjectArray JNICALL
Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jlong startHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_syncJni" << endl;
  throw runtime_error("Not implemented");
//  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
//  uint64_t blocksFetched;
//  bool receivedMoney;
//  wallet->refresh(wallet->is_trusted_daemon(), startHeight, blocksFetched, receivedMoney, true);
//  cout << "Done refreshing.  Blocks fetched: " << blocksFetched << ", received money: " << receivedMoney << endl;
//
//  // build and return results as Object[2]{(long) numBlocksFetched, (boolean) receivedMoney}
//  jobjectArray results = env->NewObjectArray(2, env->FindClass("java/lang/Object"), nullptr);
//  jclass longClass = env->FindClass("java/lang/Long");
//  jmethodID longConstructor = env->GetMethodID(longClass, "<init>", "(J)V");
//  jobject numBlocksFetchedWrapped = env->NewObject(longClass, longConstructor, static_cast<jlong>(blocksFetched));
//  env->SetObjectArrayElement(results, 0, numBlocksFetchedWrapped);
//  jclass booleanClass = env->FindClass("java/lang/Boolean");
//  jmethodID booleanConstructor = env->GetMethodID(booleanClass, "<init>", "(Z)V");
//  jobject receivedMoneyWrapped = env->NewObject(booleanClass, booleanConstructor, static_cast<jboolean>(receivedMoney));
//  env->SetObjectArrayElement(results, 1, receivedMoneyWrapped);
//  return results;
}

#ifdef __cplusplus
}
#endif
