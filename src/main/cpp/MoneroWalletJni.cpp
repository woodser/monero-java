#include <iostream>
#include "MoneroWalletJni.h"
#include "utils/MoneroUtils.h"

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
  class_WalletListener = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("monero/wallet/MoneroWalletJni$WalletJniListener")));
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

/**
 * Listens for wallet notifications and notifies the cpp listener in Java.
 */
struct WalletJniListener : public MoneroWalletListener {
  jobject jlistener;

  WalletJniListener(JNIEnv* env, jobject listener) {
    jlistener = env->NewGlobalRef(listener);
  }

  ~WalletJniListener() { };

  void deleteGlobalJavaRef(JNIEnv *env) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    env->DeleteGlobalRef(jlistener);
    jlistener = nullptr;
  }

  virtual void onNewBlock(MoneroBlock& block) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *jenv;
    int envStat = attachJVM(&jenv);
    if (envStat == JNI_ERR) return;

    jlong jheight = static_cast<jlong>(*block.height);
    jmethodID listenerClass_onNewBlock = jenv->GetMethodID(class_WalletListener, "onNewBlock", "(J)V");
    jenv->CallVoidMethod(jlistener, listenerClass_onNewBlock, jheight);
    detachJVM(jenv, envStat);
  }

  virtual void onSyncProgress(uint64_t startHeight, uint64_t numBlocksDone, uint64_t numBlocksTotal, double percentDone, string& message) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *jenv;
    int envStat = attachJVM(&jenv);
    if (envStat == JNI_ERR) return;

    // prepare callback arguments
    jlong jstartHeight = static_cast<jlong>(startHeight);
    jlong jnumBlocksDone = static_cast<jlong>(numBlocksDone);
    jlong jnumBlocksTotal = static_cast<jlong>(numBlocksTotal);
    jdouble jpercentDone = static_cast<jdouble>(percentDone);
    jstring jmessage = jenv->NewStringUTF(message.c_str());

    jmethodID listenerClass_onSyncProgress = jenv->GetMethodID(class_WalletListener, "onSyncProgress", "(JJJDLjava/lang/String;)V");
    jenv->CallVoidMethod(jlistener, listenerClass_onSyncProgress, jstartHeight, jnumBlocksDone, jnumBlocksTotal, jpercentDone, jmessage);
    jenv->DeleteLocalRef(jmessage);
    detachJVM(jenv, envStat);
  }
};

// ------------------------- RESPONSE CONTAINER STRUCTS -----------------------

struct AccountsContainer {
  vector<MoneroAccount> accounts;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(accounts)
  END_KV_SERIALIZE_MAP()
};

struct SubaddressesContainer {
  vector<MoneroSubaddress> subaddresses;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(subaddresses)
  END_KV_SERIALIZE_MAP()
};

struct BlocksContainer {
  vector<MoneroBlock> blocks;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(blocks)
  END_KV_SERIALIZE_MAP()
};

// ----------------------------- COMMON HELPERS -------------------------------

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

//void toModel(const boost::property_tree::ptree& root, MoneroTxRequest& request) {
//
//  // print node to convert for shiggles
//  std::stringstream ss;
//  boost::property_tree::write_json(ss, root, false);
//  string serialized = ss.str();
//  cout << "Converting property tree to MoneroTxRequest:  " << serialized << endl;
//
//  // initialize block
//  MoneroBlock block;
//  blockNodeToModel(root, block);
//
//  // initialize tx request extensions
//  for (boost::property_tree::ptree::const_iterator it = root.begin(); it != root.end(); ++it) {
//    string key = it->first;
//    if (key == string("isOutgoing")) {
//      cout << "Handling isOutgoing key" << endl;
//      request.isOutgoing = shared_ptr<bool>(make_shared<bool>(boost::lexical_cast<bool>(it->second.data())));
//      cout << *request.isOutgoing << endl;
//    }
//  }
//
//  cout << "Block's height: " << endl;
//  throw runtime_error("Need to initialize fields specific to request");
//}

//void toModel(const boost::property_tree::ptree& node, MoneroTxWallet& tx) {
//  cout << "toModel(txWallet)" << endl;
//  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
//    string key = it->first;
//    cout << "Property tree key: " << key << endl;
//    if (key == string("height")) {
//      MoneroBlock block;
//      block.height = std::shared_ptr<uint64_t>(std::make_shared<uint64_t>((uint64_t) 7));
//      tx.block = std::shared_ptr<MoneroBlock>(std::make_shared<MoneroBlock>(block));
//    }
//  }
//}

// TODO: no common utility?  make common utility
bool stringToBool(string str) {
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  if (string("true") == str) return true;
  if (string("false") == str) return false;
  return boost::lexical_cast<bool>(str);
}

void txNodeToModel(const boost::property_tree::ptree& node, MoneroTx& tx) {
  cout << "txNodeToModel()" << endl;

//  // print for debug
//  std::stringstream ss;
//  boost::property_tree::write_json(ss, node, false);
//  string receivedNode = ss.str();
//  cout << "Received node: " << receivedNode << endl;

  // initialize tx from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Tx node key: " << key << endl;
    if (key == string("id")) tx.id = it->second.data();
  }

  //throw runtime_error("txNodeToModel");
}

void txWalletNodeToModel(const boost::property_tree::ptree& node, MoneroTxWallet& tx) {
  txNodeToModel(node, tx);
}

void txRequestNodeToModel(const boost::property_tree::ptree& node, MoneroTxRequest& txRequest) {
  txWalletNodeToModel(node, txRequest);

  // initialize request from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Tx request node key: " << key << endl;
    if (key == string("isOutgoing")) txRequest.isOutgoing = shared_ptr<bool>(make_shared<bool>(stringToBool(it->second.data())));
  }
}

void transferNodeToModel(const boost::property_tree::ptree& node, MoneroTransfer& transfer) {
  throw runtime_error("transferNodeToModel");
}

void transferRequestNodeToModel(const boost::property_tree::ptree& node, MoneroTransferRequest& transferRequest) {
  throw runtime_error("transferRequestNodeToModel");
}

void outputNodeToModel(const boost::property_tree::ptree& node, MoneroOutput& output) {
  throw runtime_error("outputNodeToModel");
}

void outputRequestNodeToModel(const boost::property_tree::ptree& node, MoneroOutputRequest& outputRequest) {
  throw runtime_error("outputRequestNodeToModel");
}

void blockNodeToModel(const boost::property_tree::ptree& node, MoneroBlock& block) {
  cout << "blockNodeToModel()" << endl;
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Block node Key: " << key << endl;
    if (key == string("height")) block.height = (uint64_t) 7;  // TODO
    else if (key == string("txs")) {
      boost::property_tree::ptree txsNode = it->second;
      for (boost::property_tree::ptree::const_iterator it2 = txsNode.begin(); it2 != txsNode.end(); ++it2) {
        MoneroTxRequest txRequest;
        txRequestNodeToModel(it2->second, txRequest);
        block.txs.push_back(make_shared<MoneroTxRequest>(txRequest));
      }
    }
  }
}

MoneroTxRequest deserializeTxRequest(string txRequestStr) {
  cout << "deserializeTxRequest(): " <<  txRequestStr << endl;

  // deserialize tx request string to property rooted at block
  std::istringstream iss = txRequestStr.empty() ? std::istringstream() : std::istringstream(txRequestStr);
  boost::property_tree::ptree blockNode;
  boost:property_tree:read_json(iss, blockNode);

  // convert property tree to block
  MoneroBlock block;
  blockNodeToModel(blockNode, block);

  // return tx which represents request
  return static_cast<MoneroTxRequest&>(*block.txs[0]);
}

// ------------------------------- JNI STATIC ---------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

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
Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType, jstring jdaemonUri, jstring jdaemonUsername, jstring jdaemonPassword, jstring jlanguage) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletRandomJni" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _daemonUri = jdaemonUri ? env->GetStringUTFChars(jdaemonUri, NULL) : nullptr;
  const char* _daemonUsername = jdaemonUsername ? env->GetStringUTFChars(jdaemonUsername, NULL) : nullptr;
  const char* _daemonPassword = jdaemonPassword ? env->GetStringUTFChars(jdaemonPassword, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;

  // construct wallet
  MoneroRpcConnection daemonConnection = MoneroRpcConnection(string(_daemonUri ? _daemonUri : ""), string(_daemonUsername ? _daemonUsername : ""), string(_daemonPassword ? _daemonPassword : ""));
  MoneroWallet* wallet = new MoneroWallet(string(_path ? _path : ""), string(_password ? _password : ""), static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, string(_language ? _language : ""));

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jdaemonUri, _daemonUri);
  env->ReleaseStringUTFChars(jdaemonUsername, _daemonUsername);
  env->ReleaseStringUTFChars(jdaemonPassword, _daemonPassword);
  env->ReleaseStringUTFChars(jlanguage, _language);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jstring jmnemonic, jint jnetworkType, jlong jrestoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _mnemonic = env->GetStringUTFChars(jmnemonic, NULL);

  // construct wallet
  MoneroRpcConnection daemonConnection;
  MoneroWallet* wallet = new MoneroWallet(string(_path ? _path : ""), string(_password ? _password : ""), string(_mnemonic), static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, (uint64_t) jrestoreHeight);

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jmnemonic, _mnemonic);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jstring address, jstring viewKey, jstring spendKey, jint networkType, jlong restoreHeight, jstring language) {
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

//  ------------------------------- JNI INSTANCE ------------------------------

JNIEXPORT jobjectArray JNICALL
Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni()" << endl;

  // get wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

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
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  setDaemonConnection(env, wallet, juri, jusername, jpassword);
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPathJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getPath().c_str());
}

JNIEXPORT jint JNICALL
Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getNetworkType();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getMnemonicJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getMnemonic().c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguageJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getLanguage().c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  string address = wallet->getAddress((uint32_t) accountIdx, (uint32_t) subaddressIdx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAddressIndexJni(JNIEnv *env, jobject instance, jstring jaddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressIndexJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;

  // get indices of address's subaddress
  MoneroSubaddress subaddress;
  try {
    subaddress = wallet->getAddressIndex(string(_address));
  } catch (runtime_error& e) {
    jclass jcls = env->FindClass("monero/utils/MoneroException");
    env->ThrowNew(jcls, e.what());
  }

  // serialize subaddresses which contain indices
  string subaddressJson = string("temp");
  env->ReleaseStringUTFChars(jaddress, _address);
  return env->NewStringUTF(subaddressJson.c_str());
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  cout << "Java_monero_wallet_MoneroWalletJni_setListenerJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // clear old listener
  wallet->setListener(boost::none);
  WalletJniListener *oldListener = getHandle<WalletJniListener>(env, instance, "jniListenerHandle");
  if (oldListener != nullptr) {
    oldListener->deleteGlobalJavaRef(env);
    delete oldListener;
  }

  // set new listener
  if (jlistener == nullptr) {
    return 0;
  } else {
    WalletJniListener* listener = new WalletJniListener(env, jlistener);
    wallet->setListener(*listener);
    return reinterpret_cast<jlong>(listener);
  }
}

JNIEXPORT jobjectArray JNICALL
Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jlong startHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_syncJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // sync wallet
  MoneroSyncResult result = wallet->sync(startHeight);
  cout << "Done syncing.  Blocks fetched: " << result.numBlocksFetched << ", received money: " << result.receivedMoney << endl;

  // build and return results as Object[2]{(long) numBlocksFetched, (boolean) receivedMoney}
  jobjectArray results = env->NewObjectArray(2, env->FindClass("java/lang/Object"), nullptr);
  jclass longClass = env->FindClass("java/lang/Long");
  jmethodID longConstructor = env->GetMethodID(longClass, "<init>", "(J)V");
  jobject numBlocksFetchedWrapped = env->NewObject(longClass, longConstructor, static_cast<jlong>(result.numBlocksFetched));
  env->SetObjectArrayElement(results, 0, numBlocksFetchedWrapped);
  jclass booleanClass = env->FindClass("java/lang/Boolean");
  jmethodID booleanConstructor = env->GetMethodID(booleanClass, "<init>", "(Z)V");
  jobject receivedMoneyWrapped = env->NewObject(booleanClass, booleanConstructor, static_cast<jboolean>(result.receivedMoney));
  env->SetObjectArrayElement(results, 1, receivedMoneyWrapped);
  return results;
}

// rescanBlockchain

// isMultisigImportNeeded

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getHeight();
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getChainHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getChainHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getChainHeight();
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getRestoreHeight();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAccountsJni(JNIEnv* env, jobject instance, jboolean includeSubaddresses, jstring jtag) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _tag = jtag ? env->GetStringUTFChars(jtag, NULL) : nullptr;

  // get accounts
  vector<MoneroAccount> accounts = wallet->getAccounts(includeSubaddresses, _tag ? string(_tag) : "");

//  // print account info
//  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
//  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
//    MoneroAccount account = accounts.at(accountIdx);
//    cout << "Account index: " << account.index << endl;
//    cout << "Account label: " << account.label << endl;
//    cout << "Account balance: " << account.balance << endl;
//    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
//
//    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
//	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
//	    cout << "Converted to JSON: " << json << endl;
//    }
//  }

  // convert accounts to property tree
  boost::property_tree::ptree accountsNode = MoneroUtils::toPropertyTree(accounts);

  // serialize property tree to json
  boost::property_tree::ptree root;
  root.add_child("accounts", accountsNode);
  std::stringstream ss;
  boost::property_tree::write_json(ss, root, false);
  string accountsJson = ss.str();
  cout << "Serialized " << accountsJson << endl;
  env->ReleaseStringUTFChars(jtag, _tag);
  return env->NewStringUTF(accountsJson.c_str());

//  string accountsJson = MoneroUtils::serialize(accounts);
//  cout << "Serialized " << accountsJson << endl;
//  env->ReleaseStringUTFChars(jtag, _tag);
//  return env->NewStringUTF(accountsJson.c_str());




  //std::stringstream ss;
  //json_archive<true> ar(ss);
  //accounts[0].serialize_base(ar);

//  for (int i = 0; i < accounts.size(); i++) {
//    ar << accounts[i];
//  }
  //string accountsJson = ss.str();
  //string accountsJson = string("temp");
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountJni(JNIEnv* env, jobject instance, jint accountIdx, jboolean includeSubaddresses) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get account
  MoneroAccount account = wallet->getAccount(accountIdx, includeSubaddresses);

//  // print account info
//  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
//  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
//    MoneroAccount account = accounts.at(accountIdx);
//    cout << "Account index: " << account.index << endl;
//    cout << "Account label: " << account.label << endl;
//    cout << "Account balance: " << account.balance << endl;
//    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
//
//    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
//	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
//	    cout << "Converted to JSON: " << json << endl;
//    }
//  }

  // serialize and returna account
  string accountJson = string("temp");
  return env->NewStringUTF(accountJson.c_str());
}


JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getSubaddressesJni(JNIEnv* env, jobject instance, jint accountIdx, jintArray jsubaddressIndices) {
  cout << "Java_monero_wallet_MoneroWalletJni_getSubaddressesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get subaddress indices
  vector<uint32_t> subaddressIndices;
  if (jsubaddressIndices != nullptr) {
    jsize numSubaddressIndices = env->GetArrayLength(jsubaddressIndices);
    jint* intArr = env->GetIntArrayElements(jsubaddressIndices, 0);
    for (int subaddressIndicesIdx = 0; subaddressIndicesIdx < numSubaddressIndices; subaddressIndicesIdx++) {
      subaddressIndices.push_back(intArr[subaddressIndicesIdx]);
    }
  }

  // get subaddresses
  vector<MoneroSubaddress> subaddresses = wallet->getSubaddresses(accountIdx, subaddressIndices);

  //  // print account info
  //  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
  //  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
  //    MoneroAccount account = accounts.at(accountIdx);
  //    cout << "Account index: " << account.index << endl;
  //    cout << "Account label: " << account.label << endl;
  //    cout << "Account balance: " << account.balance << endl;
  //    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
  //
  //    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
  //	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
  //	    cout << "Converted to JSON: " << json << endl;
  //    }
  //  }

  // serialize and return subaddresses
  string subaddressesJson = string("temp");
  return env->NewStringUTF(subaddressesJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxsJni(JNIEnv* env, jobject instance, jstring jtxRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txRequest = jtxRequest ? env->GetStringUTFChars(jtxRequest, NULL) : nullptr;

  // deserialize tx request
  MoneroTxRequest txRequest = deserializeTxRequest(string(_txRequest ? _txRequest : ""));

  // get txs
  vector<MoneroTxWallet> txs = wallet->getTxs(txRequest);

  // return unique blocks to preserve model relationships as tree
  vector<MoneroBlock> blocks;
  unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
  for (auto const& tx : txs) {
    if (tx.block == boost::none) throw runtime_error("Tx block is null");
    unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(*tx.block);
    if (got == seenBlockPtrs.end()) {
      seenBlockPtrs.insert(*tx.block);
      blocks.push_back(**tx.block);
    }
  }
  cout << "Returning " << blocks.size() << " blocks" << endl;

  // wrap and serialize blocks
  //BlocksContainer resp;
  //resp.blocks = blocks;
  string blocksJson = string("temp");
  //string blocksJson = "temp";
  env->ReleaseStringUTFChars(jtxRequest, _txRequest);
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_saveJni(JNIEnv* env, jobject instance, jstring jpath, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_saveJni(path, password)" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpath ? env->GetStringUTFChars(jpassword, NULL) : nullptr;

  // attempt to save, return error if one happens
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->save(string(_path ? _path : ""), string(_password ? _password : ""));
  } catch (runtime_error& e) {
    string msg = e.what();
    return env->NewStringUTF(msg.c_str());
  }

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  return nullptr;
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_closeJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_CloseJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  delete wallet;
  wallet = nullptr;
}

#ifdef __cplusplus
}
#endif
