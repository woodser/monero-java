/**
 * Copyright (c) 2017 m2049r
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "monero_wallet_MoneroWalletJni.h"
#include <iostream>
#include "wallet/wallet2.h"
#include "mnemonics/electrum-words.h"
#include "mnemonics/english.h"
using namespace std;

//// --------------------------------- LISTENER ---------------------------------
//
//#ifdef __cplusplus
//extern "C"
//{
//#endif
//
//static JavaVM *cachedJVM;
////static jclass class_ArrayList;
//static jclass class_WalletListener;
////static jclass class_TransactionInfo;
////static jclass class_Transfer;
////static jclass class_Ledger;
//
//std::mutex _listenerMutex;
//
//JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
//  cachedJVM = jvm;
//  JNIEnv *jenv;
//  if (jvm->GetEnv(reinterpret_cast<void **>(&jenv), JNI_VERSION_1_6) != JNI_OK) {
//    return -1;
//  }
//
////  class_ArrayList = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("java/util/ArrayList")));
////  class_TransactionInfo = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/TransactionInfo")));
////  class_Transfer = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/Transfer")));
//  class_WalletListener = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("monero/wallet/MoneroWalletJni$WalletListenerJni")));
////  class_Ledger = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/ledger/Ledger")));
//  return JNI_VERSION_1_6;
//}
//#ifdef __cplusplus
//}
//#endif
//
//int attachJVM(JNIEnv **jenv) {
//  int envStat = cachedJVM->GetEnv((void **) jenv, JNI_VERSION_1_6);
//  if (envStat == JNI_EDETACHED) {
//    if (cachedJVM->AttachCurrentThread((void **) jenv, nullptr) != 0) {
//      return JNI_ERR;
//    }
//  } else if (envStat == JNI_EVERSION) {
//    return JNI_ERR;
//  }
//  return envStat;
//}
//
//void detachJVM(JNIEnv *jenv, int envStat) {
//  if (jenv->ExceptionCheck()) {
//    jenv->ExceptionDescribe();
//  }
//  if (envStat == JNI_EDETACHED) {
//    cachedJVM->DetachCurrentThread();
//  }
//}
//
//struct WalletListenerJni : Bitmonero::WalletListener {
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
//  void updated() {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//    JNIEnv *jenv;
//    int envStat = attachJVM(&jenv);
//    if (envStat == JNI_ERR) return;
//
//    jmethodID listenerClass_updated = jenv->GetMethodID(class_WalletListener, "updated", "()V");
//    jenv->CallVoidMethod(jlistener, listenerClass_updated);
//
//    detachJVM(jenv, envStat);
//  }
//
//  void moneySpent(const std::string &txId, uint64_t amount) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void moneyReceived(const std::string &txId, uint64_t amount) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void unconfirmedMoneyReceived(const std::string &txId, uint64_t amount) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//  }
//
//  void newBlock(uint64_t height) {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//    JNIEnv *jenv;
//    int envStat = attachJVM(&jenv);
//    if (envStat == JNI_ERR) return;
//
//    jlong h = static_cast<jlong>(height);
//    jmethodID listenerClass_newBlock = jenv->GetMethodID(class_WalletListener, "newBlock", "(J)V");
//    jenv->CallVoidMethod(jlistener, listenerClass_newBlock, h);
//
//    detachJVM(jenv, envStat);
//  }
//
//  void refreshed() {
//    std::lock_guard<std::mutex> lock(_listenerMutex);
//    if (jlistener == nullptr) return;
//    JNIEnv *jenv;
//
//    int envStat = attachJVM(&jenv);
//    if (envStat == JNI_ERR) return;
//
//    jmethodID listenerClass_refreshed = jenv->GetMethodID(class_WalletListener, "refreshed", "()V");
//    jenv->CallVoidMethod(jlistener, listenerClass_refreshed);
//    detachJVM(jenv, envStat);
//  }
//};
////
//////// helper methods
////std::vector<std::string> java2cpp(JNIEnv *env, jobject arrayList) {
////
////    jmethodID java_util_ArrayList_size = env->GetMethodID(class_ArrayList, "size", "()I");
////    jmethodID java_util_ArrayList_get = env->GetMethodID(class_ArrayList, "get",
////                                                         "(I)Ljava/lang/Object;");
////
////    jint len = env->CallIntMethod(arrayList, java_util_ArrayList_size);
////    std::vector<std::string> result;
////    result.reserve(len);
////    for (jint i = 0; i < len; i++) {
////        jstring element = static_cast<jstring>(env->CallObjectMethod(arrayList,
////                                                                     java_util_ArrayList_get, i));
////        const char *pchars = env->GetStringUTFChars(element, NULL);
////        result.emplace_back(pchars);
////        env->ReleaseStringUTFChars(element, pchars);
////        env->DeleteLocalRef(element);
////    }
////    return result;
////}
////
////jobject cpp2java(JNIEnv *env, std::vector<std::string> vector) {
////
////    jmethodID java_util_ArrayList_ = env->GetMethodID(class_ArrayList, "<init>", "(I)V");
////    jmethodID java_util_ArrayList_add = env->GetMethodID(class_ArrayList, "add",
////                                                         "(Ljava/lang/Object;)Z");
////
////    jobject result = env->NewObject(class_ArrayList, java_util_ArrayList_, vector.size());
////    for (std::string &s: vector) {
////        jstring element = env->NewStringUTF(s.c_str());
////        env->CallBooleanMethod(result, java_util_ArrayList_add, element);
////        env->DeleteLocalRef(element);
////    }
////    return result;
////}
////
/////// end helpers
//

#ifdef __cplusplus
extern "C"
{
#endif

// ------------------------------------ STATIC --------------------------------

JNIEXPORT jboolean JNICALL
Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring path) {
  cout << "Java_monero_wallet_MoneroWalletJni_walletExistsJni" << endl;
  const char* _path = env->GetStringUTFChars(path, NULL);
  bool keys_file_exists;
  bool wallet_file_exists;
  tools::wallet2::wallet_exists(std::string(_path), keys_file_exists, wallet_file_exists);
  env->ReleaseStringUTFChars(path, _path);
  return static_cast<jboolean>(wallet_file_exists);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jint networkType) {
  cout << "Java_monero_wallet_MoneroWalletJni_openWalletJni" << endl;
  const char* _path = env->GetStringUTFChars(path, NULL);
  const char* _password = env->GetStringUTFChars(password, NULL);

  // load wallet from file
  tools::wallet2* wallet = new tools::wallet2(static_cast<cryptonote::network_type>(networkType), 1, true);
  wallet->load(string(_path), string(_password));

  env->ReleaseStringUTFChars(path, _path);
  env->ReleaseStringUTFChars(password, _password);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jint networkType, jstring language) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletRandomJni" << endl;
  const char* _language = env->GetStringUTFChars(language, NULL);

  tools::wallet2* wallet = new tools::wallet2(static_cast<cryptonote::network_type>(networkType), 1, true);
  wallet->set_seed_language(string(_language));
  crypto::secret_key recovery_val, secret_key;
  wallet->generate(string(""), string(""), secret_key, false, false);
  cout << wallet->get_refresh_from_block_height() << endl;

  // print the mnemonic
  epee::wipeable_string mnemonic;
  wallet->get_seed(mnemonic);
  cout << "Mnemonic: " << string(mnemonic.data(), mnemonic.size()) << endl;

  env->ReleaseStringUTFChars(language, _language);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring mnemonic, jint networkType, jint restoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni" << endl;
  const char* _mnemonic = env->GetStringUTFChars(mnemonic, NULL);

  // validate mnemonic and get recovery key and language
  crypto::secret_key recoveryKey;
  std::string language;
  bool isValid = crypto::ElectrumWords::words_to_bytes(string(_mnemonic), recoveryKey, language);
  if (!isValid) throw runtime_error("Invalid mnemnic");	// TODO: need proper error handling
  if (language == crypto::ElectrumWords::old_language_name) language = Language::English().get_language_name();

  // initialize wallet
  tools::wallet2* wallet = new tools::wallet2(static_cast<cryptonote::network_type>(networkType), 1, true);
  wallet->set_seed_language(language);
  wallet->generate(string(""), string(""), recoveryKey, true, false);
  wallet->set_refresh_from_block_height(restoreHeight);

  // print the mnemonic
  epee::wipeable_string fetchedMnemonic;
  wallet->get_seed(fetchedMnemonic);
  cout << "Mnemonic: " << string(fetchedMnemonic.data(), fetchedMnemonic.size()) << endl;

  env->ReleaseStringUTFChars(mnemonic, _mnemonic);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring address, jstring viewKey, jstring spendKey, jint networkType, jint restoreHeight, jstring language) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni" << endl;

//  const char *_language = env->GetStringUTFChars(language, NULL);
//  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);
//  const char *_address = env->GetStringUTFChars(address, NULL);
//  const char *_viewKey = env->GetStringUTFChars(viewKey, NULL);
//  const char *_spendKey = env->GetStringUTFChars(spendKey, NULL);
  throw std::runtime_error("Not implemented");

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

// ----------------------------------- INSTANCE -------------------------------

JNIEXPORT jobjectArray JNICALL
Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {

  // get wallet
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");

  // initialize String[3] for uri, username, and password
  jobjectArray vals = env->NewObjectArray(3, env->FindClass("java/lang/String"), nullptr);

  // set daemon address
  if (wallet->get_daemon_address().length() > 0) env->SetObjectArrayElement(vals, 0, env->NewStringUTF(wallet->get_daemon_address().c_str()));

  // set daemon username and password
  if (wallet->get_daemon_login()) {
    if (wallet->get_daemon_login()->username.length() > 0) env->SetObjectArrayElement(vals, 1, env->NewStringUTF(wallet->get_daemon_login()->username.c_str()));
    epee::wipeable_string wipeablePassword = wallet->get_daemon_login()->password;
    string password = string(wipeablePassword.data(), wipeablePassword.size());
    if (password.length() > 0) env->SetObjectArrayElement(vals, 2, env->NewStringUTF(password.c_str()));
  }
  return vals;
}

JNIEXPORT void JNICALL
Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring jurl, jstring jusername, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni" << endl;
  const char* _url = jurl ? env->GetStringUTFChars(jurl, NULL) : nullptr;
  const char* _username = jusername ? env->GetStringUTFChars(jusername, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;

  // prepare url, login, and isTrusted
  string url = string(jurl ? _url : "");
  boost::optional<epee::net_utils::http::login> login{};
  if (jusername) login.emplace(string(_username), string(_password));
  bool isTrusted = false;
  try { isTrusted = tools::is_local_address(url); }	// wallet is trusted iff local
  catch (const exception &e) { }

  // set wallet's daemon connection
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  wallet->set_daemon(url, login, isTrusted);

  env->ReleaseStringUTFChars(jurl, _url);
  env->ReleaseStringUTFChars(jusername, _username);
  env->ReleaseStringUTFChars(jpassword, _password);
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPathJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  return env->NewStringUTF(wallet->path().c_str());
}

JNIEXPORT jint JNICALL
Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  return wallet->nettype();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguageJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  return env->NewStringUTF(wallet->get_seed_language().c_str());
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getHeightJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  return wallet->get_blockchain_current_height();
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  cout << wallet->get_refresh_from_block_height() << endl;
  return wallet->get_refresh_from_block_height();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getMnemonicJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  epee::wipeable_string mnemonic;
  wallet->get_seed(mnemonic);
  return env->NewStringUTF(string(mnemonic.data(), mnemonic.size()).c_str());
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressJni" << endl;
  tools::wallet2* wallet = getHandle<tools::wallet2>(env, instance, "walletHandle");
  string address = wallet->get_subaddress_as_str({(uint32_t) accountIdx, (uint32_t) subaddressIdx});
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  cout << "Java_monero_wallet_MoneroWalletJni_setListenerJni" << endl;
  //throw std::runtime_error("Not implemented");
//  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance, "walletHandle");
//
//  // clear old listener
//  wallet->setListener(nullptr);
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
//    wallet->setListener(listener);
//    return reinterpret_cast<jlong>(listener);
//  }
}

JNIEXPORT void JNICALL
Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jint startHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_syncJni" << endl;
  throw std::runtime_error("Not implemented");
  //Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance, "walletHandle");
  //wallet->refresh();
}

#ifdef __cplusplus
}
#endif
