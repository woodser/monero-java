/**
 * Copyright (c) 2017-2019 woodser
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Parts of this file are originally copyright (c) 2017 m2049r Monerujo
 * *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <iostream>
#include "MoneroWalletJni.h"
#include "utils/MoneroUtils.h"

using namespace std;
using namespace monero;

// initialize names of private instance variables used in Java JNI wallet which contain memory references to native wallet and listener
static const char* JNI_WALLET_HANDLE = "jniWalletHandle";
static const char* JNI_LISTENER_HANDLE = "jniListenerHandle";

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

  virtual void onSyncProgress(uint64_t height, uint64_t startHeight, uint64_t endHeight, double percentDone, const string& message) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *jenv;
    int envStat = attachJVM(&jenv);
    if (envStat == JNI_ERR) return;

    // prepare callback arguments
    jlong jheight = static_cast<jlong>(height);
    jlong jstartHeight = static_cast<jlong>(startHeight);
    jlong jendHeight = static_cast<jlong>(endHeight);
    jdouble jpercentDone = static_cast<jdouble>(percentDone);
    jstring jmessage = jenv->NewStringUTF(message.c_str());

    jmethodID listenerClass_onSyncProgress = jenv->GetMethodID(class_WalletListener, "onSyncProgress", "(JJJDLjava/lang/String;)V");
    jenv->CallVoidMethod(jlistener, listenerClass_onSyncProgress, jheight, jstartHeight, jendHeight, jpercentDone, jmessage);
    jenv->DeleteLocalRef(jmessage);
    detachJVM(jenv, envStat);
  }
};

// ----------------------------- COMMON HELPERS -------------------------------

// Based on: https://stackoverflow.com/questions/2054598/how-to-catch-jni-java-exception/2125673#2125673
void rethrow_cpp_exception_as_java_exception(JNIEnv* env) {
  try {
    throw;  // throw exception to determine and handle type
  } catch (const std::bad_alloc& e) {
    jclass jc = env->FindClass("java/lang/OutOfMemoryError");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (const std::ios_base::failure& e) {
    jclass jc = env->FindClass("java/io/IOException");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (const std::exception& e) {
    jclass jc = env->FindClass("java/lang/Exception");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (...) {
    jclass jc = env->FindClass("java/lang/Exception");
    if (jc) env->ThrowNew(jc, "Unidentfied C++ exception");
  }
}

void setDaemonConnection(JNIEnv *env, MoneroWallet* wallet, jstring juri, jstring jusername, jstring jpassword) {

  // collect and release string params
  const char* _uri = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  const char* _username = jusername ? env->GetStringUTFChars(jusername, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string uri = string(juri ? _uri : "");
  string username = string(_username ? _username : "");
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(juri, _uri);
  env->ReleaseStringUTFChars(jusername, _username);
  env->ReleaseStringUTFChars(jpassword, _password);

  // set daemon connection
  try {
    wallet->setDaemonConnection(uri, username, password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

// ------------------------------- JNI STATIC ---------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring jpath) {
  cout << "Java_monero_wallet_MoneroWalletJni_walletExistsJni" << endl;
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  string path = string(_path);
  env->ReleaseStringUTFChars(jpath, _path);
  bool walletExists = MoneroWallet::walletExists(path);
  return static_cast<jboolean>(walletExists);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType) {
  cout << "Java_monero_wallet_MoneroWalletJni_openWalletJni" << endl;
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  const char* _password = env->GetStringUTFChars(jpassword, NULL);
  string path = string(_path);
  string password = string(_password);
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);

  // load wallet from file
  try {
    MoneroWallet* wallet = new MoneroWallet(path, password, static_cast<MoneroNetworkType>(jnetworkType));
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType, jstring jdaemonUri, jstring jdaemonUsername, jstring jdaemonPassword, jstring jlanguage) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletRandomJni" << endl;

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _daemonUri = jdaemonUri ? env->GetStringUTFChars(jdaemonUri, NULL) : nullptr;
  const char* _daemonUsername = jdaemonUsername ? env->GetStringUTFChars(jdaemonUsername, NULL) : nullptr;
  const char* _daemonPassword = jdaemonPassword ? env->GetStringUTFChars(jdaemonPassword, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string language = string(_language ? _language : "");
  string daemonUri = string(_daemonUri ? _daemonUri : "");
  string daemonUsername = string(_daemonUsername ? _daemonUsername : "");
  string daemonPassword = string(_daemonPassword ? _daemonPassword : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jdaemonUri, _daemonUri);
  env->ReleaseStringUTFChars(jdaemonUsername, _daemonUsername);
  env->ReleaseStringUTFChars(jdaemonPassword, _daemonPassword);
  env->ReleaseStringUTFChars(jlanguage, _language);

  // construct wallet
  try {
    MoneroRpcConnection daemonConnection = MoneroRpcConnection(daemonUri, daemonUsername, daemonPassword);
    MoneroWallet* wallet = new MoneroWallet(path, password, static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, language);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

// TODO: update this impl and others like it to be like e.g. createWalletFromKeysJni
JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jstring jmnemonic, jint jnetworkType, jlong jrestoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni" << endl;

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _mnemonic = env->GetStringUTFChars(jmnemonic, NULL);
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string mnemonic = string(_mnemonic ? _mnemonic : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jmnemonic, _mnemonic);

  // construct wallet
  try {
    MoneroRpcConnection daemonConnection;
    MoneroWallet* wallet = new MoneroWallet(path, password, mnemonic, static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, (uint64_t) jrestoreHeight);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jstring jaddress, jstring jviewKey, jstring jspendKey, jint networkType, jlong restoreHeight, jstring jlanguage) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni" << endl;

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _viewKey = jviewKey ? env->GetStringUTFChars(jviewKey, NULL) : nullptr;
  const char* _spendKey = jspendKey ? env->GetStringUTFChars(jspendKey, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string address = string(_address ? _address : "");
  string viewKey = string(_viewKey ? _viewKey : "");
  string spendKey = string(_spendKey ? _spendKey : "");
  string language = string(_language ? _language : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jviewKey, _viewKey);
  env->ReleaseStringUTFChars(jspendKey, _spendKey);
  env->ReleaseStringUTFChars(jlanguage, _language);

  // construct wallet and return reference
  try {
    MoneroRpcConnection daemonConnection; // TODO: take daemon connection parameters
    MoneroWallet* wallet = new MoneroWallet(path, password, address, viewKey, spendKey, static_cast<MoneroNetworkType>(networkType), daemonConnection, restoreHeight, language);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

//  ------------------------------- JNI INSTANCE ------------------------------

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni()" << endl;

  // get wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get daemon connection
  try {
    shared_ptr<MoneroRpcConnection> daemonConnection = wallet->getDaemonConnection();
    if (daemonConnection == nullptr) return 0;

    // return string[uri, username, password]
    jobjectArray vals = env->NewObjectArray(3, env->FindClass("java/lang/String"), nullptr);
    if (!daemonConnection->uri.empty()) env->SetObjectArrayElement(vals, 0, env->NewStringUTF(daemonConnection->uri.c_str()));
    if (!daemonConnection->username.empty()) env->SetObjectArrayElement(vals, 1, env->NewStringUTF(daemonConnection->username.c_str()));
    if (!daemonConnection->password.empty()) env->SetObjectArrayElement(vals, 2, env->NewStringUTF(daemonConnection->password.c_str()));
    return vals;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring juri, jstring jusername, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    setDaemonConnection(env, wallet, juri, jusername, jpassword);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_getIsConnectedJni(JNIEnv* env, jobject instance) {
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return static_cast<jboolean>(wallet->getIsConnected());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonHeightJni(JNIEnv* env, jobject instance) {
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->getDaemonHeight();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonTargetHeightJni(JNIEnv* env, jobject instance) {
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->getDaemonTargetHeight();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_getIsDaemonSyncedJni(JNIEnv* env, jobject instance) {
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->getIsDaemonSynced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_getIsSyncedJni(JNIEnv* env, jobject instance) {
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->getIsSynced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPathJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getPath().c_str());
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->getNetworkType();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getMnemonicJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getMnemonic().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguageJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getLanguage().c_str());
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getLanguagesJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get languages
  vector<string> languages;
  try {
    languages = wallet->getLanguages();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // build and java string array
  jobjectArray jlanguages = env->NewObjectArray(languages.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < languages.size(); i++) {
    env->SetObjectArrayElement(jlanguages, i, env->NewStringUTF(languages[i].c_str()));
  }
  return jlanguages;
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getPublicViewKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getPrivateViewKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicSpendKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPublicSpendKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getPublicSpendKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateSpendKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPrivateSpendKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->getPrivateSpendKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  string address = wallet->getAddress((uint32_t) accountIdx, (uint32_t) subaddressIdx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressIndexJni(JNIEnv *env, jobject instance, jstring jaddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressIndexJni" << endl;

  // collect and release string param
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  string address = string(_address ? _address : "");
  env->ReleaseStringUTFChars(jaddress, _address);

  // get indices of addresse's subaddress
  try {
    MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
    MoneroSubaddress subaddress = wallet->getAddressIndex(address);
    string subaddressJson = subaddress.serialize();
    return env->NewStringUTF(subaddressJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  cout << "Java_monero_wallet_MoneroWalletJni_setListenerJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // clear old listener
  wallet->setListener(boost::none);
  WalletJniListener *oldListener = getHandle<WalletJniListener>(env, instance, JNI_LISTENER_HANDLE);
  if (oldListener != nullptr) {
    oldListener->deleteGlobalJavaRef(env);
    delete oldListener;
  }

  // set new listener
  if (jlistener == nullptr) {
    return 0;
  } else {
    WalletJniListener* listener = new WalletJniListener(env, jlistener);
    MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
    wallet->setListener(*listener);
    return reinterpret_cast<jlong>(listener);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jstandardAddress, jstring jpaymentId) {
  cout << "Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // collect and release string params
  const char* _standardAddress = jstandardAddress ? env->GetStringUTFChars(jstandardAddress, NULL) : nullptr;
  const char* _paymentId = jpaymentId ? env->GetStringUTFChars(jpaymentId, NULL) : nullptr;
  string standardAddress = string(_standardAddress ? _standardAddress : "");
  string paymentId = string(_paymentId ? _paymentId : "");
  env->ReleaseStringUTFChars(jstandardAddress, _standardAddress);
  env->ReleaseStringUTFChars(jpaymentId, _paymentId);

  // get and serialize integrated address
  try {
    MoneroIntegratedAddress integratedAddress = wallet->getIntegratedAddress(standardAddress, paymentId);
    string integratedAddressJson = integratedAddress.serialize();
    return env->NewStringUTF(integratedAddressJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jintegratedAddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _integratedAddress = jintegratedAddress ? env->GetStringUTFChars(jintegratedAddress, NULL) : nullptr;
  string integratedAddress = string(_integratedAddress ? _integratedAddress : "");
  env->ReleaseStringUTFChars(jintegratedAddress, _integratedAddress);

  // serialize and return decoded integrated address
  try {
    MoneroIntegratedAddress integratedAddress = wallet->decodeIntegratedAddress(string(_integratedAddress ? _integratedAddress : ""));
    string integratedAddressJson = integratedAddress.serialize();
    return env->NewStringUTF(integratedAddressJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jlong startHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_syncJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {

    // sync wallet
    MoneroSyncResult result = wallet->sync(startHeight);

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
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setAutoSyncJni(JNIEnv *env, jobject instance, jboolean autoSync) {
  cout << "Java_monero_wallet_MoneroWalletJni_setAutoSyncJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->setAutoSync(autoSync);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_rescanBlockchainJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_rescanBlockchainJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->rescanBlockchain();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

// isMultisigImportNeeded

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->getHeight();
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getChainHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getChainHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->getChainHeight();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->getRestoreHeight();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setRestoreHeightJni(JNIEnv *env, jobject instance, jlong restoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_setRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->setRestoreHeight(restoreHeight);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getUnlockedBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getUnlockedBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->getUnlockedBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountsJni(JNIEnv* env, jobject instance, jboolean includeSubaddresses, jstring jtag) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tag = jtag ? env->GetStringUTFChars(jtag, NULL) : nullptr;
  string tag = string(_tag ? _tag : "");
  env->ReleaseStringUTFChars(jtag, _tag);

  // get accounts
  vector<MoneroAccount> accounts = wallet->getAccounts(includeSubaddresses, tag);

  // wrap and serialize accounts
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!accounts.empty()) container.add_child("accounts", MoneroUtils::toPropertyTree(accounts));
  boost::property_tree::write_json(ss, container, false);
  string accountsJson = ss.str();
  return env->NewStringUTF(accountsJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountJni(JNIEnv* env, jobject instance, jint accountIdx, jboolean includeSubaddresses) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get account
  MoneroAccount account = wallet->getAccount(accountIdx, includeSubaddresses);

  // serialize and returna account
  string accountJson = account.serialize();
  return env->NewStringUTF(accountJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createAccountJni(JNIEnv* env, jobject instance, jstring jlabel) {
  cout << "Java_monero_wallet_MoneroWalletJni_createAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  string label = string(_label ? _label : "");
  env->ReleaseStringUTFChars(jlabel, _label);

  // create account
  MoneroAccount account = wallet->createAccount(label);

  // serialize and return account
  string accountJson = account.serialize();
  return env->NewStringUTF(accountJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSubaddressesJni(JNIEnv* env, jobject instance, jint accountIdx, jintArray jsubaddressIndices) {
  cout << "Java_monero_wallet_MoneroWalletJni_getSubaddressesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // convert subaddress indices from jintArray to vector<uint32_t>
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

  // wrap and serialize subaddresses
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!subaddresses.empty()) container.add_child("subaddresses", MoneroUtils::toPropertyTree(subaddresses));
  boost::property_tree::write_json(ss, container, false);
  string subaddressesJson = ss.str();
  return env->NewStringUTF(subaddressesJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createSubaddressJni(JNIEnv* env, jobject instance, jint accountIdx, jstring jlabel) {
  cout << "Java_monero_wallet_MoneroWalletJni_createSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  string label = string(_label ? _label : "");
  env->ReleaseStringUTFChars(jlabel, _label);

  // create subaddress
  MoneroSubaddress subaddress = wallet->createSubaddress(accountIdx, label);

  // serialize and return subaddress
  string subaddressJson = subaddress.serialize();
  return env->NewStringUTF(subaddressJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxsJni(JNIEnv* env, jobject instance, jstring jtxRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txRequest = jtxRequest ? env->GetStringUTFChars(jtxRequest, NULL) : nullptr;
  string txRequestJson = string(_txRequest ? _txRequest : "");
  env->ReleaseStringUTFChars(jtxRequest, _txRequest);
  try {

    // deserialize tx request
    shared_ptr<MoneroTxRequest> txRequest = MoneroUtils::deserializeTxRequest(txRequestJson);
    cout << "Fetching txs with request: " << txRequest->serialize() << endl;

    // get txs
    vector<shared_ptr<MoneroTxWallet>> txs = wallet->getTxs(*txRequest);
    cout << "Got " << txs.size() << " txs" << endl;

    // return unique blocks to preserve model relationships as tree
    shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<MoneroBlock>> blocks;
    unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
    for (const shared_ptr<MoneroTxWallet>& tx : txs) {
      if (tx->block == boost::none) {
        if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
        tx->block = unconfirmedBlock;
        unconfirmedBlock->txs.push_back(tx);
      }
      unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(tx->block.get());
      if (got == seenBlockPtrs.end()) {
        seenBlockPtrs.insert(tx->block.get());
        blocks.push_back(tx->block.get());
      }
    }
    cout << "Returning " << blocks.size() << " blocks" << endl;

    // wrap and serialize blocks
    std::stringstream ss;
    boost::property_tree::ptree container;
    if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
    boost::property_tree::write_json(ss, container, false);
    string blocksJson = ss.str();
    return env->NewStringUTF(blocksJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTransfersJni(JNIEnv* env, jobject instance, jstring jtransferRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTransfersJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _transferRequest = jtransferRequest ? env->GetStringUTFChars(jtransferRequest, NULL) : nullptr;
  string transferRequestJson = string(_transferRequest ? _transferRequest : "");
  env->ReleaseStringUTFChars(jtransferRequest, _transferRequest);
  try {

    // deserialize transfer request
    shared_ptr<MoneroTransferRequest> transferRequest = MoneroUtils::deserializeTransferRequest(transferRequestJson);
    cout << "Fetching transfers with request: " << transferRequest->serialize() << endl;

    // get transfers
    vector<shared_ptr<MoneroTransfer>> transfers = wallet->getTransfers(*transferRequest);
    cout << "Got " << transfers.size() << " transfers" << endl;

    // return unique blocks to preserve model relationships as tree
    shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<MoneroBlock>> blocks;
    unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
    for (auto const& transfer : transfers) {
      shared_ptr<MoneroTxWallet> tx = transfer->tx;
      if (tx->block == boost::none) {
        if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
        tx->block = unconfirmedBlock;
        unconfirmedBlock->txs.push_back(tx);
      }
      unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(tx->block.get());
      if (got == seenBlockPtrs.end()) {
        seenBlockPtrs.insert(tx->block.get());
        blocks.push_back(tx->block.get());
      }
    }

    // wrap and serialize blocks
    std::stringstream ss;
    boost::property_tree::ptree container;
    if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
    boost::property_tree::write_json(ss, container, false);
    string blocksJson = ss.str();
    return env->NewStringUTF(blocksJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsJni(JNIEnv* env, jobject instance, jstring joutputRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _outputRequest = joutputRequest ? env->GetStringUTFChars(joutputRequest, NULL) : nullptr;
  string outputRequestJson = string(_outputRequest ? _outputRequest : "");
  env->ReleaseStringUTFChars(joutputRequest, _outputRequest);
  try {

    // deserialize output request
    shared_ptr<MoneroOutputRequest> outputRequest = MoneroUtils::deserializeOutputRequest(outputRequestJson);
    cout << "Fetching outputs with request: " << outputRequest->serialize() << endl;

    // get outputs
    vector<shared_ptr<MoneroOutputWallet>> outputs = wallet->getOutputs(*outputRequest);
    cout << "Got " << outputs.size() << " outputs" << endl;

    // return unique blocks to preserve model relationships as tree
    vector<MoneroBlock> blocks;
    unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
    for (auto const& output : outputs) {
      shared_ptr<MoneroTxWallet> tx = static_pointer_cast<MoneroTxWallet>(output->tx);
      if (tx->block == boost::none) throw runtime_error("Need to handle unconfirmed output");
      unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(*tx->block);
      if (got == seenBlockPtrs.end()) {
        seenBlockPtrs.insert(*tx->block);
        blocks.push_back(**tx->block);
      }
    }
    cout << "Returning " << blocks.size() << " blocks" << endl;

    // wrap and serialize blocks
    std::stringstream ss;
    boost::property_tree::ptree container;
    if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
    boost::property_tree::write_json(ss, container, false);
    string blocksJson = ss.str();
    return env->NewStringUTF(blocksJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsHexJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->getOutputsHex().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importOutputsHexJni(JNIEnv* env, jobject instance, jstring joutputsHex) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _outputsHex = joutputsHex ? env->GetStringUTFChars(joutputsHex, NULL) : nullptr;
  string outputsHex = string(_outputsHex ? _outputsHex : "");
  env->ReleaseStringUTFChars(joutputsHex, _outputsHex);
  try {
    return wallet->importOutputsHex(outputsHex);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getKeyImagesJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getKeyImagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // fetch key images
  vector<shared_ptr<MoneroKeyImage>> keyImages = wallet->getKeyImages();
  cout << "Fetched " << keyImages.size() << " key images" << endl;

  // wrap and serialize key images
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!keyImages.empty()) container.add_child("keyImages", MoneroUtils::toPropertyTree(keyImages));
  boost::property_tree::write_json(ss, container, false);
  string keyImagesJson = ss.str();
  return env->NewStringUTF(keyImagesJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_importKeyImagesJni(JNIEnv* env, jobject instance, jstring jkeyImagesJson) {
  cout << "Java_monero_wallet_MoneroWalletJni_importKeyImagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _keyImagesJson = jkeyImagesJson ? env->GetStringUTFChars(jkeyImagesJson, NULL) : nullptr;
  string keyImagesJsonn = string(_keyImagesJson ? _keyImagesJson : "");
  env->ReleaseStringUTFChars(jkeyImagesJson, _keyImagesJson);

  // deserialize key images to import
  vector<shared_ptr<MoneroKeyImage>> keyImages = MoneroUtils::deserializeKeyImages(keyImagesJsonn);
  cout << "Deserialized " << keyImages.size() << " key images from java json" << endl;

  // import key images
  shared_ptr<MoneroKeyImageImportResult> result;
  try {
    result = wallet->importKeyImages(keyImages);
    return env->NewStringUTF(result->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sendSplitJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_sendSplitJni(request)" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;
  string sendRequestJson = string(_sendRequest ? _sendRequest : "");
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);

  // deserialize send request
  shared_ptr<MoneroSendRequest> sendRequest = MoneroUtils::deserializeSendRequest(sendRequestJson);
  cout << "Deserialized send request, re-serialized: " << sendRequest->serialize() << endl;

  // submit send request
  vector<shared_ptr<MoneroTxWallet>> txs;
  try {
    txs = wallet->sendSplit(*sendRequest);
    cout << "Got " << txs.size() << " txs" << endl;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // return unique blocks to preserve model relationships as tree
  shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
  vector<MoneroBlock> blocks;
  unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
  for (auto const& tx : txs) {
    if (tx->block == boost::none) {
      if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
      tx->block = unconfirmedBlock;
      unconfirmedBlock->txs.push_back(tx);
    }
    unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(*tx->block);
    if (got == seenBlockPtrs.end()) {
      seenBlockPtrs.insert(*tx->block);
      blocks.push_back(**tx->block);
    }
  }
  cout << "Returning " << blocks.size() << " blocks" << endl;

  // wrap and serialize blocks
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepOutputJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_sweepOutputJni(request)" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;
  string sendRequestJson = string(_sendRequest);
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);

  cout << "Send request json: " << sendRequestJson << endl;

  // deserialize send request
  shared_ptr<MoneroSendRequest> sendRequest = MoneroUtils::deserializeSendRequest(sendRequestJson);
  cout << "Deserialized send request, re-serialized: " << sendRequest->serialize() << endl;

  // submit send request
  shared_ptr<MoneroTxWallet> tx;
  try {
    tx = wallet->sweepOutput(*sendRequest);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // wrap and serialize blocks to preserve model relationships as tree
  MoneroBlock block;
  block.txs.push_back(tx);
  vector<MoneroBlock> blocks;
  blocks.push_back(block);
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepDustJni(JNIEnv* env, jobject instance, jboolean doNotRelay) {
  cout << "Java_monero_wallet_MoneroWalletJni_sweepDustJni(request)" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // sweep dust
  vector<shared_ptr<MoneroTxWallet>> txs;
  try {
    txs = wallet->sweepDust(doNotRelay);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // wrap and serialize blocks to preserve model relationships as tree
  vector<MoneroBlock> blocks;
  if (!txs.empty()) {
    MoneroBlock block;
    for (const auto& tx : txs) block.txs.push_back(tx);
    blocks.push_back(block);
  }
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_relayTxsJni(JNIEnv* env, jobject instance, jobjectArray jtxMetadatas) {
  cout << "Java_monero_wallet_MoneroWalletJni_relayTxsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx metadatas from jobjectArray to vector<string>
  vector<string> txMetadatas;
  if (jtxMetadatas != nullptr) {
    jsize size = env->GetArrayLength(jtxMetadatas);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxMetadatas, idx);
      txMetadatas.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // relay tx metadata
  vector<string> txIds;
  try {
    txIds = wallet->relayTxs(txMetadatas);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // convert and return tx ids as jobjectArray
  jobjectArray jtxIds = env->NewObjectArray(txIds.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < txIds.size(); i++) {
    env->SetObjectArrayElement(jtxIds, i, env->NewStringUTF(txIds[i].c_str()));
  }
  return jtxIds;
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtxIds) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxNotesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx ids from jobjectArray to vector<string>
  vector<string> txIds;
  if (jtxIds != nullptr) {
    jsize size = env->GetArrayLength(jtxIds);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxIds, idx);
      txIds.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // get tx notes
  vector<string> txNotes;
  try {
    txNotes = wallet->getTxNotes(txIds);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // convert and return tx notes as jobjectArray
  jobjectArray jtxNotes = env->NewObjectArray(txNotes.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < txNotes.size(); i++) {
    env->SetObjectArrayElement(jtxNotes, i, env->NewStringUTF(txNotes[i].c_str()));
  }
  return jtxNotes;
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtxIds, jobjectArray jtxNotes) {
  cout << "Java_monero_wallet_MoneroWalletJni_setTxNotesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx ids from jobjectArray to vector<string>
  vector<string> txIds;
  if (jtxIds != nullptr) {
    jsize size = env->GetArrayLength(jtxIds);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxIds, idx);
      txIds.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // get tx notes from jobjectArray to vector<string>
  vector<string> txNotes;
  if (jtxNotes != nullptr) {
    jsize size = env->GetArrayLength(jtxNotes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxNotes, idx);
      txNotes.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // set tx notes
  try {
    wallet->setTxNotes(txIds, txNotes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signJni(JNIEnv* env, jobject instance, jstring jmsg) {
  cout << "Java_monero_wallet_MoneroWalletJni_signJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _msg = jmsg ? env->GetStringUTFChars(jmsg, NULL) : nullptr;
  string msg = string(_msg ? _msg : "");
  env->ReleaseStringUTFChars(jmsg, _msg);
  try {
    string signature = wallet->sign(msg);
    return env->NewStringUTF(signature.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_verifyJni(JNIEnv* env, jobject instance, jstring jmsg, jstring jaddress, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_verifyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _msg = jmsg ? env->GetStringUTFChars(jmsg, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string msg = string(_msg ? _msg : "");
  string address = string(_address ? _address : "");
  string signature = string(_signature ? _signature : "");
  env->ReleaseStringUTFChars(jmsg, _msg);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    bool isGood = wallet->verify(msg, address, signature);
    return static_cast<jboolean>(isGood);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxKeyJni(JNIEnv* env, jobject instance, jstring jtxId) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxKeyJniJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  env->ReleaseStringUTFChars(jtxId, _txId);
  try {
    return env->NewStringUTF(wallet->getTxKey(txId).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxKeyJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jtxKey, jstring jaddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkTxKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _txKey = jtxKey ? env->GetStringUTFChars(jtxKey, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string txKey = string(_txKey == nullptr ? "" : _txKey);
  string address = string(_address == nullptr ? "" : _address);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jtxKey, _txKey);
  env->ReleaseStringUTFChars(jaddress, _address);
  try {
    return env->NewStringUTF(wallet->checkTxKey(txId, txKey, address)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jaddress, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getTxProof(txId, address, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jaddress, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkTxProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return env->NewStringUTF(wallet->checkTxProof(txId, address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSpendProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getSpendProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getSpendProof(txId, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_checkSpendProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkSpendProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return static_cast<jboolean>(wallet->checkSpendProof(txId, message, signature));
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni(JNIEnv* env, jobject instance, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getReserveProofWallet(message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofAccountJni(JNIEnv* env, jobject instance, jint accountIdx, jstring jamountStr, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _amountStr = jamountStr ? env->GetStringUTFChars(jamountStr, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string amountStr = string(_amountStr == nullptr ? "" : _amountStr);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jamountStr, _amountStr);
  env->ReleaseStringUTFChars(jmessage, _message);
  uint64_t amount = boost::lexical_cast<uint64_t>(amountStr);
  try {
    return env->NewStringUTF(wallet->getReserveProofAccount(accountIdx, amount, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkReserveProofJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkReserveProofAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return env->NewStringUTF(wallet->checkReserveProof(address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createPaymentUriJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_createPaymentUriJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;
  string sendRequestJson = string(_sendRequest ? _sendRequest : "");
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);

  // deserialize send request
  shared_ptr<MoneroSendRequest> sendRequest = MoneroUtils::deserializeSendRequest(sendRequestJson);
  cout << "Fetching payment uri with : " << sendRequest->serialize() << endl;

  // get payment uri
  string paymentUri;
  try {
    paymentUri = wallet->createPaymentUri(*sendRequest.get());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return
  return env->NewStringUTF(paymentUri.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni(JNIEnv* env, jobject instance, jstring juri) {
  cout << "Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _uri = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  string uri = string(_uri ? _uri : "");
  env->ReleaseStringUTFChars(juri, _uri);

  // parse uri to send request
  shared_ptr<MoneroSendRequest> sendRequest;
  try {
    sendRequest = wallet->parsePaymentUri(uri);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return serialized request
  return env->NewStringUTF(sendRequest->serialize().c_str());
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setAttributeJni(JNIEnv* env, jobject instance, jstring jkey, jstring jval) {
  cout << "Java_monero_wallet_MoneroWalletJni_setAttribute()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  const char* _val = jval ? env->GetStringUTFChars(jval, NULL) : nullptr;
  string key = string(_key);
  string val = string(_val);
  env->ReleaseStringUTFChars(jkey, _key);
  env->ReleaseStringUTFChars(jval, _val);
  try {
    wallet->setAttribute(key, val);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAttributeJni(JNIEnv* env, jobject instance, jstring jkey) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAttribute()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  string key = string(_key);
  env->ReleaseStringUTFChars(jkey, _key);
  try {
    return env->NewStringUTF(wallet->getAttribute(key).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startMiningJni(JNIEnv* env, jobject instance, jlong numThreads, jboolean backgroundMining, jboolean ignoreBattery) {
  cout << "Java_monero_wallet_MoneroWalletJni_startMiningJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->startMining(numThreads, backgroundMining, ignoreBattery);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopMiningJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_startMiningJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->stopMining();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_saveJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_saveJni(path, password)" << endl;

  // save wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->save();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_moveToJni(JNIEnv* env, jobject instance, jstring jpath, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_moveToJni(path, password)" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpath ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);

  // move wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->moveTo(path, password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_closeJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_CloseJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, JNI_WALLET_HANDLE);
  delete wallet;
  wallet = nullptr;
}

#ifdef __cplusplus
}
#endif
