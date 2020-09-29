/**
 * Copyright (c) 2017-2020 woodser
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
 *
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
#include "chacha.h" // TODO: explicitly include because wallet2.h #include "crypto/chacha.h" is ignored
#include "monero_wallet_jni_bridge.h"
#include "wallet/monero_wallet_core.h"
#include "utils/monero_utils.h"

using namespace std;
using namespace monero;

// initialize names of private instance variables used in Java JNI wallet which contain memory references to native wallet and listener
static const char* JNI_WALLET_HANDLE = "jniWalletHandle";
static const char* JNI_LISTENER_HANDLE = "jniListenerHandle";

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

void rethrow_java_exception_as_cpp_exception(JNIEnv* env, jthrowable jexception) {

  // print stacktrace to the console
  env->ExceptionDescribe();

  // clear the exception
  //env->ExceptionClear();

  // get the exception's message
  jclass throwableClass = env->FindClass("java/lang/Throwable");
  jmethodID throwableClass_getMessage = env->GetMethodID(throwableClass, "getMessage", "()Ljava/lang/String;");
  jstring jmsg = (jstring) env->CallObjectMethod(jexception, throwableClass_getMessage);
  const char* _msg = jmsg == 0 ? 0 : env->GetStringUTFChars(jmsg, NULL);
  string msg = string(_msg == 0 ? "" : _msg);
  env->ReleaseStringUTFChars(jmsg, _msg);

  // throw exception in c++
  MERROR("Exception occurred in Java: " << msg);
  throw runtime_error(msg);
}

void set_daemon_connection(JNIEnv *env, monero_wallet* wallet, jstring juri, jstring jusername, jstring jpassword) {

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
    wallet->set_daemon_connection(uri, username, password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

string strip_last_char(const string& str) {
  return str.substr(0, str.size() - 1);
}

// ---------------------------- WALLET LISTENER -------------------------------

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
  JNIEnv *env;
  if (jvm->GetEnv(reinterpret_cast<void **>(&env), JNI_VERSION_1_6) != JNI_OK) {
    return -1;
  }

//  class_ArrayList = static_cast<jclass>(env->NewGlobalRef(env->FindClass("java/util/ArrayList")));
//  class_TransactionInfo = static_cast<jclass>(env->NewGlobalRef(env->FindClass("com/m2049r/xmrwallet/model/TransactionInfo")));
//  class_Transfer = static_cast<jclass>(env->NewGlobalRef(env->FindClass("com/m2049r/xmrwallet/model/Transfer")));
  class_WalletListener = static_cast<jclass>(env->NewGlobalRef(env->FindClass("monero/wallet/MoneroWalletJni$WalletJniListener")));
//  class_Ledger = static_cast<jclass>(env->NewGlobalRef(env->FindClass("com/m2049r/xmrwallet/ledger/Ledger")));
  return JNI_VERSION_1_6;
}
#ifdef __cplusplus
}
#endif

int attachJVM(JNIEnv **env) {
  int envStat = cachedJVM->GetEnv((void **) env, JNI_VERSION_1_6);
  if (envStat == JNI_EDETACHED) {
    if (cachedJVM->AttachCurrentThread((void **) env, nullptr) != 0) {
      return JNI_ERR;
    }
  } else if (envStat == JNI_EVERSION) {
    return JNI_ERR;
  }
  return envStat;
}

void detachJVM(JNIEnv *env, int envStat) {
  if (env->ExceptionCheck()) {
    env->ExceptionDescribe();
  }
  if (envStat == JNI_EDETACHED) {
    cachedJVM->DetachCurrentThread();
  }
}

/**
 * Listens for wallet notifications and notifies the listener in Java.
 */
struct wallet_jni_listener : public monero_wallet_listener {

  jobject jlistener;
  JNIEnv* m_env;

  // TODO: use this env instead of attaching each time? performance improvement?
  wallet_jni_listener(JNIEnv* env, jobject listener) {
    jlistener = env->NewGlobalRef(listener);
    m_env = env;
  }

  ~wallet_jni_listener() {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    m_env->DeleteGlobalRef(jlistener);
    jlistener = nullptr;
  };

  void on_sync_progress(uint64_t height, uint64_t start_height, uint64_t end_height, double percent_done, const string& message) override {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *env;
    int envStat = attachJVM(&env);	// TODO: necessary to attach every time?
    if (envStat == JNI_ERR) return;

    // prepare callback arguments
    jlong jheight = static_cast<jlong>(height);
    jlong jstart_height = static_cast<jlong>(start_height);
    jlong jend_height = static_cast<jlong>(end_height);
    jdouble jpercent_done = static_cast<jdouble>(percent_done);
    jstring jmessage = env->NewStringUTF(message.c_str());

    // invoke Java listener's onSyncProgress()
    jmethodID listenerClass_onSyncProgress = env->GetMethodID(class_WalletListener, "onSyncProgress", "(JJJDLjava/lang/String;)V");
    env->CallVoidMethod(jlistener, listenerClass_onSyncProgress, jheight, jstart_height, jend_height, jpercent_done, jmessage);
    env->DeleteLocalRef(jmessage);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception);  // TODO: does not detach JVM

    detachJVM(env, envStat);
  }

  void on_new_block(uint64_t height) override {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *env;
    int envStat = attachJVM(&env);
    if (envStat == JNI_ERR) return;

    // invoke Java listener's onNewBlock()
    jlong jheight = static_cast<jlong>(height);
    jmethodID listenerClass_onNewBlock = env->GetMethodID(class_WalletListener, "onNewBlock", "(J)V");
    env->CallVoidMethod(jlistener, listenerClass_onNewBlock, jheight);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception);  // TODO: does not detach JVM

    detachJVM(env, envStat);
  }

  void on_balances_changed(uint64_t new_balance, uint64_t new_unlocked_balance) override {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *env;
    int envStat = attachJVM(&env);
    if (envStat == JNI_ERR) return;

    // invoke Java listener's onBalancesChanged()
    jstring jbalance_str = env->NewStringUTF(to_string(new_balance).c_str());
    jstring junlocked_balance_str = env->NewStringUTF(to_string(new_unlocked_balance).c_str());
    jmethodID listenerClass_onBalanceChanged = env->GetMethodID(class_WalletListener, "onBalancesChanged", "(Ljava/lang/String;Ljava/lang/String;)V");
    env->CallVoidMethod(jlistener, listenerClass_onBalanceChanged, jbalance_str, junlocked_balance_str);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception);  // TODO: does not detach JVM

    detachJVM(env, envStat);
  }

  void on_output_received(const monero_output_wallet& output) override {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *env;
    int envStat = attachJVM(&env);
    if (envStat == JNI_ERR) return;

    // prepare parameters to invoke Java listener
    boost::optional<uint64_t> height = output.m_tx->get_height();
    jstring jtx_hash = env->NewStringUTF(output.m_tx->m_hash.get().c_str());
    jstring jamount_str = env->NewStringUTF(to_string(*output.m_amount).c_str());
    int version = output.m_tx->m_version == boost::none ? 1 : *output.m_tx->m_version;  // TODO: version not present in unlocked output notification, defaulting to 1
    bool is_locked = std::static_pointer_cast<monero_tx_wallet>(output.m_tx)->m_is_locked.get();

    // invoke Java listener's onOutputReceived()
    jmethodID listenerClass_onOutputReceived = env->GetMethodID(class_WalletListener, "onOutputReceived", "(JLjava/lang/String;Ljava/lang/String;IIIJZ)V");
    env->CallVoidMethod(jlistener, listenerClass_onOutputReceived, height == boost::none ? 0 : *height, jtx_hash, jamount_str, *output.m_account_index, *output.m_subaddress_index, version, *output.m_tx->m_unlock_height, is_locked);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception);  // TODO: does not detach JVM

    detachJVM(env, envStat);
  }

  void on_output_spent(const monero_output_wallet& output) override {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *env;
    int envStat = attachJVM(&env);
    if (envStat == JNI_ERR) return;

    // prepare parameters to invoke Java listener
    boost::optional<uint64_t> height = output.m_tx->get_height();
    jstring jtx_hash = env->NewStringUTF(output.m_tx->m_hash.get().c_str());
    jstring jamount_str = env->NewStringUTF(to_string(*output.m_amount).c_str());

    // invoke Java listener's onOutputSpent()
    jmethodID listenerClass_onOutputSpent = env->GetMethodID(class_WalletListener, "onOutputSpent", "(JLjava/lang/String;Ljava/lang/String;III)V");
    env->CallVoidMethod(jlistener, listenerClass_onOutputSpent, height == boost::none ? 0 : *height, jtx_hash, jamount_str, *output.m_account_index, output.m_subaddress_index, *output.m_tx->m_version);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception);  // TODO: does not detach JVM

    detachJVM(env, envStat);
  }
};

// ------------------------------- JNI STATIC ---------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring jpath) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_walletExistsJni");
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  string path = string(_path);
  env->ReleaseStringUTFChars(jpath, _path);
  bool wallet_exists = monero_wallet_core::wallet_exists(path);
  return static_cast<jboolean>(wallet_exists);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetwork_type) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_openWalletJni");
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  const char* _password = env->GetStringUTFChars(jpassword, NULL);
  string path = string(_path);
  string password = string(_password);
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);

  // load wallet from file
  try {
    monero_wallet* wallet = monero_wallet_core::open_wallet(path, password, static_cast<monero_network_type>(jnetwork_type));
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetwork_type, jstring jdaemon_uri, jstring jdaemon_username, jstring jdaemon_password, jstring jlanguage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createWalletRandomJni");

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _daemonUri = jdaemon_uri ? env->GetStringUTFChars(jdaemon_uri, NULL) : nullptr;
  const char* _daemonUsername = jdaemon_username ? env->GetStringUTFChars(jdaemon_username, NULL) : nullptr;
  const char* _daemonPassword = jdaemon_password ? env->GetStringUTFChars(jdaemon_password, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string language = string(_language ? _language : "");
  string daemon_uri = string(_daemonUri ? _daemonUri : "");
  string daemon_username = string(_daemonUsername ? _daemonUsername : "");
  string daemon_password = string(_daemonPassword ? _daemonPassword : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jdaemon_uri, _daemonUri);
  env->ReleaseStringUTFChars(jdaemon_username, _daemonUsername);
  env->ReleaseStringUTFChars(jdaemon_password, _daemonPassword);
  env->ReleaseStringUTFChars(jlanguage, _language);

  // construct wallet
  try {
    monero_rpc_connection daemon_connection = monero_rpc_connection(daemon_uri, daemon_username, daemon_password);
    monero_wallet* wallet = monero_wallet_core::create_wallet_random(path, password, static_cast<monero_network_type>(jnetwork_type), daemon_connection, language);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetwork_type, jstring jmnemonic, jlong jrestore_height, jstring joffset) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni");

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _mnemonic = jmnemonic ? env->GetStringUTFChars(jmnemonic, NULL) : nullptr;
  const char* _offset = joffset ? env->GetStringUTFChars(joffset, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string mnemonic = string(_mnemonic ? _mnemonic : "");
  string offset = string(_offset ? _offset : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jmnemonic, _mnemonic);
  env->ReleaseStringUTFChars(joffset, _offset);

  // construct wallet
  try {
    monero_rpc_connection daemon_connection;
    monero_wallet* wallet = monero_wallet_core::create_wallet_from_mnemonic(path, password, static_cast<monero_network_type>(jnetwork_type), mnemonic, daemon_connection, (uint64_t) jrestore_height, offset);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint network_type, jstring jaddress, jstring jview_key, jstring jspend_key, jlong restore_height, jstring jlanguage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni");

  // collect and release string params
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _viewKey = jview_key ? env->GetStringUTFChars(jview_key, NULL) : nullptr;
  const char* _spendKey = jspend_key ? env->GetStringUTFChars(jspend_key, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  string address = string(_address ? _address : "");
  string view_key = string(_viewKey ? _viewKey : "");
  string spend_key = string(_spendKey ? _spendKey : "");
  string language = string(_language ? _language : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jview_key, _viewKey);
  env->ReleaseStringUTFChars(jspend_key, _spendKey);
  env->ReleaseStringUTFChars(jlanguage, _language);

  // construct wallet and return reference
  try {
    monero_rpc_connection daemon_connection; // TODO: take daemon connection parameters
    monero_wallet* wallet = monero_wallet_core::create_wallet_from_keys(path, password, static_cast<monero_network_type>(network_type), address, view_key, spend_key, daemon_connection, restore_height, language);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicLanguagesJni(JNIEnv *env, jclass clazz) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getLanguagesJni");

  // get languages
  vector<string> languages;
  try {
    languages = monero_wallet_core::get_mnemonic_languages();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // build java string array
  jobjectArray jlanguages = env->NewObjectArray(languages.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < languages.size(); i++) {
    env->SetObjectArrayElement(jlanguages, i, env->NewStringUTF(languages[i].c_str()));
  }
  return jlanguages;
}

//  ------------------------------- JNI INSTANCE ------------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isViewOnlyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_isViewOnlyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->is_view_only();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring juri, jstring jusername, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    set_daemon_connection(env, wallet, juri, jusername, jpassword);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni()");

  // get wallet
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get daemon connection
  try {
    boost::optional<monero_rpc_connection> daemon_connection = wallet->get_daemon_connection();
    if (daemon_connection == boost::none) return 0;

    // return string[uri, username, password]
    jobjectArray vals = env->NewObjectArray(3, env->FindClass("java/lang/String"), nullptr);
    if (daemon_connection->m_uri != boost::none && !daemon_connection->m_uri.get().empty()) env->SetObjectArrayElement(vals, 0, env->NewStringUTF(daemon_connection->m_uri.get().c_str()));
    if (daemon_connection->m_username != boost::none && !daemon_connection->m_username.get().empty()) env->SetObjectArrayElement(vals, 1, env->NewStringUTF(daemon_connection->m_username.get().c_str()));
    if (daemon_connection->m_password != boost::none && !daemon_connection->m_password.get().empty()) env->SetObjectArrayElement(vals, 2, env->NewStringUTF(daemon_connection->m_password.get().c_str()));
    return vals;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isConnectedJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return static_cast<jboolean>(wallet->is_connected());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isDaemonSyncedJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->is_daemon_synced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isSyncedJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->is_synced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getVersionJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getVersionJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_version().serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getPathJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_path().c_str());
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_network_type();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getMnemonicJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_mnemonic().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicLanguageJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getMnemonicLanguageJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_mnemonic_language().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_public_view_key().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_private_view_key().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicSpendKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getPublicSpendKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_public_spend_key().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateSpendKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getPrivateSpendKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_private_spend_key().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  string address = wallet->get_address((uint32_t) account_idx, (uint32_t) subaddress_idx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressIndexJni(JNIEnv *env, jobject instance, jstring jaddress) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAddressIndexJni");

  // collect and release string param
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  string address = string(_address ? _address : "");
  env->ReleaseStringUTFChars(jaddress, _address);

  // get indices of addresse's subaddress
  try {
    monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
    monero_subaddress subaddress = wallet->get_address_index(address);
    string subaddress_json = subaddress.serialize();
    return env->NewStringUTF(subaddress_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

/**
 * Only one listener needs to subscribe over JNI, so this removes the previously registered listener
 * and registers the new listener.
 */
JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_setListenerJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // remove old listener
  wallet_jni_listener* old_listener = get_handle<wallet_jni_listener>(env, instance, JNI_LISTENER_HANDLE);
  if (old_listener != nullptr) {
    wallet->remove_listener(*old_listener);
    delete old_listener;
  }

  // add new listener
  if (jlistener == nullptr) return 0;
  wallet_jni_listener* listener = new wallet_jni_listener(env, jlistener);
  wallet->add_listener(*listener);
  return reinterpret_cast<jlong>(listener);
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jstandard_address, jstring jpayment_id) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // collect and release string params
  const char* _standardAddress = jstandard_address ? env->GetStringUTFChars(jstandard_address, NULL) : nullptr;
  const char* _paymentId = jpayment_id ? env->GetStringUTFChars(jpayment_id, NULL) : nullptr;
  string standard_address = string(_standardAddress ? _standardAddress : "");
  string payment_id = string(_paymentId ? _paymentId : "");
  env->ReleaseStringUTFChars(jstandard_address, _standardAddress);
  env->ReleaseStringUTFChars(jpayment_id, _paymentId);

  // get and serialize integrated address
  try {
    monero_integrated_address integrated_address = wallet->get_integrated_address(standard_address, payment_id);
    string integrated_address_json = integrated_address.serialize();
    return env->NewStringUTF(integrated_address_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jintegrated_address) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _integratedAddress = jintegrated_address ? env->GetStringUTFChars(jintegrated_address, NULL) : nullptr;
  string integrated_address = string(_integratedAddress ? _integratedAddress : "");
  env->ReleaseStringUTFChars(jintegrated_address, _integratedAddress);

  // serialize and return decoded integrated address
  try {
    monero_integrated_address integrated_address = wallet->decode_integrated_address(string(_integratedAddress ? _integratedAddress : ""));
    string integrated_address_json = integrated_address.serialize();
    return env->NewStringUTF(integrated_address_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_height();
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getChainHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getChainHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getSyncHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getSyncHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_sync_height();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setSyncHeightJni(JNIEnv *env, jobject instance, jlong sync_height) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_setSyncHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->set_sync_height(sync_height);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonHeightJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonMaxPeerHeightJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_max_peer_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightByDateJni(JNIEnv* env, jobject instance, jint year, jint month, jint day) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_height_by_date(year, month, day);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jlong start_height) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_syncJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {

    // sync wallet
    monero_sync_result result = wallet->sync(start_height);

    // build and return results as Object[2]{(long) num_blocks_fetched, (boolean) received_money}
    jobjectArray results = env->NewObjectArray(2, env->FindClass("java/lang/Object"), nullptr);
    jclass longClass = env->FindClass("java/lang/Long");
    jmethodID longConstructor = env->GetMethodID(longClass, "<init>", "(J)V");
    jobject numBlocksFetchedWrapped = env->NewObject(longClass, longConstructor, static_cast<jlong>(result.m_num_blocks_fetched));
    env->SetObjectArrayElement(results, 0, numBlocksFetchedWrapped);
    jclass booleanClass = env->FindClass("java/lang/Boolean");
    jmethodID booleanConstructor = env->GetMethodID(booleanClass, "<init>", "(Z)V");
    jobject receivedMoneyWrapped = env->NewObject(booleanClass, booleanConstructor, static_cast<jboolean>(result.m_received_money));
    env->SetObjectArrayElement(results, 1, receivedMoneyWrapped);
    return results;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startSyncingJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_startSyncingJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->start_syncing();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopSyncingJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_stopSyncingJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->stop_syncing();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_rescanSpentJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_rescanSpentJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->rescan_spent();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_rescanBlockchainJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_rescanBlockchainJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->rescan_blockchain();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *env, jobject instance, jint account_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance(account_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance(account_idx, subaddress_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint account_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance(account_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance(account_idx, subaddress_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountsJni(JNIEnv* env, jobject instance, jboolean include_subaddresses, jstring jtag) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAccountsJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tag = jtag ? env->GetStringUTFChars(jtag, NULL) : nullptr;
  string tag = string(_tag ? _tag : "");
  env->ReleaseStringUTFChars(jtag, _tag);

  // get accounts
  vector<monero_account> accounts = wallet->get_accounts(include_subaddresses, tag);

  // wrap and serialize accounts
  rapidjson::Document doc;
  doc.SetObject();
  doc.AddMember("accounts", monero_utils::to_rapidjson_val(doc.GetAllocator(), accounts), doc.GetAllocator());
  string accounts_json = monero_utils::serialize(doc);
  return env->NewStringUTF(accounts_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountJni(JNIEnv* env, jobject instance, jint account_idx, jboolean include_subaddresses) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get account
  monero_account account = wallet->get_account(account_idx, include_subaddresses);

  // serialize and return account
  string account_json = account.serialize();
  return env->NewStringUTF(account_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createAccountJni(JNIEnv* env, jobject instance, jstring jlabel) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  string label = string(_label ? _label : "");
  env->ReleaseStringUTFChars(jlabel, _label);

  // create account
  monero_account account = wallet->create_account(label);

  // serialize and return account
  string account_json = account.serialize();
  return env->NewStringUTF(account_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSubaddressesJni(JNIEnv* env, jobject instance, jint account_idx, jintArray jsubaddressIndices) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getSubaddressesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // convert subaddress indices from jintArray to vector<uint32_t>
  vector<uint32_t> subaddress_indices;
  if (jsubaddressIndices != nullptr) {
    jsize numSubaddressIndices = env->GetArrayLength(jsubaddressIndices);
    jint* intArr = env->GetIntArrayElements(jsubaddressIndices, 0);
    for (int subaddressIndicesIdx = 0; subaddressIndicesIdx < numSubaddressIndices; subaddressIndicesIdx++) {
      subaddress_indices.push_back(intArr[subaddressIndicesIdx]);
    }
  }

  // get subaddresses
  vector<monero_subaddress> subaddresses = wallet->get_subaddresses(account_idx, subaddress_indices);

  // wrap and serialize subaddresses
  rapidjson::Document doc;
  doc.SetObject();
  doc.AddMember("subaddresses", monero_utils::to_rapidjson_val(doc.GetAllocator(), subaddresses), doc.GetAllocator());
  string subaddresses_json = monero_utils::serialize(doc);
  return env->NewStringUTF(subaddresses_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createSubaddressJni(JNIEnv* env, jobject instance, jint account_idx, jstring jlabel) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createSubaddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  string label = string(_label ? _label : "");
  env->ReleaseStringUTFChars(jlabel, _label);

  // create subaddress
  monero_subaddress subaddress = wallet->create_subaddress(account_idx, label);

  // serialize and return subaddress
  string subaddress_json = subaddress.serialize();
  return env->NewStringUTF(subaddress_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxsJni(JNIEnv* env, jobject instance, jstring jtx_query) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getTxsJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_query = jtx_query ? env->GetStringUTFChars(jtx_query, NULL) : nullptr;
  string tx_query_json = string(_tx_query ? _tx_query : "");
  env->ReleaseStringUTFChars(jtx_query, _tx_query);
  try {

    // deserialize tx query
    shared_ptr<monero_tx_query> tx_query = monero_tx_query::deserialize_from_block(tx_query_json);
    //cout << "Fetching txs with query: " << tx_query->serialize() << endl;

    // get txs
    vector<string> missing_tx_hashes;
    vector<shared_ptr<monero_tx_wallet>> txs = wallet->get_txs(*tx_query, missing_tx_hashes);
    MTRACE("Got " << txs.size() << " txs");

    // return unique blocks to preserve model relationships as tree
    shared_ptr<monero_block> unconfirmed_block = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<monero_block>> blocks;
    unordered_set<shared_ptr<monero_block>> seen_block_ptrs;
    for (const shared_ptr<monero_tx_wallet>& tx : txs) {
      if (tx->m_block == boost::none) {
        if (unconfirmed_block == nullptr) unconfirmed_block = make_shared<monero_block>();
        tx->m_block = unconfirmed_block;
        unconfirmed_block->m_txs.push_back(tx);
      }
      unordered_set<shared_ptr<monero_block>>::const_iterator got = seen_block_ptrs.find(tx->m_block.get());
      if (got == seen_block_ptrs.end()) {
        seen_block_ptrs.insert(tx->m_block.get());
        blocks.push_back(tx->m_block.get());
      }
    }
    MTRACE("Returning " << blocks.size() << " blocks");

    // wrap and serialize blocks
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    if (!missing_tx_hashes.empty()) doc.AddMember("missingTxHashes", monero_utils::to_rapidjson_val(doc.GetAllocator(), missing_tx_hashes), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTransfersJni(JNIEnv* env, jobject instance, jstring jtransfer_query) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getTransfersJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _transfer_query = jtransfer_query ? env->GetStringUTFChars(jtransfer_query, NULL) : nullptr;
  string transfer_query_json = string(_transfer_query ? _transfer_query : "");
  env->ReleaseStringUTFChars(jtransfer_query, _transfer_query);
  try {

    // deserialize transfer query
    shared_ptr<monero_transfer_query> transfer_query = monero_transfer_query::deserialize_from_block(transfer_query_json);

//    // log query
//    if (transfer_query->m_tx_query != boost::none) {
//      if ((*transfer_query->m_tx_query)->m_block == boost::none) cout << "Transfer query's tx query rooted at [tx]:" << (*transfer_query->m_tx_query)->serialize() << endl;
//      else cout << "Transfer query's tx query rooted at [block]: " << (*(*transfer_query->m_tx_query)->m_block)->serialize() << endl;
//    } else cout << "Transfer query: " << transfer_query->serialize() << endl;

    // get transfers
    vector<shared_ptr<monero_transfer>> transfers = wallet->get_transfers(*transfer_query);

    // return unique blocks to preserve model relationships as tree
    shared_ptr<monero_block> unconfirmed_block = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<monero_block>> blocks;
    unordered_set<shared_ptr<monero_block>> seen_block_ptrs;
    for (auto const& transfer : transfers) {
      shared_ptr<monero_tx_wallet> tx = transfer->m_tx;
      if (tx->m_block == boost::none) {
        if (unconfirmed_block == nullptr) unconfirmed_block = make_shared<monero_block>();
        tx->m_block = unconfirmed_block;
        unconfirmed_block->m_txs.push_back(tx);
      }
      unordered_set<shared_ptr<monero_block>>::const_iterator got = seen_block_ptrs.find(tx->m_block.get());
      if (got == seen_block_ptrs.end()) {
        seen_block_ptrs.insert(tx->m_block.get());
        blocks.push_back(tx->m_block.get());
      }
    }

    // wrap and serialize blocks
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsJni(JNIEnv* env, jobject instance, jstring joutput_query) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getOutputsJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _output_query = joutput_query ? env->GetStringUTFChars(joutput_query, NULL) : nullptr;
  string output_query_json = string(_output_query ? _output_query : "");
  env->ReleaseStringUTFChars(joutput_query, _output_query);
  try {

    // deserialize output query
    shared_ptr<monero_output_query> output_query = monero_output_query::deserialize_from_block(output_query_json);
    MTRACE("Fetching outputs with request: " << output_query->serialize());

    // get outputs
    vector<shared_ptr<monero_output_wallet>> outputs = wallet->get_outputs(*output_query);
    MTRACE("Got " << outputs.size() << " outputs");

    // return unique blocks to preserve model relationships as tree
    vector<monero_block> blocks;
    unordered_set<shared_ptr<monero_block>> seen_block_ptrs;
    for (auto const& output : outputs) {
      shared_ptr<monero_tx_wallet> tx = static_pointer_cast<monero_tx_wallet>(output->m_tx);
      if (tx->m_block == boost::none) throw runtime_error("Need to handle unconfirmed output");
      unordered_set<shared_ptr<monero_block>>::const_iterator got = seen_block_ptrs.find(*tx->m_block);
      if (got == seen_block_ptrs.end()) {
        seen_block_ptrs.insert(*tx->m_block);
        blocks.push_back(**tx->m_block);
      }
    }
    MTRACE("Returning " << blocks.size() << " blocks");

    // wrap and serialize blocks
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsHexJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_outputs_hex().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importOutputsHexJni(JNIEnv* env, jobject instance, jstring joutputs_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _outputs_hex = joutputs_hex ? env->GetStringUTFChars(joutputs_hex, NULL) : nullptr;
  string outputs_hex = string(_outputs_hex ? _outputs_hex : "");
  env->ReleaseStringUTFChars(joutputs_hex, _outputs_hex);
  try {
    return wallet->import_outputs_hex(outputs_hex);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getKeyImagesJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getKeyImagesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // fetch key images
  vector<shared_ptr<monero_key_image>> key_images = wallet->get_key_images();
  MTRACE("Fetched " << key_images.size() << " key images");

  // wrap and serialize key images
  rapidjson::Document doc;
  doc.SetObject();
  doc.AddMember("keyImages", monero_utils::to_rapidjson_val(doc.GetAllocator(), key_images), doc.GetAllocator());
  string key_images_json = monero_utils::serialize(doc);
  return env->NewStringUTF(key_images_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_importKeyImagesJni(JNIEnv* env, jobject instance, jstring jkey_images_json) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_importKeyImagesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key_images_json = jkey_images_json ? env->GetStringUTFChars(jkey_images_json, NULL) : nullptr;
  string key_images_json = string(_key_images_json ? _key_images_json : "");
  env->ReleaseStringUTFChars(jkey_images_json, _key_images_json);

  // deserialize key images to import
  vector<shared_ptr<monero_key_image>> key_images = monero_key_image::deserialize_key_images(key_images_json);
  //MTRACE("Deserialized " << key_images.size() << " key images from java json");

  // import key images
  shared_ptr<monero_key_image_import_result> result;
  try {
    result = wallet->import_key_images(key_images);
    return env->NewStringUTF(result->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createTxsJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_sendTxsJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize send request
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  //MTRACE("Deserialized tx config, re-serialized: " << config->serialize());

  // submit send request
  vector<shared_ptr<monero_tx_wallet>> txs;
  try {
    txs = wallet->create_txs(*config);
    //MTRACE("Got " << txs.size() << " txs");
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // serialize and return tx set
  return env->NewStringUTF(txs[0]->m_tx_set.get()->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepUnlockedJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_sweepUnlockedJni(config)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize send request
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  //MTRACE("Deserialized tx config, re-serialized: " << config->serialize());

  // submit request with configuration
  vector<shared_ptr<monero_tx_wallet>> txs;
  try {
    txs = wallet->sweep_unlocked(*config);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // collect tx sets
  vector<shared_ptr<monero_tx_set>> tx_sets;
  for (int i = 0; i < txs.size(); i++) {
    if (std::find(tx_sets.begin(), tx_sets.end(), txs[i]->m_tx_set.get()) == tx_sets.end()) {
      tx_sets.push_back(txs[i]->m_tx_set.get());
    }
  }

  // wrap and serialize tx sets
  rapidjson::Document doc;
  doc.SetObject();
  doc.AddMember("txSets", monero_utils::to_rapidjson_val(doc.GetAllocator(), tx_sets), doc.GetAllocator());
  string tx_sets_json = monero_utils::serialize(doc);
  return env->NewStringUTF(tx_sets_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepOutputJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_sweepOutputJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config);
  env->ReleaseStringUTFChars(jconfig, _config);

  MTRACE("Send request json: " << config_json);

  // deserialize send request
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  MTRACE("Deserialized send request, re-serialized: " << config->serialize());

  // submit request with configuration
  shared_ptr<monero_tx_wallet> tx;
  try {
    tx = wallet->sweep_output(*config);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // serialize and return tx set
  return env->NewStringUTF(tx->m_tx_set.get()->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepDustJni(JNIEnv* env, jobject instance, jboolean relay) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_sweepDustJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // sweep dust
  vector<shared_ptr<monero_tx_wallet>> txs;
  try {
    txs = wallet->sweep_dust(relay);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // serialize and return tx set
  return env->NewStringUTF(txs[0]->m_tx_set.get()->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parseTxSetJni(JNIEnv* env, jobject instance, jstring jtx_set_json) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_parseTxSetJson(tx_set_json)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx set json string
  const char* _tx_set_json = jtx_set_json ? env->GetStringUTFChars(jtx_set_json, NULL) : nullptr;
  string tx_set_json = string(_tx_set_json);
  env->ReleaseStringUTFChars(jtx_set_json, _tx_set_json);

  try {

    // deserialize tx set to parse
    monero_tx_set tx_set = monero_tx_set::deserialize(tx_set_json);

    // parse tx set
    monero_tx_set parsed_tx_set = wallet->parse_tx_set(tx_set);

    // serialize and return parsed tx set
    return env->NewStringUTF(parsed_tx_set.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signTxsJni(JNIEnv* env, jobject instance, jstring junsigned_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_signTxsJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get unsigned tx set as string
  const char* _unsigned_tx_hex = junsigned_tx_hex ? env->GetStringUTFChars(junsigned_tx_hex, NULL) : nullptr;
  string unsigned_tx_hex = string(_unsigned_tx_hex ? _unsigned_tx_hex : "");
  env->ReleaseStringUTFChars(junsigned_tx_hex, _unsigned_tx_hex);

  // sign txs
  try {
    return env->NewStringUTF(wallet->sign_txs(unsigned_tx_hex).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_submitTxsJni(JNIEnv* env, jobject instance, jstring jsigned_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_submitTxsJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get signed tx set as string
  const char* _signed_tx_hex = jsigned_tx_hex ? env->GetStringUTFChars(jsigned_tx_hex, NULL) : nullptr;
  string signed_tx_hex = string(_signed_tx_hex ? _signed_tx_hex : "");
  env->ReleaseStringUTFChars(jsigned_tx_hex, _signed_tx_hex);

  try {

    // submit signed txs
    vector<string> tx_hashes = wallet->submit_txs(signed_tx_hex);

    // return tx hashes as jobjectArray
    jobjectArray jtx_hashes = env->NewObjectArray(tx_hashes.size(), env->FindClass("java/lang/String"), nullptr);
    for (int i = 0; i < tx_hashes.size(); i++) env->SetObjectArrayElement(jtx_hashes, i, env->NewStringUTF(tx_hashes[i].c_str()));
    return jtx_hashes;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_relayTxsJni(JNIEnv* env, jobject instance, jobjectArray jtx_metadatas) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_relayTxsJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx metadatas from jobjectArray to vector<string>
  vector<string> tx_metadatas;
  if (jtx_metadatas != nullptr) {
    jsize size = env->GetArrayLength(jtx_metadatas);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtx_metadatas, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      tx_metadatas.push_back(str);
    }
  }

  // relay tx metadata
  vector<string> tx_hashes;
  try {
    tx_hashes = wallet->relay_txs(tx_metadatas);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // return tx hashes as jobjectArray
  jobjectArray jtx_hashes = env->NewObjectArray(tx_hashes.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < tx_hashes.size(); i++) env->SetObjectArrayElement(jtx_hashes, i, env->NewStringUTF(tx_hashes[i].c_str()));
  return jtx_hashes;
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signMessageJni(JNIEnv* env, jobject instance, jstring jmsg, jint message_signature_type, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_signMessageJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _msg = jmsg ? env->GetStringUTFChars(jmsg, NULL) : nullptr;
  string msg = string(_msg ? _msg : "");
  env->ReleaseStringUTFChars(jmsg, _msg);
  monero_message_signature_type signature_type = message_signature_type == 0 ? monero_message_signature_type::SIGN_WITH_SPEND_KEY : monero_message_signature_type::SIGN_WITH_VIEW_KEY;
  try {
    string signature = wallet->sign_message(msg, signature_type, account_idx, subaddress_idx);
    return env->NewStringUTF(signature.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_verifyMessageJni(JNIEnv* env, jobject instance, jstring jmsg, jstring jaddress, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_verifyMessageJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
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
    monero_message_signature_result result = wallet->verify_message(msg, address, signature);
    return env->NewStringUTF(result.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxKeyJni(JNIEnv* env, jobject instance, jstring jtx_hash) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getTxKeyJniJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  try {
    return env->NewStringUTF(wallet->get_tx_key(tx_hash).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxKeyJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jtx_key, jstring jaddress) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_checktx_keyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  const char* _tx_key = jtx_key ? env->GetStringUTFChars(jtx_key, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  string tx_key = string(_tx_key == nullptr ? "" : _tx_key);
  string address = string(_address == nullptr ? "" : _address);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  env->ReleaseStringUTFChars(jtx_key, _tx_key);
  env->ReleaseStringUTFChars(jaddress, _address);
  try {
    return env->NewStringUTF(wallet->check_tx_key(tx_hash, tx_key, address)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jaddress, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getTxProofJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->get_tx_proof(tx_hash, address, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jaddress, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_checkTxProofJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return env->NewStringUTF(wallet->check_tx_proof(tx_hash, address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSpendProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getSpendProofJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->get_spend_proof(tx_hash, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_checkSpendProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_checkSpendProofJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_hash = jtx_hash ? env->GetStringUTFChars(jtx_hash, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string tx_hash = string(_tx_hash == nullptr ? "" : _tx_hash);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtx_hash, _tx_hash);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return static_cast<jboolean>(wallet->check_spend_proof(tx_hash, message, signature));
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni(JNIEnv* env, jobject instance, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->get_reserve_proof_wallet(message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofAccountJni(JNIEnv* env, jobject instance, jint account_idx, jstring jamount_str, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _amount_str = jamount_str ? env->GetStringUTFChars(jamount_str, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string amount_str = string(_amount_str == nullptr ? "" : _amount_str);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jamount_str, _amount_str);
  env->ReleaseStringUTFChars(jmessage, _message);
  uint64_t amount = boost::lexical_cast<uint64_t>(amount_str);
  try {
    return env->NewStringUTF(wallet->get_reserve_proof_account(account_idx, amount, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkReserveProofJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_checkReserveProofAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
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
    return env->NewStringUTF(wallet->check_reserve_proof(address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtx_hashes) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getTxNotesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx hashes from jobjectArray to vector<string>
  vector<string> tx_hashes;
  if (jtx_hashes != nullptr) {
    jsize size = env->GetArrayLength(jtx_hashes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtx_hashes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      tx_hashes.push_back(str);
    }
  }

  // get tx notes
  vector<string> notes;
  try {
    notes = wallet->get_tx_notes(tx_hashes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // convert and return tx notes as jobjectArray
  jobjectArray jtx_notes = env->NewObjectArray(notes.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < notes.size(); i++) {
    env->SetObjectArrayElement(jtx_notes, i, env->NewStringUTF(notes[i].c_str()));
  }
  return jtx_notes;
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtx_hashes, jobjectArray jtx_notes) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_setTxNotesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx hashes from jobjectArray to vector<string>
  vector<string> tx_hashes;
  if (jtx_hashes != nullptr) {
    jsize size = env->GetArrayLength(jtx_hashes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtx_hashes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      tx_hashes.push_back(str);
    }
  }

  // get tx notes from jobjectArray to vector<string>
  vector<string> notes;
  if (jtx_notes != nullptr) {
    jsize size = env->GetArrayLength(jtx_notes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtx_notes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      notes.push_back(str);
    }
  }

  // set tx notes
  try {
    wallet->set_tx_notes(tx_hashes, notes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressBookEntriesJni(JNIEnv* env, jobject instance, jintArray jindices) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAddressBookEntriesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // convert subaddress indices from jintArray to vector<uint32_t>
  vector<uint64_t> indices;
  if (jindices != nullptr) {
    jsize numIndices = env->GetArrayLength(jindices);
    jint* intArr = env->GetIntArrayElements(jindices, 0);
    for (int indicesIdx = 0; indicesIdx < numIndices; indicesIdx++) {
      indices.push_back(intArr[indicesIdx]);
    }
  }

  try {

    // get address book entries
    vector<monero_address_book_entry> entries = wallet->get_address_book_entries(indices);

    // wrap and serialize entries
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("entries", monero_utils::to_rapidjson_val(doc.GetAllocator(), entries), doc.GetAllocator());
    string entries_json = monero_utils::serialize(doc);
    return env->NewStringUTF(entries_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

// TODO: return jlong for uint64_t
JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_addAddressBookEntryJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jdescription) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_addAddressBookEntryJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // collect string params
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _description = jdescription ? env->GetStringUTFChars(jdescription, NULL) : nullptr;
  string address = string(_address == nullptr ? "" : _address);
  string description = string(_description == nullptr ? "" : _description);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jdescription, _description);

  // add address book entry
  try {
    return wallet->add_address_book_entry(address, description);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_editAddressBookEntryJni(JNIEnv* env, jobject instance, jint index, jboolean set_address, jstring jaddress, jboolean set_description, jstring jdescription) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_editAddressBookEntryJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // collect string params
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _description = jdescription ? env->GetStringUTFChars(jdescription, NULL) : nullptr;
  string address = string(_address == nullptr ? "" : _address);
  string description = string(_description == nullptr ? "" : _description);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jdescription, _description);

  // edit address book entry
  try {
    wallet->edit_address_book_entry(index, set_address, address, set_description, description);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_deleteAddressBookEntryJni(JNIEnv* env, jobject instance, jint index) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_deleteAddressBookEntryJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // delete address book entry
  try {
    wallet->delete_address_book_entry(index);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createPaymentUriJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_createPaymentUriJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize send request
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  MTRACE("Fetching payment uri with : " << config->serialize());

  // get payment uri
  string payment_uri;
  try {
    payment_uri = wallet->create_payment_uri(*config.get());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return
  return env->NewStringUTF(payment_uri.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni(JNIEnv* env, jobject instance, jstring juri) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _uri = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  string uri = string(_uri ? _uri : "");
  env->ReleaseStringUTFChars(juri, _uri);

  // parse uri to send request
  shared_ptr<monero_tx_config> config;
  try {
    config = wallet->parse_payment_uri(uri);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // return serialized request
  return env->NewStringUTF(config->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAttributeJni(JNIEnv* env, jobject instance, jstring jkey) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getAttribute()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  string key = string(_key);
  env->ReleaseStringUTFChars(jkey, _key);
  try {
    string value;
    if (!wallet->get_attribute(key, value)) return 0;
    return env->NewStringUTF(value.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setAttributeJni(JNIEnv* env, jobject instance, jstring jkey, jstring jval) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_setAttribute()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  const char* _val = jval ? env->GetStringUTFChars(jval, NULL) : nullptr;
  string key = string(_key);
  string val = string(_val);
  env->ReleaseStringUTFChars(jkey, _key);
  env->ReleaseStringUTFChars(jval, _val);
  try {
    wallet->set_attribute(key, val);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startMiningJni(JNIEnv* env, jobject instance, jlong num_threads, jboolean background_mining, jboolean ignore_battery) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_startMiningJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->start_mining(num_threads, background_mining, ignore_battery);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopMiningJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_startMiningJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->stop_mining();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_saveJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_saveJni(path, password)");

  // save wallet
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->save();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_moveToJni(JNIEnv* env, jobject instance, jstring jpath, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_moveToJni(path, password)");
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpath ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string path = string(_path ? _path : "");
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);

  // move wallet
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->move_to(path, password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_closeJni(JNIEnv* env, jobject instance, jboolean save) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_CloseJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  if (save) wallet->save();
  delete wallet;
  wallet = nullptr;
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isMultisigImportNeededJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_isMultisigImportNeededJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    bool is_multisig_import_needed = wallet->is_multisig_import_needed();
    return static_cast<jboolean>(is_multisig_import_needed);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMultisigInfoJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getMultisigInfoJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    monero_multisig_info info = wallet->get_multisig_info();
    return env->NewStringUTF(info.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_prepareMultisigJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_prepareMultisigJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    string multisig_hex = wallet->prepare_multisig();
    return env->NewStringUTF(multisig_hex.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_makeMultisigJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes, jint threshold, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_makeMultisigJni");

  // get multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      multisig_hexes.push_back(str);
    }
  }

  // get password as string
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);

  // make the wallet multisig and return result
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    monero_multisig_init_result result = wallet->make_multisig(multisig_hexes, threshold, password);
    return env->NewStringUTF(result.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_exchangeMultisigKeysJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_exchangeMultisigKeysJni");

  // get multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      multisig_hexes.push_back(str);
    }
  }

  // get password as string
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);

  // import peer multisig keys and export result with address xor multisig hex for next round
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    monero_multisig_init_result result = wallet->exchange_multisig_keys(multisig_hexes, password);
    return env->NewStringUTF(result.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMultisigHexJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_getMultisigHexJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    string multisig_hex = wallet->get_multisig_hex();
    return env->NewStringUTF(multisig_hex.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importMultisigHexJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_importMultisigHexJni");

  // get peer multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      multisig_hexes.push_back(str);
    }
  }

  // import peer multisig hex and return the number of outputs they signed
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    int num_outputs = wallet->import_multisig_hex(multisig_hexes);
    return num_outputs;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signMultisigTxHexJni(JNIEnv* env, jobject instance, jstring jmultisig_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_signMultisigTxHexJni");

  // get multisig tx hex as string
  const char* _multisig_tx_hex = jmultisig_tx_hex ? env->GetStringUTFChars(jmultisig_tx_hex, NULL) : nullptr;
  string multisig_tx_hex = string(_multisig_tx_hex ? _multisig_tx_hex : "");
  env->ReleaseStringUTFChars(jmultisig_tx_hex, _multisig_tx_hex);

  // sign multisig tx hex and return result
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    monero_multisig_sign_result result = wallet->sign_multisig_tx_hex(multisig_tx_hex);
    return env->NewStringUTF(result.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_submitMultisigTxHexJni(JNIEnv* env, jobject instance, jstring jsigned_multisig_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletJni_submitMultisigTxHexJni");

  // get signed multisig tx hex as string
  const char* _signed_multisig_tx_hex = jsigned_multisig_tx_hex ? env->GetStringUTFChars(jsigned_multisig_tx_hex, NULL) : nullptr;
  string signed_multisig_tx_hex = string(_signed_multisig_tx_hex ? _signed_multisig_tx_hex : "");
  env->ReleaseStringUTFChars(jsigned_multisig_tx_hex, _signed_multisig_tx_hex);

  // submit signed multisig tx hex and return the resulting tx hashes
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    vector<string> tx_hashes = wallet->submit_multisig_tx_hex(signed_multisig_tx_hex);
    jobjectArray jtx_hashes = env->NewObjectArray(tx_hashes.size(), env->FindClass("java/lang/String"), nullptr);
    for (int i = 0; i < tx_hashes.size(); i++) env->SetObjectArrayElement(jtx_hashes, i, env->NewStringUTF(tx_hashes[i].c_str()));
    return jtx_hashes;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

#ifdef __cplusplus
}
#endif
