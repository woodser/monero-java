/**
 * Copyright (c) woodser
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
#include "monero_jni_bridge.h"
#include "wallet/monero_wallet_full.h"
#include "utils/monero_utils.h"

using namespace std;
using namespace monero;

// initialize names of private instance variables used in Java full wallet which contain memory references to native wallet and listener
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

// credit: https://stackoverflow.com/questions/41820039/jstringjni-to-stdstringc-with-utf8-characters
std::string jstring2string(JNIEnv *env, jstring jStr) {
  if (!jStr) return "";

  const jclass stringClass = env->GetObjectClass(jStr);
  const jmethodID getBytes = env->GetMethodID(stringClass, "getBytes", "(Ljava/lang/String;)[B");
  const jbyteArray stringJbytes = (jbyteArray) env->CallObjectMethod(jStr, getBytes, env->NewStringUTF("UTF-8"));

  size_t length = (size_t) env->GetArrayLength(stringJbytes);
  jbyte* pBytes = env->GetByteArrayElements(stringJbytes, NULL);

  std::string ret = std::string((char *)pBytes, length);
  env->ReleaseByteArrayElements(stringJbytes, pBytes, JNI_ABORT);

  env->DeleteLocalRef(stringJbytes);
  env->DeleteLocalRef(stringClass);
  return ret;
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

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
  cachedJVM = jvm;
  JNIEnv *env;
  if (jvm->GetEnv(reinterpret_cast<void **>(&env), JNI_VERSION_1_6) != JNI_OK) {
    return -1;
  }

//  class_ArrayList = static_cast<jclass>(env->NewGlobalRef(env->FindClass("java/util/ArrayList")));
//  class_TransactionInfo = static_cast<jclass>(env->NewGlobalRef(env->FindClass("com/m2049r/xmrwallet/model/TransactionInfo")));
//  class_Transfer = static_cast<jclass>(env->NewGlobalRef(env->FindClass("com/m2049r/xmrwallet/model/Transfer")));
  class_WalletListener = static_cast<jclass>(env->NewGlobalRef(env->FindClass("monero/wallet/MoneroWalletFull$WalletJniListener")));
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
  std::mutex _listenerMutex;

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
    int envStat = attachJVM(&env); // TODO: necessary to attach every time?
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
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception); // TODO: does not detach JVM

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
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception); // TODO: does not detach JVM

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
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception); // TODO: does not detach JVM

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
    int version = output.m_tx->m_version == boost::none ? 2 : *output.m_tx->m_version; // TODO: version not present in unlocked output notification, defaulting to 2
    bool is_locked = std::static_pointer_cast<monero_tx_wallet>(output.m_tx)->m_is_locked.get();
    jstring junlock_time = env->NewStringUTF(to_string(*output.m_tx->m_unlock_time).c_str());

    // invoke Java listener's onOutputReceived()
    jmethodID listenerClass_onOutputReceived = env->GetMethodID(class_WalletListener, "onOutputReceived", "(JLjava/lang/String;Ljava/lang/String;IIILjava/lang/String;Z)V");
    env->CallVoidMethod(jlistener, listenerClass_onOutputReceived, height == boost::none ? 0 : *height, jtx_hash, jamount_str, *output.m_account_index, *output.m_subaddress_index, version, junlock_time, is_locked);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception); // TODO: does not detach JVM

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
    jstring jaccount_idx_str = env->NewStringUTF(output.m_account_index == boost::none ? "" : to_string(*output.m_account_index).c_str()); // TODO: subaddress indices not known for some inputs (e.g. after tx creation)
    jstring jsubaddress_idx_str = env->NewStringUTF(output.m_subaddress_index == boost::none ? "" : to_string(*output.m_subaddress_index).c_str());
    int version = output.m_tx->m_version == boost::none ? 2 : *output.m_tx->m_version; // TODO: version not present in unlocked output notification, defaulting to 2
    bool is_locked = std::static_pointer_cast<monero_tx_wallet>(output.m_tx)->m_is_locked.get();
    jstring junlock_time = env->NewStringUTF(to_string(*output.m_tx->m_unlock_time).c_str());

    // invoke Java listener's onOutputSpent()
    jmethodID listenerClass_onOutputSpent = env->GetMethodID(class_WalletListener, "onOutputSpent", "(JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;ILjava/lang/String;Z)V");
    env->CallVoidMethod(jlistener, listenerClass_onOutputSpent, height == boost::none ? 0 : *height, jtx_hash, jamount_str, jaccount_idx_str, jsubaddress_idx_str, version, junlock_time, is_locked);

    // check for and rethrow Java exception
    jthrowable jexception = env->ExceptionOccurred();
    if (jexception) rethrow_java_exception_as_cpp_exception(env, jexception); // TODO: does not detach JVM

    detachJVM(env, envStat);
  }
};

#ifdef __cplusplus
extern "C"
{
#endif

// ------------------------------ STATIC UTILS --------------------------------

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_getIntegratedAddressJni(JNIEnv *env, jclass clazz, jint jnetwork_type, jstring jstandard_address, jstring jpayment_id) {

  // convert jstrings to strings
  string standard_address = jstring2string(env, jstandard_address);
  string payment_id = jstring2string(env, jpayment_id);

  // get integrated address
  try {
    monero_integrated_address integrated_address = monero_utils::get_integrated_address(static_cast<monero_network_type>(jnetwork_type), standard_address, payment_id);
    return env->NewStringUTF(integrated_address.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jbyteArray JNICALL Java_monero_common_MoneroUtils_jsonToBinaryJni(JNIEnv *env, jclass clazz, jstring json) {

  // convert json jstring to string
  string json_str = jstring2string(env, json);

  // convert json to monero's portable storage binary format
  string bin_str;
  monero_utils::json_to_binary(json_str, bin_str);

  // convert binary string to jbyteArray
  jbyteArray result = env->NewByteArray(bin_str.length());
  if (result == NULL) {
     return NULL; // out of memory error thrown
  }

  // fill a temp structure to use to populate the java byte array
  jbyte fill[bin_str.length()];
  for (int i = 0; i < bin_str.length(); i++) {
     fill[i] = bin_str[i];
  }
  env->SetByteArrayRegion(result, 0, bin_str.length(), fill);
  return result;
}

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_binaryToJsonJni(JNIEnv *env, jclass clazz, jbyteArray bin) {

  // convert the jbyteArray to a string
  int binLength = env->GetArrayLength(bin);
  jboolean is_copy;
  jbyte* jbytes = env->GetByteArrayElements(bin, &is_copy);
  string bin_str = string((char*) jbytes, binLength);

  // convert monero's portable storage binary format to json
  string json_str;
  monero_utils::binary_to_json(bin_str, json_str);

  // convert string to jstring
  return env->NewStringUTF(json_str.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_binaryBlocksToJsonJni(JNIEnv *env, jclass clazz, jbyteArray blocks_bin) {

  // convert the jbyteArray to a string
  int binLength = env->GetArrayLength(blocks_bin);
  jboolean is_copy;
  jbyte* jbytes = env->GetByteArrayElements(blocks_bin, &is_copy);
  string bin_str = string((char*) jbytes, binLength);

  // convert monero's portable storage binary format to json
  string json_str;
  monero_utils::binary_blocks_to_json(bin_str, json_str);

  // convert string to jstring
  return env->NewStringUTF(json_str.c_str());
}

JNIEXPORT void JNICALL Java_monero_common_MoneroUtils_setLogLevelJni(JNIEnv* env, jclass clazz, jint level) {
  monero_utils::set_log_level(level);
}

JNIEXPORT void JNICALL Java_monero_common_MoneroUtils_configureLoggingJni(JNIEnv* env, jclass clazz, jstring jpath, jboolean console) {
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  string path = string(_path ? _path : "");
  env->ReleaseStringUTFChars(jpath, _path);
  monero_utils::configure_logging(path, console);
}

// --------------------------- STATIC WALLET UTILS ----------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_walletExistsJni(JNIEnv *env, jclass clazz, jstring jpath) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_walletExistsJni");
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  string path = string(_path);
  env->ReleaseStringUTFChars(jpath, _path);
  bool wallet_exists = monero_wallet_full::wallet_exists(path);
  return static_cast<jboolean>(wallet_exists);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_openWalletJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetwork_type) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_openWalletJni");
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  const char* _password = env->GetStringUTFChars(jpassword, NULL);
  string path = string(_path);
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);

  // load wallet from file
  try {
    monero_wallet* wallet = monero_wallet_full::open_wallet(path, password, static_cast<monero_network_type>(jnetwork_type));
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_openWalletDataJni(JNIEnv *env, jclass clazz, jstring jpassword, jint jnetwork_type, jbyteArray jkeys_data, jbyteArray jcache_data) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_openWalletDataJni()");

  // convert password to string
  const char* _password = env->GetStringUTFChars(jpassword, NULL);
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);

  // convert keys bytes to string
  int keys_length = env->GetArrayLength(jkeys_data);
  jboolean is_copy;
  jbyte* _keys_data = env->GetByteArrayElements(jkeys_data, &is_copy);
  string keys_data = string((char*) _keys_data, keys_length);
  env->ReleaseByteArrayElements(jkeys_data, _keys_data, JNI_ABORT);

  // convert cache bytes to string
  int cache_length = env->GetArrayLength(jcache_data);
  jbyte* _cache_data = env->GetByteArrayElements(jcache_data, &is_copy);
  string cache_data = string((char*) _cache_data, cache_length);
  env->ReleaseByteArrayElements(jcache_data, _cache_data, JNI_ABORT);

  // load wallet from data
  try {
    monero_wallet* wallet = monero_wallet_full::open_wallet_data(password, static_cast<monero_network_type>(jnetwork_type), keys_data, cache_data);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_createWalletJni(JNIEnv *env, jclass clazz, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_createWalletJni");

  // get config as json string
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize wallet config
  shared_ptr<monero_wallet_config> config = monero_wallet_config::deserialize(config_json);

  // construct wallet
  try {
    monero_wallet* wallet = monero_wallet_full::create_wallet(*config);
    return reinterpret_cast<jlong>(wallet);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getSeedLanguagesJni(JNIEnv *env, jclass clazz) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getLanguagesJni");

  // get languages
  vector<string> languages;
  try {
    languages = monero_wallet_full::get_seed_languages();
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

//  ------------------------ WALLET INSTANCE METHODS --------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isViewOnlyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_isViewOnlyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->is_view_only();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring juri, jstring jusername, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setDaemonConnectionJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    set_daemon_connection(env, wallet, juri, jusername, jpassword);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setProxyJni(JNIEnv *env, jobject instance, jstring juri) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setProxyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _uri = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  string uri = string(_uri ? _uri : "");
  env->ReleaseStringUTFChars(juri, _uri);
  try {
    wallet->set_daemon_proxy(uri);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getDaemonConnectionJni()");

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

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isConnectedToDaemonJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return static_cast<jboolean>(wallet->is_connected_to_daemon());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isDaemonSyncedJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->is_daemon_synced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isSyncedJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->is_synced();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getVersionJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getVersionJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_version().serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPathJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPathJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return env->NewStringUTF(wallet->get_path().c_str());
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getNetworkTypeJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_network_type();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSeedJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getSeedJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_seed().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSeedLanguageJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getSeedLanguageJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_seed_language().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPublicViewKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPublicViewKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_public_view_key().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPrivateViewKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPrivateViewKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_private_view_key().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPublicSpendKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPublicSpendKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_public_spend_key().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPrivateSpendKeyJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPrivateSpendKeyJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->get_private_spend_key().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  string address = wallet->get_address((uint32_t) account_idx, (uint32_t) subaddress_idx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressIndexJni(JNIEnv *env, jobject instance, jstring jaddress) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAddressIndexJni");

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
JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setListenerJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jstandard_address, jstring jpayment_id) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getIntegratedAddressJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_decodeIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jintegrated_address) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_decodeIntegratedAddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _integratedAddress = jintegrated_address ? env->GetStringUTFChars(jintegrated_address, NULL) : nullptr;
  string integrated_address_str = string(_integratedAddress ? _integratedAddress : "");
  env->ReleaseStringUTFChars(jintegrated_address, _integratedAddress);

  // serialize and return decoded integrated address
  try {
    monero_integrated_address integrated_address = wallet->decode_integrated_address(integrated_address_str);
    string integrated_address_json = integrated_address.serialize();
    return env->NewStringUTF(integrated_address_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_height();
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getChainHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getChainHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getRestoreHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_restore_height();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setRestoreHeightJni(JNIEnv *env, jobject instance, jlong restore_height) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setRestoreHeightJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->set_restore_height(restore_height);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonHeightJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonMaxPeerHeightJni(JNIEnv* env, jobject instance) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_daemon_max_peer_height();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getHeightByDateJni(JNIEnv* env, jobject instance, jint year, jint month, jint day) {
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return wallet->get_height_by_date(year, month, day);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_syncJni(JNIEnv *env, jobject instance, jlong start_height) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_syncJni");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_startSyncingJni(JNIEnv *env, jobject instance, jlong sync_period_in_ms) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_startSyncingJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->start_syncing(sync_period_in_ms);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_stopSyncingJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_stopSyncingJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->stop_syncing();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_scanTxsJni(JNIEnv* env, jobject instance, jobjectArray jtx_hashes) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_scanTxsJni");
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
      env->DeleteLocalRef(jstr);
      tx_hashes.push_back(str);
    }
    env->DeleteLocalRef(jtx_hashes);
  }

  // scan txs
  try {
    wallet->scan_txs(tx_hashes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_rescanSpentJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_rescanSpentJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->rescan_spent();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_rescanBlockchainJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_rescanBlockchainJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->rescan_blockchain();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getBalanceWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceAccountJni(JNIEnv *env, jobject instance, jint account_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getBalanceAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance(account_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getBalanceSubaddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_balance(account_idx, subaddress_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceWalletJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint account_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance(account_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceSubaddressJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  uint64_t balance = wallet->get_unlocked_balance(account_idx, subaddress_idx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAccountsJni(JNIEnv* env, jobject instance, jboolean include_subaddresses, jstring jtag) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAccountsJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAccountJni(JNIEnv* env, jobject instance, jint account_idx, jboolean include_subaddresses) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAccountJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get account
  monero_account account = wallet->get_account(account_idx, include_subaddresses);

  // serialize and return account
  string account_json = account.serialize();
  return env->NewStringUTF(account_json.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createAccountJni(JNIEnv* env, jobject instance, jstring jlabel) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_createAccountJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSubaddressesJni(JNIEnv* env, jobject instance, jint account_idx, jintArray jsubaddressIndices) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getSubaddressesJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createSubaddressJni(JNIEnv* env, jobject instance, jint account_idx, jstring jlabel) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_createSubaddressJni");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setSubaddressLabelJni(JNIEnv* env, jobject instance, jint account_idx, jint subaddress_idx, jstring jlabel) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setSubaddressLabelJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  string label = string(_label ? _label : "");
  env->ReleaseStringUTFChars(jlabel, _label);
  try {
    wallet->set_subaddress_label(account_idx, subaddress_idx, label);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxsJni(JNIEnv* env, jobject instance, jstring jtx_query) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getTxsJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _tx_query = jtx_query ? env->GetStringUTFChars(jtx_query, NULL) : nullptr;
  string tx_query_json = string(_tx_query ? _tx_query : "");
  env->ReleaseStringUTFChars(jtx_query, _tx_query);
  try {

    // deserialize tx query
    shared_ptr<monero_tx_query> tx_query = monero_tx_query::deserialize_from_block(tx_query_json);
    //cout << "Fetching txs with query: " << tx_query->serialize() << endl;

    // get txs
    vector<shared_ptr<monero_tx_wallet>> txs = wallet->get_txs(*tx_query);
    MTRACE("Got " << txs.size() << " txs");

    // wrap and serialize blocks
    vector<shared_ptr<monero_block>> blocks = monero_utils::get_blocks_from_txs(txs);
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);

    // free memory
    monero_utils::free(blocks);
    monero_utils::free(tx_query);
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTransfersJni(JNIEnv* env, jobject instance, jstring jtransfer_query) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getTransfersJni");
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

    // wrap and serialize blocks
    vector<shared_ptr<monero_block>> blocks = monero_utils::get_blocks_from_transfers(transfers);
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);

    // free memory
    monero_utils::free(blocks);
    monero_utils::free(transfer_query->m_tx_query.get());
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getOutputsJni(JNIEnv* env, jobject instance, jstring joutput_query) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getOutputsJni");
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

    // wrap and serialize blocks
    vector<shared_ptr<monero_block>> blocks = monero_utils::get_blocks_from_outputs(outputs);
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("blocks", monero_utils::to_rapidjson_val(doc.GetAllocator(), blocks), doc.GetAllocator());
    string blocks_json = monero_utils::serialize(doc);

    // free memory
    monero_utils::free(blocks);
    monero_utils::free(output_query->m_tx_query.get());
    return env->NewStringUTF(blocks_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportOutputsJni(JNIEnv* env, jobject instance, jboolean all) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_exportOutputsJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    return env->NewStringUTF(wallet->export_outputs(all).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_importOutputsJni(JNIEnv* env, jobject instance, jstring joutputs_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_exportOutputsJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _outputs_hex = joutputs_hex ? env->GetStringUTFChars(joutputs_hex, NULL) : nullptr;
  string outputs_hex = string(_outputs_hex ? _outputs_hex : "");
  env->ReleaseStringUTFChars(joutputs_hex, _outputs_hex);
  try {
    return wallet->import_outputs(outputs_hex);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportKeyImagesJni(JNIEnv* env, jobject instance, jboolean all) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_exportKeyImagesJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {

    // fetch key images
    vector<shared_ptr<monero_key_image>> key_images = wallet->export_key_images(all);
    MTRACE("Fetched " << key_images.size() << " key images");

    // wrap and serialize key images
    rapidjson::Document doc;
    doc.SetObject();
    doc.AddMember("keyImages", monero_utils::to_rapidjson_val(doc.GetAllocator(), key_images), doc.GetAllocator());
    string key_images_json = monero_utils::serialize(doc);
    return env->NewStringUTF(key_images_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_importKeyImagesJni(JNIEnv* env, jobject instance, jstring jkey_images_json) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_importKeyImagesJni");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_freezeOutputJni(JNIEnv* env, jobject instance, jstring jkey_image) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_freezeJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key_image = jkey_image ? env->GetStringUTFChars(jkey_image, NULL) : nullptr;
  string key_image = string(_key_image ? _key_image : "");
  env->ReleaseStringUTFChars(jkey_image, _key_image);

  // freeze output
  try {
    wallet->freeze_output(key_image);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_thawOutputJni(JNIEnv* env, jobject instance, jstring jkey_image) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_thawJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key_image = jkey_image ? env->GetStringUTFChars(jkey_image, NULL) : nullptr;
  string key_image = string(_key_image ? _key_image : "");
  env->ReleaseStringUTFChars(jkey_image, _key_image);

  // thaw output
  try {
    wallet->thaw_output(key_image);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT bool JNICALL Java_monero_wallet_MoneroWalletFull_isOutputFrozenJni(JNIEnv* env, jobject instance, jstring jkey_image) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_isFrozenJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _key_image = jkey_image ? env->GetStringUTFChars(jkey_image, NULL) : nullptr;
  string key_image = string(_key_image ? _key_image : "");
  env->ReleaseStringUTFChars(jkey_image, _key_image);

  // check if output is frozen
  try {
    return wallet->is_output_frozen(key_image);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return false;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_getDefaultFeePriorityJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getDefaultFeePriorityJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  return wallet->get_default_fee_priority();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createTxsJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_sendTxsJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize tx config
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  //MTRACE("Deserialized tx config, re-serialized: " << config->serialize());

  // create txs
  try {
    vector<shared_ptr<monero_tx_wallet>> txs = wallet->create_txs(*config);
    string tx_set_json = txs[0]->m_tx_set.get()->serialize();
    monero_utils::free(txs);
    return env->NewStringUTF(tx_set_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepUnlockedJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_sweepUnlockedJni(config)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config ? _config : "");
  env->ReleaseStringUTFChars(jconfig, _config);

  // deserialize tx config
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  //MTRACE("Deserialized tx config, re-serialized: " << config->serialize());

  try {
    
    // create txs
    vector<shared_ptr<monero_tx_wallet>> txs = wallet->sweep_unlocked(*config);

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

    // free and return
    monero_utils::free(txs);
    return env->NewStringUTF(tx_sets_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepOutputJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_sweepOutputJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _config = jconfig ? env->GetStringUTFChars(jconfig, NULL) : nullptr;
  string config_json = string(_config);
  env->ReleaseStringUTFChars(jconfig, _config);

  MTRACE("Send request json: " << config_json);

  // deserialize tx config
  shared_ptr<monero_tx_config> config = monero_tx_config::deserialize(config_json);
  MTRACE("Deserialized send request, re-serialized: " << config->serialize());

  // submit request with configuration
  try {
    shared_ptr<monero_tx_wallet> tx = wallet->sweep_output(*config);
    string tx_set_json = tx->m_tx_set.get()->serialize();
    monero_utils::free(tx);
    return env->NewStringUTF(tx_set_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepDustJni(JNIEnv* env, jobject instance, jboolean relay) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_sweepDustJni(request)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // sweep dust
  try {
    vector<shared_ptr<monero_tx_wallet>> txs = wallet->sweep_dust(relay);
    string tx_set_json = txs.empty() ? string("{}") : txs[0]->m_tx_set.get()->serialize();
    monero_utils::free(txs);
    return env->NewStringUTF(tx_set_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_describeTxSetJni(JNIEnv* env, jobject instance, jstring jtx_set_json) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_describeTxSetJson(tx_set_json)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get tx set json string
  const char* _tx_set_json = jtx_set_json ? env->GetStringUTFChars(jtx_set_json, NULL) : nullptr;
  string tx_set_json = string(_tx_set_json);
  env->ReleaseStringUTFChars(jtx_set_json, _tx_set_json);

  try {

    // deserialize tx set to describe
    monero_tx_set tx_set = monero_tx_set::deserialize(tx_set_json);

    // describe tx set
    monero_tx_set described_tx_set = wallet->describe_tx_set(tx_set);

    // serialize, free, and return
    std::string monero_tx_set_json = described_tx_set.serialize();
    monero_utils::free(described_tx_set.m_txs);
    return env->NewStringUTF(monero_tx_set_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signTxsJni(JNIEnv* env, jobject instance, jstring junsigned_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_signTxsJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // get unsigned tx set as string
  const char* _unsigned_tx_hex = junsigned_tx_hex ? env->GetStringUTFChars(junsigned_tx_hex, NULL) : nullptr;
  string unsigned_tx_hex = string(_unsigned_tx_hex ? _unsigned_tx_hex : "");
  env->ReleaseStringUTFChars(junsigned_tx_hex, _unsigned_tx_hex);

  try {

    // sign txs
    monero_tx_set signed_tx_set = wallet->sign_txs(unsigned_tx_hex);

    // serialize, free, and return
    std::string monero_tx_set_json = signed_tx_set.serialize();
    monero_utils::free(signed_tx_set.m_txs);
    return env->NewStringUTF(monero_tx_set_json.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_submitTxsJni(JNIEnv* env, jobject instance, jstring jsigned_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_submitTxsJni()");
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

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_relayTxsJni(JNIEnv* env, jobject instance, jobjectArray jtx_metadatas) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_relayTxsJni");
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
      env->DeleteLocalRef(jstr);
      tx_metadatas.push_back(str);
    }
    env->DeleteLocalRef(jtx_metadatas);
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signMessageJni(JNIEnv* env, jobject instance, jstring jmsg, jint message_signature_type, jint account_idx, jint subaddress_idx) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_signMessageJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_verifyMessageJni(JNIEnv* env, jobject instance, jstring jmsg, jstring jaddress, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_verifyMessageJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxKeyJni(JNIEnv* env, jobject instance, jstring jtx_hash) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getTxKeyJniJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkTxKeyJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jtx_key, jstring jaddress) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_checktx_keyJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jaddress, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getTxProofJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkTxProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jaddress, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_checkTxProofJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSpendProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getSpendProofJni");
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

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_checkSpendProofJni(JNIEnv* env, jobject instance, jstring jtx_hash, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_checkSpendProofJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getReserveProofWalletJni(JNIEnv* env, jobject instance, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getReserveProofWalletJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getReserveProofAccountJni(JNIEnv* env, jobject instance, jint account_idx, jstring jamount_str, jstring jmessage) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getReserveProofWalletJni");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkReserveProofJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jmessage, jstring jsignature) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_checkReserveProofAccountJni");
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

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtx_hashes) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getTxNotesJni");
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
      env->DeleteLocalRef(jstr);
      tx_hashes.push_back(str);
    }
    env->DeleteLocalRef(jtx_hashes);
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtx_hashes, jobjectArray jtx_notes) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setTxNotesJni");
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
      env->DeleteLocalRef(jstr);
      tx_hashes.push_back(str);
    }
    env->DeleteLocalRef(jtx_hashes);
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
      env->DeleteLocalRef(jstr);
      notes.push_back(str);
    }
    env->DeleteLocalRef(jtx_notes);
  }

  // set tx notes
  try {
    wallet->set_tx_notes(tx_hashes, notes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressBookEntriesJni(JNIEnv* env, jobject instance, jintArray jindices) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAddressBookEntriesJni");
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
JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_addAddressBookEntryJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jdescription) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_addAddressBookEntryJni");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_editAddressBookEntryJni(JNIEnv* env, jobject instance, jint index, jboolean set_address, jstring jaddress, jboolean set_description, jstring jdescription) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_editAddressBookEntryJni");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_deleteAddressBookEntryJni(JNIEnv* env, jobject instance, jint index) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_deleteAddressBookEntryJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);

  // delete address book entry
  try {
    wallet->delete_address_book_entry(index);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPaymentUriJni(JNIEnv* env, jobject instance, jstring jconfig) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getPaymentUriJni()");
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
    payment_uri = wallet->get_payment_uri(*config.get());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return
  return env->NewStringUTF(payment_uri.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_parsePaymentUriJni(JNIEnv* env, jobject instance, jstring juri) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_parsePaymentUriJni()");
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAttributeJni(JNIEnv* env, jobject instance, jstring jkey) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getAttribute()");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setAttributeJni(JNIEnv* env, jobject instance, jstring jkey, jstring jval) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_setAttribute()");
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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_startMiningJni(JNIEnv* env, jobject instance, jlong num_threads, jboolean background_mining, jboolean ignore_battery) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_startMiningJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->start_mining(num_threads, background_mining, ignore_battery);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_stopMiningJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_startMiningJni()");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->stop_mining();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isMultisigImportNeededJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_isMultisigImportNeededJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    bool is_multisig_import_needed = wallet->is_multisig_import_needed();
    return static_cast<jboolean>(is_multisig_import_needed);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getMultisigInfoJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getMultisigInfoJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    monero_multisig_info info = wallet->get_multisig_info();
    return env->NewStringUTF(info.serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_prepareMultisigJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_prepareMultisigJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    string multisig_hex = wallet->prepare_multisig();
    return env->NewStringUTF(multisig_hex.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_makeMultisigJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes, jint threshold, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_makeMultisigJni");

  // get multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      env->DeleteLocalRef(jstr);
      multisig_hexes.push_back(str);
    }
    env->DeleteLocalRef(jmultisig_hexes);
  }

  // get password as string
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);

  // make the wallet multisig and return result
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    string multisig_hex = wallet->make_multisig(multisig_hexes, threshold, password);
    return env->NewStringUTF(multisig_hex.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exchangeMultisigKeysJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_exchangeMultisigKeysJni");

  // get multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      env->DeleteLocalRef(jstr);
      multisig_hexes.push_back(str);
    }
    env->DeleteLocalRef(jmultisig_hexes);
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportMultisigHexJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_exportMultisigHexJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    string multisig_hex = wallet->export_multisig_hex();
    return env->NewStringUTF(multisig_hex.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_importMultisigHexJni(JNIEnv* env, jobject instance, jobjectArray jmultisig_hexes) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_importMultisigHexJni");

  // get peer multisig hex as vector<string>
  vector<string> multisig_hexes;
  if (jmultisig_hexes != nullptr) {
    jsize size = env->GetArrayLength(jmultisig_hexes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jmultisig_hexes, idx);
      const char* _str = jstr ? env->GetStringUTFChars(jstr, NULL) : nullptr;
      string str = string(_str ? _str : "");
      env->ReleaseStringUTFChars(jstr, _str);
      env->DeleteLocalRef(jstr);
      multisig_hexes.push_back(str);
    }
    env->DeleteLocalRef(jmultisig_hexes);
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

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signMultisigTxHexJni(JNIEnv* env, jobject instance, jstring jmultisig_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_signMultisigTxHexJni");

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

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_submitMultisigTxHexJni(JNIEnv* env, jobject instance, jstring jsigned_multisig_tx_hex) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_submitMultisigTxHexJni");

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

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_changePasswordJni(JNIEnv* env, jobject instance, jstring jold_password, jstring jnew_password) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_changePasswordJni(oldPassword, newPassword)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  const char* _old_password = jold_password ? env->GetStringUTFChars(jold_password, NULL) : nullptr;
  const char* _new_password = jnew_password ? env->GetStringUTFChars(jnew_password, NULL) : nullptr;
  string old_password = string(_old_password ? _old_password : "");
  string new_password = string(_new_password ? _new_password : "");
  try {
    wallet->change_password(old_password, new_password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_moveToJni(JNIEnv* env, jobject instance, jstring jpath, jstring jpassword) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_moveToJni(path, password)");
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string path = string(_path ? _path : "");
  env->ReleaseStringUTFChars(jpath, _path);
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->move_to(path, password);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_saveJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_saveJni(path, password)");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  try {
    wallet->save();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_closeJni(JNIEnv* env, jobject instance, jboolean save) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_CloseJni");
  monero_wallet* wallet = get_handle<monero_wallet>(env, instance, JNI_WALLET_HANDLE);
  if (save) wallet->save();
  delete wallet;
  wallet = nullptr;
}

JNIEXPORT jbyteArray JNICALL Java_monero_wallet_MoneroWalletFull_getKeysFileBufferJni(JNIEnv* env, jobject instance, jstring jpassword, jboolean view_only) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getKeysFileBufferJni(password, view_only)");
  monero_wallet_full* wallet = get_handle<monero_wallet_full>(env, instance, JNI_WALLET_HANDLE);
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  string password = string(_password ? _password : "");
  env->ReleaseStringUTFChars(jpassword, _password);
  try {

    // get keys buffer
    std::string keys_buf = wallet->get_keys_file_buffer(password, view_only);

    // create java object
    jbyteArray jkeys_buf = env->NewByteArray(keys_buf.length());
    env->SetByteArrayRegion(jkeys_buf, 0, keys_buf.length(), (jbyte*) keys_buf.c_str());
    return jkeys_buf;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jbyteArray JNICALL Java_monero_wallet_MoneroWalletFull_getCacheFileBufferJni(JNIEnv* env, jobject instance) {
  MTRACE("Java_monero_wallet_MoneroWalletFull_getCacheFileBufferJni()");
  monero_wallet_full* wallet = get_handle<monero_wallet_full>(env, instance, JNI_WALLET_HANDLE);
  try {

      // get cache buffer
      std::string cache_buf = wallet->get_cache_file_buffer();

      // create java object
      jbyteArray jcache_buf = env->NewByteArray(cache_buf.length());
      env->SetByteArrayRegion(jcache_buf, 0, cache_buf.length(), (jbyte*) cache_buf.c_str());
      return jcache_buf;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

#ifdef __cplusplus
}
#endif
