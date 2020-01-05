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
 */

#include <iostream>
#include "chacha.h" // TODO: explicitly include because wallet2.h #include "crypto/chacha.h" is ignored
#include "monero_utils_jni_bridge.h"
#include "utils/monero_utils.h"

using namespace std;

JNIEXPORT jbyteArray JNICALL Java_monero_utils_MoneroUtilsJni_jsonToBinaryJni(JNIEnv *env, jclass clazz, jstring json) {

  // convert json jstring to string
  string json_str = jstring2string(env, json);
  //string json_str = "{\"heights\":[123456,1234567,870987]}";

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

JNIEXPORT jstring JNICALL Java_monero_utils_MoneroUtilsJni_binaryToJsonJni(JNIEnv *env, jclass clazz, jbyteArray bin) {

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

JNIEXPORT jstring JNICALL Java_monero_utils_MoneroUtilsJni_binaryBlocksToJsonJni(JNIEnv *env, jclass clazz, jbyteArray blocks_bin) {

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

JNIEXPORT void JNICALL Java_monero_utils_MoneroUtilsJni_initLoggingJni(JNIEnv* env, jclass clazz, jstring jpath, jboolean console) {
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  string path = string(_path ? _path : "");
  env->ReleaseStringUTFChars(jpath, _path);
  mlog_configure(path, console);
}

JNIEXPORT void JNICALL Java_monero_utils_MoneroUtilsJni_setLogLevelJni(JNIEnv* env, jclass clazz, jint level) {
  mlog_set_log_level(level);
}
