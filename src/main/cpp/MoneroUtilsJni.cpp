#include <jni.h>
#include <iostream>
#include "MoneroUtilsJni.h"
#include "utils/MoneroUtils.h"
using namespace std;

JNIEXPORT jbyteArray JNICALL Java_monero_utils_MoneroCppUtils_jsonToBinaryJni(JNIEnv *env, jclass utilsClass, jstring json) {

  // convert json jstring to string
  string jsonStr = jstring2string(env, json);
  //string jsonStr = "{\"heights\":[123456,1234567,870987]}";

  // convert json to monero's portable storage binary format
  string binStr;
  MoneroUtils::jsonToBinary(jsonStr, binStr);

  // convert binary string to jbyteArray
  jbyteArray result = env->NewByteArray(binStr.length());
  if (result == NULL) {
     return NULL; // out of memory error thrown
  }

  // fill a temp structure to use to populate the java byte array
  jbyte fill[binStr.length()];
  for (int i = 0; i < binStr.length(); i++) {
     fill[i] = binStr[i];
  }
  env->SetByteArrayRegion(result, 0, binStr.length(), fill);
  return result;
}

JNIEXPORT jstring JNICALL Java_monero_utils_MoneroCppUtils_binaryToJsonJni(JNIEnv *env, jclass utilsClass, jbyteArray bin) {

  // convert the jbyteArray to a string
  int binLength = env->GetArrayLength(bin);
  jboolean isCopy;
  jbyte* jbytes = env->GetByteArrayElements(bin, &isCopy);
  string binStr = string((char*) jbytes, binLength);

  // convert monero's portable storage binary format to json
  string jsonStr;
  MoneroUtils::binaryToJson(binStr, jsonStr);

  // convert string to jstring
  return env->NewStringUTF(jsonStr.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_utils_MoneroCppUtils_binaryBlocksToJsonJni(JNIEnv *env, jclass utilsClass, jbyteArray blocksBin) {

  // convert the jbyteArray to a string
  int binLength = env->GetArrayLength(blocksBin);
  jboolean isCopy;
  jbyte* jbytes = env->GetByteArrayElements(blocksBin, &isCopy);
  string binStr = string((char*) jbytes, binLength);

  // convert monero's portable storage binary format to json
  string jsonStr;
  MoneroUtils::binaryBlocksToJson(binStr, jsonStr);

  // convert string to jstring
  return env->NewStringUTF(jsonStr.c_str());
}

// credit: https://stackoverflow.com/questions/41820039/jstringjni-to-stdstringc-with-utf8-characters
std::string jstring2string(JNIEnv *env, jstring jStr) {
    if (!jStr)
        return "";

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
