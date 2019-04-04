#include <jni.h>
#include <iostream>
#include "monero_cpp_bridge_MoneroCppUtilsJni.h"
#include "monero_binary_utils.hpp"
//#include "./submodules/mymonero-core-cpp/src/monero_binary_utils.hpp"
using namespace std;

JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_sayHello(JNIEnv *env, jclass utilClass) {
  cout << "Hello World from C++!" << endl;
  binary_utils::say_hello_binary_utils();
}

JNIEXPORT jbyteArray JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_jsonToBinary(JNIEnv *env, jclass utilsClass, jstring json) {

  // convert json jstring to string
  string jsonStr = jstring2string(env, json);
  cout << "Converted: " << jsonStr << endl;
  //string jsonStr = "{\"heights\":[123456,1234567,870987]}";

  // convert json to monero's portable storage binary format
  string binStr;
  binary_utils::json_to_binary(jsonStr, binStr);
  cout << "Binary: " << binStr << endl;
  cout << "Binary length " << binStr.length() << endl;

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




//

//  int size = 5;
//  jbyteArray result;
//  result = (env)->NewByteArray(size);
//  if (result == NULL) {
//     return NULL; /* out of memory error thrown */
//  }
//  int i;
//  // fill a temp structure to use to populate the java int array
//  jbyte fill[size];
//  for (i = 0; i < size; i++) {
//     fill[i] = 2; // put whatever logic you want to populate the values here.
//  }
//  // move from the temp structure to the java structure
//  (env)->SetByteArrayRegion(result, 0, size, fill);
//  return result;

//  // convert jstring to string
//  const char *str = env->GetStringUTFChars(json, 0);
//  cout << str << endl;
//  env->ReleaseStringUTFChars(json, str);
}

JNIEXPORT jstring JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_binaryToJson(JNIEnv *env, jclass utilsClass, jbyteArray bin) {

  // convert the jbyteArray to a string
  cout << "Binary: " << bin << endl;
  int binLength = env->GetArrayLength(bin);
  cout << "Binary length: " << binLength << endl;
  jboolean isCopy;
  jbyte* jbytes = env->GetByteArrayElements(bin, &isCopy);
  string binStr = string((char*) jbytes, binLength);

  //string binStr = "temp";

  // convert monero's portable storage binary format to json
  string jsonStr;
  binary_utils::binary_to_json(binStr, jsonStr);

  // convert string to jstring
  return env->NewStringUTF(jsonStr.c_str());

  //string str = "{\"heights\":[123456,1234567,870987]}";
}

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
