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

  // convert jstring to string
  const char *str = env->GetStringUTFChars(json, 0);
  //printf(“%s”, str);

  cout << str << endl;

  // release string
  env->ReleaseStringUTFChars(json, str);

  int size = 5;
  jbyteArray result;
  result = (env)->NewByteArray(size);
  if (result == NULL) {
     return NULL; /* out of memory error thrown */
  }
  int i;
  // fill a temp structure to use to populate the java int array
  jbyte fill[size];
  for (i = 0; i < size; i++) {
     fill[i] = 2; // put whatever logic you want to populate the values here.
  }
  // move from the temp structure to the java structure
  (env)->SetByteArrayRegion(result, 0, size, fill);
  return result;
}

JNIEXPORT jstring JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_binaryToJson(JNIEnv *env, jclass utilsClass, jbyteArray bin) {
  string str = "{\"heights\":[123456,1234567,870987]}";
  return env->NewStringUTF(str.c_str());
}
