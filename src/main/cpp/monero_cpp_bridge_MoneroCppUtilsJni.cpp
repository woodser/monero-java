#include <jni.h>
#include <iostream>
#include "monero_cpp_bridge_MoneroCppUtilsJni.h"
using namespace std;

JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_sayHello(JNIEnv *env, jclass utilClass) {
  cout << "Hello World from C++!" << endl;
}

//JNIEXPORT jintArray JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_mapToBinary(JNIEnv *env, jclass utilClass, jobject jmap) {
//  cout << "cpp_bridge.mapToBinary()" << endl;
////  jintArray intArray = env->NewIntArray(5);
////  jint values[5] = {69, 69, 69, 69, 69};
////  env->SetIntArrayRegion(intArray, 0, 5, values);
////  env->ReleaseIntArrayElements(intArray, values, 0);
////  return intArray;
//}

JNIEXPORT jintArray JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_jsonToBinary(JNIEnv *env, jclass utilsClass, jstring json) {
  int size = 5;
  jintArray result;
  result = (env)->NewIntArray(size);
  if (result == NULL) {
     return NULL; /* out of memory error thrown */
  }
  int i;
  // fill a temp structure to use to populate the java int array
  jint fill[size];
  for (i = 0; i < size; i++) {
     fill[i] = 2; // put whatever logic you want to populate the values here.
  }
  // move from the temp structure to the java structure
  (env)->SetIntArrayRegion(result, 0, size, fill);
  return result;
}

JNIEXPORT jstring JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_binaryToJson(JNIEnv *env, jclass utilsClass, jintArray bin) {
  string str = "{\"heights\":[123456,1234567,870987]}";
  return env->NewStringUTF(str.c_str());
}

//JNIEXPORT jstring JNICALL Java_monero_cpp_1bridge_MoneroCppUtilsJni_binaryBlocksToJson(JNIEnv *env, jclass utilsClass, jintArray bin) {
//
//}
