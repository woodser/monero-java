#include <jni.h>
#include <iostream>
#include "monero_cpp_bridge_MoneroCppUtils.h"
using namespace std;

JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_sayHello(JNIEnv *env, jclass utilClass) {
  cout << "Hello World from C++!" << endl;
}

JNIEXPORT jintArray JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_mapToBinary(JNIEnv *env, jclass utilClass, jobject jmap) {
  cout << "cpp_bridge.mapToBinary()" << endl;
//  jintArray intArray = env->NewIntArray(5);
//  jint values[5] = {69, 69, 69, 69, 69};
//  env->SetIntArrayRegion(intArray, 0, 5, values);
//  env->ReleaseIntArrayElements(intArray, values, 0);
//  return intArray;

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

//JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_binaryToMap(JNIEnv *env, jclass utilClass, jintArray jbin, jobject jmap) {
//
//}
 d