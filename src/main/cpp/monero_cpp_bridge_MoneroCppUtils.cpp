#include <jni.h>
#include <iostream>
#include "monero_cpp_bridge_MoneroCppUtils.h"
using namespace std;

JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_sayHello(JNIEnv *env, jclass utilClass) {
  cout << "Hello World from C++!" << endl;
}

//JNIEXPORT jintArray JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_mapToBinary(JNIEnv *env, jclass utilClass, jobject jmap) {
//
//}
//
//JNIEXPORT void JNICALL Java_monero_cpp_1bridge_MoneroCppUtils_binaryToMap(JNIEnv *env, jclass utilClass, jintArray jbin, jobject jmap) {
//
//}
