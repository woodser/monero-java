#include <jni.h>

#ifndef _Included_monero_wallet_MoneroWalletJni
#define _Included_monero_wallet_MoneroWalletJni

jfieldID getHandleField(JNIEnv *env, jobject obj, const char *fieldName) {
  jclass c = env->GetObjectClass(obj);
  return env->GetFieldID(c, fieldName, "J"); // of type long
}

template<typename T>
T *getHandle(JNIEnv *env, jobject obj, const char *fieldName) {
  jlong handle = env->GetLongField(obj, getHandleField(env, obj, fieldName));
  return reinterpret_cast<T *>(handle);
}

#ifdef __cplusplus
extern "C" {
#endif

// ----------------------------- WALLET MANAGEMENT ----------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *, jclass, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *, jclass, jstring, jstring, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletJni(JNIEnv *, jclass, jstring, jstring, jstring, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *, jclass, jstring, jstring, jint, jstring, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *, jclass, jstring, jstring, jstring, jint, jstring, jstring, jstring, jint);

// ----------------------------- INSTANCE METHODS -----------------------------

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *, jobject, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *, jobject, jint);	// TODO: return sync results

#ifdef __cplusplus
}
#endif
#endif
