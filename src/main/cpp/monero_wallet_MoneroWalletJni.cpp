#include <jni.h>
#include <iostream>
#include "monero_wallet_MoneroWalletJni.h"
#include "wallet2_api.h"
using namespace std;

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jint networkType) {
    const char* _path = env->GetStringUTFChars(path, NULL);
    const char* _password = env->GetStringUTFChars(password, NULL);
    Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);

    Bitmonero::Wallet *wallet =
            Bitmonero::WalletManagerFactory::getWalletManager()->openWallet(
                    std::string(_path),
                    std::string(_password),
                    _networkType);

    env->ReleaseStringUTFChars(path, _path);
    env->ReleaseStringUTFChars(password, _password);
    return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jint JNICALL
Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  return Bitmonero::WalletManagerFactory::getWalletManager()->blockchainHeight();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  return env->NewStringUTF(wallet->seed().c_str());
}

//JNIEXPORT jstring JNICALL
//Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
//  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
//  wallet->balanceAll();
//  std::string balanceStr = "2";	// TODO
//  return env->NewStringUTF(balanceStr.c_str());
//}
