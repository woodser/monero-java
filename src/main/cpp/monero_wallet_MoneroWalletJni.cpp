#include <jni.h>
#include <iostream>
#include "monero_wallet_MoneroWalletJni.h"
#include "wallet2_api.h"
using namespace std;

JNIEXPORT jboolean JNICALL
Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring path) {
  const char* _path = env->GetStringUTFChars(path, NULL);
  bool exists = Bitmonero::WalletManagerFactory::getWalletManager()->walletExists(std::string(_path));
  env->ReleaseStringUTFChars(path, _path);
  return static_cast<jboolean>(exists);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jint networkType) {
  const char* _path = env->GetStringUTFChars(path, NULL);
  const char* _password = env->GetStringUTFChars(password, NULL);
  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);

  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->openWallet(std::string(_path), std::string(_password), _networkType);

  env->ReleaseStringUTFChars(path, _path);
  env->ReleaseStringUTFChars(password, _password);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jstring language, jint networkType) {
  cout << "createWalletJni" << endl;
  const char *_path = env->GetStringUTFChars(path, NULL);
  const char *_password = env->GetStringUTFChars(password, NULL);
  const char *_language = env->GetStringUTFChars(language, NULL);
  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);

  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->createWallet(std::string(_path), std::string(_password), std::string(_language), _networkType);

  env->ReleaseStringUTFChars(path, _path);
  env->ReleaseStringUTFChars(password, _password);
  env->ReleaseStringUTFChars(language, _language);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jint networkType, jstring mnemonic, jint restoreHeight) {
  const char *_path = env->GetStringUTFChars(path, NULL);
  const char *_password = env->GetStringUTFChars(password, NULL);
  const char *_mnemonic = env->GetStringUTFChars(mnemonic, NULL);
  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);

  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->recoveryWallet(std::string(_path), std::string(_password), std::string(_mnemonic), _networkType, (uint64_t) restoreHeight);

  env->ReleaseStringUTFChars(path, _path);
  env->ReleaseStringUTFChars(password, _password);
  env->ReleaseStringUTFChars(mnemonic, _mnemonic);
  return reinterpret_cast<jlong>(wallet);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL
Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jstring language, jint networkType, jstring address, jstring viewKey, jstring spendKey, jint restoreHeight) {
  const char *_path = env->GetStringUTFChars(path, NULL);
  const char *_password = env->GetStringUTFChars(password, NULL);
  const char *_language = env->GetStringUTFChars(language, NULL);
  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);
  const char *_address = env->GetStringUTFChars(address, NULL);
  const char *_viewKey = env->GetStringUTFChars(viewKey, NULL);
  const char *_spendKey = env->GetStringUTFChars(spendKey, NULL);

  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->createWalletFromKeys( std::string(_path), std::string(_password), std::string(_language), _networkType, (uint64_t) restoreHeight, std::string(_address), std::string(_viewKey), std::string(_spendKey));

  env->ReleaseStringUTFChars(path, _path);
  env->ReleaseStringUTFChars(password, _password);
  env->ReleaseStringUTFChars(language, _language);
  env->ReleaseStringUTFChars(address, _address);
  env->ReleaseStringUTFChars(viewKey, _viewKey);
  env->ReleaseStringUTFChars(spendKey, _spendKey);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT void JNICALL
Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring url, jstring username, jstring password) {
  //Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  cout << "Setting daemon connection in c++: " << url << ", " << username << ", " << password << endl;
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  return env->NewStringUTF(wallet->path().c_str());
}

JNIEXPORT jint JNICALL
Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  return wallet->nettype();
}

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  return env->NewStringUTF(wallet->getSeedLanguage().c_str());
}

JNIEXPORT jlong JNICALL
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

JNIEXPORT jstring JNICALL
Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  Bitmonero::Wallet *wallet = getHandle<Bitmonero::Wallet>(env, instance);
  return env->NewStringUTF(wallet->address((uint32_t) accountIdx, (uint32_t) subaddressIdx).c_str());
}
