/**
 * Copyright (c) 2017-2020 woodser
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

#include <jni.h>

#ifndef _Included_MoneroWalletJniBridge
#define _Included_MoneroWalletJniBridge

jfieldID get_handle_field(JNIEnv *env, jobject obj, const char *field_name) {
  jclass c = env->GetObjectClass(obj);
  return env->GetFieldID(c, field_name, "J"); // of type long
}

template<typename T>
T *get_handle(JNIEnv *env, jobject obj, const char *field_name) {
  jlong handle = env->GetLongField(obj, get_handle_field(env, obj, field_name));
  return reinterpret_cast<T *>(handle);
}

#ifdef __cplusplus
extern "C" {
#endif

// ------------------------------ STATIC UTILS ------------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *, jclass, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *, jclass, jstring, jstring, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *, jclass, jstring, jstring, jint, jstring, jstring, jstring, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *, jclass, jstring, jstring, jint, jstring, jlong, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *, jclass, jstring, jstring, jint, jstring, jstring, jstring, jlong, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicLanguagesJni(JNIEnv *, jclass);

// ----------------------------- INSTANCE METHODS -----------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isViewOnlyJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isConnectedJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isDaemonSyncedJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isSyncedJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getVersionJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicLanguageJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicSpendKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateSpendKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressIndexJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni(JNIEnv *, jobject, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getSyncHeightJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setSyncHeightJni(JNIEnv *, jobject, jlong);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonMaxPeerHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightByDateJni(JNIEnv *, jobject, jint, jint, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *, jobject, jobject);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *, jobject, jlong);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startSyncing(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopSyncing(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_rescanSpentJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_rescanBlockchainJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountsJni(JNIEnv *, jobject, jboolean, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountJni(JNIEnv *, jobject, jint, jboolean);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createAccountJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSubaddressesJni(JNIEnv *, jobject, jint, jintArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createSubaddressJni(JNIEnv *, jobject, jint, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTransfersJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getKeyImagesJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_importKeyImagesJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sendTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepUnlockedJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepOutputJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepDustJni(JNIEnv *, jobject, jboolean);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parseTxSetJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_submitTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_relayTxsJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signMessageJni(JNIEnv *, jobject, jstring, jint, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_verifyMessageJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxKeyJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxKeyJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxProofJni(JNIEnv *, jobject, jstring, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSpendProofJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_checkSpendProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofAccountJni(JNIEnv *, jobject, jint, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkReserveProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createPaymentUriJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsHexJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importOutputsHexJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getTxNotesJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setTxNotesJni(JNIEnv *, jobject, jobjectArray, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressBookEntriesJni(JNIEnv *, jobject, jintArray);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_addAddressBookEntryJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_editAddressBookEntryJni(JNIEnv *, jobject, jint, jboolean, jstring, jboolean, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_deleteAddressBookEntryJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAttributeJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setAttributeJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startMiningJni(JNIEnv *, jobject, jlong, jboolean, jboolean);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopMiningJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_isMultisigImportNeededJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMultisigInfoJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_prepareMultisigJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_makeMultisigJni(JNIEnv *, jobject, jobjectArray, jint, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_exchangeMultisigKeysJni(JNIEnv *, jobject, jobjectArray, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMultisigHexJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importMultisigHexJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signMultisigTxHexJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_submitMultisigTxHexJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_saveJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_moveToJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_closeJni(JNIEnv *, jobject, jboolean);

#ifdef __cplusplus
}
#endif
#endif
