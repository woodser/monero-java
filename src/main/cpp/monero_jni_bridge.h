/**
 * Copyright (c) woodser
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

#ifndef _Included_MoneroJniBridge // TODO: rename
#define _Included_MoneroJniBridge

// TODO: this causes warning
std::string jstring2string(JNIEnv *env, jstring jStr);

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

// ------------------------------ STATIC UTILS --------------------------------

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_getIntegratedAddressJni(JNIEnv *, jclass, jint, jstring, jstring);

JNIEXPORT jbyteArray JNICALL Java_monero_common_MoneroUtils_jsonToBinaryJni(JNIEnv *, jclass, jstring);

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_binaryToJsonJni(JNIEnv *, jclass, jbyteArray);

JNIEXPORT jstring JNICALL Java_monero_common_MoneroUtils_binaryBlocksToJsonJni(JNIEnv *, jclass, jbyteArray);

JNIEXPORT void JNICALL Java_monero_common_MoneroUtils_initLoggingJni(JNIEnv *, jclass, jstring jpath, jboolean);

JNIEXPORT void JNICALL Java_monero_common_MoneroUtils_setLogLevelJni(JNIEnv *, jclass, jint);

// --------------------------- STATIC WALLET UTILS ----------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_walletExistsJni(JNIEnv *, jclass, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_openWalletJni(JNIEnv *, jclass, jstring, jstring, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_openWalletDataJni(JNIEnv *, jclass, jstring, jint, jbyteArray, jbyteArray);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_createWalletJni(JNIEnv *, jclass, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getMnemonicLanguagesJni(JNIEnv *, jclass);

// ----------------------------- INSTANCE METHODS -----------------------------

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isViewOnlyJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setDaemonConnectionJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonConnectionJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isConnectedToDaemonJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isDaemonSyncedJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isSyncedJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getVersionJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPathJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_getNetworkTypeJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getMnemonicJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getMnemonicLanguageJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPublicViewKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPrivateViewKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPublicSpendKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPrivateSpendKeyJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressIndexJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getIntegratedAddressJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_decodeIntegratedAddressJni(JNIEnv *, jobject, jstring);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getSyncHeightJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setSyncHeightJni(JNIEnv *, jobject, jlong);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getDaemonMaxPeerHeightJni(JNIEnv *, jobject);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_getHeightByDateJni(JNIEnv *, jobject, jint, jint, jint);

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletFull_setListenerJni(JNIEnv *, jobject, jobject);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_syncJni(JNIEnv *, jobject, jlong);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_startSyncing(JNIEnv *, jobject, jlong);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_stopSyncing(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_scanTxsJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_rescanSpentJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_rescanBlockchainJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceWalletJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceAccountJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getBalanceSubaddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceWalletJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceAccountJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getUnlockedBalanceSubaddressJni(JNIEnv *, jobject, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAccountsJni(JNIEnv *, jobject, jboolean, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAccountJni(JNIEnv *, jobject, jint, jboolean);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createAccountJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSubaddressesJni(JNIEnv *, jobject, jint, jintArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createSubaddressJni(JNIEnv *, jobject, jint, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setSubaddressLabelJni(JNIEnv *, jobject, jint, jint, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTransfersJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getOutputsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportKeyImagesJni(JNIEnv *, jobject, jboolean);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_importKeyImagesJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_freezeOutputJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_thawOutputJni(JNIEnv *, jobject, jstring);

JNIEXPORT bool JNICALL Java_monero_wallet_MoneroWalletFull_isOutputFrozenJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_createTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepUnlockedJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepOutputJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_sweepDustJni(JNIEnv *, jobject, jboolean);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_describeTxSetJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_submitTxsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_relayTxsJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signMessageJni(JNIEnv *, jobject, jstring, jint, jint, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_verifyMessageJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxKeyJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkTxKeyJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getTxProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkTxProofJni(JNIEnv *, jobject, jstring, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getSpendProofJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_checkSpendProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getReserveProofWalletJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getReserveProofAccountJni(JNIEnv *, jobject, jint, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_checkReserveProofJni(JNIEnv *, jobject, jstring, jstring, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getPaymentUriJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_parsePaymentUriJni(JNIEnv *, jobject, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportOutputsJni(JNIEnv *, jobject, jboolean);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_importOutputsJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_getTxNotesJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setTxNotesJni(JNIEnv *, jobject, jobjectArray, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAddressBookEntriesJni(JNIEnv *, jobject, jintArray);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_addAddressBookEntryJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_editAddressBookEntryJni(JNIEnv *, jobject, jint, jboolean, jstring, jboolean, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_deleteAddressBookEntryJni(JNIEnv *, jobject, jint);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getAttributeJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_setAttributeJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_startMiningJni(JNIEnv *, jobject, jlong, jboolean, jboolean);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_stopMiningJni(JNIEnv *, jobject);

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletFull_isMultisigImportNeededJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_getMultisigInfoJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_prepareMultisigJni(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_makeMultisigJni(JNIEnv *, jobject, jobjectArray, jint, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exchangeMultisigKeysJni(JNIEnv *, jobject, jobjectArray, jstring);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_exportMultisigHexJni(JNIEnv *, jobject);

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletFull_importMultisigHexJni(JNIEnv *, jobject, jobjectArray);

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletFull_signMultisigTxHexJni(JNIEnv *, jobject, jstring);

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletFull_submitMultisigTxHexJni(JNIEnv *, jobject, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_changePasswordJniJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_moveToJni(JNIEnv *, jobject, jstring, jstring);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_saveJni(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletFull_closeJni(JNIEnv *, jobject, jboolean);

JNIEXPORT jbyteArray JNICALL Java_monero_wallet_MoneroWalletFull_getKeysFileBufferJni(JNIEnv *, jobject, jstring, jboolean);

JNIEXPORT jbyteArray JNICALL Java_monero_wallet_MoneroWalletFull_getCacheFileBufferJni(JNIEnv *, jobject, jstring);

#ifdef __cplusplus
}
#endif
#endif
