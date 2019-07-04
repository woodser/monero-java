#include <iostream>
#include "MoneroWalletJni.h"
#include "utils/MoneroUtils.h"

// TODO: add warnings to all deserialization methods for unrecognized fields
// TODO: do rethrows return before memory freed causing memory leak? re-write like Java_monero_wallet_MoneroWalletJni_checkTxKeyJni

using namespace std;
using namespace monero;

// --------------------------------- LISTENER ---------------------------------

// ---- START MONERUJO-BASED CODE ----

/**
 * Copyright (c) 2017 m2049r
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifdef __cplusplus
extern "C"
{
#endif

static JavaVM *cachedJVM;
//static jclass class_ArrayList;
static jclass class_WalletListener;
//static jclass class_TransactionInfo;
//static jclass class_Transfer;
//static jclass class_Ledger;

std::mutex _listenerMutex;

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
  cachedJVM = jvm;
  JNIEnv *jenv;
  if (jvm->GetEnv(reinterpret_cast<void **>(&jenv), JNI_VERSION_1_6) != JNI_OK) {
    return -1;
  }

//  class_ArrayList = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("java/util/ArrayList")));
//  class_TransactionInfo = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/TransactionInfo")));
//  class_Transfer = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/model/Transfer")));
  class_WalletListener = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("monero/wallet/MoneroWalletJni$WalletJniListener")));
//  class_Ledger = static_cast<jclass>(jenv->NewGlobalRef(jenv->FindClass("com/m2049r/xmrwallet/ledger/Ledger")));
  return JNI_VERSION_1_6;
}
#ifdef __cplusplus
}
#endif

int attachJVM(JNIEnv **jenv) {
  int envStat = cachedJVM->GetEnv((void **) jenv, JNI_VERSION_1_6);
  if (envStat == JNI_EDETACHED) {
    if (cachedJVM->AttachCurrentThread((void **) jenv, nullptr) != 0) {
      return JNI_ERR;
    }
  } else if (envStat == JNI_EVERSION) {
    return JNI_ERR;
  }
  return envStat;
}

void detachJVM(JNIEnv *jenv, int envStat) {
  if (jenv->ExceptionCheck()) {
    jenv->ExceptionDescribe();
  }
  if (envStat == JNI_EDETACHED) {
    cachedJVM->DetachCurrentThread();
  }
}

// ---- END MONERUJO-BASE CODE ----

/**
 * Listens for wallet notifications and notifies the cpp listener in Java.
 */
struct WalletJniListener : public MoneroWalletListener {
  jobject jlistener;

  WalletJniListener(JNIEnv* env, jobject listener) {
    jlistener = env->NewGlobalRef(listener);
  }

  ~WalletJniListener() { };

  void deleteGlobalJavaRef(JNIEnv *env) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    env->DeleteGlobalRef(jlistener);
    jlistener = nullptr;
  }

  virtual void onNewBlock(MoneroBlock& block) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *jenv;
    int envStat = attachJVM(&jenv);
    if (envStat == JNI_ERR) return;

    jlong jheight = static_cast<jlong>(*block.height);
    jmethodID listenerClass_onNewBlock = jenv->GetMethodID(class_WalletListener, "onNewBlock", "(J)V");
    jenv->CallVoidMethod(jlistener, listenerClass_onNewBlock, jheight);
    detachJVM(jenv, envStat);
  }

  virtual void onSyncProgress(uint64_t startHeight, uint64_t numBlocksDone, uint64_t numBlocksTotal, double percentDone, string& message) {
    std::lock_guard<std::mutex> lock(_listenerMutex);
    if (jlistener == nullptr) return;
    JNIEnv *jenv;
    int envStat = attachJVM(&jenv);
    if (envStat == JNI_ERR) return;

    // prepare callback arguments
    jlong jstartHeight = static_cast<jlong>(startHeight);
    jlong jnumBlocksDone = static_cast<jlong>(numBlocksDone);
    jlong jnumBlocksTotal = static_cast<jlong>(numBlocksTotal);
    jdouble jpercentDone = static_cast<jdouble>(percentDone);
    jstring jmessage = jenv->NewStringUTF(message.c_str());

    jmethodID listenerClass_onSyncProgress = jenv->GetMethodID(class_WalletListener, "onSyncProgress", "(JJJDLjava/lang/String;)V");
    jenv->CallVoidMethod(jlistener, listenerClass_onSyncProgress, jstartHeight, jnumBlocksDone, jnumBlocksTotal, jpercentDone, jmessage);
    jenv->DeleteLocalRef(jmessage);
    detachJVM(jenv, envStat);
  }
};

// ------------------------- RESPONSE CONTAINER STRUCTS -----------------------

struct AccountsContainer {
  vector<MoneroAccount> accounts;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(accounts)
  END_KV_SERIALIZE_MAP()
};

struct SubaddressesContainer {
  vector<MoneroSubaddress> subaddresses;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(subaddresses)
  END_KV_SERIALIZE_MAP()
};

struct BlocksContainer {
  vector<MoneroBlock> blocks;
  BEGIN_KV_SERIALIZE_MAP()
    KV_SERIALIZE(blocks)
  END_KV_SERIALIZE_MAP()
};

// ----------------------------- COMMON HELPERS -------------------------------

void setDaemonConnection(JNIEnv *env, MoneroWallet* wallet, jstring juri, jstring jusername, jstring jpassword) {
  const char* _url = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;
  const char* _username = jusername ? env->GetStringUTFChars(jusername, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;

  // set daemon connection
  wallet->setDaemonConnection(string(juri ? _url : ""), string(jusername ? _username : ""), string(jpassword ? _password : ""));

  env->ReleaseStringUTFChars(juri, _url);
  env->ReleaseStringUTFChars(jusername, _username);
  env->ReleaseStringUTFChars(jpassword, _password);
}

//void toModel(const boost::property_tree::ptree& root, MoneroTxRequest& request) {
//
//  // print node to convert for shiggles
//  std::stringstream ss;
//  boost::property_tree::write_json(ss, root, false);
//  string serialized = ss.str();
//  cout << "Converting property tree to MoneroTxRequest:  " << serialized << endl;
//
//  // initialize block
//  MoneroBlock block;
//  blockNodeToModel(root, block);
//
//  // initialize tx request extensions
//  for (boost::property_tree::ptree::const_iterator it = root.begin(); it != root.end(); ++it) {
//    string key = it->first;
//    if (key == string("isOutgoing")) {
//      cout << "Handling isOutgoing key" << endl;
//      request.isOutgoing = shared_ptr<bool>(make_shared<bool>(boost::lexical_cast<bool>(it->second.data())));
//      cout << *request.isOutgoing << endl;
//    }
//  }
//
//  cout << "Block's height: " << endl;
//  throw runtime_error("Need to initialize fields specific to request");
//}

//void toModel(const boost::property_tree::ptree& node, MoneroTxWallet& tx) {
//  cout << "toModel(txWallet)" << endl;
//  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
//    string key = it->first;
//    cout << "Property tree key: " << key << endl;
//    if (key == string("height")) {
//      MoneroBlock block;
//      block.height = std::shared_ptr<uint64_t>(std::make_shared<uint64_t>((uint64_t) 7));
//      tx.block = std::shared_ptr<MoneroBlock>(std::make_shared<MoneroBlock>(block));
//    }
//  }
//}

// Based on: https://stackoverflow.com/questions/2054598/how-to-catch-jni-java-exception/2125673#2125673
void rethrow_cpp_exception_as_java_exception(JNIEnv* env) {
  try {
    throw;  // throw exception to determine and handle type
  } catch (const std::bad_alloc& e) {
    jclass jc = env->FindClass("java/lang/OutOfMemoryError");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (const std::ios_base::failure& e) {
    jclass jc = env->FindClass("java/io/IOException");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (const std::exception& e) {
    jclass jc = env->FindClass("java/lang/Exception");
    if (jc) env->ThrowNew(jc, e.what());
  } catch (...) {
    jclass jc = env->FindClass("java/lang/Exception");
    if (jc) env->ThrowNew(jc, "Unidentfied C++ exception");
  }
}

// TODO: no common utility?  make common utility
bool stringToBool(string str) {
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  if (string("true") == str) return true;
  if (string("false") == str) return false;
  return boost::lexical_cast<bool>(str);
}

void nodeToTransfer(const boost::property_tree::ptree& node, shared_ptr<MoneroTransfer> transfer) {

  // initialize transfer from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Transfer node key: " << key << endl;
    if (key == string("accountIndex")) transfer->accountIndex = it->second.get_value<uint32_t>();
  }
}

shared_ptr<MoneroTransferRequest> nodeToTransferRequest(const boost::property_tree::ptree& node) {
  shared_ptr<MoneroTransferRequest> transferRequest = shared_ptr<MoneroTransferRequest>(new MoneroTransferRequest());
  nodeToTransfer(node, transferRequest);

  // initialize request from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Transfer request node key: " << key << endl;
    if (key == string("isIncoming")) transferRequest->isIncoming = stringToBool(it->second.data());
    else if (key == string("address")) transferRequest->address = it->second.data();
    else if (key == string("addresses")) throw runtime_error("addresses not implemented");
    else if (key == string("subaddressIndex")) transferRequest->subaddressIndex = it->second.get_value<uint32_t>();
    else if (key == string("subaddressIndices")) {
      vector<uint32_t> subaddressIndices;
      for (const auto& child : it->second) subaddressIndices.push_back(child.second.get_value<uint32_t>());
      transferRequest->subaddressIndices = subaddressIndices;
    }
    else if (key == string("destinations")) throw runtime_error("destinations not implemented");
    else if (key == string("hasDestinations")) transferRequest->hasDestinations = stringToBool(it->second.data());
    else if (key == string("txRequest")) throw runtime_error("txRequest not implemented");
  }

  return transferRequest;
}

shared_ptr<MoneroKeyImage> nodeToKeyImage(const boost::property_tree::ptree& node) {

  // initialize key image from node
  shared_ptr<MoneroKeyImage> keyImage = shared_ptr<MoneroKeyImage>(new MoneroKeyImage());
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Key image node key: " << key << endl;
    if (key == string("hex")) keyImage->hex = it->second.data();
    else if (key == string("signature")) keyImage->signature = it->second.data();
  }

  return keyImage;
}

void nodeToOutput(const boost::property_tree::ptree& node, shared_ptr<MoneroOutput> output) {
  cout << "nodeToOutput()" << endl;

//  // print for debug
//  std::stringstream ss;
//  boost::property_tree::write_json(ss, node, false);
//  string receivedNode = ss.str();
//  cout << "Received node: " << receivedNode << endl;

  // initialize output from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Output node key: " << key << endl;
    if (key == string("keyImage")) output->keyImage = nodeToKeyImage(it->second);
    else if (key == string("amount")) output->amount = it->second.get_value<uint64_t>();
    else if (key == string("index")) output->index = it->second.get_value<uint32_t>();
    else if (key == string("ringOutputIndices")) throw runtime_error("nodeToTx() deserialize ringOutputIndices not implemented");
    else if (key == string("stealthPublicKey")) throw runtime_error("nodeToTx() deserialize stealthPublicKey not implemented");
  }
}

void nodeToOutputWallet(const boost::property_tree::ptree& node, shared_ptr<MoneroOutputWallet> outputWallet) {
  nodeToOutput(node, outputWallet);
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Output wallet node key: " << key << endl;
    if (key == string("accountIndex")) outputWallet->accountIndex = it->second.get_value<uint32_t>();
    else if (key == string("subaddressIndex")) outputWallet->subaddressIndex = it->second.get_value<uint32_t>();
    else if (key == string("isSpent")) outputWallet->isSpent = stringToBool(it->second.data());
    else if (key == string("isUnlocked")) outputWallet->isUnlocked = stringToBool(it->second.data());
    else if (key == string("isFrozen")) outputWallet->isFrozen = stringToBool(it->second.data());
  }
}

shared_ptr<MoneroOutputRequest> nodeToOutputRequest(const boost::property_tree::ptree& node) {
  shared_ptr<MoneroOutputRequest> outputRequest = shared_ptr<MoneroOutputRequest>(new MoneroOutputRequest());
  nodeToOutputWallet(node, outputRequest);

  // initialize request from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Output request node key: " << key << endl;
    if (key == string("subaddressIndices")) for (boost::property_tree::ptree::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) outputRequest->subaddressIndices.push_back(it2->second.get_value<uint32_t>());
    else if (key == string("txRequest")) {} // ignored
  }

  return outputRequest;
}

void nodeToTx(const boost::property_tree::ptree& node, shared_ptr<MoneroTx> tx) {
  cout << "nodeToTx()" << endl;

//  // print for debug
//  std::stringstream ss;
//  boost::property_tree::write_json(ss, node, false);
//  string receivedNode = ss.str();
//  cout << "Received node: " << receivedNode << endl;

  // initialize tx from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Tx node key: " << key << endl;
    if (key == string("id")) tx->id = it->second.data();
    else if (key == string("version")) throw runtime_error("version deserializationn not implemented");
    else if (key == string("isCoinbase")) tx->isCoinbase = stringToBool(it->second.data());
    else if (key == string("paymentId")) throw runtime_error("paymentId deserializationn not implemented");
    else if (key == string("fee")) throw runtime_error("fee deserialization not implemented");
    else if (key == string("mixin")) throw runtime_error("mixin deserialization not implemented");
    else if (key == string("doNotRelay")) tx->doNotRelay = stringToBool(it->second.data());
    else if (key == string("isRelayed")) tx->isRelayed = stringToBool(it->second.data());
    else if (key == string("isConfirmed")) tx->isConfirmed = stringToBool(it->second.data());
    else if (key == string("inTxPool")) tx->inTxPool = stringToBool(it->second.data());
    else if (key == string("numConfirmations")) throw runtime_error("numConfirmations deserialization not implemented");
    else if (key == string("unlockTime")) throw runtime_error("unlockTime deserialization not implemented");
    else if (key == string("lastRelayedTimestamp")) throw runtime_error("lastRelayedTimestamp deserialization not implemented");
    else if (key == string("receivedTimestamp")) throw runtime_error("receivedTimestamp deserializationn not implemented");
    else if (key == string("isDoubleSpend")) tx->isDoubleSpend = stringToBool(it->second.data());
    else if (key == string("key")) tx->key = it->second.data();
    else if (key == string("fullHex")) tx->fullHex = it->second.data();
    else if (key == string("prunedHex")) tx->prunedHex = it->second.data();
    else if (key == string("prunableHex")) tx->prunableHex = it->second.data();
    else if (key == string("prunableHash")) tx->prunableHash = it->second.data();
    else if (key == string("size")) throw runtime_error("size deserialization not implemented");
    else if (key == string("weight")) throw runtime_error("weight deserialization not implemented");
    else if (key == string("vins")) throw runtime_error("vins deserializationn not implemented");
    else if (key == string("vouts")) throw runtime_error("vouts deserializationn not implemented");
    else if (key == string("outputIndices")) throw runtime_error("outputIndices deserialization not implemented");
    else if (key == string("metadata")) throw runtime_error("metadata deserialization not implemented");
    else if (key == string("commonTxSets")) throw runtime_error("commonTxSets deserialization not implemented");
    else if (key == string("extra")) throw runtime_error("extra deserialization not implemented");
    else if (key == string("rctSignatures")) throw runtime_error("rctSignatures deserialization not implemented");
    else if (key == string("rctSigPrunable")) throw runtime_error("rctSigPrunable deserialization not implemented");
    else if (key == string("isKeptByBlock")) tx->isKeptByBlock = stringToBool(it->second.data());
    else if (key == string("isFailed")) tx->isFailed = stringToBool(it->second.data());
    else if (key == string("lastFailedHeight")) throw runtime_error("lastFailedHeight deserialization not implemented");
    else if (key == string("lastFailedId")) tx->lastFailedId = it->second.data();
    else if (key == string("maxUsedBlockHeight")) throw runtime_error("maxUsedBlockHeight deserialization not implemented");
    else if (key == string("maxUsedBlockId")) tx->maxUsedBlockId = it->second.data();
    else if (key == string("signatures")) throw runtime_error("signatures deserialization not implemented");
  }
}

void nodeToTxWallet(const boost::property_tree::ptree& node, shared_ptr<MoneroTxWallet> txWallet) {
  nodeToTx(node, txWallet);

  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Tx wallet node key: " << key << endl;
    //if (key == string("id")) tx->id = it->second.data();
  }
}

shared_ptr<MoneroTxRequest> nodeToTxRequest(const boost::property_tree::ptree& node) {
  shared_ptr<MoneroTxRequest> txRequest = shared_ptr<MoneroTxRequest>(new MoneroTxRequest());
  nodeToTxWallet(node, txRequest);

  // initialize request from node
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Tx request node key: " << key << endl;
    if (key == string("isOutgoing")) txRequest->isOutgoing = stringToBool(it->second.data());
    else if (key == string("isIncoming")) txRequest->isIncoming = stringToBool(it->second.data());
    else if (key == string("txIds")) for (boost::property_tree::ptree::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) txRequest->txIds.push_back(it2->second.data());
    else if (key == string("hasPaymentId")) txRequest->hasPaymentId = stringToBool(it->second.data());
    else if (key == string("paymentIds")) for (boost::property_tree::ptree::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) txRequest->paymentIds.push_back(it2->second.data());
    else if (key == string("minHeight")) txRequest->minHeight = it->second.get_value<uint32_t>();
    else if (key == string("maxHeight")) txRequest->maxHeight = it->second.get_value<uint32_t>();
    else if (key == string("includeOutputs")) txRequest->includeOutputs = stringToBool(it->second.data());
    else if (key == string("transferRequest")) txRequest->transferRequest = nodeToTransferRequest(it->second);
    else if (key == string("outputRequest")) txRequest->outputRequest = nodeToOutputRequest(it->second);
  }

  return txRequest;
}

shared_ptr<MoneroBlock> nodeToBlockRequest(const boost::property_tree::ptree& node) {
  cout << "nodeToBlockRequest()" << endl;
  shared_ptr<MoneroBlock> block = shared_ptr<MoneroBlock>(new MoneroBlock());
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Block node key: " << key << endl;
    if (key == string("height")) block->height = (uint64_t) 7;  // TODO
    else if (key == string("txs")) {
      boost::property_tree::ptree txsNode = it->second;
      for (boost::property_tree::ptree::const_iterator it2 = txsNode.begin(); it2 != txsNode.end(); ++it2) {
        block->txs.push_back(nodeToTxRequest(it2->second));
      }
    }
  }
  return block;
}

shared_ptr<MoneroTxRequest> deserializeTxRequest(const string& txRequestStr) {
  cout << "deserializeTxRequest(): " <<  txRequestStr << endl;

  // deserialize tx request string to property rooted at block
  std::istringstream iss = txRequestStr.empty() ? std::istringstream() : std::istringstream(txRequestStr);
  boost::property_tree::ptree blockNode;
  boost::property_tree::read_json(iss, blockNode);

  // convert request property tree to block
  shared_ptr<MoneroBlock> block = nodeToBlockRequest(blockNode);

  // get tx request
  shared_ptr<MoneroTxRequest> txRequest = static_pointer_cast<MoneroTxRequest>(block->txs[0]);

  cout << "Returning deserialized tx request" << endl;

  // return deserialized request
  return txRequest;
}

shared_ptr<MoneroTransferRequest> deserializeTransferRequest(const string& transferRequestStr) {
  cout << "deserializeTransferRequest(): " <<  transferRequestStr << endl;

  // deserialize transfer request string to property rooted at block
  std::istringstream iss = transferRequestStr.empty() ? std::istringstream() : std::istringstream(transferRequestStr);
  boost::property_tree::ptree blockNode;
  boost::property_tree::read_json(iss, blockNode);

  // convert request property tree to block
  shared_ptr<MoneroBlock> block = nodeToBlockRequest(blockNode);

  // return mpty request if no txs
  if (block->txs.empty()) return shared_ptr<MoneroTransferRequest>(new MoneroTransferRequest());

  // get tx request
  shared_ptr<MoneroTxRequest> txRequest = static_pointer_cast<MoneroTxRequest>(block->txs[0]);

  // get / create transfer request
  shared_ptr<MoneroTransferRequest> transferRequest = txRequest->transferRequest == boost::none ? shared_ptr<MoneroTransferRequest>(new MoneroTransferRequest()) : *txRequest->transferRequest;

  // transfer request references tx request but not the other way around to avoid circular loop // TODO: could add check within meetsCriterias()
  transferRequest->txRequest = txRequest;
  txRequest->transferRequest = boost::none;

  cout << "Returning deserialized transfer request" << endl;

  // return deserialized request
  return transferRequest;
}

shared_ptr<MoneroOutputRequest> deserializeOutputRequest(const string& outputRequestStr) {
  cout << "deserializeOutputRequest(): " <<  outputRequestStr << endl;

  // deserialize output request string to property rooted at block
  std::istringstream iss = outputRequestStr.empty() ? std::istringstream() : std::istringstream(outputRequestStr);
  boost::property_tree::ptree blockNode;
  boost::property_tree::read_json(iss, blockNode);

  // convert request property tree to block
  shared_ptr<MoneroBlock> block = nodeToBlockRequest(blockNode);

  // empty request if no txs
  if (block->txs.empty()) return shared_ptr<MoneroOutputRequest>(new MoneroOutputRequest());

  // get tx request
  shared_ptr<MoneroTxRequest> txRequest = static_pointer_cast<MoneroTxRequest>(block->txs[0]);

  // get / create output request
  shared_ptr<MoneroOutputRequest> outputRequest = txRequest->outputRequest == boost::none ? shared_ptr<MoneroOutputRequest>(new MoneroOutputRequest()) : *txRequest->outputRequest;

  // output request references tx request but not the other way around to avoid circular loop // TODO: could add check within meetsCriterias()
  outputRequest->txRequest = txRequest;
  txRequest->outputRequest = boost::none;

  //cout << block->serialize() << endl;
  cout << "Returning deserialized output request" << endl;

  // return deserialized request
  return outputRequest;
}

shared_ptr<MoneroDestination> nodeToDestination(const boost::property_tree::ptree& node) {
  cout << "nodeToDestination()" << endl;
  shared_ptr<MoneroDestination> destination = shared_ptr<MoneroDestination>(new MoneroDestination());
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Destination node key: " << key << endl;
    if (key == string("address")) destination->address = it->second.data();
    else if (key == string("amount")) destination->amount = it->second.get_value<uint64_t>();
  }
  return destination;
}

shared_ptr<MoneroSendRequest> deserializeSendRequest(const string& sendRequestStr) {
  cout << "deserializeSendRequest(): " <<  sendRequestStr << endl;

  // deserialize send request json to property node
  std::istringstream iss = sendRequestStr.empty() ? std::istringstream() : std::istringstream(sendRequestStr);
  boost::property_tree::ptree node;
  boost::property_tree::read_json(iss, node);

  // convert request property tree to MoneroSendRequest
  shared_ptr<MoneroSendRequest> sendRequest = shared_ptr<MoneroSendRequest>(new MoneroSendRequest());
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    cout << "Send request node key: " << key << endl;
    if (key == string("destinations")) {
      boost::property_tree::ptree destinationsNode = it->second;
      for (boost::property_tree::ptree::const_iterator it2 = destinationsNode.begin(); it2 != destinationsNode.end(); ++it2) {
        sendRequest->destinations.push_back(nodeToDestination(it2->second));
      }
    }
    else if (key == string("paymentId")) sendRequest->paymentId = it->second.data();
    else if (key == string("priority")) throw runtime_error("deserializeSendRequest() paymentId not implemented");
    else if (key == string("mixin")) sendRequest->mixin = it->second.get_value<uint32_t>();
    else if (key == string("ringSize")) sendRequest->ringSize = it->second.get_value<uint32_t>();
    else if (key == string("fee")) sendRequest->fee = it->second.get_value<uint64_t>();
    else if (key == string("accountIndex")) sendRequest->accountIndex = it->second.get_value<uint32_t>();
    else if (key == string("subaddressIndices")) for (boost::property_tree::ptree::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) sendRequest->subaddressIndices.push_back(it2->second.get_value<uint32_t>());
    else if (key == string("unlockTime")) sendRequest->unlockTime = it->second.get_value<uint64_t>();
    else if (key == string("canSplit")) sendRequest->canSplit = it->second.get_value<bool>();
    else if (key == string("doNotRelay")) sendRequest->doNotRelay = it->second.get_value<bool>();
    else if (key == string("note")) sendRequest->note = it->second.data();
    else if (key == string("recipientName")) sendRequest->recipientName = it->second.data();
    else if (key == string("belowAmount")) sendRequest->belowAmount = it->second.get_value<uint64_t>();
    else if (key == string("sweepEachSubaddress")) sendRequest->sweepEachSubaddress = it->second.get_value<bool>();
    else if (key == string("keyImage")) sendRequest->keyImage = it->second.data();
  }

  cout << "Returning deserialized send request" << endl;
  return sendRequest;
}

vector<shared_ptr<MoneroKeyImage>> deserializeKeyImages(const string& keyImagesJson) {

  // deserialize json to property node
  std::istringstream iss = keyImagesJson.empty() ? std::istringstream() : std::istringstream(keyImagesJson);
  boost::property_tree::ptree node;
  boost::property_tree::read_json(iss, node);

  // convert property tree to key images
  vector<shared_ptr<MoneroKeyImage>> keyImages;
  for (boost::property_tree::ptree::const_iterator it = node.begin(); it != node.end(); ++it) {
    string key = it->first;
    //cout << "deserializeKeyImage() key: " << key << endl;
    if (key == string("keyImages")) {
      for (boost::property_tree::ptree::const_iterator it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
        keyImages.push_back(nodeToKeyImage(it2->second));
      }
    }
    else cout << "WARNING MoneroWalletJni::deserializeKeyImages() unrecognized key: " << key << endl;
  }
  return keyImages;
}

// ------------------------------- JNI STATIC ---------------------------------

#ifdef __cplusplus
extern "C"
{
#endif

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_walletExistsJni(JNIEnv *env, jclass clazz, jstring path) {
  cout << "Java_monero_wallet_MoneroWalletJni_walletExistsJni" << endl;
  const char* _path = env->GetStringUTFChars(path, NULL);
  bool walletExists = MoneroWallet::walletExists(string(_path));
  env->ReleaseStringUTFChars(path, _path);
  return static_cast<jboolean>(walletExists);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_openWalletJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType) {
  cout << "Java_monero_wallet_MoneroWalletJni_openWalletJni" << endl;
  const char* _path = env->GetStringUTFChars(jpath, NULL);
  const char* _password = env->GetStringUTFChars(jpassword, NULL);

  // load wallet from file
  MoneroWallet* wallet = new MoneroWallet(string(_path), string(_password), static_cast<MoneroNetworkType>(jnetworkType));

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletRandomJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jint jnetworkType, jstring jdaemonUri, jstring jdaemonUsername, jstring jdaemonPassword, jstring jlanguage) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletRandomJni" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _daemonUri = jdaemonUri ? env->GetStringUTFChars(jdaemonUri, NULL) : nullptr;
  const char* _daemonUsername = jdaemonUsername ? env->GetStringUTFChars(jdaemonUsername, NULL) : nullptr;
  const char* _daemonPassword = jdaemonPassword ? env->GetStringUTFChars(jdaemonPassword, NULL) : nullptr;
  const char* _language = jlanguage ? env->GetStringUTFChars(jlanguage, NULL) : nullptr;

  // construct wallet
  MoneroRpcConnection daemonConnection = MoneroRpcConnection(string(_daemonUri ? _daemonUri : ""), string(_daemonUsername ? _daemonUsername : ""), string(_daemonPassword ? _daemonPassword : ""));
  MoneroWallet* wallet = new MoneroWallet(string(_path ? _path : ""), string(_password ? _password : ""), static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, string(_language ? _language : ""));

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jdaemonUri, _daemonUri);
  env->ReleaseStringUTFChars(jdaemonUsername, _daemonUsername);
  env->ReleaseStringUTFChars(jdaemonPassword, _daemonPassword);
  env->ReleaseStringUTFChars(jlanguage, _language);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni(JNIEnv *env, jclass clazz, jstring jpath, jstring jpassword, jstring jmnemonic, jint jnetworkType, jlong jrestoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromMnemonicJni" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpassword ? env->GetStringUTFChars(jpassword, NULL) : nullptr;
  const char* _mnemonic = env->GetStringUTFChars(jmnemonic, NULL);

  // construct wallet
  MoneroRpcConnection daemonConnection;
  MoneroWallet* wallet = new MoneroWallet(string(_path ? _path : ""), string(_password ? _password : ""), string(_mnemonic), static_cast<MoneroNetworkType>(jnetworkType), daemonConnection, (uint64_t) jrestoreHeight);

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
  env->ReleaseStringUTFChars(jmnemonic, _mnemonic);
  return reinterpret_cast<jlong>(wallet);
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni(JNIEnv *env, jclass clazz, jstring path, jstring password, jstring address, jstring viewKey, jstring spendKey, jint networkType, jlong restoreHeight, jstring language) {
  cout << "Java_monero_wallet_MoneroWalletJni_createWalletFromKeysJni" << endl;
  throw std::runtime_error("Not implemented");

//  const char *_language = env->GetStringUTFChars(language, NULL);
//  Monero::NetworkType _networkType = static_cast<Monero::NetworkType>(networkType);
//  const char *_address = env->GetStringUTFChars(address, NULL);
//  const char *_viewKey = env->GetStringUTFChars(viewKey, NULL);
//  const char *_spendKey = env->GetStringUTFChars(spendKey, NULL);

//  Bitmonero::Wallet *wallet = Bitmonero::WalletManagerFactory::getWalletManager()->createWalletFromKeys( std::string(_path), std::string(_password), std::string(_language), _networkType, (uint64_t) restoreHeight, std::string(_address), std::string(_viewKey), std::string(_spendKey));
//
//  env->ReleaseStringUTFChars(path, _path);
//  env->ReleaseStringUTFChars(password, _password);
//  env->ReleaseStringUTFChars(language, _language);
//  env->ReleaseStringUTFChars(address, _address);
//  env->ReleaseStringUTFChars(viewKey, _viewKey);
//  env->ReleaseStringUTFChars(spendKey, _spendKey);
//  return reinterpret_cast<jlong>(wallet);
}

//  ------------------------------- JNI INSTANCE ------------------------------

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getDaemonConnectionJni()" << endl;

  // get wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get daemon connection
  shared_ptr<MoneroRpcConnection> daemonConnection = wallet->getDaemonConnection();
  if (daemonConnection == nullptr) return 0;

  // return string[uri, username, password]
  jobjectArray vals = env->NewObjectArray(3, env->FindClass("java/lang/String"), nullptr);
  if (!daemonConnection->uri.empty()) env->SetObjectArrayElement(vals, 0, env->NewStringUTF(daemonConnection->uri.c_str()));
  if (!daemonConnection->username.empty()) env->SetObjectArrayElement(vals, 1, env->NewStringUTF(daemonConnection->username.c_str()));
  if (!daemonConnection->password.empty()) env->SetObjectArrayElement(vals, 2, env->NewStringUTF(daemonConnection->password.c_str()));
  return vals;
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni(JNIEnv *env, jobject instance, jstring juri, jstring jusername, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_setDaemonConnectionJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  setDaemonConnection(env, wallet, juri, jusername, jpassword);
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPathJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPathJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getPath().c_str());
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getNetworkTypeJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getNetworkType();
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getMnemonicJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getMnemonicJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getMnemonic().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPublicViewKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getPublicViewKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getPrivateViewKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getPrivateViewKey().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getLanguageJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguageJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return env->NewStringUTF(wallet->getLanguage().c_str());
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getLanguagesJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getLanguagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get languages
  vector<string> languages;
  try {
    languages = wallet->getLanguages();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // build and java string array
  jobjectArray jlanguages = env->NewObjectArray(languages.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < languages.size(); i++) {
    env->SetObjectArrayElement(jlanguages, i, env->NewStringUTF(languages[i].c_str()));
  }
  return jlanguages;
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  string address = wallet->getAddress((uint32_t) accountIdx, (uint32_t) subaddressIdx);
  return env->NewStringUTF(address.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAddressIndexJni(JNIEnv *env, jobject instance, jstring jaddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAddressIndexJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;

  // get indices of address's subaddress
  // TODO: try...catch is unecessary because all jni can invoke Java Exception class, catch in Java
  MoneroSubaddress subaddress;
  try {
    subaddress = wallet->getAddressIndex(string(_address));
  } catch (runtime_error& e) {
    jclass jcls = env->FindClass("monero/utils/MoneroException");
    env->ThrowNew(jcls, e.what());
  }

  // serialize subaddresses which contain indices
  string subaddressJson = subaddress.serialize();
  env->ReleaseStringUTFChars(jaddress, _address);
  return env->NewStringUTF(subaddressJson.c_str());
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_setListenerJni(JNIEnv *env, jobject instance, jobject jlistener) {
  cout << "Java_monero_wallet_MoneroWalletJni_setListenerJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // clear old listener
  wallet->setListener(boost::none);
  WalletJniListener *oldListener = getHandle<WalletJniListener>(env, instance, "jniListenerHandle");
  if (oldListener != nullptr) {
    oldListener->deleteGlobalJavaRef(env);
    delete oldListener;
  }

  // set new listener
  if (jlistener == nullptr) {
    return 0;
  } else {
    WalletJniListener* listener = new WalletJniListener(env, jlistener);
    wallet->setListener(*listener);
    return reinterpret_cast<jlong>(listener);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jstandardAddress, jstring jpaymentId) {
  cout << "Java_monero_wallet_MoneroWalletJni_getIntegratedAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _standardAddress = jstandardAddress ? env->GetStringUTFChars(jstandardAddress, NULL) : nullptr;
  const char* _paymentId = jpaymentId ? env->GetStringUTFChars(jpaymentId, NULL) : nullptr;
  try {

    // get integrated address
    MoneroIntegratedAddress integratedAddress = wallet->getIntegratedAddress(string(_standardAddress ? _standardAddress : ""), string(_paymentId ? _paymentId : ""));

    // serialize integrated address
    string integratedAddressJson = integratedAddress.serialize();
    env->ReleaseStringUTFChars(jstandardAddress, _standardAddress);
    env->ReleaseStringUTFChars(jpaymentId, _paymentId);
    return env->NewStringUTF(integratedAddressJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni(JNIEnv *env, jobject instance, jstring jintegratedAddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_decodeIntegratedAddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _integratedAddress = jintegratedAddress ? env->GetStringUTFChars(jintegratedAddress, NULL) : nullptr;
  try {

    // get integrated address
    MoneroIntegratedAddress integratedAddress = wallet->decodeIntegratedAddress(string(_integratedAddress ? _integratedAddress : ""));

    // serialize integrated address
    string integratedAddressJson = integratedAddress.serialize();
    env->ReleaseStringUTFChars(jintegratedAddress, _integratedAddress);
    return env->NewStringUTF(integratedAddressJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_syncJni(JNIEnv *env, jobject instance, jlong startHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_syncJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // sync wallet
  MoneroSyncResult result = wallet->sync(startHeight);
  cout << "Done syncing.  Blocks fetched: " << result.numBlocksFetched << ", received money: " << result.receivedMoney << endl;

  // build and return results as Object[2]{(long) numBlocksFetched, (boolean) receivedMoney}
  jobjectArray results = env->NewObjectArray(2, env->FindClass("java/lang/Object"), nullptr);
  jclass longClass = env->FindClass("java/lang/Long");
  jmethodID longConstructor = env->GetMethodID(longClass, "<init>", "(J)V");
  jobject numBlocksFetchedWrapped = env->NewObject(longClass, longConstructor, static_cast<jlong>(result.numBlocksFetched));
  env->SetObjectArrayElement(results, 0, numBlocksFetchedWrapped);
  jclass booleanClass = env->FindClass("java/lang/Boolean");
  jmethodID booleanConstructor = env->GetMethodID(booleanClass, "<init>", "(Z)V");
  jobject receivedMoneyWrapped = env->NewObject(booleanClass, booleanConstructor, static_cast<jboolean>(result.receivedMoney));
  env->SetObjectArrayElement(results, 1, receivedMoneyWrapped);
  return results;
}

// rescanBlockchain

// isMultisigImportNeeded

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getHeight();
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getChainHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getChainHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    return wallet->getChainHeight();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jlong JNICALL Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  return wallet->getRestoreHeight();
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setRestoreHeightJni(JNIEnv *env, jobject instance, jlong restoreHeight) {
  cout << "Java_monero_wallet_MoneroWalletJni_setRestoreHeightJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->setRestoreHeight(restoreHeight);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni(JNIEnv *env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance();
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni(JNIEnv *env, jobject instance, jint accountIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance(accountIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni(JNIEnv *env, jobject instance, jint accountIdx, jint subaddressIdx) {
  cout << "Java_monero_wallet_MoneroWalletJni_getUnlockedBalanceSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  uint64_t balance = wallet->getUnlockedBalance(accountIdx, subaddressIdx);
  return env->NewStringUTF(boost::lexical_cast<std::string>(balance).c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountsJni(JNIEnv* env, jobject instance, jboolean includeSubaddresses, jstring jtag) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _tag = jtag ? env->GetStringUTFChars(jtag, NULL) : nullptr;

  // get accounts
  vector<MoneroAccount> accounts = wallet->getAccounts(includeSubaddresses, _tag ? string(_tag) : "");

//  // print account info
//  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
//  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
//    MoneroAccount account = accounts.at(accountIdx);
//    cout << "Account index: " << account.index << endl;
//    cout << "Account label: " << account.label << endl;
//    cout << "Account balance: " << account.balance << endl;
//    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
//
//    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
//	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
//	    cout << "Converted to JSON: " << json << endl;
//    }
//  }

  // wrap and serialize accounts
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!accounts.empty()) container.add_child("accounts", MoneroUtils::toPropertyTree(accounts));
  boost::property_tree::write_json(ss, container, false);
  string accountsJson = ss.str();
  env->ReleaseStringUTFChars(jtag, _tag);
  return env->NewStringUTF(accountsJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAccountJni(JNIEnv* env, jobject instance, jint accountIdx, jboolean includeSubaddresses) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get account
  MoneroAccount account = wallet->getAccount(accountIdx, includeSubaddresses);

//  // print account info
//  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
//  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
//    MoneroAccount account = accounts.at(accountIdx);
//    cout << "Account index: " << account.index << endl;
//    cout << "Account label: " << account.label << endl;
//    cout << "Account balance: " << account.balance << endl;
//    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
//
//    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
//	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
//	    cout << "Converted to JSON: " << json << endl;
//    }
//  }

  // serialize and returna account
  string accountJson = account.serialize();
  return env->NewStringUTF(accountJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createAccountJni(JNIEnv* env, jobject instance, jstring jlabel) {
  cout << "Java_monero_wallet_MoneroWalletJni_createAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // create account
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  MoneroAccount account = wallet->createAccount(string(_label ? _label : ""));
  env->ReleaseStringUTFChars(jlabel, _label);

  // serialize and return account
  string accountJson = account.serialize();
  cout << "JNI returning serialized account: " << accountJson << endl;
  return env->NewStringUTF(accountJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSubaddressesJni(JNIEnv* env, jobject instance, jint accountIdx, jintArray jsubaddressIndices) {
  cout << "Java_monero_wallet_MoneroWalletJni_getSubaddressesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // convert subaddress indices from jintArray to vector<uint32_t>
  vector<uint32_t> subaddressIndices;
  if (jsubaddressIndices != nullptr) {
    jsize numSubaddressIndices = env->GetArrayLength(jsubaddressIndices);
    jint* intArr = env->GetIntArrayElements(jsubaddressIndices, 0);
    for (int subaddressIndicesIdx = 0; subaddressIndicesIdx < numSubaddressIndices; subaddressIndicesIdx++) {
      subaddressIndices.push_back(intArr[subaddressIndicesIdx]);
    }
  }

  // get subaddresses
  vector<MoneroSubaddress> subaddresses = wallet->getSubaddresses(accountIdx, subaddressIndices);

  //  // print account info
  //  cout << "Retrieved " << accounts.size() << " accounts!" << endl;
  //  for (uint32_t accountIdx = 0; accountIdx < accounts.size(); accountIdx++) {
  //    MoneroAccount account = accounts.at(accountIdx);
  //    cout << "Account index: " << account.index << endl;
  //    cout << "Account label: " << account.label << endl;
  //    cout << "Account balance: " << account.balance << endl;
  //    cout << "Account subaddresses: " << account.subaddresses.size() << endl;
  //
  //    for (uint32_t subaddressIdx = 0; subaddressIdx < account.subaddresses.size(); subaddressIdx++) {
  //	    string json = epee::serialization::store_t_to_json(account.subaddresses.at(subaddressIdx));
  //	    cout << "Converted to JSON: " << json << endl;
  //    }
  //  }

  // wrap and serialize subaddresses
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!subaddresses.empty()) container.add_child("subaddresses", MoneroUtils::toPropertyTree(subaddresses));
  boost::property_tree::write_json(ss, container, false);
  string subaddressesJson = ss.str();
  return env->NewStringUTF(subaddressesJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createSubaddressJni(JNIEnv* env, jobject instance, jint accountIdx, jstring jlabel) {
  cout << "Java_monero_wallet_MoneroWalletJni_createSubaddressJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // create subaddress
  const char* _label = jlabel ? env->GetStringUTFChars(jlabel, NULL) : nullptr;
  MoneroSubaddress subaddress = wallet->createSubaddress(accountIdx, string(_label ? _label : ""));
  env->ReleaseStringUTFChars(jlabel, _label);

  // serialize and return subaddress
  string subaddressJson = subaddress.serialize();
  cout << "JNI returning serialized subaddress: " << subaddressJson << endl;
  return env->NewStringUTF(subaddressJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxsJni(JNIEnv* env, jobject instance, jstring jtxRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxsJni" << endl;
  try {
    MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
    const char* _txRequest = jtxRequest ? env->GetStringUTFChars(jtxRequest, NULL) : nullptr;

    // deserialize tx request
    cout << "JNI received tx request string: " << string(_txRequest ? _txRequest : "") << endl;
    shared_ptr<MoneroTxRequest> txRequest = deserializeTxRequest(string(_txRequest ? _txRequest : ""));
    cout << "Fetching txs with request: " << txRequest->serialize() << endl;

    // get txs
    vector<shared_ptr<MoneroTxWallet>> txs = wallet->getTxs(*txRequest);
    cout << "Got " << txs.size() << " txs" << endl;

    // return unique blocks to preserve model relationships as tree
    shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<MoneroBlock>> blocks;
    unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
    for (const shared_ptr<MoneroTxWallet>& tx : txs) {
      if (tx->block == boost::none) {
        if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
        tx->block = unconfirmedBlock;
        unconfirmedBlock->txs.push_back(tx);
      }
      unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(tx->block.get());
      if (got == seenBlockPtrs.end()) {
        seenBlockPtrs.insert(tx->block.get());
        blocks.push_back(tx->block.get());
      }
    }
    cout << "Returning " << blocks.size() << " blocks" << endl;

    // wrap and serialize blocks
    std::stringstream ss;
    boost::property_tree::ptree container;
    if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
    boost::property_tree::write_json(ss, container, false);
    string blocksJson = ss.str();
    env->ReleaseStringUTFChars(jtxRequest, _txRequest);
    return env->NewStringUTF(blocksJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTransfersJni(JNIEnv* env, jobject instance, jstring jtransferRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTransfersJni" << endl;
  try {
    MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
    const char* _transferRequest = jtransferRequest ? env->GetStringUTFChars(jtransferRequest, NULL) : nullptr;

    // deserialize transfer request
    cout << "JNI received transfer request string: " << string(_transferRequest ? _transferRequest : "") << endl;
    shared_ptr<MoneroTransferRequest> transferRequest = deserializeTransferRequest(string(_transferRequest ? _transferRequest : ""));
    cout << "Fetching transfers with request: " << transferRequest->serialize() << endl;

    // get transfers
    vector<shared_ptr<MoneroTransfer>> transfers = wallet->getTransfers(*transferRequest);
    cout << "Got " << transfers.size() << " transfers" << endl;

    // return unique blocks to preserve model relationships as tree
    shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
    vector<shared_ptr<MoneroBlock>> blocks;
    unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
    for (auto const& transfer : transfers) {
      shared_ptr<MoneroTxWallet> tx = transfer->tx;
      if (tx->block == boost::none) {
        if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
        tx->block = unconfirmedBlock;
        unconfirmedBlock->txs.push_back(tx);
      }
      unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(tx->block.get());
      if (got == seenBlockPtrs.end()) {
        seenBlockPtrs.insert(tx->block.get());
        blocks.push_back(tx->block.get());
      }
    }

    // wrap and serialize blocks
    std::stringstream ss;
    boost::property_tree::ptree container;
    if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
    boost::property_tree::write_json(ss, container, false);
    string blocksJson = ss.str();
    env->ReleaseStringUTFChars(jtransferRequest, _transferRequest);
    return env->NewStringUTF(blocksJson.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsJni(JNIEnv* env, jobject instance, jstring joutputRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _outputRequest = joutputRequest ? env->GetStringUTFChars(joutputRequest, NULL) : nullptr;

  // deserialize output request
  cout << "JNI received output request string: " << string(_outputRequest ? _outputRequest : "") << endl;
  shared_ptr<MoneroOutputRequest> outputRequest = deserializeOutputRequest(string(_outputRequest ? _outputRequest : ""));
  cout << "Fetching outputs with request: " << outputRequest->serialize() << endl;

  // get outputs
  vector<shared_ptr<MoneroOutputWallet>> outputs = wallet->getOutputs(*outputRequest);
  cout << "Got " << outputs.size() << " outputs" << endl;

  // return unique blocks to preserve model relationships as tree
  vector<MoneroBlock> blocks;
  unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
  for (auto const& output : outputs) {
    shared_ptr<MoneroTxWallet> tx = static_pointer_cast<MoneroTxWallet>(output->tx);
    if (tx->block == boost::none) throw runtime_error("Need to handle unconfirmed output");
    unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(*tx->block);
    if (got == seenBlockPtrs.end()) {
      seenBlockPtrs.insert(*tx->block);
      blocks.push_back(**tx->block);
    }
  }
  cout << "Returning " << blocks.size() << " blocks" << endl;

  // wrap and serialize blocks
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  env->ReleaseStringUTFChars(joutputRequest, _outputRequest);
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getKeyImagesJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getKeyImagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // fetch key images
  vector<shared_ptr<MoneroKeyImage>> keyImages = wallet->getKeyImages();
  cout << "Fetched " << keyImages.size() << " key images" << endl;

  // wrap and serialize key images
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!keyImages.empty()) container.add_child("keyImages", MoneroUtils::toPropertyTree(keyImages));
  boost::property_tree::write_json(ss, container, false);
  string keyImagesJson = ss.str();
  return env->NewStringUTF(keyImagesJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_importKeyImagesJni(JNIEnv* env, jobject instance, jstring jkeyImagesJson) {
  cout << "Java_monero_wallet_MoneroWalletJni_importKeyImagesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _keyImagesJson = jkeyImagesJson ? env->GetStringUTFChars(jkeyImagesJson, NULL) : nullptr;

  // deserialize key images to import
  vector<shared_ptr<MoneroKeyImage>> keyImages = deserializeKeyImages(string(_keyImagesJson));
  cout << "Deserialized " << keyImages.size() << " key images from java json" << endl;

  // import key images
  shared_ptr<MoneroKeyImageImportResult> result;
  try {
    result = wallet->importKeyImages(keyImages);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // serialize and return result
  env->ReleaseStringUTFChars(jkeyImagesJson, _keyImagesJson);
  return env->NewStringUTF(result->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sendSplitJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_sendSplitJni(request)" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;

  cout << "Send request json: " << string(_sendRequest ? _sendRequest : "") << endl;

  // deserialize send request
  shared_ptr<MoneroSendRequest> sendRequest = deserializeSendRequest(string(_sendRequest ? _sendRequest : ""));
  cout << "Deserialized send request, re-serialized: " << sendRequest->serialize() << endl;

  // submit send request
  vector<shared_ptr<MoneroTxWallet>> txs;
  try {
    txs = wallet->sendSplit(*sendRequest);
    cout << "Got " << txs.size() << " txs" << endl;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // return unique blocks to preserve model relationships as tree
  shared_ptr<MoneroBlock> unconfirmedBlock = nullptr; // placeholder to store unconfirmed txs in return json
  vector<MoneroBlock> blocks;
  unordered_set<shared_ptr<MoneroBlock>> seenBlockPtrs;
  for (auto const& tx : txs) {
    if (tx->block == boost::none) {
      if (unconfirmedBlock == nullptr) unconfirmedBlock = shared_ptr<MoneroBlock>(new MoneroBlock());
      tx->block = unconfirmedBlock;
      unconfirmedBlock->txs.push_back(tx);
    }
    unordered_set<shared_ptr<MoneroBlock>>::const_iterator got = seenBlockPtrs.find(*tx->block);
    if (got == seenBlockPtrs.end()) {
      seenBlockPtrs.insert(*tx->block);
      blocks.push_back(**tx->block);
    }
  }
  cout << "Returning " << blocks.size() << " blocks" << endl;

  // wrap and serialize blocks
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_sweepOutputJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_sweepOutputJni(request)" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;
  string sendRequestJson = string(_sendRequest);
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);

  cout << "Send request json: " << sendRequestJson << endl;

  // deserialize send request
  shared_ptr<MoneroSendRequest> sendRequest = deserializeSendRequest(sendRequestJson);
  cout << "Deserialized send request, re-serialized: " << sendRequest->serialize() << endl;

  // submit send request
  shared_ptr<MoneroTxWallet> tx;
  try {
    tx = wallet->sweepOutput(*sendRequest);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // wrap and serialize blocks to preserve model relationships as tree
  MoneroBlock block;
  block.txs.push_back(tx);
  vector<MoneroBlock> blocks;
  blocks.push_back(block);
  std::stringstream ss;
  boost::property_tree::ptree container;
  if (!blocks.empty()) container.add_child("blocks", MoneroUtils::toPropertyTree(blocks));
  boost::property_tree::write_json(ss, container, false);
  string blocksJson = ss.str();
  return env->NewStringUTF(blocksJson.c_str());
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_relayTxsJni(JNIEnv* env, jobject instance, jobjectArray jtxMetadatas) {
  cout << "Java_monero_wallet_MoneroWalletJni_relayTxsJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get tx metadatas from jobjectArray to vector<string>
  vector<string> txMetadatas;
  if (jtxMetadatas != nullptr) {
    jsize size = env->GetArrayLength(jtxMetadatas);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxMetadatas, idx);
      txMetadatas.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // relay tx metadata
  vector<string> txIds;
  try {
    txIds = wallet->relayTxs(txMetadatas);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // convert and return tx ids as jobjectArray
  jobjectArray jtxIds = env->NewObjectArray(txIds.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < txIds.size(); i++) {
    env->SetObjectArrayElement(jtxIds, i, env->NewStringUTF(txIds[i].c_str()));
  }
  return jtxIds;
}

JNIEXPORT jobjectArray JNICALL Java_monero_wallet_MoneroWalletJni_getTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtxIds) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxNotesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get tx ids from jobjectArray to vector<string>
  vector<string> txIds;
  if (jtxIds != nullptr) {
    jsize size = env->GetArrayLength(jtxIds);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxIds, idx);
      txIds.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // get tx notes
  vector<string> txNotes;
  try {
    txNotes = wallet->getTxNotes(txIds);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // convert and return tx notes as jobjectArray
  jobjectArray jtxNotes = env->NewObjectArray(txNotes.size(), env->FindClass("java/lang/String"), nullptr);
  for (int i = 0; i < txNotes.size(); i++) {
    env->SetObjectArrayElement(jtxNotes, i, env->NewStringUTF(txNotes[i].c_str()));
  }
  return jtxNotes;
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setTxNotesJni(JNIEnv* env, jobject instance, jobjectArray jtxIds, jobjectArray jtxNotes) {
  cout << "Java_monero_wallet_MoneroWalletJni_setTxNotesJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");

  // get tx ids from jobjectArray to vector<string>
  vector<string> txIds;
  if (jtxIds != nullptr) {
    jsize size = env->GetArrayLength(jtxIds);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxIds, idx);
      txIds.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // get tx notes from jobjectArray to vector<string>
  vector<string> txNotes;
  if (jtxNotes != nullptr) {
    jsize size = env->GetArrayLength(jtxNotes);
    for (int idx = 0; idx < size; idx++) {
      jstring jstr = (jstring) env->GetObjectArrayElement(jtxNotes, idx);
      txNotes.push_back(env->GetStringUTFChars(jstr, NULL));
    }
  }

  // set tx notes
  try {
    wallet->setTxNotes(txIds, txNotes);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_signJni(JNIEnv* env, jobject instance, jstring jmsg) {
  cout << "Java_monero_wallet_MoneroWalletJni_signJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _msg = jmsg ? env->GetStringUTFChars(jmsg, NULL) : nullptr;
  try {
    string signature = wallet->sign(string(_msg == nullptr ? "" : _msg));
    env->ReleaseStringUTFChars(jmsg, _msg);
    return env->NewStringUTF(signature.c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_verifyJni(JNIEnv* env, jobject instance, jstring jmsg, jstring jaddress, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_verifyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _msg = jmsg ? env->GetStringUTFChars(jmsg, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  try {
    bool isGood = wallet->verify(string(_msg == nullptr ? "" : _msg), string(_address == nullptr ? "" : _address), string(_signature == nullptr ? "" : _signature));
    env->ReleaseStringUTFChars(jmsg, _msg);
    env->ReleaseStringUTFChars(jaddress, _address);
    env->ReleaseStringUTFChars(jsignature, _signature);
    return static_cast<jboolean>(isGood);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxKeyJni(JNIEnv* env, jobject instance, jstring jtxId) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxKeyJniJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  env->ReleaseStringUTFChars(jtxId, _txId);
  try {
    return env->NewStringUTF(wallet->getTxKey(txId).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxKeyJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jtxKey, jstring jaddress) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkTxKeyJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _txKey = jtxKey ? env->GetStringUTFChars(jtxKey, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string txKey = string(_txKey == nullptr ? "" : _txKey);
  string address = string(_address == nullptr ? "" : _address);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jtxKey, _txKey);
  env->ReleaseStringUTFChars(jaddress, _address);
  try {
    return env->NewStringUTF(wallet->checkTxKey(txId, txKey, address)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getTxProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jaddress, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getTxProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getTxProof(txId, address, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkTxProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jaddress, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkTxProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return env->NewStringUTF(wallet->checkTxProof(txId, address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getSpendProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getSpendProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getSpendProof(txId, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jboolean JNICALL Java_monero_wallet_MoneroWalletJni_checkSpendProofJni(JNIEnv* env, jobject instance, jstring jtxId, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkSpendProofJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _txId = jtxId ? env->GetStringUTFChars(jtxId, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string txId = string(_txId == nullptr ? "" : _txId);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jtxId, _txId);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return static_cast<jboolean>(wallet->checkSpendProof(txId, message, signature));
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni(JNIEnv* env, jobject instance, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jmessage, _message);
  try {
    return env->NewStringUTF(wallet->getReserveProofWallet(message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getReserveProofAccountJni(JNIEnv* env, jobject instance, jint accountIdx, jstring jamountStr, jstring jmessage) {
  cout << "Java_monero_wallet_MoneroWalletJni_getReserveProofWalletJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _amountStr = jamountStr ? env->GetStringUTFChars(jamountStr, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  string amountStr = string(_amountStr == nullptr ? "" : _amountStr);
  string message = string(_message == nullptr ? "" : _message);
  env->ReleaseStringUTFChars(jamountStr, _amountStr);
  env->ReleaseStringUTFChars(jmessage, _message);
  uint64_t amount = boost::lexical_cast<uint64_t>(amountStr);
  try {
    return env->NewStringUTF(wallet->getReserveProofAccount(accountIdx, amount, message).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_checkReserveProofJni(JNIEnv* env, jobject instance, jstring jaddress, jstring jmessage, jstring jsignature) {
  cout << "Java_monero_wallet_MoneroWalletJni_checkReserveProofAccountJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _address = jaddress ? env->GetStringUTFChars(jaddress, NULL) : nullptr;
  const char* _message = jmessage ? env->GetStringUTFChars(jmessage, NULL) : nullptr;
  const char* _signature = jsignature ? env->GetStringUTFChars(jsignature, NULL) : nullptr;
  string address = string(_address == nullptr ? "" : _address);
  string message = string(_message == nullptr ? "" : _message);
  string signature = string(_signature == nullptr ? "" : _signature);
  env->ReleaseStringUTFChars(jaddress, _address);
  env->ReleaseStringUTFChars(jmessage, _message);
  env->ReleaseStringUTFChars(jsignature, _signature);
  try {
    return env->NewStringUTF(wallet->checkReserveProof(address, message, signature)->serialize().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_createPaymentUriJni(JNIEnv* env, jobject instance, jstring jsendRequest) {
  cout << "Java_monero_wallet_MoneroWalletJni_createPaymentUriJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _sendRequest = jsendRequest ? env->GetStringUTFChars(jsendRequest, NULL) : nullptr;

  // deserialize send request
  cout << "JNI received send request string: " << string(_sendRequest ? _sendRequest : "") << endl;
  shared_ptr<MoneroSendRequest> sendRequest = deserializeSendRequest(string(_sendRequest ? _sendRequest : ""));
  cout << "Fetching payment uri with : " << sendRequest->serialize() << endl;

  // get payment uri
  string paymentUri;
  try {
    paymentUri = wallet->createPaymentUri(*sendRequest.get());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return
  env->ReleaseStringUTFChars(jsendRequest, _sendRequest);
  return env->NewStringUTF(paymentUri.c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni(JNIEnv* env, jobject instance, jstring juri) {
  cout << "Java_monero_wallet_MoneroWalletJni_parsePaymentUriJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _uri = juri ? env->GetStringUTFChars(juri, NULL) : nullptr;

  // parse uri to send request
  shared_ptr<MoneroSendRequest> sendRequest;
  try {
    sendRequest = wallet->parsePaymentUri(string(_uri ? _uri : ""));
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }

  // release and return serialized request
  env->ReleaseStringUTFChars(juri, _uri);
  return env->NewStringUTF(sendRequest->serialize().c_str());
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getOutputsHexJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    return env->NewStringUTF(wallet->getOutputsHex().c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT jint JNICALL Java_monero_wallet_MoneroWalletJni_importOutputsHexJni(JNIEnv* env, jobject instance, jstring joutputsHex) {
  cout << "Java_monero_wallet_MoneroWalletJni_getOutputsHexJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _outputsHex = joutputsHex ? env->GetStringUTFChars(joutputsHex, NULL) : nullptr;
  try {
    int numImported = wallet->importOutputsHex(string(_outputsHex == nullptr ? "" : _outputsHex));
    env->ReleaseStringUTFChars(joutputsHex, _outputsHex);
    return numImported;
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
    return 0;
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_setAttributeJni(JNIEnv* env, jobject instance, jstring jkey, jstring jval) {
  cout << "Java_monero_wallet_MoneroWalletJni_setAttribute()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  const char* _val = jval ? env->GetStringUTFChars(jval, NULL) : nullptr;
  string key = string(_key);
  string val = string(_val);
  env->ReleaseStringUTFChars(jkey, _key);
  env->ReleaseStringUTFChars(jval, _val);
  try {
    wallet->setAttribute(key, val);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT jstring JNICALL Java_monero_wallet_MoneroWalletJni_getAttributeJni(JNIEnv* env, jobject instance, jstring jkey) {
  cout << "Java_monero_wallet_MoneroWalletJni_getAttribute()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  const char* _key = jkey ? env->GetStringUTFChars(jkey, NULL) : nullptr;
  string key = string(_key);
  env->ReleaseStringUTFChars(jkey, _key);
  try {
    return env->NewStringUTF(wallet->getAttribute(key).c_str());
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_startMiningJni(JNIEnv* env, jobject instance, jlong numThreads, jboolean backgroundMining, jboolean ignoreBattery) {
  cout << "Java_monero_wallet_MoneroWalletJni_startMiningJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->startMining(numThreads, backgroundMining, ignoreBattery);
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_stopMiningJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_startMiningJni()" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->stopMining();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_saveJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_saveJni(path, password)" << endl;

  // save wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->save();
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_moveToJni(JNIEnv* env, jobject instance, jstring jpath, jstring jpassword) {
  cout << "Java_monero_wallet_MoneroWalletJni_moveToJni(path, password)" << endl;
  const char* _path = jpath ? env->GetStringUTFChars(jpath, NULL) : nullptr;
  const char* _password = jpath ? env->GetStringUTFChars(jpassword, NULL) : nullptr;

  // move wallet
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  try {
    wallet->moveTo(string(_path ? _path : ""), string(_password ? _password : ""));
  } catch (...) {
    rethrow_cpp_exception_as_java_exception(env);
  }

  env->ReleaseStringUTFChars(jpath, _path);
  env->ReleaseStringUTFChars(jpassword, _password);
}

JNIEXPORT void JNICALL Java_monero_wallet_MoneroWalletJni_closeJni(JNIEnv* env, jobject instance) {
  cout << "Java_monero_wallet_MoneroWalletJni_CloseJni" << endl;
  MoneroWallet* wallet = getHandle<MoneroWallet>(env, instance, "jniWalletHandle");
  delete wallet;
  wallet = nullptr;
}

#ifdef __cplusplus
}
#endif
