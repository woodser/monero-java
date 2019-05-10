cmake_minimum_required(VERSION 3.4.1)

#SET(CMAKE_C_COMPILER /path/to/c/compiler)
#SET(CMAKE_CXX_COMPILER /path/to/cpp/compiler)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++11 -pthread")

project(JavaCppBridge)

###################
# Compiler includes
###################

include_directories("$ENV{JAVA_HOME}")
include_directories("$ENV{JAVA_HOME}/include")
include_directories("$ENV{JAVA_HOME}/include/darwin")

#include_directories("build/boost/include") # must exist already - run bin/build-boost-emscripten.sh


SET(boost_SRC /Users/Eric/git/build_stuff/boost_1_69_0/boost)
SET(boost_LIB /Users/Eric/git/build_stuff/boost_1_69_0/stage/lib)
#include_directories(/Users/Eric/git/build_stuff/boost_1_69_0)
#include_directories("${boost_SRC}")
link_directories("${boost_LIB}")

set(MONERO "submodules/monero-core")
set(MONERO_SRC "${MONERO}/src")

include_directories("${MONERO}/contrib/epee/include")
include_directories("${MONERO}/contrib/epee/include/net")
include_directories("${MONERO}/contrib/libsodium/include") # support sodium/… paths
include_directories("${MONERO}/contrib/libsodium/include/sodium")
include_directories("${MONERO}/external")
include_directories("${MONERO}/external/easylogging++")
include_directories("${MONERO}/external/boost/archive")
include_directories("${MONERO}/external/rapidjson/include")
include_directories("${MONERO}/external/db_drivers/liblmdb")
include_directories("${MONERO}/build")	# for translation_files.h

include_directories("${MONERO_SRC}")
include_directories("${MONERO_SRC}/wallet")
include_directories("${MONERO_SRC}/wallet/api")
include_directories("${MONERO_SRC}/common")
include_directories("${MONERO_SRC}/vtlogger")
include_directories("${MONERO_SRC}/crypto")
include_directories("${MONERO_SRC}/cryptonote_basic")
include_directories("${MONERO_SRC}/multisig")
include_directories("${MONERO_SRC}/cryptonote_core")
include_directories("${MONERO_SRC}/cryptonote_protocol")
include_directories("${MONERO_SRC}/rpc")
include_directories("${MONERO_SRC}/mnemonics")
#include_directories("${MONERO_SRC}/checkpoints")

############
# Boost
############

#set(Boost_NO_SYSTEM_PATH ON)
#set(Boost_USE_STATIC_LIBS ON)
#set(Boost_USE_MULTITHREADED ON)
#set(Boost_USE_STATIC_RUNTIME ON)

set(boost_DIR ${CMAKE_SOURCE_DIR}/build/boost)
#include_directories(${boost_DIR})
#link_directories(${boost_DIR})
#set(boost_SRC ${boost_DIR}/include/boost)
#include_directories(${boost_SRC})
#link_directories(${boost_SRC})
find_package(Boost 1.69.0 REQUIRED COMPONENTS thread chrono system filesystem)

add_library(boost_chrono STATIC IMPORTED)
set_target_properties(
    boost_chrono PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_chrono.a
)
add_library(boost_system STATIC IMPORTED)
set_target_properties(
    boost_system PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_system.a
)
add_library(boost_thread STATIC IMPORTED)
set_target_properties(
    boost_thread PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_thread.a
)
add_library(boost_filesystem STATIC IMPORTED)
set_target_properties(
    boost_filesystem PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_filesystem.a
)
add_library(boost_serialization STATIC IMPORTED)
set_target_properties(
    boost_serialization PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_serialization.a
)
add_library(boost_program_options STATIC IMPORTED)
set_target_properties(
    boost_program_options PROPERTIES IMPORTED_LOCATION
    ${boost_DIR}/lib/libboost_program_options.a
)

############
# OpenSSL
############

set(openssl_DIR ${CMAKE_SOURCE_DIR}/build/openssl)

add_library(crypto STATIC IMPORTED)
set_target_properties(
	crypto PROPERTIES IMPORTED_LOCATION
	${openssl_DIR}/lib/libcrypto.a
)

add_library(ssl STATIC IMPORTED)
set_target_properties(
	ssl PROPERTIES IMPORTED_LOCATION
	${openssl_DIR}/lib/libssl.a
)

#
# keeping test files in a separate source directory 
# file(GLOB TEST_SRCS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/test_*.cpp)
#
add_definitions("-DDEFAULT_DB_TYPE=\"lmdb\"")
set(
    SRC_FILES
    monero_wallet_MoneroWalletJni.cpp
    #
    ${MONERO}/contrib/epee/src/hex.cpp
    ${MONERO}/contrib/epee/src/string_tools.cpp
    ${MONERO}/contrib/epee/src/memwipe.c
    ${MONERO}/contrib/epee/src/mlocker.cpp
    ${MONERO}/contrib/epee/src/wipeable_string.cpp
    ${MONERO}/contrib/epee/src/mlog.cpp
    ${MONERO}/contrib/epee/src/http_auth.cpp
    ${MONERO}/contrib/libsodium/src/crypto_verify/verify.c
    ${MONERO}/external/easylogging++/easylogging++.cc
    #
    ${MONERO_SRC}/wallet/api/wallet_manager.cpp
    ${MONERO_SRC}/wallet/api/wallet.cpp
    ${MONERO_SRC}/wallet/wallet2.cpp
    ${MONERO_SRC}/wallet/message_store.cpp
    ${MONERO_SRC}/wallet/message_transporter.cpp
    ${MONERO_SRC}/wallet/node_rpc_proxy.cpp
    #
    ${MONERO_SRC}/checkpoints/checkpoints.cpp
    ${MONERO_SRC}/multisig/multisig.cpp
    ${MONERO_SRC}/blockchain_db/blockchain_db.cpp
    ${MONERO_SRC}/blockchain_utilities/blockchain_blackball.cpp
    #
    ${MONERO_SRC}/cryptonote_basic/cryptonote_basic_impl.cpp
    ${MONERO_SRC}/cryptonote_basic/account.cpp
    ${MONERO_SRC}/cryptonote_basic/cryptonote_format_utils.cpp
    ${MONERO_SRC}/cryptonote_basic/difficulty.cpp
    ${MONERO_SRC}/cryptonote_basic/miner.cpp
    ${MONERO_SRC}/crypto/crypto.cpp
    ${MONERO_SRC}/crypto/hash.c
    ${MONERO_SRC}/crypto/oaes_lib.c
    ${MONERO_SRC}/crypto/crypto-ops.c
    ${MONERO_SRC}/crypto/crypto-ops-data.c
    ${MONERO_SRC}/crypto/keccak.c
    ${MONERO_SRC}/crypto/chacha.c
    ${MONERO_SRC}/crypto/random.c
    ${MONERO_SRC}/crypto/aesb.c
    ${MONERO_SRC}/crypto/tree-hash.c
    ${MONERO_SRC}/crypto/hash-extra-blake.c
    ${MONERO_SRC}/crypto/blake256.c
    ${MONERO_SRC}/crypto/hash-extra-groestl.c
    ${MONERO_SRC}/crypto/hash-extra-jh.c
    ${MONERO_SRC}/crypto/hash-extra-skein.c
    ${MONERO_SRC}/crypto/groestl.c
    ${MONERO_SRC}/crypto/jh.c
    ${MONERO_SRC}/crypto/skein.c
    ${MONERO_SRC}/cryptonote_core/cryptonote_tx_utils.cpp
    ${MONERO_SRC}/common/base58.cpp
    ${MONERO_SRC}/common/threadpool.cpp
    ${MONERO_SRC}/common/aligned.c
    ${MONERO_SRC}/common/util.cpp
    ${MONERO_SRC}/common/i18n.cpp
    ${MONERO_SRC}/common/combinator.cpp
    ${MONERO_SRC}/common/password.cpp
    ${MONERO_SRC}/common/updates.cpp
    ${MONERO_SRC}/device/device.cpp
    ${MONERO_SRC}/device/device_default.cpp
    ${MONERO_SRC}/ringct/rctOps.cpp
    ${MONERO_SRC}/ringct/rctTypes.cpp
    ${MONERO_SRC}/ringct/rctCryptoOps.c
    ${MONERO_SRC}/ringct/rctSigs.cpp
    ${MONERO_SRC}/ringct/bulletproofs.cc
    ${MONERO_SRC}/ringct/multiexp.cc
    ${MONERO_SRC}/mnemonics/electrum-words.cpp
    #${MONERO_SRC}/vtlogger/logger.cpp
)

########################## BUILD MONERO LIBRARY ###############################

add_library(monero SHARED ${SRC_FILES})
#target_link_libraries(monero)
target_link_libraries(monero
	boost_chrono
	boost_system
	boost_thread
	boost_filesystem
	boost_regex
	boost_serialization
	boost_program_options
	crypto
	ssl
)

#set_target_properties(monero PROPERTIES POSITION_INDEPENDENT_CODE ON)
#set_target_properties(monero PROPERTIES dynamiclib "")
#set_target_properties(monero PROPERTIES o libmonero.dylib)
#
#
# boost_atomic
# boost_date_time
# boost_filesystem
# boost_locale
# boost_program_options
# boost_regex
# boost_serialization
# boost_signals
# boost_timer
# boost_wserialization
