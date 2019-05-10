cmake_minimum_required(VERSION 3.4.1)

#SET(CMAKE_C_COMPILER /path/to/c/compiler)
#SET(CMAKE_CXX_COMPILER /path/to/cpp/compiler)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++11 -pthread")

project(JavaCppBridge)

set(EXTERNAL_LIBS_DIR ${CMAKE_SOURCE_DIR}/external-libs)

############
# libsodium
############

#add_library(sodium STATIC IMPORTED)
#set_target_properties(sodium PROPERTIES IMPORTED_LOCATION
#        ${EXTERNAL_LIBS_DIR}/libsodium/libsodium.a)

############
# OpenSSL
############

add_library(crypto STATIC IMPORTED)
set_target_properties(crypto PROPERTIES IMPORTED_LOCATION
        ${EXTERNAL_LIBS_DIR}/openssl/libcrypto.a)

add_library(ssl STATIC IMPORTED)
set_target_properties(ssl PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/openssl/libssl.a)

############
# Boost
############

add_library(boost_chrono STATIC IMPORTED)
set_target_properties(boost_chrono PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_chrono.a)

add_library(boost_date_time STATIC IMPORTED)
set_target_properties(boost_date_time PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_date_time.a)

add_library(boost_filesystem STATIC IMPORTED)
set_target_properties(boost_filesystem PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_filesystem.a)

add_library(boost_program_options STATIC IMPORTED)
set_target_properties(boost_program_options PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_program_options.a)

add_library(boost_regex STATIC IMPORTED)
set_target_properties(boost_regex PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_regex.a)

add_library(boost_serialization STATIC IMPORTED)
set_target_properties(boost_serialization PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_serialization.a)
    
 add_library(boost_wserialization STATIC IMPORTED)
set_target_properties(boost_wserialization PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_wserialization.a)

add_library(boost_system STATIC IMPORTED)
set_target_properties(boost_system PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_system.a)

add_library(boost_thread STATIC IMPORTED)
set_target_properties(boost_thread PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/boost/libboost_thread.a)

#############
# Monero
#############

#add_library(wallet_api STATIC IMPORTED)
#set_target_properties(wallet_api PROPERTIES IMPORTED_LOCATION
#    ${EXTERNAL_LIBS_DIR}/monero/libwallet_api.a)

add_library(wallet STATIC IMPORTED)
set_target_properties(wallet PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libwallet.a)

add_library(cryptonote_core STATIC IMPORTED)
set_target_properties(cryptonote_core PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libcryptonote_core.a)

add_library(cryptonote_basic STATIC IMPORTED)
set_target_properties(cryptonote_basic PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libcryptonote_basic.a)

add_library(mnemonics STATIC IMPORTED)
set_target_properties(mnemonics PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libmnemonics.a)

add_library(common STATIC IMPORTED)
set_target_properties(common PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libcommon.a)

add_library(cncrypto STATIC IMPORTED)
set_target_properties(cncrypto PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libcncrypto.a)

add_library(ringct STATIC IMPORTED)
set_target_properties(ringct PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libringct.a)

add_library(ringct_basic STATIC IMPORTED)
set_target_properties(ringct_basic PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libringct_basic.a)

add_library(blockchain_db STATIC IMPORTED)
set_target_properties(blockchain_db PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libblockchain_db.a)

add_library(lmdb STATIC IMPORTED)
set_target_properties(lmdb PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/liblmdb.a)

add_library(easylogging STATIC IMPORTED)
set_target_properties(easylogging PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libeasylogging.a)

add_library(unbound STATIC IMPORTED)
set_target_properties(unbound PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libunbound.a)

add_library(epee STATIC IMPORTED)
set_target_properties(epee PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libepee.a)

add_library(blocks STATIC IMPORTED)
set_target_properties(blocks PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libblocks.a)

add_library(checkpoints STATIC IMPORTED)
set_target_properties(checkpoints PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libcheckpoints.a)

add_library(device STATIC IMPORTED)
set_target_properties(device PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libdevice.a)

add_library(multisig STATIC IMPORTED)
set_target_properties(multisig PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libmultisig.a)

add_library(version STATIC IMPORTED)
set_target_properties(version PROPERTIES IMPORTED_LOCATION
    ${EXTERNAL_LIBS_DIR}/monero/libversion.a)

#############
# System
#############

#find_library(log-lib log)

set(MONERO "src/main/cpp/submodules/monero-core")
set(MONERO_SRC "${MONERO}/src")

include_directories("$ENV{JAVA_HOME}")
include_directories("$ENV{JAVA_HOME}/include")
include_directories("$ENV{JAVA_HOME}/include/darwin")

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
include_directories("${MONERO_SRC}/wallet/api")	# TODO: could replace with include_directories(${EXTERNAL_LIBS_DIR}/monero/include) and put wallet2_api.h in there
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

message(STATUS EXTERNAL_LIBS_DIR : ${EXTERNAL_LIBS_DIR})

#################
# Build library
#################

set(
    SRC_FILES
    src/main/cpp/monero_wallet_MoneroWalletJni.cpp
    ${MONERO_SRC}/wallet/api/wallet_manager.cpp
    ${MONERO_SRC}/wallet/api/wallet.cpp
    ${MONERO_SRC}/wallet/wallet2.cpp
)
add_library(monero-java SHARED ${SRC_FILES})

target_link_libraries(monero-java

#wallet_api
wallet
cryptonote_core
cryptonote_basic
mnemonics
ringct
ringct_basic
common
cncrypto
blockchain_db
lmdb
easylogging
unbound
epee
blocks
checkpoints
device
multisig
version

boost_chrono
boost_date_time
boost_filesystem
boost_program_options
boost_regex
boost_serialization
boost_wserialization
boost_system
boost_thread

ssl
crypto

#sodium

#${log-lib}
)