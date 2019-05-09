cmake_minimum_required(VERSION 3.4.1)

#SET(CMAKE_C_COMPILER /path/to/c/compiler)
#SET(CMAKE_CXX_COMPILER /path/to/cpp/compiler)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++11 -pthread")

project(JavaCppBridge)

############################# COMPILER INCLUDES ##################################

include_directories("$ENV{JAVA_HOME}")
include_directories("$ENV{JAVA_HOME}/include")
include_directories("$ENV{JAVA_HOME}/include/darwin")

#include_directories("build/boost/include") # must exist already - run bin/build-boost-emscripten.sh


SET(boost_SRC /Users/Eric/git/build_stuff/boost_1_69_0/boost)
SET(boost_LIB /Users/Eric/git/build_stuff/boost_1_69_0/stage/lib)
#include_directories(/Users/Eric/git/build_stuff/boost_1_69_0)
#include_directories("${boost_SRC}")
link_directories("${boost_LIB}")

set(MYMONERO_CORE_CPP "submodules/mymonero-core-cpp")
set(MYMONERO_CORE_CPP_SRC "${MYMONERO_CORE_CPP}/src")
set(MYMONERO_MONERO_SRC "${MYMONERO_CORE_CPP}/contrib/monero-core-custom")

include_directories("${MYMONERO_CORE_CPP_SRC}")

include_directories(${MYMONERO_MONERO_SRC})
include_directories("${MYMONERO_MONERO_SRC}/epee/include")
include_directories("${MYMONERO_MONERO_SRC}/common")
include_directories("${MYMONERO_MONERO_SRC}/vtlogger")
include_directories("${MYMONERO_MONERO_SRC}/crypto")
include_directories("${MYMONERO_MONERO_SRC}/cryptonote_basic")
include_directories("${MYMONERO_MONERO_SRC}/multisig")
include_directories("${MYMONERO_MONERO_SRC}/cryptonote_core")
include_directories("${MYMONERO_MONERO_SRC}/cryptonote_protocol")
include_directories("${MYMONERO_MONERO_SRC}/wallet")
include_directories("${MYMONERO_MONERO_SRC}/rpc")
include_directories("${MYMONERO_MONERO_SRC}/mnemonics")
include_directories("${MYMONERO_MONERO_SRC}/contrib/libsodium/include") # support sodium/… paths
include_directories("${MYMONERO_MONERO_SRC}/contrib/libsodium/include/sodium")

set(MONERO "submodules/monero-core")
set(MONERO_SRC "${MONERO}/src")

include_directories("${MONERO_SRC}")
include_directories("${MONERO_SRC}/wallet")
include_directories("${MONERO_SRC}/wallet/api")

############################# BOOST SETTINGS ##################################

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
find_package(Boost 1.69.0 REQUIRED COMPONENTS thread chrono system)

set(boost_DIR ${CMAKE_SOURCE_DIR}/build/boost)

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
#
# keeping test files in a separate source directory 
# file(GLOB TEST_SRCS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/test_*.cpp)
#
set(
    SRC_FILES
    monero_cpp_bridge_MoneroCppUtilsNative.h
    monero_cpp_bridge_MoneroCppUtilsNative.cpp
    #monero_wallet_MoneroWalletJni.h
    #monero_wallet_MoneroWalletJni.cpp
	#
    ${MYMONERO_CORE_CPP_SRC}/monero_address_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_address_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_paymentID_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_paymentID_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_key_image_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_key_image_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_fee_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_fee_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_transfer_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_transfer_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_fork_rules.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_fork_rules.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_wallet_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_wallet_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/serial_bridge_index.hpp
    ${MYMONERO_CORE_CPP_SRC}/serial_bridge_index.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_send_routine.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_send_routine.cpp
    ${MYMONERO_CORE_CPP_SRC}/serial_bridge_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/serial_bridge_utils.cpp
    ${MYMONERO_CORE_CPP_SRC}/tools__ret_vals.hpp
    ${MYMONERO_CORE_CPP_SRC}/tools__ret_vals.cpp
    ${MYMONERO_CORE_CPP_SRC}/monero_binary_utils.hpp
    ${MYMONERO_CORE_CPP_SRC}/monero_binary_utils.cpp
    #
    ${MYMONERO_MONERO_SRC}/cryptonote_basic/cryptonote_basic_impl.cpp
    ${MYMONERO_MONERO_SRC}/cryptonote_basic/account.cpp
    ${MYMONERO_MONERO_SRC}/cryptonote_basic/cryptonote_format_utils.cpp
    ${MYMONERO_MONERO_SRC}/crypto/crypto.cpp
    ${MYMONERO_MONERO_SRC}/crypto/hash.c
    ${MYMONERO_MONERO_SRC}/crypto/slow-hash-dummied.cpp
    ${MYMONERO_MONERO_SRC}/crypto/oaes_lib.c
    ${MYMONERO_MONERO_SRC}/crypto/crypto-ops.c
    ${MYMONERO_MONERO_SRC}/crypto/crypto-ops-data.c
    ${MYMONERO_MONERO_SRC}/crypto/keccak.c
    ${MYMONERO_MONERO_SRC}/crypto/chacha.c
    ${MYMONERO_MONERO_SRC}/crypto/random.c
    ${MYMONERO_MONERO_SRC}/crypto/aesb.c
    ${MYMONERO_MONERO_SRC}/crypto/tree-hash.c
    ${MYMONERO_MONERO_SRC}/crypto/hash-extra-blake.c
    ${MYMONERO_MONERO_SRC}/crypto/blake256.c
    ${MYMONERO_MONERO_SRC}/crypto/hash-extra-groestl.c
    ${MYMONERO_MONERO_SRC}/crypto/hash-extra-jh.c
    ${MYMONERO_MONERO_SRC}/crypto/hash-extra-skein.c
    ${MYMONERO_MONERO_SRC}/crypto/groestl.c
    ${MYMONERO_MONERO_SRC}/crypto/jh.c
    ${MYMONERO_MONERO_SRC}/crypto/skein.c
    ${MYMONERO_MONERO_SRC}/cryptonote_core/cryptonote_tx_utils.cpp
    ${MYMONERO_MONERO_SRC}/common/base58.cpp
    ${MYMONERO_MONERO_SRC}/common/threadpool.cpp
    ${MYMONERO_MONERO_SRC}/common/aligned.c
    ${MYMONERO_MONERO_SRC}/common/util.cpp
    ${MYMONERO_MONERO_SRC}/epee/src/hex.cpp
    ${MYMONERO_MONERO_SRC}/epee/src/string_tools.cpp
    ${MYMONERO_MONERO_SRC}/epee/src/memwipe.c
    ${MYMONERO_MONERO_SRC}/epee/src/mlocker.cpp
    ${MYMONERO_MONERO_SRC}/epee/src/wipeable_string.cpp
    ${MYMONERO_MONERO_SRC}/device/device.cpp
    ${MYMONERO_MONERO_SRC}/device/device_default.cpp
    ${MYMONERO_MONERO_SRC}/ringct/rctOps.cpp
    ${MYMONERO_MONERO_SRC}/ringct/rctTypes.cpp
    ${MYMONERO_MONERO_SRC}/ringct/rctCryptoOps.c
    ${MYMONERO_MONERO_SRC}/ringct/rctSigs.cpp
    ${MYMONERO_MONERO_SRC}/ringct/bulletproofs.cc
    ${MYMONERO_MONERO_SRC}/ringct/multiexp.cc
    ${MYMONERO_MONERO_SRC}/mnemonics/electrum-words.cpp
    ${MYMONERO_MONERO_SRC}/vtlogger/logger.cpp
    ${MYMONERO_MONERO_SRC}/contrib/libsodium/src/crypto_verify/verify.c
    #
    #${boost_SRC}/thread/thread.hpp
    #
    #${MONERO_SRC}/wallet/api/wallet2_api.h
    #/Users/Eric/git/monero-java-rpc/src/main/cpp/submodules/monero-core/src/wallet/api/wallet2_api.h
)

########################## BUILD MONERO LIBRARY ###############################

add_library(monero SHARED ${SRC_FILES})
#target_link_libraries(monero)
target_link_libraries(monero boost_chrono boost_system boost_thread)

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
