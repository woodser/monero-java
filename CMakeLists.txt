cmake_minimum_required(VERSION 3.4.1)

if (WIN32)
  add_definitions( "-D_GLIBCXX_USE_NANOSLEEP=1" )
  add_definitions( "-DWIN32_LEAN_AND_MEAN" )
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wa,-mbig-obj -O2 -fPIC -std=c++14 -F/Library/Frameworks -pthread -lcrypto -lcrypt32") 
else()
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++14 -F/Library/Frameworks -pthread")
endif()

#SET(CMAKE_C_COMPILER /path/to/c/compiler)
#SET(CMAKE_CXX_COMPILER /path/to/cpp/compiler)

project(monero-java-jni)

#############
# System
#############

set(MONERO_CPP "${CMAKE_SOURCE_DIR}/external/monero-cpp")
message(STATUS MONERO_CPP : ${MONERO_CPP} : ${MONERO_CPP})

set(MONERO_CPP_SRC "${MONERO_CPP}/src")
set(MONERO_PROJECT ${MONERO_CPP}/external/monero-project)
set(MONERO_PROJECT_SRC "${MONERO_PROJECT}/src")

list(APPEND CMAKE_MODULE_PATH "${MONERO_PROJECT}/cmake")
list(APPEND CMAKE_MODULE_PATH "${MONERO_CPP}/cmake")

# check JAVA_HOME
if(NOT DEFINED ENV{JAVA_HOME} OR "$ENV{JAVA_HOME}" STREQUAL "")
  message(FATAL_ERROR "JAVA_HOME variable not set, for example: export JAVA_HOME=/path/to/jdk")
endif()

# TODO: remove TRUEs, how are APPLE, DEPENDS, etc initialized?
if (TRUE OR HIDAPI_FOUND OR LibUSB_COMPILE_TEST_PASSED)
  if (APPLE)
    if(TRUE OR DEPENDS)
      list(APPEND EXTRA_LIBRARIES "-framework Foundation -framework IOKit -framework AppKit")
    else()
      find_library(COREFOUNDATION CoreFoundation)
      find_library(IOKIT IOKit)
      find_library(APPKIT AppKit)
      list(APPEND EXTRA_LIBRARIES ${IOKIT})
      list(APPEND EXTRA_LIBRARIES ${COREFOUNDATION})
      list(APPEND EXTRA_LIBRARIES ${APPKIT})
    endif()
  endif()
  if (WIN32)
    list(APPEND EXTRA_LIBRARIES setupapi)
  endif()
endif()

message(STATUS EXTRA_LIBRARIES: ${EXTRA_LIBRARIES})

find_library(ZMQ_LIB zmq)

############
# Protobuf
############

if (NOT APPLE)
  include(FindProtobuf)
  set(Protobuf_USE_STATIC_LIBS ON)
  find_package(Protobuf)
  message(STATUS "Protobuf lib: ${Protobuf_LIBRARY}, inc: ${Protobuf_INCLUDE_DIR}, protoc: ${Protobuf_PROTOC_EXECUTABLE}")
endif()

############
# LibUSB
############

find_library(usb_LIBRARY NAMES usb-1.0 libusb usb)
set(LibUSB_LIBRARIES ${usb_LIBRARY})

############
# Boost
############

set(Boost_NO_BOOST_CMAKE 1)
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.58 QUIET REQUIRED COMPONENTS chrono date_time filesystem program_options regex serialization wserialization system thread)
message(STATUS "Using Boost include dir at ${Boost_INCLUDE_DIR}")

############
# OpenSSL
############

if (APPLE AND NOT IOS)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility=default -std=c++14")
  if (NOT OPENSSL_ROOT_DIR)
      EXECUTE_PROCESS(COMMAND brew --prefix openssl
        OUTPUT_VARIABLE OPENSSL_ROOT_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    message(STATUS "Using OpenSSL found at ${OPENSSL_ROOT_DIR}")
  endif()
endif()

find_package(OpenSSL REQUIRED)
message(STATUS "Using OpenSSL include dir at ${OPENSSL_INCLUDE_DIR}")

if(STATIC AND NOT IOS)
  if(UNIX)
    set(OPENSSL_LIBRARIES "${OPENSSL_LIBRARIES};${CMAKE_DL_LIBS};${CMAKE_THREAD_LIBS_INIT}")
  endif()
endif()

if (WIN32)
  list(APPEND OPENSSL_LIBRARIES ws2_32 crypt32)
endif()

############
# libsodium
############

find_library(SODIUM_LIBRARY sodium REQUIRED)
message(STATUS "Using libsodium library at ${SODIUM_LIBRARY}")

############
# HIDAPI
############

if(APPLE)
  include_directories(SYSTEM /usr/include/malloc)
  if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
  endif()
endif()

find_package(HIDAPI REQUIRED)
message(STATUS "Using HIDAPI include dir at ${HIDAPI_INCLUDE_DIR}")
add_definitions(-DHAVE_HIDAPI)

#############
# Monero
#############

set(MONERO_PROJECT_BUILD "${MONERO_PROJECT}/build/release" CACHE STRING "Monero project build directory")
message(STATUS "Using monero-project build: " ${MONERO_PROJECT_BUILD})

list(APPEND CMAKE_MODULE_PATH "${MONERO_PROJECT}/cmake")

add_library(wallet STATIC IMPORTED)
set_target_properties(wallet PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/lib/libwallet.a)

# libwallet-crypto.a provides x86_64 asm for some wallet functions
if (EXISTS ${MONERO_PROJECT_BUILD}/src/crypto/wallet/libwallet-crypto.a)
  add_library(wallet_crypto_lib STATIC IMPORTED)
  set_target_properties(wallet_crypto_lib PROPERTIES IMPORTED_LOCATION
      ${MONERO_PROJECT_BUILD}/src/crypto/wallet/libwallet-crypto.a)
  set(wallet_crypto wallet_crypto_lib)
endif()

add_library(lmdb STATIC IMPORTED)
set_target_properties(lmdb PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/external/db_drivers/liblmdb/liblmdb.a)

add_library(epee STATIC IMPORTED)
set_target_properties(epee PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/contrib/epee/src/libepee.a)

add_library(rpc_base STATIC IMPORTED)
set_target_properties(rpc_base PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/rpc/librpc_base.a)

add_library(net STATIC IMPORTED)
set_target_properties(net PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/net/libnet.a)

add_library(hardforks STATIC IMPORTED)
set_target_properties(hardforks PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/hardforks/libhardforks.a)

add_library(easylogging STATIC IMPORTED)
set_target_properties(easylogging PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/external/easylogging++/libeasylogging.a)

add_library(cryptonote_core STATIC IMPORTED)
set_target_properties(cryptonote_core PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/cryptonote_core/libcryptonote_core.a)

add_library(cryptonote_protocol STATIC IMPORTED)
set_target_properties(cryptonote_protocol PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/cryptonote_protocol/libcryptonote_protocol.a)

add_library(cryptonote_basic STATIC IMPORTED)
set_target_properties(cryptonote_basic PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/cryptonote_basic/libcryptonote_basic.a)

add_library(cryptonote_format_utils_basic STATIC IMPORTED)
set_target_properties(cryptonote_format_utils_basic PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/cryptonote_basic/libcryptonote_format_utils_basic.a)

add_library(mnemonics STATIC IMPORTED)
set_target_properties(mnemonics PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/mnemonics/libmnemonics.a)

add_library(common STATIC IMPORTED)
set_target_properties(common PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/common/libcommon.a)

add_library(cncrypto STATIC IMPORTED)
set_target_properties(cncrypto PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/crypto/libcncrypto.a)

add_library(ringct STATIC IMPORTED)
set_target_properties(ringct PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/ringct/libringct.a)

add_library(ringct_basic STATIC IMPORTED)
set_target_properties(ringct_basic PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/ringct/libringct_basic.a)

add_library(blockchain_db STATIC IMPORTED)
set_target_properties(blockchain_db PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/blockchain_db/libblockchain_db.a)

add_library(blocks STATIC IMPORTED)
set_target_properties(blocks PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/blocks/libblocks.a)

add_library(checkpoints STATIC IMPORTED)
set_target_properties(checkpoints PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/checkpoints/libcheckpoints.a)

add_library(device STATIC IMPORTED)
set_target_properties(device PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/device/libdevice.a)

add_library(device_trezor STATIC IMPORTED)
set_target_properties(device_trezor PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/device_trezor/libdevice_trezor.a)

add_library(multisig STATIC IMPORTED)
set_target_properties(multisig PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/multisig/libmultisig.a)

add_library(version STATIC IMPORTED)
set_target_properties(version PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/src/libversion.a)

add_library(randomx STATIC IMPORTED)
set_target_properties(randomx PROPERTIES IMPORTED_LOCATION
    ${MONERO_PROJECT_BUILD}/external/randomx/librandomx.a)

############
# Unbound
############

find_package(Unbound)
if(NOT UNBOUND_INCLUDE_DIR)
  message(FATAL_ERROR "Could not find libunbound")
else()
  message(STATUS "Found libunbound include (unbound.h) in ${UNBOUND_INCLUDE_DIR}")
  if(UNBOUND_LIBRARIES)
    message(STATUS "Found libunbound library")
    if (WIN32)
      add_library(unbound STATIC IMPORTED)
    else()
      add_library(unbound SHARED IMPORTED)
    endif()
    set_target_properties(unbound PROPERTIES IMPORTED_LOCATION ${UNBOUND_LIBRARIES})
  else()
    message(FATAL_ERROR "Found libunbound includes, but could not find libunbound library. Please make sure you have installed libunbound or libunbound-dev or the equivalent")
  endif()
endif()

######################
# monero-cpp
######################

add_library(monero-cpp STATIC IMPORTED)

# import static c++ library
if (WIN32)
  set_target_properties(monero-cpp PROPERTIES IMPORTED_LOCATION ./libmonero-cpp.lib)
else()
  set_target_properties(monero-cpp PROPERTIES IMPORTED_LOCATION ./libmonero-cpp.a)
endif()

###############################################
# Build Java dynamic library for JNI
###############################################

set(
    MONERO_JNI_SRC_FILES
    src/main/cpp/monero_jni_bridge.cpp
)
add_library(monero-java-static STATIC ${MONERO_JNI_SRC_FILES})

target_include_directories(monero-java-static PUBLIC
  "$ENV{JAVA_HOME}"
  "$ENV{JAVA_HOME}/include"
  "${MONERO_CPP}/external/libsodium/include/sodium"
  "${MONERO_CPP}/external/openssl-sdk/include"
  "${MONERO_CPP_SRC}/"
  "${MONERO_PROJECT}/contrib/epee/include"
  "${MONERO_PROJECT}/external/"
  "${MONERO_PROJECT}/external/easylogging++"
  "${MONERO_PROJECT}/external/rapidjson/include"
  "${MONERO_PROJECT_SRC}/"
  "${MONERO_PROJECT_SRC}/crypto"
  "${MONERO_PROJECT_SRC}/crypto/crypto_ops_builder/include/"
  "${MONERO_PROJECT_SRC}/wallet"
  "${MONERO_PROJECT_SRC}/wallet/api"
  ${Boost_INCLUDE_DIR}
  ${OPENSSL_INCLUDE_DIR}
)

if (APPLE)
  target_include_directories(monero-java-static PUBLIC "$ENV{JAVA_HOME}/include/darwin")
elseif (WIN32)
  target_include_directories(monero-java-static PUBLIC "$ENV{JAVA_HOME}/include/win32")
else()
  target_include_directories(monero-java-static PUBLIC "$ENV{JAVA_HOME}/include/linux")
endif()

target_link_libraries(monero-java-static
    monero-cpp
    wallet
    rpc_base
    net
    lmdb
    unbound
    easylogging
    cryptonote_core
    cryptonote_protocol
    cryptonote_basic
    cryptonote_format_utils_basic
    mnemonics
    ringct
    ringct_basic
    common
    cncrypto
    blockchain_db
    blocks
    checkpoints
    device
    device_trezor
    multisig
    version
    randomx
    epee
    hardforks
    ${wallet_crypto}
    ${ZMQ_LIB}
    ${UNBOUND_LIBRARIES}
    ${Boost_LIBRARIES}
    ${Protobuf_LIBRARY}
    ${LibUSB_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    ${SODIUM_LIBRARY}
    ${HIDAPI_LIBRARIES}
    ${EXTRA_LIBRARIES}
)

file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/null.cpp" "")
add_library(monero-java SHARED "${CMAKE_CURRENT_BINARY_DIR}/null.cpp")

target_link_libraries(monero-java PUBLIC
  -Wl,--whole-archive
    cncrypto
  -Wl,--no-whole-archive
  -Wl,--whole-archive
    monero-java-static
  -Wl,--no-whole-archive
)

if (WIN32)
  target_link_options(monero-java PUBLIC "-Wl,--enable-auto-import,--export-all-symbols")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_options(monero-java PRIVATE "-z" "noexecstack")
endif()

INSTALL(TARGETS monero-java
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT Runtime
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Runtime
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Development)

# search for libmonero-cpp.dylib in same directory as libmonero-java.dylib on mac for portability
# command: install_name_tool -add_rpath @loader_path/ ./libmonero-java.dylib 
if (APPLE)
    add_custom_command(TARGET monero-java 
        POST_BUILD COMMAND 
        ${CMAKE_INSTALL_NAME_TOOL} -add_rpath "@loader_path/"
        $<TARGET_FILE:monero-java>)
endif()
