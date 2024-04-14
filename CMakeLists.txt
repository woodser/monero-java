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

############
# Boost
############

set(Boost_NO_BOOST_CMAKE 1)
set(Boost_USE_MULTITHREADED ON)
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

######################
# monero-cpp
######################

add_library(monero-cpp SHARED IMPORTED)

# import shared c++ library
if (APPLE)
  set_target_properties(monero-cpp PROPERTIES IMPORTED_LOCATION ./libmonero-cpp.dylib)
elseif (WIN32)
  set_target_properties(monero-cpp PROPERTIES IMPORTED_LOCATION ./libmonero-cpp.dll)
  set_target_properties(monero-cpp PROPERTIES IMPORTED_IMPLIB ./libmonero-cpp.dll.a)
else()
  set_target_properties(monero-cpp PROPERTIES IMPORTED_LOCATION ./libmonero-cpp.so)
endif()

###############################################
# Build Java dynamic library for JNI
###############################################

set(
    MONERO_JNI_SRC_FILES
    src/main/cpp/monero_jni_bridge.cpp
)
add_library(monero-java SHARED ${MONERO_JNI_SRC_FILES})

target_include_directories(monero-java PUBLIC
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
  target_include_directories(monero-java PUBLIC "$ENV{JAVA_HOME}/include/darwin")
elseif (WIN32)
  target_include_directories(monero-java PUBLIC "$ENV{JAVA_HOME}/include/win32")
else()
  target_include_directories(monero-java PUBLIC "$ENV{JAVA_HOME}/include/linux")
endif()

target_link_libraries(monero-java
    monero-cpp
    ${Boost_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    ${EXTRA_LIBRARIES}
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
