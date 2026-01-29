# CMakeLists.txt for MPC Designation Converter (C implementation)
#
# Build with:
#   mkdir build && cd build
#   cmake ..
#   cmake --build .
#
# Or for Release build:
#   cmake -DCMAKE_BUILD_TYPE=Release ..
#   cmake --build .

cmake_minimum_required(VERSION 3.10)

# Read version from parent VERSION file
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION" VERSION_CONTENT)
string(STRIP "${VERSION_CONTENT}" MPC_VERSION)

project(mpc_designation VERSION ${MPC_VERSION} LANGUAGES C)

# C standard
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler warnings
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra)
endif()

# Version definition
add_compile_definitions(MPC_VERSION="${MPC_VERSION}")

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)

# --- Library (optional, for linking into other projects) ---
add_library(mpc_designation_lib STATIC
    src/mpc_designation.c
    src/mpc_designation.h
)
set_target_properties(mpc_designation_lib PROPERTIES OUTPUT_NAME mpc_designation)

# --- CLI executable ---
add_executable(mpc_designation
    src/mpc_designation_cli.c
    src/mpc_designation.c
)

# --- Test executables ---
add_executable(test_csv
    test/test_csv.c
    src/mpc_designation.c
)

add_executable(test_errors
    test/test_errors.c
    src/mpc_designation.c
)

# --- Installation ---
install(TARGETS mpc_designation RUNTIME DESTINATION bin)
install(TARGETS mpc_designation_lib ARCHIVE DESTINATION lib)
install(FILES src/mpc_designation.h DESTINATION include)

# --- CTest support ---
enable_testing()

# Find test data directory
set(TEST_DATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../test-data")

add_test(NAME error_tests
    COMMAND test_errors "${TEST_DATA_DIR}/error_test_cases.csv"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# CSV test requires decompressed data - only add if file exists
if(EXISTS "${TEST_DATA_DIR}/prov_unpack_to_pack.csv")
    add_test(NAME conversion_tests
        COMMAND test_csv "${TEST_DATA_DIR}/prov_unpack_to_pack.csv"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
endif()
