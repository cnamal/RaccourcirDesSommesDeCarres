cmake_minimum_required(VERSION 3.5)
project(Cpp)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp Tree.hpp)
add_executable(Cpp ${SOURCE_FILES})