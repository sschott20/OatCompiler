* processing file: pset6programs/alex.oat
..typechecking..frontend..optimizing..writing file: output/alex_39.ll
------------------------------------------------------------------------ alex.ll
define i64 @f(i64 %_a23, i64 %_b20, i64 %_c17, i64 %_d14, i64 %_e11, i64 %_x8) {
  %_a24 = alloca i64
  %_b21 = alloca i64
  %_c18 = alloca i64
  %_d15 = alloca i64
  %_e12 = alloca i64
  %_x9 = alloca i64
  store i64 %_a23, i64* %_a24
  store i64 %_b20, i64* %_b21
  store i64 %_c17, i64* %_c18
  store i64 %_d14, i64* %_d15
  store i64 %_e11, i64* %_e12
  store i64 %_x8, i64* %_x9
  %_a26 = load i64, i64* %_a24
  %_b27 = load i64, i64* %_b21
  %_bop28 = add i64 %_a26, %_b27
  %_c29 = load i64, i64* %_c18
  %_bop30 = add i64 %_bop28, %_c29
  %_d31 = load i64, i64* %_d15
  %_bop32 = add i64 %_bop30, %_d31
  %_e33 = load i64, i64* %_e12
  %_bop34 = add i64 %_bop32, %_e33
  %_x35 = load i64, i64* %_x9
  %_bop36 = add i64 %_bop34, %_x35
  ret i64 %_bop36
}

define i64 @main(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_result7 = call i64 @f(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6)
  ret i64 %_result7
}


declare i64* @oat_malloc(i64)
declare i64* @oat_alloc_array(i64)
declare void @oat_assert_not_null(i8*)
declare void @oat_assert_array_length(i64*, i64)
declare { i64, [0 x i64] }* @array_of_string(i8*)
declare i8* @string_of_array({ i64, [0 x i64] }*)
declare i64 @length_of_string(i8*)
declare i8* @string_of_int(i64)
declare i8* @string_cat(i8*, i8*)
declare void @print_string(i8*)
declare void @print_int(i64)
declare void @print_bool(i1)
..compiling with backend
allocated: %r10 <- _a24
allocated: %r11 <- _b21
allocated: LStk -2 <- _c18
allocated: LStk -3 <- _d15
allocated: LStk -4 <- _e12
allocated: LStk -5 <- _x9
allocated: %rdx <- _a26
allocated: %rsi <- _b27
allocated: %rdi <- _bop28
allocated: %rdx <- _c29
allocated: %rsi <- _bop30
allocated: %rdx <- _d31
allocated: %rdi <- _bop32
allocated: %rdx <- _e33
allocated: %rsi <- _bop34
allocated: %rdx <- _x35
allocated: %rdi <- _bop36
allocated: %rdx <- _result7
call: f live = 
* clang -c -O1 -Wall -Wno-override-module  -o output/alex_39.o output/alex_39.s
* clang -Wno-override-module -O1 -Wall  -o a.out output/alex_39.o   
* ./a.out 
--------------------------------------------------------------- Executing: a.out
* a.out returned 21
