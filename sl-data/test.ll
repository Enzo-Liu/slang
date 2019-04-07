; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"
@s = unnamed_addr constant [32 x i8] c"hello world-and will print size\00"
@strFormat = unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @putInt32(i32 %a) {
  %1 = getelementptr inbounds [4 x i8], [4 x i8]* @intFormat, i32 0, i32 0
  %2 = call i32 (i8*, ...) @printf(i8* %1, i32 %a)
  ret i32 %2
}

define i32 @a(i32 %b) {
  %1 = add i32 %b, 1
  ret i32 %1
}

define i32 @b(i32 %c, i32 %d) {
  %1 = sub i32 %c, %d
  %2 = udiv i32 %c, %d
  %3 = udiv i32 %2, %1
  ret i32 %3
}

define i32 @main() {
  %1 = getelementptr inbounds [32 x i8], [32 x i8]* @s, i32 0, i32 0
  %2 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat, i32 0, i32 0
  %3 = call i32 (i8*, ...) @printf(i8* %2, i8* %1)
  %4 = call i32 @putInt32(i32 %3)
  %5 = call i32 @b(i32 3000, i32 4002)
  %6 = udiv i32 23, 30
  %7 = mul i32 7, 8
  %8 = mul i32 %7, 9
  %9 = mul i32 %8, 1
  %10 = call i32 @a(i32 10000)
  %11 = call i32 @a(i32 %10)
  %12 = add i32 1123, %5
  %13 = add i32 %12, %6
  %14 = add i32 %13, %9
  %15 = add i32 %14, 1
  %16 = add i32 %15, %11
  %17 = call i32 @putInt32(i32 %16)
  %18 = udiv i32 2, 3
  %19 = call i32 @putInt32(i32 %18)
  %20 = add i32 3, 4
  %21 = call i32 @putInt32(i32 %20)
  ret i32 0
}
