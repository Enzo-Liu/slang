; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"

declare void @printf(i8*, i32)

define void @putInt32(i32 %a) {
  %1 = getelementptr inbounds [4 x i8], [4 x i8]* @intFormat, i32 0, i32 0
  call void @printf(i8* %1, i32 %a)
  ret void
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
  %1 = call i32 @b(i32 3000, i32 4002)
  %2 = udiv i32 23, 30
  %3 = mul i32 7, 8
  %4 = mul i32 %3, 9
  %5 = mul i32 %4, 1
  %6 = call i32 @a(i32 10000)
  %7 = call i32 @a(i32 %6)
  %8 = add i32 1123, %1
  %9 = add i32 %8, %2
  %10 = add i32 %9, %5
  %11 = add i32 %10, 1
  %12 = add i32 %11, %7
  call void @putInt32(i32 %12)
  %13 = udiv i32 2, 3
  call void @putInt32(i32 %13)
  %14 = add i32 3, 4
  call void @putInt32(i32 %14)
  ret i32 0
}
