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

define i32 @main() {
  %1 = add i32 3000, 4002
  %2 = add i32 %1, 1
  %3 = add i32 %2, 3
  %4 = add i32 %3, 2
  %5 = udiv i32 23, 30
  %6 = mul i32 7, 8
  %7 = mul i32 %6, 9
  %8 = mul i32 %7, 1
  %9 = call i32 @a(i32 10000)
  %10 = call i32 @a(i32 %9)
  %11 = add i32 1123, %4
  %12 = add i32 %11, %5
  %13 = add i32 %12, %8
  %14 = add i32 %13, 1
  %15 = add i32 %14, %10
  call void @putInt32(i32 %15)
  %16 = udiv i32 2, 3
  call void @putInt32(i32 %16)
  %17 = add i32 3, 4
  call void @putInt32(i32 %17)
  ret i32 0
}
