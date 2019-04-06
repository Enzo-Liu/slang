; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [3 x i8] c"%d\00"

declare void @printf(i8*, i32)

define void @putInt32(i32 %a) {
  %1 = getelementptr inbounds [3 x i8], [3 x i8]* @intFormat, i32 0, i32 0
  call void @printf(i8* %1, i32 %a)
  ret void
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
  %9 = add i32 1123, %4
  %10 = add i32 %9, %5
  %11 = add i32 %10, %8
  %12 = add i32 %11, 1
  call void @putInt32(i32 %12)
  ret i32 0
}
