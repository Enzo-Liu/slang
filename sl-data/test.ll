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
  %2 = add i32 1123, %1
  call void @putInt32(i32 %2)
  ret i32 0
}
