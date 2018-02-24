; ModuleID = 'test'
source_filename = "<string>"

@t = constant [5 x i8] c"test\00"

declare void @puts(i8*)

define i32 @main() {
entry:
  call void @puts(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @t, i32 0, i32 0))
  ret i32 1
}

