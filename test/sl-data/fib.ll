; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define i32 @putInt32(i32 %a) {
  %1 = getelementptr inbounds [4 x i8], [4 x i8]* @intFormat, i32 0, i32 0
  %2 = call i32 (i8*, ...) @printf(i8* %1, i32 %a)
  ret i32 %2
}

define i32 @fib(i32 %n) {
  br label %if-entry

if-entry:                                         ; preds = %0
  %1 = icmp eq i32 %n, 0
  %2 = icmp eq i1 %1, true
  br i1 %2, label %if-then, label %if-else

if-then:                                          ; preds = %if-entry
  br label %if-exit1

if-else:                                          ; preds = %if-entry
  br label %if-entry1

if-entry1:                                        ; preds = %if-else
  %3 = icmp eq i32 %n, 1
  %4 = icmp eq i1 %3, true
  br i1 %4, label %if-then1, label %if-else1

if-then1:                                         ; preds = %if-entry1
  br label %if-exit

if-else1:                                         ; preds = %if-entry1
  %5 = sub i32 %n, 1
  %6 = call i32 @fib(i32 %5)
  %7 = sub i32 %n, 2
  %8 = call i32 @fib(i32 %7)
  %9 = add i32 %6, %8
  br label %if-exit

if-exit:                                          ; preds = %if-else1, %if-then1
  %10 = phi i32 [ 1, %if-then1 ], [ %9, %if-else1 ]
  br label %if-exit1

if-exit1:                                         ; preds = %if-exit, %if-then
  %11 = phi i32 [ 1, %if-then ], [ %10, %if-exit ]
  ret i32 %11
}

define i32 @lambda(i32 %a) {
  %1 = add i32 %a, 1
  ret i32 %1
}

define i32 @main() {
main-entry:
  %0 = call i32 @fib(i32 10)
  %1 = call i32 @putInt32(i32 %0)
  %2 = call i32 @lambda(i32 100)
  %3 = call i32 @putInt32(i32 %2)
  ret i32 0
}
