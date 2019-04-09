; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"
@const-str = unnamed_addr constant [13 x i8] c"fib 10 is : \00"
@strFormat = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str1 = unnamed_addr constant [8 x i8] c"end fib\00"
@strFormat.1 = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str2 = unnamed_addr constant [17 x i8] c"in if: \22test sdf\00"
@strFormat.2 = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str3 = unnamed_addr constant [13 x i8] c"in else: sdf\00"
@strFormat.3 = unnamed_addr constant [4 x i8] c"%s\0A\00"

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
  %2 = icmp eq i32 %n, 1
  %3 = or i1 %1, %2
  %4 = icmp eq i1 %3, true
  br i1 %4, label %if-then, label %if-else

if-then:                                          ; preds = %if-entry
  br label %if-exit

if-else:                                          ; preds = %if-entry
  %5 = sub i32 %n, 1
  %6 = call i32 @fib(i32 %5)
  %7 = sub i32 %n, 2
  %8 = call i32 @fib(i32 %7)
  %9 = add i32 %6, %8
  br label %if-exit

if-exit:                                          ; preds = %if-else, %if-then
  %10 = phi i32 [ 1, %if-then ], [ %9, %if-else ]
  ret i32 %10
}

define i32 @b(i32 %c, i32 %d) {
  %1 = sub i32 %c, %d
  %2 = udiv i32 %c, %d
  %3 = udiv i32 %2, %1
  ret i32 %3
}

define i32 @main() {
main-entry:
  %0 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str, i32 0, i32 0
  %1 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %2 = call i32 (i8*, ...) @printf(i8* %1, i8* %0)
  %3 = call i32 @fib(i32 10)
  %4 = call i32 @putInt32(i32 %3)
  %5 = getelementptr inbounds [8 x i8], [8 x i8]* @const-str1, i32 0, i32 0
  %6 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %7 = call i32 (i8*, ...) @printf(i8* %6, i8* %5)
  %8 = call i32 @b(i32 3000, i32 4002)
  %9 = udiv i32 23, 30
  %10 = mul i32 7, 8
  %11 = mul i32 %10, 9
  %12 = mul i32 %11, 1
  %13 = add i32 1123, %8
  %14 = add i32 %13, %9
  %15 = add i32 %14, %12
  %16 = add i32 %15, 1
  br label %if-entry

if-entry:                                         ; preds = %main-entry
  %17 = icmp eq i1 true, true
  br i1 %17, label %if-then, label %if-else

if-then:                                          ; preds = %if-entry
  %18 = getelementptr inbounds [17 x i8], [17 x i8]* @const-str2, i32 0, i32 0
  %19 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %20 = call i32 (i8*, ...) @printf(i8* %19, i8* %18)
  br label %if-exit

if-else:                                          ; preds = %if-entry
  %21 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str3, i32 0, i32 0
  %22 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %23 = call i32 (i8*, ...) @printf(i8* %22, i8* %21)
  br label %if-exit

if-exit:                                          ; preds = %if-else, %if-then
  %24 = phi i32 [ %20, %if-then ], [ %23, %if-else ]
  %25 = udiv i32 2, 3
  %26 = call i32 @putInt32(i32 %25)
  %27 = add i32 3, 4
  ret i32 0
}
